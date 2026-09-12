#!/usr/bin/env python3
"""Stateful Jcode wire fixture; only tests use Python. No model or real daemon."""
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import threading
import time

if sys.argv[1:] == ['--version']:
    print('jcode fixture')
    sys.exit(0)

home = Path(os.environ['JCODE_HOME'])
assert os.environ['JCODE_WAKE_MODE'] == 'external'
assert all(os.environ[k] == '0' for k in ('JCODE_SWARM_ENABLED', 'JCODE_AUTO_POKE', 'JCODE_MEMORY_ENABLED'))
assert not any(os.environ.get(k) for k in (
    'TRASHTALK_RUN_TOKEN', 'OPENAI_API_KEY', 'CODEX_API_KEY', 'OPENROUTER_API_KEY'))
if sys.argv[-3:] == ['server', 'stop', '--force']:
    sys.exit(0)
assert sys.argv[-2:] == ['api-bridge', '--stdio']
assert '--tools' in sys.argv
state = home / 'fixture-state'
calls = home / 'fixture-calls.jsonl'
gate = Path(os.environ['JCODE_TEST_GATE'])
mode = os.environ.get('JCODE_TEST_MODE', 'normal')
native = 'jcode-fixture-session'
lock = threading.Lock()


def emit(ev, reply=None, **fields):
    with lock:
        record = dict(v=1, ev=ev, **fields)
        if reply is not None:
            record['reply_to'] = reply
        print(json.dumps(record), flush=True)


def finish(prompt, request_id=None):
    while not gate.exists():
        time.sleep(.03)
    if (home / 'fail-after-input').exists():
        emit('error', request_id, code='internal', message='fixture provider failure after steering')
        return
    delivery = None
    for line in prompt.splitlines():
        if line.startswith('--- delivery '):
            delivery = line.removeprefix('--- delivery ')
        if line.startswith('Read: '):
            argv = shlex.split(line.removeprefix('Read: '))
            # Exercise the actual Inbox show: operation through run authority.
            subprocess.run(argv, check=True, capture_output=True, text=True)
            body = subprocess.check_output([argv[0], argv[-1], 'body'], text=True).strip()
            subprocess.run([argv[0], 'AgentRun', 'result:', 'fixture got: ' + body,
                            'forDelivery:', delivery], check=True, capture_output=True)
            subprocess.run([argv[0], 'AgentRun', 'settle:', delivery],
                           check=True, capture_output=True)
    emit('text_delta', session_id=native, text='Direct fixture answer\n')
    if os.environ.get('JCODE_TEST_IDLE_LATE'):
        emit('turn_done', session_id=native)
        time.sleep(.4)
        state.write_text('idle')
    else:
        state.write_text('idle')
        emit('turn_done', session_id=native)


previous = 0
for line in sys.stdin:
    req = json.loads(line)
    assert req['v'] == 1 and req['id'] > previous
    previous = req['id']
    with calls.open('a') as f:
        f.write(json.dumps(req) + '\n')
    kind, rid = req['req'], req['id']
    if kind == 'hello':
        emit('hello_ok', rid, version=1, server='jcode/fixture')
    elif kind in ('create_session', 'attach_session'):
        if kind == 'create_session':
            assert not state.exists(), 'must reuse the native session'
            state.write_text('idle')
            (home / 'sessions').mkdir(exist_ok=True)
            (home / 'sessions' / (native + '.json')).write_text(json.dumps({'id': native, 'working_dir': os.getcwd(), 'messages': [{'role': 'user', 'content': 'retained history'}]}))
        else:
            assert req['session_id'] == native
        emit('attached', rid, session=dict(session_id=native, status=state.read_text(), working_dir=os.getcwd()))
    elif kind == 'ping':
        emit('pong', rid)
    elif kind == 'compact':
        assert req['session_id'] == native
        if (home / 'lose-compact').exists():
            os._exit(0)
        if (home / 'refuse-compact').exists():
            emit('error', rid, code='invalid_request', message='fixture compaction refused')
        else:
            def apply_compaction():
                # Exiting the bridge before completion loses this background job.
                time.sleep(3)
                history = home / 'sessions' / (native + '.json')
                journal = history.with_suffix('.journal.jsonl')
                with journal.open('a') as output:
                    output.write(json.dumps({'meta': {'updated_at': '2026-09-12T12:00:00Z', 'working_dir': os.getcwd(),
                        'compaction': {'summary': 'fixture retained summary'}}}) + '\n')
            threading.Thread(target=apply_compaction, daemon=True).start()
            emit('compacted', rid, session_id=native, message='Compaction scheduled')
    elif kind in ('set_model', 'set_reasoning_effort'):

        emit('ok', rid)
    elif kind == 'send_message':
        assert state.read_text() == 'idle', 'overlapping prompt'
        assert 'FIRST_SECRET' not in req['content'] and 'SECOND_SECRET' not in req['content']
        state.write_text('processing')
        emit('ok', rid)  # This is not completion or daemon acceptance.
        emit('turn_done', session_id='some-other-session')
        emit('turn_done', session_id=native)  # Stale event before acceptance.
        emit('message_accepted', session_id=native)
        if mode == 'lost':
            # Native Jcode Bash work can outlive both the bridge and cancel.
            subprocess.Popen([str(home / 'bin/bash'), '-c',
                              'echo $$ > "$1"; sleep 120', 'fixture-tool',
                              str(home / 'fixture-work.pid')],
                             stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                             stderr=subprocess.DEVNULL)
            while not (home / 'fixture-work.pid').exists():
                time.sleep(.01)
            os._exit(0)  # Daemon state survives the adapter connection.
        if mode == 'error':
            emit('error', rid, code='internal', message='fixture provider failure')
            continue
        threading.Thread(target=finish, args=(req['content'], rid), daemon=True).start()
    elif kind == 'soft_interrupt':
        assert req['session_id'] == native
        if (home / 'refuse-input').exists():
            emit('error', rid, code='invalid_request', message='fixture input refused')
        else:
            emit('ok', rid)
            emit('text_delta', session_id=native, text='Steering received\n')
            if state.read_text() == 'idle':
                state.write_text('processing')
                threading.Thread(target=finish, args=(req['content'], rid), daemon=True).start()
    elif kind == 'cancel':
        assert req['session_id'] == native
        if not (home / 'refuse-stop').exists():
            state.write_text('idle')
        emit('ok', rid)
    else:
        raise AssertionError(kind)
