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


def finish(prompt):
    while not gate.exists():
        time.sleep(.03)
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
        else:
            assert req['session_id'] == native
        emit('attached', rid, session=dict(session_id=native, status=state.read_text()))
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
        threading.Thread(target=finish, args=(req['content'],), daemon=True).start()
    elif kind == 'cancel':
        assert req['session_id'] == native
        if not (home / 'refuse-stop').exists():
            state.write_text('idle')
        emit('ok', rid)
    else:
        raise AssertionError(kind)
