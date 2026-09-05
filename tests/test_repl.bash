#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
export REPL_TEST_ROOT="$TRASHTALK_DIR"
python3 - <<'PY'
import errno, os, pty, select, subprocess, time
root = os.environ['REPL_TEST_ROOT']
history = root + '/repl-history'
env = dict(os.environ, TRASHTALK_HISTORY_FILE=history, TERM='xterm')
env.pop('TRASH_SESSION_ID', None)
master, slave = pty.openpty()
proc = subprocess.Popen([root + '/bin/trash'], stdin=slave, stdout=slave, stderr=slave, env=env)
os.close(slave)
transcript = b''
cursor = 0
def until(needle, timeout=10):
    global transcript, cursor
    deadline = time.monotonic() + timeout
    while True:
        found = transcript.find(needle, cursor)
        if found >= 0:
            cursor = found + len(needle)
            return
        if time.monotonic() > deadline:
            raise AssertionError((needle, transcript))
        if select.select([master], [], [], .05)[0]:
            try: chunk = os.read(master, 65536)
            except OSError as e:
                if e.errno == errno.EIO: raise AssertionError(transcript)
                raise
            if not chunk: raise AssertionError(transcript)
            transcript += chunk
try:
    until(b'@ ')
    os.write(master, b'Count\t')
    until(b'Counter ')
    os.write(master, b'descrip\t\n')
    until(b'A simple counter')
    os.write(master, b'\x1b[A\n')
    until(b'A simple counter')
    os.write(master, b'counter=$(Counter new)\n')
    until(b'counter=counter_')
    os.write(master, b'$counter getVal\t\n')
    until(b'getValue')
    until(b'@ ')
    os.write(master, b'exit\n')
    until(b'Goodbye!')
    try: proc.wait(timeout=5)
    except subprocess.TimeoutExpired: raise AssertionError(transcript)
    assert proc.returncode == 0, transcript
    assert b'Cannot exec filter' not in transcript and b'warning: line editing' not in transcript, transcript
    lines = open(history).read().splitlines()
    assert 'Counter description ' in lines or 'Counter description' in lines, lines
    assert len(lines) == 5 and not any(line.startswith('echo ') for line in lines), lines
finally:
    if proc.poll() is None: proc.kill(); proc.wait()
    os.close(master)
help_result = subprocess.run([root + '/bin/trash', '--help'], env=env, capture_output=True)
assert help_result.returncode == 0 and b'Usage:' in help_result.stdout, help_result
headless = subprocess.run([root + '/bin/trash'], input=b'Counter description\nexit\n', env=env, capture_output=True)
assert headless.returncode == 0 and b'A simple counter' in headless.stdout and not headless.stderr, headless
print('PASS: REPL launch, receiver/method/instance completion, history recall, help, and piped input')
PY
