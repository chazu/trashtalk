#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
export PROGRESS_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 - <<'PY'
import errno, fcntl, glob, json, os, pty, select, signal, subprocess, tempfile, termios, time

root = os.environ['PROGRESS_ROOT']
with tempfile.TemporaryDirectory() as tmp:
    def run(delay, mode='1', cancel=False, terminal=True, facade=False):
        env = os.environ.copy()
        env.update(TRASHTALK_PROGRESS=mode, TMPDIR=tmp)
        env.pop('TRASHTALK_INTERACTIVE', None)
        env.pop('_TRASH_PROGRESS_ACTIVE', None)
        env.pop('TRASH_SESSION_ID', None)
        env.update(SQLITE_JSON_DB=tmp + '/instances.db', TRASHDIR=root + '/trash')
        # The terminal owner stays alive through cancellation, as an interactive
        # shell does. Killing the PTY session itself revokes its output device.
        script = 'trap ":" TERM HUP; source "$PROGRESS_ROOT/lib/trash-progress.bash"; _trash_with_progress "Waiting for fixture" bash -c \'printf "{\\"ok\\":true}\\n"; printf "diagnostic\\n" >&2; sleep "$1"; exit 7\' bash "$1"'
        if facade:
            script = 'source "$PROGRESS_ROOT/lib/trash.bash" || exit; trap _env_cleanup EXIT; @ Tool captureArgvJson: "$2" input: "" progress: "Waiting for fixture"'
        child_argv = json.dumps(['bash', '-c', 'printf \'{"ok":true}\\n\'; printf \'diagnostic\\n\' >&2; sleep "$1"; exit 7', 'bash', str(delay)])
        master, slave = pty.openpty()
        def setup():
            os.setsid()
            if terminal:
                fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
        proc = subprocess.Popen(['bash', '-c', script, 'bash', str(delay), child_argv], env=env,
                                stdin=slave if terminal else subprocess.DEVNULL,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                preexec_fn=setup)
        os.close(slave)
        ui = b''
        deadline = time.monotonic() + 5
        cancelled = False
        while time.monotonic() < deadline:
            ready, _, _ = select.select([master], [], [], .02)
            if ready:
                try:
                    chunk = os.read(master, 65536)
                    if not chunk: break
                    ui += chunk
                except OSError as error:
                    if error.errno == errno.EIO: break
                    raise
            if cancel and b'Waiting for fixture' in ui and not cancelled:
                os.killpg(proc.pid, signal.SIGTERM)
                cancelled = True
            if proc.poll() is not None and not terminal: break
        else:
            os.killpg(proc.pid, signal.SIGKILL)
            raise AssertionError('progress failed to terminate')
        out, err = proc.communicate(timeout=2)
        os.close(master)
        assert not glob.glob(tmp + '/trash-progress.*'), 'renderer leaked temporary state'
        if facade:
            assert proc.returncode == 0 and not err, (proc.returncode, out, err)
            assert json.loads(out) == {'schema_version': 1, 'exit_code': 7,
                                       'stdout': '{"ok":true}\n', 'stderr': 'diagnostic\n'}, out
        elif not cancel:
            assert proc.returncode == 7, proc.returncode
            assert out == b'{"ok":true}\n', out
            assert err == b'diagnostic\n', err
        return ui

    assert b'Waiting for fixture' not in run(0), 'fast work flashed a status line'
    slow = run(.35)
    assert slow.count(b'Waiting for fixture') == 1, slow
    assert slow.endswith(b'\r\x1b[2K'), 'status was not cleared before returning'
    assert run(.25, mode='auto') == b'', 'noninteractive call displayed progress'
    assert run(.25, terminal=False) == b'', 'headless call displayed progress'
    cancelled = run(2, cancel=True)
    assert cancelled.endswith(b'\r\x1b[2K'), ('cancelled work left its status line', cancelled)
    captured = run(.35, facade=True)
    assert captured.count(b'Waiting for fixture') == 1 and captured.endswith(b'\r\x1b[2K'), captured
print('PASS: delayed TTY progress preserves output/status and cleans up on cancellation')
PY
