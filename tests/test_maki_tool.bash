#!/usr/bin/env bash
export TRASHTALK_GUSGUS_PROFILE=maki
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" MAKI_INSTALL_DIR="$tmp/local bin"
export MAKI_TEST_BINARY="$tmp/fixture-maki" MAKI_TEST_INSTALLER="$tmp/fixture-installer"
export MAKI_TEST_DOWNLOADS="$tmp/downloads" MAKI_TEST_ARGS="$tmp/args" MAKI_TEST_TTY="$tmp/tty"
db_init
# Completely controlled PATH: missing-tool tests must never find the host Maki
# or use a real curl. All subprocesses and installations stay in this fixture.
mkdir "$tmp/bin"
for command in bash sh env jq jo sqlite3 uuidgen cat cp chmod mkdir rm mktemp find wc tr uname date sed head tail cut sort dirname basename perl shasum readlink python3; do
    executable=$(command -v "$command") || continue
    ln -s "$executable" "$tmp/bin/$command"
done
cat > "$tmp/bin/curl" <<'CURL'
#!/usr/bin/env bash
printf '%s\n' "$@" >> "$MAKI_TEST_DOWNLOADS"
while [[ $# -gt 0 ]]; do
    if [[ "$1" == -o ]]; then dest=$2; shift; fi
    shift
done
cp "$MAKI_TEST_INSTALLER" "$dest"
exit "${MAKI_TEST_DOWNLOAD_STATUS:-0}"
CURL
cat > "$MAKI_TEST_INSTALLER" <<'INSTALL'
#!/bin/sh
echo 'installer diagnostics'
test "${MAKI_TEST_INSTALL_STATUS:-0}" = 0 || exit "$MAKI_TEST_INSTALL_STATUS"
cp "$MAKI_TEST_BINARY" "$MAKI_INSTALL_DIR/maki"
chmod +x "$MAKI_INSTALL_DIR/maki"
INSTALL
cat > "$MAKI_TEST_BINARY" <<'MAKI'
#!/usr/bin/env bash
case "$1" in
    --version) echo 'maki fixture'; exit "${MAKI_TEST_VERSION_STATUS:-0}" ;;
    auth)
        printf '%s\n' "$@" > "$MAKI_TEST_ARGS"
        if [[ "$2" == status ]]; then echo 'fixture auth status'; exit; fi
        [[ -t 0 && -t 1 && -t 2 ]] && echo true > "$MAKI_TEST_TTY"
        printf 'Provider login prompt: '
        IFS= read -r answer
        printf 'received %s\n' "$answer"
        exit "${MAKI_TEST_LOGIN_STATUS:-0}"
        ;;
esac
MAKI
chmod +x "$tmp/bin/curl" "$MAKI_TEST_BINARY"
export PATH="$tmp/bin"
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; exit 1; fi; }
contains() { [[ "$3" == *"$2"* ]] || { echo "FAIL: $1 missing $2"; exit 1; }; echo "PASS: $1"; passed=$((passed+1)); }
@ Maki name >/dev/null
check 'Maki is a Tool subclass' Tool "$(@ Runtime superclassOf: Maki)"
check 'missing Maki is detected' false "$(@ Maki isInstalled)"
check 'missing version is readable' 'not installed' "$(@ Maki version)"
@ Maki loginToProvider: openai >/dev/null 2>&1; status=$?
check 'missing login returns 127' 127 "$status"
_clear_error

doctor=$(@ Trash doctor 2>&1); status=$?
check 'doctor installs missing Maki' 0 "$status"
contains 'doctor reports Maki' 'Maki found' "$doctor"
contains 'installer downloads official URL' 'https://maki.sh/install.sh' "$(cat "$MAKI_TEST_DOWNLOADS")"
check 'user-local executable found outside PATH' "$MAKI_INSTALL_DIR/maki" "$(@ Maki path)"
check 'version uses exact executable path with spaces' 'maki fixture' "$(@ Maki version)"
check 'doctor does not start authentication' false "$([[ -e "$MAKI_TEST_ARGS" ]] && echo true || echo false)"
downloads=$(cat "$MAKI_TEST_DOWNLOADS")
check 'ensure stdout is boolean only' true "$(@ Maki ensure)"
@ Trash doctor >/dev/null
check 'already installed Maki is not downloaded again' "$downloads" "$(cat "$MAKI_TEST_DOWNLOADS")"
check 'auth status delegates to Maki' 'fixture auth status' "$(@ Maki authStatus)"
check 'auth status exact arguments' $'auth\nstatus' "$(cat "$MAKI_TEST_ARGS")"
export MAKI_TEST_LOGIN_STATUS=27
@ Maki loginToProvider: openai <<< 'test answer' > "$tmp/login-output"; status=$?
check 'login preserves provider exit status' 27 "$status"
check 'login exact arguments' $'auth\nlogin\nopenai' "$(cat "$MAKI_TEST_ARGS")"
contains 'login inherits stdin' 'received test answer' "$(cat "$tmp/login-output")"
@ Maki loginToProvider: '--help' >/dev/null 2>&1; status=$?
check 'provider cannot inject an option' 2 "$status"
_clear_error
@ Maki loginToProvider: 'openai; touch should-not-exist' >/dev/null 2>&1; status=$?
check 'invalid provider is rejected' 2 "$status"
_clear_error

# A controlling terminal proves the @ dispatcher does not capture login output.
export MAKI_TEST_LOGIN_STATUS=0
python3 - "$root" <<'PY'
import errno, os, pty, select, subprocess, sys, time
master, slave = pty.openpty()
proc = subprocess.Popen(['bash', '-c', 'source "$1/lib/trash.bash"; @ Maki loginToProvider: openai', 'test', sys.argv[1]], stdin=slave, stdout=slave, stderr=slave)
os.close(slave)
output = b''
answered = False
deadline = time.monotonic() + 20
try:
    while time.monotonic() < deadline:
        if not select.select([master], [], [], 0.1)[0]:
            continue
        try:
            chunk = os.read(master, 4096)
        except OSError as error:
            if error.errno == errno.EIO:
                break
            raise
        if not chunk:
            break
        output += chunk
        if b'Provider login prompt:' in output and not answered:
            os.write(master, b'tty answer\n')
            answered = True
    assert answered and b'received tty answer' in output, output.decode(errors='replace')
    assert proc.wait(timeout=5) == 0
finally:
    if proc.poll() is None:
        proc.kill()
        proc.wait()
    os.close(master)
PY
check 'login prompt streams before terminal input' 0 "$?"
check 'login inherits all terminal descriptors' true "$(cat "$MAKI_TEST_TTY")"

rm "$MAKI_INSTALL_DIR/maki"
export MAKI_TEST_DOWNLOAD_STATUS=22
@ Maki ensure >/dev/null 2>&1; status=$?
check 'download failure propagates' 1 "$status"
check 'partial download is never executed' false "$(@ Maki isInstalled)"
check 'temporary installer directories removed' 0 "$(find "$TMPDIR" -maxdepth 1 -type d -name 'trash-maki-install.*' | wc -l | tr -d ' ')"
unset MAKI_TEST_DOWNLOAD_STATUS
export MAKI_TEST_INSTALL_STATUS=7
doctor=$(@ Trash doctor 2>&1); status=$?
check 'failed installation makes doctor fail' 1 "$status"
contains 'doctor explains installation failure' 'Maki installation/check failed' "$doctor"
unset MAKI_TEST_INSTALL_STATUS
export MAKI_TEST_VERSION_STATUS=9
@ Maki ensure >/dev/null 2>&1; status=$?
check 'installer must produce runnable Maki' 1 "$status"
@ Maki ensure >/dev/null 2>&1; status=$?
check 'broken existing executable makes ensure fail' 1 "$status"
echo "=== $passed Maki checks passed ==="
