#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail

# Tools::Tmux drives a fixture tmux through exact argv. Regression: tmux exit
# statuses were once read through `if @ Shell succeeds:`, which always
# succeeded and leaked the word true/false into the method's output.
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_dir=$(mktemp -d)
mkdir -p "$test_dir/bin"
trap 'rm -rf "$test_dir"' EXIT

cat > "$test_dir/bin/tmux" <<'SH'
#!/usr/bin/env bash
printf '%s\0' "$@" | jq -Rs 'split("\u0000")[:-1]' > "$TMUX_ARGV"
case "$1" in
  has-session|kill-session) [[ "$3" == live ]] ;;
  new-session|send-keys) exit 0 ;;
  list-sessions) printf 'live\nother\n' ;;
  display-message) printf 'live:100:200\n' ;;
  *) exit 2 ;;
esac
SH
chmod +x "$test_dir/bin/tmux"
export PATH="$test_dir/bin:$PATH" SQLITE_JSON_DB="$test_dir/instances.db"
export TMUX_ARGV="$test_dir/argv.json"
source "$root/lib/trash.bash" 2>/dev/null

check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }
argv() { jq -c . "$TMUX_ARGV"; }

check 'missing session is exactly false' false "$(@ Tools::Tmux sessionExists: absent)"
check 'live session is exactly true' true "$(@ Tools::Tmux sessionExists: live)"
check 'has-session uses exact argv' '["has-session","-t","live"]' "$(argv)"

check 'createSession: reports success' true "$(@ Tools::Tmux createSession: "a b" withCommand: "echo 'hi' \$HOME")"
check 'createSession: passes name and command verbatim' '["new-session","-d","-s","a b","echo '"'"'hi'"'"' $HOME"]' "$(argv)"

check 'sendToSession: sends keys then Enter' true "$(@ Tools::Tmux sendToSession: live command: 'ls -la')"
check 'send-keys argv' '["send-keys","-t","live","ls -la","Enter"]' "$(argv)"
out=$(@ Tools::Tmux sendToSession: absent command: 'ls' 2>/dev/null); status=$?
check 'sendToSession: to a missing session fails' 1 "$status"
check 'failed send prints nothing' '' "$out"

check 'killSession: live' true "$(@ Tools::Tmux killSession: live)"
check 'killSession: absent' false "$(@ Tools::Tmux killSession: absent)"
check 'listSessions returns names' $'live\nother' "$(@ Tools::Tmux listSessions)"
check 'sessionInfo: returns display output' 'live:100:200' "$(@ Tools::Tmux sessionInfo: live)"

