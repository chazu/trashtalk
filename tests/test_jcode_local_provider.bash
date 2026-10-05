#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export TRASHTALK_DIR="$root"
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs with spaces"
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=jcode-owner
unset TRASHTALK_GUSGUS_PROFILE
export JCODE_TEST_GATE="$tmp/gate" JCODE_HOME="$tmp/source-auth" CODEX_HOME="$tmp/no-codex"
mkdir -p "$tmp/bin" "$JCODE_HOME" "$tmp/workspace with spaces"
# Interactive shells may source Trashtalk without exporting its optional root.
export HOME="$tmp/home"
mkdir -p "$HOME"
ln -s "$root" "$HOME/.trashtalk"
unset TRASHTALK_DIR
printf '{}\n' > "$JCODE_HOME/openai-auth.json"
cp "$root/tests/fixtures/jcode-api.py" "$tmp/bin/jcode"
chmod +x "$tmp/bin/jcode"
export PATH="$tmp/bin:$PATH" OPENAI_API_KEY=fixture CODEX_API_KEY=fixture OPENROUTER_API_KEY=fixture
cleanup() {
    for config in "$TRASHTALK_RUN_DIR"/*/jcode.json; do
        [[ ! -s "$config" ]] || bash "$root/lib/jcode-processes.bash" stop "$(jq -r .home "$config")" >/dev/null 2>&1 || true
    done
    for pf in "$TRASHTALK_RUN_DIR"/*/pid; do
        [[ -s "$pf" ]] || continue
        read -r pid < "$pf"
        kill -TERM -- "-$pid" 2>/dev/null || true
    done
    for path in "$TRASHTALK_RUN_DIR"/hosts/*/jcode/runtime.path; do
        [[ ! -s "$path" ]] || rmdir "$(cat "$path")" 2>/dev/null || true
    done
    rm -rf "$tmp"
}
trap cleanup EXIT
db_init
passed=0
source tests/helpers/check.bash
field() { db_get "$1" | jq -r --arg f "$2" '.[$f] // empty'; }
await_file() { for i in {1..100}; do [[ ! -s "$1" ]] || return; sleep .1; done; echo "FAIL: missing $1"; exit 1; }
host() { printf '%s/hosts/%s/jcode' "$TRASHTALK_RUN_DIR" "$session"; }

# Subscription default: the bridge uses the openai provider and refreshes the catalog.
session=$(@ Gusgus sessionFor: "$tmp/workspace with spaces")
@ Inbox send: first to: "session:$session" from: jcode-owner >/dev/null
run=$(@ Agent::Worker tickSession: "$session")
await_file "$(host)/fixture-bridge-argv"
check 'default provider run launches' running "$(field "$run" state)"
contains 'default bridge uses openai' $'--provider\nopenai' "$(cat "$(host)/fixture-bridge-argv")"
check 'default provider refreshes the catalog' refreshed "$(cat "$(host)/fixture-catalog-ready")"
check 'default provider has no generated config' false "$([[ -e "$(host)/config.toml" ]] && echo true || echo false)"

# Local provider: generated profile, linked key, no OpenAI credentials needed.
rm -rf "$JCODE_HOME/openai-auth.json" "$HOME/.config"
export XDG_CONFIG_HOME="$HOME/.config"
export TRASHTALK_JCODE_PROVIDER=omlx TRASHTALK_JCODE_MODEL=local-model
@ "$run" stop >/dev/null
mkdir -p "$tmp/workspace-local" "$tmp/workspace-key" "$tmp/workspace-ready"
session=$(@ Gusgus fresh: "$tmp/workspace-local")
@ Inbox send: second to: "session:$session" from: jcode-owner >/dev/null
run=$(@ Agent::Worker tickSession: "$session")
check 'local provider without base URL is refused' '' "$run"
contains 'base URL diagnostic' 'jcode.baseUrl is required' "$(cat "$TRASHTALK_RUN_DIR"/*/stderr.log)"
export TRASHTALK_JCODE_BASE_URL=http://127.0.0.1:9/v1
# A refused launch leaves its delivery needing review, so each case gets a session.
session=$(@ Gusgus fresh: "$tmp/workspace-key")
@ Inbox send: again to: "session:$session" from: jcode-owner >/dev/null
run=$(@ Agent::Worker tickSession: "$session")
check 'local provider without key file is refused' '' "$run"
contains 'key file diagnostic' 'provider-omlx.env' "$(cat "$TRASHTALK_RUN_DIR"/*/stderr.log)"
mkdir -p "$HOME/.config/jcode"
printf 'JCODE_PROVIDER_OMLX_API_KEY=local-key\n' > "$HOME/.config/jcode/provider-omlx.env"
session=$(@ Gusgus fresh: "$tmp/workspace-ready")
@ Inbox send: third to: "session:$session" from: jcode-owner >/dev/null
run=$(@ Agent::Worker tickSession: "$session")
await_file "$(host)/fixture-bridge-argv"
check 'local provider run launches without OpenAI login' running "$(field "$run" state)"
argv=$(cat "$(host)/fixture-bridge-argv")
contains 'local bridge selects the configured profile' $'--provider\nauto' "$argv"
contains 'local bridge passes the model' $'--model\nlocal-model' "$argv"
check 'local provider skips the OpenAI catalog' false "$([[ -e "$(host)/fixture-catalog-ready" ]] && echo true || echo false)"
config=$(cat "$(host)/config.toml")
contains 'profile is the default provider' 'default_provider = "omlx"' "$config"
contains 'profile has the base URL' 'base_url = "http://127.0.0.1:9/v1"' "$config"
contains 'profile names its key variable' 'api_key_env = "JCODE_PROVIDER_OMLX_API_KEY"' "$config"
contains 'profile declares the model window' 'context_window = 65536' "$config"
check 'key file is linked, not copied' "$HOME/.config/jcode/provider-omlx.env" "$(readlink "$(host)/config/jcode/provider-omlx.env")"
check 'recorded run config names the provider' omlx "$(jq -r .provider "$TRASHTALK_RUN_DIR/$run/jcode.json")"
export TRASHTALK_JCODE_PROVIDER='bad name"'
check 'invalid provider names are rejected' 1 "$(@ Agent::JcodeDriver prepareLocalProvider: 'bad name"' home: "$tmp/h" model: m >/dev/null 2>&1; echo $?)"
echo "=== $passed Jcode local provider checks passed ==="
