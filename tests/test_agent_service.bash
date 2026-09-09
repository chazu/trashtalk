#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/bin" "$tmp/home"
cat > "$tmp/bin/uname" <<'UNAME'
#!/usr/bin/env bash
printf '%s\n' "$TEST_PLATFORM"
UNAME
cat > "$tmp/bin/systemctl" <<'SYSTEMCTL'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$SERVICE_CALLS"
SYSTEMCTL
chmod +x "$tmp/bin/"*
export PATH="$tmp/bin:$PATH" SERVICE_CALLS="$tmp/calls" TEST_PLATFORM=Darwin
HOME="$tmp/home" TRASHTALK_DIR="$root" "$root/bin/trash-worker-service" install
plist="$tmp/home/Library/LaunchAgents/org.trashtalk.agent-worker.plist"
# Installed arguments must name this checkout and preserve detached children.
plutil -extract ProgramArguments.1 raw -o - "$plist" | rg -F "$root/bin/trash-worker"
[[ "$(plutil -extract AbandonProcessGroup raw -o - "$plist")" == true ]]
[[ "$(plutil -extract KeepAlive raw -o - "$plist")" == true ]]
export TEST_PLATFORM=Linux
HOME="$tmp/home" XDG_CONFIG_HOME="$tmp/config" TRASHTALK_DIR="$root" "$root/bin/trash-worker-service" install
unit="$tmp/config/systemd/user/org.trashtalk.agent-worker.service"
rg -q '^KillMode=process$' "$unit"
rg -q '^Restart=always$' "$unit"
rg -q '^--user daemon-reload$' "$SERVICE_CALLS"
HOME="$tmp/home" XDG_CONFIG_HOME="$tmp/config" "$root/bin/trash-worker-service" start
rg -q '^--user enable --now org.trashtalk.agent-worker.service$' "$SERVICE_CALLS"
echo 'PASS: launchd and systemd installation contracts (no host service activated)'
