#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
# Raw bodies are rebuilt from tokens and then normalized; the normalization
# must never reach inside quoted strings or heredoc bodies.
cat > "$tmp/RawSpacing.trash" <<'EOF'
RawSpacing subclass: Object
  rawClassMethod: quoted [
    printf '%s\n' "a    b" 'c  d' "e  \$f  \"g\"" "x = 'y'" "( p ) > /q"
  ]
  rawClassMethod: heredoc [
    cat <<'TEXT'
  indented  text = 1
    deeper ( kept )
TEXT
  ]
  rawClassMethod: code [
    local value=3
    echo "$(( value * 2 ))"
  ]
EOF
"$root/lib/jq-compiler/driver.bash" compile "$tmp/RawSpacing.trash" --check > "$tmp/compiled"
source "$tmp/compiled"
check() {
    [[ "$2" == "$3" ]] || { printf 'FAIL: %s expected=%s actual=%s\n' "$1" "$2" "$3"; exit 1; }
    printf 'PASS: %s\n' "$1"
}
check 'quoted strings keep every space' $'a    b\nc  d\ne  $f  "g"\nx = \'y\'\n( p ) > /q' "$(__RawSpacing__class__quoted)"
check 'heredoc bodies keep spacing and indentation' $'  indented  text = 1\n    deeper ( kept )' "$(__RawSpacing__class__heredoc)"
check 'unquoted code is still normalized and runs' '6' "$(__RawSpacing__class__code)"
