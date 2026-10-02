#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
# Raw bodies are reproduced from token positions, so Bash keeps the spelling
# and spacing it was written with; only indentation is normalized.
cat > "$tmp/RawFidelity.trash" <<'EOF'
RawFidelity subclass: Object
  rawClassMethod: quoted [
    printf '%s\n' "a    b" 'c  d' "e  \$f  \"g\"" "x = 'y'" "( p ) > /q"
  ]
  rawClassMethod: words [
    local a=b final=unsettled
    printf '%s|' send-keys -- "--flag" $'\t' "$a" $final
  ]
  rawClassMethod: arrays [
    local -a list=()
    list+=(one)
    list+=("two words")
    local x="pre/two words/post"
    printf '%s|' "${#list[@]}" "${x#*"${list[1]}"}"
  ]
  rawClassMethod: patterns: value [
    [[ $1 = "x" ]] && printf 'eq|' || printf 'ne|'
    case "$1" in *[!0-9]*) printf 'word' ;; [0-9]) printf 'digit' ;; *) printf 'number' ;; esac
  ]
  rawClassMethod: heredoc [
    cat <<'TEXT' | tr a-z A-Z
  $HOME  stays  literal
TEXT
  ]
  rawClassMethod: code [
    local value=3
    echo "$(( value * 2 ))"
  ]
EOF
"$root/lib/jq-compiler/driver.bash" compile "$tmp/RawFidelity.trash" --check > "$tmp/compiled"
source "$tmp/compiled"
check() {
    [[ "$2" == "$3" ]] || { printf 'FAIL: %s expected=%s actual=%s\n' "$1" "$2" "$3"; exit 1; }
    printf 'PASS: %s\n' "$1"
}
check 'quoted strings keep every space' $'a    b\nc  d\ne  $f  "g"\nx = \'y\'\n( p ) > /q' "$(__RawFidelity__class__quoted)"
check 'hyphenated words, --, ANSI-C quoting and bare assignments survive' $'send-keys|--|--flag|\t|b|unsettled|' "$(__RawFidelity__class__words)"
check 'array appends and nested quotes in expansions survive' '2|/post|' "$(__RawFidelity__class__arrays)"
check 'a single = test compares text' 'eq|word' "$(__RawFidelity__class__patterns_ x)"
check 'bracket globs keep negation and ranges' 'ne|word ne|digit ne|number' \
    "$(__RawFidelity__class__patterns_ ab) $(__RawFidelity__class__patterns_ 7) $(__RawFidelity__class__patterns_ 42)"
check 'quoted heredocs stay literal and keep their pipe' '  $HOME  STAYS  LITERAL' "$(__RawFidelity__class__heredoc)"
check 'arithmetic still runs' '6' "$(__RawFidelity__class__code)"
