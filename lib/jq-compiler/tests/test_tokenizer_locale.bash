#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# ==============================================================================
# Tokenizer locale, multibyte, and scaling tests
# ==============================================================================
# The scanner indexes a character array under LC_ALL=C. Decoded tokens must not
# depend on the caller's locale, multibyte text must survive byte-for-byte with
# character-based columns, and cost must stay linear in source size: the
# substring scan it replaced needed about 24 s of CPU for the 50 KB Trash.trash
# in either locale.
# ==============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.bash"

TOKENIZER="$COMPILER_DIR/tokenizer.bash"
TEST_TMP=$(mktemp -d)
trap 'rm -rf "$TEST_TMP"; print_test_summary' EXIT

# The suite pins LC_ALL=C; find a UTF-8 locale to stand in for an interactive
# build environment.
utf8_locale=''
for candidate in en_US.UTF-8 C.UTF-8 C.utf8 en_US.utf8; do
    if locale -a 2>/dev/null | grep -qx "$candidate"; then utf8_locale=$candidate; break; fi
done
tokenize_utf8() {
    env -u LC_ALL LANG="$utf8_locale" LC_CTYPE="$utf8_locale" "$TOKENIZER" "$@"
}

fixture="$TEST_TMP/Unicode.trash"
{
    printf '%s\n' '# Ünïcödé comment 雪'
    printf '%s\n' 'Unicode subclass: Object'
    printf '%s\n' "  instanceVars: label:'héllo'"
    printf '%s\n' '  method: greet: name ['
    printf '%s\n' '    | text |'
    printf '%s\n' "    text := 'héllo 雪 ' , name."
    printf '%s\n' '    text := text , "wörld".'
    printf '%s\n' '    ^ text'
    printf '%s\n' '  ]'
} > "$fixture"

echo "Testing locale independence..."
tokens_c=$(LC_ALL=C "$TOKENIZER" "$fixture")
run_test "C locale tokenizes multibyte source" 0 "$?"
if [[ -n "$utf8_locale" ]]; then
    tokens_utf8=$(tokenize_utf8 "$fixture")
    run_test "UTF-8 locale tokenizes multibyte source" 0 "$?"
    run_test "decoded tokens do not depend on the locale ($utf8_locale)" \
        "$(jq -Sc . <<<"$tokens_c" | shasum)" "$(jq -Sc . <<<"$tokens_utf8" | shasum)"
else
    echo "  (no UTF-8 locale available; locale comparison skipped)"
fi

echo "Testing multibyte token values and character columns..."
run_test "comment keeps multibyte text" '# Ünïcödé comment 雪' \
    "$(jq -r '[.[] | select(.type == "COMMENT")][0].value' <<<"$tokens_c")"
run_test "string keeps multibyte text" "'héllo 雪 '" \
    "$(jq -r '[.[] | select(.type == "STRING")][1].value' <<<"$tokens_c")"
run_test "double-quoted string keeps multibyte text" '"wörld"' \
    "$(jq -r '[.[] | select(.type == "DSTRING")][0].value' <<<"$tokens_c")"
run_test "columns count characters, not bytes" 23 \
    "$(jq -r '.[] | select(.type == "COMMA" and .line == 6) | .col' <<<"$tokens_c")"
run_test "a bare multibyte character is one LITERAL token" 'IDENTIFIER,ASSIGN,LITERAL,IDENTIFIER' \
    "$(printf 'x := ⊕ y\n' | LC_ALL=C "$TOKENIZER" | jq -r '[.[] | select(.type != "NEWLINE") | .type] | join(",")')"
run_test "LITERAL value is the whole character" '⊕' \
    "$(printf 'x := ⊕ y\n' | LC_ALL=C "$TOKENIZER" | jq -r '.[] | select(.type == "LITERAL") | .value')"
run_test "token after a multibyte character keeps its character column" 7 \
    "$(printf 'x := ⊕ y\n' | LC_ALL=C "$TOKENIZER" | jq -r '.[] | select(.value == "y") | .col')"
run_test "identifier classes are ASCII in every locale" 'IDENTIFIER:caf,LITERAL:é' \
    "$(printf 'café\n' | LC_ALL=C "$TOKENIZER" | jq -r '[.[] | select(.type != "NEWLINE") | "\(.type):\(.value)"] | join(",")')"
if [[ -n "$utf8_locale" ]]; then
    run_test "identifier classes are ASCII under UTF-8 too" 'IDENTIFIER:caf,LITERAL:é' \
        "$(printf 'café\n' | tokenize_utf8 | jq -r '[.[] | select(.type != "NEWLINE") | "\(.type):\(.value)"] | join(",")')"
fi

echo "Testing scan status..."
# The driver strips trailing newlines, so a source whose last line is a single
# character at column 0 (a closing brace) ends the scan on ((col++)) from 0.
# That incidental status must not fail the scan (Kube/Cluster.trash regression).
tokens_brace=$(printf 'x := 1\n}' | LC_ALL=C "$TOKENIZER")
run_test "source ending in a column-0 token tokenizes" 0 "$?"
run_test "source ending in a column-0 token yields every token" 5 "$(jq length <<<"$tokens_brace")"

echo "Testing scan cost on the largest class..."
large="$COMPILER_DIR/../../trash/Trash.trash"
if [[ -f "$large" && -n "${EPOCHREALTIME:-}" ]]; then
    if [[ -n "$utf8_locale" ]]; then scan() { tokenize_utf8 "$large"; }; else scan() { LC_ALL=C "$TOKENIZER" "$large"; }; fi
    start=${EPOCHREALTIME/./}
    count=$(scan | jq length)
    elapsed_ms=$(( (${EPOCHREALTIME/./} - start) / 1000 ))
    run_test "Trash.trash produces a full token stream" true "$([[ "$count" -gt 5000 ]] && echo true || echo "false ($count tokens)")"
    # The replaced substring scan needed more than 20 s even on an idle host;
    # the array scan takes about one second, so this bound survives a loaded
    # parallel test run while still failing for a quadratic regression.
    run_test "Trash.trash tokenizes in bounded time (${elapsed_ms} ms)" true \
        "$([[ "$elapsed_ms" -lt 20000 ]] && echo true || echo "false (${elapsed_ms} ms)")"
else
    echo "  (largest class or EPOCHREALTIME unavailable; timing skipped)"
fi
