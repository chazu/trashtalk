#!/usr/bin/env bash
# ==============================================================================
# PASS/FAIL tally and compile helpers for compiler tests
# ==============================================================================
# Source after `cd "$COMPILER_DIR"`: the compile helpers run ./driver.bash.
# This is the counting style; helper.bash is the run_test style with an EXIT
# trap summary. A test uses one or the other.
# ==============================================================================

PASS=0
FAIL=0

pass() {
  echo "  PASS: $1"
  ((PASS++))
}

fail() {
  echo "  FAIL: $1"
  echo "    Expected: $2"
  echo "    Got:      $3"
  ((FAIL++))
}

# Compile a class source and print one method's body, unindented.
compile_method() {
  local source="$1"
  local method="$2"
  local tmpfile
  tmpfile=$(mktemp)
  echo "$source" > "$tmpfile"
  local output
  output=$(timeout 30 ./driver.bash compile "$tmpfile" 2>/dev/null)
  rm -f "$tmpfile"
  echo "$output" | awk "/^__.*__${method}\(\)/,/^\}/" | tail -n +2 | sed '$d' | sed 's/^  //'
}

# Print "true" if a class source compiles without error, "false" otherwise.
compiles_ok() {
  local source="$1"
  local tmpfile
  tmpfile=$(mktemp)
  echo "$source" > "$tmpfile"
  if timeout 30 ./driver.bash compile "$tmpfile" >/dev/null 2>&1; then
    rm -f "$tmpfile"
    echo "true"
  else
    rm -f "$tmpfile"
    echo "false"
  fi
}

# Compile a class source and print the full generated output.
compile_full() {
  local source="$1"
  local tmpfile
  tmpfile=$(mktemp)
  echo "$source" > "$tmpfile"
  timeout 30 ./driver.bash compile "$tmpfile" 2>/dev/null
  rm -f "$tmpfile"
}

# Print the tally under "=== <title> ===" (default Results); exit 1 on any FAIL.
print_results() {
  echo ""
  echo "=== ${1:-Results} ==="
  echo "Passed: $PASS"
  echo "Failed: $FAIL"

  if ((FAIL > 0)); then
    exit 1
  fi
}
