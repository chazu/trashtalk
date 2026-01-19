#!/usr/bin/env bash
# Tests for source code and hash embedding in compiled classes

cd "$(dirname "$0")/.."
source lib/trash.bash 2>/dev/null

PASSED=0
FAILED=0

pass() { echo "  ✓ $1"; ((PASSED++)); }
fail() { echo "  ✗ $1"; ((FAILED++)); }

echo "=== Source Embedding Tests ==="

# --- Test 1: Compiled class has sourceHash variable ---
echo ""
echo "Test 1: Compiled class has sourceHash variable"

if [[ -n "$__Counter__sourceHash" ]]; then
  pass "Counter has sourceHash variable"
else
  # Try loading it
  source trash/.compiled/Counter 2>/dev/null
  if [[ -n "$__Counter__sourceHash" ]]; then
    pass "Counter has sourceHash variable (after loading)"
  else
    fail "Counter missing sourceHash variable"
  fi
fi

# --- Test 2: sourceHash is valid SHA-256 (64 hex chars) ---
echo ""
echo "Test 2: sourceHash is valid SHA-256 format"

hash="$__Counter__sourceHash"
if [[ "$hash" =~ ^[a-f0-9]{64}$ ]]; then
  pass "sourceHash is valid SHA-256 format (64 hex chars)"
else
  fail "sourceHash is not valid SHA-256: $hash"
fi

# --- Test 3: Compiled class has __source function ---
echo ""
echo "Test 3: Compiled class has __source function"

if declare -f __Counter__source >/dev/null 2>&1; then
  pass "Counter has __source function"
else
  fail "Counter missing __source function"
fi

# --- Test 4: __source function returns source code ---
echo ""
echo "Test 4: __source function returns valid source code"

source_code=$(__Counter__source 2>/dev/null)
if [[ "$source_code" == *"Counter subclass:"* ]]; then
  pass "__source returns source containing class definition"
else
  fail "__source did not return expected source code"
fi

# --- Test 5: Hash matches actual source content ---
echo ""
echo "Test 5: Hash matches source file content"

source_file="trash/Counter.trash"
if [[ -f "$source_file" ]]; then
  expected_hash=$(shasum -a 256 "$source_file" | cut -d' ' -f1)
  if [[ "$__Counter__sourceHash" == "$expected_hash" ]]; then
    pass "Embedded hash matches source file hash"
  else
    fail "Hash mismatch: embedded=$__Counter__sourceHash, file=$expected_hash"
  fi
else
  fail "Source file not found: $source_file"
fi

# --- Test 6: Trash sourceFor: method works ---
echo ""
echo "Test 6: @ Trash sourceFor: Counter works"

source_via_trash=$(@ Trash sourceFor: Counter 2>/dev/null)
if [[ "$source_via_trash" == *"Counter subclass:"* ]]; then
  pass "sourceFor: returns source code"
else
  fail "sourceFor: did not return expected source"
fi

# --- Test 7: Trash hashFor: method works ---
echo ""
echo "Test 7: @ Trash hashFor: Counter works"

hash_via_trash=$(@ Trash hashFor: Counter 2>/dev/null)
if [[ "$hash_via_trash" =~ ^[a-f0-9]{64}$ ]]; then
  pass "hashFor: returns valid hash"
else
  fail "hashFor: did not return valid hash: $hash_via_trash"
fi

# --- Test 8: hashFor: matches embedded hash ---
echo ""
echo "Test 8: hashFor: matches embedded sourceHash"

if [[ "$hash_via_trash" == "$__Counter__sourceHash" ]]; then
  pass "hashFor: matches embedded hash"
else
  fail "hashFor: mismatch: method=$hash_via_trash, var=$__Counter__sourceHash"
fi

# --- Test 9: Traits also have source embedding ---
echo ""
echo "Test 9: Traits have source embedding"

source trash/.compiled/traits/Persistable 2>/dev/null
if [[ -n "$__Persistable__sourceHash" ]] && declare -f __Persistable__source >/dev/null 2>&1; then
  pass "Trait Persistable has sourceHash and __source"
else
  fail "Trait missing source embedding"
fi

# --- Test 10: sourceFor: works for non-loaded class ---
echo ""
echo "Test 10: sourceFor: works for class not yet loaded"

# Use a class we haven't touched yet
unset -f __Widget__source 2>/dev/null
source_widget=$(@ Trash sourceFor: Widget 2>/dev/null)
if [[ "$source_widget" == *"Widget subclass:"* ]]; then
  pass "sourceFor: loads and returns source for unloaded class"
else
  fail "sourceFor: failed for unloaded class"
fi

# --- Cleanup ---
echo ""
echo "================================"
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
