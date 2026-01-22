#!/usr/bin/env bash
# Tests for source hash embedding in compiled classes
# Note: Source code embedding (__source function) was removed to reduce compiled file size.
# Only sourceHash is now embedded for cache invalidation.

cd "$(dirname "$0")/.."
source lib/trash.bash 2>/dev/null

PASSED=0
FAILED=0

pass() { echo "  ✓ $1"; ((PASSED++)); }
fail() { echo "  ✗ $1"; ((FAILED++)); }

echo "=== Source Hash Tests ==="

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

# --- Test 3: Hash matches actual source content ---
echo ""
echo "Test 3: Hash matches source file content"

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

# --- Test 4: Trash hashFor: method works ---
echo ""
echo "Test 4: @ Trash hashFor: Counter works"

hash_via_trash=$(@ Trash hashFor: Counter 2>/dev/null)
if [[ "$hash_via_trash" =~ ^[a-f0-9]{64}$ ]]; then
  pass "hashFor: returns valid hash"
else
  fail "hashFor: did not return valid hash: $hash_via_trash"
fi

# --- Test 5: hashFor: matches embedded hash ---
echo ""
echo "Test 5: hashFor: matches embedded sourceHash"

if [[ "$hash_via_trash" == "$__Counter__sourceHash" ]]; then
  pass "hashFor: matches embedded hash"
else
  fail "hashFor: mismatch: method=$hash_via_trash, var=$__Counter__sourceHash"
fi

# --- Test 6: Traits also have sourceHash ---
echo ""
echo "Test 6: Traits have sourceHash"

source trash/.compiled/traits/Persistable 2>/dev/null
if [[ -n "$__Persistable__sourceHash" ]]; then
  pass "Trait Persistable has sourceHash"
else
  fail "Trait missing sourceHash"
fi

# --- Test 7: Trait sourceHash is valid format ---
echo ""
echo "Test 7: Trait sourceHash is valid SHA-256"

if [[ "$__Persistable__sourceHash" =~ ^[a-f0-9]{64}$ ]]; then
  pass "Trait sourceHash is valid SHA-256 format"
else
  fail "Trait sourceHash is not valid: $__Persistable__sourceHash"
fi

# --- Cleanup ---
echo ""
echo "================================"
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
