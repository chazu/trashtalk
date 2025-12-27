#!/usr/bin/env bash
# Tests for object reference helpers (_ivar_ref, _ivar_set_ref, etc.)

cd "$(dirname "$0")/.."
source lib/trash.bash 2>/dev/null

PASSED=0
FAILED=0

pass() { echo "  ✓ $1"; ((PASSED++)); }
fail() { echo "  ✗ $1"; ((FAILED++)); }

echo "=== Object Reference Tests ==="

# --- Setup: Create test objects ---
echo ""
echo "Setup: Creating test objects..."

# Create test instances
counter1=$(@ Counter new)
counter2=$(@ Counter new)

if [[ -z "$counter1" || -z "$counter2" ]]; then
  echo "FATAL: Could not create test objects"
  exit 1
fi

echo "  Created: $counter1, $counter2"

# Set some values so we can identify them
@ $counter1 increment
@ $counter1 increment
@ $counter2 increment

# Create a holder object to store references
test_holder=$(@ Counter new)

# --- Test 1: _ivar_ref returns stored reference ---
echo ""
echo "Test 1: _ivar_ref returns stored reference"

# Set _RECEIVER to simulate being inside a method of test_holder
_RECEIVER="$test_holder"
_ivar_set "myRef" "$counter1"
retrieved=$(_ivar_ref "myRef")

if [[ "$retrieved" == "$counter1" ]]; then
  pass "_ivar_ref returns the stored reference"
else
  fail "_ivar_ref returned '$retrieved', expected '$counter1'"
fi

# --- Test 2: _ivar_ref_valid returns true for valid reference ---
echo ""
echo "Test 2: _ivar_ref_valid returns true for valid reference"

if _ivar_ref_valid "myRef"; then
  pass "_ivar_ref_valid returns true for existing instance"
else
  fail "_ivar_ref_valid returned false for valid reference"
fi

# --- Test 3: _ivar_ref_valid returns false for invalid reference ---
echo ""
echo "Test 3: _ivar_ref_valid returns false for invalid reference"

_ivar_set "badRef" "nonexistent_abc123"
if _ivar_ref_valid "badRef"; then
  fail "_ivar_ref_valid returned true for non-existent instance"
else
  pass "_ivar_ref_valid returns false for non-existent instance"
fi

# --- Test 4: _ivar_ref_valid returns false for empty reference ---
echo ""
echo "Test 4: _ivar_ref_valid returns false for empty reference"

_ivar_set "emptyRef" ""
if _ivar_ref_valid "emptyRef"; then
  fail "_ivar_ref_valid returned true for empty reference"
else
  pass "_ivar_ref_valid returns false for empty reference"
fi

# --- Test 5: _ivar_ref_class returns correct class ---
echo ""
echo "Test 5: _ivar_ref_class returns correct class"

class=$(_ivar_ref_class "myRef")
if [[ "$class" == "Counter" ]]; then
  pass "_ivar_ref_class returns 'Counter'"
else
  fail "_ivar_ref_class returned '$class', expected 'Counter'"
fi

# --- Test 6: _ivar_send sends message to referenced object ---
echo ""
echo "Test 6: _ivar_send sends message to referenced object"

# counter1 was incremented twice, so getValue should return 2
value=$(_ivar_send "myRef" getValue)
if [[ "$value" == "2" ]]; then
  pass "_ivar_send correctly sends message to referenced object"
else
  fail "_ivar_send returned '$value', expected '2'"
fi

# --- Test 7: _ivar_set_ref validates reference exists ---
echo ""
echo "Test 7: _ivar_set_ref accepts valid reference"

if _ivar_set_ref "validatedRef" "$counter2" 2>/dev/null; then
  retrieved=$(_ivar_ref "validatedRef")
  if [[ "$retrieved" == "$counter2" ]]; then
    pass "_ivar_set_ref accepts valid reference"
  else
    fail "_ivar_set_ref stored wrong value"
  fi
else
  fail "_ivar_set_ref rejected valid reference"
fi

# --- Test 8: _ivar_set_ref rejects invalid reference ---
echo ""
echo "Test 8: _ivar_set_ref rejects invalid reference"

error_output=$(_ivar_set_ref "shouldFail" "fake_instance_xyz" 2>&1)
status=$?

if [[ $status -ne 0 ]]; then
  pass "_ivar_set_ref rejects non-existent reference (exit code $status)"
else
  fail "_ivar_set_ref accepted invalid reference"
fi

# --- Test 9: _ivar_set_ref allows clearing reference ---
echo ""
echo "Test 9: _ivar_set_ref allows clearing reference"

_ivar_set_ref "clearableRef" "$counter1" 2>/dev/null
if _ivar_set_ref "clearableRef" "" 2>/dev/null; then
  cleared=$(_ivar_ref "clearableRef")
  if [[ -z "$cleared" ]]; then
    pass "_ivar_set_ref allows clearing with empty string"
  else
    fail "_ivar_set_ref did not clear reference (got '$cleared')"
  fi
else
  fail "_ivar_set_ref rejected empty string for clearing"
fi

# --- Test 10: References persist to database ---
echo ""
echo "Test 10: References persist to database"

# Set a reference and save to store
_ivar_set_ref "persistRef" "$counter1" 2>/dev/null
@ $test_holder save

# Clear the in-memory environment cache
_env_delete "$test_holder"

# Now reload and check - _ivar should auto-load from Store
_RECEIVER="$test_holder"
ref_after_reload=$(_ivar_ref "persistRef")
if [[ "$ref_after_reload" == "$counter1" ]]; then
  pass "Reference persists to Store and reloads"
else
  fail "Reference lost after reload: got '$ref_after_reload', expected '$counter1'"
fi

# --- Cleanup ---
echo ""
echo "Cleanup: Deleting test objects..."
@ $counter1 delete 2>/dev/null
@ $counter2 delete 2>/dev/null
@ $test_holder delete 2>/dev/null

echo ""
echo "================================"
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
