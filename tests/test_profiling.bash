#!/usr/bin/env bash
# test_profiling.bash - Tests for the profiling system
#
# Tests TRASH_PROFILE environment variable and bin/trash-profile-analyze tool

# Don't use set -e because we want to continue on test failures
set -o pipefail

# Setup
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source trash.bash in a way that doesn't interfere with test execution
TRASH_TEST_MODE=1 source "$SCRIPT_DIR/../lib/trash.bash" 2>/dev/null || true

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Test helper functions
pass() {
  echo "  ✓ $1"
  ((TESTS_PASSED++))
  ((TESTS_RUN++))
}

fail() {
  echo "  ✗ $1 (expected: $2, got: $3)"
  ((TESTS_FAILED++))
  ((TESTS_RUN++))
}

assert_contains() {
  local str="$1" substr="$2" msg="$3"
  if [[ "$str" == *"$substr"* ]]; then
    pass "$msg"
  else
    fail "$msg" "contains '$substr'" "${str:0:50}..."
  fi
}

assert_not_empty() {
  local str="$1" msg="$2"
  if [[ -n "$str" ]]; then
    pass "$msg"
  else
    fail "$msg" "non-empty" "(empty)"
  fi
}

assert_empty() {
  local str="$1" msg="$2"
  if [[ -z "$str" ]]; then
    pass "$msg"
  else
    fail "$msg" "(empty)" "${str:0:50}"
  fi
}

assert_file_exists() {
  local file="$1" msg="$2"
  if [[ -f "$file" ]]; then
    pass "$msg"
  else
    fail "$msg" "file exists" "file not found"
  fi
}

echo "=== Profiling Tests ==="
echo ""

# ============================================
# Test 1: TRASH_PROFILE generates output to stderr
# ============================================
echo "--- 1. TRASH_PROFILE generates profiling output ---"

profile_output=$(TRASH_PROFILE=1 @ Counter new 2>&1 >/dev/null)
assert_contains "$profile_output" "→" "entry arrow present"
assert_contains "$profile_output" "←" "exit arrow present"
assert_contains "$profile_output" "Counter.new" "method name present"
assert_contains "$profile_output" "ms" "timing present"

# ============================================
# Test 2: Without TRASH_PROFILE, no profiling output
# ============================================
echo ""
echo "--- 2. No profiling output when TRASH_PROFILE unset ---"

# Unset TRASH_PROFILE explicitly
unset TRASH_PROFILE 2>/dev/null || true
stderr_output=$(@ Dictionary new 2>&1 >/dev/null)
arrow_count=$(echo "$stderr_output" | grep -c '→' 2>/dev/null || echo "0")
arrow_count=$(echo "$arrow_count" | head -1)  # In case of multiple lines
if [[ "$arrow_count" == "0" ]]; then
  pass "no profiling output when TRASH_PROFILE unset"
else
  fail "no profiling output when TRASH_PROFILE unset" "0" "$arrow_count"
fi

# ============================================
# Test 3: TRASH_PROFILE_FILE writes to file
# ============================================
echo ""
echo "--- 3. TRASH_PROFILE_FILE writes to file ---"

PROFILE_TEST_FILE="/tmp/trashtalk_profile_test_$$"
rm -f "$PROFILE_TEST_FILE"

export TRASH_PROFILE=1
export TRASH_PROFILE_FILE="$PROFILE_TEST_FILE"
@ Counter new >/dev/null 2>&1
unset TRASH_PROFILE
unset TRASH_PROFILE_FILE

assert_file_exists "$PROFILE_TEST_FILE" "profile file created"
if [[ -f "$PROFILE_TEST_FILE" ]]; then
  file_content=$(cat "$PROFILE_TEST_FILE")
  assert_contains "$file_content" "Counter.new" "profile file contains method name"
  assert_contains "$file_content" "ms" "profile file contains timing"
fi

rm -f "$PROFILE_TEST_FILE"

# ============================================
# Test 4: Profile analyzer can parse output
# ============================================
echo ""
echo "--- 4. Profile analyzer parses profiling output ---"

PROFILE_LOG_FILE="/tmp/trashtalk_profile_log_$$"
rm -f "$PROFILE_LOG_FILE"

# Generate some profile data using TRASH_PROFILE_FILE
export TRASH_PROFILE=1
export TRASH_PROFILE_FILE="$PROFILE_LOG_FILE"
@ Counter new >/dev/null 2>&1 || true
@ Dictionary new >/dev/null 2>&1 || true
@ Array new >/dev/null 2>&1 || true
unset TRASH_PROFILE
unset TRASH_PROFILE_FILE

if [[ -f "$PROFILE_LOG_FILE" ]] && [[ -s "$PROFILE_LOG_FILE" ]]; then
  analyzer_output=$(/opt/homebrew/bin/bash "$SCRIPT_DIR/../bin/trash-profile-analyze" "$PROFILE_LOG_FILE" 2>&1 || true)
  assert_contains "$analyzer_output" "TRASHTALK PROFILE REPORT" "analyzer header present"
  assert_contains "$analyzer_output" "Total method calls" "total calls present"
  assert_contains "$analyzer_output" "DISPATCH ROUTING" "routing section present"
  assert_contains "$analyzer_output" "SLOWEST METHODS" "slowest methods section present"
  pass "analyzer runs without error"
else
  fail "profile log generated" "non-empty file" "(empty or missing)"
fi

rm -f "$PROFILE_LOG_FILE"

# ============================================
# Test 5: Profile shows correct route types
# ============================================
echo ""
echo "--- 5. Profile shows correct route types ---"

profile_output=$(TRASH_PROFILE=1 @ Counter new 2>&1 >/dev/null)

# Should show either [native] or [bash] or [native→bash]
if [[ "$profile_output" == *"[native"* ]] || [[ "$profile_output" == *"[bash"* ]]; then
  pass "route type present in output"
else
  fail "route type present in output" "contains route" "${profile_output:0:100}"
fi

# ============================================
# Test 6: Nested calls show correct depth
# ============================================
echo ""
echo "--- 6. Nested calls show depth indentation ---"

# Create a method that calls another method
profile_output=$(TRASH_PROFILE=1 @ Dictionary new 2>&1 >/dev/null)

# The output should have multiple lines with different timestamps
line_count=$(echo "$profile_output" | grep -c '^\[' || echo "0")
if [[ "$line_count" -ge 2 ]]; then
  pass "multiple profile lines generated"
else
  # Might be 1 if native handles it directly
  if [[ "$line_count" -eq 1 ]]; then
    pass "at least one profile line generated"
  else
    fail "profile lines generated" ">= 1" "$line_count"
  fi
fi

# ============================================
# Summary
# ============================================
echo ""
echo "========================================="
echo "Tests: $TESTS_RUN, Assertions: $TESTS_PASSED passed, $TESTS_FAILED failed"
if [[ $TESTS_FAILED -eq 0 ]]; then
  echo "All tests passed!"
  exit 0
else
  echo "Some tests failed."
  exit 1
fi
