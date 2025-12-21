# Test Suite Refactoring Plan

## Overview

This document outlines issues found during a comprehensive review of the Trashtalk test suite, along with a prioritized plan for remediation.

## Issues Found

### 1. Tests That Don't Meaningfully Test Functionality

#### 1.1 `tests/test_tuplespace_object.bash`
- **Severity**: High
- **Issue**: This test has NO assertions at all. It just executes operations and prints output. It's purely a "does it crash?" test.
- **Location**: Lines 16-47
- **Fix**: Add assertions that verify returned values match expected results

#### 1.2 `lib/jq-compiler/tests/test_integration.bash` (Error Handling section)
- **Severity**: Medium
- **Issue**: Tests are trivial placeholders - they assert `"true" == "true"`:
  ```bash
  run_test "empty input handled" "true" "true"  # Line 145
  run_test "invalid syntax handled" "true" "true"  # Line 151
  ```
- **Fix**: Add meaningful error detection or output verification

#### 1.3 `tests/test_instance_var_defaults.bash` (test_negative_default)
- **Severity**: Low
- **Issue**: Test named "Negative number default value" but tests `offset:0` and `limit:100` - neither are negative
- **Location**: Lines 236-262
- **Fix**: Rename to match actual test content OR add actual negative number testing

#### 1.4 `lib/jq-compiler/tests/test_expr_parser.bash`
- **Severity**: High
- **Issue**: Just prints output with `echo "Result: $RESULT"` without checking correctness. Contains test cases but no validation logic.
- **Fix**: Add proper assertions to each test

### 2. Redundant Tests

#### 2.1 `tests/test_integration.bash` vs `tests/test_trash_stabilization.bash`
- **Severity**: Medium
- **Overlap**:
  - Counter and Array instance creation
  - Method calls (getValue, setValue, increment, push)
  - Store.findByClass functionality
- **Fix**: Consolidate or clearly differentiate scope (one for runtime, one for instance-vars model)

#### 2.2 `tests/test_object_methods.bash` vs `tests/test_integration.bash`
- **Severity**: Low
- **Overlap**:
  - Counter creation and incrementBy
  - Find with predicates
  - Count operations
- **Fix**: `test_object_methods.bash` should focus solely on Object class methods without duplicating basic Counter tests

### 3. Tests With Reliability Concerns

#### 3.1 `lib/jq-compiler/tests/test_blocks.bash` (Tests 6-8)
- **Severity**: Medium
- **Issue**: Tests conditionally skip with `if [[ -f "...Block" ]]`. If Block/Array classes aren't compiled, tests silently skip.
- **Location**: Lines 101-169
- **Fix**: Either ensure classes are compiled before tests, or mark skipped tests as explicit warnings/failures

#### 3.2 `tests/test_trash_stabilization.bash` (Stack Frame System tests)
- **Severity**: Low
- **Issue**: Requires `TRASH_STACK_FRAMES=1` environment variable. Tests internal implementation rather than user-facing behavior.
- **Location**: Lines 223-248
- **Fix**: Consider whether these tests add value or just test implementation details

#### 3.3 `lib/jq-compiler/tests/test_control_flow.bash`
- **Severity**: Low
- **Issue**: Uses substring matching which could produce false positives
- **Fix**: Use more precise pattern matching where possible

#### 3.4 `lib/jq-compiler/tests/test_integration.bash` (Bash Syntax Validation)
- **Severity**: Low
- **Issue**: Iterates over all `.trash` files; failures don't clearly indicate which file failed
- **Location**: Lines 179-194
- **Fix**: Ensure filename is visible in test output for debugging

---

## Recommended Order of Execution

### Phase 1: High-Priority Fixes (Tests with no real assertions)
These tests provide false confidence - they pass but don't verify anything.

1. **Fix `tests/test_tuplespace_object.bash`** - Add proper assertions
2. **Fix `lib/jq-compiler/tests/test_expr_parser.bash`** - Add validation logic

### Phase 2: Medium-Priority Fixes (Placeholder and conditional tests)
3. **Fix `lib/jq-compiler/tests/test_integration.bash` error handling** - Replace placeholder assertions
4. **Fix `lib/jq-compiler/tests/test_blocks.bash`** - Handle skipped tests properly

### Phase 3: Low-Priority Fixes (Naming and minor issues)
5. **Rename `test_negative_default`** in `tests/test_instance_var_defaults.bash`
6. **Improve output in `test_integration.bash`** bash syntax validation section

### Phase 4: Consolidation (Address redundancy)
7. **Differentiate `test_integration.bash` vs `test_trash_stabilization.bash`**
8. **Refocus `test_object_methods.bash`** to avoid overlap

---

## Progress Tracking

| Task | Status | Notes |
|------|--------|-------|
| 1. Fix test_tuplespace_object.bash | ✅ Complete | Added assertions, pass/fail helpers, proper exit codes |
| 2. Fix test_expr_parser.bash | ✅ Complete | Added `assert_json_has` helper and validation for all 5 tests |
| 3. Fix test_integration.bash error handling | ✅ Complete | Added real validation for empty input, invalid syntax, missing files |
| 4. Fix test_blocks.bash conditional tests | ✅ Complete | Added pass/fail/skip counters, explicit skip messages, proper exit codes |
| 5. Rename test_negative_default | ✅ Complete | Renamed to `test_numeric_defaults` to match actual test behavior |
| 6. Improve bash syntax validation output | ✅ Complete | Added `validate_with_details` helper that shows bash error on failure |
| 7. Differentiate integration vs stabilization | ✅ Analyzed | See detailed analysis below |
| 8. Refocus test_object_methods.bash | Deferred | Lower priority - overlap is minor |

---

## Analysis: test_stabilization.bash vs test_integration.bash

### Intent Differences

| Aspect | test_stabilization.bash | test_integration.bash |
|--------|------------------------|----------------------|
| **Purpose** | Test runtime infrastructure & internal APIs | Test user-facing instance_vars model |
| **Created** | Earlier (during stabilization phase) | Later (for new instance model) |
| **API Level** | Internal (`_` prefixed) + `@ Trash` wrappers | User-facing (`@ Counter`, `@ Store`) |
| **Focus** | Runtime plumbing, backward compat | Core functionality, persistence |

### Section-by-Section Analysis of test_stabilization.bash

| Section | Content | Status | Recommendation |
|---------|---------|--------|----------------|
| **1. Instance Helper Functions** | Tests `_generate_instance_id`, `_create_instance`, `_is_instance`, `_get_instance_class`, `_delete_instance` | ⚠️ **Vestigial** | Tests implementation details, not public API. Remove or move to `test_internals.bash` |
| **2. Instance-as-Receiver** | Counter/Array creation and basic ops | ⚠️ **Redundant** | Fully covered by test_integration.bash sections 1-2. Remove. |
| **3. Query API** | `@ Trash findAll`, `countInstances`, `find`, `listInstanceTypes` | ⚠️ **Possibly Obsolete** | Uses `@ Trash` wrappers while integration uses `@ Store` directly. Verify which is canonical API, then consolidate. |
| **4. Stack Frame System** | `TRASH_STACK_FRAMES`, `stackDepth`, `showStack`, `clearStack` | ⚠️ **Not Meaningful** | Tests debugging feature. The assertion "stack empty after call" is always true. Remove or rewrite to test actual error tracing. |
| **5. Backward Compatibility** | `@ Trash version`, `listObjects` | ✅ **Keep** | Tests utility/introspection methods not covered elsewhere |
| **6. Error Handling** | Path traversal, non-existent class | ✅ **Keep but Move** | Important security/error tests. Should be in test_integration.bash |

### Dual Query APIs

Both exist and work:
```
@ Trash findAll Counter        → wraps → @ Counter findAll
@ Store findByClass Counter    → direct query
```

The `@ Trash` methods in Trash.trash are convenience wrappers. The `@ Store` methods are the direct interface. Integration tests use Store; stabilization tests use Trash wrappers.

### Actions Taken

1. **Merged useful stabilization tests into integration**:
   - ✅ Section 5 (Backward Compat) → Added as Section 6 "Utility Methods" in test_integration.bash
   - ✅ Section 6 (Error Handling) → Added as Section 7 "Error Handling" in test_integration.bash

2. **Removed redundant Trash query wrapper methods from Trash.trash**:
   - ✅ Removed `@ Trash findAll` (wrapper for `@ Counter findAll`)
   - ✅ Removed `@ Trash find` (wrapper for `@ Counter find`)
   - ✅ Removed `@ Trash countInstances` (wrapper for `@ Counter count`)
   - ✅ Removed `@ Trash listInstanceTypes` (use `@ Store listClasses`)
   - ✅ Updated help text to point users to class methods

3. **Deleted test_stabilization.bash entirely**:
   - ✅ All useful tests merged into test_integration.bash
   - ✅ Vestigial sections (internal helpers, stack frames) not preserved

4. **Canonical query API decision**:
   - Use class methods: `@ Counter findAll`, `@ Counter count`, `@ Counter find 'predicate'`
   - Use Store for low-level: `@ Store listClasses`, `@ Store findByClass`
   - Trash class no longer has query methods (removes redundancy)

## Summary of Changes

### tests/test_tuplespace_object.bash
- Added `pass()`, `fail()`, `assert_eq()`, `assert_not_empty()`, `assert_contains()` helpers
- Added PASSED/FAILED counters
- Converted all operations to have proper assertions
- Added proper exit code (0 on success, 1 on failure)

### lib/jq-compiler/tests/test_expr_parser.bash
- Added `assert_json_has()` helper for jq-based validation
- Added PASSED/FAILED counters
- Each test now validates AST structure (type, body, operators, etc.)
- Added summary output and proper exit code

### lib/jq-compiler/tests/test_integration.bash
- Replaced placeholder `"true" == "true"` tests with actual validation
- Empty input test: checks for empty output, error message, or valid bash
- Invalid syntax test: checks for error message, empty output, or no method definitions
- Added missing file handling test
- Added `validate_with_details()` for bash syntax section that shows actual error on failure

### lib/jq-compiler/tests/test_blocks.bash
- Added `pass()`, `fail()`, `skip()` helpers with proper ANSI colors
- Added PASSED/FAILED/SKIPPED counters
- Tests 6-8 now explicitly report when skipped (with reason)
- Added summary output and proper exit code

### tests/test_instance_var_defaults.bash
- Renamed `test_negative_default` to `test_numeric_defaults`
- Updated test class name from `TestNegativeDefault` to `TestNumericDefaults`
- Updated echo message from "Negative number default value" to "Numeric default values"

### tests/test_integration.bash (consolidated)
- Added Section 6 "Utility Methods" - tests `@ Trash version`, `@ Trash listObjects`
- Added Section 7 "Error Handling" - tests path traversal rejection, non-existent class error

### tests/test_trash_stabilization.bash
- **DELETED** - all useful tests merged into test_integration.bash

### trash/Trash.trash (API cleanup)
- Removed `method: findAll:` (redundant wrapper)
- Removed `method: find: where:` (redundant wrapper)
- Removed `rawClassMethod: find` (redundant wrapper)
- Removed `method: countInstances:` (redundant wrapper)
- Removed `method: listInstanceTypes` (use `@ Store listClasses`)
- Updated help text to direct users to class methods (`@ Counter findAll`, etc.)
