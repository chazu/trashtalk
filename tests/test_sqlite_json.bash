#!/usr/bin/env bash

# Test suite for sqlite-json.bash

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/vendor/sqlite-json.bash"

# Use a test-specific database
export SQLITE_JSON_DB="/tmp/test_sqlite_json_$$.db"

PASSED=0
FAILED=0

pass() {
    echo "  PASS: $1"
    ((PASSED++))
}

fail() {
    echo "  FAIL: $1"
    ((FAILED++))
}

assert_eq() {
    local expected="$1"
    local actual="$2"
    local msg="$3"
    if [[ "$expected" == "$actual" ]]; then
        pass "$msg"
    else
        fail "$msg (expected '$expected', got '$actual')"
    fi
}

assert_contains() {
    local needle="$1"
    local haystack="$2"
    local msg="$3"
    if [[ "$haystack" == *"$needle"* ]]; then
        pass "$msg"
    else
        fail "$msg (expected to contain '$needle')"
    fi
}

assert_exit_code() {
    local expected="$1"
    local actual="$2"
    local msg="$3"
    if [[ "$expected" == "$actual" ]]; then
        pass "$msg"
    else
        fail "$msg (expected exit $expected, got $actual)"
    fi
}

cleanup() {
    rm -f "$SQLITE_JSON_DB"
}

trap cleanup EXIT

echo "=== sqlite-json.bash Test Suite ==="
echo "Database: $SQLITE_JSON_DB"
echo ""

# ==========================================
echo "1. Initialization"
# ==========================================

db_init
assert_exit_code 0 $? "db_init succeeds"

columns=$(db_list_columns)
assert_contains "id" "$columns" "has id column"
assert_contains "data" "$columns" "has data column"
assert_contains "class" "$columns" "has class virtual column"
assert_contains "created_at" "$columns" "has created_at virtual column"

indices=$(db_list_indices)
assert_contains "idx_instances_class" "$indices" "has class index"

echo ""

# ==========================================
echo "2. Basic CRUD"
# ==========================================

db_put "counter_001" '{"class":"Counter","value":10,"created_at":"2024-01-01"}'
assert_exit_code 0 $? "db_put succeeds"

result=$(db_get "counter_001")
assert_contains '"class":"Counter"' "$result" "db_get returns correct data"
assert_contains '"value":10' "$result" "db_get returns value field"

db_put "counter_001" '{"class":"Counter","value":20,"created_at":"2024-01-01"}'
result=$(db_get "counter_001")
assert_contains '"value":20' "$result" "db_put updates existing record"

db_delete "counter_001"
result=$(db_get "counter_001")
assert_eq "" "$result" "db_delete removes record"

echo ""

# ==========================================
echo "3. Queries"
# ==========================================

db_put "counter_a" '{"class":"Counter","value":5,"created_at":"2024-01-01"}'
db_put "counter_b" '{"class":"Counter","value":15,"created_at":"2024-01-02"}'
db_put "counter_c" '{"class":"Counter","value":25,"created_at":"2024-01-03"}'
db_put "array_a" '{"class":"Array","items":[],"created_at":"2024-01-04"}'

result=$(db_find_by_class "Counter")
assert_contains "counter_a" "$result" "db_find_by_class finds counter_a"
assert_contains "counter_b" "$result" "db_find_by_class finds counter_b"
assert_contains "counter_c" "$result" "db_find_by_class finds counter_c"

count=$(db_count_by_class "Counter")
assert_eq "3" "$count" "db_count_by_class returns 3"

count=$(db_count_by_class "Array")
assert_eq "1" "$count" "db_count_by_class Array returns 1"

classes=$(db_list_classes)
assert_contains "Counter" "$classes" "db_list_classes includes Counter"
assert_contains "Array" "$classes" "db_list_classes includes Array"

result=$(db_query "json_extract(data, '\$.value') > 10")
assert_contains "counter_b" "$result" "db_query finds counter_b (value 15)"
assert_contains "counter_c" "$result" "db_query finds counter_c (value 25)"

result=$(db_query_data "class = 'Counter' AND json_extract(data, '\$.value') >= 15")
assert_contains '"value":15' "$result" "db_query_data returns counter_b data"
assert_contains '"value":25' "$result" "db_query_data returns counter_c data"

echo ""

# ==========================================
echo "4. Virtual Columns and Indexes"
# ==========================================

db_ensure_virtual_column "value" '$.value'
assert_exit_code 0 $? "db_ensure_virtual_column succeeds"

columns=$(db_list_columns)
assert_contains "value" "$columns" "value virtual column created"

db_create_index "value"
assert_exit_code 0 $? "db_create_index succeeds"

indices=$(db_list_indices)
assert_contains "idx_instances_value" "$indices" "value index created"

echo ""

# ==========================================
echo "5. Security: ID Validation"
# ==========================================

db_put "valid_id-123.test:ok" '{"class":"Test"}' 2>/dev/null
assert_exit_code 0 $? "valid ID accepted"

db_put "invalid id spaces" '{"class":"Test"}' 2>/dev/null
assert_exit_code 1 $? "ID with spaces rejected"

db_put "injection'; DROP TABLE instances;--" '{"class":"Test"}' 2>/dev/null
assert_exit_code 1 $? "SQL injection in ID rejected"

db_put "" '{"class":"Test"}' 2>/dev/null
assert_exit_code 1 $? "empty ID rejected"

# Verify table still intact
count=$(db_count_by_class "Counter")
assert_eq "3" "$count" "table intact after injection attempts"

echo ""

# ==========================================
echo "6. Security: Quote Escaping"
# ==========================================

db_put "book_1" '{"class":"O'\''Reilly","title":"Bash Guide"}'
assert_exit_code 0 $? "JSON with apostrophe accepted"

result=$(db_find_by_class "O'Reilly")
assert_contains "book_1" "$result" "find by class with apostrophe works"

result=$(db_get "book_1")
assert_contains "O'Reilly" "$result" "data with apostrophe retrieved correctly"

echo ""

# ==========================================
echo "7. Column/Index Name Validation"
# ==========================================

db_ensure_virtual_column "valid_column" '$.test' 2>/dev/null
assert_exit_code 0 $? "valid column name accepted"

db_ensure_virtual_column "invalid-column" '$.test' 2>/dev/null
assert_exit_code 1 $? "column name with hyphen rejected"

db_ensure_virtual_column "injection; DROP TABLE" '$.test' 2>/dev/null
assert_exit_code 1 $? "SQL injection in column name rejected"

db_create_index "valid_column" 2>/dev/null
assert_exit_code 0 $? "valid index name accepted"

db_create_index "invalid-index" 2>/dev/null
assert_exit_code 1 $? "index name with hyphen rejected"

echo ""

# ==========================================
echo "8. Clear and Drop"
# ==========================================

db_clear
count=$(db_count_by_class "Counter")
assert_eq "0" "$count" "db_clear removes all data"

columns=$(db_list_columns)
assert_contains "class" "$columns" "db_clear preserves schema"

db_drop
[[ ! -f "$SQLITE_JSON_DB" ]]
assert_exit_code 0 $? "db_drop removes database file"

echo ""

# ==========================================
echo "=== Summary ==="
# ==========================================
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo ""

if [[ $FAILED -eq 0 ]]; then
    echo "All tests passed!"
    exit 0
else
    echo "Some tests failed."
    exit 1
fi
