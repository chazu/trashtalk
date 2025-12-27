#!/usr/bin/env bash
# Tests for lazy schema migration when instance vars change

cd "$(dirname "$0")/.."
source lib/trash.bash 2>/dev/null

PASSED=0
FAILED=0

pass() { echo "  ✓ $1"; ((PASSED++)); }
fail() { echo "  ✗ $1"; ((FAILED++)); }

echo "=== Schema Migration Tests ==="

# Clean up any previous test instances
db_query "DELETE FROM objects WHERE id LIKE 'testmigration_%'" 2>/dev/null

# --- Test 1: Create instance with original schema ---
echo ""
echo "Test 1: Create instance with original schema"

# Create a test class with 2 fields
cat > /tmp/TestMigration.trash << 'EOF'
TestMigration subclass: Object
  include: Persistable
  instanceVars: fieldA fieldB
EOF

lib/jq-compiler/driver.bash compile /tmp/TestMigration.trash > trash/.compiled/TestMigration 2>/dev/null
source trash/.compiled/TestMigration

# Create and save an instance
instance=$(@ TestMigration create)
@ $instance fieldA: "valueA"
@ $instance fieldB: "valueB"
@ $instance save

# Verify it saved correctly
saved_data=$(db_get "$instance")
if echo "$saved_data" | jq -e '.fieldA == "valueA" and .fieldB == "valueB"' >/dev/null 2>&1; then
  pass "Instance created with original schema"
else
  fail "Instance not saved correctly"
fi

# --- Test 2: Add a new field to the class ---
echo ""
echo "Test 2: Add new field - should get default value on load"

# Redefine class with new field
cat > /tmp/TestMigration.trash << 'EOF'
TestMigration subclass: Object
  include: Persistable
  instanceVars: fieldA fieldB fieldC:'defaultC'
EOF

lib/jq-compiler/driver.bash compile /tmp/TestMigration.trash > trash/.compiled/TestMigration 2>/dev/null
source trash/.compiled/TestMigration

# Clear from memory to force reload
_env_delete "$instance" 2>/dev/null

# Load and check - fieldC should be added with default
loaded_data=$(@ $instance asJson)
if echo "$loaded_data" | jq -e '.fieldC == "defaultC"' >/dev/null 2>&1; then
  pass "New field added with default value"
else
  fail "New field not added or wrong default: $(echo "$loaded_data" | jq '.fieldC')"
fi

# Check _vars includes new field
if echo "$loaded_data" | jq -e '._vars | index("fieldC")' >/dev/null 2>&1; then
  pass "_vars updated to include new field"
else
  fail "_vars not updated"
fi

# --- Test 3: Remove a field from the class ---
echo ""
echo "Test 3: Remove field - should update _vars but preserve data"

# Save so we have fieldC in DB
@ $instance save

# Redefine class without fieldB
cat > /tmp/TestMigration.trash << 'EOF'
TestMigration subclass: Object
  include: Persistable
  instanceVars: fieldA fieldC:'defaultC'
EOF

lib/jq-compiler/driver.bash compile /tmp/TestMigration.trash > trash/.compiled/TestMigration 2>/dev/null
source trash/.compiled/TestMigration

# Clear from memory to force reload
_env_delete "$instance" 2>/dev/null

# Load and check
loaded_data=$(@ $instance asJson)

# _vars should NOT include fieldB
if echo "$loaded_data" | jq -e '._vars | index("fieldB") | not' >/dev/null 2>&1; then
  pass "_vars no longer includes removed field"
else
  fail "_vars still includes removed field"
fi

# But fieldB data should still be in JSON (preserved)
if echo "$loaded_data" | jq -e '.fieldB == "valueB"' >/dev/null 2>&1; then
  pass "Removed field data preserved in JSON"
else
  fail "Removed field data lost"
fi

# --- Test 4: Re-add a removed field ---
echo ""
echo "Test 4: Re-add removed field - should recover preserved data"

# Redefine class with fieldB back
cat > /tmp/TestMigration.trash << 'EOF'
TestMigration subclass: Object
  include: Persistable
  instanceVars: fieldA fieldB fieldC:'defaultC'
EOF

lib/jq-compiler/driver.bash compile /tmp/TestMigration.trash > trash/.compiled/TestMigration 2>/dev/null
source trash/.compiled/TestMigration

# Clear from memory to force reload
_env_delete "$instance" 2>/dev/null

# Load and check - fieldB should have original value
loaded_data=$(@ $instance asJson)
if echo "$loaded_data" | jq -e '.fieldB == "valueB"' >/dev/null 2>&1; then
  pass "Re-added field recovered original value"
else
  fail "Re-added field did not recover value"
fi

# --- Test 5: New field with null default ---
echo ""
echo "Test 5: New field without default gets null"

cat > /tmp/TestMigration.trash << 'EOF'
TestMigration subclass: Object
  include: Persistable
  instanceVars: fieldA fieldB fieldC:'defaultC' fieldD
EOF

lib/jq-compiler/driver.bash compile /tmp/TestMigration.trash > trash/.compiled/TestMigration 2>/dev/null
source trash/.compiled/TestMigration

_env_delete "$instance" 2>/dev/null

loaded_data=$(@ $instance asJson)
if echo "$loaded_data" | jq -e '.fieldD == null' >/dev/null 2>&1; then
  pass "New field without default gets null"
else
  fail "New field should be null"
fi

# --- Test 6: New field with numeric default ---
echo ""
echo "Test 6: New field with numeric default"

cat > /tmp/TestMigration.trash << 'EOF'
TestMigration subclass: Object
  include: Persistable
  instanceVars: fieldA fieldB fieldC:'defaultC' fieldD fieldE:42
EOF

lib/jq-compiler/driver.bash compile /tmp/TestMigration.trash > trash/.compiled/TestMigration 2>/dev/null
source trash/.compiled/TestMigration

_env_delete "$instance" 2>/dev/null

loaded_data=$(@ $instance asJson)
if echo "$loaded_data" | jq -e '.fieldE == 42' >/dev/null 2>&1; then
  pass "New field with numeric default works"
else
  fail "Numeric default not applied: $(echo "$loaded_data" | jq '.fieldE')"
fi

# --- Cleanup ---
echo ""
db_query "DELETE FROM objects WHERE id LIKE 'testmigration_%'" 2>/dev/null
rm -f trash/.compiled/TestMigration /tmp/TestMigration.trash

echo "================================"
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
