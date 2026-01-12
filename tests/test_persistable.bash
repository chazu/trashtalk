#!/usr/bin/env bash
# Test script for Persistable trait
#
# Note: With native compilation, `new` now immediately persists to the database.
# This is the simplified model where all instances are persistent by default.
# Use `delete` for explicit cleanup.

cd "$(dirname "${BASH_SOURCE[0]}")/.."
source lib/trash.bash 2>/dev/null

echo "=== Test Persistable Trait ==="
echo ""

echo "1. Create counter (now persists immediately)"
counter=$(@ Counter new)
echo "   Created: $counter"
@ $counter increment
@ $counter increment
# Save to persist the updated value (Bash-only classes need explicit save)
@ $counter save
echo "   Value: $(@ $counter getValue)"
echo ""

echo "2. Verify instance IS in database (new model: immediate persistence)"
if db_get "$counter" 2>/dev/null | grep -q .; then
  echo "   In DB immediately (correct - new model)"
else
  echo "   ERROR: Should be in DB after new!"
  exit 1
fi
echo ""

echo "3. Check isPersisted"
is_persisted=$(@ $counter isPersisted)
echo "   isPersisted: $is_persisted"
if [[ "$is_persisted" != "true" ]]; then
  echo "   ERROR: isPersisted should be true!"
  exit 1
fi
echo ""

echo "4. Verify state persisted correctly"
db_data=$(db_get "$counter")
echo "   DB data: $db_data"
db_value=$(echo "$db_data" | jq -r '.value')
if [[ "$db_value" != "2" ]]; then
  echo "   ERROR: Expected value=2 in DB, got $db_value"
  exit 1
fi
echo "   Value in DB matches (correct)"
echo ""

echo "5. Test Counter create (equivalent to new in new model)"
counter2=$(@ Counter create)
echo "   Created via create: $counter2"
is_persisted2=$(@ $counter2 isPersisted)
echo "   isPersisted: $is_persisted2"
if [[ "$is_persisted2" != "true" ]]; then
  echo "   ERROR: create should auto-persist!"
  exit 1
fi
echo ""

echo "6. Test findAll"
all_counters=$(@ Counter findAll)
echo "   All counters in DB:"
echo "$all_counters" | sed 's/^/     /'
echo ""

echo "7. Test count"
count=$(@ Counter count)
echo "   Counter count: $count"
if [[ "$count" -lt 2 ]]; then
  echo "   ERROR: Should have at least 2 counters!"
  exit 1
fi
echo ""

echo "8. Test delete from DB"
@ $counter delete
if db_get "$counter" 2>/dev/null | grep -q .; then
  echo "   ERROR: Still in DB after delete!"
  exit 1
else
  echo "   Deleted from DB (correct)"
fi
echo ""

echo "9. Cleanup second counter"
@ $counter2 delete
echo "   Cleaned up $counter2"
echo ""

echo "=== All tests passed! ==="
