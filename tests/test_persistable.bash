#!/usr/bin/env bash
# Test script for Persistable trait

cd "$(dirname "${BASH_SOURCE[0]}")/.."
source lib/trash.bash 2>/dev/null

echo "=== Test Persistable Trait ==="
echo ""

echo "1. Create counter in memory"
counter=$(@ Counter new)
echo "   Created: $counter"
@ $counter increment
@ $counter increment
echo "   Value: $(@ $counter getValue)"
echo ""

echo "2. Check NOT in database yet"
if db_get "$counter" 2>/dev/null | grep -q .; then
  echo "   ERROR: Found in DB before save!"
  exit 1
else
  echo "   Not in DB (correct)"
fi
echo ""

echo "3. Save to database"
@ $counter save
echo "   Saved!"
echo ""

echo "4. Verify in database"
db_data=$(db_get "$counter")
echo "   DB data: $db_data"
echo ""

echo "5. Check isPersisted"
is_persisted=$(@ $counter isPersisted)
echo "   isPersisted: $is_persisted"
if [[ "$is_persisted" != "true" ]]; then
  echo "   ERROR: isPersisted should be true!"
  exit 1
fi
echo ""

echo "6. Test Counter create (new + save in one step)"
counter2=$(@ Counter create)
echo "   Created via create: $counter2"
is_persisted2=$(@ $counter2 isPersisted)
echo "   isPersisted: $is_persisted2"
if [[ "$is_persisted2" != "true" ]]; then
  echo "   ERROR: create should auto-persist!"
  exit 1
fi
echo ""

echo "7. Test findAll"
all_counters=$(@ Counter findAll)
echo "   All counters in DB:"
echo "$all_counters" | sed 's/^/     /'
echo ""

echo "8. Test count"
count=$(@ Counter count)
echo "   Counter count: $count"
echo ""

echo "9. Test delete from DB"
@ $counter delete
if db_get "$counter" 2>/dev/null | grep -q .; then
  echo "   ERROR: Still in DB after delete!"
  exit 1
else
  echo "   Deleted from DB (correct)"
fi
echo ""

echo "=== All tests passed! ==="
