#!/bin/bash

# Test script for the Tuplespace object wrapper

# Set up environment
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Source trash system (which sources all dependencies)
echo "Sourcing trash system..."
source "$TRASHTALK_DIR/lib/trash.bash"

echo "=== Testing Tuplespace Object ==="

echo -e "\n1. Testing basic operations..."
@ Tuplespace init
@ Tuplespace clear

echo -e "\n2. Testing put/get operations..."
tuple_id=$(@ Tuplespace put "person" "name" "Alice" "age" "30")
echo "Created tuple: $tuple_id"

echo -e "\n3. Testing get operation..."
result=$(@ Tuplespace get "person" "name" "Alice")
echo "Retrieved tuple:"
echo "$result"

echo -e "\n4. Testing count operation..."
count=$(@ Tuplespace count "person")
echo "Person count: $count"

echo -e "\n5. Testing key-value operations..."
@ Tuplespace putKV "test.key" "test.value"
value=$(@ Tuplespace getKV "test.key")
echo "Retrieved value: $value"

echo -e "\n6. Testing event operations..."
@ Tuplespace putEvent "test_event" "test_data"
event_count=$(@ Tuplespace count "event")
echo "Event count: $event_count"

echo -e "\n7. Testing info..."
@ Tuplespace info

echo -e "\n8. Running built-in test suite..."
@ Tuplespace test

echo -e "\n=== Tuplespace Object Test Complete ==="
