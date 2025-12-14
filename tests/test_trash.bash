#!/bin/bash

# Comprehensive test script for the enhanced trash system

# Source from trashtalk lib directory
TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"
source "$TRASHTALK_DIR/lib/trash-parser.bash"

echo "=== Testing Enhanced Trash System ==="

echo -e "\n1. Basic Message Sending"
echo "@ Object fooBar"
@ Object fooBar

echo -e "\n@ Tool fooBar"
@ Tool fooBar

echo -e "\n2. Inheritance Chain"
echo "@ Tool baz (should find in Object)"
@ Tool baz

echo -e "\n3. Trait System"
echo "@ Tool debug 'Testing debug trait'"
@ Tool debug "Testing debug trait"

echo "@ Tool inspect"
@ Tool inspect

echo -e "\n4. Objective-C Style Parser"
echo "@ [Object fooBar]"
@ [Object fooBar]

echo -e "\n@ [Tool install]"
@ [Tool install]

echo -e "\n5. String Literals"
echo "@ [Tool debug:@\"Hello from parser\"]"
@ [Tool debug:@"Hello from parser"]

echo -e "\n6. Array Operations"
echo "Creating array..."
arr=$(@ Array new)
echo "Array ID: $arr"

echo "Adding values..."
@ $arr push hello
@ $arr push world
@ $arr push 123

echo "Array contents:"
@ $arr show

echo "Getting element at index 1:"
@ $arr at 1

echo -e "\n7. Counter and Nested Messages"
echo "Creating counter..."
counter=$(@ Counter new)
echo "Counter ID: $counter"

echo "Setting initial value..."
@ $counter setValue 5
@ $counter show

echo "Incrementing..."
@ $counter increment 3
@ $counter show

echo -e "\n8. Nested Message Example"
echo "Setting array element to counter value..."
# This demonstrates: [arr at:0 put:[counter getValue]]
counter_value=$(@ $counter getValue)
@ $arr at_put 0 $counter_value
@ $arr show

echo -e "\n9. Process Spawning"
echo "Spawning Tool process..."
process_id=$(@ Process spawn Tool)
echo "Process ID: $process_id"

if [[ -n "$process_id" ]]; then
    echo "Sending message to process..."
    @ Process sendToProcess $process_id fooBar

    echo "Getting result..."
    result=$(@ Process getFromProcess $process_id 5)
    echo "Result: $result"

    echo "Terminating process..."
    @ Process terminate $process_id
fi

echo -e "\n=== All Tests Complete ==="
