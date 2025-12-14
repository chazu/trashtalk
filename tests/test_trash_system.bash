#!/bin/bash

# Test script for the Trash system object

# Source from trashtalk lib directory
TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"
source "$TRASHTALK_DIR/lib/trash-parser.bash"

echo "=== Testing Trash System Object ==="

echo -e "\n1. System Information"
@ Trash info

echo -e "\n2. List All Objects"
echo "Available objects:"
@ Trash listObjects

echo -e "\n3. List All Traits"
echo "Available traits:"
@ Trash listTraits

echo -e "\n4. Show Methods for Object"
echo "Methods for Array:"
@ Trash methodsFor Array

echo -e "\n5. Show Inheritance Hierarchy"
echo "Hierarchy for Fzf:"
@ Trash hierarchyFor Fzf

echo -e "\n6. Create a New Object"
echo "Creating TestService object..."
@ Trash createObject TestService Tool

echo "Verifying TestService was created:"
@ Trash listObjects | grep TestService

echo -e "\n7. Test the New Object"
echo "Testing TestService example method:"
@ TestService example

echo -e "\n8. Show Methods for New Object"
@ Trash methodsFor TestService

echo -e "\n9. System Statistics"
@ Trash stats

echo -e "\n10. Using Objective-C Syntax with Trash"
echo "@ [Trash info]"
@ [Trash info]

echo -e "\n@ [Trash methodsFor:Array]"
@ [Trash methodsFor:Array]

echo -e "\n11. Help System"
@ Trash help

echo -e "\n12. Cleanup Test Object"
echo "Removing TestService object..."
echo "y" | @ Trash removeObject TestService

echo "Verifying TestService was removed:"
if @ Trash listObjects | grep -q TestService; then
    echo "ERROR: TestService still exists"
else
    echo "SUCCESS: TestService removed"
fi

echo -e "\n13. System Cleanup"
@ Trash cleanup

echo -e "\n=== Trash System Object Tests Complete ==="

echo -e "\n=== Quick Demo of System Capabilities ==="
echo "The Trash object provides a complete API for system management:"
echo "- Introspection: info, listObjects, methodsFor, hierarchyFor"
echo "- Object management: createObject, removeObject"
echo "- System maintenance: reload, stats, cleanup"
echo "- Help and documentation: help"
echo ""
echo "This makes the trash system self-documenting and manageable!"
