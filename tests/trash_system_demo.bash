#!/bin/bash

# Comprehensive demo of the Trash system capabilities

# Source from trashtalk lib directory
TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"
source "$TRASHTALK_DIR/lib/trash-parser.bash"

echo "🗑️  Welcome to the Trash System Demo!"
echo "======================================"

# System overview
echo -e "\n📊 System Overview"
@ Trash version
echo ""
@ Trash stats

# Object management
echo -e "\n🏗️  Object Management"
echo "Creating a web service using template..."
@ Trash quickCreate WebService service

echo -e "\nCreating a deployment tool..."
@ Trash quickCreate DeployTool tool

echo -e "\nCreating a monitoring actor..."
@ Trash quickCreate Monitor actor

echo -e "\nCurrent objects:"
@ Trash listObjects

# Testing the new objects
echo -e "\n🧪 Testing New Objects"
echo "Starting web service:"
@ WebService start

echo -e "\nInstalling deploy tool:"
@ DeployTool install

echo -e "\nInitializing monitor:"
@ Monitor initialize

# Object introspection
echo -e "\n🔍 Object Introspection"
echo "Detailed inspection of WebService:"
@ Trash inspect WebService

# Using Objective-C syntax
echo -e "\n🎯 Objective-C Style Syntax"
echo "@ [WebService status]"
@ [WebService status]

echo -e "\n@ [DeployTool configure:@\"production\"]"
@ [DeployTool configure:@"production"]

# Process spawning with new actor
echo -e "\n⚡ Concurrent Processing"
echo "Spawning monitor as background process..."
monitor_process=$(@ Process spawn Monitor)

if [[ -n "$monitor_process" ]]; then
    echo "Monitor process ID: $monitor_process"
    
    echo "Sending ping to monitor..."
    @ Process sendToProcess $monitor_process receive ping
    
    echo "Getting response..."
    response=$(@ Process getFromProcess $monitor_process 3)
    echo "Monitor responded: $response"
    
    echo "Checking monitor status..."
    @ Process sendToProcess $monitor_process receive status
    status=$(@ Process getFromProcess $monitor_process 3)
    echo "Monitor status: $status"
    
    echo "Terminating monitor process..."
    @ Process terminate $monitor_process
fi

# Data structures
echo -e "\n📚 Data Structures"
echo "Creating a task queue..."
tasks=$(@ Array new)
@ $tasks push "Deploy to staging"
@ $tasks push "Run tests"
@ $tasks push "Deploy to production"

echo "Task queue contents:"
@ $tasks show

echo -e "\nProcessing tasks with counter..."
completed=$(@ Counter new)

for i in {0..2}; do
    task=$(@ $tasks at $i)
    if [[ -n "$task" ]]; then
        echo "Processing: $task"
        @ $completed increment
    fi
done

echo "Completed $(@ $completed getValue) tasks"

# System maintenance
echo -e "\n🧹 System Maintenance"
echo "System cleanup..."
@ Trash cleanup

echo -e "\nFinal system stats:"
@ Trash stats

# Cleanup demo objects
echo -e "\n🗑️  Cleaning Up Demo Objects"
echo "y" | @ Trash removeObject WebService
echo "y" | @ Trash removeObject DeployTool  
echo "y" | @ Trash removeObject Monitor

echo -e "\n✅ Demo Complete!"
echo "======================================"
echo ""
echo "🎉 The Trash system provides:"
echo "   • Object-oriented programming in Bash"
echo "   • Smalltalk/Objective-C inspired syntax"
echo "   • Prototypal inheritance"
echo "   • Trait-based mixins"
echo "   • Concurrent processing with actors"
echo "   • Persistent object state"
echo "   • Rich introspection and debugging"
echo "   • Self-documenting system API"
echo ""
echo "Try: @ Trash help"
echo "Or:  @ [Trash createObject:MyObject]"
