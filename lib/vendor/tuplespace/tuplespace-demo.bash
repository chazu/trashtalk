#!/usr/bin/env bash
# Simple demo of tuplespace event-driven scripting

source "$(dirname "${BASH_SOURCE[0]}")/tuplespace.bash"

echo "=== Tuplespace Event-Driven Demo ==="
echo "This demo shows how to use the tuplespace for event coordination"
echo ""

# Clear the space
ts_clear

echo "1. Setting up a simple task queue..."
# Producer: Add some tasks
ts_put "task" "id" "1" "command" "echo 'Processing order #1001'" "status" "pending"
ts_put "task" "id" "2" "command" "echo 'Sending email to customer'" "status" "pending"
ts_put "task" "id" "3" "command" "echo 'Updating inventory'" "status" "pending"

echo "   Added 3 tasks to the queue"
echo "   Current task count: $(ts_count task)"
echo ""

echo "2. Processing tasks (simulating a worker)..."
# Worker: Process tasks one by one
for i in {1..3}; do
    # Take a pending task
    task=$(ts_take "task" "status" "pending" | head -1)
    
    if [[ -n "$task" ]]; then
        task_id=$(echo "$task" | ts_field - "id")
        command=$(echo "$task" | ts_field - "command")
        
        echo "   Worker processing task $task_id..."
        eval "$command"
        
        # Mark as completed
        ts_put "task" "id" "$task_id" "command" "$command" "status" "completed"
        
        echo "   Task $task_id completed"
    fi
done

echo ""
echo "3. Checking results..."
echo "   Pending tasks: $(ts_count task | grep -c pending || echo 0)"
echo "   Completed tasks: $(ts_get task status completed | grep -c "^Type:" || echo 0)"
echo ""

echo "4. Event-driven configuration example..."
# Put some configuration
ts_kv_put "app.debug" "false"
ts_kv_put "app.timeout" "30"
ts_kv_put "app.max_connections" "100"

echo "   Configuration set:"
echo "   - Debug: $(ts_kv_get app.debug)"
echo "   - Timeout: $(ts_kv_get app.timeout)"
echo "   - Max connections: $(ts_kv_get app.max_connections)"
echo ""

echo "5. Simulating configuration change..."
ts_kv_put "app.debug" "true"
echo "   Changed debug mode to: $(ts_kv_get app.debug)"
echo ""

echo "6. Event logging example..."
ts_event_put "user_login" "alice@example.com"
ts_event_put "user_login" "bob@example.com"
ts_event_put "user_logout" "alice@example.com"

echo "   Logged 3 events"
echo "   Total events: $(ts_count event)"
echo ""

echo "7. Current tuplespace contents:"
echo "   Total tuples: $(ts_count)"
echo "   By type:"
echo "   - Tasks: $(ts_count task)"
echo "   - Key-value pairs: $(ts_count kv)"
echo "   - Events: $(ts_count event)"
echo ""

echo "8. Example queries:"
echo "   All login events:"
ts_get "event" "name" "user_login" | grep -E "^(name|data):" | sed 's/^/     /'
echo ""

echo "   All completed tasks:"
ts_get "task" "status" "completed" | grep -E "^(id|command):" | sed 's/^/     /'
echo ""

echo "Demo complete! The tuplespace provides:"
echo "- Decoupled communication between scripts"
echo "- Event-driven coordination"
echo "- Simple task queuing"
echo "- Configuration management"
echo "- Persistent storage using recutils"
echo ""
echo "Try running the examples in bash/tuplespace/tuplespace-examples.bash for more complex scenarios!"
