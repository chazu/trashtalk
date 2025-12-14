#!/usr/bin/env bash
# Tuplespace Examples - Event-driven scripting patterns

source "$(dirname "${BASH_SOURCE[0]}")/tuplespace.bash"

# Example 1: File watcher that puts events into tuplespace
file_watcher() {
    local watch_dir="${1:-.}"
    echo "Watching directory: $watch_dir"
    
    # Use entr to watch for file changes
    find "$watch_dir" -type f | entr -r bash -c "
        source $(dirname "${BASH_SOURCE[0]}")/tuplespace.bash
        ts_event_put 'file_changed' \"\$(date)\"
        echo 'File change detected at \$(date)'
    " &
    
    echo "File watcher started with PID: $!"
}

# Example 2: Log processor that reacts to file changes
log_processor() {
    echo "Starting log processor..."
    
    # Listen for file change events
    ts_listen "event" '
        tuple_data=$(ts_get "event" "ID" "$1")
        event_name=$(echo "$tuple_data" | ts_field - "name")
        
        if [[ "$event_name" == "file_changed" ]]; then
            echo "Processing log files due to file change..."
            # Your log processing logic here
            ts_event_put "log_processed" "$(date)"
        fi
    '
}

# Example 3: System monitor that puts metrics into tuplespace
system_monitor() {
    echo "Starting system monitor..."
    
    while true; do
        # Get system metrics
        local cpu_usage=$(top -l 1 -n 0 | grep "CPU usage" | awk '{print $3}' | sed 's/%//')
        local memory_usage=$(vm_stat | grep "Pages active" | awk '{print $3}' | sed 's/\.//')
        local disk_usage=$(df -h / | tail -1 | awk '{print $5}' | sed 's/%//')
        
        # Put metrics into tuplespace
        ts_put "metric" "type" "cpu" "value" "$cpu_usage" "timestamp" "$(date +%s)"
        ts_put "metric" "type" "memory" "value" "$memory_usage" "timestamp" "$(date +%s)"
        ts_put "metric" "type" "disk" "value" "$disk_usage" "timestamp" "$(date +%s)"
        
        sleep 10
    done &
    
    echo "System monitor started with PID: $!"
}

# Example 4: Alert handler that reacts to high metrics
alert_handler() {
    echo "Starting alert handler..."
    
    ts_listen "metric" '
        tuple_data=$(ts_get "metric" "ID" "$1")
        metric_type=$(echo "$tuple_data" | ts_field - "type")
        metric_value=$(echo "$tuple_data" | ts_field - "value")
        
        case "$metric_type" in
            "cpu")
                if [[ $(echo "$metric_value > 80" | bc -l) -eq 1 ]]; then
                    ts_event_put "alert" "High CPU usage: ${metric_value}%"
                    echo "ALERT: High CPU usage: ${metric_value}%"
                fi
                ;;
            "disk")
                if [[ $(echo "$metric_value > 90" | bc -l) -eq 1 ]]; then
                    ts_event_put "alert" "High disk usage: ${metric_value}%"
                    echo "ALERT: High disk usage: ${metric_value}%"
                fi
                ;;
        esac
    '
}

# Example 5: Simple task queue using tuplespace
task_queue_producer() {
    local num_tasks="${1:-5}"
    echo "Producing $num_tasks tasks..."
    
    for i in $(seq 1 "$num_tasks"); do
        ts_put "task" "id" "$i" "command" "echo 'Processing task $i'" "status" "pending"
        echo "Queued task $i"
        sleep 1
    done
}

task_queue_worker() {
    local worker_id="${1:-worker1}"
    echo "Starting task worker: $worker_id"
    
    while true; do
        # Look for pending tasks
        local task=$(ts_take "task" "status" "pending" | head -1)
        
        if [[ -n "$task" ]]; then
            local task_id=$(echo "$task" | ts_field - "id")
            local command=$(echo "$task" | ts_field - "command")
            
            echo "[$worker_id] Processing task $task_id"
            
            # Execute the task
            eval "$command"
            
            # Mark as completed
            ts_put "task" "id" "$task_id" "command" "$command" "status" "completed" "worker" "$worker_id"
            
            echo "[$worker_id] Completed task $task_id"
        else
            sleep 1
        fi
    done
}

# Example 6: Configuration change propagation
config_watcher() {
    echo "Starting configuration watcher..."
    
    # Watch for config changes
    ts_listen "kv" '
        tuple_data=$(ts_get "kv" "ID" "$1")
        key=$(echo "$tuple_data" | ts_field - "key")
        
        if [[ "$key" == config.* ]]; then
            echo "Configuration changed: $key"
            ts_event_put "config_changed" "$key"
        fi
    '
}

# Demo function to show all examples
demo_all() {
    echo "=== Tuplespace Event-Driven Demo ==="
    
    # Clear the space
    ts_clear
    
    echo -e "\n1. Starting system monitor..."
    system_monitor
    sleep 2
    
    echo -e "\n2. Starting alert handler..."
    alert_handler
    sleep 2
    
    echo -e "\n3. Adding some test data..."
    ts_kv_put "config.debug" "true"
    ts_kv_put "config.timeout" "30"
    
    echo -e "\n4. Starting config watcher..."
    config_watcher
    sleep 2
    
    echo -e "\n5. Changing config to trigger events..."
    ts_kv_put "config.debug" "false"
    
    echo -e "\n6. Current tuplespace contents:"
    ts_list
    
    echo -e "\nDemo running... Press Ctrl+C to stop"
    wait
}

# Usage information
usage() {
    echo "Tuplespace Examples Usage:"
    echo "  $0 demo                    - Run full demo"
    echo "  $0 file_watcher [dir]      - Watch directory for changes"
    echo "  $0 log_processor           - Process file change events"
    echo "  $0 system_monitor          - Monitor system metrics"
    echo "  $0 alert_handler           - Handle metric alerts"
    echo "  $0 task_producer [n]       - Produce n tasks"
    echo "  $0 task_worker [id]        - Start task worker"
    echo "  $0 config_watcher          - Watch config changes"
}

# Main dispatch
case "${1:-}" in
    "demo") demo_all ;;
    "file_watcher") file_watcher "$2" ;;
    "log_processor") log_processor ;;
    "system_monitor") system_monitor ;;
    "alert_handler") alert_handler ;;
    "task_producer") task_queue_producer "$2" ;;
    "task_worker") task_queue_worker "$2" ;;
    "config_watcher") config_watcher ;;
    *) usage ;;
esac
