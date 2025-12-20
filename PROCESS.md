# Process API

Process provides an interface to spawn and manage external OS (POSIX) processes. Use it for running shell commands, capturing output, and managing background tasks.

## Quick Execution (Class Methods)

For simple one-off commands:

```bash
# Execute and get stdout
output=$(@ Process exec: "ls -la")
echo "$output"

# Execute and get exit code
@ Process run: "make test"
echo "Exit code: $?"

# Spawn in background, get PID
pid=$(@ Process spawn: "long-running-task")
echo "Started with PID: $pid"

# Wait for a PID
exit_code=$(@ Process waitPid: $pid)

# Check if running
@ Process isRunningPid: $pid  # => "true" or "false"

# Kill a PID
@ Process killPid: $pid
```

## Managed Processes (Instance Methods)

For processes you need to monitor, capture output from, or manage:

```bash
# Create a process wrapper
proc=$(@ Process for: "curl -s https://api.example.com/data")

# Run (blocking) - captures stdout, stderr, exit code
@ $proc run

# Check result
if [[ $(@ $proc succeeded) == "true" ]]; then
    echo "Response: $(@ $proc output)"
else
    echo "Error: $(@ $proc errors)"
    echo "Exit code: $(@ $proc exitCode)"
fi
```

### Background Execution

```bash
# Create process
proc=$(@ Process for: "sleep 10 && echo done")

# Start in background
pid=$(@ $proc start)
echo "Started with PID: $pid"

# Do other work...

# Check if still running
@ $proc isRunning  # => "true" or "false"

# Wait for completion
exit_code=$(@ $proc wait)
echo "Output: $(@ $proc output)"
```

### Signals

```bash
proc=$(@ Process for: "long-running-task")
@ $proc start

# Send SIGTERM
@ $proc terminate

# Send SIGKILL (if terminate doesn't work)
@ $proc kill

# Send any signal
@ $proc signal: HUP
@ $proc signal: USR1
```

### Process Information

```bash
@ $proc info

# Output:
# Command: curl -s https://api.example.com
# PID: 12345
# Status: completed
# Exit Code: 0
# Started: Fri Dec 19 12:00:00 EST 2025
# Ended: Fri Dec 19 12:00:01 EST 2025
# Duration: 1s
```

## Instance Variables

| Variable | Description |
|----------|-------------|
| `command` | The shell command to execute |
| `pid` | Process ID (when running in background) |
| `status` | created, running, completed, terminated, killed |
| `exitCode` | Exit code after completion |
| `stdout` | Captured standard output |
| `stderr` | Captured standard error |
| `startTime` | Unix timestamp when started |
| `endTime` | Unix timestamp when completed |

## Examples

### Run a build and check for errors

```bash
proc=$(@ Process for: "make build")
@ $proc run

if [[ $(@ $proc succeeded) == "true" ]]; then
    echo "Build succeeded!"
else
    echo "Build failed:"
    @ $proc errors
fi
```

### Fetch data with timeout

```bash
proc=$(@ Process for: "curl -s --max-time 10 https://api.example.com")
@ $proc run

if [[ $(@ $proc exitCode) == "0" ]]; then
    data=$(@ $proc output)
    echo "Got data: $data"
else
    echo "Request failed with code $(@ $proc exitCode)"
fi
```

### Run parallel tasks

```bash
# Start multiple processes
proc1=$(@ Process for: "task1")
proc2=$(@ Process for: "task2")
proc3=$(@ Process for: "task3")

@ $proc1 start
@ $proc2 start
@ $proc3 start

# Wait for all
@ $proc1 wait
@ $proc2 wait
@ $proc3 wait

# Check results
echo "Task 1: $(@ $proc1 exitCode)"
echo "Task 2: $(@ $proc2 exitCode)"
echo "Task 3: $(@ $proc3 exitCode)"
```

## Differences from Actor

| Process | Actor |
|---------|-------|
| External OS process wrapper | Internal concurrency primitive |
| Direct stdin/stdout capture | Communicates via Tuplespace |
| Runs any shell command | Runs Trashtalk code |
| Command execution model | Message passing model |
| `@ Process exec: "curl ..."` | `@ Actor spawn: Counter` |

## Quick Reference

### Class Methods
| Method | Description |
|--------|-------------|
| `@ Process for: <cmd>` | Create managed process |
| `@ Process exec: <cmd>` | Run, return stdout |
| `@ Process run: <cmd>` | Run, return exit code |
| `@ Process spawn: <cmd>` | Background, return PID |
| `@ Process waitPid: <pid>` | Wait for PID |
| `@ Process isRunningPid: <pid>` | Check if PID is running |
| `@ Process killPid: <pid>` | Kill PID |

### Instance Methods
| Method | Description |
|--------|-------------|
| `@ $proc run` | Run blocking |
| `@ $proc start` | Run in background |
| `@ $proc wait` | Wait for completion |
| `@ $proc isRunning` | Check if running |
| `@ $proc terminate` | Send SIGTERM |
| `@ $proc kill` | Send SIGKILL |
| `@ $proc signal: <sig>` | Send signal |
| `@ $proc output` | Get stdout |
| `@ $proc errors` | Get stderr |
| `@ $proc exitCode` | Get exit code |
| `@ $proc succeeded` | Check if exit 0 |
| `@ $proc info` | Show process info |
