# Future API

Future provides simple async computation with result retrieval. Use it for running Trashtalk code in the background and getting results later.

## Basic Usage

Future uses a two-call pattern to avoid Bash subshell issues:

```bash
# Create the future (captures the ID)
future=$(@ Future for: '@ Counter expensiveCalculation')

# Start the background process (outside of $())
@ $future start

# Do other work while computation runs...
@ SomeClass doOtherStuff

# Get the result (blocks until done)
result=$(@ $future await)
echo "Result: $result"

# Clean up
@ $future cleanup
```

## Methods

### Class Methods

| Method | Description |
|--------|-------------|
| `@ Future for: <command>` | Create future (stores command, does not start) |
| `@ Future help` | Show help message |

### Instance Methods

| Method | Description |
|--------|-------------|
| `@ $future start` | Start the background computation |
| `@ $future await` | Block until done, return result |
| `@ $future poll` | Check status without blocking |
| `@ $future isDone` | Returns 0 if done, 1 if pending |
| `@ $future cancel` | Kill the computation |
| `@ $future status` | Show status (created/pending/completed/failed/cancelled) |
| `@ $future exitCode` | Get exit code (empty if pending) |
| `@ $future cleanup` | Remove result files and delete future |

## Status Lifecycle

```
created → pending     (after start is called)
pending → completed   (success, exit code 0)
pending → failed      (command failed, exit code non-zero)
pending → cancelled   (cancelled via cancel method)
```

## Examples

### Run Multiple Computations in Parallel

```bash
# Create three futures
f1=$(@ Future for: '@ Report generateSales')
f2=$(@ Future for: '@ Report generateInventory')
f3=$(@ Future for: '@ Report generateCustomers')

# Start all of them
@ $f1 start
@ $f2 start
@ $f3 start

# Wait for all results
sales=$(@ $f1 await)
inventory=$(@ $f2 await)
customers=$(@ $f3 await)

# Combine and display
echo "Sales: $sales"
echo "Inventory: $inventory"
echo "Customers: $customers"

# Cleanup
@ $f1 cleanup
@ $f2 cleanup
@ $f3 cleanup
```

### Check Completion Without Blocking

```bash
future=$(@ Future for: '@ DataProcessor crunchNumbers')
@ $future start

# Poll periodically
while ! @ $future isDone; do
    echo "Still processing..."
    sleep 1
done

result=$(@ $future await)
echo "Done: $result"
@ $future cleanup
```

### Handle Failed Computations

```bash
future=$(@ Future for: '@ SomeClass riskyOperation')
@ $future start
@ $future await >/dev/null

status=$(@ $future status)
if [[ "$status" == "completed" ]]; then
    echo "Success!"
else
    echo "Failed with exit code: $(@ $future exitCode)"
fi

@ $future cleanup
```

## Instance Variables

| Variable | Description |
|----------|-------------|
| `command` | The command to execute |
| `pid` | Process ID of background computation |
| `result_file` | Path to file containing result output |
| `status` | Current status (created/pending/completed/failed/cancelled) |
| `exit_code` | Exit code after completion |

## Storage

Future results are stored in `/tmp/trashtalk/futures/`:
- Result file: `<future_id>` (stdout/stderr)
- Exit file: `<future_id>.exit` (exit code)

Call `cleanup` to remove these files when done.

## Differences from Process

| Future | Process |
|--------|---------|
| Runs Trashtalk code | Runs external commands |
| Simple await/cancel | Full process control (signals) |
| Auto-captures result | Manual output/errors access |
| `@ Future for: '@ obj method'` | `@ Process for: "shell cmd"` |

Use Future for Trashtalk computations, Process for external commands.

## Quick Reference

```bash
# Create, start, and await (two-call pattern)
future=$(@ Future for: '@ MyClass compute')
@ $future start       # MUST be outside $()
result=$(@ $future await)
@ $future cleanup

# Check status
@ $future status      # created, pending, completed, failed, cancelled
@ $future isDone      # returns 0 if done
@ $future poll        # non-blocking status check
@ $future exitCode    # exit code (0 = success)

# Cancel
@ $future cancel

# Help
@ Future help
```

## Why the Two-Call Pattern?

Bash terminates background processes started in `$()` when the subshell exits. The two-call pattern avoids this:

```bash
future=$(@ Future for: 'long_computation')  # Creates instance only (in subshell)
@ $future start                              # Starts process in main shell
```

The `for:` call runs in a subshell (due to `$()`), but it only creates the instance - no background process yet. The `start` call runs in the main shell, so the background process persists normally.

The Process class uses the same pattern for the same reason.
