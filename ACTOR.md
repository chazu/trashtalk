# Actor API

Actors are Erlang/Go-style concurrency primitives for Trashtalk. Each actor runs in isolation, communicating only via message passing through Tuplespace.

## Concepts

- **Isolation**: Each actor runs in a separate bash subshell - no shared state
- **Message Passing**: Actors communicate via Tuplespace, not direct calls
- **One-at-a-time**: Each actor processes one message at a time (no internal races)
- **Location Transparency**: Send messages by actor ID, not implementation details

## Basic Usage

### Spawn an Actor

```bash
# Spawn an actor for a class (singleton pattern)
actor_id=$(@ Actor spawn: Counter)

# The actor runs a message loop, waiting for messages
```

### Send Messages

```bash
# Send a message (async - returns immediately)
msg_id=$(@ Actor sendTo: $actor_id message: increment)

# The actor will execute: @ Counter increment
```

### Get Responses

```bash
# Wait for a response (blocking with timeout)
result=$(@ Actor getFrom: $actor_id timeout: 10)

# Returns the result of the last message
```

### Check Status

```bash
@ Actor status: $actor_id

# Output:
# Actor ID: actor_abc123
# Object: Counter
# Status: running
# PID: 12345
# Created: Fri Dec 19 12:00:00 EST 2025
# Actor is alive
```

### Terminate

```bash
# Graceful shutdown (waits up to 5 seconds, then SIGKILL)
@ Actor terminate: $actor_id
```

## Actor Management

```bash
# List all actors
@ Actor listActors

# List only running actors
@ Actor listRunning

# Clean up dead actors
@ Actor cleanupAll

# System statistics
@ Actor stats
```

## Complete Example

```bash
# Start fresh
@ Tuplespace clear

# Spawn an actor for Counter class
actor=$(@ Actor spawn: Counter)
echo "Spawned actor: $actor"

# Send messages
@ Actor sendTo: $actor message: increment
@ Actor sendTo: $actor message: increment
@ Actor sendTo: $actor message: getValue

# Get the last result
value=$(@ Actor getFrom: $actor timeout: 5)
echo "Counter value: $value"

# Clean up
@ Actor terminate: $actor
```

## How It Works

1. `spawn:` creates a background bash subshell running a message loop
2. The loop polls Tuplespace for messages targeted at this actor
3. When a message arrives, it executes `@ $object $payload`
4. Results are put back into Tuplespace as responses
5. The caller can retrieve responses with `getFrom:timeout:`

## Message Flow

```
Caller                    Tuplespace                 Actor
  |                           |                        |
  |-- put message ----------->|                        |
  |                           |<-- poll for messages --|
  |                           |--- message found ----->|
  |                           |                        |-- execute
  |                           |<-- put response -------|
  |<-- get response ----------|                        |
```

## Differences from Process

| Actor | Process |
|-------|---------|
| Internal concurrency primitive | External OS process wrapper |
| Communicates via Tuplespace | Direct stdin/stdout capture |
| Runs Trashtalk code | Runs any shell command |
| Message passing model | Command execution model |
| `@ Actor spawn: Counter` | `@ Process exec: "curl ..."` |

## Quick Reference

| Operation | Command |
|-----------|---------|
| Spawn | `@ Actor spawn: <object>` |
| Send message | `@ Actor sendTo: <id> message: <msg>` |
| Get response | `@ Actor getFrom: <id> timeout: <sec>` |
| Terminate | `@ Actor terminate: <id>` |
| Status | `@ Actor status: <id>` |
| List all | `@ Actor listActors` |
| List running | `@ Actor listRunning` |
| Cleanup dead | `@ Actor cleanupAll` |
| Stats | `@ Actor stats` |
| Help | `@ Actor help` |
