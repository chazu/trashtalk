# Bytecode Blocks User Guide

Bytecode blocks are a high-performance alternative to Bash-based blocks in Trashtalk. They execute natively in Go via the Procyon daemon, providing significant performance improvements while maintaining full compatibility with existing code.

## Overview

When a Trashtalk block is compiled to bytecode:
- Arithmetic and control flow execute natively in Go (no subprocess spawning)
- Loops run 10-100x faster than Bash equivalents
- Captured variables maintain reference semantics
- Full fallback to Bash for unsupported operations

## Requirements

Bytecode blocks require the NativeDaemon to be running:

```bash
source lib/trash.bash
daemon=$(@ NativeDaemon instance)
```

The daemon manages a block registry and executes bytecode via a Unix socket.

## Identifying Bytecode Blocks

Bytecode block IDs have the prefix `bytecode_block_`:

```bash
# Check if a value is a bytecode block
if _is_bytecode_block "$block_id"; then
    echo "This is a bytecode block"
fi
```

## Runtime Functions

### `_is_bytecode_block`

Test if a string is a bytecode block ID.

```bash
_is_bytecode_block "bytecode_block_123"  # Returns 0 (true)
_is_bytecode_block "counter_abc"         # Returns 1 (false)
```

### `_register_bytecode_block`

Register bytecode with the daemon and get back a block ID.

```bash
# bytecode_hex: Hex-encoded bytecode (from compilation)
# captures_json: JSON array of captured variable descriptors
block_id=$(_register_bytecode_block "$bytecode_hex" "$captures_json")
```

### `_invoke_bytecode_block`

Execute a registered block with arguments.

```bash
result=$(_invoke_bytecode_block "$block_id" "arg1" "arg2")

# Special exit codes:
# 0   - Success, result on stdout
# 1   - Error, message on stderr
# 200 - Fallback to Bash (unsupported operation)
```

### `_serialize_bytecode_block`

Serialize a block for storage or transport.

```bash
data=$(_serialize_bytecode_block "$block_id")
# Returns JSON: {"block_data": "...", "captures": [...]}
```

## Capture Semantics

Bytecode blocks capture variables by reference:

```smalltalk
method: example [
    | count |
    count := 0.
    block := [ count := count + 1 ].
    block value.
    block value.
    count "=> 2, captures saw the changes"
]
```

### Capture Types

| Source | Behavior |
|--------|----------|
| Local | Changes visible in enclosing scope |
| Instance Variable | Written back to SQLite immediately |
| Parameter | Read-only (assignment creates new local) |

## Supported Operations

### Arithmetic
- `+`, `-`, `*`, `/`, `%` (modulo)
- Works with integers and floats

### Comparison
- `<`, `<=`, `>`, `>=`, `==`, `!=`

### Control Flow
- `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`
- `whileTrue:`, `whileFalse:`
- `timesRepeat:`
- `to:do:`, `to:by:do:`

### Collections
- `do:` iteration
- `select:`, `collect:`

### String Operations
- Concatenation (`,`)
- String interpolation

## Unsupported Operations (Fallback to Bash)

- Process spawning (`Process run:`)
- File I/O
- Network operations
- External command execution
- Complex shell features (pipes, redirects)

When bytecode encounters an unsupported operation, it returns exit code 200 and the runtime falls back to Bash execution transparently.

## Performance Characteristics

Typical benchmark results (Apple M3):

| Operation | Time |
|-----------|------|
| Simple arithmetic | ~1.8μs |
| 10-iteration loop | ~5μs |
| 100-iteration loop | ~11μs |
| 1000-iteration loop | ~113μs |
| Block compilation | ~0.7-1.2μs |
| Serialization | ~45ns |
| Deserialization | ~90-115ns |

Compare to Bash blocks where each iteration may spawn subprocesses at ~1-10ms each.

## Troubleshooting

### "daemon not available"

Ensure NativeDaemon is running:
```bash
@ NativeDaemon instance
```

Check socket exists:
```bash
ls -la "$_NATIVE_DAEMON_SOCKET"
```

### Block not found

Blocks are transient - they're lost when the daemon restarts. For persistent blocks, serialize and re-register them.

### Unexpected fallback to Bash

Check the daemon logs for unsupported operations. The daemon returns exit code 200 when it can't execute an operation natively.

## Best Practices

1. **Start daemon early**: Initialize NativeDaemon at the start of your session
2. **Use simple blocks**: Pure computation benefits most from bytecode
3. **Avoid I/O in hot loops**: Move file operations outside loops
4. **Monitor fallbacks**: Frequent fallbacks indicate blocks that could be optimized

## See Also

- `docs/BYTECODE_BLOCK_IMPLEMENTATION_PLAN.md` - Full implementation details
- `docs/block-compilation-plan.md` - Compilation pipeline design
- `pkg/bytecode/doc.go` - Go package documentation
