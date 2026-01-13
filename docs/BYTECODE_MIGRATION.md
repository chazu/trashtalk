# Migration Guide: Bash Blocks to Bytecode Blocks

This guide helps you transition existing Trashtalk code to take advantage of bytecode block execution.

## Overview

Bytecode blocks are an optional performance enhancement. Your existing code continues to work without changes. However, understanding which patterns benefit from bytecode execution can help you optimize performance-critical code.

## Prerequisites

Enable bytecode execution by starting the NativeDaemon:

```bash
source lib/trash.bash
daemon=$(@ NativeDaemon instance)
```

## Automatic vs Manual Migration

### Automatic (Recommended)

Most blocks automatically benefit from bytecode execution when:
1. The NativeDaemon is running
2. The block uses only supported operations

No code changes required. The runtime transparently uses bytecode when available.

### Manual Optimization

For maximum performance, you can restructure code to minimize fallbacks to Bash.

## Patterns That Benefit Most

### 1. Numeric Loops

**Before (Bash execution):**
```smalltalk
method: sum: n [
    | total i |
    total := 0.
    i := 1.
    [ i <= n ] whileTrue: [
        total := total + i.
        i := i + 1.
    ].
    ^ total
]
```

**After (identical, but runs 10-100x faster with bytecode):**
The exact same code runs natively when the daemon is available.

### 2. Collection Iteration

**Before:**
```smalltalk
method: processAll: items [
    items do: [ :item |
        result := result + (item * 2)
    ]
]
```

**After (same code, automatic bytecode):**
Pure computation in `do:` blocks executes natively.

### 3. Conditionals

**Before:**
```smalltalk
method: classify: x [
    x > 0 ifTrue: [
        ^ 'positive'
    ] ifFalse: [
        ^ 'non-positive'
    ]
]
```

**After (same code, automatic bytecode):**
Conditionals compile to efficient jump instructions.

## Patterns That Don't Benefit

### 1. I/O Operations

```smalltalk
method: readAndProcess [
    | content |
    content := File read: '/path/to/file'.  " Falls back to Bash "
    ^ content size
]
```

**Recommendation:** Keep I/O outside hot loops:

```smalltalk
method: processFile: path [
    | content |
    content := File read: path.  " I/O once, in Bash "
    ^ self processContent: content  " Pure computation, bytecode "
]

method: processContent: content [
    | result |
    result := 0.
    content lines do: [ :line |
        result := result + (line size)  " Bytecode execution "
    ].
    ^ result
]
```

### 2. Process Spawning

```smalltalk
method: runCommand [
    ^ Process run: 'ls -la'  " Always Bash "
]
```

**Recommendation:** Batch external commands:

```smalltalk
method: processFiles: paths [
    | allOutput results |
    " One Bash call to get all data "
    allOutput := Process run: ('ls -la ' , (paths join: ' ')).

    " Pure computation in bytecode "
    results := allOutput lines collect: [ :line |
        self parseLine: line
    ].
    ^ results
]
```

### 3. Complex String Interpolation

```smalltalk
method: formatOutput: data [
    ^ 'Result: ' , data , ' (computed at ' , Time now , ')'
]
```

If `Time now` requires a system call, this falls back to Bash. Pure string concatenation uses bytecode.

## Performance Comparison

| Operation | Bash | Bytecode | Speedup |
|-----------|------|----------|---------|
| Simple arithmetic | ~1-10ms | ~1-2μs | 500-5000x |
| 100-iteration loop | ~100ms-1s | ~11μs | 9000-90000x |
| String concat | ~1ms | ~3μs | 300x |
| Method dispatch | varies | varies | 2-10x |

## Migration Checklist

### Step 1: Enable Daemon

Add to your initialization:
```bash
source lib/trash.bash
@ NativeDaemon instance
```

### Step 2: Profile Current Code

Identify slow blocks by adding timing:
```smalltalk
method: myMethod [
    | start result |
    start := Time milliseconds.
    result := self doExpensiveWork.
    Transcript show: 'Took: ' , (Time milliseconds - start) , 'ms'.
    ^ result
]
```

### Step 3: Separate I/O from Computation

Move file/network/process operations to the boundaries. Keep inner loops pure.

### Step 4: Verify Behavior

Run your test suite. Bytecode execution should produce identical results.

### Step 5: Monitor Fallbacks

Check daemon logs for fallback notifications. Each fallback indicates an unsupported operation.

## Common Issues

### Variable Shadowing

**Problem:** Block doesn't modify outer variable as expected.

```smalltalk
method: broken [
    | count |
    count := 0.
    self items do: [ :item |
        count := count + 1  " Creates new local in bytecode! "
    ].
    ^ count  " Still 0 "
]
```

**Solution:** This is a known semantic difference. Use instance variables for mutation across block boundaries:

```smalltalk
method: working [
    _count := 0.
    self items do: [ :item |
        _count := _count + 1  " Instance var, properly captured "
    ].
    ^ _count
]
```

Or use explicit return:

```smalltalk
method: alsoWorking [
    | count |
    count := 0.
    self items inject: count into: [ :acc :item |
        acc + 1
    ]
]
```

### Daemon Not Available

**Symptom:** Performance not improved.

**Check:**
```bash
@ NativeDaemon instance
# Should return instance ID, not error
```

### Unexpected Results

**Symptom:** Different behavior with/without daemon.

**Debug:**
1. Check for unsupported operations causing fallback
2. Look for variable shadowing issues
3. Verify capture semantics match expectations

## Gradual Migration Strategy

1. **Week 1:** Enable daemon in development, run tests
2. **Week 2:** Profile and identify hot spots
3. **Week 3:** Refactor I/O to boundaries where beneficial
4. **Week 4:** Enable daemon in production with monitoring

## Rollback

If issues arise, simply don't start the daemon. All code falls back to pure Bash execution automatically.

```bash
source lib/trash.bash
# Don't call: @ NativeDaemon instance
# Everything runs in Bash as before
```

## See Also

- `docs/BYTECODE_BLOCKS.md` - User guide
- `pkg/bytecode/README.md` - Developer guide
- `docs/BYTECODE_BLOCK_IMPLEMENTATION_PLAN.md` - Design details
