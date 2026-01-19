# Procyon Shared Runtime Architecture Plan

## Overview

Migrate from hub-and-spoke plugin architecture to IDST-style shared runtime where all native code participates equally in a unified object space.

## Current State

```
Bash Runtime (lib/trash.bash)
      │
      ▼ (Unix socket / JSON)
Daemon (tt)
      │
      ▼ (FFI / JSON)
Plugins (*.dylib)
  - GetClassName() → string
  - Dispatch(instance, selector, args) → JSON
```

**Problems:**
- Plugins cannot message each other directly
- Every cross-boundary call serializes to JSON
- Block invocation shells out to bash
- No shared object space

## Target Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  libtrashtalk.dylib                         │
│                  (Shared Runtime)                           │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ ObjectSpace  │  │ Dispatcher   │  │ BlockVM      │      │
│  │              │  │              │  │              │      │
│  │ instances{}  │  │ send()       │  │ registry{}   │      │
│  │ classes{}    │  │ lookup()     │  │ invoke()     │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ Persistence  │  │ BashBridge   │  │ Reflection   │      │
│  │              │  │              │  │              │      │
│  │ sqlite       │  │ fallback()   │  │ methods()    │      │
│  │ load/save    │  │ bashOnly     │  │ ivars()      │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
        ▲                ▲                ▲
        │                │                │
   ┌────┴────┐      ┌────┴────┐      ┌────┴────┐
   │ Counter │      │ GrpcClient     │ Array   │
   │ .dylib  │      │ .dylib  │      │ .dylib  │
   └─────────┘      └─────────┘      └─────────┘
```

## Shared Runtime API (C ABI)

```c
// === Object Space ===

// Register a class with the runtime (called from plugin init)
void TT_RegisterClass(
    const char* className,
    const char* superclass,
    const char** instanceVars,
    int numVars,
    TTMethodTable* methods
);

// Create new instance, returns instance ID
const char* TT_New(const char* className);

// Look up instance by ID, returns opaque pointer
TTInstance* TT_Lookup(const char* instanceId);

// Get/set instance variable (by index for speed)
TTValue TT_GetIvar(TTInstance* inst, int index);
void TT_SetIvar(TTInstance* inst, int index, TTValue val);


// === Message Dispatch ===

// Send message, returns result
TTValue TT_Send(
    const char* receiver,      // instance ID or class name
    const char* selector,
    TTValue* args,
    int numArgs
);

// Direct dispatch (when you have the instance pointer)
TTValue TT_SendDirect(
    TTInstance* receiver,
    const char* selector,
    TTValue* args,
    int numArgs
);


// === Blocks ===

// Register bytecode block, returns block ID
const char* TT_RegisterBlock(
    const uint8_t* bytecode,
    size_t bytecodeLen,
    TTCaptureCell** captures,
    int numCaptures
);

// Invoke block by ID
TTValue TT_InvokeBlock(
    const char* blockId,
    TTValue* args,
    int numArgs
);

// Invoke block directly (when you have pointer)
TTValue TT_InvokeBlockDirect(
    TTBlock* block,
    TTValue* args,
    int numArgs
);


// === Persistence ===

// Save instance to database
void TT_Persist(const char* instanceId);

// Load instance from database
TTInstance* TT_Load(const char* instanceId);


// === Bash Bridge ===

// Call through to bash runtime (for bashOnly methods)
const char* TT_BashFallback(
    const char* receiver,
    const char* selector,
    const char** args,
    int numArgs
);
```

## Value Representation

```c
typedef enum {
    TT_TYPE_NIL,
    TT_TYPE_INT,
    TT_TYPE_FLOAT,
    TT_TYPE_STRING,
    TT_TYPE_BOOL,
    TT_TYPE_INSTANCE,  // Reference to object
    TT_TYPE_BLOCK,     // Reference to block
    TT_TYPE_ARRAY,     // Native array
} TTValueType;

typedef struct {
    TTValueType type;
    union {
        int64_t intVal;
        double floatVal;
        const char* stringVal;
        TTInstance* instanceVal;
        TTBlock* blockVal;
        TTArray* arrayVal;
    };
} TTValue;
```

## Method Table Structure

```c
typedef TTValue (*TTMethodFunc)(TTInstance* self, TTValue* args, int numArgs);

typedef struct {
    const char* selector;
    TTMethodFunc impl;
    int numArgs;
    uint32_t flags;  // NATIVE, BASH_FALLBACK, etc.
} TTMethodEntry;

typedef struct {
    TTMethodEntry* instanceMethods;
    int numInstanceMethods;
    TTMethodEntry* classMethods;
    int numClassMethods;
} TTMethodTable;
```

## Implementation Phases

### Phase 1: Shared Runtime Library

**Goal:** Create libtrashtalk.dylib with core runtime APIs

**Files to create:**
```
lib/runtime/
├── runtime.go          # Main runtime state
├── objectspace.go      # Instance/class registry
├── dispatch.go         # Message send implementation
├── values.go           # TTValue type and conversions
├── blocks.go           # Block registry, VM integration
├── persistence.go      # SQLite integration
├── bridge.go           # Bash fallback mechanism
├── capi.go             # CGO exports (C ABI)
└── capi.h              # C header for plugins
```

**Key tasks:**
1. Extract bytecode VM from daemon into runtime package
2. Implement ObjectSpace with concurrent-safe maps
3. Implement TT_Send dispatch with method lookup
4. Implement TT_RegisterClass for plugin init
5. Build as c-shared: `go build -buildmode=c-shared`

**Deliverable:** `libtrashtalk.dylib` + `libtrashtalk.h`


### Phase 2: Update Procyon Codegen

**Goal:** Generate plugins that use shared runtime instead of standalone dispatch

**Current generated plugin structure:**
```go
type Counter struct { ... }
func (c *Counter) increment() { ... }

//export Dispatch
func Dispatch(instance, selector, args *C.char) *C.char {
    // Local dispatch switch
}
```

**New generated plugin structure:**
```go
// #cgo LDFLAGS: -L${SRCDIR} -ltrashtalk
// #include "libtrashtalk.h"
import "C"

type Counter struct { ... }

func init() {
    // Register class with runtime at load time
    methods := []C.TTMethodEntry{
        {selector: "increment", impl: C.counter_increment, numArgs: 0},
        {selector: "getValue", impl: C.counter_getValue, numArgs: 0},
    }
    C.TT_RegisterClass("Counter", "Object", ivars, methods)
}

//export counter_increment
func counter_increment(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
    // Method implementation
    // Can call C.TT_Send() to message other objects
}
```

**Procyon changes:**
- `pkg/codegen/codegen.go`: Generate runtime-linked plugins
- `pkg/codegen/capi.go`: New file for C API code generation
- Remove standalone `dispatchInternal`, use runtime dispatch


### Phase 3: Update Daemon

**Goal:** Daemon becomes thin wrapper that initializes runtime and handles bash IPC

**New daemon role:**
1. Initialize shared runtime (`TT_Init()`)
2. Load plugins (which self-register via `init()`)
3. Handle Unix socket for bash communication
4. Translate bash requests to `TT_Send()` calls
5. Translate responses back to JSON for bash

**Daemon no longer:**
- Maintains its own plugin registry (runtime does this)
- Has its own dispatch logic (runtime does this)
- Has its own block VM (runtime does this)

```go
func main() {
    // Initialize shared runtime
    C.TT_Init(dbPath, pluginDir)

    // Load all plugins (they self-register)
    loadPlugins(pluginDir)

    // Handle bash requests via socket
    for req := range socketRequests {
        result := C.TT_Send(req.Receiver, req.Selector, req.Args)
        respond(result)
    }
}
```


### Phase 4: Native Object References

**Goal:** Pass object references directly between plugins, no serialization

**Current:** Instance passed as JSON string on every call
```go
Dispatch(`{"class":"Counter","value":42}`, "increment", "[]")
```

**New:** Instance is a pointer, only serialize at bash boundary
```go
// Within native code - direct pointer
counter := C.TT_Lookup("counter_abc123")
C.TT_SendDirect(counter, "increment", nil, 0)

// At bash boundary - serialize
json := C.TT_Serialize(counter)
```

**Benefits:**
- No JSON parse/serialize for native-to-native calls
- Direct ivar access via pointer
- Method dispatch is just function pointer call


### Phase 5: Native Block Callbacks

**Goal:** gRPC streaming with zero-overhead block invocation

**GrpcClient.serverStream implementation:**
```go
func (c *GrpcClient) serverStream(method, payload, blockId string) string {
    block := C.TT_LookupBlock(blockId)  // Get block pointer

    stream := openGrpcStream(method, payload)
    for event := range stream {
        // Direct block invocation - no IPC, no bash
        args := []C.TTValue{{type: TT_TYPE_STRING, stringVal: event}}
        C.TT_InvokeBlockDirect(block, &args[0], 1)
    }
}
```


### Phase 6: Cross-Plugin Messaging

**Goal:** Any plugin can message any object

**Example: GrpcClient creating an Event and sending to EventDispatcher**
```go
// In GrpcClient plugin
func handleStreamEvent(jsonData string) {
    // Create Event instance (from another plugin!)
    event := C.TT_Send("Yutani::Event", "fromJson:", jsonData)

    // Send to dispatcher (another plugin!)
    C.TT_Send(dispatcherId, "dispatch:", event)
}
```

No bash, no daemon mediation - direct native calls between plugins.


## Migration Strategy

### Backward Compatibility

1. **Bash runtime unchanged** - lib/trash.bash continues to work
2. **bashOnly methods** - Runtime calls back to bash via TT_BashFallback
3. **Existing .trash files** - No changes needed
4. **Gradual plugin migration** - Old-style plugins can coexist

### Testing Plan

1. **Unit tests for runtime** - ObjectSpace, dispatch, blocks
2. **Integration tests** - Plugin loading, cross-plugin messaging
3. **Compatibility tests** - Same behavior as current bash+daemon
4. **Performance benchmarks** - Compare native vs current

### Rollout

1. Build and test libtrashtalk.dylib standalone
2. Update one simple plugin (Counter) as proof of concept
3. Update Procyon codegen to generate new-style plugins
4. Rebuild all plugins
5. Update daemon to use runtime
6. Performance validation
7. Remove old dispatch code


## File Changes Summary

### New Files
```
lib/runtime/           # Shared runtime implementation
├── runtime.go
├── objectspace.go
├── dispatch.go
├── values.go
├── blocks.go
├── persistence.go
├── bridge.go
├── capi.go
└── capi.h
```

### Modified Files
```
cmd/tt/main.go                # Thin wrapper around runtime
pkg/codegen/codegen.go        # Generate runtime-linked plugins
pkg/codegen/plugin.go         # New plugin generation logic
Makefile                      # Build libtrashtalk.dylib
```

### Unchanged
```
lib/trash.bash               # Bash runtime
lib/jq-compiler/             # Compiler toolchain
pkg/bytecode/                # VM (extracted into runtime)
pkg/parser/                  # Parser
trash/*.trash                # Source files
```


## Performance Expectations

| Operation | Current | With Shared Runtime |
|-----------|---------|---------------------|
| Native method call | ~50μs (JSON + FFI) | ~0.1μs (direct) |
| Cross-plugin message | ~100μs (daemon mediated) | ~0.2μs (direct) |
| Block invocation | ~5ms (bash subprocess) | ~1μs (native VM) |
| gRPC event callback | ~10ms (bash round-trip) | ~2μs (native) |


## Open Questions

1. **Thread safety**: How to handle concurrent access to ObjectSpace?
   - Proposal: RWMutex for registry, per-instance locks for mutation

2. **Memory management**: When to free instances?
   - Proposal: Reference counting + explicit release at bash boundary

3. **Error handling**: How to propagate errors across C ABI?
   - Proposal: TT_GetLastError() + error codes in TTValue

4. **Hot reloading**: Can plugins be reloaded?
   - Proposal: Phase 2 - version stamps, graceful migration
