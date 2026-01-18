# Bytecode-Based Block Execution for Procyon

## Executive Summary

This document provides a detailed implementation plan for adding bytecode-based native block execution to Procyon. The goal is to enable blocks to execute natively in Go without shelling out to Bash, while maintaining full backward compatibility with the existing Bash runtime.

### Design Decisions (from user input)
1. **Capture semantics**: Reference capture - changes to captured variables should be visible in the block
2. **Recursive blocks**: Supported (but may be dropped if burdensome)
3. **Runtime compilation from strings**: Nice-to-have, not critical path
4. **Debugging**: Printf-style initially, interactive debugger later

---

## Phase 0: Preparation

### Goals
- Refactor existing code to prepare for bytecode integration
- Create foundational abstractions that both Bash and native backends will share
- Establish testing infrastructure for the new bytecode system

### Deliverables
1. Block interface abstraction in runtime package
2. Test harness for bytecode execution
3. Documentation of current block behavior for compatibility verification

### Files to Create

#### `/Users/chazu/dev/go/procyon/pkg/bytecode/doc.go`
```go
// Package bytecode provides a stack-based virtual machine for executing
// Trashtalk blocks natively in Go. This enables blocks to execute without
// shelling out to Bash, providing significant performance improvements.
//
// The bytecode format is designed for:
// - Compact representation (typically 2-4 bytes per instruction)
// - Fast decoding (fixed-width opcodes, simple operand formats)
// - Easy serialization (can be stored in SQLite or passed between processes)
//
// Key features:
// - Reference capture for closures (changes visible in block)
// - Cross-plugin invocation via shared memory or IPC
// - Full compatibility with existing Bash-based blocks
package bytecode
```

#### `/Users/chazu/dev/go/procyon/pkg/runtime/block.go`
```go
package runtime

// BlockKind indicates the execution mode of a block
type BlockKind int

const (
    BlockKindBash     BlockKind = iota // Execute via Bash runtime
    BlockKindBytecode                  // Execute via bytecode VM
    BlockKindNative                    // Inlined Go code (future)
)

// BlockInterface abstracts over different block implementations.
// This allows native code to work with blocks regardless of how they're stored.
type BlockInterface interface {
    // NumArgs returns the number of parameters the block expects
    NumArgs() int

    // ParamNames returns the parameter names in order
    ParamNames() []string

    // Kind returns how this block should be executed
    Kind() BlockKind

    // Invoke executes the block with the given arguments
    // The runtime context provides access to captured variables
    Invoke(ctx *BlockContext, args ...string) (string, error)
}

// BlockContext provides the execution context for a block.
// This includes access to captured variables and the runtime.
type BlockContext struct {
    Runtime      *Runtime
    InstanceID   string                 // The receiver that created this block
    Captured     map[string]*CapturedVar // Reference-captured variables
    ParentFrame  *BlockContext          // For nested blocks
}

// CapturedVar represents a mutable captured variable.
// Changes made inside a block are visible outside.
type CapturedVar struct {
    Name   string
    Value  string
    Source VarSource // Where to persist changes
}

// VarSource indicates where a captured variable comes from
type VarSource int

const (
    VarSourceLocal    VarSource = iota // Local variable (in-memory only)
    VarSourceIVar                      // Instance variable (persisted to SQLite)
    VarSourceGlobal                    // Global/class variable
)
```

### Files to Modify

#### `/Users/chazu/dev/go/procyon/pkg/runtime/runtime.go`
Add block registry and context management:
```go
// Add to Runtime struct:
type Runtime struct {
    // ... existing fields ...

    // Block management
    blockRegistry  map[string]BlockInterface  // blockID -> block
    blockRegistryMu sync.RWMutex
}

// RegisterBlock registers a block implementation for invocation
func (r *Runtime) RegisterBlock(id string, block BlockInterface) {
    r.blockRegistryMu.Lock()
    defer r.blockRegistryMu.Unlock()
    r.blockRegistry[id] = block
}

// InvokeBlock invokes a registered block
func (r *Runtime) InvokeBlock(id string, args ...string) (string, error) {
    r.blockRegistryMu.RLock()
    block, ok := r.blockRegistry[id]
    r.blockRegistryMu.RUnlock()

    if !ok {
        // Fall back to Bash for unregistered blocks
        return r.invokeBashBlock(id, args...)
    }

    ctx := &BlockContext{
        Runtime:    r,
        InstanceID: extractInstanceFromBlockID(id),
        Captured:   make(map[string]*CapturedVar),
    }

    return block.Invoke(ctx, args...)
}
```

### Testing Strategy
1. Create test fixtures with various block patterns:
   - Zero-arg blocks `[body]`
   - Single-arg blocks `[:x | body]`
   - Multi-arg blocks `[:x :y | body]`
   - Blocks with local variables
   - Nested blocks
   - Blocks capturing instance variables
2. Verify that existing Bash block execution still works after refactoring
3. Establish performance baseline with benchmarks

### Dependencies
- None (this is the foundation phase)

---

## Phase 1: Core Bytecode Infrastructure

### Goals
- Define the complete bytecode instruction set
- Implement bytecode encoding/decoding
- Create the serialization format for cross-process transport

### Deliverables
1. Complete opcode definitions with encoding
2. Bytecode serialization to/from bytes
3. Constant pool implementation
4. Debug information structures

### Files to Create

#### `/Users/chazu/dev/go/procyon/pkg/bytecode/opcodes.go`
```go
package bytecode

// Opcode represents a bytecode instruction
type Opcode byte

const (
    // Stack manipulation (0x00-0x0F)
    OpNop     Opcode = 0x00 // No operation
    OpPop     Opcode = 0x01 // Pop top of stack
    OpDup     Opcode = 0x02 // Duplicate top of stack
    OpSwap    Opcode = 0x03 // Swap top two stack elements

    // Constants (0x10-0x1F)
    OpConst      Opcode = 0x10 // Push constant from pool: OpConst <index:u16>
    OpConstNil   Opcode = 0x11 // Push nil/empty string
    OpConstTrue  Opcode = 0x12 // Push "true"
    OpConstFalse Opcode = 0x13 // Push "false"
    OpConstZero  Opcode = 0x14 // Push "0"
    OpConstOne   Opcode = 0x15 // Push "1"
    OpConstEmpty Opcode = 0x16 // Push ""

    // Local variables (0x20-0x2F)
    OpLoadLocal    Opcode = 0x20 // Push local variable: OpLoadLocal <slot:u8>
    OpStoreLocal   Opcode = 0x21 // Pop and store to local: OpStoreLocal <slot:u8>
    OpLoadParam    Opcode = 0x22 // Push parameter: OpLoadParam <index:u8>

    // Captured variables (0x30-0x3F) - for reference capture
    OpLoadCapture  Opcode = 0x30 // Push captured variable: OpLoadCapture <index:u8>
    OpStoreCapture Opcode = 0x31 // Pop and store to capture: OpStoreCapture <index:u8>
    OpMakeCapture  Opcode = 0x32 // Create capture cell for variable

    // Instance variables (0x40-0x4F)
    OpLoadIVar   Opcode = 0x40 // Push ivar value: OpLoadIVar <name_index:u16>
    OpStoreIVar  Opcode = 0x41 // Pop and store to ivar: OpStoreIVar <name_index:u16>

    // Arithmetic (0x50-0x5F)
    OpAdd   Opcode = 0x50 // Pop two, push sum
    OpSub   Opcode = 0x51 // Pop two, push difference
    OpMul   Opcode = 0x52 // Pop two, push product
    OpDiv   Opcode = 0x53 // Pop two, push quotient
    OpMod   Opcode = 0x54 // Pop two, push remainder
    OpNeg   Opcode = 0x55 // Negate top of stack

    // Comparison (0x60-0x6F)
    OpEq    Opcode = 0x60 // Pop two, push "true"/"false"
    OpNe    Opcode = 0x61
    OpLt    Opcode = 0x62
    OpLe    Opcode = 0x63
    OpGt    Opcode = 0x64
    OpGe    Opcode = 0x65

    // String operations (0x70-0x7F)
    OpConcat Opcode = 0x70 // Concatenate top two strings
    OpStrLen Opcode = 0x71 // Get string length

    // Control flow (0x80-0x8F)
    OpJump       Opcode = 0x80 // Unconditional jump: OpJump <offset:i16>
    OpJumpTrue   Opcode = 0x81 // Jump if top is truthy: OpJumpTrue <offset:i16>
    OpJumpFalse  Opcode = 0x82 // Jump if top is falsy: OpJumpFalse <offset:i16>
    OpJumpNil    Opcode = 0x83 // Jump if top is nil/empty
    OpJumpNotNil Opcode = 0x84 // Jump if top is not nil/empty

    // Message sends (0x90-0x9F)
    OpSend      Opcode = 0x90 // Send message: OpSend <selector:u16> <argc:u8>
    OpSendSelf  Opcode = 0x91 // Send to self: OpSendSelf <selector:u16> <argc:u8>
    OpSendClass Opcode = 0x92 // Send to class: OpSendClass <class:u16> <selector:u16> <argc:u8>

    // Block operations (0xA0-0xAF)
    OpMakeBlock   Opcode = 0xA0 // Create block: OpMakeBlock <codeOffset:u16> <numCaptures:u8>
    OpInvokeBlock Opcode = 0xA1 // Invoke block on stack: OpInvokeBlock <argc:u8>
    OpBlockValue  Opcode = 0xA2 // Invoke with 0 args (common case)

    // Return (0xF0-0xFF)
    OpReturn      Opcode = 0xF0 // Return top of stack from block
    OpReturnNil   Opcode = 0xF1 // Return nil/empty string
    OpNonLocalRet Opcode = 0xF2 // Non-local return (exit enclosing method)

    // JSON operations (0xB0-0xBF)
    OpArrayPush     Opcode = 0xB0 // array arrayPush: value
    OpArrayAt       Opcode = 0xB1 // array arrayAt: index
    OpArrayLength   Opcode = 0xB2 // array arrayLength
    OpObjectAt      Opcode = 0xB3 // object objectAt: key
    OpObjectAtPut   Opcode = 0xB4 // object objectAt: key put: value
)

// OpcodeInfo provides metadata about each opcode
type OpcodeInfo struct {
    Name       string
    StackPop   int  // How many values popped from stack
    StackPush  int  // How many values pushed to stack
    OperandLen int  // Number of operand bytes
}

var opcodeInfo = map[Opcode]OpcodeInfo{
    OpNop:         {"NOP", 0, 0, 0},
    OpPop:         {"POP", 1, 0, 0},
    OpDup:         {"DUP", 1, 2, 0},
    OpSwap:        {"SWAP", 2, 2, 0},
    OpConst:       {"CONST", 0, 1, 2},
    OpConstNil:    {"CONST_NIL", 0, 1, 0},
    OpConstTrue:   {"CONST_TRUE", 0, 1, 0},
    OpConstFalse:  {"CONST_FALSE", 0, 1, 0},
    OpLoadLocal:   {"LOAD_LOCAL", 0, 1, 1},
    OpStoreLocal:  {"STORE_LOCAL", 1, 0, 1},
    OpLoadParam:   {"LOAD_PARAM", 0, 1, 1},
    OpLoadCapture: {"LOAD_CAPTURE", 0, 1, 1},
    OpStoreCapture:{"STORE_CAPTURE", 1, 0, 1},
    OpLoadIVar:    {"LOAD_IVAR", 0, 1, 2},
    OpStoreIVar:   {"STORE_IVAR", 1, 0, 2},
    OpAdd:         {"ADD", 2, 1, 0},
    OpSub:         {"SUB", 2, 1, 0},
    OpMul:         {"MUL", 2, 1, 0},
    OpDiv:         {"DIV", 2, 1, 0},
    OpMod:         {"MOD", 2, 1, 0},
    OpNeg:         {"NEG", 1, 1, 0},
    OpEq:          {"EQ", 2, 1, 0},
    OpNe:          {"NE", 2, 1, 0},
    OpLt:          {"LT", 2, 1, 0},
    OpLe:          {"LE", 2, 1, 0},
    OpGt:          {"GT", 2, 1, 0},
    OpGe:          {"GE", 2, 1, 0},
    OpConcat:      {"CONCAT", 2, 1, 0},
    OpJump:        {"JUMP", 0, 0, 2},
    OpJumpTrue:    {"JUMP_TRUE", 1, 0, 2},
    OpJumpFalse:   {"JUMP_FALSE", 1, 0, 2},
    OpSend:        {"SEND", -1, 1, 3}, // Variable stack effect
    OpSendSelf:    {"SEND_SELF", -1, 1, 3},
    OpMakeBlock:   {"MAKE_BLOCK", -1, 1, 3}, // Pops captures, pushes block
    OpInvokeBlock: {"INVOKE_BLOCK", -1, 1, 1}, // Pops block + args, pushes result
    OpBlockValue:  {"BLOCK_VALUE", 1, 1, 0},
    OpReturn:      {"RETURN", 1, 0, 0},
    OpReturnNil:   {"RETURN_NIL", 0, 0, 0},
    // ... more entries
}
```

#### `/Users/chazu/dev/go/procyon/pkg/bytecode/chunk.go`
```go
package bytecode

import (
    "encoding/binary"
    "fmt"
)

// Chunk represents compiled bytecode for a block or method.
type Chunk struct {
    // Header
    Version    uint16  // Bytecode format version
    Flags      uint16  // Compilation flags

    // Code section
    Code       []byte  // Bytecode instructions

    // Constant pool
    Constants  []string // String constants

    // Metadata
    ParamCount  uint8    // Number of parameters
    ParamNames  []string // Parameter names (for debugging/reflection)
    LocalCount  uint8    // Number of local variable slots
    CaptureInfo []CaptureDescriptor // Captured variable descriptors

    // Debug info (optional)
    SourceMap  []SourceLocation // Bytecode offset -> source location
    VarNames   []string         // Local variable names for debugging
}

// CaptureDescriptor describes a captured variable
type CaptureDescriptor struct {
    Name      string    // Variable name
    Source    VarSource // Where the variable comes from
    SlotIndex uint8     // Slot in enclosing scope (for nested blocks)
}

type VarSource uint8

const (
    VarSourceLocal   VarSource = 0 // Local in enclosing scope
    VarSourceIVar    VarSource = 1 // Instance variable
    VarSourceCapture VarSource = 2 // Already captured (for nested blocks)
)

// SourceLocation maps bytecode position to source
type SourceLocation struct {
    BytecodeOffset uint32
    Line           uint32
    Column         uint16
}

// Serialize encodes the chunk to bytes for storage/transport
func (c *Chunk) Serialize() ([]byte, error) {
    // Format:
    // [magic:4] [version:2] [flags:2]
    // [code_len:4] [code:...]
    // [const_count:2] [constants:...]
    // [param_count:1] [param_names:...]
    // [local_count:1]
    // [capture_count:1] [captures:...]
    // [debug_present:1] [debug_info:...]

    buf := make([]byte, 0, len(c.Code)+1024)

    // Magic number: "TTBC" (TrashTalk ByteCode)
    buf = append(buf, 'T', 'T', 'B', 'C')

    // Version and flags
    buf = binary.BigEndian.AppendUint16(buf, c.Version)
    buf = binary.BigEndian.AppendUint16(buf, c.Flags)

    // Code section
    buf = binary.BigEndian.AppendUint32(buf, uint32(len(c.Code)))
    buf = append(buf, c.Code...)

    // Constants
    buf = binary.BigEndian.AppendUint16(buf, uint16(len(c.Constants)))
    for _, s := range c.Constants {
        buf = binary.BigEndian.AppendUint16(buf, uint16(len(s)))
        buf = append(buf, s...)
    }

    // Parameters
    buf = append(buf, c.ParamCount)
    for _, name := range c.ParamNames {
        buf = append(buf, byte(len(name)))
        buf = append(buf, name...)
    }

    // Locals
    buf = append(buf, c.LocalCount)

    // Captures
    buf = append(buf, byte(len(c.CaptureInfo)))
    for _, cap := range c.CaptureInfo {
        buf = append(buf, byte(len(cap.Name)))
        buf = append(buf, cap.Name...)
        buf = append(buf, byte(cap.Source))
        buf = append(buf, cap.SlotIndex)
    }

    return buf, nil
}

// Deserialize decodes a chunk from bytes
func Deserialize(data []byte) (*Chunk, error) {
    if len(data) < 8 {
        return nil, fmt.Errorf("bytecode too short")
    }

    // Check magic
    if string(data[0:4]) != "TTBC" {
        return nil, fmt.Errorf("invalid bytecode magic")
    }

    c := &Chunk{
        Version: binary.BigEndian.Uint16(data[4:6]),
        Flags:   binary.BigEndian.Uint16(data[6:8]),
    }

    pos := 8

    // Code section
    codeLen := binary.BigEndian.Uint32(data[pos:])
    pos += 4
    c.Code = make([]byte, codeLen)
    copy(c.Code, data[pos:pos+int(codeLen)])
    pos += int(codeLen)

    // Constants
    constCount := binary.BigEndian.Uint16(data[pos:])
    pos += 2
    c.Constants = make([]string, constCount)
    for i := range c.Constants {
        strLen := binary.BigEndian.Uint16(data[pos:])
        pos += 2
        c.Constants[i] = string(data[pos : pos+int(strLen)])
        pos += int(strLen)
    }

    // Parameters
    c.ParamCount = data[pos]
    pos++
    c.ParamNames = make([]string, c.ParamCount)
    for i := range c.ParamNames {
        nameLen := data[pos]
        pos++
        c.ParamNames[i] = string(data[pos : pos+int(nameLen)])
        pos += int(nameLen)
    }

    // Locals
    c.LocalCount = data[pos]
    pos++

    // Captures
    capCount := data[pos]
    pos++
    c.CaptureInfo = make([]CaptureDescriptor, capCount)
    for i := range c.CaptureInfo {
        nameLen := data[pos]
        pos++
        c.CaptureInfo[i].Name = string(data[pos : pos+int(nameLen)])
        pos += int(nameLen)
        c.CaptureInfo[i].Source = VarSource(data[pos])
        pos++
        c.CaptureInfo[i].SlotIndex = data[pos]
        pos++
    }

    return c, nil
}
```

#### `/Users/chazu/dev/go/procyon/pkg/bytecode/disasm.go`
```go
package bytecode

import (
    "fmt"
    "strings"
)

// Disassemble returns human-readable bytecode listing
func (c *Chunk) Disassemble() string {
    var sb strings.Builder

    sb.WriteString(fmt.Sprintf("; Trashtalk Bytecode v%d\n", c.Version))
    sb.WriteString(fmt.Sprintf("; %d params, %d locals, %d captures\n\n",
        c.ParamCount, c.LocalCount, len(c.CaptureInfo)))

    // Constants
    if len(c.Constants) > 0 {
        sb.WriteString("; Constants:\n")
        for i, s := range c.Constants {
            sb.WriteString(fmt.Sprintf(";   [%d] %q\n", i, s))
        }
        sb.WriteString("\n")
    }

    // Captures
    if len(c.CaptureInfo) > 0 {
        sb.WriteString("; Captures:\n")
        for i, cap := range c.CaptureInfo {
            sb.WriteString(fmt.Sprintf(";   [%d] %s (source=%d, slot=%d)\n",
                i, cap.Name, cap.Source, cap.SlotIndex))
        }
        sb.WriteString("\n")
    }

    // Code
    sb.WriteString("; Code:\n")
    offset := 0
    for offset < len(c.Code) {
        line := c.disassembleInstruction(offset)
        sb.WriteString(line)
        sb.WriteString("\n")
        offset += c.instructionSize(Opcode(c.Code[offset]))
    }

    return sb.String()
}

func (c *Chunk) disassembleInstruction(offset int) string {
    op := Opcode(c.Code[offset])
    info := opcodeInfo[op]

    switch op {
    case OpConst:
        idx := binary.BigEndian.Uint16(c.Code[offset+1:])
        return fmt.Sprintf("%04x  CONST %d ; %q", offset, idx, c.Constants[idx])
    case OpLoadLocal, OpStoreLocal:
        slot := c.Code[offset+1]
        return fmt.Sprintf("%04x  %s %d", offset, info.Name, slot)
    case OpLoadParam:
        idx := c.Code[offset+1]
        name := ""
        if int(idx) < len(c.ParamNames) {
            name = " ; " + c.ParamNames[idx]
        }
        return fmt.Sprintf("%04x  LOAD_PARAM %d%s", offset, idx, name)
    case OpJump, OpJumpTrue, OpJumpFalse:
        delta := int16(binary.BigEndian.Uint16(c.Code[offset+1:]))
        target := offset + 3 + int(delta)
        return fmt.Sprintf("%04x  %s %d (-> %04x)", offset, info.Name, delta, target)
    case OpSend:
        selIdx := binary.BigEndian.Uint16(c.Code[offset+1:])
        argc := c.Code[offset+3]
        return fmt.Sprintf("%04x  SEND %d (%s) argc=%d", offset, selIdx, c.Constants[selIdx], argc)
    default:
        return fmt.Sprintf("%04x  %s", offset, info.Name)
    }
}

func (c *Chunk) instructionSize(op Opcode) int {
    return 1 + opcodeInfo[op].OperandLen
}
```

### Testing Strategy
1. Round-trip serialization tests (serialize -> deserialize -> compare)
2. Disassembly tests for human verification
3. Opcode coverage tests

### Dependencies
- Phase 0 (runtime abstractions)

---

## Phase 2: Bytecode Compiler (AST to Bytecode)

### Goals
- Convert parser AST (`parser.BlockExpr`) to bytecode chunks
- Implement capture analysis to identify free variables
- Generate correct bytecode for all block constructs

### Deliverables
1. Block compiler that converts `parser.BlockExpr` to `Chunk`
2. Capture analysis pass
3. Source map generation for debugging

### Files to Create

#### `/Users/chazu/dev/go/procyon/pkg/bytecode/compiler.go`
```go
package bytecode

import (
    "fmt"

    "github.com/chazu/procyon/pkg/parser"
)

// CompilerContext provides context for block compilation
type CompilerContext struct {
    // The enclosing method's variables
    MethodParams    []string
    MethodLocals    []string
    InstanceVars    []string
    ClassVars       []string

    // The instance ID for ivar access
    InstanceID      string

    // Parent block context (for nested blocks)
    Parent          *CompilerContext
}

// Compiler converts parser AST to bytecode
type Compiler struct {
    chunk      *Chunk
    context    *CompilerContext

    // Constant pool management
    constants  map[string]uint16

    // Variable slot allocation
    paramSlots   map[string]uint8  // Param name -> slot
    localSlots   map[string]uint8  // Local name -> slot
    captureSlots map[string]uint8  // Captured var name -> capture index

    // For nested blocks
    scopeDepth int

    // Error collection
    errors []string
}

// CompileBlock compiles a block expression to bytecode
func CompileBlock(block *parser.BlockExpr, ctx *CompilerContext) (*Chunk, error) {
    c := &Compiler{
        chunk: &Chunk{
            Version:    1,
            Code:       make([]byte, 0, 256),
            Constants:  []string{},
        },
        context:      ctx,
        constants:    make(map[string]uint16),
        paramSlots:   make(map[string]uint8),
        localSlots:   make(map[string]uint8),
        captureSlots: make(map[string]uint8),
    }

    // Analyze captures first
    captures := c.analyzeCaptures(block)

    // Allocate parameter slots
    for i, param := range block.Params {
        c.paramSlots[param] = uint8(i)
    }
    c.chunk.ParamCount = uint8(len(block.Params))
    c.chunk.ParamNames = append([]string{}, block.Params...)

    // Set up capture descriptors
    for i, cap := range captures {
        c.captureSlots[cap.Name] = uint8(i)
        c.chunk.CaptureInfo = append(c.chunk.CaptureInfo, cap)
    }

    // Compile statements
    for _, stmt := range block.Statements {
        if err := c.compileStatement(stmt); err != nil {
            return nil, err
        }
    }

    // Ensure block returns something
    if len(c.chunk.Code) == 0 || Opcode(c.chunk.Code[len(c.chunk.Code)-1]) != OpReturn {
        c.emit(OpReturnNil)
    }

    return c.chunk, nil
}

// analyzeCaptures identifies variables that need to be captured
func (c *Compiler) analyzeCaptures(block *parser.BlockExpr) []CaptureDescriptor {
    var captures []CaptureDescriptor
    freeVars := c.findFreeVariables(block)

    for _, name := range freeVars {
        // Determine source
        var source VarSource
        if c.isInstanceVar(name) {
            source = VarSourceIVar
        } else if c.isMethodLocal(name) || c.isMethodParam(name) {
            source = VarSourceLocal
        } else if c.context.Parent != nil && c.isParentCapture(name) {
            source = VarSourceCapture
        } else {
            continue // Not a captured variable
        }

        captures = append(captures, CaptureDescriptor{
            Name:   name,
            Source: source,
        })
    }

    return captures
}

// findFreeVariables returns all variable references not defined in the block
func (c *Compiler) findFreeVariables(block *parser.BlockExpr) []string {
    defined := make(map[string]bool)

    // Block parameters are defined
    for _, p := range block.Params {
        defined[p] = true
    }

    // Find all referenced variables
    var referenced []string
    for _, stmt := range block.Statements {
        c.collectReferences(stmt, defined, &referenced)
    }

    // Filter to unique free variables
    seen := make(map[string]bool)
    var result []string
    for _, name := range referenced {
        if !defined[name] && !seen[name] {
            seen[name] = true
            result = append(result, name)
        }
    }

    return result
}

func (c *Compiler) compileStatement(stmt parser.Statement) error {
    switch s := stmt.(type) {
    case *parser.Assignment:
        return c.compileAssignment(s)
    case *parser.Return:
        return c.compileReturn(s)
    case *parser.ExprStmt:
        if err := c.compileExpr(s.Expr); err != nil {
            return err
        }
        c.emit(OpPop) // Discard result
        return nil
    case *parser.IfExpr:
        return c.compileIf(s)
    case *parser.WhileExpr:
        return c.compileWhile(s)
    case *parser.IterationExpr:
        return c.compileIteration(s)
    case *parser.MessageSend:
        if err := c.compileExpr(s); err != nil {
            return err
        }
        c.emit(OpPop)
        return nil
    default:
        return fmt.Errorf("unsupported statement type: %T", stmt)
    }
}

func (c *Compiler) compileExpr(expr parser.Expr) error {
    switch e := expr.(type) {
    case *parser.NumberLit:
        c.emitConstant(e.Value)
    case *parser.StringLit:
        c.emitConstant(e.Value)
    case *parser.Identifier:
        return c.compileIdentifier(e)
    case *parser.BinaryExpr:
        return c.compileBinary(e)
    case *parser.ComparisonExpr:
        return c.compileComparison(e)
    case *parser.MessageSend:
        return c.compileMessageSend(e)
    case *parser.BlockExpr:
        return c.compileNestedBlock(e)
    default:
        return fmt.Errorf("unsupported expression type: %T", expr)
    }
    return nil
}

func (c *Compiler) compileIdentifier(id *parser.Identifier) error {
    name := id.Name

    // Check locals first
    if slot, ok := c.localSlots[name]; ok {
        c.emit(OpLoadLocal, slot)
        return nil
    }

    // Check parameters
    if slot, ok := c.paramSlots[name]; ok {
        c.emit(OpLoadParam, slot)
        return nil
    }

    // Check captures
    if slot, ok := c.captureSlots[name]; ok {
        c.emit(OpLoadCapture, slot)
        return nil
    }

    // Check instance variables
    if c.isInstanceVar(name) {
        idx := c.addConstant(name)
        c.emit(OpLoadIVar)
        c.emitUint16(idx)
        return nil
    }

    return fmt.Errorf("undefined variable: %s", name)
}

func (c *Compiler) compileAssignment(a *parser.Assignment) error {
    // Compile the value
    if err := c.compileExpr(a.Value); err != nil {
        return err
    }

    name := a.Target

    // Store to appropriate location
    if slot, ok := c.localSlots[name]; ok {
        c.emit(OpStoreLocal, slot)
        return nil
    }

    if slot, ok := c.captureSlots[name]; ok {
        c.emit(OpStoreCapture, slot) // This updates the captured cell
        return nil
    }

    if c.isInstanceVar(name) {
        idx := c.addConstant(name)
        c.emit(OpStoreIVar)
        c.emitUint16(idx)
        return nil
    }

    // New local variable
    slot := c.chunk.LocalCount
    c.chunk.LocalCount++
    c.localSlots[name] = slot
    c.emit(OpStoreLocal, slot)
    return nil
}

func (c *Compiler) compileMessageSend(m *parser.MessageSend) error {
    // Compile receiver
    if m.IsSelf {
        // For self sends, we'll handle specially
        // Load the instance ID from context
        c.emitConstant("self") // Placeholder - will be resolved at runtime
    } else {
        if err := c.compileExpr(m.Receiver); err != nil {
            return err
        }
    }

    // Compile arguments
    for _, arg := range m.Args {
        if err := c.compileExpr(arg); err != nil {
            return err
        }
    }

    // Emit send
    selIdx := c.addConstant(m.Selector)
    if m.IsSelf {
        c.emit(OpSendSelf)
    } else {
        c.emit(OpSend)
    }
    c.emitUint16(selIdx)
    c.emit(byte(len(m.Args)))

    return nil
}

func (c *Compiler) compileIf(ifExpr *parser.IfExpr) error {
    // Compile condition
    if err := c.compileExpr(ifExpr.Condition); err != nil {
        return err
    }

    // Jump over true block if false
    falseJump := c.emitJump(OpJumpFalse)

    // Compile true block
    for _, stmt := range ifExpr.TrueBlock {
        if err := c.compileStatement(stmt); err != nil {
            return err
        }
    }

    if len(ifExpr.FalseBlock) > 0 {
        // Jump over false block
        endJump := c.emitJump(OpJump)

        // Patch false jump to here
        c.patchJump(falseJump)

        // Compile false block
        for _, stmt := range ifExpr.FalseBlock {
            if err := c.compileStatement(stmt); err != nil {
                return err
            }
        }

        // Patch end jump
        c.patchJump(endJump)
    } else {
        c.patchJump(falseJump)
    }

    return nil
}

func (c *Compiler) compileNestedBlock(block *parser.BlockExpr) error {
    // Create nested compiler context
    nestedCtx := &CompilerContext{
        MethodParams: c.context.MethodParams,
        MethodLocals: c.context.MethodLocals,
        InstanceVars: c.context.InstanceVars,
        InstanceID:   c.context.InstanceID,
        Parent:       c.context,
    }

    // Compile nested block
    nestedChunk, err := CompileBlock(block, nestedCtx)
    if err != nil {
        return err
    }

    // Emit bytecode for nested block
    // Store the nested chunk in constants as serialized bytes
    chunkBytes, _ := nestedChunk.Serialize()
    chunkIdx := c.addConstant(string(chunkBytes))

    // Push captured values onto stack
    for _, cap := range nestedChunk.CaptureInfo {
        if err := c.compileIdentifier(&parser.Identifier{Name: cap.Name}); err != nil {
            return err
        }
    }

    // Create block object
    c.emit(OpMakeBlock)
    c.emitUint16(chunkIdx)
    c.emit(byte(len(nestedChunk.CaptureInfo)))

    return nil
}

// Helper methods

func (c *Compiler) emit(op Opcode, operands ...byte) {
    c.chunk.Code = append(c.chunk.Code, byte(op))
    c.chunk.Code = append(c.chunk.Code, operands...)
}

func (c *Compiler) emitUint16(val uint16) {
    c.chunk.Code = append(c.chunk.Code, byte(val>>8), byte(val))
}

func (c *Compiler) emitConstant(value string) {
    idx := c.addConstant(value)
    if idx == 0 && value == "" {
        c.emit(OpConstEmpty)
    } else if idx == 0 && value == "0" {
        c.emit(OpConstZero)
    } else if idx == 0 && value == "1" {
        c.emit(OpConstOne)
    } else {
        c.emit(OpConst)
        c.emitUint16(idx)
    }
}

func (c *Compiler) addConstant(value string) uint16 {
    if idx, ok := c.constants[value]; ok {
        return idx
    }
    idx := uint16(len(c.chunk.Constants))
    c.chunk.Constants = append(c.chunk.Constants, value)
    c.constants[value] = idx
    return idx
}

func (c *Compiler) emitJump(op Opcode) int {
    c.emit(op, 0xFF, 0xFF) // Placeholder
    return len(c.chunk.Code) - 2
}

func (c *Compiler) patchJump(offset int) {
    // Calculate jump distance
    jump := len(c.chunk.Code) - offset - 2
    c.chunk.Code[offset] = byte(jump >> 8)
    c.chunk.Code[offset+1] = byte(jump)
}

func (c *Compiler) isInstanceVar(name string) bool {
    for _, ivar := range c.context.InstanceVars {
        if ivar == name {
            return true
        }
    }
    return false
}

func (c *Compiler) isMethodParam(name string) bool {
    for _, p := range c.context.MethodParams {
        if p == name {
            return true
        }
    }
    return false
}

func (c *Compiler) isMethodLocal(name string) bool {
    for _, l := range c.context.MethodLocals {
        if l == name {
            return true
        }
    }
    return false
}

func (c *Compiler) isParentCapture(name string) bool {
    if c.context.Parent == nil {
        return false
    }
    // Check if parent has this variable in scope
    return true // Simplified - real impl checks parent scope
}

func (c *Compiler) collectReferences(stmt parser.Statement, defined map[string]bool, refs *[]string) {
    // Walk the AST collecting variable references
    // Implementation omitted for brevity
}
```

### Testing Strategy
1. Compile simple blocks and verify bytecode output
2. Test capture analysis with various nesting patterns
3. Verify correct slot allocation for locals/params/captures
4. Test source map generation

### Dependencies
- Phase 1 (bytecode format)

---

## Phase 3: Bytecode Interpreter (VM)

### Goals
- Implement the stack-based virtual machine
- Handle all opcodes defined in Phase 1
- Implement reference capture semantics

### Deliverables
1. Complete VM implementation
2. Capture cell implementation for reference semantics
3. Integration with runtime for ivar access

### Files to Create

#### `/Users/chazu/dev/go/procyon/pkg/bytecode/vm.go`
```go
package bytecode

import (
    "encoding/binary"
    "fmt"
    "strconv"

    "github.com/chazu/procyon/pkg/runtime"
)

// VM executes bytecode
type VM struct {
    // Current execution state
    chunk     *Chunk
    ip        int      // Instruction pointer
    stack     []string // Value stack
    sp        int      // Stack pointer

    // Variable storage
    locals    []string        // Local variable slots
    params    []string        // Parameter values
    captures  []*CaptureCell  // Captured variable cells

    // Runtime context
    runtime    *runtime.Runtime
    instanceID string          // For ivar access

    // Call stack for nested blocks
    frames     []*CallFrame
    frameDepth int

    // Debug
    trace      bool
}

// CaptureCell holds a captured variable with reference semantics
type CaptureCell struct {
    Value   string
    Source  VarSource
    Name    string      // For debugging and ivar writeback
    Closed  bool        // True if cell holds its own value (for long-lived closures)
}

// CallFrame represents an active block invocation
type CallFrame struct {
    chunk    *Chunk
    ip       int
    bp       int         // Base pointer into stack
    locals   []string
    captures []*CaptureCell
}

// NewVM creates a new VM for executing bytecode
func NewVM(rt *runtime.Runtime) *VM {
    return &VM{
        stack:   make([]string, 256),
        frames:  make([]*CallFrame, 64),
        runtime: rt,
    }
}

// Execute runs a bytecode chunk with the given arguments
func (vm *VM) Execute(chunk *Chunk, instanceID string, args []string) (string, error) {
    vm.chunk = chunk
    vm.ip = 0
    vm.sp = 0
    vm.instanceID = instanceID

    // Initialize parameters
    vm.params = args

    // Initialize locals
    vm.locals = make([]string, chunk.LocalCount)

    // Initialize captures (will be populated by OpMakeBlock or passed in)
    vm.captures = make([]*CaptureCell, len(chunk.CaptureInfo))

    return vm.run()
}

// ExecuteWithCaptures runs bytecode with pre-populated captures
func (vm *VM) ExecuteWithCaptures(chunk *Chunk, instanceID string, args []string, captures []*CaptureCell) (string, error) {
    vm.chunk = chunk
    vm.ip = 0
    vm.sp = 0
    vm.instanceID = instanceID
    vm.params = args
    vm.locals = make([]string, chunk.LocalCount)
    vm.captures = captures

    return vm.run()
}

func (vm *VM) run() (string, error) {
    for {
        if vm.ip >= len(vm.chunk.Code) {
            break
        }

        op := Opcode(vm.chunk.Code[vm.ip])
        vm.ip++

        if vm.trace {
            fmt.Printf("[%04x] %s (sp=%d)\n", vm.ip-1, opcodeInfo[op].Name, vm.sp)
        }

        switch op {
        // Stack operations
        case OpNop:
            // Do nothing

        case OpPop:
            vm.sp--

        case OpDup:
            vm.stack[vm.sp] = vm.stack[vm.sp-1]
            vm.sp++

        case OpSwap:
            vm.stack[vm.sp-1], vm.stack[vm.sp-2] = vm.stack[vm.sp-2], vm.stack[vm.sp-1]

        // Constants
        case OpConst:
            idx := vm.readUint16()
            vm.push(vm.chunk.Constants[idx])

        case OpConstNil, OpConstEmpty:
            vm.push("")

        case OpConstTrue:
            vm.push("true")

        case OpConstFalse:
            vm.push("false")

        case OpConstZero:
            vm.push("0")

        case OpConstOne:
            vm.push("1")

        // Local variables
        case OpLoadLocal:
            slot := vm.chunk.Code[vm.ip]
            vm.ip++
            vm.push(vm.locals[slot])

        case OpStoreLocal:
            slot := vm.chunk.Code[vm.ip]
            vm.ip++
            vm.locals[slot] = vm.pop()

        case OpLoadParam:
            idx := vm.chunk.Code[vm.ip]
            vm.ip++
            if int(idx) < len(vm.params) {
                vm.push(vm.params[idx])
            } else {
                vm.push("") // Missing param
            }

        // Captures (reference semantics)
        case OpLoadCapture:
            idx := vm.chunk.Code[vm.ip]
            vm.ip++
            cell := vm.captures[idx]
            if cell == nil {
                vm.push("")
            } else {
                vm.push(cell.Value)
            }

        case OpStoreCapture:
            idx := vm.chunk.Code[vm.ip]
            vm.ip++
            value := vm.pop()
            cell := vm.captures[idx]
            if cell != nil {
                cell.Value = value
                // If this is an ivar capture, write back to SQLite
                if cell.Source == VarSourceIVar && vm.runtime != nil {
                    vm.writeIVar(cell.Name, value)
                }
            }

        // Instance variables
        case OpLoadIVar:
            nameIdx := vm.readUint16()
            name := vm.chunk.Constants[nameIdx]
            value := vm.readIVar(name)
            vm.push(value)

        case OpStoreIVar:
            nameIdx := vm.readUint16()
            name := vm.chunk.Constants[nameIdx]
            value := vm.pop()
            vm.writeIVar(name, value)

        // Arithmetic
        case OpAdd:
            b := vm.popInt()
            a := vm.popInt()
            vm.pushInt(a + b)

        case OpSub:
            b := vm.popInt()
            a := vm.popInt()
            vm.pushInt(a - b)

        case OpMul:
            b := vm.popInt()
            a := vm.popInt()
            vm.pushInt(a * b)

        case OpDiv:
            b := vm.popInt()
            a := vm.popInt()
            if b == 0 {
                vm.push("0") // Division by zero
            } else {
                vm.pushInt(a / b)
            }

        case OpMod:
            b := vm.popInt()
            a := vm.popInt()
            if b == 0 {
                vm.push("0")
            } else {
                vm.pushInt(a % b)
            }

        case OpNeg:
            a := vm.popInt()
            vm.pushInt(-a)

        // Comparison
        case OpEq:
            b := vm.pop()
            a := vm.pop()
            vm.pushBool(a == b)

        case OpNe:
            b := vm.pop()
            a := vm.pop()
            vm.pushBool(a != b)

        case OpLt:
            b := vm.popInt()
            a := vm.popInt()
            vm.pushBool(a < b)

        case OpLe:
            b := vm.popInt()
            a := vm.popInt()
            vm.pushBool(a <= b)

        case OpGt:
            b := vm.popInt()
            a := vm.popInt()
            vm.pushBool(a > b)

        case OpGe:
            b := vm.popInt()
            a := vm.popInt()
            vm.pushBool(a >= b)

        // String operations
        case OpConcat:
            b := vm.pop()
            a := vm.pop()
            vm.push(a + b)

        // Control flow
        case OpJump:
            offset := vm.readInt16()
            vm.ip += int(offset)

        case OpJumpTrue:
            offset := vm.readInt16()
            cond := vm.pop()
            if vm.isTruthy(cond) {
                vm.ip += int(offset)
            }

        case OpJumpFalse:
            offset := vm.readInt16()
            cond := vm.pop()
            if !vm.isTruthy(cond) {
                vm.ip += int(offset)
            }

        case OpJumpNil:
            offset := vm.readInt16()
            val := vm.pop()
            if val == "" {
                vm.ip += int(offset)
            }

        case OpJumpNotNil:
            offset := vm.readInt16()
            val := vm.pop()
            if val != "" {
                vm.ip += int(offset)
            }

        // Message sends
        case OpSend:
            selIdx := vm.readUint16()
            argc := int(vm.chunk.Code[vm.ip])
            vm.ip++
            selector := vm.chunk.Constants[selIdx]

            // Pop args in reverse order
            args := make([]string, argc)
            for i := argc - 1; i >= 0; i-- {
                args[i] = vm.pop()
            }
            receiver := vm.pop()

            result, err := vm.runtime.SendMessage(receiver, selector, args...)
            if err != nil {
                return "", err
            }
            vm.push(result)

        case OpSendSelf:
            selIdx := vm.readUint16()
            argc := int(vm.chunk.Code[vm.ip])
            vm.ip++
            selector := vm.chunk.Constants[selIdx]

            args := make([]string, argc)
            for i := argc - 1; i >= 0; i-- {
                args[i] = vm.pop()
            }
            vm.pop() // Pop "self" placeholder

            result, err := vm.runtime.SendMessage(vm.instanceID, selector, args...)
            if err != nil {
                return "", err
            }
            vm.push(result)

        // Block operations
        case OpMakeBlock:
            chunkIdx := vm.readUint16()
            numCaptures := int(vm.chunk.Code[vm.ip])
            vm.ip++

            // Deserialize nested chunk
            chunkBytes := vm.chunk.Constants[chunkIdx]
            nestedChunk, err := Deserialize([]byte(chunkBytes))
            if err != nil {
                return "", err
            }

            // Create capture cells from stack values
            captures := make([]*CaptureCell, numCaptures)
            for i := numCaptures - 1; i >= 0; i-- {
                value := vm.pop()
                captures[i] = &CaptureCell{
                    Value:  value,
                    Source: nestedChunk.CaptureInfo[i].Source,
                    Name:   nestedChunk.CaptureInfo[i].Name,
                }
            }

            // Create block and push as "block:XXXX" ID
            blockID := vm.registerBlock(nestedChunk, captures)
            vm.push(blockID)

        case OpInvokeBlock:
            argc := int(vm.chunk.Code[vm.ip])
            vm.ip++

            // Pop args and block ID
            args := make([]string, argc)
            for i := argc - 1; i >= 0; i-- {
                args[i] = vm.pop()
            }
            blockID := vm.pop()

            result, err := vm.invokeBlock(blockID, args)
            if err != nil {
                return "", err
            }
            vm.push(result)

        case OpBlockValue:
            blockID := vm.pop()
            result, err := vm.invokeBlock(blockID, nil)
            if err != nil {
                return "", err
            }
            vm.push(result)

        // Return
        case OpReturn:
            return vm.pop(), nil

        case OpReturnNil:
            return "", nil

        default:
            return "", fmt.Errorf("unknown opcode: 0x%02x", op)
        }
    }

    // Implicit return of stack top or empty
    if vm.sp > 0 {
        return vm.stack[vm.sp-1], nil
    }
    return "", nil
}

// Stack helpers
func (vm *VM) push(val string) {
    vm.stack[vm.sp] = val
    vm.sp++
}

func (vm *VM) pop() string {
    vm.sp--
    return vm.stack[vm.sp]
}

func (vm *VM) popInt() int {
    s := vm.pop()
    n, _ := strconv.Atoi(s)
    return n
}

func (vm *VM) pushInt(n int) {
    vm.push(strconv.Itoa(n))
}

func (vm *VM) pushBool(b bool) {
    if b {
        vm.push("true")
    } else {
        vm.push("false")
    }
}

func (vm *VM) isTruthy(s string) bool {
    return s != "" && s != "false" && s != "0"
}

// Bytecode reading
func (vm *VM) readUint16() uint16 {
    val := binary.BigEndian.Uint16(vm.chunk.Code[vm.ip:])
    vm.ip += 2
    return val
}

func (vm *VM) readInt16() int16 {
    return int16(vm.readUint16())
}

// Instance variable access
func (vm *VM) readIVar(name string) string {
    if vm.runtime == nil || vm.instanceID == "" {
        return ""
    }
    instance, err := vm.runtime.LoadInstance(vm.instanceID)
    if err != nil {
        return ""
    }
    return instance.GetVarString(name)
}

func (vm *VM) writeIVar(name, value string) {
    if vm.runtime == nil || vm.instanceID == "" {
        return
    }
    instance, err := vm.runtime.LoadInstance(vm.instanceID)
    if err != nil {
        return
    }
    instance.SetVar(name, value)
    vm.runtime.SaveInstance(vm.instanceID, instance)
}

// Block management
var blockCounter int
var blockRegistry = make(map[string]*registeredBlock)

type registeredBlock struct {
    chunk    *Chunk
    captures []*CaptureCell
}

func (vm *VM) registerBlock(chunk *Chunk, captures []*CaptureCell) string {
    blockCounter++
    id := fmt.Sprintf("bytecode_block_%d", blockCounter)
    blockRegistry[id] = &registeredBlock{
        chunk:    chunk,
        captures: captures,
    }
    return id
}

func (vm *VM) invokeBlock(blockID string, args []string) (string, error) {
    block, ok := blockRegistry[blockID]
    if !ok {
        // Fall back to runtime for Bash blocks
        return vm.runtime.InvokeBlock(blockID, args...)
    }

    // Create new VM for nested execution
    nestedVM := NewVM(vm.runtime)
    return nestedVM.ExecuteWithCaptures(block.chunk, vm.instanceID, args, block.captures)
}
```

### Testing Strategy
1. Unit tests for each opcode
2. Integration tests for control flow (if/while)
3. Capture semantics tests (verify mutations visible)
4. Nested block tests
5. Performance benchmarks against Bash invocation

### Dependencies
- Phase 1 (opcodes)
- Phase 2 (compiler)

---

## Phase 4: Procyon Codegen Integration

### Goals
- Modify Procyon's code generator to emit bytecode for blocks
- Generate VM invocation code instead of `invokeBlock()` shell-out
- Support both bytecode and Bash blocks in generated code

### Deliverables
1. Modified codegen that compiles blocks to bytecode
2. Generated code that uses VM for block invocation
3. Fallback to Bash for complex cases

### Files to Modify

#### `/Users/chazu/dev/go/procyon/pkg/codegen/codegen.go`

Add bytecode compilation for blocks:

```go
// In generator struct:
type generator struct {
    // ... existing fields ...

    // Bytecode compilation
    bytecodeCompiler *bytecode.Compiler
    compiledBlocks   map[string]*bytecode.Chunk // For embedding
}

// In generateTypeHelpers, replace invokeBlock with bytecode version:
func (g *generator) generateTypeHelpers(f *jen.File) {
    // ... existing code ...

    // Add bytecode VM helper
    f.Comment("// bytecodeVM is reused for block execution")
    f.Var().Id("bytecodeVM").Op("*").Qual("github.com/chazu/procyon/pkg/bytecode", "VM")
    f.Line()

    f.Comment("// invokeBlock executes a block natively via bytecode VM")
    f.Func().Id("invokeBlock").Params(
        jen.Id("blockID").String(),
        jen.Id("args").Op("...").Interface(),
    ).String().Block(
        // Check if block is bytecode-compiled
        jen.If(jen.Qual("strings", "HasPrefix").Call(
            jen.Id("blockID"), jen.Lit("bytecode_"),
        )).Block(
            // Use bytecode VM
            jen.If(jen.Id("bytecodeVM").Op("==").Nil()).Block(
                jen.Id("bytecodeVM").Op("=").Qual(
                    "github.com/chazu/procyon/pkg/bytecode", "NewVM",
                ).Call(jen.Nil()),
            ),
            jen.Id("stringArgs").Op(":=").Make(jen.Index().String(), jen.Lit(0)),
            jen.For(jen.List(jen.Id("_"), jen.Id("arg")).Op(":=").Range().Id("args")).Block(
                jen.Id("stringArgs").Op("=").Append(
                    jen.Id("stringArgs"),
                    jen.Qual("fmt", "Sprint").Call(jen.Id("arg")),
                ),
            ),
            jen.List(jen.Id("result"), jen.Id("_")).Op(":=").Id("bytecodeVM").Dot("InvokeRegistered").Call(
                jen.Id("blockID"),
                jen.Id("stringArgs"),
            ),
            jen.Return(jen.Id("result")),
        ),
        // Fall back to Bash for non-bytecode blocks
        // ... existing Bash invocation code ...
    )
}
```

#### New method for compiling inline blocks:

```go
// compileBlockToBytecode converts a parser.BlockExpr to bytecode
func (g *generator) compileBlockToBytecode(block *parser.BlockExpr, m *compiledMethod) (*bytecode.Chunk, error) {
    ctx := &bytecode.CompilerContext{
        MethodParams: m.args,
        MethodLocals: g.getLocalVars(m),
        InstanceVars: g.getInstanceVarNames(),
        InstanceID:   "", // Will be filled at runtime
    }

    return bytecode.CompileBlock(block, ctx)
}

// generateBlockCreation emits code that creates a bytecode block at runtime
func (g *generator) generateBlockCreation(block *parser.BlockExpr, m *compiledMethod) *jen.Statement {
    chunk, err := g.compileBlockToBytecode(block, m)
    if err != nil {
        // Fall back to Bash-style block creation
        return g.generateBashBlockCreation(block, m)
    }

    // Serialize chunk and embed as constant
    chunkBytes, _ := chunk.Serialize()
    chunkHex := hex.EncodeToString(chunkBytes)

    // Generate code that:
    // 1. Deserializes chunk from hex
    // 2. Creates capture cells for captured variables
    // 3. Registers the block
    return jen.Func().Params().String().Block(
        jen.Id("chunkData").Op(",").Id("_").Op(":=").Qual("encoding/hex", "DecodeString").Call(
            jen.Lit(chunkHex),
        ),
        jen.Id("chunk").Op(",").Id("_").Op(":=").Qual(
            "github.com/chazu/procyon/pkg/bytecode", "Deserialize",
        ).Call(jen.Id("chunkData")),
        // Create capture cells...
        jen.Id("captures").Op(":=").Index().Op("*").Qual(
            "github.com/chazu/procyon/pkg/bytecode", "CaptureCell",
        ).Values(
            // Generate capture cell initialization for each captured var
        ),
        jen.Return(jen.Qual(
            "github.com/chazu/procyon/pkg/bytecode", "RegisterBlock",
        ).Call(jen.Id("chunk"), jen.Id("captures"))),
    ).Call()
}
```

### Testing Strategy
1. Compile existing test classes and verify bytecode generation
2. Compare behavior with and without bytecode
3. Performance comparison: bytecode vs Bash shell-out
4. Edge cases: recursive blocks, deep nesting

### Dependencies
- Phase 3 (VM)

---

## Phase 5: Reference Capture / Closure Support

### Goals
- Implement full reference capture semantics
- Handle ivar mutations from within blocks
- Support closures that outlive their creating scope

### Deliverables
1. Capture cell writeback for ivars
2. Long-lived closure support
3. Proper cleanup when closures are garbage collected

### Files to Modify

#### `/Users/chazu/dev/go/procyon/pkg/bytecode/vm.go`

Enhance capture handling:

```go
// CaptureCell with lifecycle management
type CaptureCell struct {
    Value    string
    Source   VarSource
    Name     string
    Closed   bool       // True when cell owns its value
    RefCount int        // For garbage collection
    Runtime  *runtime.Runtime
    Instance string     // For ivar writeback
}

// Close captures a snapshot of the value for long-lived closures
func (c *CaptureCell) Close() {
    if c.Closed {
        return
    }
    // For ivars, read current value from DB
    if c.Source == VarSourceIVar && c.Runtime != nil {
        instance, err := c.Runtime.LoadInstance(c.Instance)
        if err == nil {
            c.Value = instance.GetVarString(c.Name)
        }
    }
    c.Closed = true
}

// Set updates the cell value, writing back to source if needed
func (c *CaptureCell) Set(value string) {
    c.Value = value

    // If this is an unclosed ivar capture, write back
    if c.Source == VarSourceIVar && !c.Closed && c.Runtime != nil {
        instance, err := c.Runtime.LoadInstance(c.Instance)
        if err == nil {
            instance.SetVar(c.Name, value)
            c.Runtime.SaveInstance(c.Instance, instance)
        }
    }
}

// Get reads the cell value, refreshing from source if needed
func (c *CaptureCell) Get() string {
    // For unclosed ivar captures, read fresh from DB
    if c.Source == VarSourceIVar && !c.Closed && c.Runtime != nil {
        instance, err := c.Runtime.LoadInstance(c.Instance)
        if err == nil {
            c.Value = instance.GetVarString(c.Name)
        }
    }
    return c.Value
}
```

### Testing Strategy
1. Verify ivar mutations in blocks are persisted
2. Test block returns block that captures outer block's variables
3. Verify capture cells are properly cleaned up

### Dependencies
- Phase 4 (codegen integration)

---

## Phase 6: Cross-Plugin and Daemon Integration

### Goals
- Enable blocks created in one plugin to execute in another
- Integrate bytecode execution with tt daemon
- Support serialized block transfer over Unix socket

### Deliverables
1. Block serialization for cross-plugin transport
2. Daemon protocol extension for bytecode blocks
3. Plugin-to-plugin block invocation

### Files to Modify

#### `/Users/chazu/dev/go/procyon/cmd/tt/main.go`

Add bytecode block registry and invocation:

```go
// Add to Daemon struct:
type Daemon struct {
    // ... existing fields ...

    // Bytecode block management
    blockRegistry map[string]*bytecode.RegisteredBlock
    blockMu       sync.RWMutex
    vm            *bytecode.VM
}

// Extended request format
type Request struct {
    // ... existing fields ...

    // For bytecode block operations
    BlockOp      string `json:"block_op,omitempty"`      // "register", "invoke", "serialize"
    BlockID      string `json:"block_id,omitempty"`
    BlockData    string `json:"block_data,omitempty"`    // Hex-encoded bytecode
    BlockCaptures string `json:"block_captures,omitempty"` // JSON captures
}

// Extended response format
type Response struct {
    // ... existing fields ...

    BlockID   string `json:"block_id,omitempty"`
    BlockData string `json:"block_data,omitempty"`
}

// Handle block operations in daemon
func (d *Daemon) handleBlockOp(req Request) Response {
    switch req.BlockOp {
    case "register":
        // Deserialize bytecode and register
        chunkData, _ := hex.DecodeString(req.BlockData)
        chunk, err := bytecode.Deserialize(chunkData)
        if err != nil {
            return Response{ExitCode: 1, Error: err.Error()}
        }

        var captures []*bytecode.CaptureCell
        json.Unmarshal([]byte(req.BlockCaptures), &captures)

        id := d.registerBlock(chunk, captures)
        return Response{ExitCode: 0, BlockID: id}

    case "invoke":
        var args []string
        json.Unmarshal([]byte(req.BlockData), &args)

        result, err := d.invokeBlock(req.BlockID, args)
        if err != nil {
            return Response{ExitCode: 1, Error: err.Error()}
        }
        return Response{ExitCode: 0, Result: result}

    case "serialize":
        // Export a block for cross-process transfer
        block := d.getBlock(req.BlockID)
        if block == nil {
            return Response{ExitCode: 1, Error: "block not found"}
        }
        data, _ := block.Chunk.Serialize()
        return Response{ExitCode: 0, BlockData: hex.EncodeToString(data)}

    default:
        return Response{ExitCode: 1, Error: "unknown block operation"}
    }
}
```

#### `/Users/chazu/.trashtalk/lib/trash.bash`

Add Bash-side support for bytecode blocks:

```bash
# Invoke a bytecode block through the daemon
_invoke_bytecode_block() {
    local block_id="$1"
    shift
    local args_json
    args_json=$(printf '%s\n' "$@" | jq -R . | jq -s .)

    local req
    req=$(jq -n \
        --arg op "invoke" \
        --arg id "$block_id" \
        --arg args "$args_json" \
        '{block_op: $op, block_id: $id, block_data: $args}')

    local resp
    resp=$(_daemon_request "$req")
    echo "$resp" | jq -r '.result // empty'
}

# Check if a block is bytecode-compiled
_is_bytecode_block() {
    [[ "$1" == bytecode_* ]]
}
```

### Testing Strategy
1. Create block in plugin A, invoke from plugin B
2. Test serialization round-trip
3. Stress test daemon with many concurrent block invocations
4. Verify capture values are properly transferred

### Dependencies
- Phase 5 (closures)

---

## Phase 7: Testing and Hardening

### Goals
- Comprehensive test coverage
- Performance optimization
- Documentation and examples

### Deliverables
1. Test suite covering all features
2. Benchmark suite
3. Migration guide from Bash blocks
4. API documentation

### Testing Categories

#### Unit Tests
```
pkg/bytecode/
    opcodes_test.go       - Opcode metadata
    chunk_test.go         - Serialization
    compiler_test.go      - AST to bytecode
    vm_test.go            - Each opcode execution
    capture_test.go       - Reference semantics
```

#### Integration Tests
```
pkg/codegen/
    bytecode_codegen_test.go  - Codegen produces correct bytecode

cmd/procyon/
    block_e2e_test.go         - End-to-end block compilation
```

#### System Tests
```
tests/
    test_bytecode_blocks.bash     - Verify blocks work in Bash runtime
    test_cross_plugin_blocks.bash - Cross-plugin invocation
    test_closure_semantics.bash   - Reference capture verification
```

#### Benchmarks
```
pkg/bytecode/
    vm_bench_test.go      - VM instruction throughput

comparison/
    bash_vs_bytecode/     - Side-by-side performance comparison
```

### Documentation

#### Files to Create

`/Users/chazu/.trashtalk/docs/BYTECODE_BLOCKS.md` - User guide
`/Users/chazu/dev/go/procyon/pkg/bytecode/README.md` - Developer guide

### Dependencies
- All previous phases

---

## Implementation Timeline

| Phase | Estimated Duration | Dependencies |
|-------|-------------------|--------------|
| Phase 0: Preparation | 2-3 days | None |
| Phase 1: Core Infrastructure | 3-4 days | Phase 0 |
| Phase 2: Bytecode Compiler | 4-5 days | Phase 1 |
| Phase 3: VM Implementation | 4-5 days | Phase 1, 2 |
| Phase 4: Codegen Integration | 3-4 days | Phase 3 |
| Phase 5: Reference Capture | 2-3 days | Phase 4 |
| Phase 6: Daemon Integration | 3-4 days | Phase 5 |
| Phase 7: Testing/Hardening | 4-5 days | Phase 6 |

**Total: 25-33 days of focused development**

---

## Risk Mitigation

### Risk: Capture semantics differ from Bash
**Mitigation**: Comprehensive test suite comparing behavior; option to disable bytecode and fall back to Bash.

### Risk: Performance regression in some cases
**Mitigation**: Benchmarks in CI; ability to selectively disable bytecode per block via pragma.

### Risk: Cross-plugin complexity
**Mitigation**: Start with simple case (same plugin); daemon handles coordination.

### Risk: Debug difficulty
**Mitigation**: Disassembler, trace mode, source maps from Phase 1.

---

## Success Criteria

1. **Functional parity**: All existing block tests pass with bytecode enabled
2. **Performance improvement**: At least 5x faster than Bash shell-out for typical blocks
3. **Reference semantics**: Mutations to captured ivars visible outside block
4. **Cross-plugin**: Blocks can be passed between plugins via daemon
5. **Graceful fallback**: Complex/unsupported blocks fall back to Bash transparently
