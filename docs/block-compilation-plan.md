# Procyon Block Compilation Plan

## Problem Statement

Trashtalk's iteration methods (`do:`, `collect:`, `select:`, `inject:into:`) use blocks - first-class code objects that can be passed around and executed. Currently these are implemented as `rawMethod` in Bash and cannot be compiled by Procyon.

Example of what we want to compile:
```smalltalk
method: sum [
  | total |
  total := 0
  @ items do: [:each | total := total + each]
  ^ total
]
```

### Current Block Semantics (Bash)

```bash
# Block creation (in compiler)
block_id=$(_create_block "each" "@ Console print: \$each")

# Block invocation (in do: method)
@ "$block_id" valueWith: "$element"
```

Blocks are runtime objects with:
- Parameter names (`:each`, `:key :value`)
- Body code (string of Trashtalk/Bash)
- Captured variable context (lexical scope)

---

## Design Goals

1. **Compile inline block literals** to native Go closures for maximum performance
2. **Fall back gracefully** when blocks are dynamic (variables/parameters)
3. **Maintain compatibility** with existing Bash Trashtalk runtime
4. **Enable incremental adoption** - methods can mix compiled and interpreted blocks

---

## Phase 1: Block Literal Parsing and Closure Generation

### 1.1 Lexer Changes

Add new token types to recognize block syntax:

**File**: `pkg/ast/tokens.go`
```go
const (
    // Existing tokens...

    // Block tokens
    TokenBlockStart    TokenType = "BLOCK_START"    // [
    TokenBlockEnd      TokenType = "BLOCK_END"      // ]
    TokenBlockArg      TokenType = "BLOCK_ARG"      // :argName
    TokenBlockBar      TokenType = "BLOCK_BAR"      // | (separates args from body)
)
```

Note: The jq tokenizer already handles these. We need to ensure they're passed through in the AST JSON.

### 1.2 AST Node for Blocks

**File**: `pkg/parser/expr.go`
```go
// BlockLiteral represents an inline block expression
// e.g., [:each | total := total + each]
type BlockLiteral struct {
    Params []string   // Parameter names without colons
    Body   []Stmt     // Statements in block body
}

func (b *BlockLiteral) exprNode() {}
```

### 1.3 Parser Changes

**File**: `pkg/parser/parser.go`

```go
// parseBlockLiteral parses [:args | body]
func (p *Parser) parseBlockLiteral() (*BlockLiteral, error) {
    // Expect [
    if p.peek().Type != ast.TokenBlockStart {
        return nil, fmt.Errorf("expected '[' at start of block")
    }
    p.advance()

    // Parse block arguments (optional)
    var params []string
    for p.peek().Type == ast.TokenBlockArg {
        // :argName -> argName
        argName := strings.TrimPrefix(p.peek().Value, ":")
        params = append(params, argName)
        p.advance()
    }

    // If we have args, expect | separator
    if len(params) > 0 {
        if p.peek().Type != ast.TokenBlockBar {
            return nil, fmt.Errorf("expected '|' after block arguments")
        }
        p.advance()
    }

    // Parse body statements until ]
    var body []Stmt
    for p.peek().Type != ast.TokenBlockEnd {
        stmt, err := p.parseStatement()
        if err != nil {
            return nil, err
        }
        body = append(body, stmt)
    }
    p.advance() // consume ]

    return &BlockLiteral{Params: params, Body: body}, nil
}
```

Update `parseMessageArg` to recognize block literals:
```go
func (p *Parser) parseMessageArg() (Expr, error) {
    // Check for block literal
    if p.peek().Type == ast.TokenBlockStart {
        return p.parseBlockLiteral()
    }
    // ... existing logic
}
```

### 1.4 Code Generation for Inline Blocks

**File**: `pkg/codegen/blocks.go` (NEW)

```go
package codegen

import (
    "github.com/dave/jennifer/jen"
)

// generateInlineBlock generates a Go closure from a block literal
func (g *generator) generateInlineBlock(block *parser.BlockLiteral, m *compiledMethod) *jen.Statement {
    // Generate closure parameters
    params := make([]jen.Code, len(block.Params))
    for i, param := range block.Params {
        params[i] = jen.Id(param).Interface()
    }

    // Generate closure body
    body := make([]jen.Code, 0)
    for _, stmt := range block.Body {
        body = append(body, g.generateStatement(stmt, m))
    }

    // Return closure
    // func(each interface{}) interface{} { ... }
    return jen.Func().Params(params...).Interface().Block(body...)
}

// generateIterationWithInlineBlock generates optimized iteration
// when the block is a literal that can be inlined
func (g *generator) generateIterationWithInlineBlock(
    receiver Expr,
    selector string,
    block *parser.BlockLiteral,
    m *compiledMethod,
) *jen.Statement {

    receiverCode := g.generateExpr(receiver, m)

    switch selector {
    case "do:":
        return g.generateDo(receiverCode, block, m)
    case "collect:":
        return g.generateCollect(receiverCode, block, m)
    case "select:":
        return g.generateSelect(receiverCode, block, m)
    case "inject:into:":
        // Handled separately due to two arguments
        return nil
    default:
        return nil
    }
}

// generateDo generates: for _, each := range items { body }
func (g *generator) generateDo(receiver *jen.Statement, block *parser.BlockLiteral, m *compiledMethod) *jen.Statement {
    if len(block.Params) != 1 {
        return nil // Invalid block arity
    }
    param := block.Params[0]

    // Generate body statements
    body := make([]jen.Code, len(block.Body))
    for i, stmt := range block.Body {
        body[i] = g.generateStatement(stmt, m)
    }

    // for _, param := range receiver { body }
    return jen.For(
        jen.Id("_"), jen.Id(param),
    ).Op(":=").Range().Add(receiver).Block(body...)
}

// generateCollect generates:
// result := make([]interface{}, 0, len(items))
// for _, each := range items { result = append(result, transform(each)) }
func (g *generator) generateCollect(receiver *jen.Statement, block *parser.BlockLiteral, m *compiledMethod) *jen.Statement {
    if len(block.Params) != 1 {
        return nil
    }
    param := block.Params[0]

    // The block body should be a single expression (the transform)
    // For now, assume last statement is the return value
    var transformExpr jen.Code
    if len(block.Body) == 1 {
        if expr, ok := block.Body[0].(*parser.ExprStmt); ok {
            transformExpr = g.generateExpr(expr.Expr, m)
        }
    }
    if transformExpr == nil {
        return nil // Complex block, can't inline
    }

    // Generate the loop
    resultVar := g.freshVar("result")
    return jen.Block(
        // result := make([]interface{}, 0)
        jen.Id(resultVar).Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
        // for _, param := range receiver { result = append(result, transform) }
        jen.For(
            jen.Id("_"), jen.Id(param),
        ).Op(":=").Range().Add(receiver).Block(
            jen.Id(resultVar).Op("=").Append(jen.Id(resultVar), transformExpr),
        ),
    )
}

// generateSelect generates:
// result := make([]interface{}, 0)
// for _, each := range items { if predicate(each) { result = append(result, each) } }
func (g *generator) generateSelect(receiver *jen.Statement, block *parser.BlockLiteral, m *compiledMethod) *jen.Statement {
    if len(block.Params) != 1 {
        return nil
    }
    param := block.Params[0]

    // Block body should evaluate to a boolean
    var predicateExpr jen.Code
    if len(block.Body) == 1 {
        if expr, ok := block.Body[0].(*parser.ExprStmt); ok {
            predicateExpr = g.generateExpr(expr.Expr, m)
        }
    }
    if predicateExpr == nil {
        return nil
    }

    resultVar := g.freshVar("result")
    return jen.Block(
        jen.Id(resultVar).Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
        jen.For(
            jen.Id("_"), jen.Id(param),
        ).Op(":=").Range().Add(receiver).Block(
            jen.If(predicateExpr).Block(
                jen.Id(resultVar).Op("=").Append(jen.Id(resultVar), jen.Id(param)),
            ),
        ),
    )
}
```

### 1.5 Integration with Message Send

Detect when a message send is an iteration method with an inline block:

**File**: `pkg/codegen/codegen.go`

```go
func (g *generator) generateMessageSend(send *parser.MessageSend, m *compiledMethod) *jen.Statement {
    // Check for iteration methods with inline blocks
    if isIterationMethod(send.Selector) && len(send.Args) == 1 {
        if block, ok := send.Args[0].(*parser.BlockLiteral); ok {
            if code := g.generateIterationWithInlineBlock(send.Receiver, send.Selector, block, m); code != nil {
                return code
            }
            // Fall through to Phase 2 shell-out if inlining failed
        }
    }

    // ... existing message send logic
}

func isIterationMethod(selector string) bool {
    switch selector {
    case "do:", "collect:", "select:", "reject:", "detect:", "inject:into:":
        return true
    }
    return false
}
```

---

## Phase 2: Shell-out Fallback for Dynamic Blocks

When a block is passed as a variable or parameter (not a literal), we need to call back to the Bash runtime.

### 2.1 Block Invocation Helper

**File**: `pkg/codegen/runtime.go`

```go
// invokeBlock calls a Trashtalk block through the Bash runtime
// blockID is the instance ID of the Block object
// args are the values to pass to the block
func invokeBlock(blockID string, args ...interface{}) (string, error) {
    // Build the @ command
    // @ "$blockID" valueWith: arg1 and: arg2 ...

    cmd := exec.Command("bash", "-c", buildBlockCall(blockID, args))
    output, err := cmd.Output()
    if err != nil {
        return "", err
    }
    return strings.TrimSpace(string(output)), nil
}

func buildBlockCall(blockID string, args []interface{}) string {
    var sb strings.Builder
    sb.WriteString("source ~/.trashtalk/lib/trash.bash; ")
    sb.WriteString(fmt.Sprintf("@ %q ", blockID))

    switch len(args) {
    case 0:
        sb.WriteString("value")
    case 1:
        sb.WriteString(fmt.Sprintf("valueWith: %q", fmt.Sprint(args[0])))
    case 2:
        sb.WriteString(fmt.Sprintf("valueWith: %q and: %q", fmt.Sprint(args[0]), fmt.Sprint(args[1])))
    }

    return sb.String()
}
```

### 2.2 Shell-out Iteration

**File**: `pkg/codegen/blocks.go`

```go
// generateShelloutIteration generates iteration that calls back to Bash for each element
func (g *generator) generateShelloutIteration(
    receiver Expr,
    selector string,
    blockVar Expr, // The variable holding the block ID
    m *compiledMethod,
) *jen.Statement {

    receiverCode := g.generateExpr(receiver, m)
    blockCode := g.generateExpr(blockVar, m)

    switch selector {
    case "do:":
        return g.generateShelloutDo(receiverCode, blockCode, m)
    case "collect:":
        return g.generateShelloutCollect(receiverCode, blockCode, m)
    // ... etc
    }
    return nil
}

func (g *generator) generateShelloutDo(receiver, block *jen.Statement, m *compiledMethod) *jen.Statement {
    // for _, elem := range receiver {
    //     invokeBlock(block.(string), elem)
    // }
    return jen.For(
        jen.Id("_"), jen.Id("elem"),
    ).Op(":=").Range().Add(receiver).Block(
        jen.Id("invokeBlock").Call(
            block.Assert(jen.String()),
            jen.Id("elem"),
        ),
    )
}

func (g *generator) generateShelloutCollect(receiver, block *jen.Statement, m *compiledMethod) *jen.Statement {
    resultVar := g.freshVar("result")
    return jen.Block(
        jen.Id(resultVar).Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
        jen.For(
            jen.Id("_"), jen.Id("elem"),
        ).Op(":=").Range().Add(receiver).Block(
            jen.List(jen.Id("val"), jen.Id("_")).Op(":=").Id("invokeBlock").Call(
                block.Assert(jen.String()),
                jen.Id("elem"),
            ),
            jen.Id(resultVar).Op("=").Append(jen.Id(resultVar), jen.Id("val")),
        ),
    )
}
```

### 2.3 Daemon Protocol Extension (Future Optimization)

When the daemon architecture is in place, block invocation can use the persistent connection:

```go
// BlockCallRequest sent to daemon
type BlockCallRequest struct {
    Type    string        `json:"type"`    // "block_call"
    BlockID string        `json:"block_id"`
    Args    []interface{} `json:"args"`
}

// BlockCallResponse from daemon
type BlockCallResponse struct {
    Result   string `json:"result"`
    ExitCode int    `json:"exit_code"`
}
```

This avoids the subprocess spawn overhead per iteration.

---

## Phase 3: AST Interpretation for Simple Blocks

For blocks containing only simple expressions (arithmetic, comparisons, variable access), we can interpret the AST directly in Go without calling back to Bash.

### 3.1 Block Complexity Analysis

**File**: `pkg/codegen/block_analysis.go` (NEW)

```go
package codegen

import "github.com/chazu/procyon/pkg/parser"

// BlockComplexity indicates how a block should be compiled
type BlockComplexity int

const (
    // BlockSimple - can be fully interpreted in Go
    // Contains only: arithmetic, comparisons, variable access, literals
    BlockSimple BlockComplexity = iota

    // BlockMessageSend - contains message sends but no side effects
    // Can be interpreted if we have method metadata
    BlockMessageSend

    // BlockComplex - requires Bash execution
    // Contains: assignments to captured vars, shell commands, complex control flow
    BlockComplex
)

// analyzeBlockComplexity determines how a block should be handled
func analyzeBlockComplexity(block *parser.BlockLiteral) BlockComplexity {
    for _, stmt := range block.Body {
        complexity := analyzeStatementComplexity(stmt)
        if complexity == BlockComplex {
            return BlockComplex
        }
    }

    // Check if all statements are simple expressions
    if allSimpleExpressions(block.Body) {
        return BlockSimple
    }

    return BlockMessageSend
}

func analyzeStatementComplexity(stmt parser.Stmt) BlockComplexity {
    switch s := stmt.(type) {
    case *parser.ExprStmt:
        return analyzeExprComplexity(s.Expr)
    case *parser.Assignment:
        // Assignments to captured variables require Bash
        // (they modify state outside the block)
        return BlockComplex
    case *parser.Return:
        return analyzeExprComplexity(s.Value)
    default:
        return BlockComplex
    }
}

func analyzeExprComplexity(expr parser.Expr) BlockComplexity {
    switch e := expr.(type) {
    case *parser.Number, *parser.StringLit, *parser.Identifier:
        return BlockSimple

    case *parser.BinaryOp:
        left := analyzeExprComplexity(e.Left)
        right := analyzeExprComplexity(e.Right)
        if left == BlockComplex || right == BlockComplex {
            return BlockComplex
        }
        // Arithmetic and comparison operators are simple
        if isSimpleOperator(e.Op) {
            return BlockSimple
        }
        return BlockComplex

    case *parser.MessageSend:
        // Message sends might be simple if they're JSON primitives
        // or known pure methods
        return BlockMessageSend

    case *parser.JSONPrimitiveExpr:
        // JSON primitives are simple
        return BlockSimple

    default:
        return BlockComplex
    }
}

func isSimpleOperator(op string) bool {
    switch op {
    case "+", "-", "*", "/", "%",
         "==", "!=", "<", ">", "<=", ">=",
         "&&", "||":
        return true
    }
    return false
}

func allSimpleExpressions(stmts []parser.Stmt) bool {
    for _, stmt := range stmts {
        if analyzeStatementComplexity(stmt) != BlockSimple {
            return false
        }
    }
    return true
}
```

### 3.2 Simple Block Interpreter

**File**: `pkg/codegen/block_interp.go` (NEW)

```go
package codegen

import (
    "fmt"
    "github.com/dave/jennifer/jen"
)

// generateInterpretedBlock generates code that evaluates a simple block
// without calling back to Bash
func (g *generator) generateInterpretedBlock(
    block *parser.BlockLiteral,
    m *compiledMethod,
) *jen.Statement {

    // For simple blocks, we generate a closure that directly computes the result
    // The closure captures the parameter and evaluates the body expression

    if len(block.Params) != 1 || len(block.Body) != 1 {
        return nil // Only handle single-param, single-expression blocks
    }

    param := block.Params[0]

    // Get the body expression
    exprStmt, ok := block.Body[0].(*parser.ExprStmt)
    if !ok {
        return nil
    }

    // Generate the expression with param as a local variable
    bodyCode := g.generateSimpleExpr(exprStmt.Expr, param, m)
    if bodyCode == nil {
        return nil
    }

    // func(param interface{}) interface{} { return bodyCode }
    return jen.Func().Params(
        jen.Id(param).Interface(),
    ).Interface().Block(
        jen.Return(bodyCode),
    )
}

// generateSimpleExpr generates Go code for a simple expression
// that can be evaluated without Bash
func (g *generator) generateSimpleExpr(
    expr parser.Expr,
    blockParam string,
    m *compiledMethod,
) *jen.Statement {

    switch e := expr.(type) {
    case *parser.Number:
        return jen.Lit(e.Value)

    case *parser.StringLit:
        return jen.Lit(e.Value)

    case *parser.Identifier:
        if e.Name == blockParam {
            // Reference to block parameter - needs type assertion for arithmetic
            return jen.Id(e.Name)
        }
        // Instance variable or local
        if g.isInstanceVar(e.Name, m) {
            return jen.Id("c").Dot(capitalize(e.Name))
        }
        return jen.Id(e.Name)

    case *parser.BinaryOp:
        left := g.generateSimpleExpr(e.Left, blockParam, m)
        right := g.generateSimpleExpr(e.Right, blockParam, m)
        if left == nil || right == nil {
            return nil
        }

        // For arithmetic with interface{} values, we need type assertions
        if needsNumericAssertion(e.Op) {
            return g.generateNumericBinaryOp(left, e.Op, right, blockParam)
        }

        return left.Op(e.Op).Add(right)

    case *parser.JSONPrimitiveExpr:
        // Delegate to existing JSON primitive codegen
        return g.generateJSONPrimitive(e, m)

    default:
        return nil // Can't handle this expression type
    }
}

func needsNumericAssertion(op string) bool {
    switch op {
    case "+", "-", "*", "/", "%":
        return true
    }
    return false
}

// generateNumericBinaryOp handles arithmetic on interface{} values
func (g *generator) generateNumericBinaryOp(
    left *jen.Statement,
    op string,
    right *jen.Statement,
    blockParam string,
) *jen.Statement {
    // Generate: toInt(left) op toInt(right)
    // where toInt handles interface{} -> int conversion

    leftInt := jen.Id("toInt").Call(left)
    rightInt := jen.Id("toInt").Call(right)

    return leftInt.Op(op).Add(rightInt)
}
```

### 3.3 Runtime Helper for Type Conversion

**File**: Generated in each compiled class

```go
// toInt converts interface{} to int for arithmetic in blocks
func toInt(v interface{}) int {
    switch x := v.(type) {
    case int:
        return x
    case int64:
        return int(x)
    case float64:
        return int(x)
    case string:
        n, _ := strconv.Atoi(x)
        return n
    default:
        return 0
    }
}

// toString converts interface{} to string
func toString(v interface{}) string {
    return fmt.Sprint(v)
}
```

### 3.4 Unified Block Handling

**File**: `pkg/codegen/blocks.go`

```go
// handleBlockArg processes a block argument to an iteration method
// Returns generated code and whether it succeeded
func (g *generator) handleBlockArg(
    block parser.Expr,
    receiver *jen.Statement,
    selector string,
    m *compiledMethod,
) (*jen.Statement, bool) {

    // Case 1: Inline block literal
    if lit, ok := block.(*parser.BlockLiteral); ok {
        complexity := analyzeBlockComplexity(lit)

        switch complexity {
        case BlockSimple:
            // Phase 3: Interpret directly in Go
            if code := g.generateInterpretedIteration(receiver, selector, lit, m); code != nil {
                return code, true
            }
            // Fall through to closure if interpretation failed
            fallthrough

        case BlockMessageSend:
            // Phase 1: Generate Go closure
            if code := g.generateIterationWithInlineBlock(receiver, selector, lit, m); code != nil {
                return code, true
            }
            // Fall through to shell-out
            fallthrough

        case BlockComplex:
            // Phase 2: Shell out to Bash
            // Convert block literal to Bash block at runtime
            return g.generateBlockCreationAndShellout(receiver, selector, lit, m), true
        }
    }

    // Case 2: Block variable/parameter
    // Always shell out since we don't know the block's content
    return g.generateShelloutIteration(receiver, selector, block, m), true
}

// generateInterpretedIteration uses the simple block interpreter
func (g *generator) generateInterpretedIteration(
    receiver *jen.Statement,
    selector string,
    block *parser.BlockLiteral,
    m *compiledMethod,
) *jen.Statement {

    if len(block.Params) != 1 {
        return nil
    }
    param := block.Params[0]

    // Get the transform/predicate expression
    if len(block.Body) != 1 {
        return nil
    }
    exprStmt, ok := block.Body[0].(*parser.ExprStmt)
    if !ok {
        return nil
    }

    bodyCode := g.generateSimpleExpr(exprStmt.Expr, param, m)
    if bodyCode == nil {
        return nil
    }

    switch selector {
    case "do:":
        // Simple loop, body is for side effects
        return jen.For(
            jen.Id("_"), jen.Id(param),
        ).Op(":=").Range().Add(receiver).Block(
            jen.Id("_").Op("=").Add(bodyCode), // Discard result
        )

    case "collect:":
        resultVar := g.freshVar("result")
        return jen.Block(
            jen.Id(resultVar).Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
            jen.For(
                jen.Id("_"), jen.Id(param),
            ).Op(":=").Range().Add(receiver).Block(
                jen.Id(resultVar).Op("=").Append(jen.Id(resultVar), bodyCode),
            ),
        )

    case "select:":
        resultVar := g.freshVar("result")
        return jen.Block(
            jen.Id(resultVar).Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
            jen.For(
                jen.Id("_"), jen.Id(param),
            ).Op(":=").Range().Add(receiver).Block(
                jen.If(jen.Id("toBool").Call(bodyCode)).Block(
                    jen.Id(resultVar).Op("=").Append(jen.Id(resultVar), jen.Id(param)),
                ),
            ),
        )
    }

    return nil
}
```

---

## Implementation Checklist

### Phase 1: Block Literal Parsing ✅ COMPLETE
- [x] Add block token types to `pkg/ast/tokens.go` (already existed: `TokenBlockParam`, `TokenLBracket`, `TokenRBracket`, `TokenPipe`)
- [x] Add `BlockLiteral` node to `pkg/parser/parser.go` (exists as `BlockExpr`)
- [x] Implement `parseBlockLiteral()` in parser (exists as `parseBlockExpr()`)
- [x] Update `parseMessageArg()` to handle block literals
- [x] Write parser tests for block syntax (`TestParseBlockExpr`, `TestParseIterationExpr`)
- [x] Implement `generateIterationStatement()` with inline loop generation
- [x] Implement `generateCollectBody()`, `generateSelectBody()` for result extraction
- [x] Write codegen tests for inline blocks (`testdata/block_iteration/`)
- [x] Add `toInt()`, `toBool()` type conversion helpers
- [x] Test with Array.trash iteration methods - compiles successfully

**Phase 1 Implementation Notes:**
- Parser uses `IterationExpr` node which captures collection, iterVar, body, and kind
- Codegen detects native arrays via `exprResultsInArray()` for direct iteration
- Type conversion uses `_iterVar` (raw interface{}) → `iterVar` (typed int) pattern
- Generated code verified to compile and run correctly

### Phase 2: Shell-out Fallback (Week 2-3) ✅ COMPLETED
- [x] Implement `invokeBlock()` runtime helper
- [x] Implement `generateDynamicIterationStatement()`
- [x] Detect block variables vs literals in parser
- [x] Test with block parameters passed to methods
- [ ] Add daemon protocol for block calls (optional - future optimization)

**Phase 2 Implementation Notes:**
- Added `DynamicIterationExpr` AST node for block variables (vs `IterationExpr` for literals)
- Parser detects `TokenLBracket` for literal blocks vs identifiers for block variables
- Added `generateExprAsString()` to keep block IDs as strings (no int conversion)
- Added `IterationExprAsValue` and `DynamicIterationExprAsValue` wrappers for return statements
- `invokeBlock()` generates different command strings based on argument count (0, 1, 2)
- Return statements with iteration now properly marshal results to JSON
- Test case: `testdata/dynamic_block/` with `eachDo:`, `collectWith:`, `selectWith:` methods

### Phase 3: AST Interpretation (Week 3-4)
- [x] Add `toInt()`, `toString()`, `toBool()` runtime helpers (done in Phase 1)
- [ ] Implement `analyzeBlockComplexity()`
- [ ] Implement `generateSimpleExpr()` for interpreted blocks
- [ ] Implement `generateInterpretedIteration()`
- [ ] Write tests for simple vs complex block detection
- [ ] Benchmark interpreted vs closure vs shell-out

---

## Expected Performance

| Approach | Latency per Iteration | Use Case |
|----------|----------------------|----------|
| Phase 3: Interpreted | ~10ns | `[:x | x * 2]` |
| Phase 1: Go Closure | ~50ns | `[:x | @ x someMethod]` |
| Phase 2: Shell-out | ~2ms | Dynamic blocks, complex captures |

For a 1000-element array:
- Interpreted: ~10μs total
- Closure: ~50μs total
- Shell-out: ~2s total

---

## Compatibility Notes

1. **Existing Bash code unchanged** - rawMethods continue to work
2. **Gradual adoption** - Methods can mix compiled and Bash blocks
3. **Fallback safety** - Unknown patterns shell out rather than fail
4. **Variable capture** - Phase 2 handles by delegating to Bash

---

## Future Enhancements

1. **Multi-param blocks**: `[:key :value | ...]` for Dictionary iteration
2. **inject:into:** Support two-argument accumulator blocks
3. **Non-local returns**: `^` from within a block
4. **Block storage**: Compiled blocks as first-class Go values
5. **Cross-method blocks**: Blocks passed between compiled methods
