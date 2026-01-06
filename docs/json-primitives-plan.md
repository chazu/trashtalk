# JSON Primitives Implementation Plan

## Progress Summary

| Phase | Status | Completion Date |
|-------|--------|-----------------|
| Phase 1: JSON Primitives in jq-compiler | ✅ Complete | 2026-01-06 |
| Phase 2: Rewrite Array/Dictionary | ✅ Complete | 2026-01-06 |
| Phase 3: JSON Primitives in Procyon | ✅ Complete | 2026-01-06 |
| Phase 4: Optimization | ✅ Complete | 2026-01-06 |

---

## Overview

This plan eliminates Trashtalk's dependency on jq for JSON operations by introducing a set of **JSON primitive operations** that compile to:
- **jq pipelines** in the Bash compiler (backward compatible)
- **Native Go code** in Procyon (high performance)

This enables rewriting Array/Dictionary as regular methods (not rawMethods), making them fully compilable by Procyon while maintaining 100% backward compatibility for Bash-only users.

## Goals

1. **Eliminate jq dependency for Procyon-compiled code**
2. **Maintain full backward compatibility** - existing code works unchanged
3. **Enable Array/Dictionary to be Procyon-compilable**
4. **Clean, Smalltalk-style syntax** for JSON operations
5. **Type-aware code generation** for better Go performance

---

## Phase 1: JSON Primitives in jq-compiler ✅ COMPLETE

### Implementation Notes (2026-01-06)

**Changes made to `lib/jq-compiler/codegen.jq`:**
- Added `expr_is_json_array_keyword`, `expr_is_json_object_keyword`, `expr_is_json_unary` detection functions
- Added JSON primitive parsing in `expr_parse_expr` infix loop (after test predicates, before boolean operators)
- Added `json_primitive` type code generation in `expr_gen` function
- Added `gen_jq_arg` helper for proper jq `--arg` string handling
- Updated `should_use_expr_parser` to detect JSON primitives
- **Bug fix:** `varsToString` now escapes double quotes in default values (fixes JSON defaults like `'["x","y","z"]'`)

**Tests created:**
- `lib/jq-compiler/tests/test_json_primitives.bash` - 16 codegen tests (all pass)
- `lib/jq-compiler/tests/test_json_primitives_runtime.bash` - 6 runtime tests (all pass)

### 1.1 New Token Types

Add to `lib/jq-compiler/tokenizer.bash`:

```
JSON_ARRAY_OP    - arrayPush:, arrayAt:, arrayLength, etc.
JSON_OBJECT_OP   - objectAt:, objectKeys, objectHasKey:, etc.
```

These are recognized as special keywords (like `ifTrue:`) rather than regular method calls.

### 1.2 Primitive Operations

#### Array Primitives

| Primitive | Syntax | Description |
|-----------|--------|-------------|
| `arrayPush:` | `arr arrayPush: val` | Append element, return new array |
| `arrayAt:` | `arr arrayAt: idx` | Get element at index |
| `arrayAt:put:` | `arr arrayAt: idx put: val` | Set element, return new array |
| `arrayLength` | `arr arrayLength` | Get array size |
| `arrayRemoveAt:` | `arr arrayRemoveAt: idx` | Remove element, return new array |
| `arrayFirst` | `arr arrayFirst` | Get first element |
| `arrayLast` | `arr arrayLast` | Get last element |
| `arrayIsEmpty` | `arr arrayIsEmpty` | Check if empty |

#### Object Primitives

| Primitive | Syntax | Description |
|-----------|--------|-------------|
| `objectAt:` | `obj objectAt: key` | Get value for key |
| `objectAt:put:` | `obj objectAt: key put: val` | Set key, return new object |
| `objectKeys` | `obj objectKeys` | Get all keys as array |
| `objectValues` | `obj objectValues` | Get all values as array |
| `objectHasKey:` | `obj objectHasKey: key` | Check key existence |
| `objectRemoveKey:` | `obj objectRemoveKey: key` | Delete key, return new object |
| `objectLength` | `obj objectLength` | Get number of keys |
| `objectIsEmpty` | `obj objectIsEmpty` | Check if empty |

### 1.3 Parser Changes

In `lib/jq-compiler/parser.jq`:

- Recognize JSON primitive keywords after expressions
- Parse as special AST nodes (not regular message sends)
- Example AST:

```json
{
  "type": "json_primitive",
  "operation": "arrayPush",
  "receiver": { "type": "identifier", "name": "items" },
  "args": [{ "type": "identifier", "name": "value" }]
}
```

### 1.4 Codegen Changes

In `lib/jq-compiler/codegen.jq`:

Generate jq pipelines for each primitive:

```bash
# arrayPush:
items=$(echo "$items" | jq -c --arg v "$value" '. + [$v]')

# arrayAt:
_result=$(echo "$items" | jq -r --argjson i "$index" '.[$i] // empty')

# objectAt:put:
data=$(echo "$data" | jq -c --arg k "$key" --arg v "$value" '.[$k] = $v')

# objectKeys
_result=$(echo "$data" | jq -r 'keys[]')
```

### 1.5 Testing

Create `lib/jq-compiler/tests/test_json_primitives.bash`:

- Test each primitive operation
- Test chaining: `arr arrayPush: x arrayPush: y`
- Test with instance variables
- Test error cases (index out of bounds, missing keys)

---

## Phase 2: Rewrite Array/Dictionary ✅ COMPLETE

### Implementation Notes (2026-01-06)

**Array.trash changes:**
- Simplified `at:`, `at:put:`, `push:`, `size` to use JSON primitives
- `pop` now uses `items arrayLast` + `items arrayRemoveAt: -1`
- Added new methods: `isEmpty`, `first`, `last` using primitives
- Kept `_getItemsArray` helper for rawMethods that still need it
- Kept `do:`, `collect:`, `select:`, `inject:` as rawMethods (need block iteration)

**Dictionary.trash changes:**
- Simplified `at:`, `at:put:`, `includesKey:`, `size`, `isEmpty` to use JSON primitives
- `clear` method uses `@ self setItems: '{}'` (direct assignment had quoting issues)
- Kept `_getItemsObject` helper for rawMethods
- Kept `keys`, `values`, `removeAt:` as rawMethods (need special jq behavior)
- Kept iteration methods as rawMethods

**Test results:**
- Dictionary tests: 24/24 pass
- Array manual tests: all operations verified working

### 2.1 Array.trash Rewrite

```smalltalk
Array subclass: Object
  include: Debuggable
  include: Persistable
  instanceVars: items:'[]'

  method: new [
    | array_id |
    array_id := $(_generate_instance_id "Array")
    _create_instance "Array" "$array_id"
    ^ $array_id
  ]

  method: getItems [
    ^ items
  ]

  method: setItems: newItems [
    items := newItems
  ]

  method: at: index [
    ^ items arrayAt: index
  ]

  method: at: index put: value [
    items := items arrayAt: index put: value
    ^ value
  ]

  method: push: value [
    items := items arrayPush: value
    ^ value
  ]

  method: pop [
    | last |
    last := items arrayLast
    items := items arrayRemoveAt: (items arrayLength - 1)
    ^ last
  ]

  method: size [
    ^ items arrayLength
  ]

  method: isEmpty [
    ^ items arrayIsEmpty
  ]

  method: first [
    ^ items arrayFirst
  ]

  method: last [
    ^ items arrayLast
  ]

  # Iteration methods still need blocks - keep as rawMethod for now
  rawMethod: do: block [
    # ... existing implementation ...
  ]
```

### 2.2 Dictionary.trash Rewrite

```smalltalk
Dictionary subclass: Object
  include: Debuggable
  instanceVars: items:'{}'

  method: new [
    | dict_id |
    dict_id := $(_generate_instance_id "Dictionary")
    _create_instance "Dictionary" "$dict_id"
    ^ $dict_id
  ]

  method: at: key [
    ^ items objectAt: key
  ]

  method: at: key put: value [
    items := items objectAt: key put: value
    ^ value
  ]

  method: includesKey: key [
    ^ items objectHasKey: key
  ]

  method: removeAt: key [
    | oldValue |
    oldValue := items objectAt: key
    items := items objectRemoveKey: key
    ^ oldValue
  ]

  method: keys [
    ^ items objectKeys
  ]

  method: values [
    ^ items objectValues
  ]

  method: size [
    ^ items objectLength
  ]

  method: isEmpty [
    ^ items objectIsEmpty
  ]

  # Iteration methods still need blocks - keep as rawMethod
  rawMethod: do: block [
    # ... existing implementation ...
  ]
```

### 2.3 Backward Compatibility

- All existing code continues to work
- The `do:`, `collect:`, `select:` methods remain rawMethods (they need Block evaluation)
- Tests must pass before and after rewrite

---

## Phase 3: JSON Primitives in Procyon ✅ COMPLETE

### Implementation Notes (2026-01-06)

**Changes made to `~/dev/go/procyon/pkg/parser/parser.go`:**
- Added `JSONPrimitiveExpr` type for representing JSON primitive operations
- Added `isJSONPrimitiveUnary()` to detect unary primitives (arrayLength, arrayFirst, etc.)
- Added `isJSONPrimitiveKeyword()` to detect keyword primitives (arrayPush:, arrayAt:, etc.)
- Added `parseJSONPrimitive()` to parse JSON primitive expressions after primary expressions
- Modified `parseMulDiv()` to call `parseJSONPrimitive` after parsing primaries
- Added STRING token support for method arguments

**Changes made to `~/dev/go/procyon/pkg/codegen/codegen.go`:**
- Added `isJSONArrayType()` and `isJSONObjectType()` to detect JSON-typed instance variables
- Updated `inferType()` to return `[]interface{}` for array defaults and `map[string]interface{}` for object defaults
- Updated struct initialization to create empty slices/maps for JSON types
- Added `generateStringArg()` to preserve string keys for object operations
- Added `generateJSONPrimitive()` to generate Go code for all JSON primitive operations
- Added `generateJSONHelpers()` to generate helper functions:
  - `_boolToString`, `_toStr` - conversion helpers
  - `_arrayFirst`, `_arrayLast`, `_arrayAtPut`, `_arrayRemoveAt` - native slice helpers
  - `_mapKeys`, `_mapValues`, `_mapHasKey`, `_mapAtPut`, `_mapRemoveKey` - native map helpers
  - `_jsonArray*`, `_jsonObject*` - JSON string parsing helpers

**Test results:**
- Array.trash: 10/18 methods compile (including all JSON primitive methods)
- Dictionary.trash: 8/24 methods compile (including all JSON primitive methods)
- Skipped methods are raw methods or use unsupported features (subshell, block iteration)

### 3.1 AST Types

Add to `pkg/ast/types.go`:

```go
// JSONPrimitive represents a JSON operation
type JSONPrimitive struct {
    Operation string   // "arrayPush", "objectAt", etc.
    Args      []string // argument names from source
}

// Token types for JSON primitives
const (
    TokenArrayPush     = "ARRAY_PUSH"
    TokenArrayAt       = "ARRAY_AT"
    TokenArrayLength   = "ARRAY_LENGTH"
    // ... etc
)
```

### 3.2 Parser Changes

Add to `pkg/parser/parser.go`:

```go
// JSONPrimitiveExpr represents: receiver arrayPush: value
type JSONPrimitiveExpr struct {
    Receiver  Expr
    Operation string   // "arrayPush", "objectAt", etc.
    Args      []Expr
}

func (JSONPrimitiveExpr) exprNode() {}

// Recognize JSON primitive keywords after expression
func (p *Parser) parseJSONPrimitive(receiver Expr) (Expr, error) {
    keyword := p.peek().Value
    switch keyword {
    case "arrayPush:":
        return p.parseArrayPush(receiver)
    case "arrayAt:":
        return p.parseArrayAt(receiver)
    // ... etc
    }
    return nil, nil // Not a JSON primitive
}
```

### 3.3 Type-Aware Struct Generation

Modify `pkg/codegen/codegen.go` to recognize JSON-typed instance variables:

```go
func (g *generator) inferType(iv ast.InstanceVar) *jen.Statement {
    switch iv.Default.Value {
    case "'[]'", "[]":
        return jen.Index().Interface() // []interface{}
    case "'{}'", "{}":
        return jen.Map(jen.String()).Interface() // map[string]interface{}
    }
    if iv.Default.Type == "number" {
        return jen.Int()
    }
    return jen.String()
}
```

Generated struct:
```go
type Array struct {
    Class     string        `json:"class"`
    CreatedAt string        `json:"created_at"`
    Items     []interface{} `json:"items"`  // Native slice!
}
```

### 3.4 Codegen for JSON Primitives

Add to `pkg/codegen/codegen.go`:

```go
func (g *generator) generateJSONPrimitive(expr *parser.JSONPrimitiveExpr, m *compiledMethod) *jen.Statement {
    receiver := g.generateExpr(expr.Receiver, m)

    switch expr.Operation {
    case "arrayPush":
        // c.Items = append(c.Items, value)
        arg := g.generateExpr(expr.Args[0], m)
        return receiver.Op("=").Append(receiver, arg)

    case "arrayAt":
        // c.Items[index]
        idx := g.generateExpr(expr.Args[0], m)
        return receiver.Index(idx)

    case "arrayLength":
        // len(c.Items)
        return jen.Len(receiver)

    case "objectAt":
        // c.Data[key]
        key := g.generateExpr(expr.Args[0], m)
        return receiver.Index(key)

    case "objectAtPut":
        // c.Data[key] = value
        key := g.generateExpr(expr.Args[0], m)
        val := g.generateExpr(expr.Args[1], m)
        return receiver.Index(key).Op("=").Add(val)

    // ... etc
    }
}
```

### 3.5 String-Typed Fallback

For receivers that aren't typed ivars (just strings containing JSON), generate marshal/unmarshal:

```go
// items arrayPush: value (where items is a string variable)
func generateArrayPushOnString(receiver, value *jen.Statement) []jen.Code {
    return []jen.Code{
        jen.Var().Id("_arr").Index().Interface(),
        jen.Qual("encoding/json", "Unmarshal").Call(
            jen.Index().Byte().Parens(receiver),
            jen.Op("&").Id("_arr"),
        ),
        jen.Id("_arr").Op("=").Append(jen.Id("_arr"), value),
        jen.List(jen.Id("_data"), jen.Id("_")).Op(":=").
            Qual("encoding/json", "Marshal").Call(jen.Id("_arr")),
        receiver.Op("=").String().Parens(jen.Id("_data")),
    }
}
```

---

## Phase 4: Optimization

### 4.1 Direct Field Access

When we know the receiver is a typed instance variable, skip marshal/unmarshal:

```go
// c.Items is []interface{}, so:
// items arrayPush: value
// becomes simply:
c.Items = append(c.Items, value)
```

### 4.2 Operation Chaining

Optimize chains like `arr arrayPush: x arrayPush: y`:

```go
// Instead of two append calls, generate:
c.Items = append(c.Items, x, y)
```

### 4.3 Lazy Serialization

Only serialize back to JSON when saving to database, not after every operation.

### Implementation Notes (2026-01-06)

**4.1 Direct Field Access** - Already implemented in Phase 3:
- `isJSONArrayType()` and `isJSONObjectType()` detect typed instance variables
- Typed fields generate direct Go operations (`len()`, `append()`, map indexing)
- Untyped fields fall back to `_json*` helper functions

**4.2 Operation Chaining:**

Changes to `pkg/parser/parser.go`:
- Modified `parseJSONPrimitive()` to loop and parse chained primitives
- Supports: `items arrayPush: x arrayPush: y arrayPush: z`

Changes to `pkg/codegen/codegen.go`:
- Added `exprResultsInArray()` and `exprResultsInObject()` for recursive type tracking
- Added `collectArrayPushArgs()` to collect all args from chained arrayPush operations
- Generates single `append(c.Items, x, y, z)` instead of nested `append(append(append(...)))`

**4.3 Lazy Serialization** - Already implemented in Phase 3:
- Struct fields use native Go types: `[]interface{}`, `map[string]interface{}`
- JSON serialization only happens during `loadInstance()` and `saveInstance()`

---

## File Changes Summary

### New Files

| File | Purpose |
|------|---------|
| `lib/jq-compiler/tests/test_json_primitives.bash` | Test JSON primitives in bash |
| `docs/json-primitives.md` | User documentation |

### Modified Files

| File | Changes |
|------|---------|
| `lib/jq-compiler/tokenizer.bash` | Add JSON primitive token types |
| `lib/jq-compiler/parser.jq` | Parse JSON primitive expressions |
| `lib/jq-compiler/codegen.jq` | Generate jq code for primitives |
| `trash/Array.trash` | Rewrite using primitives |
| `trash/Dictionary.trash` | Rewrite using primitives |
| `~/dev/go/procyon/pkg/ast/types.go` | Add JSON primitive AST types |
| `~/dev/go/procyon/pkg/parser/parser.go` | Parse JSON primitives |
| `~/dev/go/procyon/pkg/codegen/codegen.go` | Generate Go for primitives |

---

## Testing Strategy

### Unit Tests
- Each primitive operation in isolation
- Error handling (out of bounds, missing keys)
- Type coercion edge cases

### Integration Tests
- Array/Dictionary methods work as before
- Round-trip: create, modify, save, load, verify
- Mix of primitives and regular methods

### Performance Tests
- Compare jq vs native Go for bulk operations
- Measure improvement in Array/Dictionary operations

### Compatibility Tests
- Existing tests pass unchanged
- Code written before this change still works

---

## Rollout Plan

1. **Phase 1** (jq-compiler primitives)
   - Implement primitives in bash compiler
   - All existing tests must pass
   - No user-visible changes yet

2. **Phase 2** (Rewrite stdlib)
   - Rewrite Array/Dictionary using primitives
   - Verify identical behavior
   - Release as minor version

3. **Phase 3** (Procyon support)
   - Add primitives to Procyon
   - Array/Dictionary now compile natively
   - Significant performance improvement

4. **Phase 4** (Optimization)
   - Type-aware optimizations
   - Operation chaining
   - Profile and tune

---

## Success Metrics

| Metric | Before | After |
|--------|--------|-------|
| Array.push native compilation | No | Yes |
| Dictionary.at: native compilation | No | Yes |
| Methods using jq in Array | 12 | 0 (except iteration) |
| Methods using jq in Dictionary | 14 | 0 (except iteration) |
| Performance (1000 array pushes) | ~5s (jq) | ~50ms (native) |

---

## Open Questions

1. **Nesting syntax**: How to express `data objectAt: 'users' arrayAt: 0`?
   - Option A: Chain naturally (current plan)
   - Option B: Path syntax `data jsonPath: '$.users[0]'`

2. **Error handling**: What on index out of bounds?
   - Option A: Return empty string (current jq behavior)
   - Option B: Return nil/null
   - Option C: Throw error

3. **Mutability**: Primitives return new values. Should we also have mutating versions?
   - `arrayPush:` returns new array
   - `arrayPush!:` modifies in place?

4. **Iteration**: Keep `do:`/`collect:`/`select:` as rawMethods or find a way to compile them?
   - Requires Block compilation - future enhancement

---

## Timeline Estimate

- Phase 1: 2-3 sessions (jq-compiler changes)
- Phase 2: 1 session (rewrite Array/Dictionary)
- Phase 3: 2-3 sessions (Procyon changes)
- Phase 4: 1-2 sessions (optimization)

Total: ~7-9 sessions of focused work
