# Why the Yutani IDE Fails: A Diagnostic Report

## Executive Summary

The Yutani IDE fails to launch due to a cascade of failures across the compilation and runtime layers. The native plugins compile but only implement a fraction of the required methods. When native dispatch returns "unknown selector", the bash fallback encounters fatal variable naming errors. This document details the root causes and provides recommendations for fixes.

## Error Symptoms

When running `@ Yutani::IDE launch`, the following errors occur:

```
lib/trash.bash: line 2092: __yutani_verticallayout_114fc207-9ce4-4779-93f2-125ddd46216d__setDispatcher___bashOnly: invalid variable name
lib/trash.bash: line 2092: __yutani_textview_ae4b81dd-de77-47d0-8bf0-3268aed29c9f__setDispatcher___bashOnly: invalid variable name
lib/trash.bash: line 514: __"yutani__eventdispatcher_2201069a"__superclass: invalid variable name
lib/trash.bash: line 2506: __Yutani__IDE__@  yutani__ide_a5713505 handleKey___procyonOnly: invalid variable name
```

---

## Root Cause Analysis

### Problem 1: Procyon Type Inference Bug - Conflating "Not JSON" with "Numeric"

**Location**: `~/dev/go/procyon/pkg/codegen/codegen.go:675`

**The Bug**:
```go
isNumericIvar := !g.jsonVars[target] // Numeric ivars are NOT in jsonVars
```

The `jsonVars` map contains instance variables that use `json.RawMessage` (JSON objects/arrays). The logic `!g.jsonVars[target]` evaluates to `true` for:
- Integer ivars (correct - these need `toInt()` conversion)
- **String ivars (incorrect - these should NOT use `toInt()`)**

**Impact**: When assigning a method parameter to a string ivar:

```smalltalk
method: setSession: s [
    session := s
]
```

Procyon generates:
```go
c.Session = toInt(s)  // WRONG! session is a string, not int
```

This causes Go validation to fail:
```
Widget.go:1256:14: cannot use toInt(s) (value of type int) as string value in assignment
```

**Affected Methods in Widget**:
- `setSession_`
- `setWidgetId_`
- `setTitle_`
- `setDispatcher_`
- `onSubmitDo_` (assigns handler parameter)
- `valueWith_` (assigns from message send result)

---

### Problem 2: Local Variables Typed as `interface{}` Break Predicates

**Location**: `~/dev/go/procyon/pkg/codegen/codegen.go:648-654`

**The Bug**:
```go
// Local variables - rename if they conflict with Go builtins
// Use interface{} for dynamic typing (Trashtalk is dynamically typed)
for _, v := range m.body.LocalVars {
    safeName := safeGoName(v)
    stmts = append(stmts, jen.Var().Id(safeName).Interface())
}
```

All local variables declared with `| var |` are typed as `interface{}`. When these variables are:
1. Assigned from string instance variables
2. Used with predicates like `notEmpty` (which internally calls `len()`)

Go fails because `len()` doesn't accept `interface{}`:

```smalltalk
method: focus [
    | s w |
    s := session.        "s is interface{}, assigned from string ivar"
    (s notEmpty) ifTrue: [...]  "len(s) fails - can't call len on interface{}"
]
```

**Go Error**:
```
Widget.go:1311:9: invalid argument: s (variable of type interface{}) for built-in len
```

**Affected Methods**:
- `delete` (uses `d notEmpty`, `w notEmpty`, `s notEmpty`)
- `focus` (uses `s notEmpty`, `w notEmpty`)
- `onEventDo_` (uses `d notEmpty`, `w notEmpty`)

---

### Problem 3: Native Methods Not Compiled - Cascade to Bash Fallback

**Evidence**: Running Procyon with validation shows most methods are skipped:

```
⚠ setSession_ - skipped: Go validation failed
⚠ setWidgetId_ - skipped: Go validation failed
⚠ setTitle_ - skipped: Go validation failed
⚠ setDispatcher_ - skipped: Go validation failed
⚠ delete - skipped: Go validation failed
⚠ focus - skipped: Go validation failed
⚠ onEventDo_ - skipped: Go validation failed
⚠ valueWith_ - skipped: Go validation failed
⚠ onSubmitDo_ - skipped: Go validation failed
```

The generated `dispatch()` function only handles 4 simple getters:

```go
func dispatch(c *Widget, selector string, args []string) (string, error) {
    switch selector {
    case "getSession":
        return c.GetSession(), nil
    case "getWidgetId":
        return c.GetWidgetId(), nil
    case "getTitle":
        return c.GetTitle(), nil
    case "getDispatcher":
        return c.GetDispatcher(), nil
    default:
        return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
    }
}
```

When `setDispatcher_` is called, the daemon returns `exit_code: 200`, triggering bash fallback.

---

### Problem 4: Bash Fallback - Instance ID Used as Class Name

**Location**: `lib/trash.bash:2025-2040`

**The Bug**:
```bash
if _is_instance "$_RECEIVER"; then
    class_name=$(_get_instance_class "$_RECEIVER")
    # ...
else
    class_name="$_RECEIVER"  # BUG: Instance ID becomes class name!
    # ...
fi
```

If `_is_instance` returns false (instance not yet in memory/database), the instance ID is used directly as the class name. Instance IDs contain UUIDs with hyphens:

```
yutani_verticallayout_114fc207-9ce4-4779-93f2-125ddd46216d
```

This ID is then used to construct bash variable names:

```bash
local func_prefix=$(_to_func_prefix "$class_name")
# Result: __yutani_verticallayout_114fc207-9ce4-4779-93f2-125ddd46216d

local bashOnly_marker="${func_prefix}__${normalized_selector}__bashOnly"
# Result: __yutani_verticallayout_114fc207-9ce4-4779-93f2-125ddd46216d__setDispatcher___bashOnly
```

Bash variable names cannot contain hyphens, causing the error:
```
invalid variable name
```

**Why does `_is_instance` fail?**

The `_is_instance` function checks if an instance exists in memory or the database:
```bash
function _is_instance {
    local maybe_instance="$1"
    local type_val=$(_get_instance_class "$maybe_instance")
    [[ -n "$type_val" ]]
}
```

This can fail if:
1. The instance was just created but not yet persisted
2. Race condition between native and bash execution contexts
3. The `_env_get` or `db_get` calls fail for the instance

---

### Problem 5: Malformed Selector in Variable Name

**Error**:
```
__Yutani__IDE__@  yutani__ide_a5713505 handleKey___procyonOnly: invalid variable name
```

This error shows that the selector `@  yutani__ide_a5713505 handleKey_` was embedded into the variable name construction. This happens when:

1. A handler string like `@ "yutani__ide_xxx" handleKey: "$__EVENT"` is registered
2. This string passes through the dispatcher
3. The selector parsing incorrectly captures the entire embedded command

**Location**: The selector normalization at `lib/trash.bash:2486-2489`:
```bash
local ___normalized="${___full_selector%:}"
___normalized="${___normalized//:/_}"
[[ "$___full_selector" == *: ]] && ___normalized="${___normalized}_"
```

When `___full_selector` contains spaces and `@` symbols from an embedded handler, the result is malformed.

---

## Method Compilation Status for Yutani::Widget

| Method | Native Status | Reason for Skip |
|--------|--------------|-----------------|
| `getSession` | ✅ Compiled | Simple getter |
| `setSession:` | ❌ Skipped | `toInt()` on string param |
| `getWidgetId` | ✅ Compiled | Simple getter |
| `setWidgetId:` | ❌ Skipped | `toInt()` on string param |
| `getTitle` | ✅ Compiled | Simple getter |
| `setTitle:` | ❌ Skipped | `toInt()` on string param |
| `getDispatcher` | ✅ Compiled | Simple getter |
| `setDispatcher:` | ❌ Skipped | `toInt()` on string param |
| `delete` | ❌ Skipped | `len()` on interface{} |
| `focus` | ❌ Skipped | `len()` on interface{} |
| `onEventDo:` | ❌ Skipped | `len()` on interface{} |
| `valueWith:` | ❌ Skipped | `toInt()` on message send result |
| `onSubmitDo:` | ❌ Skipped | `toInt()` on handler param |

---

## Recommendations

### Fix 1: Track Instance Variable Types Properly in Procyon

**Current State**: Only tracks `jsonVars` (json.RawMessage) vs everything else

**Proposed Change**: Track three categories:
```go
type generator struct {
    // ...
    intVars     map[string]bool  // Numeric ivars (use int)
    stringVars  map[string]bool  // String ivars (use string)
    jsonVars    map[string]bool  // JSON ivars (use json.RawMessage)
}
```

Then fix the assignment logic:
```go
if g.intVars[target] {
    expr = jen.Id("toInt").Call(...)
} else if g.stringVars[target] {
    expr = jen.Id(paramName)  // Direct string assignment
} else {
    // json.RawMessage handling
}
```

### Fix 2: Infer Local Variable Types from Assignments

Instead of always using `interface{}`, analyze the first assignment to determine type:

```go
// If assigned from string ivar: use string type
// If assigned from int ivar: use int type
// If assigned from message send: use string (default return type)
// Otherwise: use interface{}
```

Or generate type assertions at point of use:
```go
s := session  // s is interface{}
if len(s.(string)) > 0 { ... }  // Type assert when using len()
```

### Fix 3: Validate Instance Existence Before Variable Name Construction

In `lib/trash.bash`, add validation before constructing variable names:

```bash
# Validate class_name doesn't look like an instance ID
if [[ "$class_name" =~ ^[a-z].*_[a-f0-9-]+$ ]]; then
    echo "Error: Instance $class_name not found in registry" >&2
    return 1
fi
```

### Fix 4: Ensure Instance Persistence Before Method Calls

The native `new` method should ensure the instance is persisted to SQLite before returning, so bash fallback can find it:

```go
// In Object.New() or equivalent
instance := createInstance()
saveToDatabase(instance)  // Must happen before returning ID
return instance.ID
```

### Fix 5: Sanitize Selector Parsing for Embedded Commands

Add validation to reject selectors containing invalid characters:

```bash
# In selector parsing
if [[ "$___full_selector" =~ [@\$\"\'] ]]; then
    echo "Error: Invalid characters in selector: $___full_selector" >&2
    return 1
fi
```

---

## Architectural Observations

1. **The hybrid native/bash model creates synchronization challenges**: Instance state must be visible to both execution contexts, but timing issues cause lookup failures.

2. **Go's static typing conflicts with Trashtalk's dynamic semantics**: The use of `interface{}` for local variables is a workaround that breaks down when Go's type system is needed (e.g., `len()`).

3. **Validation-based method skipping is silent failure**: When methods fail Go validation, they're silently excluded from the dispatch table. At runtime, this manifests as "unknown selector" with no indication of the underlying cause.

4. **The fallback chain is fragile**: native → bash fallback works only if both contexts have consistent views of instance state. Any desynchronization causes hard-to-debug failures.

---

## Test Case for Verification

After fixes are applied, this should work without errors:

```bash
source lib/trash.bash
@ Yutani::IDE launch
```

Intermediate verification:
```bash
# Check that Widget methods compile natively
cd ~/.trashtalk
lib/jq-compiler/driver.bash parse trash/Yutani/Widget.trash | \
    lib/procyon/procyon --mode=shared 2>&1 | grep -E "skipped|warning"
# Should output nothing (all methods compile)
```

---

*Report generated: 2026-01-17*
*Diagnostic session for Trashtalk/Procyon native execution failures*

---

## Language Architect Review

### 1. Assessment of the Hybrid Native/Bash Model

The current architecture attempts to provide gradual migration from interpreted Bash to compiled native execution. This is a reasonable strategy in principle, but the implementation has fundamental coherence issues.

**The Core Problem: Two Runtime Semantics**

The Bash runtime and Go native runtime have different memory models, different typing disciplines, and different execution semantics. The current design assumes they can be freely interleaved, but this creates a "semantic impedance mismatch":

- **Instance Identity**: Bash uses string-based instance IDs and SQLite for persistence. Native code uses Go structs with in-memory state. The assumption that these can stay synchronized across arbitrary interleaving is fragile. Race conditions and visibility issues are inevitable.

- **Type Representation**: Bash is stringly-typed (everything is a string). Go is statically typed. The current approach of "methods return strings, convert at boundaries" works for simple cases but fails when complex types (arrays, objects, closures) cross the boundary.

- **Failure Modes**: When native dispatch fails, the fallback to Bash assumes Bash has a complete, consistent view of the world. But if the native code has mutated state, Bash may see stale or incomplete data.

**Architectural Recommendation**

Rather than arbitrary interleaving, consider a **stratified execution model**:

1. **Primitive Layer**: Implemented entirely in Go. These are the system primitives (File, Shell, Console, etc.) that interface with the OS.

2. **Application Layer**: Pure Trashtalk classes that compile to native code and execute entirely within the native runtime.

3. **Interop Layer**: Well-defined entry/exit points between the two worlds, with explicit serialization/deserialization protocols.

This eliminates the "falling back to Bash mid-execution" problem. Either a method runs entirely native, or entirely Bash. No mixing.

---

### 2. Alternative Type Inference Strategies

The current type inference is minimal: it tracks `jsonVars` (things with JSON defaults) versus everything else. The binary classification "is it JSON or is it numeric?" is insufficient because Trashtalk has at least three distinct runtime types:

- **Integers**: Used in arithmetic, loop counters, array indices
- **Strings**: Used for identifiers, messages, handlers, file paths
- **JSON Values**: Used for structured data (arrays, objects)

**Strategy A: Flow-Sensitive Type Inference**

Track types through the control flow graph. Start from known type sources (literals, ivar declarations with typed defaults, method parameters of known types), then propagate:

```
value := 0            --> value is INT
name := 'foo'         --> name is STRING
items := '[]'         --> items is JSON

result := value + 1   --> result is INT (INT op INT -> INT)
message := name , bar --> message is STRING (STRING concat STRING -> STRING)
```

This requires:
1. Building a def-use chain for each variable
2. Propagating types along assignments
3. Using meet/join operations at control flow merge points

For local variables declared with `| x y z |`, infer from first assignment. If no assignment before use, default to `interface{}` but generate type assertions at use sites.

**Strategy B: Bidirectional Type Checking**

Use the expected type (from the assignment target) to guide expression type checking:

```smalltalk
method: setName: n [
    name := n    "name is STRING ivar, so n must be STRING, no toInt()"
]
```

The assignment `name := n` knows the target type is `string`. Push that expectation down to the expression `n`. Since `n` is a parameter, we can either:
- Assume parameters are strings (they come from the dispatcher as strings)
- Add explicit type annotations to method signatures in the DSL

**Strategy C: Union Types with Runtime Dispatch**

Accept that Trashtalk is dynamically typed. Use a union type representation:

```go
type TTValue struct {
    tag   uint8  // 0=nil, 1=int, 2=string, 3=json
    int_  int
    str_  string
    json_ json.RawMessage
}
```

All local variables are `TTValue`. All operations check the tag and dispatch accordingly. This trades performance for correctness.

**Recommended Approach**

Strategy A (flow-sensitive inference) with Strategy B (bidirectional checking) as a refinement. Key insight: **parameters are always strings** because the dispatcher passes strings. Instance variables have known types from their declarations. Use these as the seeds, propagate through assignments.

The `inferType` function already does this for struct fields. Extend it to method bodies:

```go
type generator struct {
    // Existing
    instanceVars    map[string]bool
    jsonVars        map[string]bool

    // Add
    stringVars      map[string]bool   // Non-JSON, non-numeric ivars
    intVars         map[string]bool   // Numeric ivars

    // Per-method tracking
    localVarTypes   map[string]string // "int", "string", "json", "interface{}"
}
```

In `generateMethodBody`, before generating statements, analyze the body to infer local variable types. Then use those types during code generation.

---

### 3. Solutions for the interface{} vs Concrete Type Problem

The fundamental issue: `interface{}` erases type information that Go's static type system needs. The report shows two manifestations:

1. `len()` cannot be called on `interface{}`
2. Assignment of `interface{}` to typed fields requires conversion

**Solution 1: Generate Type Assertions at Use Sites**

When a local variable typed as `interface{}` is used in a context requiring a concrete type, generate a type assertion:

```go
// Before: s := session (interface{} := string)
// len(s) -- ERROR

// After:
s := session
len(s.(string))  // Type assertion at use site
```

Pros: Works with current architecture. Cons: Runtime panics on type mismatch.

**Solution 2: Generate Typed Locals with Deferred Declaration**

Don't declare local variables at method entry. Instead, infer types from first assignment and declare there:

```go
// Before:
var s interface{}
s = session
len(s)  // ERROR

// After:
s := session  // s is inferred as string from assignment
len(s)        // OK
```

This requires two passes over the method body: first to collect assignments and infer types, second to generate code with proper declarations.

**Solution 3: Wrapper Functions for Predicates**

The `notEmpty` predicate calls `len()`. Instead of generating `len(s) > 0`, generate a helper call:

```go
func _notEmpty(v interface{}) bool {
    switch x := v.(type) {
    case string:
        return len(x) > 0
    case []interface{}:
        return len(x) > 0
    case json.RawMessage:
        return len(x) > 0 && string(x) != "null" && string(x) != "[]" && string(x) != "{}"
    default:
        return v != nil
    }
}
```

Then `(s notEmpty)` compiles to `_notEmpty(s)` instead of `len(s) > 0`.

**Recommended Approach**

Combine Solutions 2 and 3:

1. Infer local variable types from assignments. Most variables will resolve to `string` (from parameters or string ivars) or `int` (from numeric ivars or arithmetic).

2. For truly dynamic cases (variable assigned from multiple sources with different types), fall back to `interface{}` but use helper functions that handle the type switching internally.

3. Never generate raw `len()` calls on `interface{}`. Always use `_notEmpty()`, `_isEmpty()`, etc.

---

### 4. The Validation-Skip Approach: Right Design or Wrong?

The current approach: generate Go code for all methods, run `go/types` validation, skip methods that fail validation, and fall back to Bash for those methods.

**Assessment: This is a Reasonable Strategy, But the Fallback is Broken**

The validation-skip approach is actually sound compiler design. It's a form of **partial evaluation**: compile what you can, interpret what you can't. Many production systems use this pattern (JIT compilers with fallback to interpreter, for example).

The problems identified in this report are not with the skip-and-fallback architecture, but with:

1. **The fallback itself is broken** (instance lookup failures, malformed selectors)
2. **Too many methods fail validation** (type inference bugs cause unnecessary skips)
3. **Silent failure modes** (no diagnostic when native fails and fallback also fails)

**Improvements to the Current Approach**

1. **Make validation errors visible**: Log which methods were skipped and why, not just to stderr during compilation, but persist this information so runtime errors can reference it.

2. **Fail fast on known-broken fallback**: If the fallback mechanism has known bugs (like the hyphen-in-variable-name issue), detect these conditions and error immediately rather than producing cryptic failures.

3. **Compile-time validation of fallback path**: If method X is skipped, verify that a Bash implementation exists and is syntactically valid before declaring success.

4. **Monotonic improvement guarantee**: Track which methods are native vs. fallback. Each compilation should either maintain or improve this set. Regressing from native to fallback should be a warning.

**Alternative: Compilation or Bust**

An alternative philosophy: if a method can't be compiled to native, that's a compiler bug, not a feature. Fix the compiler until all methods compile.

This requires:
- More sophisticated type inference
- Escape hatches for genuinely dynamic code (explicit `any` annotations)
- Incremental development of native capabilities

This is the long-term correct answer, but requires more investment.

---

### 5. Insights from Compiler/Language Implementation Theory

**The Expression Problem Manifests Here**

Trashtalk has dynamic dispatch (Smalltalk-style message sends). Go has static dispatch (method calls resolved at compile time). The hybrid model tries to bridge these, but it's a classic expression problem: extending behavior in one dimension (new methods in Bash) doesn't automatically extend in the other (native dispatch table).

Solution: Use **open recursion** or **vtable-like dispatch** in the native code. Instead of a static `switch` on selectors, use a `map[string]func(...)` that can be populated dynamically.

**The Strachey Principle**

Christopher Strachey's principle: "It is important to distinguish clearly between what a program means (semantics) and how it is executed (implementation)."

The current implementation conflates semantics with implementation strategy. The "meaning" of a Trashtalk program shouldn't depend on whether it executes in Bash or native. But currently, subtle differences in type handling, string representation, and error propagation create observable semantic differences.

Recommendation: Define a formal semantics for Trashtalk (even informally, as reference behavior). Ensure both Bash and native implementations produce identical observable results for the same inputs.

**Milner's "Well-Typed Programs Don't Go Wrong"**

The type errors in this report (assigning `int` to `string` field) are exactly what static typing should prevent. The problem is that Trashtalk is dynamically typed, but Go is statically typed, and the mapping between them is lossy.

Consider: **Gradual typing with explicit boundaries**. Mark certain Trashtalk constructs as "typed" (use type annotations in the DSL), and generate statically-typed Go for those. Leave dynamic portions to `interface{}` with runtime checks.

**The Futamura Projections**

The Futamura projections describe relationships between interpreters, compilers, and specializers. What Procyon is doing is a form of **partial evaluation**: taking a Trashtalk program and specializing it to Go code.

The validation-skip approach is related to **polyvariant specialization**: generating different code for different type configurations. Consider making this explicit: generate multiple versions of methods for different type scenarios, dispatch to the appropriate one at runtime.

**Practical Wisdom from Production Compilers**

GHC (Haskell), V8 (JavaScript), and LuaJIT all deal with dynamic-to-static compilation. Common patterns:

1. **Speculation with Guards**: Assume types based on runtime profiles, add guards that bail out to slow path if assumptions are violated.

2. **Inline Caching**: For message sends, cache the last resolved method at the call site.

3. **Type Specialization**: Generate specialized versions for common type patterns, fall back to generic version otherwise.

These are applicable if you want high-performance native execution. For correctness-first development, focus on making the basic type inference work correctly before optimizing.

---

### Summary of Recommendations

**Immediate Fixes (address the bugs in this report)**:

1. Track three type categories: `intVars`, `stringVars`, `jsonVars`. Use appropriate assignment code for each.

2. Infer local variable types from first assignment. Generate typed declarations, not `interface{}`.

3. Use helper functions (`_notEmpty`, `_isEmpty`) instead of raw `len()` on potentially-dynamic values.

4. Fix the Bash fallback: validate instance existence before constructing variable names; sanitize selectors; ensure native creates persist to SQLite before returning.

**Medium-Term Improvements**:

5. Implement flow-sensitive type inference with bidirectional checking.

6. Generate type assertions at use sites for genuinely dynamic values.

7. Add diagnostics: persist skip reasons, validate fallback availability, warn on regression.

**Long-Term Architecture**:

8. Consider stratified execution: primitive layer (all Go), application layer (all Trashtalk->native), interop layer (explicit serialization).

9. Define reference semantics for Trashtalk; ensure Bash and native implementations are observationally equivalent.

10. Explore gradual typing: allow explicit type annotations in Trashtalk for performance-critical code.

---

*Review completed: 2026-01-17*
*Reviewed by: Language Architect (Opus 4.5)*

