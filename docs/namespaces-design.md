# Trashtalk Namespaces Design Document

**Status**: Draft
**Author**: Claude + chazu
**Date**: 2025-12-22

## Overview

This document describes a simple, flat namespace system for Trashtalk to prevent class name collisions between independent packages.

## Goals

1. **Prevent class name collisions**: `MyApp::Counter` and `OtherLib::Counter` can coexist
2. **Keep it simple**: Flat namespaces, no nesting
3. **Compile-time resolution**: All namespace resolution happens during compilation
4. **Backward compatible**: Existing code continues to work
5. **Import convenience**: Avoid writing `MyApp::` everywhere within a package

## Non-Goals

1. **Method namespacing**: Cross-package class extensions remain unsupported
2. **Nested namespaces**: No `MyApp::SubModule::Counter`
3. **Runtime namespace manipulation**: No dynamic namespace changes
4. **Private classes**: All classes are public (for now)

## Syntax

### Package Declaration

Each `.trash` file can declare its package at the top:

```smalltalk
package: MyApp

Counter subclass: Object
  instanceVars: value:0

  method: increment [
    value := value + 1
  ]
```

### Import Declaration

Import other packages to use their classes without qualification:

```smalltalk
package: MyApp
  import: Logging
  import: DataStructures

Counter subclass: Object
  method: increment [
    " Logger is from Logging package "
    @ Logger info: 'incrementing'
    value := value + 1
  ]
```

### Qualified Class References

Reference classes from other packages explicitly:

```smalltalk
package: MyApp

Counter subclass: Object
  method: save [
    " Explicit package reference "
    @ Persistence::Store save: self
  ]
```

### Syntax Grammar

```
package_decl  := "package:" IDENTIFIER import_list?
import_list   := import_decl*
import_decl   := "import:" IDENTIFIER

class_ref     := IDENTIFIER                    # Unqualified
              | IDENTIFIER "::" IDENTIFIER     # Qualified

message_send  := "@" class_ref selector args*
```

## Name Resolution

### Resolution Order (Compile-Time)

When the compiler encounters an unqualified class name:

1. **Current package**: Look in the package being compiled
2. **Imported packages**: Search imports in declaration order
3. **Core package**: Built-in Trashtalk classes (Object, String, Array, etc.)
4. **Error**: If not found or ambiguous

### Ambiguity Handling

If the same class name exists in multiple imported packages:

```smalltalk
package: MyApp
  import: PackageA    " has Helper "
  import: PackageB    " also has Helper "

Foo subclass: Object
  method: test [
    @ Helper doSomething   " ERROR: ambiguous reference to Helper "
  ]
```

**Resolution**: Compile-time error. User must qualify: `@ PackageA::Helper doSomething`

### Qualified References

Qualified references (`Package::Class`) bypass the resolution order and directly reference the specified package.

## Implementation

### Phase 1: Tokenizer Changes

Add new token types:

| Token | Pattern | Example |
|-------|---------|---------|
| `PACKAGE_KW` | `package:` | `package:` |
| `IMPORT_KW` | `import:` | `import:` |
| `NAMESPACE_SEP` | `::` | `::` |

The `::` operator should be recognized when it appears between two identifiers.

### Phase 2: Parser Changes

#### AST Additions

```json
{
  "type": "class",
  "package": "MyApp",
  "imports": ["Logging", "DataStructures"],
  "name": "Counter",
  "parent": "Object",
  ...
}
```

#### New Parse Functions

```
parsePackageDecl:
  if current.value == "package:" then
    advance
    package_name = expect(IDENTIFIER)
    imports = []
    while current.value == "import:" do
      advance
      imports += expect(IDENTIFIER)
    return {package: package_name, imports: imports}
```

#### Qualified Name Parsing

In message sends, handle `@ Package::Class selector`:

```
parseMessageTarget:
  name = expect(IDENTIFIER)
  if current.type == "NAMESPACE_SEP" then
    advance
    class_name = expect(IDENTIFIER)
    return {type: "qualified", package: name, class: class_name}
  else
    return {type: "unqualified", class: name}
```

### Phase 3: Codegen Changes

#### Function Naming

Classes get namespace-prefixed function names:

| Source | Generated Function |
|--------|-------------------|
| `MyApp::Counter.increment` | `__MyApp__Counter__increment` |
| `Counter.increment` (in MyApp) | `__MyApp__Counter__increment` |
| Core `Object.new` | `__Core__Object__new` or `__Object__new` |

#### Metadata Generation

Update class metadata to include namespace:

```bash
__MyApp__Counter__class__metadata() {
  echo 'package:MyApp'
  echo 'name:Counter'
  echo 'parent:Object'
  echo 'instanceVars:value:0'
}
```

#### Name Resolution During Codegen

The codegen phase must resolve all unqualified names to fully-qualified names:

```python
def resolve_class(name, current_package, imports):
    # 1. Check current package
    if exists(current_package, name):
        return f"{current_package}::{name}"

    # 2. Check imports
    matches = []
    for pkg in imports:
        if exists(pkg, name):
            matches.append(f"{pkg}::{name}")

    if len(matches) == 1:
        return matches[0]
    elif len(matches) > 1:
        error(f"Ambiguous reference to {name}")

    # 3. Check Core
    if exists("Core", name):
        return f"Core::{name}"

    error(f"Unknown class {name}")
```

**Important**: This requires knowing what classes exist in each package at compile time. Options:

- **Option A**: Multi-pass compilation - first pass collects class names, second pass resolves
- **Option B**: Package manifest files - each package has a `.manifest` listing its classes
- **Option C**: Directory scanning - scan `trash/` for all `.trash` files and extract class names

Recommend **Option A** for simplicity.

### Phase 4: Runtime Changes

#### Instance ID Generation

Include namespace in instance IDs:

```
Current:  counter_a1b2c3d4
New:      myapp_counter_a1b2c3d4
```

Update `_generate_instance_id`:

```bash
_generate_instance_id() {
    local class="$1"
    local ns="${class%%::*}"
    local name="${class##*::}"
    local lower_ns=$(echo "$ns" | tr 'A-Z' 'a-z')
    local lower_name=$(echo "$name" | tr 'A-Z' 'a-z')
    echo "${lower_ns}_${lower_name}_$(uuidgen | tr -d '-' | head -c 8)"
}
```

#### Dispatcher Updates

The dispatcher should handle qualified class names in the `@` syntax:

```bash
# @ MyApp::Counter new
# @ $instance increment

send() {
    local target="$1"
    shift
    local selector="$1"
    shift

    # Determine class from target
    if [[ "$target" == *"::"* ]]; then
        # Qualified class reference
        _CLASS="${target}"
    elif [[ "$target" == *"_"* ]]; then
        # Instance ID - extract class
        _CLASS=$(_get_instance_class "$target")
    else
        # Unqualified - should have been resolved at compile time
        # This is a runtime reference, use as-is (legacy support)
        _CLASS="$target"
    fi

    # Convert :: to __ for function lookup
    local func_class="${_CLASS//::/__}"
    local func_name="__${func_class}__${selector}"

    ...
}
```

#### SQLite Schema

The instances table already has a `class` column. Store fully-qualified names:

```sql
-- Before
INSERT INTO instances (id, class, data) VALUES ('counter_abc', 'Counter', '{}');

-- After
INSERT INTO instances (id, class, data) VALUES ('myapp_counter_abc', 'MyApp::Counter', '{}');
```

### Phase 5: Procyon Changes

#### AST Parsing

Read the new `package` and `imports` fields from AST JSON.

#### Binary Naming

Namespace the binary:

```
Current:  ~/.trashtalk/trash/.compiled/Counter.native
New:      ~/.trashtalk/trash/.compiled/MyApp__Counter.native
```

#### Code Generation

The Go struct can stay simple (no Go package mapping needed):

```go
// Generated for MyApp::Counter
type Counter struct {
    ID    string
    Value int
}

// Binary identifies itself with namespace
var className = "MyApp::Counter"
```

## File Organization

### Recommended Structure

```
trash/
├── Core/
│   ├── Object.trash
│   ├── String.trash
│   └── Array.trash
├── MyApp/
│   ├── Counter.trash      # package: MyApp
│   └── Logger.trash       # package: MyApp
└── traits/
    ├── Debuggable.trash   # Traits remain global
    └── Serializable.trash
```

### Alternative: Flat with Headers

```
trash/
├── Object.trash           # package: Core (or no package = Core)
├── Counter.trash          # package: MyApp
├── Logger.trash           # package: MyApp
└── traits/
    └── Debuggable.trash
```

The `package:` header is authoritative; directory structure is just organization.

## Backward Compatibility

### Existing Code Without package:

Classes without a `package:` declaration go to a default namespace:

- **Option A**: `Main` namespace (explicit default)
- **Option B**: `Core` namespace (merge with stdlib)
- **Option C**: No namespace (legacy mode, functions named `__Counter__method`)

Recommend **Option C** for backward compatibility:
- Existing compiled code continues to work
- New code with `package:` gets namespaced
- Gradual migration path

### Migration Path

1. Existing classes work unchanged
2. Users can add `package:` declarations incrementally
3. Eventually, require `package:` for new classes (future version)

## Traits

Traits remain **global** (no namespace):

```smalltalk
package: MyApp

Counter subclass: Object
  include: Debuggable      # Global trait, no qualification needed
```

Rationale:
- Traits are library utilities, not application classes
- Trait method names should be unique anyway (they get mixed in)
- Keeps the common case simple

## Examples

### Basic Package

```smalltalk
" File: trash/myapp/Counter.trash "
package: MyApp

Counter subclass: Object
  instanceVars: value:0 step:1

  method: increment [
    value := value + step
  ]

  method: getValue [
    ^ value
  ]
```

### With Imports

```smalltalk
" File: trash/myapp/App.trash "
package: MyApp
  import: Logging
  import: Persistence

App subclass: Object
  method: run [
    @ Logger info: 'Starting app'

    | counter |
    counter := @ Counter new.
    @ counter increment.

    @ Store save: counter.
    @ Logger info: 'Done'
  ]
```

### Cross-Package Reference

```smalltalk
" File: trash/analytics/Tracker.trash "
package: Analytics

Tracker subclass: Object
  method: track: event [
    " Reference class from another package "
    | counter |
    counter := @ MyApp::Counter new.
    @ counter increment.
  ]
```

### Runtime Usage

```bash
source lib/trash.bash

# Create namespaced instance
counter=$(@ MyApp::Counter new)

# Use it
@ $counter increment
@ $counter getValue   # => 1

# Qualified class method
@ Analytics::Tracker track: 'pageview'
```

## Open Questions

### 1. Core Namespace Name
What should the standard library namespace be called?
- `Core`
- `Trashtalk`
- `Std`
- (no namespace - legacy)

**Recommendation**: `Core` or no namespace for backward compat.

### 2. Package Discovery
How does the compiler know what packages/classes exist?
- Directory scanning
- Manifest files
- Multi-pass compilation

**Recommendation**: Start with multi-pass compilation, add manifests later if needed.

### 3. Circular Imports
What if `MyApp` imports `OtherLib` which imports `MyApp`?

**Recommendation**: Allow it. Since resolution is compile-time and we don't execute imports, there's no actual cycle problem. Each file is compiled independently with its imports as context.

### 4. Version Conflicts
What if two packages depend on different versions of a third package?

**Recommendation**: Out of scope for v1. Users must ensure compatible versions. Future work could add version constraints.

### 5. Runtime Introspection
Should `@ object class` return `MyApp::Counter` or `Counter`?

**Recommendation**: Return fully-qualified `MyApp::Counter` for clarity.

## Implementation Plan

### Milestone 1: Parser Support
- [ ] Add tokens: `package:`, `import:`, `::`
- [ ] Parse package declaration
- [ ] Parse qualified class references
- [ ] Update AST output
- [ ] Tests

### Milestone 2: Codegen Support
- [ ] Namespace-prefixed function names
- [ ] Name resolution logic
- [ ] Updated metadata generation
- [ ] Tests

### Milestone 3: Runtime Support
- [ ] Dispatcher handles `Package::Class` syntax
- [ ] Namespaced instance IDs
- [ ] SQLite with qualified class names
- [ ] Tests

### Milestone 4: Procyon Support
- [ ] Parse namespace from AST
- [ ] Namespaced binary naming
- [ ] Update `--info` output
- [ ] Tests

### Milestone 5: Polish
- [ ] Error messages for ambiguous references
- [ ] Documentation
- [ ] Migrate Core classes (optional)

## References

- [GNU Smalltalk Namespaces](https://www.gnu.org/software/smalltalk/manual/html_node/Namespaces.html)
- [Pharo Modularization Analysis](https://inria.hal.science/hal-00780293/document)
- [Squeak Namespace Discussion](https://wiki.squeak.org/squeak/733)
