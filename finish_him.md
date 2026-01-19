# Trashtalk v1.0: Road to Completion

This document outlines everything needed to finalize Trashtalk v1.0 by discontinuing Procyon support, bringing the jq-compiler to full capability, and cleaning up the codebase.

## Executive Summary

**Goals:**
1. Remove Procyon dependency entirely (no native plugins, no tt daemon)
2. Ensure jq-compiler handles ALL .trash files (eliminate "legacy" fallback concept)
3. Remove `pragma: primitiveClass` - all classes become regular classes
4. Remove Yutani GUI framework (13 classes)
5. Get all relevant tests passing
6. Update documentation

**Current State:**
- 48 total .trash classes
- 17 primitive classes (will need conversion)
- 13 Yutani classes (will be deleted)
- 8 classes currently require "legacy" jq-compiler (Procyon can't handle them)
- 30 test files (some Procyon-specific)

---

## Part 1: Procyon Removal

### 1.1 Files to Delete

**Binaries and Build Artifacts:**
```
lib/procyon/procyon          # Procyon binary
lib/tt                        # Native daemon
lib/compile-plugin.sh         # Plugin compilation script
trash/.compiled/*.dylib       # Native plugins
trash/.compiled/*.h           # C headers
trash/.compiled/.build/       # Go build directory
```

**Procyon-related Classes:**
```
trash/Procyon.trash           # Procyon interface class
```

### 1.2 Makefile Changes

Remove these targets:
- `plugins` - builds native .dylib plugins
- `daemon` - builds tt daemon
- `build` - currently does bash + plugins + daemon
- `single` - has plugin compilation logic

Simplify to:
- `all` / `bash` - compile all classes with jq-compiler
- `test` - run tests
- `clean` - remove compiled artifacts

Remove Procyon from compile-bash.sh - no fallback needed, jq-compiler is the only compiler.

### 1.3 Runtime Changes (lib/trash.bash)

Remove:
- Native daemon communication (`_has_native_plugin`, `_call_native_daemon`)
- `procyonOnly` marker checking
- `procyonNative` marker checking
- Plugin loading logic
- tt daemon startup/shutdown

Keep:
- `bashOnly` marker (still useful for documentation)
- `direct` marker (bypasses subshell)
- All instance management (SQLite-based)
- Message dispatch for bash functions

### 1.4 Pragma Changes

**Remove entirely:**
- `pragma: procyonOnly` - no native-only methods
- `pragma: procyonNative` - no native fallback
- `pragma: primitiveClass` - see Part 3

**Keep:**
- `pragma: direct` - useful for shell variable modification
- `pragma: bashOnly` - documents bash-specific code (optional, informational)
- `pragma: primitive` - method-level, skips DSL transformation (same as rawMethod)

---

## Part 2: JQ-Compiler Feature Parity

### 2.1 Current "Legacy" Classes

These 8 classes fall back to legacy jq-compiler because Procyon can't compile them:

| Class | Why Legacy | Action |
|-------|------------|--------|
| Json.trash | JSON primitive syntax (`objectAt:put:`, `arrayPush:`) | Works fine with jq-compiler |
| Process.trash | Complex method bodies, trait inclusion | Works fine with jq-compiler |
| Transcript.trash | `pragma: primitive`, complex bash | Works fine with jq-compiler |
| Trash.trash | `pragma: primitive`, heredocs, complex bash | Works fine with jq-compiler |
| HttpServer.trash | Unknown | Investigate |
| Tools/Jq.trash | Unknown | Investigate |
| Tools/Netcat.trash | Unknown | Investigate |
| Procyon.trash | Procyon-specific (DELETE) | Delete |

**Resolution:** Once Procyon is removed, there's no "legacy" concept - jq-compiler compiles everything.

### 2.2 Features in LANGUAGE.md vs JQ-Compiler

| Feature | LANGUAGE.md | JQ-Compiler | Status |
|---------|-------------|-------------|--------|
| Class definition | Yes | Yes | ✓ |
| Inheritance | Yes | Yes | ✓ |
| Traits | Yes | Yes | ✓ |
| Instance variables | Yes | Yes | ✓ |
| Instance var defaults | Yes | Yes | ✓ |
| Class instance vars | Yes | Yes | ✓ |
| Namespaces/packages | Yes | Yes | ✓ |
| Regular methods | Yes | Yes | ✓ |
| Raw methods | Yes | Yes | ✓ |
| Class methods | Yes | Yes | ✓ |
| Keyword methods | Yes | Yes | ✓ |
| Method categories | Yes | Yes | ✓ |
| Method aliases | Yes | Yes | ✓ |
| Message sends (@) | Yes | Yes | ✓ |
| Cascades (;) | Yes | Yes | ✓ |
| Local variables | Yes | Yes | ✓ |
| Assignment (:=) | Yes | Yes | ✓ |
| Return (^) | Yes | Yes | ✓ |
| Blocks/closures | Yes | Yes | ✓ |
| Block parameters | Yes | Yes | ✓ |
| ifTrue:/ifFalse: | Yes | Yes | ✓ |
| ifNil:/ifNotNil: | Yes | Yes | ✓ |
| whileTrue:/whileFalse: | Yes | Yes | ✓ |
| timesRepeat: | Yes | Yes | ✓ |
| to:do: | Yes | Yes | ✓ |
| and:/or:/not | Yes | Yes | ✓ |
| Comparison operators | Yes | Yes | ✓ |
| Arithmetic operators | Yes | Yes | ✓ |
| Regex match (=~) | Yes | Yes | ✓ |
| String literals | Yes | Yes | ✓ |
| Triple-quoted strings | Yes | Yes | ✓ |
| Collection literals | Yes | Yes | ✓ |
| JSON primitives | Yes | Yes | ✓ |
| File test predicates | Yes | Yes | ✓ |
| String predicates | Yes | Yes | ✓ |
| Instance var inference | Yes | Yes | ✓ |
| Error handling | Yes | Yes | ✓ |
| Method advice (AOP) | Yes | Yes | ✓ |
| Source embedding | Yes | Yes | ✓ |
| pragma: direct | Yes | Yes | ✓ |
| pragma: primitive | Yes | Yes | ✓ |
| pragma: bashOnly | Yes | Yes | ✓ (informational) |
| pragma: procyonOnly | Yes | N/A | Remove from spec |
| pragma: procyonNative | Yes | N/A | Remove from spec |
| pragma: primitiveClass | Yes | Yes | Remove (see Part 3) |

**Conclusion:** JQ-compiler already supports all necessary features. No new features needed.

### 2.3 Known Issues (from LANGUAGE.md)

These are documented limitations that remain:

1. **Method name collision**: `skip:` and `skip` compile to same function
   - Mitigation: Avoid this pattern in class design

2. **Negative numbers in arguments**: `0 -1` may become `0-1`
   - Mitigation: Use variables for negative literals

3. **Non-local returns in custom blocks**: `^` only works in compiler-known control flow
   - This is a fundamental bash limitation, document and accept

---

## Part 3: Remove `pragma: primitiveClass`

### 3.1 Current Primitive Classes (17)

| Class | Raw Methods | Purpose |
|-------|-------------|---------|
| Block | 5 | Closure/block execution |
| Console | 2 | Standard I/O |
| Coproc | 10 | Coprocess management |
| Env | 5 | Environment variables |
| FIFO | 8 | Named pipes |
| File | 15 | File system operations |
| Future | 12 | Async computation |
| GrpcClient | 8 | gRPC client (uses grpcurl) |
| Http | 6 | HTTP requests (uses curl) |
| Object | 10 | Base object class |
| Protocol | 4 | Protocol definitions |
| Runtime | 8 | Runtime system |
| Shell | 15 | Shell command execution |
| Store | 8 | Key-value persistence |
| String | 20 | String manipulation |
| Time | 10 | Date/time operations |
| Tool | 3 | Tool base class |

### 3.2 Conversion Strategy

The `pragma: primitiveClass` only serves two purposes:
1. **Compiler validation**: Ensures all methods are raw (no DSL methods)
2. **Runtime optimization**: Potential dispatch optimization (not currently used)

**Action:** Remove the pragma but keep the classes as-is. They already work with raw methods.

**Steps for each primitive class:**
1. Remove `pragma: primitiveClass` line
2. Keep all `rawMethod:` and `rawClassMethod:` declarations unchanged
3. No other changes needed - raw methods already compile correctly

### 3.3 Compiler Changes

In `lib/jq-compiler/codegen.jq`:
- Remove `primitiveClass` pragma handling
- Remove validation that all methods must be raw
- Keep emitting class metadata without primitiveClass marker

In `lib/trash.bash`:
- Remove `__ClassName__primitiveClass` marker checking

---

## Part 4: Remove Yutani Framework

### 4.1 Files to Delete

```
trash/Yutani/Application.trash
trash/Yutani/Event.trash
trash/Yutani/EventDispatcher.trash
trash/Yutani/FlexLayout.trash
trash/Yutani/HorizontalLayout.trash
trash/Yutani/IDE.trash
trash/Yutani/InputField.trash
trash/Yutani/ListView.trash
trash/Yutani/Session.trash
trash/Yutani/TextView.trash
trash/Yutani/Transcript.trash
trash/Yutani/VerticalLayout.trash
trash/Yutani/Widget.trash
trash/Yutani/                    # Directory itself
```

Also delete the non-namespaced Transcript that depends on Yutani:
```
trash/Transcript.trash
```

### 4.2 Compiled Artifacts

```
trash/.compiled/Yutani__*        # All compiled Yutani classes
```

### 4.3 Tests to Delete

```
tests/test_yutani_event.bash
tests/test_yutani_event_dispatcher.bash
tests/test_yutani_widget.bash
```

### 4.4 Documentation Updates

Remove Yutani references from:
- LANGUAGE.md (if any)
- CLAUDE.md (if any)
- Any other docs mentioning GUI framework

---

## Part 5: Test Updates

### 5.1 Tests to Delete (Procyon-specific)

```
tests/test_pragma_procyonOnly.bash   # Tests procyonOnly pragma
tests/test_native_daemon.bash        # Tests tt daemon
tests/test_native_execution.bash     # Tests native plugin execution
tests/test_native_speed.bash         # Benchmarks native vs bash
```

### 5.2 Tests to Delete (Yutani-specific)

```
tests/test_yutani_event.bash
tests/test_yutani_event_dispatcher.bash
tests/test_yutani_widget.bash
```

### 5.3 Tests to Update

| Test | Change Needed |
|------|---------------|
| test_grpc_client.bash | Keep - tests GrpcClient with grpcurl fallback |
| test_grpc_integration.bash | Keep - tests gRPC integration |
| test_pragma_direct.bash | Keep - pragma: direct still works |
| All other tests | Review for Procyon references |

### 5.4 Tests to Keep (Core Functionality)

```
tests/test_bytecode_blocks.bash
tests/test_collection_methods.bash
tests/test_dictionary.bash
tests/test_env.bash
tests/test_fifo.bash
tests/test_future.bash
tests/test_instance_var_defaults.bash
tests/test_instance_var_inheritance.bash
tests/test_integration.bash
tests/test_namespaces.bash
tests/test_object_methods.bash
tests/test_object_refs.bash
tests/test_persistable.bash
tests/test_pragma_direct.bash
tests/test_process.bash
tests/test_profiling.bash
tests/test_schema_migration.bash
tests/test_source_embedding.bash
tests/test_sqlite_json.bash
tests/test_traits.bash
tests/test_tuplespace_object.bash
```

---

## Part 6: Documentation Updates

### 6.1 LANGUAGE.md

**Remove:**
- Section on `pragma: procyonOnly` (lines 194-213)
- Section on `pragma: procyonNative` (lines 172-191)
- Section on `pragma: primitiveClass` (lines 243-323)
- Cross-compiler reference table (lines 325-401)
- All mentions of "native", "Procyon", "dylib", "plugin"

**Update:**
- Pragma section to only cover `direct`, `bashOnly`, `primitive`
- Remove "Go/Procyon runtime" mentions in known issues

### 6.2 CLAUDE.md

**Remove:**
- Compilation pipeline diagram showing Procyon
- Procyon component from Key Components table
- References to `~/dev/go/procyon`
- Native mode/plugin mode descriptions
- `make plugins`, `make daemon` from build commands
- Primitive class invariants (no longer special)

**Update:**
- Compilation pipeline: `.trash` → jq-compiler → bash functions
- Simplify architecture section
- Update build commands

### 6.3 Other Docs

Review and update:
- docs/COMPILER_CAPABILITIES.md
- docs/procyon-primitives-proposal.md (DELETE)
- docs/SHARED_RUNTIME_PLAN.md (DELETE if Procyon-specific)
- docs/trashtalk-go-codegen-design.md (DELETE)
- COMPILER_EVOLUTION_PLAN.md (UPDATE or DELETE)

---

## Part 7: Implementation Checklist

### Phase 1: Cleanup (Safe deletions)
- [ ] Delete trash/Procyon.trash
- [ ] Delete trash/Yutani/ directory (13 files)
- [ ] Delete trash/Transcript.trash (Yutani-dependent)
- [ ] Delete Yutani tests (3 files)
- [ ] Delete Procyon/native tests (4 files)
- [ ] Delete lib/compile-plugin.sh
- [ ] Delete docs/procyon-primitives-proposal.md
- [ ] Delete docs/trashtalk-go-codegen-design.md

### Phase 2: Simplify Build System
- [ ] Simplify Makefile (remove plugins, daemon, single plugin mode)
- [ ] Simplify lib/compile-bash.sh (remove Procyon fallback)
- [ ] Remove lib/procyon/ directory
- [ ] Remove lib/tt binary

### Phase 3: Remove Primitive Class Pragma
- [ ] Remove `pragma: primitiveClass` from all 17 primitive classes
- [ ] Update codegen.jq to not emit primitiveClass markers
- [ ] Update lib/trash.bash to not check primitiveClass markers

### Phase 4: Clean Runtime
- [ ] Remove native daemon code from lib/trash.bash
- [ ] Remove procyonOnly/procyonNative marker checks
- [ ] Remove plugin loading logic
- [ ] Clean up any tt daemon references

### Phase 5: Update Documentation
- [ ] Update LANGUAGE.md (remove Procyon pragmas, primitiveClass)
- [ ] Update CLAUDE.md (simplify architecture)
- [ ] Review and clean other docs
- [ ] Update this file to mark as complete

### Phase 6: Verify
- [ ] Run `make clean && make bash` - all classes compile
- [ ] Run `make test` - all relevant tests pass
- [ ] Manual smoke test: create instance, call methods, persistence
- [ ] Verify no "legacy" annotations in build output

---

## Summary Statistics

**Before v1.0:**
- 48 .trash files
- 2 compilers (jq-compiler + Procyon)
- 17 primitive classes with special pragma
- 13 Yutani GUI classes
- 30 tests

**After v1.0:**
- 34 .trash files (-14)
- 1 compiler (jq-compiler only)
- 0 special class pragmas
- 0 GUI framework
- 23 tests (-7)

**Lines of code removed (estimated):**
- Procyon Go code: ~10,000 lines (external repo, not our problem)
- Yutani classes: ~3,000 lines
- Native/plugin infrastructure: ~500 lines
- Test code: ~500 lines
- Documentation: ~300 lines

**Result:** A simpler, bash-only Smalltalk-inspired DSL with all the expressive power of the original design, minus the complexity of native compilation.
