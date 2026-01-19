# Trashtalk Closure Plan

## Decision Summary

Trashtalk will be finalized as a **bash-only** project. The hybrid native/bash execution model is being abandoned in favor of simplicity and completeness.

## Key Decisions

### 1. jq-compiler becomes the canonical compiler

The jq-compiler (`lib/jq-compiler/`) will be the sole compiler for Trashtalk. It produces bash output only. No native code generation, no Go dependencies.

**Rationale**: jq-compiler is self-contained (jq + bash), has no external dependencies beyond standard Unix tools, and aligns with Trashtalk's identity as a bash DSL.

### 2. Procyon's bash backend serves as reference

During the transition, Procyon's `--mode=bash` output serves as the reference implementation. Any semantic differences between jq-compiler and Procyon should be resolved by fixing jq-compiler to match Procyon's behavior.

**Rationale**: Procyon's bash backend is more recently maintained and has addressed various edge cases. It represents the "correct" semantics.

### 3. Native execution is dropped entirely

The following components become obsolete:
- `--mode=plugin` in Procyon
- The `tt` daemon
- `.dylib` plugin generation
- Native/bash fallback logic in `lib/trash.bash`
- SQLite instance synchronization for cross-runtime state

**Rationale**: The hybrid model created semantic impedance mismatches, synchronization bugs, and maintenance burden. The complexity was not justified.

### 4. Procyon evolves separately

Procyon (or a fork) will become a separate project: a late-bound Smalltalk dialect with Go interop and goroutine-based concurrency. This is a different vision than Trashtalk's bash DSL identity.

## End State

When complete, Trashtalk will be:

- **Self-contained**: jq + bash toolchain only
- **Bash-only execution**: No native code, no daemon
- **Stable**: No ongoing architectural changes
- **Documented**: Clear semantics matching Procyon's bash backend

## Work Items

### Phase 1: Document Reference Semantics

- [ ] Document Procyon's bash output semantics as the canonical spec
- [ ] Cover: method dispatch, instance persistence, variable scoping, selector normalization, primitive class handling

### Phase 2: Audit jq-compiler

- [ ] Compare jq-compiler output against Procyon bash output for all core classes
- [ ] Identify semantic gaps and edge case differences
- [ ] Create issue for each gap

### Phase 3: Fix jq-compiler

- [ ] Address each identified gap
- [ ] Ensure test suite passes with jq-compiler
- [ ] Verify Yutani IDE launches (stretch goal, may require other fixes)

### Phase 4: Remove Native Infrastructure

- [ ] Remove `--mode=plugin` codepaths from build
- [ ] Remove `tt` daemon
- [ ] Simplify `lib/trash.bash` to remove native dispatch/fallback logic
- [ ] Remove SQLite instance sync code (if only needed for native)
- [ ] Update Makefile to remove native targets

### Phase 5: Finalize

- [ ] Make jq-compiler the default in Makefile
- [ ] Update documentation
- [ ] Tag a "1.0" release

## Out of Scope

- Performance optimization
- New language features
- Native execution
- IDE/tooling beyond current state

## Related

- `why_shit_sucks.md` - Diagnostic of native execution failures (now moot)
- Procyon repo at `~/dev/go/procyon` - Reference for bash semantics, future evolution
