# Protocol fortification plan

**Status:** Proposed plan, revised after adversarial review. This document does
not change protocol behavior.

## Outcome

Trashtalk protocols will become explicit, build-validated capability contracts.
They will preserve direct Bash method dispatch. A successful protocol check must
add no work to an ordinary compiled message send.

The plan retains structural conformance, where a class satisfies a protocol by
providing its required selectors. It adds an optional nominal declaration so a
class can state which contracts it promises to implement.

```smalltalk
ServiceClient subclass: Protocol
  # Unary requirements require the Phase 1 grammar extension below.
  requires: endpoint
  requires: readiness
  requires: request: options:

package: Tools

LiteLLM subclass: Object
  implements: ServiceClient
```

`implements:` does not alter dispatch. It asks the build to verify the promise.
A caller may still use a structurally conforming class without such a
annotation when that is useful.

## Why change the current implementation

The current system has useful pieces:

- A `Protocol` subclass stores `requires:` selectors in `__Class__requires`.
- `_conforms_to` and `Protocol isSatisfiedBy:` check selectors structurally.
- `_class_has_method` searches compiled functions and inheritance.

The check is appropriate for a diagnostic or a one-time admission boundary. It
is unsuitable for a request path because it uses Bash reflection and can walk
an inheritance chain for each required selector. It can also source a protocol
artifact on demand. Repeating that work per request would make protocol use a
performance regression.

The language documentation currently calls `requires:` documentation and
validation, but the normal build does not reject a class that promises an
unsatisfied protocol. This plan makes declared promises build errors.

The example describes target syntax, not accepted current syntax. Today
`requires:` accepts a string dependency or a keyword selector, but not unary
selectors such as `endpoint`. Package qualification uses `package: Tools`, not
a qualified class declaration. Phase 1 adds unary requirements and
`implements:` before this example becomes valid source.

## Design decisions

### Keep protocols distinct from traits and categories

A trait supplies reusable method bodies. A protocol specifies selectors that a
class must supply. A method category is a browser/documentation grouping.
These concepts may use related names, but they must not share runtime behavior.

Traits can help a class satisfy a protocol, but v1 must mirror actual runtime
lookup rather than assume compile-time trait merging. Its effective dispatch
surface is: class-declared methods, then direct traits in declaration order,
then inherited class methods. Do not count traits of ancestors unless runtime
dispatch changes and tests establish that behavior.

A class-defined method overrides all direct traits. Two direct traits defining
the same required selector are a build error for a declared protocol unless a
class-defined method disambiguates it. This is a protocol-validation rule, not
a claim that the present trait runtime has conflict-resolution metadata.

Static validation and dynamic conformance must use one specified dispatch-
surface resolver. Phase 5 will replace `_class_has_method`, or make it delegate
to the shared resolver. It must inspect class methods, direct traits in order,
the class override rule, and then inherited class methods. Independent static
and dynamic resolver algorithms are forbidden because they can disagree about
trait-provided requirements.

### Use explicit contracts, but preserve duck typing

`implements:` is optional. It supplies discoverability, a stable dependency
edge, and a build-time assertion. Existing code can continue to send messages
to structural receivers without declaring a protocol.

Do not add type annotations to every variable or infer protocols at a message
send. Bash has no cheap type system, and this would add compiler complexity
without protecting dynamic boundaries reliably.

### Make runtime validation opt-in and boundary-only

Keep `Protocol isSatisfiedBy:` and `_conforms_to` as diagnostic operations.
Improve them with a process-local cache for dynamic registration code, but do
not call them from generated method dispatch.

A dynamic extension point can validate once when it accepts an implementation:

```smalltalk
@ registry register: service requiring: ServiceClient
```

The registry stores only validated implementations. Each later request uses the
stored service directly. It does not check the protocol again.

## Source and metadata changes

### Grammar and AST

1. Parse every `requires:` form into a generic, source-located declaration.
   Extend selector-form `requires:` to accept an identifier as a unary selector.
   Preserve its canonical selector as `endpoint`. Retain string dependencies
   and keyword selectors without changing their current spelling.
2. During semantic validation, permit selector-form `requires:` only on a
   protocol. Permit string dependencies only where current class dependency
   behavior permits them. Reject selector-form requirements on ordinary classes
   rather than silently emitting protocol metadata for them.
3. A v1 protocol must directly subclass the base `Protocol`. Reject a subclass
   of a user-defined protocol and reject any ordinary class that subclasses a
   user-defined protocol. Protocol inheritance and composition remain deferred.
4. Protocol requirements are public selectors only. Reject a required selector
   beginning `_`. Private methods and aliases never enter a protocol surface.
5. Add repeatable `implements: ProtocolName` declarations to class bodies. Add
   it to parser synchronization and class-body parsing. Parse a class reference
   through the existing package-resolution path and retain source locations for
   diagnostics.
6. Parse declarations into `implementedProtocols`, preserving source order and
   rejecting duplicates. Resolve each reference before deciding whether it names
   a `Protocol` descendant. Reject declarations on traits and protocols after
   this resolution step.

The compiler must preserve canonical fully qualified protocol identities.
Relative names follow the existing package-resolution rules.

### Compiled metadata

Emit explicit metadata only for classes that declare protocols:

```bash
__Tools__LiteLLM__protocols="ServiceClient"
```

Keep protocol requirements in `__ServiceClient__requires`. Do not add protocol
branches, metadata reads, or conformance calls to generated message sends.

The generated metadata serves inspection tools, source navigation, and the
one-time runtime cache. It is not required by ordinary method dispatch.

### Protocol manifest and resolver

Before conformance validation, build a versioned compiled-artifact manifest.
Each entry records canonical identity, source path, compiled artifact path,
kind (`class`, `trait`, or `protocol`), package/import scope, API hash, and the
ordinary build receipt identity. Update the manifest atomically with that
receipt.

The resolver consumes this manifest for both static and direct dynamic
validation. V1 resolves a local package name first, then requires an explicitly
qualified global name. It has no import search because Trashtalk has no import
declaration. It rejects ambiguity, shadowing that changes a prior identity,
stale receipt references, and an unregistered artifact. It never constructs a
source path from an untrusted class or protocol name.

The build graph uses canonical resolved identities from this resolver, not raw
spelling. Dynamic validation uses the same manifest before it loads a compiled
protocol artifact. This replaces the current direct path construction in
`_conforms_to`.

### API summary for validation

Use canonical DSL selector spelling, such as `endpoint` and `request:options:`.
Do not compare requirements with emitted Bash symbols such as `request_options_`.

During the build, compute an explicit public effective dispatch-surface summary
from:

1. methods declared on the class;
2. direct included traits in runtime declaration order;
3. inherited selectors from its resolved parent;
4. aliases that the current runtime dispatch exposes;
5. generated instance-variable getter and setter selectors;
6. public instance methods only. V1 excludes class methods because an instance
   capability contract must not certify a selector that callers can send only to
   the class. A later design can add explicit class-side protocol requirements.

V1 aliases remain unary only because that is the existing alias grammar. Extend
alias syntax separately before allowing keyword aliases in protocol summaries.
Exclude selectors beginning `_`, including private aliases, from every static
and dynamic protocol surface.

Compute this surface once in the compiler/build process. Do not recreate it by
calling Bash `declare -f` while compiling. A protocol satisfies a class when
every required selector is present in this surface.

The compiler should use selector strings only. It must not attempt to prove
argument types, return shapes, streaming behavior, or side effects. Those are
documented semantic contracts and acceptance tests, not properties a Bash
compiler can verify.

## Validation lifecycle

### Build-time validation

After the build plan resolves parent and trait dependencies, validate every
`implements:` declaration. Report all missing selectors in one error, with the
class, protocol, and resolution source where possible.

Example diagnostic:

```text
ProtocolError: Tools::LiteLLM implements ServiceClient but lacks:
  readiness
  request:options:
```

Validation must run in `make bash` and incremental rebuilds after their normal
dependency graph resolves all summaries. Route hot reload through that graph
coordinator. A standalone compiler invocation must receive explicit validated
dependency summaries or return a clear unresolved-dependency error. It must
never silently skip validation.

### Incremental build cache

The conformance result depends on these inputs:

- implementing class source hash;
- resolved parent API hash;
- included trait API hashes and conflict-resolution result;
- each declared protocol's requirement hash;
- compiler version/hash.

Add resolved protocol identities to the ordinary build dependency graph. Add
their API-summary or output hashes to the normal dependency receipt. Store the
validation result atomically in that existing receipt as
`validation: {schema, implementing_surface_hash, protocol_hashes, result}`.
Do not create a separately invalidated conformance receipt.

If none of these inputs changes, reuse the normal receipt. If any changes,
rebuild and revalidate the affected class and its subclasses. A protocol
requirement change must invalidate every implementation of that protocol. A
parent or trait API change must invalidate descendants that promise protocols.

This is build-time work only. It cannot change request latency.

### One-time dynamic validation

Keep a separate cache key of `(resolved concrete dispatch-surface hash, resolved
protocol requirement hash, conformance-algorithm version)` for `_conforms_to`.
Cache both success and failure. This prevents reuse after a concrete class,
parent, trait, or protocol reload.

Use the cache only in a direct, non-capturing boundary API. Public `@` sends
often execute in a command-substitution subshell, so mutated shell variables do
not persist across those sends. If a caller uses `_conforms_to` in such a path,
cache hits are best-effort and can disappear. Do not claim a process-local cache
persists there.

Validate class and protocol identities through the compiled-artifact manifest
before sourcing them. Do not cache or source arbitrary input. Dynamic loading
remains an explicit caller responsibility. The cache stores a verification
result only and does not grant authority to invoke the class.

## Performance budget and measurement

The key invariant is: after compilation, an ordinary message send has the same
functions, process behavior, and lookup path whether or not its class declares
`implements:`.

Measure the following before and after implementation on the supported Bash
versions:

| Measurement | Acceptance criterion |
| --- | --- |
| Direct compiled method send on an implementing class | No statistically meaningful regression beyond measurement noise. |
| Hot loop of direct sends | No protocol-dependent command spawn, `source`, `declare -f`, metadata scan, or branch in `send`. |
| First dynamic conformance check | Bounded by required selectors and inheritance depth. |
| Repeated dynamic conformance check | Cached lookup only. |
| Incremental rebuild with unchanged API hashes | Reuses conformance receipt. |
| Rebuild after protocol/trait/parent API change | Revalidates each affected implementation and reports all failures. |

Build a deterministic microbenchmark that compares otherwise identical classes,
one declaring `implements:` and one not declaring it. Use shell builtins and
fixed iteration counts. Record cold first-source cost separately from hot-send
wall time and command/process counts where available.

For a class differing only by `implements:`, generated method function
definitions and every compiled send expression must be byte-identical. The
artifact may differ only in declarative metadata. The benchmark must fail if
generated method bodies contain `_conforms_to`, `isSatisfiedBy:`, `source`, or
reflection. Add a behavior test that instruments `send` and proves that an
implementing and nonimplementing class use identical lookup routes.

Do not set a percentage threshold until a baseline is recorded on supported
machines. The generated-code equivalence check is the hard guard. Measurements
set the practical regression threshold after the baseline exists.

## Implementation phases

1. **Characterize current behavior.** Add focused tests for `requires:`,
   inheritance, direct traits, trait order conflicts, class overrides,
   inherited-trait behavior, aliases, namespaces, protocol reload, and current
   `_conforms_to` behavior. Record a direct-send baseline.
2. **Add metadata and parsing.** Implement `implements:` parsing, qualified name
   resolution, duplicate rejection, compiler metadata, and source-navigation
   output. No validation yet. Verify that method function definitions and compiled
   send expressions are byte-identical for otherwise equivalent methods. Allow
   declarative protocol metadata to differ.
3. **Add static API summaries.** Build dispatch-surface summaries from the AST
   and resolved dependency metadata. Test canonical selector spelling, class
   methods, accessors, inheritance, direct traits, aliases, and unresolved
   dependency diagnostics.
4. **Validate declared conformance.** Integrate validation into the normal full
   and incremental dependency graph and its single receipt. Route hot reload
   through the graph coordinator. Add precise multi-selector diagnostics and
   cache invalidation tests.
5. **Fortify dynamic checks.** Make `_conforms_to` and `Protocol isSatisfiedBy:`
   use the shared public instance-side dispatch-surface resolver and artifact
   manifest. Add hash-keyed caching to a direct boundary API. Keep it opt-in.
   Test agreement with static validation for traits, invalidation after class,
   trait, parent, and protocol reload, plus best-effort behavior from
   command-substitution paths.
6. **Benchmark and document.** Add the benchmark and a generated-code guard.
   Update `LANGUAGE.md`, `docs/README.md`, and protocol examples. Define the
   `ServiceClient` protocol only after the mechanism passes these gates.

Each phase must land as an independently passing commit. Do not migrate current
classes to `implements:` until Phase 4 is complete and the behavior is
measured.

## Test matrix

- A class satisfies a protocol with a directly declared method.
- A class satisfies a protocol through a parent method.
- A class satisfies a protocol through an included direct trait.
- Trait ordering conflict, class override, and inherited-trait behavior match
  the documented v1 dispatch surface.
- Static validation, `_conforms_to`, and `Protocol isSatisfiedBy:` agree for
  direct traits, class overrides, trait ambiguity, and inherited traits.
- A class satisfies a protocol through an existing alias.
- A private method, alias, or required selector cannot satisfy or declare a
  public protocol requirement.
- A class-side-only method cannot satisfy an instance protocol requirement.
- A class fails with every missing selector listed once.
- A missing, non-protocol, duplicate, and incorrectly qualified declaration
  fails at compile time.
- A user-defined protocol cannot be subclassed in v1.
- Same-package, explicitly qualified, ambiguous, stale-manifest, and
  unregistered protocol identities resolve or fail according to the manifest
  rules.
- Selector-form `requires:` on an ordinary class fails without emitting protocol
  metadata.
- A protocol requirement edit invalidates every declared implementation through
  the ordinary dependency DAG and receipt.
- A trait or parent API edit invalidates affected descendants.
- An unchanged build reuses the validation receipt.
- A dynamic check caches success and failure but invalidates on reload.
- Direct sends have no protocol-check commands in generated Bash and identical
  runtime `send` lookup routes for implementing and nonimplementing classes.

## Deferred decisions

- Protocol inheritance and protocol composition. Start with flat protocols.
  Add composition only after its API hash and diagnostic rules are designed.
- Semantic requirements such as streaming, idempotency, authentication, and
  result shape. Represent these in tests and documentation first.
- Automatic interface extraction from method categories. Categories are useful
  documentation, but they must not silently create coupling.
- Mandatory declarations for all public classes. Keep `implements:` optional
  until real contracts demonstrate its value.

## First use: ServiceClient

After the mechanism is stable, define a small `ServiceClient` protocol for the
LiteLLM work. Keep it transport-level and avoid making it an LLM-specific
interface. Candidate selectors are endpoint identity, readiness, request
execution, cancellation, and streaming capability. The exact selector set must
follow the LiteLLM proof of concept.

`Tools::LiteLLM` can then explicitly implement `ServiceClient`. Existing CLI
`Tool` subclasses remain separate. A service can use common service traits or
shared primitives without falsely claiming that a remote endpoint is an
installable executable.
