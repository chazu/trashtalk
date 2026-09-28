# Protocol fortification plan

**Status:** Proposed plan. This document does not change protocol behavior.

## Outcome

Trashtalk protocols will become explicit, build-validated capability contracts.
They will preserve direct Bash method dispatch. A successful protocol check must
add no work to an ordinary compiled message send.

The plan retains structural conformance, where a class satisfies a protocol by
providing its required selectors. It adds an optional nominal declaration so a
class can state which contracts it promises to implement.

```smalltalk
ServiceClient subclass: Protocol
  requires: endpoint
  requires: readiness
  requires: request: options:

Tools::LiteLLM subclass: Object
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

## Design decisions

### Keep protocols distinct from traits and categories

A trait supplies reusable method bodies. A protocol specifies selectors that a
class must supply. A method category is a browser/documentation grouping.
These concepts may use related names, but they must not share runtime behavior.

Traits can help a class satisfy a protocol. The build resolves trait methods
and inherited methods before conformance validation. A class-defined method
wins where existing trait conflict rules permit it.

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

1. Add repeatable `implements: ProtocolName` declarations to class bodies.
2. Reject `implements:` in traits and protocol definitions in the first version.
3. Parse declarations into `implementedProtocols`, preserving source order.
4. Reject duplicate names and reject references that do not resolve to a class
   whose direct or inherited parent is `Protocol`.
5. Continue parsing `requires:` exactly as it does today. In a `Protocol`
   subclass, it means a required selector. In ordinary classes, retain current
   dependency behavior until a separate cleanup design changes it.

The compiler must preserve fully qualified protocol names. Relative names
follow the existing package-resolution rules.

### Compiled metadata

Emit explicit metadata only for classes that declare protocols:

```bash
__Tools__LiteLLM__protocols="ServiceClient"
```

Keep protocol requirements in `__ServiceClient__requires`. Do not add protocol
branches, metadata reads, or conformance calls to generated message sends.

The generated metadata serves inspection tools, source navigation, and the
one-time runtime cache. It is not required by ordinary method dispatch.

### API summary for validation

During the build, compute each class's effective public selector set from:

1. methods declared on the class;
2. included traits after trait resolution;
3. inherited selectors from its resolved parent;
4. aliases that the current runtime dispatch exposes.

Compute this set once in the compiler/build process. Do not recreate it by
calling Bash `declare -f` while compiling. A protocol satisfies a class when
every required selector is present in this effective selector set.

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

Validation must run in `make bash`, incremental rebuilds, and the compiler's
single-class path when all referenced artifacts are available. If a standalone
compile cannot resolve a protocol, return a clear unresolved-dependency error.
It must never silently skip validation.

### Incremental build cache

The conformance result depends on these inputs:

- implementing class source hash;
- resolved parent API hash;
- included trait API hashes and conflict-resolution result;
- each declared protocol's requirement hash;
- compiler version/hash.

Store these in build-cache metadata. If none changes, reuse the validation
receipt. If any changes, rebuild and revalidate the affected class and its
subclasses. A protocol requirement change must invalidate every implementation
of that protocol. A parent or trait API change must invalidate descendants that
promise protocols.

This is build-time work only. It cannot change request latency.

### One-time dynamic validation

Keep a separate cache key of `(concrete class, protocol API hash)` for
`_conforms_to`. Cache both success and failure. Include the protocol API hash
so a reload in a long-lived development process cannot reuse a stale result.

Do not cache or source arbitrary class names from untrusted input. Dynamic
loading remains an explicit caller responsibility. The cache stores a
verification result only and does not grant authority to invoke the class.

## Performance budget and measurement

The key invariant is: after compilation, an ordinary message send has the same
functions, process behavior, and lookup path whether or not its class declares
`implements:`.

Measure the following before and after implementation on the supported Bash
versions:

| Measurement | Acceptance criterion |
| --- | --- |
| Direct compiled method send on an implementing class | No statistically meaningful regression beyond measurement noise. |
| Hot loop of direct sends | No protocol-dependent command spawn, `source`, `declare -f`, or metadata scan. |
| First dynamic conformance check | Bounded by required selectors and inheritance depth. |
| Repeated dynamic conformance check | Cached lookup only. |
| Incremental rebuild with unchanged API hashes | Reuses conformance receipt. |
| Rebuild after protocol/trait/parent API change | Revalidates each affected implementation and reports all failures. |

Build a deterministic microbenchmark that compares otherwise identical classes,
one declaring `implements:` and one not declaring it. Use shell builtins and
fixed iteration counts. Record wall time and command/process counts where
available. The benchmark must fail if generated code for the implementing class
contains a conformance call in the method send path.

Do not set a percentage threshold until a baseline is recorded on supported
machines. The generated-code equivalence check is the hard guard. Measurements
set the practical regression threshold after the baseline exists.

## Implementation phases

1. **Characterize current behavior.** Add focused tests for `requires:`,
   inheritance, trait-provided selectors, aliases, namespaces, protocol reload,
   and current `_conforms_to` behavior. Record a direct-send baseline.
2. **Add metadata and parsing.** Implement `implements:` parsing, qualified name
   resolution, duplicate rejection, compiler metadata, and source-navigation
   output. No validation yet. Verify that generated message dispatch is
   byte-for-byte unchanged for equivalent methods.
3. **Add static API summaries.** Build effective selector sets from the AST and
   resolved dependency metadata. Test inheritance, traits, aliases, and
   unresolved dependency diagnostics.
4. **Validate declared conformance.** Integrate validation into full and
   incremental build paths. Add precise multi-selector diagnostics and cache
   invalidation tests.
5. **Fortify dynamic checks.** Add hash-keyed process-local caching to
   `_conforms_to` and `Protocol isSatisfiedBy:`. Keep it opt-in. Test cache
   invalidation after class/protocol reload.
6. **Benchmark and document.** Add the benchmark and a generated-code guard.
   Update `LANGUAGE.md`, `docs/README.md`, and protocol examples. Define the
   `ServiceClient` protocol only after the mechanism passes these gates.

Each phase must land as an independently passing commit. Do not migrate current
classes to `implements:` until Phase 4 is complete and the behavior is
measured.

## Test matrix

- A class satisfies a protocol with a directly declared method.
- A class satisfies a protocol through a parent method.
- A class satisfies a protocol through an included trait.
- A class satisfies a protocol through an existing alias.
- A class fails with every missing selector listed once.
- A missing, non-protocol, duplicate, and incorrectly qualified declaration
  fails at compile time.
- A protocol requirement edit invalidates every declared implementation.
- A trait or parent API edit invalidates affected descendants.
- An unchanged build reuses the validation receipt.
- A dynamic check caches success and failure but invalidates on reload.
- Direct sends have no protocol-check commands in generated Bash.

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
