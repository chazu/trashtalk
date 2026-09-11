# Trashtalk jq compiler

The canonical Bash-only compiler. Requires Bash 4.4+, jq 1.6+, and `shasum`.
Runtime requirements and installation are in the [project README](../../README.md).

## Pipeline

```text
.trash → tokenizer.bash → JSON tokens → parser.jq → class/trait AST
       → codegen.jq (including DSL expression parsing) → Bash
```

`parser.jq` contains its parsing combinators. There is no alternate expression,
IR, or grammar-module pipeline. `symbols.jq` and `senders.jq` query ASTs for
browsing; they do not generate executable code.

From the repository root:

```bash
lib/jq-compiler/driver.bash tokenize trash/Counter.trash | jq .
lib/jq-compiler/driver.bash parse trash/Counter.trash | jq .
lib/jq-compiler/driver.bash compile trash/Counter.trash > /tmp/Counter-preview.bash
make single CLASS=Counter
make bash
make verify
```

`compile` writes Bash to stdout and diagnostics to stderr. Build targets install
generated classes into `trash/.compiled/` and traits into its `traits/` directory.
The driver caches parsed ASTs there; fingerprints include the production
compiler and build helpers. Build receipts also verify source, dependency, and
artifact hashes. See [performance](../../docs/performance.md).

## Generated interface

- `Counter subclass: Object` emits metadata and methods for `Counter`.
- `method: value [...]` emits `__Counter__value`.
- `method: value: amount [...]` emits `__Counter__value_` and binds `amount`.
- `@ receiver value: amount` retains the keyword selector at the public boundary.
- `package: MyApp` uses `MyApp::Counter` publicly and `__MyApp__Counter` in Bash.
- DSL locals and fields have distinct assignment paths. `rawMethod:` reconstructs
  a Bash body, including qualified class references; it does not infer fields.
- `direct` marks a method for caller-shell dispatch. `primitive` preserves its
  body as Bash. `stream` keeps every statement's output; otherwise a non-tail
  send in a DSL body is compiled with its stdout discarded. Removed backend
  pragmas fail compilation with a migration message.
- `classPrimitive: sel: a calls: fn` emits `__Counter__class__sel_() { fn "$1"; }`.
- String intrinsics (`s startsWith: p`, `s withoutPrefix: p`, `s size`, ...) lower
  to parameter expansion; `@ SomeError signal: 'm'` lowers to `_throw`; `ifFailed:`,
  `linesDo:`, and `caseOf:` inline to `if !`, an array loop, and `case`. An
  unrecognized message on an implicit receiver is a compile error.

See [capabilities](../../docs/COMPILER_CAPABILITIES.md) and
[language reference](../../LANGUAGE.md) for supported syntax and limitations.

## Tests

`make test-compiler` runs the production-compiler tests in disposable checkouts.
Standalone invocations use the same isolation:

```bash
bash lib/jq-compiler/tests/test_known_issues.bash
bash lib/jq-compiler/tests/test_expr_codegen.bash
```

Expression parser tests load definitions from the production generator; codegen
journeys compile real `.trash` source and exercise public message sends. Test
fixtures belong in a per-run scratch directory. Do not embed a second compiler
in tests or treat printed failure text as a failing process status.
