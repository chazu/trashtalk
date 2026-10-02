# Trashtalk compiler capabilities

**Status: current reference.** Trashtalk has one compiler and one execution
model: `.trash` → Bash tokenizer → jq parser → jq code generator → Bash runtime.
The retired Procyon backends are historical, not compatibility targets.

The parser owns class/trait structure. The expression parser inside
`lib/jq-compiler/codegen.jq` handles DSL method bodies; raw bodies are reconstructed
as Bash. `driver.bash` is the production entry point. Generated files live only
in `trash/.compiled/`; edit source and rebuild with `make bash`.

| Supported surface | Regression coverage in `lib/jq-compiler/tests/` |
| --- | --- |
| Classes, inheritance, inherited fields, class variables, accessors | `test_codegen`, `test_inherited_ivars`, `test_class_instance_vars` |
| Unary/keyword messages, distinct unary and keyword selectors, negative arguments | `test_known_issues`, `test_expr_codegen` |
| Locals, assignment, arithmetic precedence, return, string concatenation, triple-quoted text | `test_expr_parser`, `test_expr_runtime`, `test_string_concat`, `test_method_semantics`, `test_triplestrings` |
| Conditions, predicates, loops, boolean operators, exceptions, nil checks, string comparison | `test_control_flow`, `test_predicates`, `test_boolean_ops`, `test_exceptions`, `test_negation`, `test_nil_handling`, `test_string_compare`, `test_standalone_predicates`, `test_expression_behavior`, `test_to_do` |
| Blocks, captures, block parameters, supported inline early returns | `test_blocks`, `test_block_params`, `test_block_early_return` |
| Arrays, dictionaries, typed JSON construction, reads and traversal | `test_arrays`, `test_dicts`, `test_json_values`, `test_json_reads`, `test_json_primitives`, `test_json_primitives_runtime` |
| Packages, qualified sends (including raw methods), `super` | `test_namespaces`, `test_known_issues`, `test_super` |
| Cascades, aliases, protocols, advice, method categories | `test_cascades`, `test_aliases`, `test_protocols`, `test_protocol_dispatch`, `test_protocol_fortification`, `test_advice`, `test_method_categories` |
| Raw Bash boundaries and sequence ranges, `pragma: direct`, `pragma: primitive` | `test_rawmethod_assignments`, `test_raw_ranges`, `test_pragmas` |
| String intrinsics on implicit receivers, unknown-message diagnostics | `test_string_intrinsics` |
| `signal:`, `self error:`, `ifFailed:`, re-raise, error recovery from captured sends | `test_failure_forms` |
| Statement value discipline, `pragma: stream`, `linesDo:`, `caseOf:`, literal `Env get:` | `test_statement_values` |
| Declared primitives (`primitive:` / `classPrimitive:` ... `calls:`) | `test_primitives` |
| Build receipts, dependency planning, symbol cache, pruning of unowned artifacts and old cache generations, `TRASHTALK_VALUE_SEND` | `test_build_cache`, `test_symbol_cache`, `test_build_moves`, `test_cache_prune`, `test_value_send` |
| Tokenizer (including locale and multibyte input), parser, compile-and-run journeys | `test_tokenizer`, `test_tokenizer_locale`, `test_parser`, `test_integration` |
| Comparisons as values, keyword-argument boundaries, literal triple-quoted text, qualified error signals, rejection of code outside methods | `test_value_forms` |

Each test name above has a `.bash` suffix. Runtime tests additionally cover
qualified traits, persistence, transactions, and public domain APIs.

## Boundaries

- `method:` and `classMethod:` are the normal implementation surface. Raw methods
  remain necessary for shell/process, filesystem, serializer, and SQLite primitives.
  String handling, failure, newline iteration, and literal dispatch are DSL forms;
  a body that only forwards to a Bash function is a `primitive:` declaration.
- A method's stdout is its value: non-tail statement sends discard their output
  unless the method declares `pragma: stream`. Blocks passed to ordinary methods
  keep every statement's output.
- A first-class block passed to an arbitrary method cannot return from its caller.
  Compiler-recognized inline control flow has separate early-return handling.
- Bash arithmetic is integer arithmetic. JSON numeric values do not add a floating
  point arithmetic runtime.
- A failed send is not an automatic method return. Guard it with `ifFailed:` or
  raise with `@ SomeError signal:` at effect boundaries. Store transactions
  additionally poison the transaction after any failed send.
- `pragma: direct` bypasses dispatcher capture for shell-state mutation;
  `primitive` preserves a Bash body. Retired backend pragmas are rejected.
- `TRASHTALK_VALUE_SEND=1` is an opt-in capture optimization, disabled by default.
  See [result passing](result-passing-design.md) before enabling it.

See [LANGUAGE.md](../LANGUAGE.md) for syntax, [the compiler guide](../lib/jq-compiler/README.md)
for stage commands, and [the documentation index](README.md) for current APIs.
Run `make verify` after compiler changes; a successful parse alone does not prove
that a program behaves correctly.
