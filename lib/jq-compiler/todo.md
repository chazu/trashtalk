# jq-compiler TODO

## Current Status

**Test Results: 183/183 passing (100%)**

All .trash files compile with valid bash syntax. The expected output for Process.trash was updated to match the new compiler's deterministic output.

---

## Architecture Assessment (Language Architect Review)

**Verdict: Current design is sound.**

The architecture uses **island parsing** (or dialect embedding) - a well-established pattern for DSLs with embedded host language code. The hybrid approach is appropriate:

- **Pass 1**: Full tokenization (Bash)
- **Pass 2a**: Parse class structure, method bodies remain as token arrays (jq)
- **Pass 2b**: Transform method body tokens during codegen (jq)

This is pragmatic given Bash's complex, context-sensitive grammar.

---

## Priority Tasks

### High Priority - COMPLETED

1. **~~Add `bash -n` syntax validation~~** - DONE
   - Added `validates_bash_syntax()` helper to test_integration.bash
   - Tests all .trash files for valid bash syntax
   - Added `--check` / `-c` flag to driver.bash compile command
   - Found and fixed two bugs during implementation:
     - Character class spacing: `[Yy]$` was becoming `[Yy ]$`
     - Process substitution: `< <(...)` was becoming `<<(...)`

2. **~~Collect errors in parser state~~** - DONE
   - Added `.errors` array to parseClassBody state
   - Records parse_error and unknown_token entries with context
   - Errors include token info (type, value, line, col)
   - Warnings displayed by driver.bash during parse
   - Parse continues after errors (error recovery)

### Medium Priority

3. **~~Recursive subshell transformation~~** - DONE
   - Implemented `transformSubshellContents` function in codegen.jq
   - Handles: self → $_RECEIVER, single-keyword methods, 2-keyword methods
   - Works with nested subshells: `$(@ self at: 1 put: "$(@ self get: key from: dict)")`
   - Added 7 new tests for nested DSL in subshells

4. **Consolidate token reconstruction** - Merge `tokensToCode` and `tokensToRawCode`
   - Create single parameterized function: `tokensToString($options)`
   - Options: `preserveSpacing`, `transformDSL`

5. **Add AST-level unit tests** - Not just end-to-end baseline comparisons
   - Test individual parser rules (parseMethodSig, parseInstanceVars, etc.)
   - Verify AST structure for keyword methods, traits, inheritance

6. **Implement synchronization points** for error recovery
   - On parse error, skip to next `method:`, `classMethod:`, `instanceVars:`, etc.
   - Prevents single error from cascading

### Low Priority

7. **Performance optimization** - Only if profiling shows problems
   - Bash tokenizer is O(n) per character access in some versions
   - Acceptable for files <2000 lines
   - Consider awk/sed for hot paths if needed

8. **Modularization** - Split codegen only if it grows beyond ~800 lines
   - Potential split: `header.jq`, `transforms.jq`, `method.jq`, `main.jq`
   - jq's `include` requires files in library path

9. **Add source locations to AST nodes** - For better error messages
   - Include `location: {line, col}` in each AST node

---

## Fixed Bugs

### Process Substitution - FIXED
The pattern `< <(...)` was incorrectly collapsed to `<<(...)`. Fixed by updating the gsub in codegen.jq to preserve space between `<` characters:
```jq
gsub("< (?<c>[^<])"; "<\(.c)")  # Only remove space when NOT followed by <
```

### Character Class Spacing - FIXED
Patterns like `[Yy]$` were becoming `[Yy ]$` with extra space. Fixed by expanding the gsub to handle letters, not just digits:
```jq
gsub("(?<a>[a-zA-Z0-9]) \\](?<b>[^\\]])"; "\(.a)]\(.b)")
```

---

## Completed Work

### Tokenizer Changes (`tokenizer.bash`)

1. **COMMENT tokens** - Comments tokenized as `COMMENT` type (preserved in raw methods)
2. **PATH tokens** - Absolute paths like `/dev/null` tokenized as `PATH` type
3. **ARITH_CMD tokens** - Standalone `((expr))` distinct from `$((expr))`
4. **Nested quote handling** - Subshell depth tracking in DSTRING
5. **Special variables** - `$!`, `$?`, `$@`, `$*`, `$#`, `$-` as VARIABLE tokens

### Parser Changes (`parser.jq`)

1. **skipNewlines** - Skips NEWLINE and COMMENT tokens at class level

### Codegen Changes (`codegen.jq`)

1. **4-space base indent** for raw methods (matches old compiler)
2. **Smart indentation** - Tracks nesting, continuation, heredocs
3. **Comment handling** - COMMENT token output in tokensToRawCode
4. **ARITH_CMD handling** - Outputs value as-is
5. **Assignment spacing** - Handles `VAR=value` vs `VAR= cmd` patterns

---

## Architecture Notes

### Token Flow
```
Source -> Tokenizer (bash) -> JSON tokens -> Parser (jq) -> AST -> Codegen (jq) -> Bash
```

### Key Files
- `tokenizer.bash` - Converts source to JSON token array (~35 token types)
- `parser.jq` - PEG-style parser, produces AST
- `codegen.jq` - AST to bash code generation
- `driver.bash` - CLI interface (tokenize, parse, compile commands)

### Raw vs Normal Methods
- **Normal methods**: Body parsed and transformed (locals, return, self, message sends)
- **Raw methods**: Minimal transformation, preserves bash code (heredocs, traps, etc.)
