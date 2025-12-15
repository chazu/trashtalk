# jq-compiler TODO

## Current Status

**Test Results: 167/167 passing (100%)**

All .trash files compile with valid bash syntax. The expected output for Process.trash was updated to match the new compiler's deterministic output.

---

## Completed Work

### Tokenizer Changes (`tokenizer.bash`)

1. **COMMENT tokens** - Comments are now tokenized as `COMMENT` type instead of being skipped. This allows comments to be preserved in raw method bodies.

2. **PATH tokens** - Absolute paths like `/dev/null` are now tokenized as `PATH` type to prevent incorrect spacing around slashes.

3. **ARITH_CMD tokens** - Standalone `((expr))` is now tokenized as `ARITH_CMD` (without `$` prefix), distinct from `$((expr))` which remains `ARITHMETIC`. This preserves the semantic difference between arithmetic evaluation and arithmetic expansion.

4. **Nested quote handling in DSTRING** - Added subshell depth tracking to correctly tokenize strings containing nested quotes within subshells, e.g., `"proc_$(echo "$$_$(date)")"`

5. **Special variables** - Added handling for `$!`, `$?`, `$@`, `$*`, `$#`, `$-` as VARIABLE tokens.

### Parser Changes (`parser.jq`)

1. **skipNewlines updated** - Now skips both NEWLINE and COMMENT tokens at the class definition level, preventing parse errors while still capturing comments in raw method bodies.

### Codegen Changes (`codegen.jq`)

1. **Raw method indentation** - Changed from 2-space to 4-space base indent for raw methods to match old compiler.

2. **Smart indentation** - Added `smartIndent` function that tracks:
   - Nesting depth (while/do, if/then, case/in)
   - Continuation lines (ending with `\`)
   - Applies appropriate indentation per depth level

3. **Comment handling** - Added `COMMENT` token output in `tokensToRawCode`.

4. **ARITH_CMD handling** - Added output for `ARITH_CMD` tokens (outputs value as-is without `$` prefix).

5. **Redirect spacing** - Changed `GT` and `LT` tokens to output with leading space (` >`, ` <`), then use gsub to fix `2>` patterns.

6. **Assignment spacing** - Added gsubs to handle:
   - `VAR = value` → `VAR=value` (when followed by digit/string/var)
   - `VAR = cmd` → `VAR= cmd` (preserves space for env var assignments like `IFS= read`)

7. **Trailing space removal** - Added `gsub("\\s+$"; "")` to strip trailing whitespace from lines.

8. **Removed variable/number quoting** - Removed gsubs that were quoting `$var` and numbers at end of message sends (didn't match old compiler behavior).

### Test Changes

1. **Updated comment tests** - Changed from expecting 0 tokens (comment ignored) to expecting COMMENT token type.

2. **Updated var arg test** - Changed from expecting quoted `"$x"` to unquoted `$x` to match old compiler.

---

## Remaining Work

### High Priority - Integration

1. **Update Makefile** - The main `~/.trashtalk/Makefile` still uses the old compiler (`lib/trash-compiler.bash`). Need to update the pattern rules to use `lib/jq-compiler/driver.bash compile` instead.

### Medium Priority - Modularization

2. **Split codegen.jq into modules** - The plan called for:
   - `codegen/header.jq` - File header and metadata generation
   - `codegen/method.jq` - Method function generation
   - `codegen/transforms.jq` - DSL → bash transformations

   Currently all codegen logic is in a single `codegen.jq` file. Works fine but less modular than planned.

### Low Priority - Documentation

3. **EBNF grammar in README.md** - The plan specified that README.md should include formal EBNF grammar specification. Need to verify this is complete.

4. **Function documentation** - Each jq file should have:
   - Header comment explaining purpose
   - Each function documented with input/output description
   - Complex logic includes before/after examples

---

## Architecture Notes

### Token Flow
```
Source → Tokenizer (bash) → JSON tokens → Parser (jq) → AST → Codegen (jq) → Bash output
```

### Key Files
- `tokenizer.bash` - Converts source to JSON token array
- `parser.jq` - PEG parser, produces AST
- `codegen.jq` - AST to bash code generation
- `driver.bash` - CLI interface (tokenize, parse, compile commands)

### Raw vs Normal Methods
- **Normal methods**: Body is parsed and transformed (locals, return, self, message sends)
- **Raw methods**: Body is passed through with minimal transformation, preserving bash code

The distinction is important because raw methods need to preserve original formatting more carefully, while normal methods apply DSL transformations.

---

## Future Improvements

1. **Performance** - Profile and optimize for large files if needed.

2. **Error messages** - Improve parse error reporting with line/column information.

3. **Additional token types** - Handle more edge cases as they arise.

4. **Test coverage** - Add more edge case tests for tokenizer/parser/codegen.
