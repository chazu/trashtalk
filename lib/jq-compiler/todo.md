# jq-compiler TODO

## Current Status

**Test Results: 166/167 passing (99.4%)**

All .trash files compile with valid bash syntax. The only failing test is an exact-match comparison for Process.trash, which has cosmetic differences from the old compiler output.

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

## Remaining Issues

### 1. Process.trash Exact Match (Low Priority)

The remaining test failure is cosmetic - Process.trash compiles with valid syntax but doesn't match character-for-character.

#### Issue A: Multiple Spaces in Strings Collapsed

**Problem:** The `gsub(" +"; " ")` normalization in `tokensToRawCode` collapses multiple spaces everywhere, including inside quoted strings.

**Example:**
```bash
# Old compiler:
echo "  @ Process spawn <object>                  - Spawn new process"

# New compiler:
echo " @ Process spawn <object> - Spawn new process"
```

**Solution approaches:**
1. **Token-aware gsub** - Apply space collapse only outside of DSTRING token boundaries. Would require tracking which parts of the output came from strings.

2. **Pre-process strings** - Before joining tokens, mark string content with placeholders, apply gsubs, then restore. Complex but preserves string content.

3. **Remove gsub entirely** - Find alternative approach to whitespace normalization that doesn't use blanket regex. Would need to handle spacing at token output level instead.

4. **Accept difference** - The difference is cosmetic (affects help text formatting). Functionally equivalent.

#### Issue B: Case Statement Spacing

**Problem:** Case patterns have different spacing around `|`.

**Example:**
```bash
# Old compiler:
"message"|*)

# New compiler:
"message" | *)
```

**Solution:** The PIPE token in `tokensToRawCode` outputs `| ` (with trailing space). Need to detect case pattern context and output without surrounding spaces. Could add gsub: `gsub("\" \\| \\*\\)"; "\"|*)")`

#### Issue C: Case/Else Block Indentation

**Problem:** Some `else` and case body lines have incorrect nesting depth.

**Solution:** The `smartIndent` function needs refinement:
- Track `case`/`esac` nesting separately from `if`/`fi`
- Handle `)` case pattern terminators for depth tracking
- Consider `elif` as same level as `if`, not nested

#### Issue D: Comment Spacing

**Problem:** `code  # comment` vs `code # comment` (double vs single space before comment).

**Solution:** COMMENT tokens should preserve leading whitespace from original source. Would need tokenizer change to capture preceding whitespace count.

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

1. **Proper whitespace preservation** - Track original whitespace in tokens for faithful reproduction.

2. **Case statement handling** - Improve depth tracking for case/esac blocks.

3. **String content protection** - Ensure gsubs don't affect content inside quoted strings.

4. **Performance** - Profile and optimize for large files if needed.
