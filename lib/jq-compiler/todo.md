# jq-compiler TODO

## Current Status

**Test Results: 243/243 passing (100%)**

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

4. **~~Consolidate token reconstruction~~** - DONE
   - Created `tokensToString($raw)` - unified function with boolean parameter
   - `$raw=false`: Transform DSL constructs (normal methods)
   - `$raw=true`: Preserve bash code (raw methods)
   - Added convenience wrappers: `tokensToCode` and `tokensToRawCode`
   - Reduced codegen.jq from ~470 lines to 438 lines (~32 lines saved)

5. **~~Add AST-level unit tests~~** - DONE
   - Added 37 new parser tests (72 total, up from 35)
   - AST structure validation: type, name, parent, arrays
   - Method signature edge cases: 3-keyword methods, underscores, multi-method
   - Instance variable edge cases: mixed defaults, no defaults
   - Trait/inheritance: full traits with methods, multiple traits
   - Parser warnings: unknown tokens generate warnings
   - Method body tokens: verify PIPE, ASSIGN, CARET, SUBSHELL, VARIABLE preserved

6. **~~Implement synchronization points~~** - DONE
   - Added `isSyncPoint` function to detect class-level keywords
   - Added `synchronize` function that skips tokens to next sync point
   - Updated `parseClassBody` to use synchronization on errors
   - Fixed infinite loop bug in `parseInstanceVarsSimple` for unknown tokens
   - Added 13 new tests for error recovery scenarios
   - Parser now gracefully recovers from: garbage tokens, bad method declarations, syntax errors

### Future Phases - Smalltalk Syntax Roadmap

11. **Phase 3: Block Closures** (Not started)
    - Block syntax: `[:x | x + 1]`
    - Block passing to methods: `@ collection do: [:each | @ each print]`
    - Common iteration patterns: `do:`, `collect:`, `select:`, `reject:`

12. **Phase 4: Control Flow** (Not started)
    - Boolean messages: `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`
    - Loops: `whileTrue:`, `timesRepeat:`
    - Example: `(count > 0) ifTrue: [@ self decrement]`

13. **Phase 5: Collection Literals** (Not started)
    - Array literals: `#(1 2 3)`
    - Dictionary literals: `#{key: value}`
    - Symbol syntax: `#symbol`

### Low Priority

7. **Performance optimization** - Only if profiling shows problems
   - Bash tokenizer is O(n) per character access in some versions
   - Acceptable for files <2000 lines
   - Consider awk/sed for hot paths if needed

8. **Modularization** - Split codegen only if it grows beyond ~800 lines
   - Potential split: `header.jq`, `transforms.jq`, `method.jq`, `main.jq`
   - jq's `include` requires files in library path

9. **~~Add source locations to AST nodes~~** - DONE
   - Added `location: {line, col}` to class, method, instanceVar, include, requires nodes
   - Locations captured from the defining token (class name, method keyword, etc.)
   - Added 10 new tests for location info
   - Enables better error messages with exact source positions

10. **~~Show source context in error messages~~** - DONE
    - Added `show_error_context()` helper to display source line with caret
    - Added `show_errors_with_context()` to process multiple errors
    - Updated `cmd_parse()` to use new context display
    - Errors now show: line number, message, actual source code, and caret pointing to column

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

### Phase 2: Smalltalk Syntax Enhancements - COMPLETED

Implemented enhanced Smalltalk-style expression parsing with instance variable inference.

#### Features Implemented

1. **Expression Parser (Pratt Parser)** - `codegen.jq` lines 25-338
   - Binding powers for operators: `:=`, `+`, `-`, `*`, `/`, `%`
   - Parses Smalltalk-like expressions into AST
   - Handles: numbers, strings, identifiers, self, subshells, arithmetic, returns

2. **Instance Variable Inference** - `expr_gen()` and `expr_gen_arith()`
   - Bare identifiers like `value` automatically become `$(_ivar value)`
   - Local variables (from `| x y |` declarations) become `$x`, `$y`
   - Method arguments tracked as locals
   - Example: `result := value + step` → `result="$(( $(_ivar value) + $(_ivar step) ))"`

3. **Expression Arguments in Keyword Messages**
   - `@ self setValue: v + 1` → `@ "$_RECEIVER" setValue $(( $v + 1 ))`
   - `@ self at: x + 1 put: y * 2` → `@ "$_RECEIVER" at_put $(( $x + 1 )) $(( $y * 2 ))`
   - Full expression parsing for each keyword argument

4. **Cascade Syntax**
   - `@ self inc; inc; inc.` generates three separate message sends:
     ```bash
     @ "$_RECEIVER" inc
     @ "$_RECEIVER" inc
     @ "$_RECEIVER" inc
     ```
   - `@ self add: 5; add: 10; add: 15.` works with keyword messages
   - AST node type: `cascade` with `receiver` and `messages` array

5. **Smart Parser Selection** - `should_use_expr_parser()`
   - Detects Smalltalk-style patterns: `identifier := identifier`, arithmetic operators
   - Excludes bash constructs: `echo`, `jq`, `if/then/fi`, command pipes
   - Legacy code (Array.trash, Store.trash) continues to use legacy parser
   - New Smalltalk code uses expression parser with ivar inference

#### Test Files
- `tests/test_expr.trash` - Test class with arithmetic, precedence, assignment, message sends, cascades
- `tests/test_expr_codegen.bash` - 7 unit tests for expression code generation

#### Key Bug Fixes
- **Null array handling**: `expr_is_local` and `expr_is_ivar` now handle null arrays with `($locals // [])`
- **jq scoping in reduce**: Capture `.locals as $current_locals` before pipes to avoid context issues
- **Tokenizer decimal handling**: `2.` no longer parsed as float when `.` is statement terminator
- **Command pipe detection**: Fixed false positives on local var declarations `| a b |`

---

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
