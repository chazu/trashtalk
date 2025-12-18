# jq-compiler

**Status: 289/289 tests passing (100%)**

All tasks consolidated into the main `/todo.md` file.
Completed work moved to `/completed.md`.

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

### Architecture Assessment
The architecture uses **island parsing** (dialect embedding) - a well-established pattern for DSLs with embedded host language code:
- **Pass 1**: Full tokenization (Bash)
- **Pass 2a**: Parse class structure, method bodies remain as token arrays (jq)
- **Pass 2b**: Transform method body tokens during codegen (jq)

This is pragmatic given Bash's complex, context-sensitive grammar.
