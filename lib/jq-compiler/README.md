# Trashtalk jq-based Compiler

A modular compiler for the Trashtalk DSL that uses JSON as an intermediate representation.

## Architecture

```
.trash source → Tokenizer (bash) → JSON tokens → Parser (jq) → JSON AST → CodeGen (jq) → .bash output
```

### Components

| File | Purpose |
|------|---------|
| `tokenizer.bash` | Converts .trash source to JSON token array |
| `parser.jq` | Parses tokens into JSON AST using PEG-style combinators |
| `codegen.jq` | Generates bash code from AST |
| `driver.bash` | CLI interface for all compilation stages |

## Usage

```bash
# Full compilation (default)
./driver.bash compile path/to/Class.trash > output.bash

# Individual stages for debugging
./driver.bash tokenize path/to/Class.trash  # Output tokens as JSON
./driver.bash parse path/to/Class.trash     # Output AST as JSON
./driver.bash ast path/to/Class.trash       # Pretty-printed AST
```

## Token Types

The tokenizer produces JSON tokens with the following types:

| Type | Example | Description |
|------|---------|-------------|
| `IDENTIFIER` | `Counter`, `myVar` | Variable/class names |
| `KEYWORD` | `method:`, `subclass:` | Identifiers ending with colon |
| `STRING` | `'hello'` | Single-quoted strings |
| `DSTRING` | `"hello"` | Double-quoted strings |
| `NUMBER` | `42`, `-3.14` | Numeric literals |
| `LBRACKET` / `RBRACKET` | `[` / `]` | Method body delimiters |
| `DLBRACKET` / `DRBRACKET` | `[[` / `]]` | Bash conditional brackets |
| `PIPE` | `\|` | Pipe character |
| `CARET` | `^` | Return operator |
| `AT` | `@` | Message send operator |
| `ASSIGN` | `:=` | Assignment operator |
| `SUBSHELL` | `$(...)` | Command substitution |
| `VARIABLE` | `$var`, `${var}` | Variable references |
| `ARITHMETIC` | `$((...))` | Arithmetic expansion |
| `SEMI` | `;` | Statement separator |
| `AND` / `OR` | `&&` / `\|\|` | Boolean operators |
| `REDIRECT` | `>&`, `>>` | Redirections |
| `GT` / `LT` | `>` / `<` | Comparison/redirect |
| `MATCH` | `=~` | Regex match |
| `EQUALS` | `=` | Assignment |
| `NEWLINE` | | Line break |

## AST Structure

### Class/Trait Definition

```json
{
  "type": "class",
  "name": "Counter",
  "parent": "Object",
  "isTrait": false,
  "instanceVars": [
    {"name": "value", "default": {"type": "number", "value": "0"}}
  ],
  "traits": ["Debuggable"],
  "requires": [],
  "methods": [...]
}
```

### Method Definition

```json
{
  "type": "method",
  "kind": "instance",       // or "class"
  "raw": false,             // true for rawMethod
  "selector": "setValue",   // joined selector for keyword methods
  "keywords": ["setValue"], // individual keywords
  "args": ["newVal"],       // argument names
  "body": {
    "type": "block",
    "tokens": [...]         // body tokens for code generation
  }
}
```

## DSL Transformations

The code generator applies these transformations to method bodies:

| Trashtalk | Bash |
|-----------|------|
| `\| var1 var2 \|` | `local var1 var2` |
| `^ expression` | `echo expression` |
| `self` | `$_RECEIVER` |
| `@ self method` | `@ "$_RECEIVER" method` |
| `var := expr` | `var=expr` |
| `@ recv key: val` | `@ "recv" key "val"` |
| `@ recv k1: a1 k2: a2` | `@ "recv" k1_k2 a1 a2` |

### Raw Methods

Methods declared with `rawMethod:` or `rawClassMethod:` receive minimal transformation,
preserving bash syntax as-is. Only essential spacing normalization is applied.

## Grammar (EBNF)

```ebnf
class       = identifier ("subclass:" identifier | "trait") body
body        = { instanceVars | include | requires | method }
instanceVars = "instanceVars:" { varSpec }
varSpec     = identifier | keyword number | keyword string
include     = "include:" identifier
requires    = "requires:" string
method      = methodKind methodSig "[" tokens "]"
methodKind  = "method:" | "classMethod:" | "rawMethod:" | "rawClassMethod:"
methodSig   = identifier | { keyword identifier }
```

## Known Limitations

1. **Nested quotes in raw methods**: Complex nested double-quote patterns inside
   double-quoted strings (e.g., `"outer $(cmd "inner")"`) may not tokenize correctly.

2. **Complex bash constructs**: Some advanced bash syntax in raw methods may require
   manual escaping or restructuring.

## Development

### Testing

Compare output with the original compiler:

```bash
# Compile with new compiler
./driver.bash compile Counter.trash > /tmp/new.bash

# Compare with existing compiled output
diff /tmp/new.bash trash/.compiled/Counter
```

### Debugging

Use individual stages to inspect intermediate representations:

```bash
# Check tokenization
./driver.bash tokenize Counter.trash | jq .

# Check parsing
./driver.bash parse Counter.trash | jq .

# Check specific AST elements
./driver.bash parse Counter.trash | jq '.methods[] | {selector, args}'
```

## Dependencies

- `bash` 4.0+
- `jq` 1.6+
