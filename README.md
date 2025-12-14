# TODO

Reimplement instance persistence using https://www.dbpro.app/blog/sqlite-json-virtual-columns-indexing



# Trashtalk

A Smalltalk-inspired message-passing system for Bash.

Trashtalk brings object-oriented programming concepts to shell scripting: message passing, inheritance, traits, and instance persistence.

## Installation

Clone or copy this repository to `~/.trashtalk`:

```bash
git clone <repo-url> ~/.trashtalk
```

Add the following to your `.bashrc` or `.zshrc`:

```bash
source ~/.trashtalk/lib/trash.bash
```

Optionally, also source the Objective-C style parser for bracket syntax:

```bash
source ~/.trashtalk/lib/trash-parser.bash
```

## Quick Start

```bash
# Send a message to an object
@ Object fooBar

# Create a counter instance
counter=$(@ Counter new)
@ $counter setValue 5
@ $counter increment 3
@ $counter show

# Create an array
arr=$(@ Array new)
@ $arr push hello
@ $arr push world
@ $arr show

# System introspection
@ Trash listObjects
@ Trash methodsFor Counter
@ Trash help
```

## Syntax

Basic message sending:
```bash
@ <Receiver> <selector> [args...]
```

With the parser, you can also use Objective-C style brackets:
```bash
@ [Object methodName]
@ [Counter setValue:10]
@ [Array at:0 put:value]
```

## Creating Objects

Objects are defined as files in the `trash/` directory:

```bash
# trash/MyObject
is_a Object
include Debuggable

myMethod() {
    echo "Hello from MyObject"
}
```

Or use the built-in templates:
```bash
@ Trash quickCreate MyService service
@ Trash quickCreate MyTool tool
```

## Dependencies

Trashtalk vendors its dependencies in `lib/vendor/`:
- `kv-bash` - Key-value persistence
- `tuplespace/` - Event coordination
- `bsfl.sh` - Bash utility functions
- `fun.sh` - Functional programming utilities

External tools (install separately):
- `jo` - JSON output from shell
- `jq` - JSON processor
- `uuidgen` - UUID generation (usually pre-installed on macOS/Linux)

## Version

0.1.0

## Author

Chaz Straney
