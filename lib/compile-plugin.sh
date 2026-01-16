#!/usr/bin/env bash
# Helper script for parallel plugin compilation
# Called by Makefile with: compile-plugin.sh <source.trash> <build_dir> <compiled_dir> <jq_compiler> <procyon> <dylib_ext> <trash_dir>

src="$1"
BUILD_DIR="$2"
COMPILED_DIR="$3"
JQ_COMPILER="$4"
PROCYON="$5"
DYLIB_EXT="$6"
TRASH_DIR="$7"

# Convert to absolute paths (needed since we cd into BUILD_DIR)
BUILD_DIR="$(cd "$BUILD_DIR" && pwd)"
COMPILED_DIR="$(cd "$COMPILED_DIR" && pwd)"

relpath="${src#$TRASH_DIR/}"
outname="$(echo "${relpath%.trash}" | sed 's/\//__/g')"
gofile="$BUILD_DIR/$outname.go"
dylibfile="$COMPILED_DIR/$outname.$DYLIB_EXT"

# Generate Go code with Procyon (discard stderr to avoid corrupting the .go file)
if ! "$JQ_COMPILER" parse "$src" 2>/dev/null | "$PROCYON" --mode=shared > "$gofile" 2>/dev/null; then
    echo "  ✗ $outname (codegen failed)"
    rm -f "$gofile"
    exit 0
fi

# Skip if no code generated (unsupported class)
if [[ ! -s "$gofile" ]]; then
    echo "  - $outname (skipped - no native support)"
    rm -f "$gofile"
    exit 0
fi

# Check if the generated file is valid Go (starts with "package")
if ! head -1 "$gofile" | grep -q '^package'; then
    echo "  - $outname (skipped - invalid Go output)"
    rm -f "$gofile"
    exit 0
fi

# Compile to shared library
if (cd "$BUILD_DIR" && CGO_ENABLED=1 go build -buildmode=c-shared -o "$dylibfile" "$outname.go") 2>&1; then
    echo "  ✓ $outname.$DYLIB_EXT"
else
    echo "  ✗ $outname (build failed)"
fi
