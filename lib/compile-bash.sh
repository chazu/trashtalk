#!/usr/bin/env bash
# Helper script for parallel bash compilation
# Called by Makefile with: compile-bash.sh <source.trash> <compiled_dir> <jq_compiler> <procyon> <trash_dir>
#
# Strategy: Try Procyon first; if it fails, fall back to legacy jq-compiler

src="$1"
COMPILED_DIR="$2"
JQ_COMPILER="$3"
PROCYON="$4"
TRASH_DIR="$5"

relpath="${src#$TRASH_DIR/}"
if [[ "$relpath" == traits/* ]]; then
    outname="traits/${relpath#traits/}"
else
    outname="$(echo "$relpath" | sed 's/\//__/g')"
fi
outname="${outname%.trash}"
outfile="$COMPILED_DIR/$outname"

mkdir -p "$(dirname "$outfile")"
srcpath="$(cd "$(dirname "$src")" && pwd)/$(basename "$src")"

# Try Procyon first (generates better bash code)
if "$JQ_COMPILER" parse "$src" 2>/dev/null | "$PROCYON" --mode=bash --source-file="$srcpath" > "$outfile" 2>/dev/null; then
    echo "  ✓ $outname"
# Fall back to legacy jq-compiler
elif "$JQ_COMPILER" compile "$src" > "$outfile" 2>/dev/null; then
    echo "  ✓ $outname (legacy)"
else
    echo "  ✗ $outname (compilation failed)"
    exit 1
fi
