#!/usr/bin/env bash
# Helper script for parallel bash compilation
# Called by Makefile with: compile-bash.sh <source.trash> <compiled_dir> <jq_compiler> <trash_dir>

src="$1"
COMPILED_DIR="$2"
JQ_COMPILER="$3"
TRASH_DIR="$4"

relpath="${src#$TRASH_DIR/}"
if [[ "$relpath" == traits/* ]]; then
    # Traits go in traits/ subdirectory
    outname="traits/${relpath#traits/}"
elif [[ "$relpath" == user/* ]]; then
    # User classes compile to top-level (user/ is organizational, not a namespace)
    outname="${relpath#user/}"
else
    # Namespaced classes: Tools/Jq -> Tools__Jq
    outname="$(echo "$relpath" | sed 's/\//__/g')"
fi
outname="${outname%.trash}"
outfile="$COMPILED_DIR/$outname"

mkdir -p "$(dirname "$outfile")"

if "$JQ_COMPILER" compile "$src" > "$outfile" 2>/dev/null; then
    echo "  ✓ $outname"
else
    echo "  ✗ $outname (compilation failed)"
    exit 1
fi
