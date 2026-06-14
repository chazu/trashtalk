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

# Compile to a temp file first, then atomically move into place. Redirecting
# straight to "$outfile" truncates it before the compiler runs, so a failure
# would leave a partial/empty compiled file behind that could be sourced later.
stderr_file=$(mktemp)
tmp_outfile=$(mktemp "$outfile.XXXXXX")
if "$JQ_COMPILER" compile "$src" > "$tmp_outfile" 2>"$stderr_file"; then
    mv -f "$tmp_outfile" "$outfile"
    echo "  ✓ $outname"
else
    echo "  ✗ $outname (compilation failed)"
    if [[ -s "$stderr_file" ]]; then
        cat "$stderr_file" >&2
    fi
    rm -f "$stderr_file" "$tmp_outfile"
    exit 1
fi
rm -f "$stderr_file"
