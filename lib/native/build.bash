#!/usr/bin/env bash
# Build native Trashtalk classes
# Usage: ./build.bash [class_name]
#        ./build.bash           # build all
#        ./build.bash Counter   # build only Counter

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"
OUTPUT_DIR="$TRASHTALK_ROOT/trash/.compiled"

build_class() {
    local class_name="$1"
    local class_dir="$SCRIPT_DIR/${class_name,,}"  # lowercase

    if [[ ! -d "$class_dir" ]]; then
        echo "Error: Native source directory not found: $class_dir" >&2
        return 1
    fi

    # Copy fresh source for embedding
    local trash_source="$TRASHTALK_ROOT/trash/${class_name}.trash"
    if [[ -f "$trash_source" ]]; then
        cp "$trash_source" "$class_dir/${class_name}.trash"
    fi

    echo "Building ${class_name}.native..."
    (
        go build -o "$OUTPUT_DIR/${class_name}.native" "$class_dir"
    )

    echo "Built: $OUTPUT_DIR/${class_name}.native"
    "$OUTPUT_DIR/${class_name}.native" --info
}

# Find all native class directories
find_native_classes() {
    for dir in "$SCRIPT_DIR"/*/; do
        if [[ -f "${dir}main.go" ]]; then
            basename "$dir"
        fi
    done
}

if [[ $# -eq 0 ]]; then
    # Build all native classes
    for class_dir in $(find_native_classes); do
        # Convert to PascalCase (simple heuristic)
        class_name="$(echo "$class_dir" | sed 's/\b\(.\)/\u\1/g')"
        build_class "$class_name" || true
    done
else
    # Build specific class
    build_class "$1"
fi
