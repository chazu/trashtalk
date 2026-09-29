#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null
# Execute the marked canonical examples themselves, so edits cannot silently
# diverge from a hand-copied fixture. Never extract installation/provider blocks.
extract() {
    awk -v marker="<!-- smoke: $2 -->" '
      $0 == marker {found=1; next}
      found && /^```/ {if (body) exit; body=1; next}
      body {print}
      END {if (!found || !body) exit 1}
    ' "$TRASHTALK_DIR/$1"
}
extract README.md walkthrough > "$TMPDIR/walkthrough.bash"
source "$TMPDIR/walkthrough.bash" > "$TMPDIR/walkthrough.out"
[[ "$(@ "$counter" getValue)" == 8 ]]
[[ "$(@ "$items" at: 0)" == hello ]]
compile_example() {
    extract "$1" "$2" > "$TMPDIR/$3.trash"
    bash "$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TMPDIR/$3.trash" > "$TRASHDIR/.compiled/$3"
}
compile_example README.md class Greeting
[[ "$(@ Greeting for: Ada)" == 'Hello, Ada' ]]
compile_example docs/trashtalk-patterns.md recipes RecipeExamples
recipe=$(@ RecipeExamples new)
[[ "$(@ "$recipe" add: 3)" == 3 ]]
[[ "$(@ RecipeExamples sumBelow: 5)" == 10 ]]
[[ "$(@ RecipeExamples printRange)" == $'1\n2\n3\ndone' ]]
[[ "$(@ RecipeExamples category: 5)" == 'small positive' ]]
[[ "$(@ RecipeExamples category: 12)" == other ]]
[[ "$(@ RecipeExamples label: ' padded ')" == padded ]]
[[ "$(@ RecipeExamples describe: '{"name":"parts","count":3}')" == 'parts: 3' ]]
[[ "$(@ RecipeExamples status: failed)" == 'Needs review' ]]
[[ "$(@ RecipeExamples nameOrDefault: '')" == anonymous ]]
extract docs/PROCESS.md process > "$TMPDIR/process.bash"
source "$TMPDIR/process.bash"
extract docs/FUTURE.md future > "$TMPDIR/future.bash"
source "$TMPDIR/future.bash" > "$TMPDIR/future.out"
[[ "$(cat "$TMPDIR/future.out")" == ready ]]
echo 'PASS: canonical README, DSL recipes, Process, and Future examples run offline'
