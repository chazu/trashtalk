# Fail-fast assertions shared by runtime tests. Each one exits the test on its
# first failure; a passing check or contains prints PASS and counts `passed`.
# A test that needs different output keeps its own definition after sourcing.

# check NAME EXPECTED ACTUAL
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }

# must COMMAND...: run a command, or fail the test naming it.
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }

# contains NAME NEEDLE HAYSTACK
contains() { [[ "$3" == *"$2"* ]] || { echo "FAIL: $1 missing $2"; exit 1; }; echo "PASS: $1"; passed=$((passed+1)); }
