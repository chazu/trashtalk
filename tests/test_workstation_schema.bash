#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
source tests/helpers/workstation.bash
command -v cue >/dev/null || { echo 'SKIP: real CUE fixture tests require cue'; exit 0; }
for file in schemas/workstation/v1/fixtures/*.json; do
 root=${file##*/}; root=${root%%.*}
 if [[ $file == *.valid.json ]]; then
  workstation_vet_fixture "$file" "$root" >/dev/null
 else
  if workstation_vet_fixture "$file" "$root" >"$TMPDIR/result" 2>"$TMPDIR/diagnostic"; then echo "Unexpected valid fixture: $file"; exit 1; fi
  [[ $(wc -c <"$TMPDIR/diagnostic") -lt 2048 ]]
  ! grep -q sensitive-do-not-echo "$TMPDIR/diagnostic"
 fi
done
[[ $(@ Workstation::Schema digest) =~ ^[a-f0-9]{64}$ ]]
# Process-local memo: exact bytes already vetted skip the CUE process; any
# other bytes still reach CUE. Failures are never remembered.
valid=$(cat schemas/workstation/v1/fixtures/EventSubscription.valid.json)
@ Workstation::Schema validate: "$valid" as: EventSubscription >/dev/null
@ Tools::Cue isInstalled >/dev/null
saved_vet=$(declare -f __Tools__Cue__class__vet_json_)
__Tools__Cue__class__vet_json_() { echo "CUE invoked unexpectedly" >&2; return 1; }
@ Workstation::Schema validate: "$valid" as: EventSubscription >/dev/null
if @ Workstation::Schema validate: "$(jq -c .id=\"eventsubscription_other\" <<<"$valid")" as: EventSubscription >/dev/null 2>&1; then echo "Modified document skipped CUE"; exit 1; fi
if @ Workstation::Schema validate: "$valid" as: Attention >/dev/null 2>&1; then echo "Different root skipped CUE"; exit 1; fi
eval "$saved_vet"
invalid=$(cat schemas/workstation/v1/fixtures/EventSubscription.id.invalid.json)
if @ Workstation::Schema validate: "$invalid" as: EventSubscription >/dev/null 2>&1; then exit 1; fi
if @ Workstation::Schema validate: "$invalid" as: EventSubscription >/dev/null 2>&1; then echo "Failure was memoized"; exit 1; fi
echo 'PASS: workstation closed CUE fixtures and bounded diagnostics'

exit 0
