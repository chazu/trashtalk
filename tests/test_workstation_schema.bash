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
[[ $(@ WorkstationSchema digest) =~ ^[a-f0-9]{64}$ ]]
echo 'PASS: workstation closed CUE fixtures and bounded diagnostics'

exit 0
