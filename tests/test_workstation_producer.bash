#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
command -v cue >/dev/null || { echo 'SKIP: CUE not installed'; exit 0; }
honker_available || { echo 'SKIP: Honker not installed'; exit 0; }
honker_bootstrap
stream=$(@ Stream named: workstation.command-receipts.v1)
work=$(mktemp -d)
work=$(cd "$work" && pwd -P)
ln -s "$work" "$work-link"
export ARGV_FILE="$work/argv.json"
cat > "$work/argv.pl" <<'PERL'
use JSON::PP;
open my $f, '>', $ENV{ARGV_FILE} or die;
print $f JSON::PP->new->encode(\@ARGV);
print scalar <STDIN>;
exit 7;
PERL
set +e
printf 'stdin unchanged\n' | bin/trash-command --cwd "$work-link" --label 'argv test' -- perl "$work/argv.pl" '' 'two words' '$(touch forbidden)' '*' $'line\nbreak' >"$work/out"
rc=$?
set -e
[[ $rc == 7 && $(cat "$work/out") == 'stdin unchanged' && ! -e "$work/forbidden" ]]
jq -e '.==["","two words","$(touch forbidden)","*","line\nbreak"]' "$ARGV_FILE" >/dev/null
@ "$stream" read | jq -e --arg w "$work" 'length==1 and (.[0].payload|fromjson|.exitCode==7 and .workspace==$w and .commandLabel=="argv test" and (.receiptId|startswith("commandreceipt_")) and (.startedAt<=.finishedAt) and (has("stdout")|not))' >/dev/null
bin/trash-command --cwd "$work" --label success -- true
bin/trash-command --cwd "$work" --label private --capture -- perl -e 'print "secret=super-secret\e[31m" x 10000; print STDERR "private-stderr"' >"$work/captured" 2>"$work/err"
grep -q private-stderr "$work/err"
[[ $(wc -c <"$work/captured") -gt 4096 ]]
receipts=$(@ "$stream" read)
jq -e 'length==3 and (.[1].payload|fromjson|.exitCode==0) and (.[2].payload|fromjson|(.display.summary|contains("output redacted (4096+ bytes)")) and (.display.summary|length)<=256)' <<<"$receipts" >/dev/null
! grep -qE 'super-secret|private-stderr|stdout|stderr' <<<"$receipts"
# system() distinguishes a real signal termination from explicit exit(143).
perl -e 'system @ARGV; exit(($? & 127)==15 ? 0:1)' bin/trash-command --cwd "$work" --label signal -- perl -e 'kill 15,$$'
set +e
bin/trash-command --cwd "$work" --label exit143 -- bash -c 'exit 143'
rc=$?
set -e
[[ $rc == 143 ]]
@ "$stream" read | jq -e 'length==5 and (.[3].payload|fromjson|.exitCode==143)' >/dev/null
# Closed fixture path is identical to the producer's public publication method.
fixture=$(cat schemas/workstation/v1/fixtures/CommandReceipt.valid.json)
bin/trash-receipt --publish "$fixture"
if bin/trash-receipt --publish "$(jq -c '.secret="do-not-print"' <<<"$fixture")" >"$work/invalid" 2>&1; then exit 1; fi
! grep -q do-not-print "$work/invalid"
# Publication failure cannot affect a successful or failing child.
mkdir "$work/fake"
printf '#!/usr/bin/env bash\nexit 1\n' > "$work/fake/cue"
chmod +x "$work/fake/cue"
for expected in 0 9; do
 set +e
 PATH="$work/fake:$PATH" bin/trash-command --cwd "$work" --label unavailable -- bash -c "exit $expected" 2>"$work/failure"
 rc=$?
 set -e
 [[ $rc == "$expected" ]]
 grep -q 'publication failed; child outcome is unchanged' "$work/failure"
done
[[ $(@ Store countByClass: Workstation::Attention) == 0 && $(@ Store countByClass: Agent::Delivery) == 0 ]]
echo 'PASS: exact argv, stdin, canonical cwd, exits/signals, safe bounded capture, fixtures and publication failure'
