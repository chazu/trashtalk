#!/usr/bin/env bash
# The compact prompt indicator: "!N ?M" over open attention and unanswered
# questions, published atomically to run/attention for a prompt to read.
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
export TRASHTALK_USER=local-user
run_dir=$(mktemp -d "${TMPDIR:-/tmp}/attention-indicator.XXXXXX")
export TRASHTALK_RUN_DIR="$run_dir"
indicator="$run_dir/attention"
# The path follows TRASHTALK_RUN_DIR, then TRASHTALK_DIR/run.
[[ $(@ AgentWorkboard indicatorPath) == "$indicator" ]]
[[ $(TRASHTALK_RUN_DIR= @ AgentWorkboard indicatorPath) == "$TRASHTALK_DIR/run/attention" ]]
# Nothing waiting: an empty line, in a file that now exists.
[[ -z $(@ AgentWorkboard indicator) ]]
[[ -z $(@ AgentWorkboard publishIndicator) ]]
[[ -f "$indicator" && $(wc -c < "$indicator") -eq 1 ]]
# Publishing is a hint: an unwritable location reports, returns success, and leaves nothing behind.
[[ $(TRASHTALK_RUN_DIR=/dev/null/nowhere @ AgentWorkboard publishIndicator 2>&1 >/dev/null) == *'not updated'* ]]
TRASHTALK_RUN_DIR=/dev/null/nowhere @ AgentWorkboard publishIndicator >/dev/null 2>&1
# An unanswered question addressed to the human counts; one to another party does not.
@ Agent::Queue ensureSchema
mine=$(@ Inbox ask: 'ship it?' to: local-user from: 'session:fixture')
theirs=$(@ Inbox ask: 'ship it?' to: someone-else from: 'session:fixture')
_db_sql "INSERT INTO agent_questions(message_id,session,run,delivery_ids) VALUES('$mine','s','r','[]'),('$theirs','s','r','[]');"
[[ $(@ AgentWorkboard unansweredQuestionCount) == 1 ]]
[[ $(@ AgentWorkboard indicator) == '?1' ]]
# Answering removes it.
_db_sql "UPDATE agent_questions SET answer_id='answered' WHERE message_id='$mine';"
[[ -z $(@ AgentWorkboard indicator) ]]
# The worker tick publishes the current text every time, so its modification
# time doubles as a liveness signal.
_db_sql "UPDATE agent_questions SET answer_id='' WHERE message_id='$mine';"
rm -f "$indicator"
@ Agent::Worker tick >/dev/null
[[ $(cat "$indicator") == '?1' ]]
before=$(stat -f %m "$indicator" 2>/dev/null || stat -c %Y "$indicator")
touch -t 200001010000 "$indicator"
@ Agent::Worker tick >/dev/null
after=$(stat -f %m "$indicator" 2>/dev/null || stat -c %Y "$indicator")
(( after >= before ))
# No temporary file is left beside it.
[[ -z $(ls -A "$run_dir" | grep -v '^attention$' || true) ]]
rm -rf "$run_dir"
echo 'PASS: attention indicator'
