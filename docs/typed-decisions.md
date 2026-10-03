# Typed decisions: Jev and Decider

`DecisionStage` separates application questions/interpretation from the provider.
`Decider::Client` uses Decider, the System 1 model on a BC-250 in the homelab;
`OpenRouter::Jev` uses OpenRouter Decisions. These stateless decision services
are separate from `Agent`.

```bash
make NPROCS=1
bash examples/decision.bash decider
bash examples/decision.bash jev  # explicit billable OpenRouter request
```

Decider's API, deployment and limits belong to Batcave (`~/inf/batcave/DECIDER.md`
and `docs/decider-service.md`). It is reachable only from tailnet hosts and is not
always on. A send never wakes a board, starts a service, or falls back to another
provider. The earlier CLM targets (`clm-local`, `clm-bc250`, `clm-prefer-bc250`)
are retired; Decider replaced them.

## Public messages

```bash
source lib/trash.bash
target=$(@ Decision::Target named: decider)
@ Decision::Target requireReady: "$target"
@ Examples::DecisionTicket decide: 'Please refund the duplicate charge.' using: "$target"
@ Gmail::Review assess: "$message_json" using: "$target"
```

Resolve once and pass the temporary JSON target through a workflow.
`requireReady:` probes Decider's `/health` (`ready` and the selected model) and
fails with `DecisionUnavailableError` when it is off; batch entry points such as
`Gmail::Review preview:limit:` call it once before reading mail. Jev needs no probe.

`TRASHTALK_DECISION_TARGET` (`decision.target`) selects `jev` or `decider` for
implicit selection, defaulting to `decider`. `decider.url` defaults to
`https://decider.tail7fd374.ts.net`, `decider.model` to `decider-2b-v11-Q4_K_M`,
and `decider.textLimit` to 4000. Decider takes no credential.
`OPENROUTER_API_KEY` is used only by Jev. Credentials go in private curl configs,
never curl argv or execution receipts.

Applications include `DecisionStage`, supply `questions`, and optionally override
`stateFor:` and `interpret:`. The replay seam is `evaluate:questions:using:`.
`decide:using:` returns `{value,response,execution}`: application interpretation,
intact provider receipt, and selected target. Failed decision sends return nonzero
with the original `{outcome,...}` failure receipt on stdout; stage and Gmail
workflow callers retain HTTP status, transport details, or malformed response text.

`Decision::Question` constructs `choice:among:`, `score:on:` and `probability:`
temporary JSON values. `Decision::Answer probability:atLeast:` compares decimals.
`Jev::Question` and `Jev::Answer` remain supported aliases for the shared typed
question/answer interfaces.

## Transport

Decider admits one inference at a time and answers HTTP 409 when busy. The HTTP
boundary retries 409 three times with jittered backoff (about 0.2, 0.4, 0.8s) and
retries nothing else. Decider requests have a 15-second deadline; its answers are
usually sub-second. Other failures, including a board that is off, are reported
as `transport_error` or `http_error` receipts.

## Context and state size

Decider's context is 2,048 tokens per scored sequence, including state, question
and option. Overflow is HTTP 400 `State exceeds trial context capacity`; input is
never silently truncated by the service. Characters per token vary from about 5
for prose to under 2 for URL-heavy text, so no fixed character limit is both safe
and generous.

A Decider target therefore carries `textLimit`, the message text an application
may send. Gmail shortens bodies to it and marks them `truncated`, so the existing
partial-content gates apply (no `likely_junk`, review needed). On a context
overflow, `Decision::Target shrink:after:` halves the limit and the Gmail stage
retries, down to 500 characters; `Gmail::Review` reruns both stages. The
`execution` receipt records the limit used. Measured on 2026-10-02: a 4,000
character prose body with 20 preference examples used about 1,700 tokens; a
URL-heavy body needed 2,000 characters.

## State and confidence

State is nonempty text. `Decision::State textFromJson:` explicitly renders JSON
into readable text for Gmail; providers never guess whether strings contain JSON.
Shared schemas do not imply calibrated probabilities. Gmail thresholds were set
against Jev and remain experimental suggestions; evaluate labelled examples before
reusing them with Decider. Decider's `confidence`, `x_p_max` and `certainty` are
distinct metrics, not acceptance thresholds. No email writes are introduced.

## Checks

`tests/test_decisions.bash` covers Decider selection, readiness, busy retries,
overflow shrinking, invalid endpoints and malformed answers. `tests/test_jev.bash`
preserves OpenRouter's contract; `tests/test_gmail_jev.bash` checks Gmail state,
receipts, thresholds, body fitting and overflow retries offline. These are not
model-accuracy qualifications.
