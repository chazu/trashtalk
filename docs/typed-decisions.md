# Typed decisions: Jev and CLM

`DecisionStage` separates application questions/interpretation from the provider.
`OpenRouter::Jev` uses OpenRouter Decisions; `CLM::Client` uses `/v1/systemone` on
the Mac or BC-250. These stateless decision services are separate from `Agent`.

```bash
make NPROCS=1
bash examples/decision.bash clm-local
CLM_BC250_URL=http://127.0.0.1:8701 bash examples/decision.bash clm-bc250
CLM_BC250_URL=http://127.0.0.1:8701 bash examples/decision.bash clm-prefer-bc250
bash examples/decision.bash jev  # explicit billable OpenRouter request
```

Install/run through `~/inf/batcave/scripts/clm-local.py`; BC-250 deployment also
belongs to Batcave (`docs/clm.md`). A send never installs weights, starts a server,
wakes a board, or falls back to OpenRouter.

## Public messages

```bash
source lib/trash.bash
target=$(@ Decision::Target named: clm-local)
@ Examples::DecisionTicket decide: 'Please refund the duplicate charge.' using: "$target"
@ Gmail::Review assess: "$message_json" using: "$target"
```

Resolve once and pass the temporary JSON target through a workflow.
`clm-prefer-bc250` checks remote then local health: `ok`, `embedder`, the selected
model, and no mock marker. A provider failure after selection fails the workflow;
it does not retry elsewhere or mix providers between Gmail stages.

`TRASHTALK_DECISION_TARGET` controls implicit selection, defaulting to `jev` for
existing applications. `CLM_BASE_URL` defaults to `http://127.0.0.1:8700`;
`CLM_BC250_URL` has no default. `CLM_MODEL` defaults to `clm-latest`.
`CLM_API_KEY` and `CLM_BC250_API_KEY` are optional independent credentials.
`OPENROUTER_API_KEY` is used only by Jev. Credentials go in private curl configs,
never curl argv or execution receipts.

Applications include `DecisionStage`, supply `questions`, and optionally override
`stateFor:` and `interpret:`. The replay seam is `evaluate:questions:using:`.
`decide:using:` returns `{value,response,execution}`: application interpretation,
intact provider receipt, and selected target. Batcave CLM adds pinned model/head
provenance in `response.deployment`.

`Decision::Question` constructs `choice:among:`, `score:on:` and `probability:`
temporary JSON values. `Decision::Answer probability:atLeast:` compares decimals.
Existing `Jev::Question`/`Jev::Answer` names inherit these implementations;
`JevDecision` remains an explicitly Jev-only legacy trait.

## State and confidence

State is nonempty text. `Decision::State textFromJson:` explicitly renders JSON
into readable text for Gmail; providers never guess whether strings contain JSON.
Shared schemas do not imply calibrated probabilities. Gmail thresholds remain
experimental suggestions, and no email writes are introduced.

Batcave initially allows 1,024 tokens per combined state/question or candidate,
one concurrent request, and rejects overflow with HTTP 422. Input is never silently
truncated. CLM confidence is a probability margin, not a correctness guarantee.
Evaluate labelled examples before reusing thresholds across providers/quantizations.

## Checks

`tests/test_decisions.bash` covers CLM health, routing, pinned targets, failures,
invalid endpoints and probabilities. `tests/test_jev.bash` preserves OpenRouter's
contract; `tests/test_gmail_jev.bash` checks Gmail state, receipts, thresholds and
proposal-only behavior offline. These are not model-accuracy qualifications.
