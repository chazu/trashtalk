# Jev via OpenRouter proof of concept

Jev makes structured decisions from state and typed questions. It does not
produce chat completions. The adapter is explicitly `OpenRouter::Jev`, using
`POST https://openrouter.ai/api/alpha/decisions` and `typesafe/jev-1.13`.
See [OpenRouter's Jev guide](https://openrouter.ai/blog/insights/what-is-jev/).

## Run the example

With `OPENROUTER_API_KEY` already exported:

```bash
make
bash examples/jev.bash
bash examples/jev.bash 'The export button crashes, and payroll is blocked today.'
```

Each invocation makes one billable request. The example's behavior is in
[`Examples::JevTicket`](../trash/Examples/JevTicket.trash): a DSL JSON literal
asks for a team (`choice`), urgency (`score`), and refund intent (`noul`).
The Bash script only loads the runtime and sends the message.

From a loaded Trashtalk shell:

```bash
source lib/trash.bash
@ Examples::JevTicket assess: 'Please refund the duplicate charge before Friday.'

questions='{"refund":{"type":"noul","instructions":"Is the customer asking for money back?"}}'
@ OpenRouter::Jev decide: 'Please refund the duplicate charge.' questions: "$questions"
```

`decide:questions:` accepts nonempty text state and an encoded JSON questions
object. DSL callers can construct questions with `#{...} asJson`, as the
example does. Jev question IDs label returned answers; describe the task in
`instructions` and the choices/levels in `criteria`.

The result preserves the provider's JSON: `answers`, served `model`, request
`id`, `provider`, and `usage`. Read `answers.team.choice`,
`answers.urgency.score`, and `answers.refund.noul` with DSL JSON accessors.
Scores can be fractional; a noul is a probability, not a Boolean. The example
returns the decisions without applying thresholds or taking external actions.

HTTP, transport, and malformed-response failures return nonzero with a JSON
`outcome` (`http_error`, `transport_error`, or `response_shape_error`). Invalid
local inputs and missing credentials fail before HTTP. Successful HTTP alone
is insufficient: every requested answer must have its matching type/value.
The adapter does basic shape checks; it is not a full API schema validator.

Only the curl/file boundary is raw Bash. Credentials go into a mode-600
curl config inside a private temporary directory, cleaned on return; they
are absent from command arguments. The old `OpenRouter complete:` POC is
removed. The provider now shares transport/schema checks with CLM; see [typed decisions](typed-decisions.md). It remains separate from Agent and makes no automatic retries.

## Verification

`bash tests/test_jev.bash` runs isolated offline regression checks.

A live run on 2026-09-28 returned `team.choice = billing`,
`urgency.score = 1.99`, and `refund.noul = 0.99` for the default example.
Served model: `typesafe/jev-1.13-20260917`; request ID:
`gen-dec-1790638911-PGtHnuGo7LLsRSzU5B44`. Usage was 400 input tokens and
69 output tokens. These are observed results, not exact-value test expectations.
