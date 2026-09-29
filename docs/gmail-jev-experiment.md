# Gmail decisions with Jev

The shared stages now also support local/BC-250 CLM: [typed decisions](typed-decisions.md).
Set `TRASHTALK_DECISION_TARGET` explicitly; Jev remains the default.

## Junk review experiment

The current experiment asks one question: would the recipient be happy never
seeing this email? [`Gmail::Junk`](../trash/Gmail/Junk.trash) applies the same
`DecisionStage` trait in one request per message. It favors keeping personal mail,
receipts, deliveries, security/account notices, subscription price changes,
and obligations. It does not assume newsletters or community events are unwanted
without personal preferences. Reviewed examples can be supplied explicitly;
they are prompt context, not model training or global runtime state.

```bash
make
bash examples/gmail-junk.bash you@gmail.com
# Optional private JSON array of {sender, subject, verdict: "keep"|"junk"}:
bash examples/gmail-junk.bash you@gmail.com /path/to/preference-examples.json
```

This assesses up to 20 inbox messages, taking up to five from each Gmail
category: primary, promotions, updates, and forums. Social and all-inbox searches
fill gaps where possible. Repeated thread IDs or identical sender/subject pairs
are skipped. Each search inspects at most ten IDs; this is a bounded
convenience sample, not a random sample or an inbox-wide estimate. Gmail's
categories are used only for sampling and are not sent to Jev.

Output is JSONL with numbered proposals and model receipts. Probability below
0.2 means `likely_keep`; at least 0.9 means `likely_junk`; the middle is
`uncertain`. Missing, snippet-only, or truncated content downgrades likely junk
to uncertainty, while preserving the model's raw probability. These thresholds
are provisional, not calibrated guarantees. Review the numbered suggestions
and record corrections before using them to change anything in Gmail.

Public entry points are `@ Gmail::Junk assess: normalizedMessageJson` and
`@ Gmail::Junk assessMessage: MESSAGE_ID`. Both return suggestions only. These
selectors also accept `examples: examplesJson`; omission supplies an empty
array. Personal examples and review labels belong outside the repository.
Replaying emails whose labels appear in that context measures whether the
model follows those preferences, not generalization or classification accuracy.
An independent, unseen sample is needed to evaluate the personalized behavior.
The same authentication and OpenRouter setup below applies. Email content is sent
to OpenRouter/TypeSafe; no labels are created and no messages are moved/deleted.

HTML-only bodies now use Perl `HTML::Parser` (required for those messages) to
extract text and decode entities, ignoring script/style/head/template content.
No browser is invoked and no URLs are fetched. Plain text is preferred;
whitespace is normalized before the 12,000-character bound. Empty extraction
falls back to Gmail's snippet. Attachments and thread history remain excluded.

Live verification on 2026-09-28 assessed 20 different sender/subject pairs using
`typesafe/jev-1.13-20260917`: five likely keep, fifteen uncertain, no likely junk.
The final sample contained five each from primary, promotions, updates, and
social; repeated forum notifications were skipped. Three bodies used HTML text
extraction, seventeen used plain text, and two remained truncated. The final
sample cost $0.002674014 in provider-reported usage. An earlier sampling pass
was retained separately because it contained repeated discussions. The 60
offline Gmail/Jev checks pass, including probability boundaries, incomplete
content gates, HTML decoding, and model failure propagation. This first run
had no human labels and was not an accuracy measurement.

The recipient then labeled eight messages keep and twelve junk. A replay with
those exact reviewed sender/subject examples explicitly supplied in context
produced eight likely keep, nine likely junk, and three uncertain. Two junk
examples remained below the 0.9 threshold; one exceeded it but was gated by
truncated content. This is an in-sample preference-following check, not a
held-out evaluation. Personal labels and receipts remain outside the repository.
The expanded offline suite passes 65 checks, including explicit preference
propagation, call isolation, and malformed JSON rejection before a model call.

## Earlier category/attention experiment

This is a read-only experiment: Google Workspace CLI (`gws`) reads a bounded
mailbox sample, and two Trashtalk classes use the same `DecisionStage` trait to
suggest categories and attention. It does not create labels, mark messages
read, archive, send, or change Gmail. Email content is sent to OpenRouter and
TypeSafe for assessment. Nothing is stored in Trashtalk objects.

## Authentication

Install [Google Workspace CLI](https://github.com/googleworkspace/cli):

```bash
brew install googleworkspace-cli
```

Create a Google Cloud project or use an existing one. Enable the Gmail API.
In Google Auth Platform, configure an External OAuth app, add your Gmail
address as a test user if the app is in testing mode, and create a **Desktop
app** OAuth client. Download its JSON to `~/.config/gws/client_secret.json`.
Keep credentials outside this repository.

```bash
chmod 600 ~/.config/gws/client_secret.json
gws auth login --scopes https://www.googleapis.com/auth/gmail.readonly
gws gmail users getProfile --params '{"userId":"me"}'
```

Sign in with the mailbox you intend to assess and grant **View your email
messages and settings** on the final consent screen. Signing in can succeed
with identity access alone; a successful `getProfile` call verifies Gmail
access. If it returns `insufficientPermissions`, repeat the login and grant
the Gmail permission. `gws` owns OAuth/token storage.

With `gws` 0.22.5, re-authentication left an old access token cached. If
`gws auth status` includes `gmail.readonly` but the API still reports insufficient
scopes, remove only `~/.config/gws/token_cache.json` and retry; the saved refresh
credentials remain intact. See the upstream [cache invalidation issue](https://github.com/googleworkspace/cli/issues/764).

The same version uses `installed.project_id` in `client_secret.json` as an
explicit quota/billing project. For this personal Gmail setup, that caused a
`serviceusage.services.use` error. Keeping a private backup of the original
JSON and setting only `installed.project_id` to an empty string let Gmail use
the OAuth client's default project without the extra quota header. Client ID,
secret, and granted scopes stayed unchanged. This is local setup, not behavior
of the Trashtalk adapter; see [`get_quota_project`](https://github.com/googleworkspace/cli/blob/main/crates/google-workspace-cli/src/auth.rs).

Set `OPENROUTER_API_KEY` in your shell as for the existing Jev POC.

## Run

```bash
make
bash examples/gmail-jev.bash you@gmail.com 'in:inbox' 5
```

The first argument must match the authenticated Gmail address. The default
query is `in:inbox`; the default limit is 5, with an allowed range of 1–10.
Each message uses two billable model requests. Output is JSONL, one proposal
per message. An error stops the run with nonzero status; proposals already
printed remain valid, so a failed run can have partial output.

Public messages, from a loaded Trashtalk shell:

```bash
source lib/trash.bash
@ Gmail::Client requireAccount: you@gmail.com
@ Gmail::Review preview: 'in:inbox' limit: 5
@ Gmail::Review assessMessage: MESSAGE_ID
```

`Gmail::Client message:` decodes nested inline MIME parts and excludes
attachments. It prefers `text/plain`, then parses `text/html`; missing text
uses Gmail's snippet and is explicitly marked `bodySource: "snippet"`.
Bodies are bounded to 12,000 characters and
marked `truncated` when shortened. Partial content always sets the proposal's
`reviewNeeded` flag. It does not fetch links, attachments, or thread history.

## The trait experiment

[`DecisionStage`](../trash/traits/DecisionStage.trash) supplies `decide:`. Classes
provide `questions`, optionally `stateFor:` and `interpret:`. The result is
`{value, response}`: the application interpretation plus the complete provider
response, including probabilities, served model, usage, and request ID.
`evaluate:questions:using:` is the explicit provider boundary and replay seam.

[`Decision::Question`](../trash/Decision/Question.trash) supplies three temporary-value
constructors:

```text
choice: instructions among: optionsJson
score: instructions on: orderedLevelsJson
probability: proposition
```

[`Gmail::Categorizer`](../trash/Gmail/Categorizer.trash) defines the initial
categories in DSL: personal, finance, purchases, events, newsletters,
promotions, notifications, and other. Revise the descriptions there after
reviewing real results. These are proposed categories, not existing labels.

[`Gmail::Attention`](../trash/Gmail/Attention.trash) applies the same trait to
reply intent, action required, and urgency. Its input explicitly includes the
first stage's interpretation. [`Gmail::Review`](../trash/Gmail/Review.trash)
owns that sequencing and retains both receipts.

Thresholds are experimental policy, not calibrated guarantees. Category
confidence below 0.8 (or `other`) requests review. Reply/action probabilities
at least 0.8 suggest reply/action; values from 0.2 to below 0.8 request review.
Only when both are below 0.2 does it suggest reading at convenience. Urgency
remains a fractional score with its distribution and rubric. `Decision::Answer`
contains the small jq decimal-comparison primitive because Trashtalk's native
arithmetic is integer-only; thresholds and branching stay in the DSL.

## Verification

```bash
bash tests/test_gmail_jev.bash
bash tests/test_jev.bash
```

Offline fixtures exercise real public messages, both decision stages, MIME
normalization, exact CLI arguments, wrong-account rejection, uncertainty, and
failure propagation. Neither test reads Gmail or calls a real model.

A live Jev call on 2026-09-28 used the synthetic `Dinner on Friday?` fixture.
It suggested `events` with confidence 0.56 (review needed), then
`consider_reply` with reply probability 0.92. Request IDs were
`gen-dec-1790642208-l2T88QtF9XsjiivT3jIT` and
`gen-dec-1790642208-qoKhRRLV5iDEykIq0hdO`.

A separate live run on 2026-09-28 verified the selected account through Gmail
`getProfile`, then assessed five inbox messages successfully through ten Jev
requests. All receipts reported `typesafe/jev-1.13-20260917`. Suggestions were
three promotions, one notification, and one event. Two messages used snippets
and two hit the body limit; all five requested review because of partial
content or uncertainty. The summed provider-reported cost was $0.001603266.
Private proposals and request receipts were retained outside the repository;
no real email content is used as a fixture. This proves the read-only end-to-end
path, not classification quality or threshold calibration.
