# jcode-sol

Gusgus on Jcode with the OpenAI subscription login and `gpt-6.1-sol`. Nothing
runs locally, so there are no omlx, pi, or Hindsight parts.

| File | Effect |
| --- | --- |
| `trashtalk.config` | `gusgus.profile = "jcode"`, `jcode.model = "gpt-6.1-sol"`. `jcode.provider` stays at its `openai` default. |

Requirements: Jcode logged in to OpenAI (`@ Jcode login`). Its token refresh is
single-use, so a login shared with another machine can stop renewing; runs then
fail with `Unsupported OpenAI model 'gpt-6.1-sol'` because the catalog shrinks.
`@ Trash doctor` reports the login state.
