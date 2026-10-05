# omlx-pi

Gusgus on the pi coding agent, with every model served locally by omlx and
Hindsight as long-term memory. Written on an M5 Pro with 64 GB; the omlx numbers
come from the tuning log in `~/sci/omlx-tune/RESULTS.md`.

## What it sets

| File | Effect |
| --- | --- |
| `trashtalk.config` | `gusgus.profile = "pi"`, `pi.model = "omlx/Qwen3.6-35B-A3B-4bit:low"` (the `:low` suffix pins thinking for Gusgus runs), plus the jcode keys used when the profile is switched back |
| `omlx/settings.json` | `sampling.max_tokens` 16384 (large write and edit tool calls exceed the 2048 default and error), `idle_timeout_seconds` 1800, `ssd_cache_max_size` 40GB, `max_concurrent_requests` 1 (raising it did not help and did not save memory) |
| `omlx/model_settings.json` | The Qwen3.6-35B agent model: sampling t1.0 / top_p .95 / top_k 20, 30-minute TTL, 4096-token thinking budget. Pinned: Qwen2.5-Coder-3B (autocomplete) and Qwen3-Embedding-0.6B. Gemma and the larger helpers unload after 5 to 10 minutes. |
| `omlx/model_profiles.json` | `gemma-4-e4b-system1`: temperature 0, 64 tokens, thinking off, for typed decisions |
| `pi/settings.json` | omlx as the default provider and model, thinking `low` |
| `hindsight/config.json` | The Hindsight API is on port 8888 (9999 is the Control Plane UI), bank `poop` |
| `patches/hindsight-pi-bank-config.patch` | `@walodayeet/hindsight-pi` 0.4.0 calls the bank-profile endpoint that Hindsight 0.10 removed. The patch uses the bank config endpoint instead. |
| `hindsight/docker-compose.yml` | The server, using the same omlx model; not applied by `trash-machine` |

Requirements: omlx with the models above, and pi with `npm:pi-omlx-picker`
(`pi install npm:pi-omlx-picker`). The picker is what sends `enable_thinking`
to omlx, so keep it loaded if you set `pi.extensionPaths`.
`trash-machine` prints a note when it is missing.

omlx settings take effect when oMLX restarts. Gusgus sessions keep the profile
they were created with: run `@ Gusgus fresh: "$PWD"` for one on pi.

## Hindsight server

`trash-machine` does not touch Docker. The compose file here has the project
name and external volume of the original `~/docker-compose.yml`, so it adopts the
running container and its data:

```bash
docker compose -f machines/omlx-pi/hindsight/docker-compose.yml up -d hindsight
```

Two settings in it matter on omlx. `HINDSIGHT_API_LLM_MODEL` must name a model
omlx still has, or every fact extraction fails with a 404. And
`HINDSIGHT_API_LLM_STRICT_SCHEMA_CONSOLIDATION=true` makes consolidation request
a grammar-enforced `json_schema`: in Hindsight's default soft `json_object` mode,
Qwen3.6 on omlx answers with a list like `[86701512]` instead of the JSON object
and consolidation fails with a validation error.

omlx runs one request at a time, so extraction and consolidation queue behind
Gusgus's turns on the same 35B model.
