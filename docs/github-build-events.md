# GitHub build events over the tailnet

`GitHub::BuildEvents` imports build events from the Loosh cluster into Honker's
durable `github.builds.v1` Stream. The GitHub App covers all repositories under
`chazu` and `loosh-industries`: Actions workflow/job events, check runs/suites,
and commit statuses. No local incoming port, GitHub credential, or agent is needed.

```bash
make bash
source lib/trash.bash
@ GitHub::BuildEvents sync
consumer=$(@ GitHub::BuildEvents consumerNamed: 'my-build-watcher')
@ "$consumer" read: 10
```

Each Stream record has its own local `offset` and JSON `payload`. The payload
contains `repository`, `kind`, `action`, `status`, `conclusion`, `name`, `sha`,
`url`, `delivery_id`, and the cloud `sequence`/`received_at`. The cloud sequence
and local Stream offset are different coordinates. Acknowledge the **local
offset**, and only after your handler succeeds:

```bash
records=$(@ "$consumer" read: 10)
while IFS= read -r record; do
  payload=$(jq -c '.payload | if type=="string" then fromjson else . end' <<<"$record")
  # Example subscription: report only completed workflow runs. Replace the
  # print command with your handler; never execute event text as shell code.
  if jq -e '.kind=="workflow_run" and .action=="completed"' <<<"$payload" >/dev/null; then
    jq -r '"\(.repository): \(.name) -> \(.conclusion) \(.url)"' <<<"$payload" || break
  fi
  @ "$consumer" acknowledgeThrough: "$(jq -r .offset <<<"$record")" || break
done < <(jq -c '.[]' <<<"$records")
```

Use distinct consumer names for independent subscribers. Handles and offsets
survive process restarts in `SQLITE_JSON_DB` (normally `~/.trashtalk/instances.db`).
`initializeFrom: 'from-now'` skips existing history only when first registering
a consumer; the default reads from the start. Processing may repeat if a handler
succeeds but the process crashes before its acknowledgement, so handlers with
external side effects should use `delivery_id` as an idempotency key.

For automatic polling on macOS:

```bash
bin/trash-github-events-service install
bin/trash-github-events-service start
bin/trash-github-events-service status
# To disable polling:
bin/trash-github-events-service stop
```

The launchd timer runs `bin/trash-github-events` every 15 seconds while logged
in. Each poll imports up to 100 records and has a 12-second HTTP timeout. On
Linux, run that command from a user systemd timer. Logs are under
`run/github-events/`; the command prints the fetched batch size. Tailscale must
be connected and permit HTTPS to `github-events.tail7fd374.ts.net`.

Cloud events are retained for 30 days. The importer atomically publishes the
whole validated batch and advances its cloud cursor in one SQLite transaction;
duplicate delivery IDs do not republish. Competing pollers use a checkpoint
comparison, so the loser fails safely and can retry. There is no second local
event log: Honker stores the messages; bridge tables hold only cursor/dedup state.

Network failures leave the cursor unchanged. A retention gap also fails without
advancing: inspect `GitHub::BuildEvents cursorFor:` and the endpoint's
`retention_floor`, recover any required missing history, then deliberately repair
the source checkpoint in `_github_build_sources`. Do not silently discard the
gap. Keep the local SQLite database backed up; local events/dedup receipts are
not automatically pruned.

The cloud receiver acknowledges only committed events, but GitHub itself does
not automatically retry failed webhook deliveries. Cluster outages require
redelivery from the GitHub App's Advanced settings. Build payloads remain data;
this bridge does not send Inbox messages, wake agents, or execute builds.
