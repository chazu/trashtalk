# Workstation v1 structural contracts

Vet `contracts.cue` together with exactly one `roots/<Definition>.cue` file.
The public feature boundary is `@ WorkstationSchema validate: "$json" as: EventSubscription`.
It uses `Tools::Cue vet:json:` outside Store transactions and reports only bounded
field paths, never fixture values. Missing CUE disables feature validation only.

Package digest, from the repository root:

```bash
{ cat schemas/workstation/v1/contracts.cue; for name in Attention CommandReceipt Coordinate Envelope EventSubscription SafeDisplay; do cat "schemas/workstation/v1/roots/$name.cue"; done; } | shasum -a 256
```

`@ WorkstationSchema digest` returns the same digest. It identifies the contract
on a feature policy revision, not a global runtime switch. CUE enforces closed
structure only. Store and native validators own identity, uniqueness and state.
Command receipt and display roots are future contracts, not a producer.

See [Phase 0 operations](../../../docs/workstation-operations.md) for installation,
record APIs, lifecycle recovery, coordinate replay, and browser/doctor integration.
