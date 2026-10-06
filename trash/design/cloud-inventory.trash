# Cloud Inventory

Schema and validation layer for tracking cloud resources across providers.

## Architecture

    User:
      trash/cloud-inventory
      --> @ CloudInventory (Trashtask class)
          --> cue (CLI tool, standalone)
              --> inventory.cue  (schema — defines structure)
              --> inventory.json (data  — actual inventory)
              --> jq       (filtering — type, state, freeform)

Cue handles validation and provider-level querying. jq handles filtering on type,
state, and other dimensions. Data lives in `~/.cloud-inventory/`, separate from
the Trashtalk repository.

## Components

### CloudInventory (Trashtalk class)

Main entry point. Wraps the `cue` CLI.

- `add(providerType:data:)` — Import raw JSON from a provider (AWS CLI, GCP CLI,
  etc.) and store in `~/.cloud-inventory/<provider>.json`.
- `list(providerType:)` — Return all resources for a provider via `cue eval -e
  <provider>.resources` or `cue eval -e <provider>.zones[]`.
- `list(type:resourceType:)` — Reusable resources from Cue and filter by resource
  type through jq.
- `list(state:stateType:)` — Reusable resources from Cue and filter by resource
  state through jq.
- `validate()` — Run `cue vet` on all data files against the schema. Returns
  unvalidated resources.
- `export(format:)` — Output as JSON or cue.
- `sync(providerName)` — Run provider CLI, parse output, merge into inventory.

### CloudInventorySchema (Trashtalk class)

Manages Cue schema files.

- `generate(providerNames:)` — Write schema `.cue` file(s) for given providers.
- `update(schema:)` — Modify schema (add providers, resource types, states).

## Cue Schema

```
aws?: {
  account: string
  region:  string
  resources: [...{
    id:    string
    type:  string
    name:  string
    state: string
    tags?: { [string]: string }
  }]
}
```

Providers are optional (`?` suffix) so data for one provider doesn't fail validation
for another. Each provider has a top-level key with metadata (account, region, etc.)
and a list of resources, each with id, type, name, state, and optional tags.

## Cue CLI Usage Notes

Verified with Cue v0.17.1. Behavior:

| Command | Works? | Notes |
|---|---|---|
| `cue eval aws.resources` | ✅ | Lists all AWS resources |
| `cue eval gcp.zones[]` | ✅ | Lists GCP zones |
| `cue eval -e gcp.zones[].items[]` | ❌ | Nested array indexing unsupported in `-e` |
| `cue vet schema.cue data.json` | ✅ | Validates JSON against schema |
| `cue export schema.cue data.json` | ✅ | Full JSON output |
| `cue export -e aws.resources[?(..)].id` | ❌ | JSONPath queries unsupported |
| `cue export -e gcp.zones[]` | ✅ | Zone-level path queries work |

**Implication for Trashtalk:**
- Cue provides provider-level queries (list AWS resources, list GCP zones).
- Filtering by type, state, or other resource attributes goes through jq, which
  gets the data and applies its filters.
- No single-tool end-to-end solution; two tools, clear boundaries.

## Data Layout

```
~/.cloud-inventory/
  inventory.cue    — schema
  inventory.json   — current inventory
  aws.json         — raw AWS data
  gcp.json         — raw GCP data
  azure.json       — raw Azure data
```

## Testing

Schema compiles, validates, and queries correctly:

```bash
# Validate JSON against schema
cue vet inventory.cue inventory.json

# List AWS resources
cue eval aws.resources inventory.cue inventory.json

# Full JSON export
cue export inventory.cue inventory.json
```
