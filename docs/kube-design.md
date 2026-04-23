# Kube Package Design

A Trashtalk library for querying and observing Kubernetes cluster state. Lives in `package: Kube`.

## Goals

- Fetch Kubernetes resources via `kubectl` and represent them as first-class Trashtalk objects
- Persist point-in-time snapshots of resource state to SQLite for historical comparison
- Diff resources across time or across clusters/environments
- Keep the API idiomatic to Trashtalk — `@` message sends, traits, blocks, Persistable

---

## Class Hierarchy

```
Object
  └── Kube::Kubectl           # thin wrapper around kubectl CLI
  └── Kube::Cluster           # value object: context + default namespace
  └── Kube::Resource          # base class for all k8s resources (include: Snapshotable)
        ├── Kube::Pod
        ├── Kube::Deployment
        ├── Kube::ReplicaSet
        ├── Kube::Service
        ├── Kube::Node
        └── Kube::Event
  └── Kube::Snapshot          # a labeled, timestamped capture of N resources
  └── Kube::Diff              # result of comparing two snapshots or resources

Traits
  └── Snapshotable            # generalizes snapshot/history/diff for any class
```

---

## Instance Variable Strategy

Resources store the full raw kubectl JSON in `rawJson` and also unpack the fields
that are worth indexing or querying into normal instance variables. Both sets are
persisted via `Persistable`. The raw JSON is the source of truth for anything
not explicitly unpacked.

Example for `Kube::Pod`:

```
instanceVars: rawJson name namespace cluster kind capturedAt
              phase restartCount readyContainerCount totalContainerCount
```

`rawJson` is set once on construction and never updated — it's the snapshot of what
kubectl returned. The unpacked fields are derived from it at construction time.
Accessors for anything beyond the unpacked fields go through `@ self get: 'path.to.field'`
which does a `jq` extraction from `rawJson`.

---

## The `Snapshotable` Trait

`Snapshotable` generalizes the concept of "a class whose instances represent a
point-in-time capture of some external state." It is more specific than `Persistable`
(which just adds save/load) — it adds the notion of a timeline per logical identity.

### What it provides

- `capturedAt` is auto-stamped on `save`
- `history` class method returns all past captures ordered by `capturedAt`
- `latestFor: identity` returns the most recent snapshot matching a logical key
- `diffWith: other` compares two instances field-by-field
- `initializeSchema` hook called at class-load time to set up virtual columns and indexes

### Schema initialization and virtual columns

The trait provides a `initializeSchema` class method that subclasses can override to
declare which JSON paths should be materialized as indexed virtual columns. This
leverages `db_ensure_virtual_column` and `db_create_index` from sqlite-json.bash, both
of which are already available.

At class load time (via a `rawClassMethod: _onLoad` hook, to be added to the runtime),
`initializeSchema` is called once. If the columns already exist, the `ALTER TABLE`
is a no-op (SQLite returns an error that the trait suppresses).

```smalltalk
Snapshotable trait

  rawClassMethod: initializeSchema [
    pragma: direct
    local cols col name path
    cols=$(@ "$_RECEIVER" indexedColumns)
    while IFS= read -r col; do
      [[ -z "$col" ]] && continue
      name=$(echo "$col" | jq -r '.name')
      path=$(echo "$col" | jq -r '.path')
      db_ensure_virtual_column "$name" "$path" 2>/dev/null || true
      db_create_index "$name" 2>/dev/null || true
    done < <(echo "$cols" | jq -c '.[]')
  ]

  # Override in subclasses to declare columns to index
  rawClassMethod: indexedColumns [
    echo '[]'
  ]

  method: save [
    capturedAt := $(date -u +%Y-%m-%dT%H:%M:%SZ)
    @ self _persistableSave
  ]

  classMethod: history [
    ^ @ Store findByClass: self orderBy: 'capturedAt'
  ]

  classMethod: latestFor: identity [
    ^ @ Store findByClass: self where: "identity = '$identity'" limit: 1 orderBy: 'capturedAt DESC'
  ]

  method: diffWith: other [
    ^ @ Kube::Diff compareResource: self with: other
  ]
```

`Kube::Resource` then declares its indexed columns:

```smalltalk
rawClassMethod: indexedColumns [
  echo '[
    {"name": "kube_kind",      "path": "$.kind"},
    {"name": "kube_name",      "path": "$.name"},
    {"name": "kube_namespace", "path": "$.namespace"},
    {"name": "kube_cluster",   "path": "$.cluster"},
    {"name": "captured_at",    "path": "$.capturedAt"}
  ]'
]
```

This means queries like "give me all Pods in namespace X from cluster Y" hit indexed
columns rather than scanning JSON blobs.

---

## `Kube::Kubectl`

Thin wrapper. All methods are `rawClassMethod:` since they're just shell invocations.
Context defaults to current kubectl context if not provided.

```smalltalk
package: Kube

Kubectl subclass: Tool

  rawClassMethod: get: kind name: name namespace: ns context: ctx [
    local args="-o json"
    [[ -n "$4" ]] && args="--context $4 $args"
    [[ -n "$3" ]] && args="-n $3 $args"
    kubectl get "$1" "$2" $args 2>/dev/null
  ]

  rawClassMethod: list: kind namespace: ns context: ctx [
    local args="-o json"
    [[ -n "$3" ]] && args="--context $3 $args"
    [[ -n "$2" ]] && args="-n $2 $args"
    kubectl get "$1" $args 2>/dev/null | jq -c '.items[]'
  ]

  rawClassMethod: currentContext [
    kubectl config current-context
  ]

  rawClassMethod: listContexts [
    kubectl config get-contexts -o name
  ]

  rawClassMethod: namespaces: ctx [
    local args="-o json"
    [[ -n "$1" ]] && args="--context $1 $args"
    kubectl get namespaces $args 2>/dev/null | jq -r '.items[].metadata.name'
  ]
```

---

## `Kube::Cluster`

A lightweight value object — not a connection, just named parameters. Cascade-friendly
so you can configure it with semicolons.

```smalltalk
package: Kube

Cluster subclass: Object
  instanceVars: context namespace:'default'

  classMethod: named: ctx [
    | c |
    c := @ Kube::Cluster new
    @ $c setContext: $ctx
    ^ $c
  ]

  classMethod: current [
    | ctx |
    ctx := $(@ Kube::Kubectl currentContext)
    ^ @ Kube::Cluster named: $ctx
  ]

  method: withNamespace: ns [
    @ self setNamespace: $ns
    ^ self
  ]

  method: fetch: kind [
    | ns ctx rawLines result |
    ns := $(@ self getNamespace)
    ctx := $(@ self getContext)
    rawLines := $(@ Kube::Kubectl list: $kind namespace: $ns context: $ctx)
    result := $(@ Array new)
    @ $rawLines do: [:line |
      | resource |
      resource := $(@ Kube::Resource fromJson: $line cluster: $ctx)
      @ $result push: $resource
    ]
    ^ $result
  ]

  method: fetch: kind named: name [
    | ns ctx raw |
    ns := $(@ self getNamespace)
    ctx := $(@ self getContext)
    raw := $(@ Kube::Kubectl get: $kind name: $name namespace: $ns context: $ctx)
    (raw isEmpty) ifTrue: [^ ''].
    ^ @ Kube::Resource fromJson: $raw cluster: $ctx
  ]
```

---

## `Kube::Resource`

Base class. Subclasses add kind-specific accessor methods on top.

```smalltalk
package: Kube

Resource subclass: Object
  include: Snapshotable
  include: Persistable
  instanceVars: rawJson kind name namespace cluster capturedAt

  rawClassMethod: indexedColumns [
    echo '[
      {"name": "kube_kind",      "path": "$.kind"},
      {"name": "kube_name",      "path": "$.name"},
      {"name": "kube_namespace", "path": "$.namespace"},
      {"name": "kube_cluster",   "path": "$.cluster"},
      {"name": "captured_at",    "path": "$.capturedAt"}
    ]'
  ]

  rawClassMethod: fromJson: json cluster: clusterName [
    local id res
    id=$(_generate_instance_id resource)
    _create_instance Resource "$id"
    @ "$id" setRawJson "$1"
    @ "$id" setCluster "$2"
    @ "$id" setKind    "$(echo "$1" | jq -r '.kind')"
    @ "$id" setName    "$(echo "$1" | jq -r '.metadata.name')"
    @ "$id" setNamespace "$(echo "$1" | jq -r '.metadata.namespace // ""')"
    echo "$id"
  ]

  # jsonPath shorthand — reaches into rawJson
  rawMethod: get: path [
    local raw
    raw=$(_ivar rawJson)
    echo "$raw" | jq -r "$1"
  ]

  method: labels [
    ^ @ self get: '.metadata.labels'
  ]

  method: annotations [
    ^ @ self get: '.metadata.annotations'
  ]

  method: age [
    | created now delta |
    created := $(@ self get: '.metadata.creationTimestamp')
    # human-friendly age calculation via date arithmetic
    now := $(date -u +%s)
    delta := $((now - $(date -d "$created" +%s 2>/dev/null || date -j -f '%Y-%m-%dT%H:%M:%SZ' "$created" +%s)))
    (delta < 3600)   ifTrue: [ ^ "$((delta / 60))m"  ].
    (delta < 86400)  ifTrue: [ ^ "$((delta / 3600))h" ].
    ^ "$((delta / 86400))d"
  ]

  method: identity [
    | k ns n |
    k := $(@ self getKind)
    ns := $(@ self getNamespace)
    n := $(@ self getName)
    ^ "$k/$ns/$n"
  ]
```

---

## Kind-Specific Subclasses

These add ergonomic accessors. Each overrides `fromJson:cluster:` to also unpack
kind-specific fields into instance variables for the fields worth indexing.

### `Kube::Pod`

```smalltalk
package: Kube

Pod subclass: Resource
  instanceVars: rawJson kind name namespace cluster capturedAt
                phase restartCount readyCount totalCount

  rawClassMethod: fromJson: json cluster: clusterName [
    local id raw
    raw="$1"
    id=$(_generate_instance_id pod)
    _create_instance Pod "$id"
    @ "$id" setRawJson     "$raw"
    @ "$id" setCluster     "$2"
    @ "$id" setKind        "Pod"
    @ "$id" setName        "$(echo "$raw" | jq -r '.metadata.name')"
    @ "$id" setNamespace   "$(echo "$raw" | jq -r '.metadata.namespace // ""')"
    @ "$id" setPhase       "$(echo "$raw" | jq -r '.status.phase // "Unknown"')"
    @ "$id" setRestartCount "$(echo "$raw" | jq '[.status.containerStatuses[]?.restartCount // 0] | add // 0')"
    @ "$id" setReadyCount  "$(echo "$raw" | jq '[.status.containerStatuses[]? | select(.ready == true)] | length')"
    @ "$id" setTotalCount  "$(echo "$raw" | jq '.spec.containers | length')"
    echo "$id"
  ]

  method: ready [
    | r t |
    r := $(@ self getReadyCount)
    t := $(@ self getTotalCount)
    ^ (r == t) ifTrue: ['true'] ifFalse: ['false']
  ]

  method: containers [
    ^ @ self get: '[.spec.containers[].name]'
  ]

  method: summary [
    | ph r t |
    ph := $(@ self getPhase)
    r := $(@ self getReadyCount)
    t := $(@ self getTotalCount)
    ^ "$ph $r/$t restarts=$(@ self getRestartCount)"
  ]
```

### `Kube::Deployment`

```smalltalk
package: Kube

Deployment subclass: Resource
  instanceVars: rawJson kind name namespace cluster capturedAt
                replicas readyReplicas unavailableReplicas

  # fromJson: unpacks replica counts
  # method: rolloutStatus — available/progressing/degraded based on conditions
  # method: image — image of the first container
```

### `Kube::Node`

```smalltalk
package: Kube

Node subclass: Resource
  instanceVars: rawJson kind name cluster capturedAt
                ready unschedulable

  # method: conditions — Array of condition objects
  # method: taints
  # method: capacity / allocatable — from .status.capacity / .status.allocatable
```

---

## `Kube::Snapshot`

A labeled, timestamped set of resource IDs captured together. The relationship
between Snapshot and its Resources is maintained as a JSON array of resource IDs
in the `resourceIds` ivar — stored in the SQLite JSON blob and traversable via
`json_each()` in queries without a join table.

**Rationale for this approach:**
SQLite's `json_each()` lets you write queries like:
```sql
SELECT s.id FROM instances s, json_each(json_extract(s.data, '$.resourceIds')) r
WHERE s.class = 'kube_snapshot' AND r.value = 'pod_abc123'
```
This avoids a separate join table while still enabling cross-reference queries.
For the scale of data this library will handle (tens to hundreds of snapshots),
this is the right tradeoff. If you later need to query "which snapshots contain
pod X" at scale, a materialized join table is the upgrade path.

```smalltalk
package: Kube

Snapshot subclass: Object
  include: Persistable
  instanceVars: label cluster takenAt resourceIds

  classMethod: take: label of: kinds onCluster: ctx [
    | snap cluster resources |
    snap := $(@ Kube::Snapshot new)
    @ $snap setLabel $label
    @ $snap setCluster $ctx
    @ $snap setTakenAt $(date -u +%Y-%m-%dT%H:%M:%SZ)
    @ $snap setResourceIds '[]'

    cluster := $(@ Kube::Cluster named: $ctx)
    kinds do: [:kind |
      resources := $(@ $cluster fetch: $kind)
      @ $resources do: [:r |
        @ $r save
        @ $snap addResource: $(@ $r getId)
      ]
    ]
    ^ $snap
  ]

  classMethod: latest: label onCluster: ctx [
    ^ @ Store findByClass: self
        where: "label = '$label' AND cluster = '$ctx'"
        limit: 1
        orderBy: 'takenAt DESC'
  ]

  rawMethod: addResource: resourceId [
    local current new
    current=$(_ivar resourceIds)
    new=$(echo "$current" | jq --arg id "$1" '. + [$id]')
    _ivar_set resourceIds "$new"
  ]

  method: resources [
    | ids arr |
    ids := $(@ self getResourceIds)
    arr := $(@ Array new)
    @ $ids do: [:id | @ $arr push: $id]
    ^ $arr
  ]

  method: resourcesOfKind: kind [
    | all filtered |
    all := $(@ self resources)
    filtered := $(@ Array new)
    @ $all do: [:id |
      | r |
      r := $(@ Store getInstance: $id)
      ($(@ $r getKind) = $kind) ifTrue: [@ $filtered push: $id]
    ]
    ^ $filtered
  ]
```

---

## `Kube::Diff`

Field-level diff between two resources or two snapshots.

```smalltalk
package: Kube

Diff subclass: Object
  instanceVars: added removed changed summary

  classMethod: compareResource: a with: b [
    | diff rawA rawB patch |
    diff := $(@ Kube::Diff new)
    rawA := $(@ $a getRawJson)
    rawB := $(@ $b getRawJson)
    patch := $(echo "$rawA" "$rawB" | jq -s '
      [ path(.[1] | .. | scalars) as $p
        | select(getpath($p) as $v1 | (.[0] | getpath($p)) != $v1)
        | {path: ($p | join(".")), from: (.[0] | getpath($p)), to: (.[1] | getpath($p))}
      ]' )
    @ $diff setChanged $patch
    ^ $diff
  ]

  classMethod: compare: snapA with: snapB [
    # Aligns resources by identity (kind/namespace/name), then diffs each pair
    # Returns a Diff with added (in B not A), removed (in A not B), changed (in both, different)
  ]

  method: report [
    | changes |
    changes := $(@ self getChanged)
    echo "$changes" | jq -r '.[] | "\(.path): \(.from) → \(.to)"'
  ]

  method: hasChanges [
    | c |
    c := $(@ self getChanged)
    ^ (echo "$c" | jq 'length > 0')
  ]
```

---

## Usage Examples

### Inspect current cluster

```bash
source lib/trash.bash

cluster=$(@ Kube::Cluster current)
pods=$(@ $cluster withNamespace: 'kube-system'; fetch: 'pods')
@ $pods do: [:p |
  @ Console print: "$(@ $p getName): $(@ $p summary)"
]
```

### Take and save a snapshot

```bash
snap=$(@ Kube::Snapshot take: 'daily' of: #('pods' 'deployments') onCluster: 'prod-us-east')
@ $snap save
```

### Compare two environments

```bash
prod=$(@ Kube::Snapshot latest: 'daily' onCluster: 'prod-us-east')
staging=$(@ Kube::Snapshot latest: 'daily' onCluster: 'staging-us-east')
diff=$(@ Kube::Diff compare: $prod with: $staging)
@ $diff report
```

### Parallel snapshot across clusters

```bash
f1=$(@ Future for: '@ Kube::Snapshot take: "daily" of: "pods" onCluster: "prod"')
f2=$(@ Future for: '@ Kube::Snapshot take: "daily" of: "pods" onCluster: "staging"')
@ $f1 start
@ $f2 start
snap1=$(@ $f1 await)
snap2=$(@ $f2 await)
@ $f1 cleanup
@ $f2 cleanup
```

### Query history for a specific pod

```bash
# All snapshots of pods named "api-server" in namespace "default" on prod
ids=$(db_query "kube_kind = 'Pod' AND kube_name = 'api-server' AND kube_namespace = 'default' AND kube_cluster = 'prod-us-east'")
```

---

## Build Order

1. `Snapshotable` trait (depends on: sqlite-json.bash functions already available)
2. `Kube::Kubectl` (no dependencies)
3. `Kube::Cluster` (depends on: Kubectl)
4. `Kube::Resource` (depends on: Snapshotable, Persistable, Cluster)
5. `Kube::Pod`, `Kube::Deployment`, `Kube::Node`, `Kube::Service` (depend on: Resource)
6. `Kube::Snapshot` (depends on: Resource, Persistable)
7. `Kube::Diff` (depends on: Resource, Snapshot)

---

## Open Questions

- **`_onLoad` hook**: `Snapshotable#initializeSchema` needs to fire at class load time.
  The runtime would need a `_onLoad` mechanism, or we call it explicitly in each
  class's file after the class definition. The explicit call is simpler for now.

- **`Store#findByClass:where:orderBy:limit:`**: The `Persistable` trait's `find:` method
  calls `Store findByClass:where:` but the current `Store` may not support `orderBy:`
  or `limit:`. These are needed for `Snapshotable#latestFor:` and `Snapshot#latest:`.
  Worth adding to `Store` before starting implementation.

- **`kinds` parameter in `Snapshot#take:`**: The example passes `#('pods' 'deployments')`.
  Need to decide if this is an Array literal, a comma-separated string, or something else.
  Array is cleanest but requires Array literals to work correctly at the call site.
