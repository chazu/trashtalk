# Pronounceable object handles

Status: proposed design, not implemented.
Date: 2026-10-04.

## Decision

Add stable proquint handles for people. Keep existing object IDs as canonical identity.

A proquint encodes 16 bits as a pronounceable five-letter word, such as `duson`.
Three words encode 48 bits. A proposed handle looks like:

```text
agentrun_duson-latim-fupor
```

Use handles in conversation and interactive object views. Keep canonical IDs in stored references, protocol payloads, and existing machine output.

Do not replace UUIDs or rewrite object references. Do not ship handle resolution without collision checks and transaction support.

## Problem and current behavior

Trashtalk generates object IDs as a lowercase class prefix followed by a UUID:

```text
agentrun_1140816F-F301-435B-B323-BBBD29C5495B
```

These IDs are easy to copy but hard to read aloud, remember, compare, or type.
Agent runs, sessions, assignments, and browser selections expose this problem frequently.

The runtime already depends on these IDs beyond SQLite keys:

- `_generate_instance_id` in `lib/trash.bash` allocates the ID through `Runtime generateId:`.
- `Object new` initializes the object and immediately persists its initial record.
- `_resolve_receiver` resolves classes and object receivers at the public send boundary.
- The session cache uses canonical IDs as filenames. Prefix operations identify groups of cached objects.
- Stored references, run directories, and external integrations carry canonical IDs.

The Store accepts custom IDs as well as generated IDs. A design cannot assume every object has a UUID suffix.

See [object persistence](persistence.md) for creation, deletion, cache, and transaction contracts.

## Goals and limits

Provide stable, pronounceable references that people can copy, type, and say aloud.
Accept existing IDs without behavior changes. Preserve class dispatch and custom Store IDs.
Keep canonical-ID sends on their current fast path.
Make collisions, deletion, concurrency, and restoration explicit.

This design does not provide semantic names, fuzzy matching, global cross-store identity, or authentication.
Proquints improve pronunciation and recognition. They do not guarantee memorability.

## Handle format

Use the existing lowercase class prefix and three hyphen-separated proquint words.
Namespaced classes use the existing prefix convention, such as `myapp_counter`.
Read the prefix from class metadata, not by splitting an arbitrary custom ID.

Each word follows consonant-vowel-consonant-vowel-consonant. The standard alphabets are:

```text
consonants: b d f g h j k l m n p r s t v z
vowels:     a i o u
```

Encode each unsigned 16-bit chunk from its most significant bits in groups of 4, 2, 4, 2, and 4.
Preserve leading zero bits. Join three chunks in their input byte order.
For example, the 16-bit value zero encodes as `babab`.

Emit lowercase ASCII with ordinary hyphens. Require the full class-prefixed handle for resolution in the first version.
Do not accept bare words, partial handles, or omitted hyphens.

Do not silently lowercase arbitrary canonical IDs. They remain exact strings.
Initially accept only the canonical lowercase spelling of a handle.

The class prefix aids recognition. It is not proof of the object's class or an authorization boundary.
Existing namespace flattening can produce shared prefixes. The uniqueness constraint must cover the entire handle string.

## Allocation and collision policy

Generate six random bytes using an OS randomness source. Encode them as three proquints.
Do not use Bash `$RANDOM`, timestamps, or process IDs as the randomness source.

Reserve the candidate in the Store with a unique constraint. Retry only a uniqueness collision, with fresh random bytes.
Use a bounded retry limit of eight. Report other database or randomness failures immediately.

Do not derive an unchecked short handle from a UUID substring. A short encoding is not automatically unique.
Random allocation also supports custom IDs without inventing a separate UUID parsing contract.

The collision space per shared class prefix is 2^48. Before retries, the approximate chance of at least one collision is:

```text
p ≈ 1 - exp(-n(n-1) / (2 × 2^48))

10,000 objects:     about 0.000018%
100,000 objects:    about 0.0018%
1,000,000 objects:  about 0.18%
```

These figures describe candidate collisions, not duplicate assigned handles. Database constraints prevent duplicate assignment.
Count retained reservations, including deleted objects, when estimating occupancy.

## Persistent registry

Keep the mapping outside object instance variables. Saving an old cached object must not overwrite its handle.

Proposed schema:

```sql
CREATE TABLE object_handles (
    handle TEXT PRIMARY KEY,
    object_id TEXT NOT NULL UNIQUE,
    format_version INTEGER NOT NULL CHECK (format_version = 1),
    created_at TEXT NOT NULL
);
```

The registry belongs to the same Store as the objects and travels with its backups.
A full handle is unique within that Store. Different Stores can assign the same handle to different objects.

Do not cascade registry deletion when an object is removed. Retain the reservation permanently to prevent reassignment.
An unresolved reservation means that no accessible object currently exists under its canonical ID.
If the same canonical ID is saved again, reuse its existing handle.

Deletion and `unpersist` can leave live cached objects. A handle can resolve to such an object while it remains accessible in that session.
Do not promise that a deleted record is unavailable in all shells. That is not the current persistence contract.

## Public interfaces and resolution

Proposed messages, not currently available:

```bash
@ "$canonical_id" handle
@ Runtime canonicalIdFor: 'agentrun_duson-latim-fupor'
@ Runtime handleFor: "$canonical_id"
@ 'agentrun_duson-latim-fupor' show
```

`handleFor:` returns or allocates the object's stable handle. Reject unknown objects.
`canonicalIdFor:` accepts an existing canonical ID or registered handle and returns a canonical ID for an accessible object.
`Object handle` delegates to the runtime service.

Resolve receivers in this order:

1. Preserve the current reserved-class and exact-object resolution behavior.
2. If exact resolution fails, recognize a complete handle and query its registry mapping.
3. Load the mapped object using its canonical ID and the existing cache/Store rules.
4. Report an unknown or unavailable handle instead of treating it as a new class name.

Preserve exact custom-ID precedence. Reject allocation if a candidate equals an existing canonical ID or reserved receiver.
A future custom-ID write must also reject an ID that equals another object's reserved handle.
Enforce this rule at the Store boundary, not only during handle allocation.
Check both namespaces and publish writes under the same SQLite write lock, including transaction commit.
Audit direct writers before enabling handles. Exact-match precedence alone does not prevent shadowing.

Normalize the receiver before setting runtime instance context. Cache keys and `self` remain canonical IDs.
Do not create a second cached object under the handle.

Do not rewrite every string argument to a message. Object-valued parameters need explicit resolution at their API boundaries.
Publish an inventory of handle-aware entry points. Receiver support alone does not make every ID-taking API handle-aware.

Handle lookups and exact-ID checks must use bound or safely escaped SQL values.
Do not interpolate unchecked user input into queries or filenames.

## Creation and transactions

For new objects, persist the initial record and registry reservation atomically in the same Store transaction.
Return the existing canonical ID from `new`. If either write fails, publish neither record nor handle.
Remove the temporary cache entry on creation failure, matching the existing rollback contract.

For existing objects, allocate lazily when a person requests a handle or an interactive view needs one.
Concurrent requests for the same object must return the same winning reservation.
Distinguish an `object_id` conflict from a candidate-handle conflict before retrying.

Handle allocation inside a Store transaction must stage registry writes and validate uniqueness at commit.
Handle reads must observe staged mappings and participate in the transaction's guarded reads.
Do not reserve a handle in the live database while its object exists only in a private transaction.

The transaction implementation currently stages object records in a private Store.
Extend its schema, read guards, and commit publication before enabling handle allocation in transactions.
Use the existing outer transaction when creating objects inside a domain operation. Do not introduce unsupported nested transactions.

The registry is not a disposable cache. Use the same durable migration discipline as other Store tables.
Avoid cross-shell resolver caches in the first implementation. Measure before adding them.

## Display and compatibility

Interactive views can show a handle plus class and object summary. Include a full-ID copy action or detail field.
Human-facing notifications can use handles after the recipient's Store context is clear.

Keep these outputs unchanged by default:

- `new`, `create`, `findAll`, and existing query result IDs.
- JSON `id` fields and stored object references.
- Assignment/session lifecycle payloads and run directory names.
- Private run launchers and authority-bearing interfaces.

Where useful, add a distinct `handle` field without changing `id`.
Provide explicit human-output mode for text commands that scripts already consume.

A handle is a public locator, not a credential. Resolve it within the existing authority and Store context.
Never construct or select a private launcher solely from a handle.

Back up the registry with objects. Restoring objects without the registry can assign different handles.
Export/import across Stores is outside the initial contract. Reject conflicting imported mappings rather than rebinding an existing handle.
Older readers can ignore the additional table. Once handles are enabled, old writers are not supported because they bypass reservation checks.
Require writer version coordination before rollout. Disabling display is not permission to resume incompatible writers.

## Alternatives

### Replace UUIDs with three-word IDs

This gives one identifier but changes persistence keys, cached filenames, stored references, and external protocols.
It also makes local registry uniqueness the identity guarantee. Reject this migration for the initial design.

### Encode the full UUID as proquints

A 128-bit UUID requires eight five-letter words, or 47 characters including hyphens, before the class prefix.
This is reversible without a registry but longer than a UUID. It improves pronunciation, not brevity.

### Short UUID prefixes

These are compact and familiar to developers. They remain hard to say aloud and need explicit ambiguity handling.
They can complement search later but do not address the main usability problem.

### Two-word proquints or chosen names

Two words provide only 32 bits. Candidate collisions become common sooner, though enforced reservations still prevent duplicate assignment.
Chosen names provide meaning but require naming, renaming, and conflict rules. Keep them separate from stable handles.

## Delivery plan

1. Implement the codec and reservation service with isolated tests and fixed encoding vectors.
2. Add registry migration, direct-writer checks, and transaction staging/commit support.
3. Add explicit handle messages and public receiver resolution without changing machine outputs.
4. Add handles to one interactive object view and agent notifications with full-ID access.
5. Measure creation and lookup costs, then decide whether to expand display defaults.

Keep arithmetic and OS integration in narrow runtime primitives. Expose behavior through Trashtalk messages where the DSL supports it.
Avoid a new external process on ordinary canonical-ID sends. Batch handle assignment for list views instead of querying once per row.

## Acceptance tests

### Encoding and allocation

Verify fixed vectors, leading zeros, byte order, round trips, and malformed input rejection.
Force candidate collisions and verify retries. Force randomness failure and verify no object or partial reservation is published.
Run concurrent same-object and different-object allocation tests against an isolated Store.

### Identity and dispatch

Verify handles and canonical IDs reach the same object and use one canonical cache entry.
Verify class dispatch, custom IDs, namespaced prefixes, and reserved receiver behavior remain unchanged.
Reject attempts to create custom IDs that shadow reserved handles.
Verify object-valued parameters remain canonical unless their APIs explicitly support handles.

### Lifecycle and transactions

Verify deletion retains reservations and cannot redirect an old handle to a different canonical ID.
Verify `unpersist`, cache-only deletion, reload, and re-saving the same ID follow current persistence rules.
Verify transaction abort, conflict, and uniqueness failure publish neither staged objects nor reservations.
Verify backup/restore preserves handles and missing-registry restoration is documented as a loss of handle continuity.

### Compatibility and cost

Verify existing machine outputs and stored references are unchanged.
Verify unsupported old writers cannot bypass registry invariants after rollout.
Run `make verify` for runtime changes. Record before/after costs for creation, canonical sends, handle sends, and interactive lists.
Set rollout performance thresholds from measurements before changing display defaults.

## Open decisions

The initial proposal uses three words and Store-local scope. Validate the word count with representative agent-session and assignment workflows.
Choose the first interactive view after testing typed, copied, and spoken handles with the user.
Record the measured overhead and acceptable thresholds before enabling allocation for every newly created object.
