# The Way of Trashtalk

**Status: current idiom guide.** Prefer ordinary message sends and small domain
objects. The [language reference](../LANGUAGE.md) documents syntax; these
principles describe how to use it.

## Put behavior where the responsibility lives

Use `method:` and `classMethod:` for validation, policy, state transitions, and
composition. A caller should say what it wants through the public selector,
without knowing the storage format or the command used to perform it.

`Assignment` is a durable responsibility record. `AgentIdentity` owns work;
`AgentSession` is a temporary execution context. Splitting a large class does
not require splitting that record into more persistent objects.

## Factor capabilities into traits and collaborators

Traits provide behavior on the existing receiver. `Assignment::Reporting`
records progress and questions; `Assignment::Authority` checks the actor;
`Assignment::Presentation` derives views. They share the Assignment record.
`Assignment::Participation` instead provides class methods that coordinate
session selection and run participation over that record.

Use a [class cluster](https://developer.apple.com/library/archive/documentation/General/Conceptual/CocoaEncyclopedia/ClassClusters/ClassClusters.html)
when interchangeable implementations need one public construction interface.
Do not introduce another object solely to hold a few parameters: local values
or a JSON request often suffice.

## Keep temporary data as values

Build argv and records with typed JSON literals rather than allocating durable
Array/Dictionary builder objects:

```smalltalk
argv := #(tool '--title' title) asJson.
request := #{objective: objective context: (context jsonValue)} asJson.
```

Dynamic leaves are strings unless explicitly typed with `jsonValue`. Read with
`jsonAt:` for encoded JSON and `jsonTextAt:` for decoded text. Distinguish a
missing field from a present `false` or `null` value. See [JSON values](json-values.md).

## Make persistence and effects explicit

Ordinary `new` saves an initial record. Subsequent field mutations update the
session cache; `Persistable save` writes them to Store. Use `reload` when durable
state, rather than a cached view, is authoritative.

An Assignment operation wraps its domain changes in one `Store transaction:`;
ordinary saves remain private until commit. `AgentQueue` publishes the outbox
and sends wake hints after commit. The hint prompts an agent to read its Inbox;
it does not substitute for the authoritative message or settle its delivery.
See [persistence](persistence.md) for the cache and transaction boundaries.

## Keep raw code at the boundary

External programs go behind `Tool` subclasses. Build exact argv in the DSL and
use shared execution/capture primitives. Raw methods own unavoidable shell,
filesystem, SQLite, or serialization work; keep them small and explain the
boundary in a comment. Repeated raw patterns belong in a reusable primitive.

A failure is not an automatic return from a DSL method. Use an explicit checked
return where needed, preserve external exit status, and avoid producing a
success-shaped result after an effect fails. Transactions reject a commit after
any failed send, but that does not make external effects transactional.

## Verify through the public surface

Compile production source and test ordinary `@` sends. Use isolated stores,
scratch files, and fixture executables for deterministic checks. Keep optional
live tests visibly opt-in, and distinguish those results from fixture evidence.
