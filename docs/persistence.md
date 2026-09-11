# Object persistence

**Status: current runtime contract.** SQLite Store holds durable records. A
file-backed session cache carries live objects across Bash command substitutions.
This cache is temporary; it is not an alternative persistence backend.

## Creation, mutation, and reads

`@ SomeClass new` allocates an ID, builds inherited defaults, and immediately
saves the initial record to Store. This applies even without `Persistable`.
Creation rolls back the cache entry if that initial save fails. Inside a Store
transaction the initial record is private until commit.

Generated setters and DSL field assignments change the session cache. They do
not automatically save later changes. `Persistable` supplies `save`, `reload`,
`unpersist`, `delete`, and class query methods:

```bash
counter=$(@ Counter new)          # initial defaults are already durable
@ "$counter" increment           # change the live session object
@ "$counter" save                # write the changed object
@ "$counter" reload              # replace cached data with durable data
@ Counter findAll                # IDs of stored Counter records
@ Counter find: 'value > 5'       # query stored state
```

`create` is a convenience method that calls `new` and then `save`; it does not
provide a distinct memory-only vs persistent creation mode.

`Runtime dataFor:` loads from Store when an object is absent from the session
cache, applying lazy schema migration. An already cached object is not refreshed
just because another process saved it. Use `reload`, or the guarded reads of a
Store transaction, when freshness matters. `Runtime setData:for:` only changes
the cache; `Store getInstance:` reads the durable JSON directly.

## Deletion

| Operation | Session cache | SQLite record |
| --- | --- | --- |
| `@ object unpersist` (Persistable) | Retained | Deleted |
| `@ object delete` (Persistable) | Deleted | Deleted |
| `@ Runtime delete: id` | Deleted | Retained; a later read can load it again |

Ordinary deletion is not a multi-object transaction. Other shells can still
hold cached copies. Explicit `save` can recreate a removed record. The runtime
retains its initializer fallback for handwritten and older compiled classes.

## Transactions

Use `@ Store transaction: receiver sending: selector with: argument` for a whole
domain operation. It supplies a private store and cache, imports accessed
records, records their original values or absence, and guards indexed query
membership. Commit checks those observations under a live write lock, then
publishes all staged records and queue rows together. Conflict or constraint
failure publishes neither data nor the operation's successful return value.

Any failed send poisons the transaction, even if the method later returns
success. Nested transactions, deletion, and arbitrary SQL are rejected. External
commands, notifications, and network calls cannot be rolled back; schedule
notifications after commit. Existing Assignment selection and completion permit
one read-only replay of a conflict, not arbitrary retry of side effects.

See [the implemented Assignment refactor](assignment-persistence-implementation.md)
for its boundaries and [the manual walkthrough](assignments.md) for a domain
example. `tests/test_persistable.bash` and `tests/test_assignment_transaction.bash`
exercise these contracts against the production runtime.
