# StateMachine trait

**Status: implemented.** [StateMachine](../trash/traits/StateMachine.trash) is a
DSL trait for validating directed state transitions. It can be included with or
without `Persistable`; it has no instance variables and creates no helper object.

## Contract

The including class supplies `transitionRules`, returning a JSON array of
allowed `from:to` pairs. For example:

```smalltalk
method: transitionRules [
  ^ '["idle:running","running:completed","running:failed"]'
]
```

Call `@ object requireTransitionFrom: 'idle' to: 'running'` to validate an edge.
The method returns the target state when allowed and signals `TransitionError`
otherwise. It uses the states supplied by the caller: it does not read a current
state field, change state, save an object, authorize an actor, or run effects.
Even a transition back to the same state requires an explicit rule.

The host's transition operation reads its current state, validates the edge,
checks any domain guards, and applies the change. The host chooses its state
field name and its persistence and concurrency boundaries.

## Persistence

An object's state is persistent when the host stores it in its durable record
and writes each change. Including `StateMachine` neither enables nor disables
that behavior.

| Host operation | Result |
| --- | --- |
| Call `requireTransitionFrom:to:` only | Validates the supplied edge; no state changes. |
| Validate and assign a state field | Changes the session cache; the later change is not automatically saved. |
| Validate, assign and `save` | Writes the changed state to Store; it survives a reload. |
| Validate and write with guarded persistence | Also rejects conflicting observations or stale state, according to the host's transaction or compare-and-set operation. |

The trait works with cached state changes without requiring a save. That does
not introduce a new memory-only allocation mode: Trashtalk's `new` already
stores initial defaults, even without `Persistable`. Subsequent mutations and
explicit saves follow the ordinary [persistence contract](persistence.md).

For concurrent durable work, validation and mutation must share the host's
guarded persistence boundary. A separate validation followed by an unguarded
save does not prevent another process changing the state between those steps.

## Current hosts

| Host | Durable transition operation |
| --- | --- |
| [Agent::Run](../trash/Agent/Run.trash) | `transitionTo:with:` reads current state, validates the edge, then conditionally updates the record only if the stored state still matches. |
| [Agent::Delivery](../trash/Agent/Delivery.trash) | `transitionTo:` validates the edge and conditionally updates the stored state. |
| [Assignment](../trash/Assignment.trash) | `complete:` and `cancel:` validate and write their outcome, delivery settlement and result publication in a guarded Store transaction. |

These objects retain separate state spaces. A successful Run does not complete
its Assignment; the Assignment needs an explicit outcome. Activity such as
queued, running or waiting is derived separately from Assignment lifecycle.

The trait does not wrap methods with AOP advice. Required effects stay in
explicit domain operations so their transaction and recovery boundaries remain
visible. Optional tracing may use advice without owning those effects.
