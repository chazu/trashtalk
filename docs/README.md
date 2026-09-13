# Documentation index

Start with [installation and usage](../README.md), the [language reference](../LANGUAGE.md),
and [The Way of Trashtalk](the-way-of-trashtalk.md). Status labels distinguish
implemented contracts from designs and historical evidence.

## Current guides

| Topic | Reference |
| --- | --- |
| Compiler, supported syntax, and tests | [Capabilities](COMPILER_CAPABILITIES.md), [compiler internals](../lib/jq-compiler/README.md) |
| DSL recipes and design idioms | [Patterns](trashtalk-patterns.md), [The Way](the-way-of-trashtalk.md) |
| Cache, Store, save/reload, and transactions | [Persistence](persistence.md), [Assignment refactor](assignment-persistence-implementation.md) |
| JSON construction, reads, and traversal | [JSON values](json-values.md) |
| Files and subprocesses | [File](FILE.md), [Process](PROCESS.md), [Future class](FUTURE.md) |
| Persistent agent use, messages, stop, and recovery | [Agent operations](agent-operations.md) |
| Attach, backlog, composer, and detach | [Live session view](agent-session-view.md) |
| Harness contracts | [Jcode](jcode-session-driver.md), [Maki](maki-session-driver.md) |
| Manual durable work | [Assignments](assignments.md) |
| Code and session search / CLI harness adapters | [Tool adapters](code-and-session-tools.md) |
| Runtime/build behavior and dated measurements | [Performance](performance.md), [opt-in result passing](result-passing-design.md) |
| Packages and qualified names | [Namespace implementation/design](namespaces-design.md) |
| Removed APIs and cleanup evidence | [September cleanup](cleanup-2026-09.md) |

## Designs, research, and partial implementations

- [Workstation event, attention, and delegated-action layer](workstation-event-attention-delegation.md): proposed durable local subscriptions, grouped attention, and safely bounded delegated effects.
- [Delegation plan](agent-delegation-implementation.md): initial slices implemented;
  specialist dispatch, repository/local-copy/worktree objects, and shared memory remain work.
- [Headless sessions](headless-agent-sessions-design.md): partial implementation;
  consult current agent guides for available behavior.
- [Code intelligence and memory research](code-intelligence-and-memory-research.md):
  dated evaluation, not a claim of installed memory services.
- [Cue/mise brainstorm](cue-mise-brainstorm.md): current wrappers plus proposed uses.
- [Kubernetes package](kube-design.md): implemented package with historical design
  sketches and open questions.

## Historical material

These capture prior reasoning, not current installation or implementation instructions:

- [Native-to-Bash closure plan](archive/TRASHTALK_CLOSURE_PLAN.md)
- [Procyon block compilation](archive/block-compilation-plan.md)
- [Original JSON primitive plan](archive/json-primitives-plan.md)
- [Self-hosting/Procyon analysis](archive/self-hosting-evolution.md)
- [Original Innards design](archive/innards-ambient-ide.md) and
  [revised execution plan](archive/innards-ambient-ide-revised.md)
- [Ambient-session brainstorm](ambient-session.md), [dated direction review](ambient-agent-direction-review.md)
- [Concurrency refactor design](concurrency-refactor.md)
- [Legacy Twin guide](twin.md)
- [Assignment experiment measurements](assignment-transaction-measurements.json)

The jq compiler is canonical. There is no Procyon compiler/plugin or `tt` daemon
to build. Old proposals are preserved in Git and the archive so they need not
remain executable scaffolding in the runtime.
