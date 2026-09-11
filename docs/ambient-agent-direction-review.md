# Ambient Agent Direction Review

**Date:** 2026-09-09

**Status:** Historical assessment of the dated baseline below. Live session attachment has since been implemented; see agent-session-view.md. Remaining recommendations have not all been adopted.

**Implementation baseline:** Trashtalk `8388cb7`, Innards `42411ac`, and the
local Whisker inbox integration.

## Assessment

We should revise the goal and rewrite the remaining roadmap. Most architectural
principles still hold. What has changed is the product's center: durable agents,
inboxes, and prompt awareness are now the main experience, while the original
documents treated them as later additions to an inline IDE.

This assessment covers the [ambient-session sketch](ambient-session.md), the
[original Innards plan](archive/innards-ambient-ide.md), the
[revised Innards plan](archive/innards-ambient-ide-revised.md), and the
[headless-session design](headless-agent-sessions-design.md). They now describe
several different stages of the project. The
[agent operations guide](agent-operations.md) describes the implemented worker
and browser contracts.

## Proposed goal

> Make durable, addressable agents part of ordinary shell work: give them work,
> continue with yours, notice when they need attention, and inspect, reply, or
> review without entering or maintaining a separate application.

This gives the components clearer jobs:

- **Trashtalk:** objects, messages, durable work, routing, and workflow.
- **Whisker:** awareness at the prompt.
- **Innards:** temporary inspection, conversation, editing, and review.
- **External harnesses:** reasoning, tool execution, and conversation continuity.

The editor/browser work remains useful infrastructure. The recent inbox,
shortcut, and message-formatting work demonstrates that managing attention and
conversation deserves equal weight with execution.

## Assessment of unfinished work

| Area | Assessment |
|---|---|
| Durable event subscriptions and settled consumer cursors | **Still valid.** This is the biggest missing step from a message-driven assistant to an agent that responds to its environment. Start with one real watcher workflow. |
| Cross-session conversation history and causal links | **Still valid; move earlier.** The current views make messages readable, but do not provide a complete explanation of which work caused which message, question, or result. |
| Questions, approvals, and attributable actions | **Still valid; sharpen the contracts.** These need stronger relationships than ordinary message text and unread status. |
| Live session focus | **Keep the user experience; reconsider the mechanism.** Existing inpick/inpage composition already provides useful focus. A new `inagent` binary and duplex bridge should follow a demonstrated need for live interaction. |
| Role enforcement, message limits, and effect deduplication | **Genuine remaining work.** Having policy fields and authenticated run selectors is only part of the contract. |
| Shared Process lifecycle implementation | **Still useful, but separate it from the product roadmap.** Consolidate around concrete duplication and failures rather than making every background abstraction migrate before the next useful feature. |
| More harnesses, live input, cache telemetry | **Valid options, weaker release prerequisites.** A second harness should solve a concrete problem; cache measurement should improve efficiency without blocking a usable single-driver product. |
| Generic stream applet, distributed fencing, parallel session runs | **Keep deferred.** These require explicit workload or deployment needs. The current single-host scope is a reasonable product boundary. |

## What implementation taught us

### Recovery policy needs correction

The headless design still describes automatically retrying some executions
when no output or result exists. We deliberately implemented a stronger rule:
once launch may have occurred, silence cannot establish that nothing happened.
Uncertain work needs review. The design should adopt that rule throughout.

See the current [persistence and recovery contract](agent-operations.md#persistence-and-recovery).

### Questions need explicit linkage before becoming approvals

At the implementation baseline, routing a new message to a session returns
every blocked delivery in that session to pending. That is broader than
“this reply answers this question.” The proposed question-to-delivery
relationship remains valuable and should become an early milestone. Reading,
archiving, answering, and approving must remain distinct operations.

See `AgentQueue assign:messages:outbox:` in
[AgentQueue.trash](../trash/AgentQueue.trash) and `AgentRun askUser:` in
[AgentRun.trash](../trash/AgentRun.trash).

### Policy promises need an implementation boundary

`AgentRole` has recipient and budget fields, but the current agent send path
authenticates the run and sends without enforcing those policies. The revised
document should distinguish implemented attribution, actual sandbox
containment, and still-planned authorization checks.

See [AgentRole.trash](../trash/AgentRole.trash) and `AgentRun send:to:` in
[AgentRun.trash](../trash/AgentRun.trash).

### The old implementation prescription should largely retire

The tmux/Coproc-centered agent architecture, a separate prompt status strip,
and mandatory new applets have been overtaken by session drivers, supervised
workers, Whisker, and existing Innards surfaces. The one-shot path remains
useful, but it no longer defines ordinary `@@` interaction.

## Proposed order of work

1. **Know what needs attention.** Distinguish unread messages, unanswered
   blocking questions, and stalled work; expose them consistently in the inbox,
   session browser, and Whisker.
2. **Resolve the right work.** Link replies and decisions to specific
   questions/deliveries, and strengthen action identity and policy checks.
3. **React to one real event source.** A test failure wakes an observer,
   survives restart/replay, and produces a useful result or question.
4. **Improve focus where those journeys demand it.** Add history paging,
   cross-session navigation, then live follow or composition where the existing
   applets become limiting.

## Proposed document revision

Expand [ambient-session.md](ambient-session.md) into the concise product
direction, treat the Innards plans as historical implementation records, and
revise the headless design into an accurate baseline plus remaining contracts
and milestones.

The revision should preserve the durable-agent ambition while making everyday
usefulness—not completing the original component inventory—the measure of
progress.

These revisions and implementation priorities remain proposals. This document
preserves the assessment for later consideration; the existing designs have
not been rewritten as part of recording it.
