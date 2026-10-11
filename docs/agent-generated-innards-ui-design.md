# Agent-generated Innards UI design

**Status:** Proposal. Nothing in this document is implemented.

## Goal

Let a Trashtalk agent propose and update small, interactive Innards surfaces
while it works. A person sees the surface in `inagent` or `inui`, supplies
input or a decision, and the normal Trashtalk application validates and acts
on that input.

The scope is strictly Innards. This design does not add a web client, CopilotKit,
AG-UI, or a general cross-platform UI protocol.

## Why this is a separate subsystem

An agent can already send text. Text is poor for a choice among structured
alternatives, an approval with evidence, a form that needs several fields, or
progress whose useful parts change independently. Innards already has retained
JSON views, keyed nodes, revisions, forms, action intents, and a handler that
validates every action.

Agent-generated UI must use those existing boundaries. An agent proposes data.
It never gains permission to run UI code, call a native widget directly, bypass
a handler, or perform an effect because a person clicked a generated button.

## Terms

- **Agent UI proposal:** A bounded declarative description sent by one agent run.
- **Surface:** One visible, retained Innards view owned by a session and a run.
- **Catalog:** The fixed list of component kinds, properties, and action forms
  that the host accepts.
- **Projection:** The trusted conversion from a validated proposal to existing
  `UI::Node` values and `UI::Surface` frames.
- **Action binding:** A host-defined mapping from a component event to a safe
  Trashtalk handler action. An agent cannot define effects in a binding.

## Requirements

1. The feature is opt-in. Existing agent sessions and Innards views behave as
   they do today.
2. A proposal is JSON data and must fit Innards' existing frame, tree, property,
   and input limits.
3. A trusted host catalog decides which component kinds and properties exist.
4. Every user action becomes an ordinary Innards intent. The owning handler
   validates identity, current state, authorization, and values before any
   durable or external effect.
5. A proposal is scoped to one `Agent::Session`, `Agent::Run`, and surface ID.
   Another run cannot update or close it.
6. The UI identifies its agent, run, and state: proposed, waiting for input,
   stale, completed, or failed.
7. Detaching or stopping a run never replays a user action. Pending actions keep
   the existing Innards acknowledgement and idempotence rules.
8. Logs and durable records retain a bounded proposal summary and action receipt,
   but never secrets or unrestricted model output by default.

## Non-goals

- Rendering arbitrary HTML, JavaScript, shell, Markdown extensions, images, or
  remote iframes.
- Giving an agent an ambient write token because it can display a button.
- Replacing Innards' JSONL transport, revision model, or application handler.
- Making a generated UI persistent widget object state in the Trashtalk SQLite
  object store.
- Supporting every control that a future web renderer might expose.

## Common architecture

Both implementation options share this trust boundary:

```text
Agent run output or declared UI tool result
                 |
                 v
       Agent UI admission and validation
                 |
                 v
      Session-scoped proposal and surface state
                 |
                 v
  trusted projection to UI::Node and UI::Surface frames
                 |
                 v
          existing Innards JSONL bridge
                 |
                 v
      Innards action/query intent to owning handler
                 |
                 v
  authorization, validation, transaction, and optional agent reply
```

The admission layer owns:

- schema-version checks and exact JSON validation;
- component and property allowlisting;
- stable, collision-free component and action IDs;
- resource limits before native JSON decoding;
- ownership checks against the active session/run/surface;
- monotonic proposal revisions and rejection of stale updates;
- redaction in errors, logs, agent transcripts, and profiling receipts.

The projection layer is the only code that can create the generated view. It
uses the existing `UI::Node` primitive kinds: `panel`, `text`, `input`,
`button`, `toggle`, `select`, `list`, `table`, `split`, `canvas`, and `plot`.
The first catalog should expose far fewer kinds.

## Initial user experience

A generated surface appears as a named card or pane within the attached agent
view, rather than as an unsolicited new terminal window. Its header states the
agent identity and one of these statuses:

- **Proposal:** The agent has supplied information for a person to inspect.
- **Waiting for input:** The agent needs a choice or form data.
- **Submitted:** Innards acknowledged the person's input to the handler.
- **Completed, failed, or stale:** The run ended, or a newer run replaced it.

The first useful surface is an approval card. It has a title, bounded evidence
text, an explicit choice, and optional short fields. The application owns what
approval means. For example, selecting “approve” can reply to the agent with a
structured answer. It cannot commit a change unless the normal workflow also
authorizes that change.

## Option A: Trashtalk-only proposal format

The recommended first implementation is a narrow Trashtalk-owned format. It
maps directly to the Innards features we already support. It avoids adopting an
external renderer contract before the local interaction model is proven.

A proposal has a fixed envelope, for example:

```json
{
  "schema_version": 1,
  "kind": "innards.ui.propose",
  "surface_id": "review-42",
  "revision": 1,
  "title": "Confirm deployment target",
  "status": "waiting_for_input",
  "root": {
    "kind": "panel",
    "key": "root",
    "props": {"direction": "column", "title": "Deployment review"},
    "children": [
      {"kind": "text", "key": "summary", "props": {"text": "Deploy to staging?"}},
      {"kind": "button", "key": "approve", "props": {"label": "Approve", "action": "respond"}},
      {"kind": "button", "key": "reject", "props": {"label": "Reject", "action": "respond"}}
    ]
  },
  "actions": {
    "respond": {"kind": "reply_to_agent", "allowed_values": ["approve", "reject"]}
  }
}
```

This example describes the shape, not a final contract. The actual format must
avoid accepting arbitrary native `props`. Each catalog component has a small,
typed schema. For example, `text` can carry bounded text and style, while
`button` can carry a label and one declared action ID. It cannot carry a shell
command, callback source, or a raw handler selector.

### Proposal delivery

The agent needs one declared way to emit a proposal. There are two candidates:

1. **A dedicated agent tool.** The harness exposes `propose_innards_ui` with a
   JSON schema. Its result is a proposal request, not a UI effect. The host
   validates and admits it.
2. **A typed event in normalized run output.** A driver recognizes a documented
   JSON event from an agent and passes it to the admission layer.

Prefer a dedicated tool. It gives the model an explicit schema and keeps
ordinary assistant text from accidentally becoming UI. A normalized event can
be added later for harnesses that already expose structured tool output.

### Updates and replies

Start with complete replacement proposals. A later `innards.ui.update` can
replace selected allowed properties or the whole root at a higher revision.
Reuse Innards' `tree` and `properties` frames after projection. Do not expose
Innards wire frames directly to the agent.

A submitted action returns a structured, bounded value to the owning run. The
handler may send it through `Agent::Session input:` or a run-specific response
path. A response includes the surface ID, proposal revision, action ID, and
value. That lets the agent distinguish a late reply from a current one.

## Option B: A2UI input adapter

[A2UI](https://a2ui.org/) is a declarative agent-to-user-interface format. An
agent describes components and data; a renderer maps those descriptions to a
trusted local component catalog. Its model fits the desired safety boundary,
but it is currently an early-stage public preview and has no Innards renderer.

An optional `UI::A2UI` adapter can accept one pinned A2UI version, validate a
restricted catalog, and project it to the same internal proposal form from
Option A:

```text
A2UI message stream
       -> versioned A2UI validator and restricted catalog
       -> Trashtalk internal proposal
       -> UI::Node projection
       -> Innards frame
```

The adapter must be one input format, not an alternate authorization system.
Its components and events still use the host-owned action bindings described
above.

### A2UI advantages

- Agents and tools that already generate A2UI can describe a familiar
  declarative UI structure.
- A2UI separates generated data from trusted renderer components.
- Its incremental surface and data updates may later map well to Innards tree
  and property revisions.

### A2UI costs and mismatches

- Innards needs a new Rust renderer or a Trashtalk translator. No existing
  A2UI renderer targets this terminal UI.
- A2UI's catalog, component semantics, bindings, and update stream do not map
  one-for-one to Innards' fixed primitives, local drafts, signals, and JSONL
  revision rules.
- Supporting the full A2UI basic catalog would enlarge the native UI and the
  security review surface without serving the initial approval-card use case.
- A2UI is pre-1.0. The adapter must pin a version and treat upstream changes as
  deliberate compatibility work.

### Restricted A2UI profile

If implemented, start with a profile that supports only:

| A2UI concept | Innards projection | Initial limit |
| --- | --- | --- |
| Surface and component IDs | Surface and node keys | One surface per owning run |
| Text | `UI::Node text` | Bounded plain text only |
| Container/card | `UI::Node panel` | Row or column layout only |
| Button | `UI::Node button` | Host-declared action only |
| Text input | `UI::Node input` | Bounded string value only |
| Select and toggle | `UI::Node select` and `toggle` | Bounded static options |
| Data update | `properties` or full `tree` frame | Monotonic admitted revision |

Reject unsupported components, styles, expressions, custom components, remote
resources, and any event that does not map to a declared host action. Keep the
first profile snapshot-based. Add incremental A2UI updates only after the
internal proposal revision model is reliable.

## Decision

Build the Trashtalk-only internal proposal model first. It directly serves
Innards, has one small surface area, and gives us a testable authorization and
revision contract.

Design its envelope so that an A2UI adapter can produce it later. That keeps
A2UI available as an interoperability input without making Innards dependent on
an evolving external protocol. Do not adopt CopilotKit or AG-UI for this
strictly Innards feature.

## Delivery plan

### Phase 0: contract and threat model

1. Define the internal proposal JSON schema and the initial catalog.
2. Choose the proposal owner fields and generated surface placement in `inagent`.
3. Specify action bindings and the structured agent reply path.
4. Record limits for text, nodes, depth, options, actions, update frequency,
   and retained proposal history.
5. Write adversarial examples for prompt injection, stale events, duplicate
   actions, cross-session updates, and malformed JSON.

### Phase 1: static trusted cards

1. Add an opt-in `Agent::GeneratedUISettings` group, disabled by default.
2. Implement the dedicated proposal tool and admission layer.
3. Project read-only proposal cards with text and approved/rejected controls.
4. Store a bounded audit summary and display run ownership and status.
5. Test malformed, oversized, unknown, unauthorized, and stale proposals.

### Phase 2: bounded input and updates

1. Add input, select, and toggle through explicit action bindings.
2. Send acknowledged structured replies only to the owning run.
3. Add complete replacement updates with revision checks.
4. Test detach, stop, retry, duplicate delivery, and agent completion during
   human input.

### Phase 3: optional A2UI adapter

1. Pin an A2UI version and define the restricted profile as a test fixture.
2. Implement the adapter into the internal proposal schema.
3. Add conformance tests for accepted and rejected A2UI messages.
4. Compare a native proposal and its A2UI equivalent in the same Innards view.
5. Adopt incremental A2UI messages only if they preserve Innards local drafts,
   revision checks, and action idempotence.

## Acceptance rules

An initial release is complete when:

- Generated UI remains disabled unless the user opts in.
- An agent can show only catalog components and typed, bounded properties.
- Every visible control maps to a host-declared action binding.
- The handler rejects stale, duplicated, unauthorized, and invalid input before
  an effect occurs.
- The displayed surface always identifies its owning run and current status.
- A stopped or detached session cannot replay a submitted action.
- Tests cover the catalog, projection, resource limits, revision behavior,
  redaction, and all action outcomes.

## Open decisions

1. Should an approved agent proposal display inline in `inagent`, in a new
   `inui` pane, or in both locations?
2. Does the first action binding only reply to an agent, or can it invoke an
   existing typed decision workflow?
3. Which agent drivers can expose the dedicated proposal tool safely?
4. What part of a proposal and user response belongs in `conversation.jsonl`?
5. Does a new run replace the old run's surface, retain it as stale evidence,
   or let a person compare both?
