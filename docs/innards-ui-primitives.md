# Innards UI Primitives for Trashtalk

**Status:** Initial implementation available (2026-09-26).

The retained toolkit, version-1 protocol, signal/binding helpers, live synthetic
event inspector, and compositional inspection stack are implemented. See the
[toolkit guide and protocol](innards-ui.md) for executable DSL messages, numeric
limits, controls, and current limitations. Examples below remain design sketches;
they do not imply new compiler syntax. Performance evidence is recorded in
[the validation report](innards-ui-performance.md).

## Purpose

Give Trashtalk a compact way to describe responsive Innards terminal user interfaces.

The design targets forms, event feeds, inspectors, tables, and live graphs. It must remain fast when an application receives frequent updates or displays large collections.

## Background: Smalltalk-80

Smalltalk-80 used the Model-View-Controller pattern.

- A **model** owns application state.
- A **view** draws state.
- A **controller** maps pointer and keyboard input to messages.
- A model announces changes to its dependents. Views update after relevant changes.

Its practical UI building blocks included windows, panes, text editors, menus, buttons, scroll bars, lists, sliders, and drawing views. Smalltalk also used **pluggable views**. A generic widget asked its model for data and sent configured messages when the user acted.

Trashtalk should adopt these ideas, but not copy the old class hierarchy.

## Decision

Innards will provide a retained-mode composition layer over its existing
Ratatui/Crossterm rendering path and shared `InlineTerminal` surface. Innards
continues to own terminal input, drawing through `/dev/tty`, resizing, and
terminal restoration. The terminal emulator owns fonts and glyph rendering.

A retained-mode system stores the current widget tree and interaction state
between redraws. Trashtalk describes the desired tree declaratively. Innards
compares that description with the retained tree, updates changed widgets,
lays them out in terminal cells, and renders through Ratatui. Ratatui's buffer
diff determines which cells need terminal output.

```text
Trashtalk UI DSL
        |
        v
immutable UI description
        |
        v
reconciler: matches descriptions to retained widgets
        |
        v
Innards widget state, cell layout, input dispatch
        |
        v
Ratatui / Crossterm / InlineTerminal
```

The renderer must not invoke arbitrary Trashtalk code while painting. It draws resolved widget state produced by reconciliation.

## First-class primitives

Start with a small primitive set.

| Primitive | Purpose |
| --- | --- |
| `panel` | A layout container. Supports rows, columns, padding, gaps, alignment, and borders. |
| `text` | Non-editable styled text. |
| `input` | Single-line or multi-line editable text. |
| `button` | A control that invokes an action. |
| `toggle` | A boolean control. |
| `select` | A compact choice from finite options. |
| `list` | A scrollable, selectable, virtualized sequence of rows. |
| `table` | A virtualized grid of rows and columns. |
| `split` | Two panes with a draggable divider. |
| `canvas` | A retained drawing surface using styled terminal cells and drawing glyphs. |
| `plot` | A specialized canvas for time series and XY data. |

Scrolling is normally part of `list`, `table`, editable text, and `panel`, rather than a control authors compose manually.

Layout dimensions, padding, gaps, and scroll offsets use terminal rows and
columns. A future `window` convenience could supply movable and resizable
panels within the terminal viewport. It is not needed for the first usable
toolkit.

## Composition patterns

Forms, event feeds, inspectors, and dashboards are convenience constructs. They lower into the primitive set.

### Form

```smalltalk
form: settings
  fields: #(
    (name input label: 'Name' required: true)
    (refreshSeconds number label: 'Refresh interval' min: 1 max: 3600)
    (enabled toggle label: 'Enabled')
  )
  onSubmit: [ :values | self saveSettings: values ].
```

`form:` creates labels, inputs, validation messages, focus order, and a submit action. It does not introduce a separate rendering path.

### Event feed

```smalltalk
list
  items: events
  key: [ :event | event id ]
  rowHeight: 1
  followEnd: true
  render: [ :event |
    panel row [
      text: event timestamp.
      text: event message.
    ]
  ]
  onSelect: [ :event | self inspect: event ].
```

`followEnd: true` keeps the viewport at the newest event only while the user already views the end. When the user scrolls away, new events increment an unread counter instead.

### Graph

```smalltalk
plot
  series: cpuSamples
  x: [ :sample | sample time ]
  y: [ :sample | sample percent ]
  domainY: 0 to: 100
  style: #line
  followLatest: true
  tooltip: [ :sample | sample percent printString, '%' ].
```

`plot` owns zooming, panning, hit testing, decimation, cached cell/glyph output,
axes, and tooltips. Its drawing resolution comes from the available terminal
cells and chosen glyphs. It must not create a widget for each point.

### Navigable object inspector

An object inspector uses a horizontal inspection stack, following the Pharo
inspector interaction model. It is not a recursive tree and it does not open a
separate window for every nested value.

Inspecting an object creates the first inspector pane. Drilling into one of its
declared values pushes that value onto the inspection stack. The prior active
pane moves left and the drilled value appears in a new active pane on the
right. Each later drill-down repeats this transition.

```text
inspect: order

  [ Order ]

drill into: order customer

  [ Order ] [ Customer ]

drill into: customer address

  [ Customer ] [ Address ]
```

The viewport keeps the active pane at the right edge. On a narrow surface it
shows the active pane alone. On a wide surface it shows the active pane and
its immediate parent. Older entries remain retained in the inspection stack,
so returning to them preserves their selected slot, scroll position, and other
pane-local UI state.

The inspector renders a compact row of dots below the panes. Each dot denotes
one stack entry. The active entry has a distinct state. Selecting a dot makes
that entry active and reveals it with its parent where space permits. Keyboard
Back and Forward traverse entries, and a pane can expose an explicit parent
action. Drilling from an older entry replaces entries after it, because the
new value starts a different inspection branch.

The stack state is data, not widget nesting:

```text
InspectionStack
  entries: [InspectionEntry]
  activeIndex: Integer

InspectionEntry
  object: ObjectReference
  key: StableObjectIdentity
  selectedSlot: SlotName | nil
  scrollOffset: Number
```

`InspectionEntry key` gives each retained pane stable reconciliation identity.
An object reference must use the inspector's existing safe snapshot or
inspection protocol. The UI never enumerates arbitrary object state while
painting. Reconciliation obtains the visible slot records, then the renderer
draws resolved pane state.

This composition lowers to `panel`, `split`, `list`, `text`, `button`, and
`toggle`-like dot controls. It does not need a new primitive. A future
`inspector` convenience construct can create this composition and own its
inspection-stack state.

## Declarative binding and actions

Every primitive uses the same broad shape.

```smalltalk
button: 'Refresh'
  enabled: [ refreshAllowed ]
  action: [ self refresh ].

input
  value: draftMessage
  placeholder: 'Write a message'
  onSubmit: [ :text | self send: text ].
```

Use ordinary Trashtalk blocks for the initial binding API. The receiving
property defines the block's role: `enabled:` accepts a read-only binding,
while `action:` and `onSubmit:` accept action callbacks. This requires no new
binding syntax. If that distinction proves ambiguous in use, an explicit
binding object can be constructed through an ordinary DSL message before
considering syntax sugar.

A binding runs on the Trashtalk side, where its reads of reactive model values
are recorded. Model changes invalidate dependent bindings; explicit
invalidation covers reads from existing mutable objects. Reevaluation sends
resolved property values to Innards for reconciliation. Innards neither
evaluates Trashtalk blocks nor discovers their dependencies during drawing.
Binding evaluation must be free of application side effects, since it may run
again after invalidation.

Actions run on the Trashtalk side in response to explicit UI intents or
scheduled application work. They can change model values, invalidating bindings
and scheduling an update. Local editing, focus, and scrolling remain Innards
interaction state. Submission or another explicit commit sends the current
input value to Trashtalk. Applications may opt into debounced asynchronous
validation or search; neither blocks editing while waiting for a reply.

Immediate rules over local input, such as enabling Send when a draft is
nonempty, use a small declarative set of Innards behaviors. They must not
invoke a Bash binding per keystroke. The `enabled:` block above reads domain
state; arbitrary Trashtalk expressions remain evaluated by Trashtalk. The
local-rule vocabulary and its DSL spelling still need to be specified.

The initial reactive model API can be small:

```smalltalk
state := Signal value: ''.
state value: 'new text'.
state value.
```

A `Signal` is a mutable value with change notifications. Derived values can arrive later. The implementation must support explicit invalidation too, so existing mutable Trashtalk objects can participate without a full rewrite.

## Existing process boundary

Reuse the existing exact-argv duplex JSONL bridge and Innards event loop.
Trashtalk supplies versioned data and acknowledgements on the applet's stdin;
Innards returns explicit action requests on stdout and owns terminal input and
drawing through `/dev/tty`. Its reader thread delivers parsed messages to the
UI event loop, which owns view state and redraws.

`Tool duplexArgvJson:handler:context:` and `Agent::Focus` already provide this
boundary for `inagent`. The current conversation bridge polls for changed
snapshots and dispatches actions through its Trashtalk handler. The general
UI protocol extends that boundary with widget descriptions, binding updates,
and batched data-source requests. It follows the granularity and pacing rules
below; the existing conversation polling and full-snapshot policy is not a
performance target for the new toolkit.

## Identity and reconciliation

The reconciler keeps a stable Innards widget when the primitive type and identity match.

- A widget with an explicit `key:` keeps its identity across insertions, removals, and reordering.
- An unkeyed child uses its position within its parent as identity.
- A changed primitive type replaces the old widget.
- A changed property updates that widget only when its resolved value differs.

Collections that can change order must use stable keys. Event IDs, database IDs, and durable names are suitable keys. Collection indexes are not suitable keys for a mutable event feed.

## Data-oriented widget protocols

Lists and tables use coarse asynchronous data-source operations. Bash work
scales with requested batches and application changes. Innards owns local
interaction with the data it already holds.

### Collection windows and presentation

The adapter can wrap an array, lazy event log, file-backed source, database
cursor, or stream. A small bounded collection may arrive as one complete
snapshot. Large collections are fetched in windows: one request obtains a
bounded range of rows, their stable keys, resolved fields, and collection
revision together. Count or continuation metadata must describe that same
revision. `itemAt:` and `keyAt:` may be internal adapter operations; they are
not individual wire requests.

Innards caches neighboring windows and prefetches near their boundaries.
Scrolling within the cache makes no Bash calls. Only visible rows and a small
overscan buffer become retained widgets; the data cache can cover more rows.

Send a reusable row or column presentation description with batches of resolved
row data. Row rendering and cell layout run locally in Innards. The composition
examples describe the intended presentation; their lowering must avoid separate
Trashtalk object sends or JSON serialization for every displayed cell.

### Incremental updates and revision checks

An initial snapshot establishes the view. Subsequent messages carry bounded
batches of property changes, row appends, replacements, and removals. Normal
update work is proportional to the changed data. Full snapshots are used for
initialization and resynchronization.

Each incremental batch identifies its target, base revision, and resulting
revision. Innards applies it only to the matching state. A missing or
incompatible revision requests resynchronization rather than applying a
partial or inconsistent view. Window replies identify the request and the
collection revision so obsolete results cannot replace the current view.

### Bounded work and backpressure

Limit in-flight window and search requests. When scrolling or search input
changes faster than Trashtalk can reply, retain the latest desired viewport
or query and coalesce obsolete pending requests. Innards continues drawing
cached content and accepting input while data is pending.

Save, Send, Delete, and other domain actions retain distinct request IDs and
acknowledgements. They must not be discarded as obsolete viewport requests or
automatically replayed after an uncertain outcome. Disabling a pending control
or showing an error is local UI feedback; mutation remains authorized by
Trashtalk.

Bound both buffered bytes and queued requests. Stream adapters expose batch
reads or appends and accumulate changes before publishing them; they must not
invoke a Bash handler for every high-rate sample. Coalescing state updates
must preserve revision continuity, or invalidate the cached state and resync.
Event loss, retention limits, and resynchronization must be explicit outcomes.
Innards' UI event loop must not block on pipe writes when Bash is slow.

### Batch preparation in Trashtalk

Keep the existing session-scoped Bash bridge alive for the view. Decode a
request together, use bulk model reads, and serialize a complete update batch
together. `asJson` already builds a nested typed value with one jq invocation.
Do not allocate persistent Trashtalk objects merely to represent widgets or
temporary wire records.

Bound external-process launches per batch independently of its row count.
A single wire response that performs hundreds of getter sends, jq processes,
or SQLite queries internally does not meet this requirement. Bulk operations
must preserve the source's authorization, identity, and snapshot semantics.

The redraw cadence is independent of Bash update cadence. A redraw never
requests reevaluation of every binding, and a change notification must not
require rescanning the entire collection or transcript history.

## Update and rendering rules

1. Model changes mark dependent bindings dirty on the Trashtalk side.
2. Trashtalk coalesces invalidations, resolves dirty bindings, and sends property updates.
3. Innards applies received values in its UI event loop and schedules a redraw.
4. The event loop coalesces pending updates into one reconciliation pass per
   redraw, applying minimal widget mutations.
5. Layout runs only for nodes whose size, content, or parent constraints changed.
6. Rendering reuses cached text and plot data to compose a Ratatui buffer;
   Ratatui emits changed cells through the existing terminal backend.

Innards' UI event loop owns retained widgets, layout, and rendering. Background
work sends immutable result values to that loop. It never mutates a widget
directly. Extend Innards' existing change-driven redraw scheduling to coalesce
these updates; an unchanged UI produces no redraw.

## Performance requirements

The first implementation must meet these constraints.

- Coalesce continuous background updates to at most 60 redraws per second.
  Reconcile once per redraw, and wake promptly for terminal input. Scheduling
  uses the event loop's clock; it does not depend on display refresh synchronization.
- Target 16.7 ms for local input feedback through completion of the resulting
  terminal writes. Measure this inside Innards; terminal-emulator display latency
  is outside that budget. Measure asynchronous model/action latency separately.
- Keep a steady event feed responsive with at least 10,000 retained data events.
- Instantiate only visible list or table rows plus an overscan buffer.
- Coalesce high-rate event arrivals before reconciliation.
- Bound live feeds. A default maximum of 10,000 events is reasonable. Applications can archive older data elsewhere.
- Cache text wrapping, terminal display widths, and styled spans. Invalidate
  the relevant cache when text, available cell width, or style changes.
- Cache static canvas and plot cell/glyph output such as axes and labels.
- Decimate plotted data to the available horizontal cell/glyph resolution.
  Preserve extrema in each displayed sampling column.
- Measure input-to-output latency, reconciliation time, cell-layout time,
  buffer-render time, terminal-write time and volume, allocated widgets,
  live rows, and dropped or coalesced updates. Include Bash subprocess counts,
  batch preparation time, wire bytes, cache misses, queued bytes, outstanding
  requests, and resynchronizations.

Do not poll every binding on each event-loop iteration. Dependency tracking and explicit invalidation are the primary update mechanisms.

Before expanding the primitive set, validate a list-and-detail slice with:

- Zero Bash calls for typing, drawing, resizing cached content, and cached scrolling.
- Bounded subprocess counts per batch as its row count grows.
- Update processing proportional to changed data, with complete row-count and
  content checks so dropped records cannot masquerade as speed improvements.
- Bounded queued bytes and requests during fast scrolling and sustained arrivals.
- Responsive typing and scrolling with an intentionally slow Bash handler.
- Revision mismatch, stale query reply, and reconnect cases that recover to the
  correct view without silently replaying domain actions.

## Profiling

Profile Trashtalk, the transport boundary, and Innards together. A fast redraw
does not establish that application updates arrive promptly. Report local UI
responsiveness separately from asynchronous application response time.

| Layer | Measurements |
| --- | --- |
| Trashtalk | Handler time, binding-evaluation time, batch preparation, subprocess launches, and database operations. |
| Transport | Requests and bytes in each direction, outstanding requests, queued bytes, response latency, stale replies, coalesced updates, and resynchronizations. |
| Innards | Local input latency, reconciliation, layout, buffer rendering, terminal writes, retained widgets, cached and visible rows, and cache misses. |

Use the view identity and request IDs to correlate requests, handler work,
acknowledgements, and resulting updates. Unsolicited updates carry their source
and revision or batch identity. Distinguish acknowledgement latency from the
time at which the resulting update has been rendered and written to the
terminal.

Use monotonic elapsed timing where it is available without spawning a process.
Measure round-trip latency in the requesting process, and measure local phases
with that process's clock; do not subtract unrelated process-local clock
origins. Bash-side phase timing must use an inexpensive available mechanism
or be collected in a dedicated benchmark harness. Starting `date`, `ps`, or
another helper for every measured operation is unacceptable.

The profiling modes are:

- **Disabled by default:** no profiling-specific subprocesses, per-event log
  writes, or trace-record allocations. Queue and revision bookkeeping required
  for normal correctness continues. Measure the residual disabled overhead.
- **Enabled:** accumulate counters and bounded timing summaries in memory at
  request, batch, and redraw boundaries. Expose an explicit summary or a
  rate-limited diagnostic view. Collection must not trigger model reads or
  redraws for every sample, and diagnostics must preserve the JSONL channel.
- **Benchmark tracing:** use detailed process and database tracing in dedicated
  runs to account for actual subprocess launches and operations, including
  work outside instrumented wrappers. Keep this tracing out of routine UI use
  and label its overhead separately from ordinary latency measurements.

Routine counters describe the operations actually instrumented; they must not
be presented as a complete OS process census. Summaries include sample counts
and latency distributions so averages cannot hide stalls. Validate disabled
overhead and enabled measurement cost on the first list-and-detail slice.

## Input, focus, and accessibility

Innards owns hit testing, pointer capture, keyboard routing, tab order, focus, selection, clipboard integration, and accessible names.

Every interactive primitive must support these properties where relevant:

```text
enabled:
visible:
focused:
accessibilityLabel:
on: event do:
style:
```

The first release needs correct input behavior before elaborate visual styling.

## Styling

Structure and presentation remain separate.

```smalltalk
button: 'Refresh'
  style: #primary
  enabled: [ refreshAllowed ]
  action: [ self refresh ].
```

A theme maps style names and widget states to terminal colors, text attributes,
cell padding, border glyphs, and interaction feedback. Style changes invalidate
rendered content and sometimes layout, but they do not replace widget identity.

## Non-goals

The initial design does not require:

- A native control wrapper for every platform widget.
- A general browser DOM or CSS implementation.
- A widget per graph point, event, or table row.
- Arbitrary user code during paint.
- Fine-grained reactive dependency tracking across every existing mutable object.

## Delivery sequence

1. Build `Signal`, dependency capture, integration with existing redraw scheduling, `panel`, `text`, `button`, and `input`.
2. Add keyed reconciliation, focus handling, scrolling, virtualized `list`, and event-feed behavior. Instrument and validate the list-and-detail performance slice before expanding the primitive set.
3. Add `table`, form conveniences, canvas, and basic plot rendering.
4. Extend profiling views and performance scenarios, graph decimation, and cached cell/glyph output as the toolkit grows.

The initial usable application should be a live event inspector. Start with
its event feed, selection, and detail panel to validate the process boundary.
Then add filtering input and a small time-series plot. This exercises the
intended performance path before broadening the widget surface area.

## Protocol specification

The transport, binding syntax, coarse data-source approach, and profiling scope
are implemented in version 1. The [toolkit guide](innards-ui.md#protocol-version-1)
specifies schemas, independent revisions, stable ordering, window invalidation,
native draft and query generations, overload bounds, and diagnostic controls.

The list/detail gate was exercised before adding the other primitives. The
initial event inspector uses generated events to make its load reproducible;
production domain sources still supply their own bulk projection and handler.
