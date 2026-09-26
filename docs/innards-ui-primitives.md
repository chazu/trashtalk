# Innards UI Primitives for Trashtalk

**Status:** Proposal

## Purpose

Give Trashtalk a compact way to describe responsive Innards user interfaces.

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

Innards will provide a retained-mode UI system.

A retained-mode system stores the current widget tree between frames. Trashtalk describes the desired tree declaratively. Innards compares that description with the retained tree, updates only changed widgets, lays out invalidated regions, and draws the result.

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
Innards widgets, layout, input dispatch, renderer
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
| `canvas` | A retained custom drawing surface. |
| `plot` | A specialized canvas for time series and XY data. |

Scrolling is normally part of `list`, `table`, editable text, and `panel`, rather than a control authors compose manually.

A future `window` primitive can supply movable and resizable top-level surfaces. It is not needed for the first usable toolkit.

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
  rowHeight: 22
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

`plot` owns zooming, panning, hit testing, decimation, geometry caching, axes, and tooltips. It must not create a widget for each point.

## Declarative binding and actions

Every primitive uses the same broad shape.

```smalltalk
button: 'Send'
  enabled: [ draftMessage notEmpty ]
  action: [ self send: draftMessage ].

input
  value: draftMessage
  placeholder: 'Write a message'
  onSubmit: [ :text | self send: text ].
```

A property accepts a literal value or a binding block. A binding block reads reactive model values. Innards records those reads during evaluation. When one changes, Innards invalidates only bindings that read it.

Actions run only in response to input or scheduled application work. Actions can change model values. The changed values then schedule reconciliation.

The initial reactive model API can be small:

```smalltalk
state := Signal value: ''.
state value: 'new text'.
state value.
```

A `Signal` is a mutable value with change notifications. Derived values can arrive later. The implementation must support explicit invalidation too, so existing mutable Trashtalk objects can participate without a full rewrite.

## Identity and reconciliation

The reconciler keeps a stable Innards widget when the primitive type and identity match.

- A widget with an explicit `key:` keeps its identity across insertions, removals, and reordering.
- An unkeyed child uses its position within its parent as identity.
- A changed primitive type replaces the old widget.
- A changed property updates that widget only when its resolved value differs.

Collections that can change order must use stable keys. Event IDs, database IDs, and durable names are suitable keys. Collection indexes are not suitable keys for a mutable event feed.

## Data-oriented widget protocols

Lists and tables must not require materializing all data as widgets. They use a data source protocol.

```text
count
itemAt: index
keyAt: index
changedFrom: first to: last
```

The adapter can wrap an array, lazy event log, file-backed data source, database cursor, or streaming source. The list asks for only visible items and a small overscan buffer.

Row rendering creates retained rows for visible items. It does not create one row per data item.

## Update and rendering rules

1. Model changes mark dependent bindings dirty.
2. Dirty bindings queue one UI reconciliation pass.
3. The scheduler coalesces many changes into one pass before the next frame.
4. Reconciliation resolves dirty bindings and applies minimal widget mutations.
5. Layout runs only for nodes whose size, content, or parent constraints changed.
6. Rendering redraws invalidated regions or layers.

The UI thread owns retained widgets, layout, and rendering. Background work sends immutable result values to the UI scheduler. It never mutates a widget directly.

## Performance requirements

The first implementation must meet these constraints.

- Do not reconcile or draw more than once per display frame.
- Keep ordinary input feedback under one frame at 60 Hz, or 16.7 ms.
- Keep a steady event feed responsive with at least 10,000 retained data events.
- Instantiate only visible list or table rows plus an overscan buffer.
- Coalesce high-rate event arrivals before reconciliation.
- Bound live feeds. A default maximum of 10,000 events is reasonable. Applications can archive older data elsewhere.
- Shape and cache repeated text. Invalidate text layout only when text, font, width, or style changes.
- Cache static canvas and plot layers such as backgrounds, axes, and labels.
- Decimate plotted data to screen resolution. Preserve extrema for each pixel column.
- Measure frame time, reconciliation time, layout time, draw time, allocated widgets, live rows, and dropped or coalesced updates.

Do not optimize by polling every binding every frame. Dependency tracking and explicit invalidation are the primary update mechanisms.

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
button: 'Send'
  style: #primary
  enabled: [ draftMessage notEmpty ]
  action: [ self send: draftMessage ].
```

A theme maps style names and widget states to resolved colors, fonts, padding, borders, and interaction feedback. Style changes invalidate paint and sometimes layout, but they do not replace widget identity.

## Non-goals

The initial design does not require:

- A native control wrapper for every platform widget.
- A general browser DOM or CSS implementation.
- A widget per graph point, event, or table row.
- Arbitrary user code during paint.
- Fine-grained reactive dependency tracking across every existing mutable object.

## Delivery sequence

1. Build `Signal`, dependency capture, frame scheduling, `panel`, `text`, `button`, and `input`.
2. Add keyed reconciliation, focus handling, scrolling, virtualized `list`, and event-feed behavior.
3. Add `table`, form conveniences, canvas, and basic plot rendering.
4. Add profiling overlays, automated performance scenarios, graph decimation, and cached drawing layers.

The initial vertical slice should be a live event inspector. It needs an event feed, selection, a detail panel, filtering input, and a small time-series plot. That slice exercises the intended performance path without broad widget surface area.

## Open questions

- Which Innards backend owns drawing and text shaping on each supported platform?
- Does Trashtalk need syntax sugar for reactive bindings, or are binding blocks clear enough?
- How will background Trashtalk work publish immutable UI updates to the UI thread?
- What data source API best maps to the existing Trashtalk collection and stream protocols?
- Which profiling data can Innards expose with negligible overhead when disabled?
