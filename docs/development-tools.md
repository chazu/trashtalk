# Development tools

## Editing and inline tests

Trashtalk supports defining tests directly in class files using `testMethod:`.
When `inmacs` is on `PATH`, `@ Trash edit: ClassName` opens the source in the
Innards inline editor with Trashtalk syntax highlighting and two-space
indentation. Saving compiles to a temporary artifact, checks the generated Bash,
installs and reloads the class, then runs its inline tests. Compiler and test
failures reopen the editor as annotations at the relevant source line; they are
never inserted into the `.trash` source.

If Innards is unavailable, the edit command falls back to `$VISUAL`, then
`$EDITOR`, then `vi`. The fallback still uses the same compile, validation,
reload, and test pipeline after the file changes. `@ Trash doctor` reports
Innards availability as an optional capability.

A DSL inline test returns `true` or `false`:

```smalltalk
testMethod: testGreeting [
  ^ (@ Greeting for: 'Ada') = 'Hello, Ada'
]
```

`@ Trash runTestsFor: Greeting` runs the class's `testMethod:` declarations;
`@ Trash hasTestsFor: Greeting` checks whether any exist. Raw tests declare
`pragma: primitive` and use `_assert_eq "$actual" "$expected" "description"`,
`_assert_contains "$text" "$part" "description"`, or `_assert_ok "command" "description"`.
`_assert_true`/`_assert_false` check nonempty/empty text, not the words
`true`/`false`. Repository-wide tests run through `make verify`.

## Browsing

`@ Trash browse` opens the read-only Innards `inbrowser` applet. It derives its
package, class, protocol, and method columns directly from the canonical jq
compiler AST and displays source from the selected method. It has no edit,
compile, or mutation path. Use arrow keys or `j`/`k` to select, Left/Right or
Enter to move across columns, PageUp/PageDown to scroll source, and `q` to
close. Install it with `cargo install --path . --bin inbrowser --locked --force`
from the Innards checkout.

```bash
@ Trash browse                         # choose any symbol and open its source
@ Trash browseClass: Counter           # browse one class and open a selection
@ Trash pickMethod: Counter            # return a structured method selection
@ Trash browseImplementorsOf: 'at:put:'
@ Trash browseSendersOf: 'at:put:'
@ Trash browseInstancesOf: Counter     # table of persisted instances, then inspect on Enter
@ Trash selectInstanceOf: Counter      # return a structured picker selection to scripts
```

Class, trait, instance-variable, class-variable, instance-method, class-method,
and test-method records carry exact source positions. Namespaced classes and
complete multi-keyword selectors remain intact. Enter in an instance table
opens its navigable object inspector with declared values and nested containers,
rather than printing the selected record JSON. Script-level picker methods
return JSON; commands that open source feed the chosen path and line into the
same edit/compile/test loop described above.

## Inspecting objects

`inspect` is the one way to look at an object. With Innards `inui` on `PATH`
and a human at the terminal, it opens the `UI::Inspector` surface; every other
caller (scripts, agents, missing `inui`) gets the textual `describe` output.

```bash
counter=$(@ Counter new)
@ "$counter" inspect                   # UI inspector, or describe's text
@ Trash inspectObject: "$counter"      # the same entry point by object id
@ Trash browseInstancesOf: Counter     # pick an instance, then inspect it
@ "$counter" describe                  # always the textual form
```

Enter drills into containers; Back/Forward and Alt-Left/Alt-Right move along
the inspection stack. Enter on a scalar instance variable opens a one-line
editor holding its JSON value; Enter or Apply submits it. The inspector only
stages an edit proposal. `ObjectEditProposal` checks that the object and the
selected value have not changed since they were shown, then applies the typed
value through `Runtime`; a stale or invalid edit is refused in the status line.
Runtime metadata is not offered as editable state.

## Readline shortcuts

Bind optional applets from an interactive Bash startup file after sourcing
Trashtalk. `bind -x` preserves the command being edited:

```bash
bind -x '"\eu": @ Gusgus focusCurrent'
bind -x '"\ey": @ Trash browse'
bind -x '"\ei": @ "$(@ Trash userInbox)" browse'
```

See [the live session guide](agent-session-view.md) for Gusgus attachment and
[Innards UI](innards-ui.md) for the separate `UI::*` toolkit.
