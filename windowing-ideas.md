# Windowing Environment Ideas

Research notes for building a Smalltalk-like editing environment for Trashtalk.

## Goal

Create a text-based windowing environment with Smalltalk-like editing functionality:
- **Class Browser** - Navigate classes, methods, hierarchy
- **Transcript** - Output log, system messages
- **Workspace** - Interactive REPL for testing expressions
- **Inspector** - Examine instance state

## Refined Vision: Terminal-Mode Acme

After research, the clearest inspiration is **Acme** (Plan 9) - but in pure terminal/text mode.

**The Acme paradigm:**
- Text IS the interface - no modes, no special UI chrome
- Any text can be selected and executed
- Output appears in a new panel (or replaces selection)
- Multiple tiled panels that can be created/resized/destroyed
- The whole system is scriptable/programmable
- It's simultaneously an editor, shell, and browser

**The gap:** Acme is graphical (X11). Few terminal-mode equivalents exist.
Kakoune comes close (`!` to execute, `|` to pipe) but is still a modal editor.

**Existing terminal-mode Acme-likes:**

### poe (github.com/prodhe/poe)
- **Terminal-mode Acme in Go** - exactly this concept!
- Mouse-driven: Right-click = Open (file/dir), Middle-click = Run (execute)
- Two-column layout with multiple windows
- Written in Go, ~2k lines
- Status: Experimental ("Do not use for production")
- Could be forked/extended for Trashtalk

### Other Acme-inspired projects (graphical):
- **editor** (github.com/jmigpin/editor) - Go, graphical, very full-featured
- **edwood** (github.com/rjkroege/edwood) - Go port of Acme, graphical
- **a** (github.com/as/a) - Go, graphical, handles huge files
- **Anvil** (anvil-editor.net) - Graphical, multi-pane tiling

**The opportunity:** Fork poe or build fresh, with Trashtalk awareness baked in.

## Design Criteria

- Lightweight
- Terminal/text-mode only (no X11, no GUI)
- Scriptable (ideally from bash, or via file/socket interface)
- The environment should feel like "the Trashtalk system" not "an IDE for Trashtalk"
- In keeping with project philosophy (Unix tools, composition)

---

## Building Terminal-Mode Acme

### Core Features Required

1. **Multi-panel tiled layout**
   - Create/destroy panels dynamically
   - Resize panels (drag borders or keyboard)
   - Each panel is a text buffer

2. **Text execution**
   - Select any text, hit a key (or chord), execute it
   - Smart detection: is this a shell command? A Trashtalk expression? A file path?
   - Output appears in new panel or replaces selection

3. **No modes**
   - Always editing, always ready to execute
   - No "insert mode" vs "command mode"
   - Minimal chrome - text dominates

4. **Scriptable interface**
   - External programs can create panels, insert text, read buffers
   - Could be via filesystem (like Acme's 9P) or Unix socket or named pipes

5. **Trashtalk awareness** (what makes it special)
   - Recognizes `@ Receiver message` syntax
   - "Execute" on a class name opens its source
   - "Execute" on a method name shows senders/implementors
   - Instance IDs are clickable/executable to inspect
   - Compilation happens automatically on save

### Framework Options for Building

| Framework | Language | Pros | Cons |
|-----------|----------|------|------|
| **Ratatui** | Rust | Very active, great docs, fast, single binary | Learning Rust |
| **Bubbletea** | Go | Elm architecture, good ecosystem, single binary | Less flexible layout |
| **Notcurses** | C | Extremely powerful, low-level control | C is tedious |
| **Pure bash** | Bash | Most aligned with Trashtalk | Significant effort, limited |

### Recommended: Ratatui (Rust)

Ratatui is the most active TUI framework right now (2024), with:
- Excellent documentation at https://ratatui.rs
- Immediate-mode rendering (you control every frame)
- Flexible layout system (nested splits)
- Good text editing primitives
- Active Discord/Matrix community
- Single static binary output

A terminal-mode Acme in Ratatui would be ~2000-4000 lines of Rust.

### Alternative: Bubbletea (Go)

If Go is preferred over Rust:
- Elm-inspired architecture (Model-Update-View)
- What gum is built on
- Good for simpler UIs, might need more work for complex tiling
- Charmbracelet ecosystem (lipgloss for styling, bubbles for components)

### The "Trashtalk Environment" Concept

The tool wouldn't just be "an editor that can run Trashtalk." It would BE the Trashtalk environment:

```
┌─────────────────────────────────────────────────────────────┐
│ Counter.trash                                         [X]  │
├─────────────────────────────────────────────────────────────┤
│ Counter subclass: Object                                    │
│   instanceVars: value step                                  │
│                                                             │
│   method: increment [                                       │
│     value := value + step.                                  │
│     ^ value                                                 │
│   ]                                                         │
│                                        ┌────────────────────┤
│   method: reset [                      │ @ Counter new      │
│     value := 0.                        │ counter_a7f2b      │
│   ]                                    │                    │
│                                        │ @ $it increment    │
├────────────────────────────────────────┤ 1                  │
│ Transcript                             │                    │
├────────────────────────────────────────┤ @ $it increment    │
│ >> @ Counter new                       │ 2                  │
│ counter_a7f2b                          │                    │
│ >> @ counter_a7f2b increment           │ value              │
│ 1                                      │ ────────────       │
│ >> Compiled Counter.trash              │ 2                  │
│                                        │                    │
└────────────────────────────────────────┴────────────────────┘
```

Features visible here:
- Source panel (editable, auto-compiles on save)
- Transcript (log of executed commands)
- Workspace (scratchpad where you execute things)
- `$it` refers to last result (like Smalltalk)
- Clicking `counter_a7f2b` would open inspector
- Clicking `increment` in source would show senders

---

## Structural Regular Expressions

A powerful concept from Rob Pike's **sam** editor (predecessor to Acme) that could serve as the query/search language for the Trashtalk environment.

### The Problem with Line-Oriented Tools

Traditional Unix tools (grep, sed, awk) operate on **lines**. But code isn't organized by lines - it's organized by structure:
- Methods span multiple lines
- Blocks are nested
- Expressions cross line boundaries

When you want to "find all methods that call `increment`", grep gives you lines, not methods.

### Structural Regex: Operating on Structure

Structural regular expressions work on **text structure**, not lines. The key insight: instead of matching lines, you define *what constitutes a unit of text* and operate on those units.

**The Four Core Commands:**

| Command | Name | Meaning |
|---------|------|---------|
| `x/regex/` | Extract | For each match, do the following command |
| `y/regex/` | Between | For each text *between* matches, do the following command |
| `g/regex/` | Guard | Only proceed if this matches |
| `v/regex/` | Inverse | Only proceed if this does NOT match |

**Composability:**
Commands chain together. The output of one becomes the input to the next.

### Examples for Trashtalk

**Find all method definitions:**
```
x/method: [^\[]+\[.*?\]/
```
This extracts each complete method block as a unit.

**Find methods containing "self":**
```
x/method: [^\[]+\[.*?\]/ g/@ self/
```
Extract methods, then guard: only keep those containing `@ self`.

**Find methods that DON'T have a return statement:**
```
x/method: [^\[]+\[.*?\]/ v/\^/
```
Extract methods, inverse guard: keep those without `^`.

**Replace "value" with "count" only inside increment methods:**
```
x/method: increment \[.*?\]/ x/value/ c/count/
```
First extract increment methods, then within those extract "value", then change to "count".

### Why This Matters for Trashtalk Environment

Instead of "find in files" that gives you a list of lines, you could have:

```
# Show all classes that have a 'new' method
x/^\w+ subclass:.*?(?=^\w+ subclass:|$)/ g/method: new/

# Find all message sends to Counter
x/@ Counter \w+/

# Extract all instance variable declarations
x/instanceVars:.*?(?=\n\n|\n  method:)/
```

This becomes a **query language** for navigating code:
- "Show me all methods longer than 10 lines"
- "Find classes that override `init`"
- "List all places where `_ivar_set` is called"

### Integration Ideas

1. **Search panel** - Type structural regex, see results as navigable list
2. **Highlight mode** - Structural regex highlights matching *regions* in source
3. **Batch refactoring** - Apply changes across matching structures
4. **Live query** - As you type, matching structures light up

### Implementation Reference

**sregx** (github.com/zyedidia/sregx) - A Go implementation of structural regular expressions. Clean, standalone, could be vendored or used as reference.

**sam command language** - The original. Documentation at http://sam.cat-v.org/

### Connection to Acme

Acme inherited many concepts from sam but simplified the interface. In Acme, you can pipe selected text through commands. Adding structural regex awareness would extend this to:
- Select "method: foo" → right-click → automatically selects the entire method body
- Run a structural search → results appear in new panel, each clickable to jump to source

This is more powerful than Smalltalk's "senders/implementors" because it's fully programmable - any query you can express in structural regex becomes a navigation command.

---

## Xiki: Lessons Learned

Xiki (2012-2015) was an ambitious project that attempted to reimagine the shell as an interactive, editable environment. It ultimately failed to gain traction, but its core ideas are worth understanding.

### What Xiki Got Right

**1. "Make a prompt anywhere just by typing a dollar sign"**

The `$` becomes a universal "execute here" marker. Any line can become a command. For Trashtalk, this translates to: `@` at the start of any line = executable expression. The whole document becomes a mix of notes and runnable code.

**2. "Expand in place"**

Directories and structures unfold where you are, not in a separate panel:
```
Counter.trash
  ▶ method: increment
  ▶ method: reset
  ▶ classMethod: new
```
Click/expand `increment` and it unfolds to show the method body right there. The structure reveals itself progressively.

**3. "Output is fully editable"**

The transcript isn't frozen history - it's live, manipulable text. You can edit a previous command and re-run it. Delete irrelevant output. Add notes inline.

**4. "Command output as interactive interface"**

When you run a command, the output isn't just text - each element is a hyperlink/command. The output IS the next input. For Trashtalk: `@ Counter new` returns `counter_a7f2b`, and that ID is clickable/executable to inspect it.

**5. "Create commands by dropping files"**

Any `.txt`, `.py`, `.rb`, `.js` file in the commands directory becomes an available command. The filesystem IS the menu system. Very Unix.

**6. "Notes with runnable commands"**

Workspaces that are simultaneously documentation and executable. Save them, share them, run them later.

### What Xiki Got Wrong (Why It Failed)

**1. Too ambitious**

It tried to do everything: "send tweets, texts, emails, try out HTML/CSS, make bootstrap layouts, interact with web browser, navigate DOM, run SQL, edit databases..." This diluted the core value proposition.

**2. Required total buy-in**

You had to use Xiki's whole environment. It wasn't composable with existing tools. You couldn't use it alongside vim or integrate it with your existing workflow.

**3. Heavy runtime**

Ruby dependency, not self-contained. Contrast with a single static binary.

**4. GUI-centric despite terminal appearance**

Heavy mouse reliance. The keyboard-driven terminal workflow didn't translate well.

**5. No clear focus**

Was it a shell replacement? An IDE? A note-taking app? A web browser? It was all of these and none of them well.

### What We Should Take for Trashtalk

| Xiki Concept | Trashtalk Translation |
|--------------|----------------------|
| `$` makes a prompt anywhere | `@` at line start = executable |
| Expand in place | Structural unfolding (method → source) |
| Output is interactive | Instance IDs are clickable/inspectable |
| Notes with runnable code | Workspace files (`.trash` extension) |
| Filesystem = commands | `trash/*.trash` = available classes |
| Editable transcript | Live workspace, not frozen history |

### The Key Difference

We stay focused. It's the **Trashtalk environment**, not a universal shell replacement. And it's built on Unix primitives (structural regex, pipes, files) rather than trying to reinvent everything.

The goal isn't "replace your terminal" - it's "be the best way to develop and interact with Trashtalk code."

---

## Options Considered (Earlier Research)

### 1. tmux (Recommended)

**Pros:**
- Already ubiquitous on Unix systems (not a new dependency)
- Entirely scriptable from bash
- Real window/pane management with splits, resizing
- Session persistence (can detach/reattach)
- Battle-tested and stable
- Philosophically aligned - it's a Unix multiplexer, not an IDE

**Key Capabilities:**
- Create sessions: `tmux new-session -d -s trashtalk`
- Create windows: `tmux new-window -t trashtalk -n 'Browser'`
- Split panes: `tmux split-window -h` (horizontal), `-v` (vertical)
- Send commands to panes: `tmux send-keys -t trashtalk:0.1 "@ Counter new" C-m`
- Target specific panes: `-t session:window.pane`
- Resize panes: `tmux resize-pane -D 10`

**Resources:**
- https://ryan.himmelwright.net/post/scripting-tmux-workspaces/
- https://github.com/junegunn/heytmux (YAML-based tmux scripting)
- https://ittavern.com/create-tmux-layouts-using-bash-scripts/

### 2. Pure Bash + ANSI Escape Sequences

**Pros:**
- Zero dependencies - everything in bash
- Most aligned with Trashtalk philosophy
- Full control over rendering

**Cons:**
- More work to implement
- Need to handle all edge cases (resize, etc.)

**Key Techniques:**
- Cursor positioning: `\e[<line>;<col>H`
- Hide/show cursor: `\e[?25l` / `\e[?25h`
- Define scroll region: `\e[<top>;<bottom>r`
- Save/restore screen: `\e[?1049h` / `\e[?1049l`
- Clear screen: `\e[2J`
- Get terminal size: `$LINES`, `$COLUMNS` or `stty size`
- Handle resize: `trap 'handle_resize' SIGWINCH`

**Resources:**
- https://github.com/dylanaraps/writing-a-tui-in-bash
- VT100 escape sequence reference

### 3. dialog / whiptail

**Pros:**
- Very lightweight
- Native bash integration
- Good for forms, menus, selection dialogs

**Cons:**
- Modal dialogs, not persistent windows
- Not suited for multi-pane browsing environment

**Best for:**
- "Select a class" dialogs
- Configuration forms
- Confirmation prompts

### 4. abom (Bash TUI Framework)

**Pros:**
- Pure bash framework
- Sets aside terminal lines as TUI bounds
- Structured approach without external deps

**Resources:**
- https://github.com/bhavanki/abom

### 5. gum (Strong Contender)

https://github.com/charmbracelet/gum

**What it is:**
A single Go binary that provides composable TUI components for shell scripts. Very Unix-y - each command does one thing, outputs to stdout, composes via pipes.

**Pros:**
- Single binary, no runtime dependencies once installed
- Designed specifically for shell script composition
- Beautiful, polished UI with minimal code
- Components map well to Smalltalk IDE needs
- Highly configurable via flags or env vars
- Active development, modern codebase

**Cons:**
- External dependency (~10MB Go binary)
- Not "pure bash" like the rest of Trashtalk

**Available Components:**

| Command | Use Case for Trashtalk |
|---------|----------------------|
| `gum choose` | Select class from list |
| `gum filter` | Fuzzy-find methods (like fzf) |
| `gum input` | Rename refactoring, new class name |
| `gum write` | Multi-line method editing |
| `gum confirm` | "Save changes?" dialogs |
| `gum spin` | Show spinner during compilation |
| `gum table` | Display class hierarchy, method list |
| `gum style` | Pretty-print transcript output |
| `gum format` | Render markdown help |
| `gum pager` | Scroll through long output |
| `gum file` | File picker for .trash files |

**Example - Class Browser in ~10 lines:**

```bash
#!/bin/bash
# Select a class
CLASS=$(ls trash/*.trash | xargs -n1 basename -s .trash | gum choose --header "Select Class")

# Select a method from that class
METHOD=$(grep -E "^\s+method:|^\s+classMethod:" "trash/$CLASS.trash" |
         sed 's/.*method: \|.*classMethod: //' |
         cut -d'[' -f1 |
         gum filter --header "Methods in $CLASS")

# Open in editor
$EDITOR "trash/$CLASS.trash" +$(grep -n "method: $METHOD" "trash/$CLASS.trash" | cut -d: -f1)
```

**Example - Styled Transcript:**

```bash
# Log levels with colors
gum style --foreground 212 ">> @ Counter new"
gum style --foreground 42 "counter_abc123"
gum style --foreground 196 --bold "Error: Method not found"
```

**Philosophy Fit:**
Despite being a Go binary, gum is philosophically aligned - it's a Unix tool, not a framework. You compose it with other tools via pipes and variables. It's closer to `jq` or `fzf` than to an IDE.

### 6. Other Tools

- **fzf** - Fuzzy finder, similar to `gum filter` but more established
- **Textual** - Python TUI framework (adds Python dependency)

---

## Recommended Architecture: tmux + bash scripts

Use tmux for window/pane management, with bash scripts providing the UI in each pane.

### Layout Concept

```
+------------------+------------------+
|                  |                  |
|  Class Browser   |     Editor       |
|  (bash script)   |   ($EDITOR)      |
|                  |                  |
+------------------+------------------+
|                  |                  |
|   Transcript     |    Workspace     |
|  (tail -f log)   |  (bash REPL)     |
|                  |                  |
+------------------+------------------+
```

### Communication Between Panes

Panes communicate through the filesystem (very Unix):
- **FIFOs/named pipes** - Real-time messaging
- **Temp files** - Pass selected class/method to editor
- **SQLite database** - Already used for instances, could store session state
- **Environment variables** - tmux can set/get env vars per session

### Component Scripts

1. **`bin/tt-browser`** - Class browser
   - Reads `trash/*.trash` files
   - Parses class/method structure
   - Uses fzf or custom menu for selection
   - Writes selection to shared state
   - Signals editor pane to open file

2. **`bin/tt-transcript`** - Transcript/log viewer
   - Tails a log file
   - Captures output from @ commands
   - Color-coded by message type

3. **`bin/tt-workspace`** - Interactive workspace
   - REPL for Trashtalk expressions
   - History support
   - Output goes to transcript

4. **`bin/tt-inspector`** - Instance inspector
   - Shows instance variables for selected object
   - Queries SQLite database
   - Live updates

5. **`bin/tt-ide`** - Main launcher
   - Creates tmux session with layout
   - Starts all component scripts
   - Handles cleanup on exit

### Example Launcher Script

```bash
#!/bin/bash
# tt-ide - Launch Trashtalk IDE

SESSION="trashtalk-ide"

# Kill existing session if any
tmux kill-session -t $SESSION 2>/dev/null

# Create new session with browser
tmux new-session -d -s $SESSION -n 'IDE'

# Split into quadrants
tmux split-window -h -t $SESSION
tmux split-window -v -t $SESSION:0.0
tmux split-window -v -t $SESSION:0.2

# Name the panes (for reference)
# 0.0 = top-left (Browser)
# 0.1 = bottom-left (Transcript)
# 0.2 = top-right (Editor)
# 0.3 = bottom-right (Workspace)

# Start components
tmux send-keys -t $SESSION:0.0 'tt-browser' C-m
tmux send-keys -t $SESSION:0.1 'tt-transcript' C-m
tmux send-keys -t $SESSION:0.3 'tt-workspace' C-m

# Attach to session
tmux attach -t $SESSION
```

---

## Smalltalk Environment Parallels

| Smalltalk | Trashtalk Equivalent |
|-----------|---------------------|
| System Browser | tt-browser (browse classes/methods) |
| Transcript | tt-transcript (log output) |
| Workspace | tt-workspace (REPL) |
| Inspector | tt-inspector (view instance state) |
| Debugger | bash -x / set -x tracing |
| Class hierarchy | @ Trash subclassesOf: |
| Method source | Read .trash files directly |
| Do it / Print it | @ commands in workspace |

---

## Next Steps

1. Prototype tt-browser with fzf for class/method selection
2. Create simple transcript logger
3. Build tmux launcher script
4. Add workspace REPL
5. Implement cross-pane communication

---

## Resources

- https://github.com/rothgar/awesome-tuis - Curated TUI list
- https://github.com/dylanaraps/writing-a-tui-in-bash - Pure bash TUI guide
- https://github.com/bhavanki/abom - Bash TUI framework
- https://fedoramagazine.org/writing-useful-terminal-tui-on-linux-with-dialog-and-jq/
