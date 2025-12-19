## Path B: Smalltalk Inside twterm + Message Passing

The idea is to use Twin as your window manager and terminal multiplexer, with your Smalltalk living inside a terminal window and spawning/coordinating other windows via shell commands.

### Basic Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    twin (server)                         │
│  ┌─────────────────┐  ┌──────────────┐  ┌────────────┐  │
│  │ twterm          │  │ twterm       │  │ twterm     │  │
│  │ ┌─────────────┐ │  │ ┌──────────┐ │  │ ┌────────┐ │  │
│  │ │ bash        │ │  │ │ inspector│ │  │ │ workspace│ │  │
│  │ │ smalltalk   │ │  │ │ helper   │ │  │ │ $EDITOR │ │  │
│  │ │ REPL        │ │  │ └──────────┘ │  │ └────────┘ │  │
│  │ └─────────────┘ │  └──────────────┘  └────────────┘  │
│  └─────────────────┘                                     │
│         │                    ▲              ▲            │
│         │ twsendmsg          │ fifo/socket  │            │
│         └────────────────────┴──────────────┘            │
└─────────────────────────────────────────────────────────┘
```

### How twsendmsg Works

Twin's message system lets clients talk to each other. The key commands:

```bash
# Open a new terminal running a command
twsendmsg "twin" open "vim /tmp/scratch.st"

# Send to twterm's message port (if one exists)
twsendmsg "twterm" open   # opens a new shell window

# Built-in control codes:
#   quit (0), restart (1), open (2)
```

The `"twin"` message port is built into the server. When you send `open "command"`, it spawns a new twterm running that command.

### Concrete Workflow

**1. Start Twin and your Smalltalk REPL:**
```bash
$ twin
# Inside twin, a terminal opens. Run:
$ st  # your bash smalltalk
```

**2. From Smalltalk, open an inspector window:**
```smalltalk
" In your Smalltalk, define something like: "
Object inspect: anObject [
    | tmpfile |
    tmpfile := '/tmp/st-inspect-' , (Date now asUnixTime asString).
    anObject printRepresentationTo: tmpfile.
    System run: 'twsendmsg "twin" open "less ' , tmpfile , '"'.
]
```

**3. Open a workspace/editor:**
```smalltalk
Workspace open [
    | file |
    file := '/tmp/st-workspace-' , (self nextId asString) , '.st'.
    '' writeTo: file.
    System run: 'twsendmsg "twin" open "$EDITOR ' , file , '; st-eval ' , file , '"'.
]
```

**4. For richer communication - spawn a helper with a FIFO:**
```smalltalk
Inspector openOn: anObject [
    | fifoPath |
    fifoPath := '/tmp/st-inspector-' , (self nextId asString).
    System run: 'mkfifo ' , fifoPath.

    " Spawn inspector UI in new window, reading from FIFO "
    System run: 'twsendmsg "twin" open "st-inspector-ui ' , fifoPath , '"'.

    " Now we can stream updates to it "
    self streamObject: anObject to: fifoPath.
]
```

### What This Gets You

**Pros:**
- Zero C code to start
- Twin handles all window management, mouse, resize, etc.
- Each "tool" window is just a terminal process - easy to iterate
- Attach/detach works (leave twin running, reconnect later)
- Can mix in any CLI tool (vim, less, fzf, etc.)

**Cons:**
- Your Smalltalk isn't drawing directly - it's inside a terminal
- Communication is via shell commands + files/fifos (not instant)
- Can't create custom widgets (buttons, etc.) without Path A
- Each window is a separate process (vs. objects in one image)

### Quick Test You Can Do Now

If you have Twin installed:

```bash
# Start twin
twin --hw=tty   # or just 'twin' if X11 is available

# In the terminal that appears:
twsendmsg "twin" open "htop"      # spawns htop in new window
twsendmsg "twin" open "vim"       # spawns vim in another
```

Drag windows around, see how it feels. If this model works for your dev workflow, you can build on it incrementally - adding richer inspector UIs, browser windows, etc. as separate helper scripts.

### Graduating to Path A Later

The nice thing is this isn't a dead end. If you find yourself wanting:
- Custom drawing (syntax-highlighted code panes)
- Real widgets (clickable class browsers)
- Lower-latency updates

...you can write a single `st-ui-bridge` that speaks libtw, and your Smalltalk talks to *that* instead of spawning twterms. The conceptual model stays the same, just the rendering gets richer.
