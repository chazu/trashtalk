# Twin Integration Guide

Twin is a text-mode window manager that provides tiled terminal windows. Trashtalk integrates with Twin to create a Smalltalk-like development environment with multiple interactive panes.

## Prerequisites

Install Twin from source or your package manager:

```bash
# From source (https://github.com/cosmos72/twin)
git clone https://github.com/cosmos72/twin.git
cd twin
./configure && make && sudo make install

# Verify installation
which twin twsendmsg
```

Add to your PATH if needed:
```bash
export PATH="/usr/local/bin:$PATH"  # or wherever twin is installed
```

## Quick Start

### 1. Start Twin

Twin needs direct terminal control, so start it from bash (not via `@`):

```bash
# From your terminal
TERM=xterm-256color twin --hw=tty
```

Or add a helper function to your `~/.bashrc`:

```bash
twin-start() { TERM=xterm-256color twin --hw=tty; }
```

Then just run `twin-start`.

### 2. Load Trashtalk Inside Twin

In the terminal window that appears inside Twin:

```bash
source ~/.trashtalk/lib/trash.bash
```

### 3. Open Additional Windows

Now you can spawn new windows:

```bash
@ Twin open: "htop"                      # System monitor
@ Twin open: "vim ~/.trashtalk/todo.md"  # Edit a file
@ Twin openTerminal                      # Empty shell
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Twin Server                             │
│  ┌─────────────────┐  ┌──────────────┐  ┌────────────────┐  │
│  │ Terminal 1      │  │ Terminal 2   │  │ Terminal 3     │  │
│  │ ┌─────────────┐ │  │ ┌──────────┐ │  │ ┌────────────┐ │  │
│  │ │ Trashtalk   │ │  │ │ vim      │ │  │ │ htop       │ │  │
│  │ │ REPL        │ │  │ │          │ │  │ │            │ │  │
│  │ └─────────────┘ │  │ └──────────┘ │  │ └────────────┘ │  │
│  └─────────────────┘  └──────────────┘  └────────────────┘  │
│         │                    ▲              ▲               │
│         │ twsendmsg          │ fifo         │               │
│         └────────────────────┴──────────────┘               │
└─────────────────────────────────────────────────────────────┘
```

- **Twin Server**: Manages all windows, handles keyboard/mouse, draws UI
- **twsendmsg**: Command-line tool to send messages to Twin
- **Trashtalk**: Runs in one terminal, spawns others via `@ Twin open:`

## Window Operations

### Opening Windows

```bash
# Run a command in a new window
@ Twin open: "command args"

# Open empty terminal
@ Twin openTerminal

# Open file in editor
@ Twin edit: "/path/to/file.txt"

# View command output (opens in less)
@ Twin view: "ls -la /etc"

# System monitor
@ Twin monitor   # Opens htop or top
```

### Object Inspector

Inspect any Trashtalk object in a new window:

```bash
counter=$(@ Counter new)
@ $counter setValue 42

# Opens inspection in a new less window
@ Twin inspect: $counter
```

The inspector shows:
```
a Counter
  id: counter_abc123
  value: 42
  step: 1
```

### Workspace

Open a scratch workspace for experimenting:

```bash
workspace=$(@ Twin workspace)
# Opens $EDITOR with a temp file
# Write code, save, and source it
```

## Streaming Data

For live-updating displays (logs, metrics, etc.), use FIFO-based streaming:

```bash
# Open a stream window
fifo=$(@ Twin openStream: "Application Logs")

# Send data to it (from your code)
@ Twin sendTo: $fifo data: "$(date): Server started"
@ Twin sendTo: $fifo data: "$(date): Processing request..."

# When done, close the stream
@ Twin closeStream: $fifo
```

The stream window shows data as it arrives, similar to `tail -f`.

## Session Management

### Detach and Reattach

Twin supports detaching (like tmux/screen):

```bash
# Detach: Usually Ctrl+Alt+D or close terminal

# Reattach later:
@ Twin attach
```

## Example Workflows

### Development Session

```bash
# Start Twin
@ Twin start

# In Twin, load Trashtalk
source ~/.trashtalk/lib/trash.bash

# Open your project
@ Twin open: "vim ~/myproject/main.trash"
@ Twin open: "cd ~/myproject && make watch"
@ Twin openTerminal  # For testing

# Create and inspect objects
obj=$(@ MyClass new)
@ Twin inspect: $obj
```

### Debugging Session

```bash
# Open log viewer
@ Twin open: "tail -f /var/log/myapp.log"

# Open REPL for inspecting objects
@ Twin openTerminal

# In the REPL:
source ~/.trashtalk/lib/trash.bash
counter=$(@ Counter findAll | head -1)
@ $counter inspect
```

### Live Dashboard

```bash
# Create streaming windows
logs=$(@ Twin openStream: "Logs")
metrics=$(@ Twin openStream: "Metrics")

# Send updates
@ Twin sendTo: $logs data: "$(date): Started"
@ Twin sendTo: $metrics data: "CPU: 45%"
```

## Keyboard Shortcuts (Twin defaults)

| Key | Action |
|-----|--------|
| Alt+Arrow | Move between windows |
| Alt+Enter | Maximize/restore window |
| Alt+F4 | Close window |
| Ctrl+Alt+D | Detach session |

(Check Twin documentation for full list)

## Troubleshooting

### Twin hangs or doesn't display (Ghostty, iTerm2, etc.)

Some terminals use custom TERM values that Twin doesn't recognize. The `@ Twin start` command handles this automatically, but if running manually:

```bash
TERM=xterm-256color twin --hw=tty
```

### Twin not found

```bash
@ Twin isAvailable  # => "false"
```

Solution: Add Twin to your PATH in `.bashrc` or `.zshrc`.

### twsendmsg fails silently

Make sure you're running *inside* Twin. The `twsendmsg` command only works when Twin is the active window manager.

### Windows don't open

Test directly:
```bash
twsendmsg "twin" open "echo test && sleep 5"
```

## Quick Reference

**Starting Twin** (run directly, not via @):
```bash
TERM=xterm-256color twin --hw=tty
```

**Inside Twin** (via @):

| Method | Description |
|--------|-------------|
| `@ Twin howToStart` | Show startup instructions |
| `@ Twin isAvailable` | Check if Twin is installed |
| `@ Twin open: "cmd"` | Open window with command |
| `@ Twin openTerminal` | Open empty terminal |
| `@ Twin edit: "path"` | Edit file in $EDITOR |
| `@ Twin inspect: $obj` | Inspect object in window |
| `@ Twin workspace` | Open code workspace |
| `@ Twin view: "cmd"` | View command output |
| `@ Twin monitor` | Open htop/top |
| `@ Twin openStream: "title"` | Create stream window |
| `@ Twin sendTo: $f data: "msg"` | Send to stream |
| `@ Twin closeStream: $f` | Close stream |

## Future Enhancements

As the integration matures, potential additions include:

- **Custom inspector UI**: A dedicated inspector tool with navigation
- **Class browser**: Browse classes and methods in a separate pane
- **Debugger integration**: Step through code with visual feedback
- **libtw bridge**: Direct drawing for richer widgets (buttons, trees, etc.)

The current shell-based approach is intentionally simple - each tool is just a terminal process, easy to iterate on.
