# File API

File provides an interface for file system operations - reading, writing, and managing files.

## Quick Operations (Class Methods)

For one-off file operations:

```bash
# Read a file
content=$(@ File read: "/path/to/file.txt")

# Write to a file
@ File write: "content" to: "/path/to/file.txt"

# Check if file exists
@ File exists: "/path/to/file.txt"  # => "true" or "false"

# Delete a file
@ File delete: "/path/to/file.txt"
```

## Managed Files (Instance Methods)

For files you need to work with repeatedly:

```bash
# Reference an existing file
file=$(@ File at: "/path/to/file.txt")

# Create a temp file
file=$(@ File temp)
file=$(@ File tempWithPrefix: "myapp-")

# Create a FIFO (named pipe)
fifo=$(@ File mkfifo: "/tmp/myfifo")
```

### Reading and Writing

```bash
# Read entire file
content=$(@ $file read)

# Write (overwrites existing content)
@ $file write: "new content"
@ $file writeLine: "content with newline"

# Append
@ $file append: "more content"
@ $file appendLine: "more content with newline"
```

### File Information

```bash
@ $file exists        # => "true" or "false"
@ $file isFile        # => "true" or "false"
@ $file isDirectory   # => "true" or "false"
@ $file isFifo        # => "true" or "false"
@ $file size          # => size in bytes
@ $file modificationTime  # => unix timestamp
```

### Path Operations

```bash
@ $file path          # => "/path/to/file.txt"
@ $file directory     # => "/path/to"
@ $file basename      # => "file.txt"
@ $file extension     # => "txt"
@ $file stem          # => "file"
```

### File Operations

```bash
@ $file delete                    # Remove the file
@ $file copyTo: "/new/path.txt"   # Copy to new location
@ $file moveTo: "/new/path.txt"   # Move/rename
@ $file touch                     # Create or update timestamp
@ $file info                      # Print file information
```

## Examples

### Process a config file

```bash
config=$(@ File at: "/etc/myapp/config.txt")

if [[ $(@ $config exists) == "true" ]]; then
    content=$(@ $config read)
    echo "Config: $content"
else
    @ $config write: "default=true"
fi
```

### Create and use a temp file

```bash
tmp=$(@ File temp)
@ $tmp writeLine: "temporary data"
@ $tmp appendLine: "more data"

# Process the file
cat "$(@ $tmp path)"

# Clean up
@ $tmp delete
```

### Log file with append

```bash
log=$(@ File at: "/var/log/myapp.log")
@ $log appendLine: "$(date): Application started"
@ $log appendLine: "$(date): Processing complete"
```

### Named pipe for IPC

```bash
fifo=$(@ File mkfifo: "/tmp/myapp-pipe")

# In one process: write to pipe
@ $fifo writeLine: "message from producer"

# In another process: read from pipe
message=$(@ $fifo read)
```

## Instance Variables

| Variable | Description |
|----------|-------------|
| `path` | The file system path |

## Quick Reference

### Class Methods
| Method | Description |
|--------|-------------|
| `@ File at: <path>` | Create File object for path |
| `@ File temp` | Create temp file |
| `@ File tempWithPrefix: <prefix>` | Temp file with prefix |
| `@ File mkfifo: <path>` | Create named pipe |
| `@ File read: <path>` | Read file contents |
| `@ File write: <content> to: <path>` | Write to file |
| `@ File exists: <path>` | Check if exists |
| `@ File delete: <path>` | Delete file |

### Instance Methods
| Method | Description |
|--------|-------------|
| `@ $file read` | Read contents |
| `@ $file write: <content>` | Write (overwrite) |
| `@ $file writeLine: <content>` | Write with newline |
| `@ $file append: <content>` | Append |
| `@ $file appendLine: <content>` | Append with newline |
| `@ $file exists` | Check existence |
| `@ $file isFile` | Is regular file? |
| `@ $file isDirectory` | Is directory? |
| `@ $file isFifo` | Is named pipe? |
| `@ $file size` | Size in bytes |
| `@ $file path` | Get path |
| `@ $file directory` | Get parent dir |
| `@ $file basename` | Get filename |
| `@ $file extension` | Get extension |
| `@ $file stem` | Filename without ext |
| `@ $file delete` | Delete file |
| `@ $file copyTo: <path>` | Copy file |
| `@ $file moveTo: <path>` | Move/rename |
| `@ $file touch` | Create/update mtime |
| `@ $file modificationTime` | Get mtime |
| `@ $file info` | Show file info |
