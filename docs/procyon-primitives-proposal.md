# Procyon Primitives Proposal

## Overview

Analysis of remaining raw methods reveals 6 major categories of bash-specific patterns that could be wrapped in DSL syntax with Go runtime primitives.

**Current State**: 81/266 methods compile (30.5%)
**Potential**: With these primitives, ~180+ methods could compile (67%+)

---

## Category 1: Runtime/Reflection Primitives

### Bash Patterns
```bash
echo "$_CLASS"                              # Get current class
echo "$_INSTANCE"                           # Get instance ID
echo "$_RECEIVER"                           # Get receiver
id=$(_generate_instance_id "$_CLASS")       # Generate unique ID
_create_instance "$_CLASS" "$id"            # Register instance
_delete_instance "$_INSTANCE"               # Delete instance
_env_get "$_RECEIVER"                       # Get instance data as JSON
_ivar fieldName                             # Get instance variable
_ivar_set fieldName "$value"                # Set instance variable
```

### Proposed DSL Syntax
```smalltalk
@ Runtime class                    "Current class name"
@ Runtime instance                 "Current instance ID"
@ Runtime receiver                 "Current receiver"
@ Runtime generateId: className    "Generate unique ID"
@ Runtime create: className id: id "Register instance"
@ Runtime delete: instanceId       "Delete instance"
@ Runtime dataFor: object          "Get JSON data"
```

### Go Implementation
```go
// pkg/runtime/runtime.go
func (r *Runtime) Class() string { return r.currentClass }
func (r *Runtime) Instance() string { return r.currentInstance }
func (r *Runtime) GenerateID(class string) string {
    return fmt.Sprintf("%s_%s", strings.ToLower(class), uuid.New().String()[:8])
}
func (r *Runtime) Create(class, id string) {
    r.instances[id] = &Instance{Class: class, Data: make(map[string]any)}
}
func (r *Runtime) Delete(id string) { delete(r.instances, id) }
func (r *Runtime) DataFor(id string) string {
    data, _ := json.Marshal(r.instances[id].Data)
    return string(data)
}
```

### Impact
Enables: `Object.new`, `Object.delete`, `Object.class`, `Object.id`, `Object.asJson`, `Object.printString`

---

## Category 2: Console/IO Primitives

### Bash Patterns
```bash
echo "message"                    # Print to stdout
echo "error" >&2                  # Print to stderr
printf "%s: %d\n" "$name" $count  # Formatted output
read -r line                      # Read line from stdin
```

### Proposed DSL Syntax
```smalltalk
@ Console print: 'message'                      "stdout with newline"
@ Console write: 'message'                      "stdout without newline"
@ Console error: 'error message'                "stderr"
@ Console printf: '%s: %d' with: name and: count  "formatted"
@ Console readLine                              "read stdin line"
```

### Go Implementation
```go
func (c *Console) Print(msg string) { fmt.Println(msg) }
func (c *Console) Write(msg string) { fmt.Print(msg) }
func (c *Console) Error(msg string) { fmt.Fprintln(os.Stderr, msg) }
func (c *Console) Printf(format string, args ...any) { fmt.Printf(format, args...) }
func (c *Console) ReadLine() string {
    reader := bufio.NewReader(os.Stdin)
    line, _ := reader.ReadString('\n')
    return strings.TrimRight(line, "\n")
}
```

### Impact
Enables: `TestCase._fail:` (uses stderr), debug output, interactive input

---

## Category 3: Process/Command Primitives

### Bash Patterns
```bash
eval "$cmd"                           # Execute command
output=$(command)                     # Capture stdout
local exitCode=$?                     # Get exit code
return $exitCode                      # Return exit code
command &                             # Background execution
wait $pid                             # Wait for process
kill -0 $pid 2>/dev/null              # Check if running
kill -TERM $pid                       # Send signal
```

### Proposed DSL Syntax
```smalltalk
@ Shell exec: 'ls -la'                         "Execute, return stdout"
@ Shell execWithStatus: 'make test'            "Returns {output, exitCode}"
@ Shell spawn: 'long-task'                     "Background, returns pid"
@ Shell wait: pid                              "Wait for pid"
@ Shell isAlive: pid                           "Check if running"
@ Shell signal: 'TERM' to: pid                 "Send signal"
@ Shell kill: pid                              "SIGKILL"
```

### Go Implementation
```go
func (s *Shell) Exec(cmd string) string {
    out, _ := exec.Command("sh", "-c", cmd).Output()
    return string(out)
}

func (s *Shell) ExecWithStatus(cmd string) (string, int) {
    cmd := exec.Command("sh", "-c", cmd)
    out, err := cmd.CombinedOutput()
    exitCode := 0
    if exitErr, ok := err.(*exec.ExitError); ok {
        exitCode = exitErr.ExitCode()
    }
    return string(out), exitCode
}

func (s *Shell) Spawn(cmd string) int {
    proc := exec.Command("sh", "-c", cmd)
    proc.Start()
    return proc.Process.Pid
}

func (s *Shell) IsAlive(pid int) bool {
    proc, err := os.FindProcess(pid)
    if err != nil { return false }
    return proc.Signal(syscall.Signal(0)) == nil
}

func (s *Shell) Signal(sig string, pid int) error {
    proc, _ := os.FindProcess(pid)
    sigNum := syscall.SIGTERM // map sig string to signal
    return proc.Signal(sigNum)
}
```

### Impact
Enables: `Process.exec:`, `Process.run:`, `Process.spawn:`, `Process.wait`, all signal methods

---

## Category 4: HTTP Primitives

### Bash Patterns
```bash
curl -s "$url"                                    # GET
curl -s -X POST -d "$data" "$url"                 # POST
curl -s -H "Header: value" "$url"                 # With headers
curl -s -o /dev/null -w "%{http_code}" "$url"     # Status only
curl -o "$file" "$url"                            # Download
```

### Proposed DSL Syntax
```smalltalk
@ Http get: url                                    "Simple GET"
@ Http post: data to: url                          "POST JSON"
@ Http put: data to: url                           "PUT JSON"
@ Http delete: url                                 "DELETE"
@ Http get: url headers: headerDict                "With headers"
@ Http status: url                                 "Status code only"
@ Http download: url to: filepath                  "Download file"
```

### Go Implementation
```go
func (h *Http) Get(url string) string {
    resp, _ := http.Get(url)
    defer resp.Body.Close()
    body, _ := io.ReadAll(resp.Body)
    return string(body)
}

func (h *Http) PostTo(data, url string) string {
    resp, _ := http.Post(url, "application/json", strings.NewReader(data))
    defer resp.Body.Close()
    body, _ := io.ReadAll(resp.Body)
    return string(body)
}

func (h *Http) Status(url string) int {
    resp, err := http.Head(url)
    if err != nil { return 0 }
    return resp.StatusCode
}

func (h *Http) DownloadTo(url, filepath string) error {
    resp, _ := http.Get(url)
    defer resp.Body.Close()
    out, _ := os.Create(filepath)
    defer out.Close()
    _, err := io.Copy(out, resp.Body)
    return err
}
```

### Impact
Enables: All `Curl` class methods (~15 methods)

---

## Category 5: Time Primitives

### Bash Patterns
```bash
date +%s                          # Unix timestamp
date +"%Y-%m-%d %H:%M:%S"         # Formatted date
date -r $timestamp                # Format timestamp
sleep 0.1                         # Delay
```

### Proposed DSL Syntax
```smalltalk
@ Time now                                  "Unix timestamp (int)"
@ Time nowFormatted: '%Y-%m-%d %H:%M:%S'    "Formatted current time"
@ Time format: timestamp as: '%Y-%m-%d'     "Format timestamp"
@ Time sleep: 0.1                           "Sleep seconds (float)"
@ Time since: startTime                     "Duration in seconds"
```

### Go Implementation
```go
func (t *Time) Now() int64 { return time.Now().Unix() }

func (t *Time) NowFormatted(format string) string {
    return time.Now().Format(goTimeFormat(format))
}

func (t *Time) FormatAs(timestamp int64, format string) string {
    return time.Unix(timestamp, 0).Format(goTimeFormat(format))
}

func (t *Time) Sleep(seconds float64) {
    time.Sleep(time.Duration(seconds * float64(time.Second)))
}

func (t *Time) Since(start int64) int64 {
    return time.Now().Unix() - start
}
```

### Impact
Enables: `Process.startTime`, `Process.endTime`, `Process.info`, timing in tests

---

## Category 6: Environment Primitives

### Bash Patterns
```bash
$HOME, $EDITOR, $PATH             # Environment variables
${EDITOR:-vi}                     # With default
export FOO=bar                    # Set variable
which curl                        # Find executable
```

### Proposed DSL Syntax
```smalltalk
@ Env get: 'HOME'                          "Get env var"
@ Env get: 'EDITOR' default: 'vi'          "Get with default"
@ Env set: 'FOO' to: 'bar'                 "Set env var"
@ Env which: 'curl'                        "Find executable path"
@ Env has: 'EDITOR'                        "Check if set"
```

### Go Implementation
```go
func (e *Env) Get(name string) string { return os.Getenv(name) }

func (e *Env) GetDefault(name, def string) string {
    if v := os.Getenv(name); v != "" { return v }
    return def
}

func (e *Env) Set(name, value string) { os.Setenv(name, value) }

func (e *Env) Which(name string) string {
    path, err := exec.LookPath(name)
    if err != nil { return "" }
    return path
}

func (e *Env) Has(name string) bool {
    _, exists := os.LookupEnv(name)
    return exists
}
```

### Impact
Enables: `Tool.path`, `Object.edit`, editor integration

---

## Implementation Priority

### Phase 1: High Impact (30+ methods)
1. **Runtime primitives** - Enables Object lifecycle, reflection
2. **Console primitives** - Enables TestCase, debug output

### Phase 2: Medium Impact (20+ methods)
3. **Shell primitives** - Enables Process class
4. **Http primitives** - Enables Curl class

### Phase 3: Convenience (10+ methods)
5. **Time primitives** - Enables timing, duration tracking
6. **Env primitives** - Enables tool integration

---

## Parser Changes Required

### New Expression Types
```go
// RuntimePrimitiveExpr - @ Runtime method: args
type RuntimePrimitiveExpr struct {
    Operation string   // "class", "instance", "generateId", etc.
    Args      []Expr
}

// ConsolePrimitiveExpr - @ Console method: args
type ConsolePrimitiveExpr struct {
    Operation string   // "print", "error", "printf"
    Args      []Expr
}

// ShellPrimitiveExpr - @ Shell method: args
type ShellPrimitiveExpr struct {
    Operation string   // "exec", "spawn", "signal"
    Args      []Expr
}

// Similar for Http, Time, Env
```

### Detection in Parser
```go
func isRuntimePrimitive(receiver string, selector string) bool {
    if receiver != "Runtime" { return false }
    ops := []string{"class", "instance", "receiver", "generateId_",
                    "create_id_", "delete_", "dataFor_"}
    return contains(ops, selector)
}
```

---

## Estimated Effort

| Category | Parser | IR | Codegen | Tests | Total |
|----------|--------|-----|---------|-------|-------|
| Runtime  | 100    | 50  | 150     | 100   | 400   |
| Console  | 50     | 30  | 80      | 50    | 210   |
| Shell    | 80     | 50  | 200     | 100   | 430   |
| Http     | 80     | 50  | 200     | 100   | 430   |
| Time     | 40     | 20  | 60      | 40    | 160   |
| Env      | 40     | 20  | 60      | 40    | 160   |
| **Total**| 390    | 220 | 750     | 430   | **1790** |

~1800 lines of Go code across all categories.

---

## Alternative: Unified Primitive System

Instead of separate expression types, could use a single `SystemPrimitiveExpr`:

```go
type SystemPrimitiveExpr struct {
    Module    string   // "Runtime", "Console", "Shell", "Http", "Time", "Env"
    Operation string   // "print", "exec", "get", etc.
    Args      []Expr
}
```

This reduces parser complexity and makes adding new primitives easier.

---

## Special Cases: Fundamentally Bash-Dependent

### Block Execution (Cannot be natively compiled)

Block execution requires `eval` to run dynamically-generated bash code:

```bash
# Block.value executes arbitrary code stored as a string
eval "$code"
```

**Why it can't compile to Go**: The code inside blocks is bash code generated at compile time. Native binaries can't execute bash without shelling out.

**Options**:
1. **Hybrid approach**: Native binary shells out to bash for block execution
2. **Compile blocks to Go closures**: Would require compiling block bodies to Go at native compile time
3. **Keep blocks in bash fallback**: Accept that block-heavy code stays interpreted

**Recommendation**: Option 2 (compile blocks to closures) is the long-term solution but requires significant compiler work. For now, keep Block as bash fallback.

### Class Introspection (Requires Runtime Metadata)

```bash
declare -F | grep "__${_CLASS}__test"   # Find test methods
local super_var="__${current}__superclass"
current="${!super_var:-}"              # Walk inheritance chain
```

**Go equivalent**: Requires building class metadata at compile time:

```go
// Generated at compile time for each class
var TestCaseMetadata = ClassMetadata{
    Name: "TestCase",
    Superclass: "Object",
    Methods: []string{"testFoo", "testBar", "setUp", "tearDown"},
    TestMethods: []string{"testFoo", "testBar"},
}

// @ Class methodsMatching: pattern
func (c *Class) MethodsMatching(pattern string) []string {
    var matches []string
    for _, m := range c.metadata.Methods {
        if regexp.MustCompile(pattern).MatchString(m) {
            matches = append(matches, m)
        }
    }
    return matches
}
```

### Instance Variable Access (Requires Runtime State)

```bash
_ivar fieldName        # Read ivar
_ivar_set fieldName $v # Write ivar
```

For native compilation, instance variables become Go struct fields:

```go
// Counter becomes:
type Counter struct {
    ObjectBase
    value int
    step  int
}

// @ counter getValue compiles to:
func (c *Counter) GetValue() int {
    return c.value
}
```

This is already how Procyon works - the complexity is in the Object.new pattern which must work for any class.

---

## Quick Wins: Highest ROI Primitives

Based on method count and implementation complexity:

| Primitive | Methods Enabled | Complexity | Priority |
|-----------|-----------------|------------|----------|
| `@ Console error:` | 5+ | Low | 1 |
| `@ Runtime class` | 3 | Low | 1 |
| `@ Runtime instance` | 2 | Low | 1 |
| `@ Time now` | 4 | Low | 2 |
| `@ Time sleep:` | 2 | Low | 2 |
| `@ Shell exec:` | 10+ | Medium | 3 |
| `@ Http get:` | 5 | Medium | 3 |

**Phase 1 Target**: Console + Runtime basics = ~10 methods = 34% compilation rate
