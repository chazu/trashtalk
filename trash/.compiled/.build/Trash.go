package main

import (
	"bufio"
	"crypto/sha256"
	"database/sql"
	_ "embed"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

//go:embed Trash.trash
var _sourceCode string

var _contentHash string

func init() {
	hash := sha256.Sum256([]byte(_sourceCode))
	_contentHash = hex.EncodeToString(hash[:])
}

var ErrUnknownSelector = errors.New("unknown selector")

type Trash struct {
	Class     string   `json:"class"`
	CreatedAt string   `json:"created_at"`
	Vars      []string `json:"_vars"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: Trash.native <instance_id> <selector> [args...]")
		fmt.Fprintln(os.Stderr, "       Trash.native --source")
		fmt.Fprintln(os.Stderr, "       Trash.native --hash")
		os.Exit(1)
	}

	switch os.Args[1] {
	case "--source":
		fmt.Print(_sourceCode)
		return
	case "--hash":
		fmt.Println(_contentHash)
		return
	case "--info":
		fmt.Printf("Class: Trash\nHash: %s\nSource length: %d bytes\n", _contentHash, len(_sourceCode))
		return
	case "--serve":
		runServeMode()
		return
	}

	if len(os.Args) < 3 {
		fmt.Fprintln(os.Stderr, "Usage: Trash.native <instance_id> <selector> [args...]")
		os.Exit(1)
	}

	receiver := os.Args[1]
	selector := os.Args[2]
	args := os.Args[3:]

	if receiver == "Trash" || receiver == "Trash" {
		result, err := dispatchClass(selector, args)
		if err != nil {
			if errors.Is(err, ErrUnknownSelector) {
				os.Exit(200)
			}
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		if result != "" {
			fmt.Println(result)
		}
		return
	}

	db, err := openDB()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	instance, err := loadInstance(db, receiver)
	if err != nil {
		os.Exit(200)
	}

	result, err := dispatch(instance, selector, args)
	if err != nil {
		if errors.Is(err, ErrUnknownSelector) {
			os.Exit(200)
		}
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := saveInstance(db, receiver, instance); err != nil {
		fmt.Fprintf(os.Stderr, "Error saving instance: %v\n", err)
		os.Exit(1)
	}

	if result != "" {
		fmt.Println(result)
	}
}

func openDB() (*sql.DB, error) {
	dbPath := os.Getenv("SQLITE_JSON_DB")
	if dbPath == "" {
		home, _ := os.UserHomeDir()
		dbPath = filepath.Join(home, ".trashtalk", "instances.db")
	}
	return sql.Open("sqlite3", dbPath)
}

func loadInstance(db *sql.DB, id string) (*Trash, error) {
	var data string
	err := db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		return nil, err
	}
	var instance Trash
	if err := json.Unmarshal([]byte(data), &instance); err != nil {
		return nil, err
	}
	return &instance, nil
}

func saveInstance(db *sql.DB, id string, instance *Trash) error {
	data, err := json.Marshal(instance)
	if err != nil {
		return err
	}
	_, err = db.Exec("INSERT OR REPLACE INTO instances (id, data) VALUES (?, json(?))", id, string(data))
	return err
}

func sendMessage(receiver interface{}, selector string, args ...interface{}) (string, error) {
	receiverStr := fmt.Sprintf("%v", receiver)
	cmdArgs := []string{receiverStr, selector}
	for _, arg := range args {
		cmdArgs = append(cmdArgs, fmt.Sprintf("%v", arg))
	}
	home, _ := os.UserHomeDir()
	dispatchScript := filepath.Join(home, ".trashtalk", "bin", "trash-send")
	cmd := exec.Command(dispatchScript, cmdArgs...)
	output, err := cmd.Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(output)), nil
}

// ServeRequest is the JSON request format for --serve mode
type ServeRequest struct {
	Instance string   `json:"instance"`
	Selector string   `json:"selector"`
	Args     []string `json:"args"`
}

// ServeResponse is the JSON response format for --serve mode
type ServeResponse struct {
	Instance string `json:"instance,omitempty"`
	Result   string `json:"result,omitempty"`
	ExitCode int    `json:"exit_code"`
	Error    string `json:"error,omitempty"`
}

func runServeMode() {
	db, err := openDB()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	scanner := bufio.NewScanner(os.Stdin)
	// Increase buffer for large instance JSON
	buf := make([]byte, 1048576)
	scanner.Buffer(buf, len(buf))

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		var req ServeRequest
		if err := json.Unmarshal([]byte(line), &req); err != nil {
			respond(ServeResponse{
				Error:    "invalid JSON: " + err.Error(),
				ExitCode: 1,
			})
			continue
		}

		resp := handleServeRequest(db, &req)
		respond(resp)
	}
}

func respond(resp ServeResponse) {
	out, _ := json.Marshal(resp)
	fmt.Println(string(out))
}

func handleServeRequest(db *sql.DB, req *ServeRequest) ServeResponse {
	if req.Instance == "" || req.Instance == "Trash" || req.Instance == "Trash" {
		result, err := dispatchClass(req.Selector, req.Args)
		if err != nil {
			if errors.Is(err, ErrUnknownSelector) {
				return ServeResponse{ExitCode: 200}
			}
			return ServeResponse{
				Error:    err.Error(),
				ExitCode: 1,
			}
		}
		return ServeResponse{
			ExitCode: 0,
			Result:   result,
		}
	}

	var instance Trash
	if err := json.Unmarshal([]byte(req.Instance), &instance); err != nil {
		return ServeResponse{
			Error:    "invalid instance JSON: " + err.Error(),
			ExitCode: 1,
		}
	}

	result, err := dispatch(&instance, req.Selector, req.Args)
	if err != nil {
		if errors.Is(err, ErrUnknownSelector) {
			return ServeResponse{ExitCode: 200}
		}
		return ServeResponse{
			Error:    err.Error(),
			ExitCode: 1,
		}
	}

	updatedJSON, _ := json.Marshal(&instance)
	return ServeResponse{
		ExitCode: 0,
		Instance: string(updatedJSON),
		Result:   result,
	}
}

func dispatch(c *Trash, selector string, args []string) (string, error) {
	switch selector {
	case "info":
		c.Info()
		return "", nil
	case "reload":
		c.Reload()
		return "", nil
	case "stats":
		c.Stats()
		return "", nil
	case "help":
		c.Help()
		return "", nil
	case "version":
		c.Version()
		return "", nil
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func dispatchClass(selector string, args []string) (string, error) {
	switch selector {
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func (c *Trash) Info() {
	echo
	"=== Trash System Information ==="
	echo
	"Version: 0.1.0"
	echo
	"Directory: $TRASHDIR"
	echo
	"Objects: $(@ self listObjects | wc -l)"
	echo
	"Traits: $(@ self listTraits | wc -l)"
	echo
	"Active Processes: $(@ Process listProcesses | wc -l)"
	echo
	"Database: $SQLITE_JSON_DB"
}

func (c *Trash) Reload() {
	echo
	"Reloading all object stubs..."
	initialize_trash
	echo
	"Reload complete"
}

func (c *Trash) Stats() {
	echo
	"=== Trash System Statistics ==="
	echo
	"Objects: $(@ self listObjects | wc -l)"
	echo
	"Traits: $(@ self listTraits | wc -l)"
	echo
	"KV entries: $(kvlist 2>/dev/null | wc -l)"
	echo
	"Active processes: $(@ Process listProcesses 2>/dev/null | wc -l)"
	echo
	"Memory usage: $(du -sh \"$TRASHDIR\" | cut -f1)"
	echo
	"KV store size: $(du -sh \"$KV_USER_DIR\" 2>/dev/null | cut -f1 || echo \"0B\")"
}

func (c *Trash) Help() {
	echo
	"=== Trash System Commands ==="
	echo
	"@ Trash info                    - Show system information"
	echo
	"@ Trash listObjects             - List all objects"
	echo
	"@ Trash listTraits              - List all traits"
	echo
	"@ Trash methodsFor <object>     - Show methods for object"
	echo
	"@ Trash hierarchyFor <object>   - Show inheritance hierarchy"
	echo
	"@ Trash new <class>              - Create class skeleton and edit"
	echo
	"@ Trash edit <class>             - Edit class and recompile on save"
	echo
	"@ Trash createObject <name> [super] - Create new object (no edit)"
	echo
	"@ Trash removeObject <name>     - Remove object"
	echo
	"@ Trash reload                  - Reload all object stubs"
	echo
	"@ Trash reloadClass <class>     - Hot reload a single class"
	echo
	"@ Trash compileAndReload <class> - Compile and reload from .trash"
	echo
	"@ Trash stats                   - Show system statistics"
	echo
	"@ Trash cleanup                 - Clean up system"
	echo
	"@ Trash version                 - Show version information"
	echo
	"@ Trash inspect <object>        - Detailed object inspection"
	echo
	"@ Trash repl                    - Launch interactive REPL"
	echo
	""
	echo
	"=== Instance Queries (use class methods) ==="
	echo
	"@ <Class> findAll               - Find all instances of class"
	echo
	"@ <Class> find '<predicate>'    - Find with predicate"
	echo
	"@ <Class> count                 - Count instances of class"
	echo
	"@ Store listClasses             - List classes with instances"
	echo
	""
	echo
	"=== Debugging ==="
	echo
	"@ Trash showStack               - Show current call stack"
	echo
	"@ Trash clearStack              - Clear call stack"
	echo
	""
	echo
	"=== Source Introspection ==="
	echo
	"@ Trash sourceFor: <class>      - Get embedded source code"
	echo
	"@ Trash hashFor: <class>        - Get source hash (SHA-256)"
}

func (c *Trash) Version() {
	echo
	"Trash System v$TRASH_VERSION"
	echo
	"Author: $TRASH_AUTHOR"
	echo
	"Description: $TRASH_DESCRIPTION"
	echo
	"Bash version: $BASH_VERSION"
	echo
	"System: $(uname -s) $(uname -r)"
}
