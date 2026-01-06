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
	"strconv"
	"strings"
)

//go:embed Tuplespace.trash
var _sourceCode string

var _contentHash string

func init() {
	hash := sha256.Sum256([]byte(_sourceCode))
	_contentHash = hex.EncodeToString(hash[:])
}

var ErrUnknownSelector = errors.New("unknown selector")

type Tuplespace struct {
	Class     string   `json:"class"`
	CreatedAt string   `json:"created_at"`
	Vars      []string `json:"_vars"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: Tuplespace.native <instance_id> <selector> [args...]")
		fmt.Fprintln(os.Stderr, "       Tuplespace.native --source")
		fmt.Fprintln(os.Stderr, "       Tuplespace.native --hash")
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
		fmt.Printf("Class: Tuplespace\nHash: %s\nSource length: %d bytes\n", _contentHash, len(_sourceCode))
		return
	case "--serve":
		runServeMode()
		return
	}

	if len(os.Args) < 3 {
		fmt.Fprintln(os.Stderr, "Usage: Tuplespace.native <instance_id> <selector> [args...]")
		os.Exit(1)
	}

	receiver := os.Args[1]
	selector := os.Args[2]
	args := os.Args[3:]

	if receiver == "Tuplespace" || receiver == "Tuplespace" {
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

func loadInstance(db *sql.DB, id string) (*Tuplespace, error) {
	var data string
	err := db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		return nil, err
	}
	var instance Tuplespace
	if err := json.Unmarshal([]byte(data), &instance); err != nil {
		return nil, err
	}
	return &instance, nil
}

func saveInstance(db *sql.DB, id string, instance *Tuplespace) error {
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
	if req.Instance == "" || req.Instance == "Tuplespace" || req.Instance == "Tuplespace" {
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

	var instance Tuplespace
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

func dispatch(c *Tuplespace, selector string, args []string) (string, error) {
	switch selector {
	case "field_name_":
		if len(args) < 2 {
			return "", fmt.Errorf("field_name_ requires 2 argument")
		}
		return c.Field_name(args[0], args[1])
	case "test":
		c.Test()
		return "", nil
	case "help":
		c.Help()
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

func (c *Tuplespace) Field_name(tuple_record string, field_name string) (string, error) {
	tuple_recordInt, err := strconv.Atoi(tuple_record)
	if err != nil {
		return "", err
	}
	_ = tuple_recordInt
	field_nameInt, err := strconv.Atoi(field_name)
	if err != nil {
		return "", err
	}
	_ = field_nameInt
	ts_field
	"$tuple_record"
	"$field_name"
	return "", nil
}

func (c *Tuplespace) Test() {
	ts_demo
}

func (c *Tuplespace) Help() {
	echo
	"=== Tuplespace Commands ==="
	echo
	""
	echo
	"Basic Operations:"
	echo
	"  @ Tuplespace put <type> <k1> <v1> ...        - Put tuple"
	echo
	"  @ Tuplespace get <type> [key] [value]        - Get tuples (non-destructive)"
	echo
	"  @ Tuplespace take <type> [key] [value]       - Take tuples (removes them)"
	echo
	"  @ Tuplespace wait <type> [key] [val] [timeout] - Block until tuple exists"
	echo
	""
	echo
	"Query Methods:"
	echo
	"  @ Tuplespace get_key_value <type> <key> <val>  - Get by key-value"
	echo
	"  @ Tuplespace take_key_value <type> <key> <val> - Take by key-value"
	echo
	"  @ Tuplespace wait_key_value_timeout <t> <k> <v> <sec> - Wait with timeout"
	echo
	"  @ Tuplespace field_name <tuple> <field>        - Extract field from tuple"
	echo
	""
	echo
	"Event System:"
	echo
	"  @ Tuplespace putEvent <name> [data]          - Put event tuple"
	echo
	"  @ Tuplespace putEvent_data <name> <data>     - Put event (explicit)"
	echo
	"  @ Tuplespace listen <type> <command>         - Listen for events"
	echo
	"  @ Tuplespace stopListener <type> <pid>       - Stop listener"
	echo
	""
	echo
	"Key-Value Convenience:"
	echo
	"  @ Tuplespace putKV <key> <value>             - Put key-value pair"
	echo
	"  @ Tuplespace getKV <key>                     - Get value by key"
	echo
	""
	echo
	"Utility:"
	echo
	"  @ Tuplespace list                            - List all tuples"
	echo
	"  @ Tuplespace count [type]                    - Count tuples"
	echo
	"  @ Tuplespace clear                           - Clear all tuples"
	echo
	"  @ Tuplespace info                            - System info"
	echo
	"  @ Tuplespace init                            - Initialize system"
}
