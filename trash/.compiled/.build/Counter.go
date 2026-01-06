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

//go:embed Counter.trash
var _sourceCode string

var _contentHash string

func init() {
	hash := sha256.Sum256([]byte(_sourceCode))
	_contentHash = hex.EncodeToString(hash[:])
}

var ErrUnknownSelector = errors.New("unknown selector")

type Counter struct {
	Class     string   `json:"class"`
	CreatedAt string   `json:"created_at"`
	Vars      []string `json:"_vars"`
	Value     int      `json:"value"`
	Step      int      `json:"step"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: Counter.native <instance_id> <selector> [args...]")
		fmt.Fprintln(os.Stderr, "       Counter.native --source")
		fmt.Fprintln(os.Stderr, "       Counter.native --hash")
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
		fmt.Printf("Class: Counter\nHash: %s\nSource length: %d bytes\n", _contentHash, len(_sourceCode))
		return
	case "--serve":
		runServeMode()
		return
	}

	if len(os.Args) < 3 {
		fmt.Fprintln(os.Stderr, "Usage: Counter.native <instance_id> <selector> [args...]")
		os.Exit(1)
	}

	receiver := os.Args[1]
	selector := os.Args[2]
	args := os.Args[3:]

	if receiver == "Counter" || receiver == "Counter" {
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

func loadInstance(db *sql.DB, id string) (*Counter, error) {
	var data string
	err := db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		return nil, err
	}
	var instance Counter
	if err := json.Unmarshal([]byte(data), &instance); err != nil {
		return nil, err
	}
	return &instance, nil
}

func saveInstance(db *sql.DB, id string, instance *Counter) error {
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
	if req.Instance == "" || req.Instance == "Counter" || req.Instance == "Counter" {
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

	var instance Counter
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

func dispatch(c *Counter, selector string, args []string) (string, error) {
	switch selector {
	case "getValue":
		return c.GetValue(), nil
	case "getStep":
		return c.GetStep(), nil
	case "setValue_":
		if len(args) < 1 {
			return "", fmt.Errorf("setValue_ requires 1 argument")
		}
		return c.SetValue(args[0])
	case "setStep_":
		if len(args) < 1 {
			return "", fmt.Errorf("setStep_ requires 1 argument")
		}
		return c.SetStep(args[0])
	case "increment":
		return c.Increment(), nil
	case "decrement":
		return c.Decrement(), nil
	case "incrementBy_":
		if len(args) < 1 {
			return "", fmt.Errorf("incrementBy_ requires 1 argument")
		}
		return c.IncrementBy(args[0])
	case "reset":
		c.Reset()
		return "", nil
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func dispatchClass(selector string, args []string) (string, error) {
	switch selector {
	case "description":
		return Description(), nil
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func (c *Counter) GetValue() string {
	return strconv.Itoa(c.Value + 0)
}

func (c *Counter) GetStep() string {
	return strconv.Itoa(c.Step + 0)
}

func (c *Counter) SetValue(val string) (string, error) {
	valInt, err := strconv.Atoi(val)
	if err != nil {
		return "", err
	}
	_ = valInt
	c.Value = valInt + 0
	return "", nil
}

func (c *Counter) SetStep(val string) (string, error) {
	valInt, err := strconv.Atoi(val)
	if err != nil {
		return "", err
	}
	_ = valInt
	c.Step = valInt + 0
	return "", nil
}

func (c *Counter) Increment() string {
	var newVal int
	newVal = c.Value + c.Step
	c.Value = newVal + 0
	return strconv.Itoa(newVal + 0)
}

func (c *Counter) Decrement() string {
	var newVal int
	newVal = c.Value - c.Step
	c.Value = newVal + 0
	return strconv.Itoa(newVal + 0)
}

func (c *Counter) IncrementBy(amount string) (string, error) {
	amountInt, err := strconv.Atoi(amount)
	if err != nil {
		return "", err
	}
	_ = amountInt
	var newVal int
	newVal = c.Value + amountInt
	c.Value = newVal + 0
	return "", nil
}

func (c *Counter) Reset() {
	c.Value = 0 + 0
}

func Description() string {
	return "A simple counter"
}
