package main

import (
	"crypto/sha256"
	"database/sql"
	_ "embed"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"

	_ "github.com/mattn/go-sqlite3"
)

// ErrUnknownSelector indicates the native binary doesn't implement this method
var ErrUnknownSelector = errors.New("unknown selector")

//go:embed Counter.trash
var sourceCode string

// Computed at init time from embedded source
var contentHash string

func init() {
	hash := sha256.Sum256([]byte(sourceCode))
	contentHash = hex.EncodeToString(hash[:])
}

// Counter represents the instance data structure
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
		fmt.Fprintln(os.Stderr, "       Counter.native --source    # print embedded source")
		fmt.Fprintln(os.Stderr, "       Counter.native --hash      # print content hash")
		os.Exit(1)
	}

	// Handle metadata queries
	switch os.Args[1] {
	case "--source":
		fmt.Print(sourceCode)
		return
	case "--hash":
		fmt.Println(contentHash)
		return
	case "--info":
		fmt.Printf("Class: Counter\nHash: %s\nSource length: %d bytes\n", contentHash, len(sourceCode))
		return
	}

	if len(os.Args) < 3 {
		fmt.Fprintln(os.Stderr, "Usage: Counter.native <instance_id> <selector> [args...]")
		os.Exit(1)
	}

	receiver := os.Args[1]
	selector := os.Args[2]
	args := os.Args[3:]

	db, err := openDB()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	// Try to load instance - if it fails, this might be a class method call
	instance, err := loadInstance(db, receiver)
	if err != nil {
		// Instance doesn't exist - check if this is a class method we handle
		// For now, we don't implement any class methods, so fall back to Bash
		os.Exit(200)
	}

	result, err := dispatch(instance, selector, args)
	if err != nil {
		// Exit code 200 = unknown selector, signals dispatcher to fall back to Bash
		if errors.Is(err, ErrUnknownSelector) {
			os.Exit(200)
		}
		fmt.Fprintf(os.Stderr, "Error dispatching %s: %v\n", selector, err)
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

func dispatch(c *Counter, selector string, args []string) (string, error) {
	switch selector {
	case "getValue":
		return c.GetValue(), nil
	case "getStep":
		return c.GetStep(), nil
	case "setValue:":
		if len(args) < 1 {
			return "", fmt.Errorf("setValue: requires 1 argument")
		}
		return c.SetValue(args[0])
	case "setStep:":
		if len(args) < 1 {
			return "", fmt.Errorf("setStep: requires 1 argument")
		}
		return c.SetStep(args[0])
	case "increment":
		return c.Increment(), nil
	case "decrement":
		return c.Decrement(), nil
	case "incrementBy:":
		if len(args) < 1 {
			return "", fmt.Errorf("incrementBy: requires 1 argument")
		}
		return c.IncrementBy(args[0])
	case "reset":
		c.Reset()
		return "", nil
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

// Method implementations matching the Trashtalk semantics

func (c *Counter) GetValue() string {
	return strconv.Itoa(c.Value)
}

func (c *Counter) GetStep() string {
	return strconv.Itoa(c.Step)
}

func (c *Counter) SetValue(val string) (string, error) {
	v, err := strconv.Atoi(val)
	if err != nil {
		return "", err
	}
	c.Value = v
	return "", nil
}

func (c *Counter) SetStep(val string) (string, error) {
	v, err := strconv.Atoi(val)
	if err != nil {
		return "", err
	}
	c.Step = v
	return "", nil
}

func (c *Counter) Increment() string {
	c.Value += c.Step
	return strconv.Itoa(c.Value)
}

func (c *Counter) Decrement() string {
	c.Value -= c.Step
	return strconv.Itoa(c.Value)
}

func (c *Counter) IncrementBy(amount string) (string, error) {
	a, err := strconv.Atoi(amount)
	if err != nil {
		return "", err
	}
	c.Value += a
	return "", nil
}

func (c *Counter) Reset() {
	c.Value = 0
}
