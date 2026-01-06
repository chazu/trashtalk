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

//go:embed Store.trash
var _sourceCode string

var _contentHash string

func init() {
	hash := sha256.Sum256([]byte(_sourceCode))
	_contentHash = hex.EncodeToString(hash[:])
}

var ErrUnknownSelector = errors.New("unknown selector")

type Store struct {
	Class     string   `json:"class"`
	CreatedAt string   `json:"created_at"`
	Vars      []string `json:"_vars"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: Store.native <instance_id> <selector> [args...]")
		fmt.Fprintln(os.Stderr, "       Store.native --source")
		fmt.Fprintln(os.Stderr, "       Store.native --hash")
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
		fmt.Printf("Class: Store\nHash: %s\nSource length: %d bytes\n", _contentHash, len(_sourceCode))
		return
	case "--serve":
		runServeMode()
		return
	}

	if len(os.Args) < 3 {
		fmt.Fprintln(os.Stderr, "Usage: Store.native <instance_id> <selector> [args...]")
		os.Exit(1)
	}

	receiver := os.Args[1]
	selector := os.Args[2]
	args := os.Args[3:]

	if receiver == "Store" || receiver == "Store" {
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

func loadInstance(db *sql.DB, id string) (*Store, error) {
	var data string
	err := db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		return nil, err
	}
	var instance Store
	if err := json.Unmarshal([]byte(data), &instance); err != nil {
		return nil, err
	}
	return &instance, nil
}

func saveInstance(db *sql.DB, id string, instance *Store) error {
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
	if req.Instance == "" || req.Instance == "Store" || req.Instance == "Store" {
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

	var instance Store
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

func dispatch(c *Store, selector string, args []string) (string, error) {
	switch selector {
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func dispatchClass(selector string, args []string) (string, error) {
	switch selector {
	case "init":
		Init()
		return "", nil
	case "getInstance_":
		if len(args) < 1 {
			return "", fmt.Errorf("getInstance_ requires 1 argument")
		}
		return GetInstance(args[0])
	case "deleteInstance_":
		if len(args) < 1 {
			return "", fmt.Errorf("deleteInstance_ requires 1 argument")
		}
		return DeleteInstance(args[0])
	case "getClass_":
		if len(args) < 1 {
			return "", fmt.Errorf("getClass_ requires 1 argument")
		}
		return GetClass(args[0])
	case "findByClass_":
		if len(args) < 1 {
			return "", fmt.Errorf("findByClass_ requires 1 argument")
		}
		return FindByClass(args[0])
	case "countByClass_":
		if len(args) < 1 {
			return "", fmt.Errorf("countByClass_ requires 1 argument")
		}
		return CountByClass(args[0])
	case "listClasses":
		ListClasses()
		return "", nil
	case "query_":
		if len(args) < 1 {
			return "", fmt.Errorf("query_ requires 1 argument")
		}
		return Query(args[0])
	case "queryData_":
		if len(args) < 1 {
			return "", fmt.Errorf("queryData_ requires 1 argument")
		}
		return QueryData(args[0])
	case "listIndices":
		ListIndices()
		return "", nil
	case "listColumns":
		ListColumns()
		return "", nil
	case "clear":
		Clear()
		return "", nil
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func Init() {
	db_init
}

func GetInstance(instance_id string) (string, error) {
	instance_idInt, err := strconv.Atoi(instance_id)
	if err != nil {
		return "", err
	}
	_ = instance_idInt
	db_get
	"$instance_id"
	return "", nil
}

func DeleteInstance(instance_id string) (string, error) {
	instance_idInt, err := strconv.Atoi(instance_id)
	if err != nil {
		return "", err
	}
	_ = instance_idInt
	db_delete
	"$instance_id"
	return "", nil
}

func GetClass(instance_id string) (string, error) {
	instance_idInt, err := strconv.Atoi(instance_id)
	if err != nil {
		return "", err
	}
	_ = instance_idInt
	sendMessage(Store, "getField_field_", "$instance_id", "class")
	return "", nil
}

func FindByClass(class_name string) (string, error) {
	class_nameInt, err := strconv.Atoi(class_name)
	if err != nil {
		return "", err
	}
	_ = class_nameInt
	db_find_by_class
	"$class_name"
	return "", nil
}

func CountByClass(class_name string) (string, error) {
	class_nameInt, err := strconv.Atoi(class_name)
	if err != nil {
		return "", err
	}
	_ = class_nameInt
	db_count_by_class
	"$class_name"
	return "", nil
}

func ListClasses() {
	db_list_classes
}

func Query(where_clause string) (string, error) {
	where_clauseInt, err := strconv.Atoi(where_clause)
	if err != nil {
		return "", err
	}
	_ = where_clauseInt
	db_query
	"$where_clause"
	return "", nil
}

func QueryData(where_clause string) (string, error) {
	where_clauseInt, err := strconv.Atoi(where_clause)
	if err != nil {
		return "", err
	}
	_ = where_clauseInt
	db_query_data
	"$where_clause"
	return "", nil
}

func ListIndices() {
	db_list_indices
}

func ListColumns() {
	db_list_columns
}

func Clear() {
	db_clear
}
