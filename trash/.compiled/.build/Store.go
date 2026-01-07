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
	uuid "github.com/google/uuid"
	_ "github.com/mattn/go-sqlite3"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
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

	result, err := dispatch(instance, receiver, selector, args)
	if err != nil {
		if errors.Is(err, ErrUnknownSelector) {
			os.Exit(200)
		}
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if selector == "delete" {
		if err := deleteInstance(db, receiver); err != nil {
			fmt.Fprintf(os.Stderr, "Error deleting instance: %v\n", err)
			os.Exit(1)
		}
	} else {
		if err := saveInstance(db, receiver, instance); err != nil {
			fmt.Fprintf(os.Stderr, "Error saving instance: %v\n", err)
			os.Exit(1)
		}
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

func generateInstanceID(className string) string {
	uuid := uuid.New().String()
	return strings.ToLower(className) + "_" + uuid
}

func createInstance(db *sql.DB, id string, instance *Store) error {
	data, err := json.Marshal(instance)
	if err != nil {
		return err
	}
	_, err = db.Exec("INSERT INTO instances (id, data) VALUES (?, json(?))", id, string(data))
	return err
}

func deleteInstance(db *sql.DB, id string) error {
	_, err := db.Exec("DELETE FROM instances WHERE id = ?", id)
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
	InstanceID string   `json:"instance_id"`
	Instance   string   `json:"instance"`
	Selector   string   `json:"selector"`
	Args       []string `json:"args"`
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

	result, err := dispatch(&instance, req.InstanceID, req.Selector, req.Args)
	if err != nil {
		if errors.Is(err, ErrUnknownSelector) {
			return ServeResponse{ExitCode: 200}
		}
		return ServeResponse{
			Error:    err.Error(),
			ExitCode: 1,
		}
	}

	if req.Selector == "delete" {
		if err := deleteInstance(db, req.InstanceID); err != nil {
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

	updatedJSON, _ := json.Marshal(&instance)
	return ServeResponse{
		ExitCode: 0,
		Instance: string(updatedJSON),
		Result:   result,
	}
}

// Common conversion helpers
func _boolToString(b bool) string {
	if b {
		return "true"
	}
	return "false"
}

func _toStr(v interface{}) string {
	if v == nil {
		return ""
	}
	return fmt.Sprintf("%v", v)
}

// Array helpers for native slice operations
func _arrayFirst(arr []interface{}) interface{} {
	if len(arr) == 0 {
		return nil
	}
	return arr[0]
}

func _arrayLast(arr []interface{}) interface{} {
	if len(arr) == 0 {
		return nil
	}
	return arr[len(arr)-1]
}

func _arrayAtPut(arr []interface{}, idx int, val interface{}) []interface{} {
	// Handle negative indices
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx < 0 || idx >= len(arr) {
		return arr
	}
	result := make([]interface{}, len(arr))
	copy(result, arr)
	result[idx] = val
	return result
}

func _arrayRemoveAt(arr []interface{}, idx int) []interface{} {
	// Handle negative indices
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx < 0 || idx >= len(arr) {
		return arr
	}
	result := make([]interface{}, 0, len(arr)-1)
	result = append(result, arr[:idx]...)
	result = append(result, arr[idx+1:]...)
	return result
}

// Map helpers for native map operations
func _mapKeys(m map[string]interface{}) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func _mapValues(m map[string]interface{}) []interface{} {
	vals := make([]interface{}, 0, len(m))
	for _, v := range m {
		vals = append(vals, v)
	}
	return vals
}

func _mapHasKey(m map[string]interface{}, key string) bool {
	_, ok := m[key]
	return ok
}

func _mapAtPut(m map[string]interface{}, key string, val interface{}) map[string]interface{} {
	result := make(map[string]interface{}, len(m)+1)
	for k, v := range m {
		result[k] = v
	}
	result[key] = val
	return result
}

func _mapRemoveKey(m map[string]interface{}, key string) map[string]interface{} {
	result := make(map[string]interface{}, len(m))
	for k, v := range m {
		if k != key {
			result[k] = v
		}
	}
	return result
}

// JSON string parsing helpers (for string-typed variables containing JSON)
func _jsonArrayLen(jsonStr string) int {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return 0
	}
	return len(arr)
}

func _jsonArrayFirst(jsonStr string) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil || len(arr) == 0 {
		return ""
	}
	return fmt.Sprintf("%v", arr[0])
}

func _jsonArrayLast(jsonStr string) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil || len(arr) == 0 {
		return ""
	}
	return fmt.Sprintf("%v", arr[len(arr)-1])
}

func _jsonArrayIsEmpty(jsonStr string) bool {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return true
	}
	return len(arr) == 0
}

func _jsonArrayPush(jsonStr string, val interface{}) string {
	var arr []interface{}
	json.Unmarshal([]byte(jsonStr), &arr)
	arr = append(arr, val)
	result, _ := json.Marshal(arr)
	return string(result)
}

func _jsonArrayAt(jsonStr string, idx int) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return ""
	}
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx < 0 || idx >= len(arr) {
		return ""
	}
	return fmt.Sprintf("%v", arr[idx])
}

func _jsonArrayAtPut(jsonStr string, idx int, val interface{}) string {
	var arr []interface{}
	json.Unmarshal([]byte(jsonStr), &arr)
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx >= 0 && idx < len(arr) {
		arr[idx] = val
	}
	result, _ := json.Marshal(arr)
	return string(result)
}

func _jsonArrayRemoveAt(jsonStr string, idx int) string {
	var arr []interface{}
	json.Unmarshal([]byte(jsonStr), &arr)
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx >= 0 && idx < len(arr) {
		arr = append(arr[:idx], arr[idx+1:]...)
	}
	result, _ := json.Marshal(arr)
	return string(result)
}

func _jsonObjectLen(jsonStr string) int {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return 0
	}
	return len(m)
}

func _jsonObjectKeys(jsonStr string) []string {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return nil
	}
	return _mapKeys(m)
}

func _jsonObjectValues(jsonStr string) []interface{} {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return nil
	}
	return _mapValues(m)
}

func _jsonObjectIsEmpty(jsonStr string) bool {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return true
	}
	return len(m) == 0
}

func _jsonObjectAt(jsonStr string, key string) string {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return ""
	}
	if v, ok := m[key]; ok {
		return fmt.Sprintf("%v", v)
	}
	return ""
}

func _jsonObjectAtPut(jsonStr string, key string, val interface{}) string {
	var m map[string]interface{}
	json.Unmarshal([]byte(jsonStr), &m)
	if m == nil {
		m = make(map[string]interface{})
	}
	m[key] = val
	result, _ := json.Marshal(m)
	return string(result)
}

func _jsonObjectHasKey(jsonStr string, key string) bool {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return false
	}
	_, ok := m[key]
	return ok
}

func _jsonObjectRemoveKey(jsonStr string, key string) string {
	var m map[string]interface{}
	json.Unmarshal([]byte(jsonStr), &m)
	delete(m, key)
	result, _ := json.Marshal(m)
	return string(result)
}

// toInt converts interface{} to int for arithmetic in iteration blocks
func toInt(v interface{}) int {
	switch x := v.(type) {
	case int:
		return x
	case int64:
		return int(x)
	case float64:
		return int(x)
	case string:
		n, _ := strconv.Atoi(x)
		return n
	default:
		return 0
	}
}

// toBool converts interface{} to bool for predicates in iteration blocks
func toBool(v interface{}) bool {
	switch x := v.(type) {
	case bool:
		return x
	case int:
		return x != 0
	case string:
		return x != ""
	default:
		return v != nil
	}
}

// invokeBlock calls a Trashtalk block through the Bash runtime
// blockID is the instance ID of the Block object
// args are the values to pass to the block
func invokeBlock(blockID string, args ...interface{}) (string, error) {
	var cmdStr string
	switch len(args) {
	case 0:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q value", blockID)
	case 1:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q valueWith: %q", blockID, fmt.Sprint(args[0]))
	case 2:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q valueWith: %q and: %q", blockID, fmt.Sprint(args[0]), fmt.Sprint(args[1]))
	default:
		return "", fmt.Errorf("invokeBlock: too many arguments (%d)", len(args))
	}

	cmd := exec.Command("bash", "-c", cmdStr)
	output, err := cmd.Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(output)), nil
}

func dispatch(c *Store, instanceID string, selector string, args []string) (string, error) {
	switch selector {
	case "class":
		return "Store", nil
	case "id":
		return instanceID, nil
	case "delete":
		return instanceID, nil
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func dispatchClass(selector string, args []string) (string, error) {
	switch selector {
	case "new":
		id := generateInstanceID("Store")
		instance := &Store{
			Class:     "Store",
			CreatedAt: time.Now().Format(time.RFC3339),
		}
		db, err := openDB()
		if err != nil {
			return "", err
		}
		defer db.Close()
		if err := createInstance(db, id, instance); err != nil {
			return "", err
		}
		return id, nil
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
