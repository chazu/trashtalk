package main

import (
	"C"
	"database/sql"
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

var ErrUnknownSelector = errors.New("unknown selector")

type Counter struct {
	Class     string   `json:"class"`
	CreatedAt string   `json:"created_at"`
	Vars      []string `json:"_vars"`
	Value     string   `json:"value"`
	Step      string   `json:"step"`
}

//export GetClassName
func GetClassName() *C.char {
	return C.CString("Counter")
}

//export Dispatch
func Dispatch(instanceJSON *C.char, selector *C.char, argsJSON *C.char) *C.char {
	instanceStr := C.GoString(instanceJSON)
	selectorStr := C.GoString(selector)
	argsStr := C.GoString(argsJSON)

	result := dispatchInternal(instanceStr, selectorStr, argsStr)
	return C.CString(result)
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

func dispatchInternal(instanceJSON string, selector string, argsJSON string) string {
	var args []string
	json.Unmarshal([]byte(argsJSON), &args)

	if instanceJSON == "" || instanceJSON == "Counter" {
		result, err := dispatchClass(selector, args)
		if err != nil {
			if errors.Is(err, ErrUnknownSelector) {
				return "{\"exit_code\":200}"
			}
			return fmt.Sprintf("{\"exit_code\":1,\"error\":%q}", err.Error())
		}
		return fmt.Sprintf("{\"result\":%q,\"exit_code\":0}", result)
	}

	var instance Counter
	if err := json.Unmarshal([]byte(instanceJSON), &instance); err != nil {
		return fmt.Sprintf("{\"exit_code\":1,\"error\":%q}", err.Error())
	}

	result, err := dispatch(&instance, selector, args)
	if err != nil {
		if errors.Is(err, ErrUnknownSelector) {
			return "{\"exit_code\":200}"
		}
		return fmt.Sprintf("{\"exit_code\":1,\"error\":%q}", err.Error())
	}

	updatedJSON, _ := json.Marshal(&instance)
	return fmt.Sprintf("{\"instance\":%s,\"result\":%q,\"exit_code\":0}", string(updatedJSON), result)
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
func invokeBlock(blockID string, args ...interface{}) string {
	var cmdStr string
	switch len(args) {
	case 0:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q value", blockID)
	case 1:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q valueWith: %q", blockID, fmt.Sprint(args[0]))
	case 2:
		cmdStr = fmt.Sprintf("source ~/.trashtalk/lib/trash.bash && @ %q valueWith: %q and: %q", blockID, fmt.Sprint(args[0]), fmt.Sprint(args[1]))
	default:
		return ""
	}

	cmd := exec.Command("bash", "-c", cmdStr)
	output, _ := cmd.Output()
	return strings.TrimSpace(string(output))
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

func _jsonArrayPush(jsonVal interface{}, val interface{}) string {
	jsonStr := fmt.Sprintf("%v", jsonVal)
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
	return _toStr(toInt(c.Value) + toInt(0))
}

func (c *Counter) GetStep() string {
	return _toStr(toInt(c.Step) + toInt(0))
}

func (c *Counter) SetValue(val string) (string, error) {
	valInt, err := strconv.Atoi(val)
	if err != nil {
		return "", err
	}
	_ = valInt
	c.Value = strconv.Itoa(toInt(valInt) + toInt(0))
	return "", nil
}

func (c *Counter) SetStep(val string) (string, error) {
	valInt, err := strconv.Atoi(val)
	if err != nil {
		return "", err
	}
	_ = valInt
	c.Step = strconv.Itoa(toInt(valInt) + toInt(0))
	return "", nil
}

func (c *Counter) Increment() string {
	var newVal interface{}
	newVal = toInt(c.Value) + toInt(c.Step)
	c.Value = strconv.Itoa(toInt(newVal) + toInt(0))
	return _toStr(toInt(newVal) + toInt(0))
}

func (c *Counter) Decrement() string {
	var newVal interface{}
	newVal = toInt(c.Value) - toInt(c.Step)
	c.Value = strconv.Itoa(toInt(newVal) + toInt(0))
	return _toStr(toInt(newVal) + toInt(0))
}

func (c *Counter) IncrementBy(amount string) (string, error) {
	amountInt, err := strconv.Atoi(amount)
	if err != nil {
		return "", err
	}
	_ = amountInt
	var newVal interface{}
	newVal = toInt(c.Value) + toInt(amountInt)
	c.Value = strconv.Itoa(toInt(newVal) + toInt(0))
	return "", nil
}

func (c *Counter) Reset() {
	c.Value = strconv.Itoa(toInt(0) + toInt(0))
}

func Description() string {
	return "A simple counter"
}

func main() {}
