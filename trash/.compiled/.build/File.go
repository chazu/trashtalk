package main

import (
	"C"
	"database/sql"
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
	"syscall"
	"time"
)

var ErrUnknownSelector = errors.New("unknown selector")

type File struct {
	Class     string   `json:"class"`
	CreatedAt string   `json:"created_at"`
	Vars      []string `json:"_vars"`
	Path      string   `json:"path"`
}

//export GetClassName
func GetClassName() *C.char {
	return C.CString("File")
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

func loadInstance(db *sql.DB, id string) (*File, error) {
	var data string
	err := db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		return nil, err
	}
	var instance File
	if err := json.Unmarshal([]byte(data), &instance); err != nil {
		return nil, err
	}
	return &instance, nil
}

func saveInstance(db *sql.DB, id string, instance *File) error {
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

	if instanceJSON == "" || instanceJSON == "File" {
		result, err := dispatchClass(selector, args)
		if err != nil {
			if errors.Is(err, ErrUnknownSelector) {
				return "{\"exit_code\":200}"
			}
			return fmt.Sprintf("{\"exit_code\":1,\"error\":%q}", err.Error())
		}
		return fmt.Sprintf("{\"result\":%q,\"exit_code\":0}", result)
	}

	var instance File
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

func sendMessage(receiver interface{}, selector string, args ...interface{}) string {
	receiverStr := fmt.Sprintf("%v", receiver)
	cmdArgs := []string{receiverStr, selector}
	for _, arg := range args {
		cmdArgs = append(cmdArgs, fmt.Sprintf("%v", arg))
	}
	home, _ := os.UserHomeDir()
	dispatchScript := filepath.Join(home, ".trashtalk", "bin", "trash-send")
	cmd := exec.Command(dispatchScript, cmdArgs...)
	output, _ := cmd.Output()
	return strings.TrimSpace(string(output))
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

func stringToJsonArray(text string) string {
	if text == "" {
		return "[]"
	}
	lines := strings.Split(text, "\n")
	arr := make([]interface{}, 0, len(lines))
	for _, line := range lines {
		arr = append(arr, line)
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

func _jsonObjectAt(jsonVal any, key string) string {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return ""
	}
	if v, ok := m[key]; ok {
		return fmt.Sprintf("%v", v)
	}
	return ""
}

func _jsonObjectAtPut(jsonVal any, key string, val any) string {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var m map[string]interface{}
	json.Unmarshal([]byte(jsonStr), &m)
	if m == nil {
		m = make(map[string]interface{})
	}
	m[key] = val
	result, _ := json.Marshal(m)
	return string(result)
}

func _jsonObjectHasKey(jsonVal any, key string) bool {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return false
	}
	_, ok := m[key]
	return ok
}

func _jsonObjectRemoveKey(jsonVal any, key string) string {
	jsonStr := fmt.Sprintf("%v", jsonVal)
	var m map[string]interface{}
	json.Unmarshal([]byte(jsonStr), &m)
	delete(m, key)
	result, _ := json.Marshal(m)
	return string(result)
}

func dispatch(c *File, selector string, args []string) (string, error) {
	switch selector {
	case "path":
		return c.GetPath()
	case "exists":
		return c.Exists()
	case "isFile":
		return c.IsFile()
	case "isDirectory":
		return c.IsDirectory()
	case "isFifo":
		return c.IsFifo()
	case "read":
		return c.Read()
	case "readLines":
		return c.ReadLines()
	case "write_":
		if len(args) < 1 {
			return "", fmt.Errorf("write_ requires 1 argument")
		}
		return c.Write_(args[0])
	case "writeLine_":
		if len(args) < 1 {
			return "", fmt.Errorf("writeLine_ requires 1 argument")
		}
		return c.WriteLine_(args[0])
	case "append_":
		if len(args) < 1 {
			return "", fmt.Errorf("append_ requires 1 argument")
		}
		return c.Append_(args[0])
	case "appendLine_":
		if len(args) < 1 {
			return "", fmt.Errorf("appendLine_ requires 1 argument")
		}
		return c.AppendLine_(args[0])
	case "delete":
		return c.Delete()
	case "size":
		return c.Size()
	case "directory":
		return c.Directory()
	case "basename":
		return c.Basename()
	case "extension":
		return c.Extension()
	case "stem":
		return c.Stem()
	case "copyTo_":
		if len(args) < 1 {
			return "", fmt.Errorf("copyTo_ requires 1 argument")
		}
		return c.CopyTo_(args[0])
	case "moveTo_":
		if len(args) < 1 {
			return "", fmt.Errorf("moveTo_ requires 1 argument")
		}
		return c.MoveTo_(args[0])
	case "touch":
		return c.Touch()
	case "modificationTime":
		return c.ModificationTime()
	case "info":
		return c.Info()
	case "printString":
		return c.PrintString()
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func dispatchClass(selector string, args []string) (string, error) {
	switch selector {
	case "at_":
		if len(args) < 1 {
			return "", fmt.Errorf("at_ requires 1 argument")
		}
		return At_(args[0])
	case "temp":
		return Temp()
	case "tempWithPrefix_":
		if len(args) < 1 {
			return "", fmt.Errorf("tempWithPrefix_ requires 1 argument")
		}
		return TempWithPrefix_(args[0])
	case "mkfifo_":
		if len(args) < 1 {
			return "", fmt.Errorf("mkfifo_ requires 1 argument")
		}
		return Mkfifo_(args[0])
	case "exists_":
		if len(args) < 1 {
			return "", fmt.Errorf("exists_ requires 1 argument")
		}
		return Exists_(args[0])
	case "isFile_":
		if len(args) < 1 {
			return "", fmt.Errorf("isFile_ requires 1 argument")
		}
		return IsFile_(args[0])
	case "isDirectory_":
		if len(args) < 1 {
			return "", fmt.Errorf("isDirectory_ requires 1 argument")
		}
		return IsDirectory_(args[0])
	case "isSymlink_":
		if len(args) < 1 {
			return "", fmt.Errorf("isSymlink_ requires 1 argument")
		}
		return IsSymlink_(args[0])
	case "isFifo_":
		if len(args) < 1 {
			return "", fmt.Errorf("isFifo_ requires 1 argument")
		}
		return IsFifo_(args[0])
	case "isSocket_":
		if len(args) < 1 {
			return "", fmt.Errorf("isSocket_ requires 1 argument")
		}
		return IsSocket_(args[0])
	case "isBlockDevice_":
		if len(args) < 1 {
			return "", fmt.Errorf("isBlockDevice_ requires 1 argument")
		}
		return IsBlockDevice_(args[0])
	case "isCharDevice_":
		if len(args) < 1 {
			return "", fmt.Errorf("isCharDevice_ requires 1 argument")
		}
		return IsCharDevice_(args[0])
	case "isReadable_":
		if len(args) < 1 {
			return "", fmt.Errorf("isReadable_ requires 1 argument")
		}
		return IsReadable_(args[0])
	case "isWritable_":
		if len(args) < 1 {
			return "", fmt.Errorf("isWritable_ requires 1 argument")
		}
		return IsWritable_(args[0])
	case "isExecutable_":
		if len(args) < 1 {
			return "", fmt.Errorf("isExecutable_ requires 1 argument")
		}
		return IsExecutable_(args[0])
	case "isEmpty_":
		if len(args) < 1 {
			return "", fmt.Errorf("isEmpty_ requires 1 argument")
		}
		return IsEmpty_(args[0])
	case "notEmpty_":
		if len(args) < 1 {
			return "", fmt.Errorf("notEmpty_ requires 1 argument")
		}
		return NotEmpty_(args[0])
	case "isNewer_than_":
		if len(args) < 2 {
			return "", fmt.Errorf("isNewer_than_ requires 2 argument")
		}
		return IsNewer_than_(args[0], args[1])
	case "isOlder_than_":
		if len(args) < 2 {
			return "", fmt.Errorf("isOlder_than_ requires 2 argument")
		}
		return IsOlder_than_(args[0], args[1])
	case "isSame_as_":
		if len(args) < 2 {
			return "", fmt.Errorf("isSame_as_ requires 2 argument")
		}
		return IsSame_as_(args[0], args[1])
	case "read_":
		if len(args) < 1 {
			return "", fmt.Errorf("read_ requires 1 argument")
		}
		return Read_(args[0])
	case "write_to_":
		if len(args) < 2 {
			return "", fmt.Errorf("write_to_ requires 2 argument")
		}
		return Write_to_(args[0], args[1])
	case "delete_":
		if len(args) < 1 {
			return "", fmt.Errorf("delete_ requires 1 argument")
		}
		return Delete_(args[0])
	default:
		return "", fmt.Errorf("%w: %s", ErrUnknownSelector, selector)
	}
}

func At_(filepath string) (string, error) {
	// Generate instance ID
	id := "file_" + strings.ReplaceAll(uuid.New().String(), "-", "")

	// Create instance in database
	db, err := openDB()
	if err != nil {
		return "", err
	}
	defer db.Close()

	instance := &File{
		Class:     "File",
		CreatedAt: time.Now().Format(time.RFC3339),
		Path:      filepath,
	}

	if err := saveInstance(db, id, instance); err != nil {
		return "", err
	}
	return id, nil
}

func Temp() (string, error) {
	// Create temp file
	tmpfile, err := os.CreateTemp("", "trashtalk-*")
	if err != nil {
		return "", err
	}
	tmpfile.Close()

	// Generate instance ID
	id := "file_" + strings.ReplaceAll(uuid.New().String(), "-", "")

	// Create instance in database
	db, err := openDB()
	if err != nil {
		return "", err
	}
	defer db.Close()

	instance := &File{
		Class:     "File",
		CreatedAt: time.Now().Format(time.RFC3339),
		Path:      tmpfile.Name(),
	}

	if err := saveInstance(db, id, instance); err != nil {
		return "", err
	}
	return id, nil
}

func TempWithPrefix_(prefix string) (string, error) {
	// Create temp file with prefix
	tmpfile, err := os.CreateTemp("", prefix+"*")
	if err != nil {
		return "", err
	}
	tmpfile.Close()

	// Generate instance ID
	id := "file_" + strings.ReplaceAll(uuid.New().String(), "-", "")

	// Create instance in database
	db, err := openDB()
	if err != nil {
		return "", err
	}
	defer db.Close()

	instance := &File{
		Class:     "File",
		CreatedAt: time.Now().Format(time.RFC3339),
		Path:      tmpfile.Name(),
	}

	if err := saveInstance(db, id, instance); err != nil {
		return "", err
	}
	return id, nil
}

func Mkfifo_(filepath string) (string, error) {
	// Create FIFO (named pipe)
	if err := syscall.Mkfifo(filepath, 420); err != nil {
		// Ignore error if FIFO already exists
		if !os.IsExist(err) {
			return "", err
		}
	}

	// Generate instance ID
	id := "file_" + strings.ReplaceAll(uuid.New().String(), "-", "")

	// Create instance in database
	db, err := openDB()
	if err != nil {
		return "", err
	}
	defer db.Close()

	instance := &File{
		Class:     "File",
		CreatedAt: time.Now().Format(time.RFC3339),
		Path:      filepath,
	}

	if err := saveInstance(db, id, instance); err != nil {
		return "", err
	}
	return id, nil
}

func (c *File) GetPath() (string, error) {
	return c.Path, nil
}

func (c *File) Exists() (string, error) {
	_, err := os.Stat(c.Path)
	if err == nil {
		return "true", nil
	}
	return "false", nil
}

func (c *File) IsFile() (string, error) {
	info, err := os.Stat(c.Path)
	if err != nil {
		return "false", nil
	}
	if info.Mode().IsRegular() {
		return "true", nil
	}
	return "false", nil
}

func (c *File) IsDirectory() (string, error) {
	info, err := os.Stat(c.Path)
	if err != nil {
		return "false", nil
	}
	if info.IsDir() {
		return "true", nil
	}
	return "false", nil
}

func (c *File) IsFifo() (string, error) {
	info, err := os.Stat(c.Path)
	if err != nil {
		return "false", nil
	}
	if info.Mode()&os.ModeNamedPipe != 0 {
		return "true", nil
	}
	return "false", nil
}

func (c *File) Read() (string, error) {
	data, err := os.ReadFile(c.Path)
	if err != nil {
		return "", err
	}
	return string(data), nil
}

func (c *File) ReadLines() (string, error) {
	info, err := os.Stat(c.Path)
	if err != nil || !info.Mode().IsRegular() {
		return "", nil
	}
	data, err := os.ReadFile(c.Path)
	if err != nil {
		return "", err
	}
	return string(data), nil
}

func (c *File) Write_(contents string) (string, error) {
	err := os.WriteFile(c.Path, []byte(contents), 420)
	if err != nil {
		return "", err
	}
	return "", nil
}

func (c *File) WriteLine_(contents string) (string, error) {
	err := os.WriteFile(c.Path, []byte(contents+"\n"), 420)
	return "", err
}

func (c *File) Append_(contents string) (string, error) {
	file, err := os.OpenFile(c.Path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 420)
	if err != nil {
		return "", err
	}
	defer file.Close()
	_, err = file.WriteString(contents)
	return "", err
}

func (c *File) AppendLine_(contents string) (string, error) {
	file, err := os.OpenFile(c.Path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 420)
	if err != nil {
		return "", err
	}
	defer file.Close()
	_, err = file.WriteString(contents + "\n")
	return "", err
}

func (c *File) Delete() (string, error) {
	err := os.Remove(c.Path)
	return "", err
}

func (c *File) Size() (string, error) {
	info, err := os.Stat(c.Path)
	if err != nil {
		return "0", nil
	}
	return strconv.FormatInt(info.Size(), 10), nil
}

func (c *File) Directory() (string, error) {
	return filepath.Dir(c.Path), nil
}

func (c *File) Basename() (string, error) {
	return filepath.Base(c.Path), nil
}

func (c *File) Extension() (string, error) {
	ext := filepath.Ext(c.Path)
	if len(ext) > 0 {
		return ext[1:], nil
	}
	return "", nil
}

func (c *File) Stem() (string, error) {
	base := filepath.Base(c.Path)
	ext := filepath.Ext(base)
	if len(ext) > 0 {
		return base[:len(base)-len(ext)], nil
	}
	return base, nil
}

func (c *File) CopyTo_(destPath string) (string, error) {
	data, err := os.ReadFile(c.Path)
	if err != nil {
		return "", err
	}
	err = os.WriteFile(destPath, data, 420)
	return "", err
}

func (c *File) MoveTo_(destPath string) (string, error) {
	err := os.Rename(c.Path, destPath)
	if err != nil {
		return "", err
	}
	c.Path = destPath
	return "", nil
}

func (c *File) Touch() (string, error) {
	now := time.Now()
	err := os.Chtimes(c.Path, now, now)
	if os.IsNotExist(err) {
		// Create the file if it doesn't exist
		f, err := os.Create(c.Path)
		if err != nil {
			return "", err
		}
		f.Close()
		return "", nil
	}
	return "", err
}

func (c *File) ModificationTime() (string, error) {
	info, err := os.Stat(c.Path)
	if err != nil {
		return "0", nil
	}
	return strconv.FormatInt(info.ModTime().Unix(), 10), nil
}

func (c *File) Info() (string, error) {
	var result strings.Builder
	result.WriteString("Path: " + c.Path + "\n")
	info, err := os.Stat(c.Path)
	if err == nil {
		result.WriteString("Exists: true\n")
		result.WriteString("Size: " + strconv.FormatInt(info.Size(), 10) + " bytes\n")
	} else {
		result.WriteString("Exists: false\n")
	}
	return result.String(), nil
}

func (c *File) PrintString() (string, error) {
	return "<File " + c.Path + ">", nil
}

func Exists_(path string) (string, error) {
	_, err := os.Stat(path)
	if err == nil {
		return "true", nil
	}
	return "false", nil
}

func IsFile_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "false", nil
	}
	if info.Mode().IsRegular() {
		return "true", nil
	}
	return "false", nil
}

func IsDirectory_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "false", nil
	}
	if info.IsDir() {
		return "true", nil
	}
	return "false", nil
}

func IsSymlink_(path string) (string, error) {
	info, err := os.Lstat(path)
	if err != nil {
		return "false", nil
	}
	if info.Mode()&os.ModeSymlink != 0 {
		return "true", nil
	}
	return "false", nil
}

func IsFifo_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "false", nil
	}
	if info.Mode()&os.ModeNamedPipe != 0 {
		return "true", nil
	}
	return "false", nil
}

func IsSocket_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "false", nil
	}
	if info.Mode()&os.ModeSocket != 0 {
		return "true", nil
	}
	return "false", nil
}

func IsBlockDevice_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "false", nil
	}
	if info.Mode()&os.ModeDevice != 0 {
		return "true", nil
	}
	return "false", nil
}

func IsCharDevice_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "false", nil
	}
	if info.Mode()&os.ModeCharDevice != 0 {
		return "true", nil
	}
	return "false", nil
}

func IsReadable_(path string) (string, error) {
	file, err := os.Open(path)
	if err != nil {
		return "false", nil
	}
	file.Close()
	return "true", nil
}

func IsWritable_(path string) (string, error) {
	file, err := os.OpenFile(path, os.O_WRONLY, 0)
	if err != nil {
		return "false", nil
	}
	file.Close()
	return "true", nil
}

func IsExecutable_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "false", nil
	}
	// Check if any execute bit is set
	if info.Mode().Perm()&73 != 0 {
		return "true", nil
	}
	return "false", nil
}

func IsEmpty_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "true", nil
	}
	if info.Size() == 0 {
		return "true", nil
	}
	return "false", nil
}

func NotEmpty_(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "false", nil
	}
	if info.Size() > 0 {
		return "true", nil
	}
	return "false", nil
}

func IsNewer_than_(path1 string, path2 string) (string, error) {
	info1, err1 := os.Stat(path1)
	info2, err2 := os.Stat(path2)
	if err1 != nil || err2 != nil {
		return "false", nil
	}
	if info1.ModTime().After(info2.ModTime()) {
		return "true", nil
	}
	return "false", nil
}

func IsOlder_than_(path1 string, path2 string) (string, error) {
	info1, err1 := os.Stat(path1)
	info2, err2 := os.Stat(path2)
	if err1 != nil || err2 != nil {
		return "false", nil
	}
	if info1.ModTime().Before(info2.ModTime()) {
		return "true", nil
	}
	return "false", nil
}

func IsSame_as_(path1 string, path2 string) (string, error) {
	info1, err1 := os.Stat(path1)
	info2, err2 := os.Stat(path2)
	if err1 != nil || err2 != nil {
		return "false", nil
	}
	if os.SameFile(info1, info2) {
		return "true", nil
	}
	return "false", nil
}

func Read_(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return string(data), nil
}

func Write_to_(contents string, path string) (string, error) {
	err := os.WriteFile(path, []byte(contents), 420)
	return "", err
}

func Delete_(path string) (string, error) {
	err := os.Remove(path)
	return "", err
}

func main() {}
