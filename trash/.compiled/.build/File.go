package main

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	uuid "github.com/google/uuid"
	_ "github.com/mattn/go-sqlite3"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"
	"unsafe"
)

/*

#cgo CFLAGS: -I${SRCDIR}/../../include
#cgo LDFLAGS: -L${SRCDIR}/../../lib -ltrashtalk
#include <libtrashtalk.h>
#include <stdlib.h>

// Extern declarations for method wrappers (defined via //export)
extern TTValue __File_method_Path(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Exists(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_IsFile(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_IsDirectory(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_IsFifo(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Read(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_ReadLines(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Write_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_WriteLine_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Append_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_AppendLine_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Delete(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Size(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Directory(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Basename(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Extension(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Stem(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_CopyTo_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_MoveTo_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Touch(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_ModificationTime(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_Info(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_method_PrintString(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_At_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_Temp(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_TempWithPrefix_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_Mkfifo_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_Exists_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsFile_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsDirectory_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsSymlink_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsFifo_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsSocket_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsBlockDevice_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsCharDevice_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsReadable_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsWritable_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsExecutable_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsEmpty_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_NotEmpty_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsNewer_than_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsOlder_than_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_IsSame_as_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_Read_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_Write_to_(TTInstance* self, TTValue* args, int numArgs);
extern TTValue __File_classmethod_Delete_(TTInstance* self, TTValue* args, int numArgs);
*/
import "C"

var ErrUnknownSelector = errors.New("unknown selector")

type File struct {
	Class     string   `json:"class"`
	CreatedAt string   `json:"created_at"`
	Vars      []string `json:"_vars"`
	Path      string   `json:"path"`
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

func sendMessage(receiver interface{}, selector string, args ...interface{}) string {
	receiverStr := fmt.Sprintf("%v", receiver)
	cReceiver := C.CString(receiverStr)
	defer C.free(unsafe.Pointer(cReceiver))
	cSelector := C.CString(selector)
	defer C.free(unsafe.Pointer(cSelector))
	cArgs := make([]C.TTValue, len(args))
	for i, arg := range args {
		argStr := fmt.Sprintf("%v", arg)
		cStr := C.CString(argStr)
		cArgs[i] = C.TT_MakeString(cStr)
	}
	var argsPtr *C.TTValue
	if len(cArgs) > 0 {
		argsPtr = &cArgs[0]
	}
	result := C.TT_Send(cReceiver, cSelector, argsPtr, C.int(len(args)))
	cResult := C.TT_ValueAsString(result)
	if cResult == nil {
		return ""
	}
	defer C.free(unsafe.Pointer(cResult))
	return C.GoString(cResult)
}

// sendMessageDirect sends a message using direct instance pointer (faster than sendMessage)
func sendMessageDirect(inst *C.TTInstance, selector string, args ...interface{}) string {
	cSelector := C.CString(selector)
	defer C.free(unsafe.Pointer(cSelector))
	cArgs := make([]C.TTValue, len(args))
	for i, arg := range args {
		argStr := fmt.Sprintf("%v", arg)
		cStr := C.CString(argStr)
		cArgs[i] = C.TT_MakeString(cStr)
	}
	var argsPtr *C.TTValue
	if len(cArgs) > 0 {
		argsPtr = &cArgs[0]
	}
	result := C.TT_SendDirect(inst, cSelector, argsPtr, C.int(len(args)))
	cResult := C.TT_ValueAsString(result)
	if cResult == nil {
		return ""
	}
	defer C.free(unsafe.Pointer(cResult))
	return C.GoString(cResult)
}

// lookupInstance retrieves an instance pointer from the shared runtime
func lookupInstance(instanceID string) *C.TTInstance {
	cID := C.CString(instanceID)
	defer C.free(unsafe.Pointer(cID))
	return C.TT_Lookup(cID)
}

// lookupBlock retrieves a block pointer from the shared runtime
func lookupBlock(blockID string) *C.TTBlock {
	cID := C.CString(blockID)
	defer C.free(unsafe.Pointer(cID))
	return C.TT_LookupBlock(cID)
}

// invokeBlockDirect invokes a block using direct pointer (faster than invokeBlock)
func invokeBlockDirect(block *C.TTBlock, args ...interface{}) string {
	if block == nil {
		return ""
	}
	cArgs := make([]C.TTValue, len(args))
	for i, arg := range args {
		argStr := fmt.Sprintf("%v", arg)
		cStr := C.CString(argStr)
		cArgs[i] = C.TT_MakeString(cStr)
	}
	var argsPtr *C.TTValue
	if len(cArgs) > 0 {
		argsPtr = &cArgs[0]
	}
	result := C.TT_InvokeBlockDirect(block, argsPtr, C.int(len(args)))
	cResult := C.TT_ValueAsString(result)
	if cResult == nil {
		return ""
	}
	defer C.free(unsafe.Pointer(cResult))
	return C.GoString(cResult)
}

// invokeBlock calls a Trashtalk block through the shared runtime
func invokeBlock(blockID string, args ...interface{}) string {
	cBlockID := C.CString(blockID)
	defer C.free(unsafe.Pointer(cBlockID))
	cArgs := make([]C.TTValue, len(args))
	for i, arg := range args {
		argStr := fmt.Sprintf("%v", arg)
		cStr := C.CString(argStr)
		cArgs[i] = C.TT_MakeString(cStr)
	}
	var argsPtr *C.TTValue
	if len(cArgs) > 0 {
		argsPtr = &cArgs[0]
	}
	result := C.TT_InvokeBlock(cBlockID, argsPtr, C.int(len(args)))
	cResult := C.TT_ValueAsString(result)
	if cResult == nil {
		return ""
	}
	defer C.free(unsafe.Pointer(cResult))
	return C.GoString(cResult)
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

//export File_Dispatch
func File_Dispatch(instanceJSON *C.char, selector *C.char, argsJSON *C.char) *C.char {
	instanceStr := C.GoString(instanceJSON)
	selectorStr := C.GoString(selector)
	argsStr := C.GoString(argsJSON)

	result := dispatchInternal(instanceStr, selectorStr, argsStr)
	return C.CString(result)
}

func dispatchInternal(instanceJSON string, selector string, argsJSON string) string {
	var args []string
	json.Unmarshal([]byte(argsJSON), &args)

	if instanceJSON == "" || instanceJSON == "File" || instanceJSON == "File" {
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
	return fmt.Sprintf("{\"instance\":%q,\"result\":%q,\"exit_code\":0}", string(updatedJSON), result)
}

// Runtime method wrappers - C-compatible functions for method registration

//export __File_method_Path
func __File_method_Path(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "path", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Exists
func __File_method_Exists(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "exists", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_IsFile
func __File_method_IsFile(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "isFile", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_IsDirectory
func __File_method_IsDirectory(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "isDirectory", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_IsFifo
func __File_method_IsFifo(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "isFifo", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Read
func __File_method_Read(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "read", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_ReadLines
func __File_method_ReadLines(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "readLines", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Write_
func __File_method_Write_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "write_", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_WriteLine_
func __File_method_WriteLine_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "writeLine_", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Append_
func __File_method_Append_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "append_", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_AppendLine_
func __File_method_AppendLine_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "appendLine_", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Delete
func __File_method_Delete(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "delete", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Size
func __File_method_Size(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "size", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Directory
func __File_method_Directory(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "directory", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Basename
func __File_method_Basename(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "basename", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Extension
func __File_method_Extension(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "extension", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Stem
func __File_method_Stem(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "stem", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_CopyTo_
func __File_method_CopyTo_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "copyTo_", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_MoveTo_
func __File_method_MoveTo_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "moveTo_", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Touch
func __File_method_Touch(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "touch", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_ModificationTime
func __File_method_ModificationTime(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "modificationTime", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_Info
func __File_method_Info(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "info", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_method_PrintString
func __File_method_PrintString(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	cJSON := C.TT_Serialize(self)
	instanceJSON := C.GoString(cJSON)
	C.free(unsafe.Pointer(cJSON))

	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal(instanceJSON, "printString", string(argsJSON))

	var result struct {
		Instance string `json:"instance"`
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.Instance != "" && self != nil {
		cNewJSON := C.CString(result.Instance)
		C.TT_Deserialize(cNewJSON)
		C.free(unsafe.Pointer(cNewJSON))
	}

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_At_
func __File_classmethod_At_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "at_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_Temp
func __File_classmethod_Temp(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "temp", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_TempWithPrefix_
func __File_classmethod_TempWithPrefix_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "tempWithPrefix_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_Mkfifo_
func __File_classmethod_Mkfifo_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "mkfifo_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_Exists_
func __File_classmethod_Exists_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "exists_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsFile_
func __File_classmethod_IsFile_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isFile_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsDirectory_
func __File_classmethod_IsDirectory_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isDirectory_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsSymlink_
func __File_classmethod_IsSymlink_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isSymlink_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsFifo_
func __File_classmethod_IsFifo_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isFifo_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsSocket_
func __File_classmethod_IsSocket_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isSocket_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsBlockDevice_
func __File_classmethod_IsBlockDevice_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isBlockDevice_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsCharDevice_
func __File_classmethod_IsCharDevice_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isCharDevice_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsReadable_
func __File_classmethod_IsReadable_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isReadable_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsWritable_
func __File_classmethod_IsWritable_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isWritable_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsExecutable_
func __File_classmethod_IsExecutable_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isExecutable_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsEmpty_
func __File_classmethod_IsEmpty_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isEmpty_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_NotEmpty_
func __File_classmethod_NotEmpty_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "notEmpty_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsNewer_than_
func __File_classmethod_IsNewer_than_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isNewer_than_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsOlder_than_
func __File_classmethod_IsOlder_than_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isOlder_than_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_IsSame_as_
func __File_classmethod_IsSame_as_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "isSame_as_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_Read_
func __File_classmethod_Read_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "read_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_Write_to_
func __File_classmethod_Write_to_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "write_to_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

//export __File_classmethod_Delete_
func __File_classmethod_Delete_(self *C.TTInstance, args *C.TTValue, numArgs C.int) C.TTValue {
	goArgs := make([]string, int(numArgs))
	if args != nil && numArgs > 0 {
		cArgs := unsafe.Slice(args, int(numArgs))
		for i, cArg := range cArgs {
			cStr := C.TT_ValueAsString(cArg)
			if cStr != nil {
				goArgs[i] = C.GoString(cStr)
				C.free(unsafe.Pointer(cStr))
			}
		}
	}

	argsJSON, _ := json.Marshal(goArgs)
	resultJSON := dispatchInternal("", "delete_", string(argsJSON))

	var result struct {
		Result   string `json:"result"`
		ExitCode int    `json:"exit_code"`
	}
	json.Unmarshal([]byte(resultJSON), &result)

	if result.ExitCode != 0 {
		return C.TT_MakeNil()
	}
	cResult := C.CString(result.Result)
	return C.TT_MakeString(cResult)
}

// init registers this class with the shared runtime
func init() {
	className := C.CString("File")
	defer C.free(unsafe.Pointer(className))
	superclass := func() *C.char {
		if "Object" == "" {
			return nil
		}
		return C.CString("Object")
	}()
	instanceVars := func() **C.char {
		vars := make([]*C.char, 1)
		vars[0] = C.CString("path")
		return &vars[0]
	}()
	instMethodsPtr := (*C.TTMethodEntry)(C.malloc(C.size_t(unsafe.Sizeof(C.TTMethodEntry{}) * 23)))
	instMethods := unsafe.Slice(instMethodsPtr, 23)
	instMethods[0] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Path),
		numArgs:  C.int(0),
		selector: C.CString("path"),
	}
	instMethods[1] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Exists),
		numArgs:  C.int(0),
		selector: C.CString("exists"),
	}
	instMethods[2] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_IsFile),
		numArgs:  C.int(0),
		selector: C.CString("isFile"),
	}
	instMethods[3] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_IsDirectory),
		numArgs:  C.int(0),
		selector: C.CString("isDirectory"),
	}
	instMethods[4] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_IsFifo),
		numArgs:  C.int(0),
		selector: C.CString("isFifo"),
	}
	instMethods[5] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Read),
		numArgs:  C.int(0),
		selector: C.CString("read"),
	}
	instMethods[6] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_ReadLines),
		numArgs:  C.int(0),
		selector: C.CString("readLines"),
	}
	instMethods[7] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Write_),
		numArgs:  C.int(1),
		selector: C.CString("write_"),
	}
	instMethods[8] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_WriteLine_),
		numArgs:  C.int(1),
		selector: C.CString("writeLine_"),
	}
	instMethods[9] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Append_),
		numArgs:  C.int(1),
		selector: C.CString("append_"),
	}
	instMethods[10] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_AppendLine_),
		numArgs:  C.int(1),
		selector: C.CString("appendLine_"),
	}
	instMethods[11] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Delete),
		numArgs:  C.int(0),
		selector: C.CString("delete"),
	}
	instMethods[12] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Size),
		numArgs:  C.int(0),
		selector: C.CString("size"),
	}
	instMethods[13] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Directory),
		numArgs:  C.int(0),
		selector: C.CString("directory"),
	}
	instMethods[14] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Basename),
		numArgs:  C.int(0),
		selector: C.CString("basename"),
	}
	instMethods[15] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Extension),
		numArgs:  C.int(0),
		selector: C.CString("extension"),
	}
	instMethods[16] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Stem),
		numArgs:  C.int(0),
		selector: C.CString("stem"),
	}
	instMethods[17] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_CopyTo_),
		numArgs:  C.int(1),
		selector: C.CString("copyTo_"),
	}
	instMethods[18] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_MoveTo_),
		numArgs:  C.int(1),
		selector: C.CString("moveTo_"),
	}
	instMethods[19] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Touch),
		numArgs:  C.int(0),
		selector: C.CString("touch"),
	}
	instMethods[20] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_ModificationTime),
		numArgs:  C.int(0),
		selector: C.CString("modificationTime"),
	}
	instMethods[21] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_Info),
		numArgs:  C.int(0),
		selector: C.CString("info"),
	}
	instMethods[22] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE,
		impl:     (C.TTMethodFunc)(C.__File_method_PrintString),
		numArgs:  C.int(0),
		selector: C.CString("printString"),
	}
	classMethodsPtr := (*C.TTMethodEntry)(C.malloc(C.size_t(unsafe.Sizeof(C.TTMethodEntry{}) * 23)))
	classMethods := unsafe.Slice(classMethodsPtr, 23)
	classMethods[0] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_At_),
		numArgs:  C.int(1),
		selector: C.CString("at_"),
	}
	classMethods[1] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_Temp),
		numArgs:  C.int(0),
		selector: C.CString("temp"),
	}
	classMethods[2] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_TempWithPrefix_),
		numArgs:  C.int(1),
		selector: C.CString("tempWithPrefix_"),
	}
	classMethods[3] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_Mkfifo_),
		numArgs:  C.int(1),
		selector: C.CString("mkfifo_"),
	}
	classMethods[4] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_Exists_),
		numArgs:  C.int(1),
		selector: C.CString("exists_"),
	}
	classMethods[5] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsFile_),
		numArgs:  C.int(1),
		selector: C.CString("isFile_"),
	}
	classMethods[6] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsDirectory_),
		numArgs:  C.int(1),
		selector: C.CString("isDirectory_"),
	}
	classMethods[7] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsSymlink_),
		numArgs:  C.int(1),
		selector: C.CString("isSymlink_"),
	}
	classMethods[8] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsFifo_),
		numArgs:  C.int(1),
		selector: C.CString("isFifo_"),
	}
	classMethods[9] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsSocket_),
		numArgs:  C.int(1),
		selector: C.CString("isSocket_"),
	}
	classMethods[10] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsBlockDevice_),
		numArgs:  C.int(1),
		selector: C.CString("isBlockDevice_"),
	}
	classMethods[11] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsCharDevice_),
		numArgs:  C.int(1),
		selector: C.CString("isCharDevice_"),
	}
	classMethods[12] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsReadable_),
		numArgs:  C.int(1),
		selector: C.CString("isReadable_"),
	}
	classMethods[13] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsWritable_),
		numArgs:  C.int(1),
		selector: C.CString("isWritable_"),
	}
	classMethods[14] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsExecutable_),
		numArgs:  C.int(1),
		selector: C.CString("isExecutable_"),
	}
	classMethods[15] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsEmpty_),
		numArgs:  C.int(1),
		selector: C.CString("isEmpty_"),
	}
	classMethods[16] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_NotEmpty_),
		numArgs:  C.int(1),
		selector: C.CString("notEmpty_"),
	}
	classMethods[17] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsNewer_than_),
		numArgs:  C.int(2),
		selector: C.CString("isNewer_than_"),
	}
	classMethods[18] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsOlder_than_),
		numArgs:  C.int(2),
		selector: C.CString("isOlder_than_"),
	}
	classMethods[19] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_IsSame_as_),
		numArgs:  C.int(2),
		selector: C.CString("isSame_as_"),
	}
	classMethods[20] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_Read_),
		numArgs:  C.int(1),
		selector: C.CString("read_"),
	}
	classMethods[21] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_Write_to_),
		numArgs:  C.int(2),
		selector: C.CString("write_to_"),
	}
	classMethods[22] = C.TTMethodEntry{
		flags:    C.TT_METHOD_NATIVE | C.TT_METHOD_CLASS_METHOD,
		impl:     (C.TTMethodFunc)(C.__File_classmethod_Delete_),
		numArgs:  C.int(1),
		selector: C.CString("delete_"),
	}
	methods := &C.TTMethodTable{
		classMethods:       classMethodsPtr,
		instanceMethods:    instMethodsPtr,
		numClassMethods:    C.int(23),
		numInstanceMethods: C.int(23),
	}
	C.TT_RegisterClass(className, superclass, instanceVars, C.int(1), methods)
}

//export GetClassName
func GetClassName() *C.char {
	return C.CString("File")
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
