// Package json_storage provides JSON-based persistence for todo lists.
// It implements saving and loading todo lists to and from JSON files.
package json_storage

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	todo "github.com/jaeyeom/experimental/codelab/go/todo/core"
)

// createParentDir ensures that the parent directory of the given path exists.
// It creates the directory and any necessary parents with 0755 permissions.
func createParentDir(path string) error {
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		return fmt.Errorf("os.MkdirAll: %v", err)
	}
	return nil
}

// Save persists a todo list to a JSON file at the specified path. It creates
// any necessary parent directories and saves the list with 0644 permissions. If
// an error occurs during any step of the process, it returns a wrapped error
// describing what went wrong.
func Save(path string, list *todo.List) error {
	if err := createParentDir(path); err != nil {
		return fmt.Errorf("createParentDir: %v", err)
	}
	b, err := json.Marshal(list)
	if err != nil {
		return fmt.Errorf("json.Marshal: %v", err)
	}
	if err := ioutil.WriteFile(path, b, 0644); err != nil {
		return fmt.Errorf("ioutil.Write: %v", err)
	}
	return nil
}

// Load reads a todo list from a JSON file at the specified path. If the file
// doesn't exist, it returns a new empty list. If any other error occurs during
// reading or parsing, it returns a wrapped error describing what went wrong.
func Load(path string) (*todo.List, error) {
	b, err := ioutil.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return todo.NewList(), nil
		}
		return nil, fmt.Errorf("ioutil.ReadFile: %v", err)
	}
	var list todo.List
	if err := json.Unmarshal(b, &list); err != nil {
		return nil, fmt.Errorf("json.Unmarshal: %v", err)
	}
	return &list, nil
}
