// Package json_storage provides JSON-based persistence for todo lists.
// It implements saving and loading todo lists to and from JSON files.
package json_storage

import (
	"encoding/json"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
)

// createParentDir ensures that the parent directory of the given path exists.
// It creates the directory and any necessary parents with 0755 permissions.
func createParentDir(path string) error {
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("os.MkdirAll: %v", err)
	}
	return nil
}

// Save persists a todo list to a JSON file at the specified path. It creates
// any necessary parent directories and saves the list with 0644 permissions. If
// an error occurs during any step of the process, it returns a wrapped error
// describing what went wrong.
func Save(path string, list *core.List) error {
	slog.Info("saving todo list", "path", path, "items", len(list.Items))
	if err := createParentDir(path); err != nil {
		return fmt.Errorf("createParentDir: %v", err)
	}
	b, err := json.Marshal(list)
	if err != nil {
		return fmt.Errorf("json.Marshal: %v", err)
	}
	if err := os.WriteFile(path, b, 0644); err != nil {
		return fmt.Errorf("os.WriteFile: %v", err)
	}
	return nil
}

// Load reads a todo list from a JSON file at the specified path. If the file
// doesn't exist, it returns a new empty list. If any other error occurs during
// reading or parsing, it returns a wrapped error describing what went wrong.
func Load(path string) (*core.List, error) {
	slog.Info("loading todo list", "path", path)
	b, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			slog.Info("todo list file not found, creating new list", "path", path)
			return core.NewList(), nil
		}
		return nil, fmt.Errorf("os.ReadFile: %v", err)
	}
	var list core.List
	if err := json.Unmarshal(b, &list); err != nil {
		return nil, fmt.Errorf("json.Unmarshal: %v", err)
	}
	slog.Info("loaded todo list", "path", path, "items", len(list.Items))
	return &list, nil
}
