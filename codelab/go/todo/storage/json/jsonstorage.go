// Package json provides a JSON file-based implementation of the storage.Storage
// interface for persisting todo lists.
package jsonstorage

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"sync"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
)

// Storage implements storage.Storage interface using JSON files.
type Storage struct {
	path string
	mu   sync.RWMutex
}

// New creates a new JSON storage that persists data to the specified path.
// It creates any necessary parent directories with 0755 permissions.
func New(path string) (*Storage, error) {
	if err := createParentDir(path); err != nil {
		return nil, fmt.Errorf("create parent directory: %v", err)
	}
	return &Storage{path: path}, nil
}

// createParentDir ensures that the parent directory of the given path exists.
// It creates the directory and any necessary parents with 0755 permissions.
func createParentDir(path string) error {
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("os.MkdirAll: %v", err)
	}
	return nil
}

// Save persists a todo list to the JSON file. It acquires a write lock to
// ensure thread safety. If an error occurs during any step of the process, it
// returns a wrapped error describing what went wrong.
func (s *Storage) Save(list *core.List) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	slog.Info("saving todo list", "path", s.path, "items", len(list.Items))
	b, err := json.Marshal(list)
	if err != nil {
		return fmt.Errorf("json.Marshal: %v", err)
	}
	if err := os.WriteFile(s.path, b, 0o600); err != nil {
		return fmt.Errorf("os.WriteFile: %v", err)
	}
	return nil
}

// Load reads a todo list from the JSON file. It acquires a read lock to ensure
// thread safety. If the file doesn't exist, it returns a new empty list. If any
// other error occurs during reading or parsing, it returns a wrapped error
// describing what went wrong.
func (s *Storage) Load() (*core.List, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	slog.Info("loading todo list", "path", s.path)
	b, err := os.ReadFile(s.path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			slog.Info("todo list file not found, creating new list", "path", s.path)
			return core.NewList(), nil
		}
		return nil, fmt.Errorf("os.ReadFile: %v", err)
	}
	var list core.List
	if err := json.Unmarshal(b, &list); err != nil {
		return nil, fmt.Errorf("json.Unmarshal: %v", err)
	}
	return &list, nil
}

// Close frees the resource. Since file-based storage doesn't maintain any
// long-lived resources, this operation is a no-op.
func (s *Storage) Close() error {
	return nil
}
