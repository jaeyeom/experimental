package json_storage

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
)

func TestSaveLoad(t *testing.T) {
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	path := filepath.Join(dir, "todo.json")
	s, err := New(path)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	// Create a todo list with one item
	list := core.NewList()
	list.Add("buy groceries")

	// Save the list
	if err := s.Save(list); err != nil {
		t.Errorf("Save() error = %v", err)
	}

	// Load the list and verify
	loaded, err := s.Load()
	if err != nil {
		t.Errorf("Load() error = %v", err)
	}

	// Compare the lists
	if len(loaded.Items) != len(list.Items) {
		t.Errorf("Load() got %d items, want %d", len(loaded.Items), len(list.Items))
	}
	if len(loaded.Items) > 0 && loaded.Items[0].Description != list.Items[0].Description {
		t.Errorf("Load() got description %q, want %q", loaded.Items[0].Description, list.Items[0].Description)
	}
}

func TestLoadNonExistent(t *testing.T) {
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	path := filepath.Join(dir, "todo.json")
	s, err := New(path)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	// Load from a non-existent file
	list, err := s.Load()
	if err != nil {
		t.Errorf("Load() error = %v", err)
	}
	if list == nil {
		t.Fatal("Load() returned nil list")
	}
	if len(list.Items) != 0 {
		t.Errorf("Load() got %d items, want 0", len(list.Items))
	}
}
