package sqlite

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
	"github.com/jaeyeom/experimental/codelab/go/todo/core/coretest"
)

func TestSaveLoad(t *testing.T) {
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	path := filepath.Join(dir, "todo.db")
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

func TestLoadEmpty(t *testing.T) {
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	path := filepath.Join(dir, "todo.db")
	s, err := New(path)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	// Load from a new database
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

func TestSaveLoadMultiple(t *testing.T) {
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	path := filepath.Join(dir, "todo.db")
	s, err := New(path)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	// Create a todo list with multiple items
	list := core.NewList()
	list.Add("buy groceries")
	list.Add("write code")
	list.Add("read book")

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

	// Compare each item
	for i := range list.Items {
		if loaded.Items[i].Description != list.Items[i].Description {
			t.Errorf("Load() item %d got description %q, want %q",
				i, loaded.Items[i].Description, list.Items[i].Description)
		}
		if loaded.Items[i].State != list.Items[i].State {
			t.Errorf("Load() item %d got state %v, want %v",
				i, loaded.Items[i].State, list.Items[i].State)
		}
	}
}

func TestSaveLoadWithState(t *testing.T) {
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	path := filepath.Join(dir, "todo.db")
	s, err := New(path)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()

	// Create a todo list with items in different states
	ig := coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)
	list := core.NewList(core.WithNewID(ig))
	list.Add("buy groceries")
	list.Add("write code")
	if err := list.Complete("11111111"); err != nil { // Complete first item
		t.Fatal(err)
	}

	// Save the list
	if err := s.Save(list); err != nil {
		t.Errorf("Save() error = %v", err)
	}

	// Load the list and verify
	loaded, err := s.Load()
	if err != nil {
		t.Errorf("Load() error = %v", err)
	}

	// Compare states
	for i := range list.Items {
		if loaded.Items[i].State != list.Items[i].State {
			t.Errorf("Load() item %d got state %v, want %v",
				i, loaded.Items[i].State, list.Items[i].State)
		}
	}
}
