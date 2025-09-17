package jsonstorage

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
		return
	}
	if len(list.Items) != 0 {
		t.Errorf("Load() got %d items, want 0", len(list.Items))
	}
}

func TestSaveLoadWithSubtasks(t *testing.T) {
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

	// Create a todo list with nested tasks
	list := core.NewList()
	list.Add("Parent task")
	if err := list.AddSubtask(list.Items[0].ID, "Subtask 1"); err != nil {
		t.Errorf("AddSubtask() error = %v", err)
	}
	if err := list.AddSubtask(list.Items[0].ID, "Subtask 2"); err != nil {
		t.Errorf("AddSubtask() error = %v", err)
	}

	// Add a sub-subtask
	parent := &list.Items[0]
	if err := list.AddSubtask(parent.Subtasks[0].ID, "Sub-subtask 1"); err != nil {
		t.Errorf("AddSubtask() error = %v", err)
	}

	// Set various states
	if err := list.Complete(parent.Subtasks[0].ID); err != nil {
		t.Errorf("Complete() error = %v", err)
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

	// Verify the structure
	if len(loaded.Items) != 1 {
		t.Errorf("got %d top-level items, want 1", len(loaded.Items))
	}

	loadedParent := &loaded.Items[0]
	if len(loadedParent.Subtasks) != 2 {
		t.Errorf("got %d subtasks, want 2", len(loadedParent.Subtasks))
	}

	if loadedParent.State != core.ItemStatePartiallyDone {
		t.Errorf("got parent state %v, want %v", loadedParent.State, core.ItemStatePartiallyDone)
	}

	if loadedParent.Subtasks[0].State != core.ItemStateDone {
		t.Errorf("got subtask state %v, want %v", loadedParent.Subtasks[0].State, core.ItemStateDone)
	}

	if len(loadedParent.Subtasks[0].Subtasks) != 1 {
		t.Errorf("got %d sub-subtasks, want 1", len(loadedParent.Subtasks[0].Subtasks))
	}

	// Verify parent-child relationships
	if loadedParent.Subtasks[0].ParentID != loadedParent.ID {
		t.Errorf("got parent ID %q, want %q", loadedParent.Subtasks[0].ParentID, loadedParent.ID)
	}

	subSubtask := &loadedParent.Subtasks[0].Subtasks[0]
	if subSubtask.ParentID != loadedParent.Subtasks[0].ID {
		t.Errorf("got sub-subtask parent ID %q, want %q", subSubtask.ParentID, loadedParent.Subtasks[0].ID)
	}
}
