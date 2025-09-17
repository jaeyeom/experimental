package sqlite

import (
	"fmt"
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
		return
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

func TestSaveLoadWithSubtasks(t *testing.T) {
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
		t.Errorf("got %d root items, want 1", len(loaded.Items))
	}

	parent = &loaded.Items[0]
	if parent.Description != "Parent task" {
		t.Errorf("got parent description %q, want %q", parent.Description, "Parent task")
	}

	// Verify subtasks
	if len(parent.Subtasks) != 2 {
		t.Errorf("got %d subtasks, want 2", len(parent.Subtasks))
	}

	// Find the completed subtask
	var completedSubtask *core.Item
	for i := range parent.Subtasks {
		if parent.Subtasks[i].State == core.ItemStateDone {
			completedSubtask = &parent.Subtasks[i]
			break
		}
	}

	if completedSubtask == nil {
		t.Fatal("completed subtask not found")
		return
	}

	// Verify sub-subtasks
	if len(completedSubtask.Subtasks) != 1 {
		t.Errorf("got %d sub-subtasks, want 1", len(completedSubtask.Subtasks))
	}

	// Verify states
	if parent.State != core.ItemStatePartiallyDone {
		t.Errorf("got parent state %v, want %v", parent.State, core.ItemStatePartiallyDone)
	}
}

// descendingUUID returns UUIDs in descending order, making the pattern visible
// by using descending hex values (ff -> ee -> dd -> cc -> ...).
var descendingUUIDCounter = byte(0xff)

func descendingUUID() string {
	// Create a UUID with all bytes set to the current counter value
	var id [16]byte
	for i := range id {
		id[i] = descendingUUIDCounter
	}

	// Decrement the counter by 0x11 to get ff -> ee -> dd -> cc pattern
	if descendingUUIDCounter >= 0x11 {
		descendingUUIDCounter -= 0x11
	}

	// Format it as a UUID string
	return fmt.Sprintf("%x-%x-%x-%x-%x",
		id[0:4],
		id[4:6],
		id[6:8],
		id[8:10],
		id[10:16])
}

func TestSaveLoadWithDescendingUUID(t *testing.T) {
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

	// Create a todo list with descending UUID generation
	list := core.NewList(core.WithNewID(descendingUUID))

	// Add items in a specific order
	items := []string{
		"First task",
		"Second task",
		"Third task",
	}
	for _, desc := range items {
		list.Add(desc)
	}

	// Add subtasks to the first task
	subtasks := []string{
		"First subtask",
		"Second subtask",
		"Third subtask",
	}
	for _, desc := range subtasks {
		if err := list.AddSubtask(list.Items[0].ID, desc); err != nil {
			t.Errorf("AddSubtask() error = %v", err)
		}
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

	// Verify root items order
	if len(loaded.Items) != len(items) {
		t.Errorf("got %d root items, want %d", len(loaded.Items), len(items))
	}
	for i, desc := range items {
		if i >= len(loaded.Items) {
			t.Errorf("missing item at index %d", i)
			continue
		}
		if loaded.Items[i].Description != desc {
			t.Errorf("item[%d]: got description %q, want %q", i, loaded.Items[i].Description, desc)
		}
	}

	// Verify subtasks order
	if len(loaded.Items[0].Subtasks) != len(subtasks) {
		t.Errorf("got %d subtasks, want %d", len(loaded.Items[0].Subtasks), len(subtasks))
	}
	for i, desc := range subtasks {
		if i >= len(loaded.Items[0].Subtasks) {
			t.Errorf("missing subtask at index %d", i)
			continue
		}
		if loaded.Items[0].Subtasks[i].Description != desc {
			t.Errorf("subtask[%d]: got description %q, want %q", i, loaded.Items[0].Subtasks[i].Description, desc)
		}
	}

	// Print the actual order of IDs to verify they are in descending order
	t.Logf("Root item IDs in order:")
	for i, item := range loaded.Items {
		t.Logf("  %d: %s", i, item.ID)
	}
	t.Logf("Subtask IDs in order:")
	for i, subtask := range loaded.Items[0].Subtasks {
		t.Logf("  %d: %s", i, subtask.ID)
	}
}
