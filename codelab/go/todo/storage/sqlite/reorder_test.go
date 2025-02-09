package sqlite

import (
	"os"
	"testing"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
)

func TestReorderPreservation(t *testing.T) {
	// Create a temporary database file
	f, err := os.CreateTemp("", "todo-test-*.db")
	if err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}
	defer os.Remove(f.Name())
	f.Close()

	// Create storage
	storage, err := New(f.Name())
	if err != nil {
		t.Fatalf("Failed to create storage: %v", err)
	}
	defer storage.Close()

	tests := []struct {
		name     string
		setup    func() *core.List
		reorder  func(*core.List) error
		validate func(*testing.T, *core.List)
	}{
		{
			name: "moving root item up preserves order",
			setup: func() *core.List {
				list := core.NewList()
				list.Add("First")
				list.Add("Second")
				list.Add("Third")
				return list
			},
			reorder: func(list *core.List) error {
				return list.MoveUp(list.Items[1].ID)
			},
			validate: func(t *testing.T, list *core.List) {
				if len(list.Items) != 3 {
					t.Fatalf("got %d items, want 3", len(list.Items))
				}
				if list.Items[0].Description != "Second" {
					t.Errorf("first item got %q, want 'Second'", list.Items[0].Description)
				}
				if list.Items[1].Description != "First" {
					t.Errorf("second item got %q, want 'First'", list.Items[1].Description)
				}
			},
		},
		{
			name: "moving root item down preserves order",
			setup: func() *core.List {
				list := core.NewList()
				list.Add("First")
				list.Add("Second")
				list.Add("Third")
				return list
			},
			reorder: func(list *core.List) error {
				return list.MoveDown(list.Items[1].ID)
			},
			validate: func(t *testing.T, list *core.List) {
				if len(list.Items) != 3 {
					t.Fatalf("got %d items, want 3", len(list.Items))
				}
				if list.Items[1].Description != "Third" {
					t.Errorf("second item got %q, want 'Third'", list.Items[1].Description)
				}
				if list.Items[2].Description != "Second" {
					t.Errorf("third item got %q, want 'Second'", list.Items[2].Description)
				}
			},
		},
		{
			name: "moving subtask up preserves order",
			setup: func() *core.List {
				list := core.NewList()
				list.Add("Parent")
				if err := list.AddSubtask(list.Items[0].ID, "First Sub"); err != nil {
					t.Fatalf("Failed to add subtask: %v", err)
				}
				if err := list.AddSubtask(list.Items[0].ID, "Second Sub"); err != nil {
					t.Fatalf("Failed to add subtask: %v", err)
				}
				if err := list.AddSubtask(list.Items[0].ID, "Third Sub"); err != nil {
					t.Fatalf("Failed to add subtask: %v", err)
				}
				return list
			},
			reorder: func(list *core.List) error {
				return list.MoveUp(list.Items[0].Subtasks[1].ID)
			},
			validate: func(t *testing.T, list *core.List) {
				subs := list.Items[0].Subtasks
				if len(subs) != 3 {
					t.Fatalf("got %d subtasks, want 3", len(subs))
				}
				if subs[0].Description != "Second Sub" {
					t.Errorf("first subtask got %q, want 'Second Sub'", subs[0].Description)
				}
				if subs[1].Description != "First Sub" {
					t.Errorf("second subtask got %q, want 'First Sub'", subs[1].Description)
				}
			},
		},
		{
			name: "moving subtask down preserves order",
			setup: func() *core.List {
				list := core.NewList()
				list.Add("Parent")
				if err := list.AddSubtask(list.Items[0].ID, "First Sub"); err != nil {
					t.Fatalf("Failed to add subtask: %v", err)
				}
				if err := list.AddSubtask(list.Items[0].ID, "Second Sub"); err != nil {
					t.Fatalf("Failed to add subtask: %v", err)
				}
				if err := list.AddSubtask(list.Items[0].ID, "Third Sub"); err != nil {
					t.Fatalf("Failed to add subtask: %v", err)
				}
				return list
			},
			reorder: func(list *core.List) error {
				return list.MoveDown(list.Items[0].Subtasks[1].ID)
			},
			validate: func(t *testing.T, list *core.List) {
				subs := list.Items[0].Subtasks
				if len(subs) != 3 {
					t.Fatalf("got %d subtasks, want 3", len(subs))
				}
				if subs[1].Description != "Third Sub" {
					t.Errorf("second subtask got %q, want 'Third Sub'", subs[1].Description)
				}
				if subs[2].Description != "Second Sub" {
					t.Errorf("third subtask got %q, want 'Second Sub'", subs[2].Description)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Setup initial list
			list := tt.setup()

			// Save initial list
			if err := storage.Save(list); err != nil {
				t.Fatalf("Failed to save initial list: %v", err)
			}

			// Load the list
			loaded, err := storage.Load()
			if err != nil {
				t.Fatalf("Failed to load initial list: %v", err)
			}

			// Perform reordering
			if err := tt.reorder(loaded); err != nil {
				t.Fatalf("Failed to reorder: %v", err)
			}

			// Save reordered list
			if err := storage.Save(loaded); err != nil {
				t.Fatalf("Failed to save reordered list: %v", err)
			}

			// Load again and validate
			finalList, err := storage.Load()
			if err != nil {
				t.Fatalf("Failed to load final list: %v", err)
			}

			// Validate the order is preserved
			tt.validate(t, finalList)
		})
	}
}
