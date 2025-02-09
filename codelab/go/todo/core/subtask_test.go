package core

import (
	"testing"
)

func TestSubtasks(t *testing.T) {
	tests := []struct {
		name string
		fn   func(t *testing.T)
	}{
		{
			name: "add_subtask",
			fn: func(t *testing.T) {
				l := NewList()
				l.Add("Parent task")
				parent := &l.Items[0]

				err := l.AddSubtask(parent.ID, "Subtask 1")
				if err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}

				if len(parent.Subtasks) != 1 {
					t.Errorf("got %d subtasks, want 1", len(parent.Subtasks))
				}

				if parent.Subtasks[0].Description != "Subtask 1" {
					t.Errorf("got description %q, want %q", parent.Subtasks[0].Description, "Subtask 1")
				}

				if parent.Subtasks[0].ParentID != parent.ID {
					t.Errorf("got parent ID %q, want %q", parent.Subtasks[0].ParentID, parent.ID)
				}
			},
		},
		{
			name: "complete_subtask_updates_parent_state",
			fn: func(t *testing.T) {
				l := NewList()
				l.Add("Parent task")
				parent := &l.Items[0]

				err := l.AddSubtask(parent.ID, "Subtask 1")
				if err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				err = l.AddSubtask(parent.ID, "Subtask 2")
				if err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}

				// Initially parent should be NotStarted
				if parent.State != ItemStateNotStarted {
					t.Errorf("got parent state %v, want %v", parent.State, ItemStateNotStarted)
				}

				// Complete first subtask
				err = l.Complete(parent.Subtasks[0].ID)
				if err != nil {
					t.Errorf("Complete() error = %v", err)
				}

				// Parent should be PartiallyDone
				if parent.State != ItemStatePartiallyDone {
					t.Errorf("got parent state %v, want %v", parent.State, ItemStatePartiallyDone)
				}

				// Complete second subtask
				err = l.Complete(parent.Subtasks[1].ID)
				if err != nil {
					t.Errorf("Complete() error = %v", err)
				}

				// Parent should be Done
				if parent.State != ItemStateDone {
					t.Errorf("got parent state %v, want %v", parent.State, ItemStateDone)
				}
			},
		},
		{
			name: "recursive_subtasks",
			fn: func(t *testing.T) {
				l := NewList()
				l.Add("Parent task")
				parent := &l.Items[0]

				err := l.AddSubtask(parent.ID, "Subtask 1")
				if err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}

				subtask := &parent.Subtasks[0]
				err = l.AddSubtask(subtask.ID, "Sub-subtask 1")
				if err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}

				if len(subtask.Subtasks) != 1 {
					t.Errorf("got %d sub-subtasks, want 1", len(subtask.Subtasks))
				}

				if subtask.Subtasks[0].Description != "Sub-subtask 1" {
					t.Errorf("got description %q, want %q", subtask.Subtasks[0].Description, "Sub-subtask 1")
				}
			},
		},
		{
			name: "add_subtask_to_completed_parent",
			fn: func(t *testing.T) {
				l := NewList()
				l.Add("Parent task")
				parent := &l.Items[0]

				err := l.AddSubtask(parent.ID, "Subtask 1")
				if err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}

				// Complete the subtask and verify parent becomes done
				err = l.Complete(parent.Subtasks[0].ID)
				if err != nil {
					t.Errorf("Complete() error = %v", err)
				}
				if parent.State != ItemStateDone {
					t.Errorf("got parent state %v, want %v after completing all subtasks", parent.State, ItemStateDone)
				}

				// Add a new subtask and verify parent becomes partially done
				err = l.AddSubtask(parent.ID, "Subtask 2")
				if err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if parent.State != ItemStatePartiallyDone {
					t.Errorf("got parent state %v, want %v after adding new subtask", parent.State, ItemStatePartiallyDone)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, tt.fn)
	}
}
