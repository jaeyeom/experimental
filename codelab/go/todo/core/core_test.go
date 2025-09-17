package core

import (
	"fmt"
	"testing"

	"github.com/jaeyeom/experimental/codelab/go/todo/core/coretest"
)

func ExampleList_Add() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	fmt.Println(l)
	// Output:
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Complete() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Complete("1"); err != nil {
		panic(err)
	}
	fmt.Println(l)
	// Output:
	// 11111111-1111-1111-1111-111111111111. [x] buy groceries
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Complete_ambiguos() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"12222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Complete("1"); err != nil {
		fmt.Println(err)
	}
	fmt.Println(l)
	// Output:
	// ambiguous item ID
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 12222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Remove() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Remove("1"); err != nil {
		fmt.Println(err)
	}
	fmt.Println(l)
	// Output:
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Remove_ambiguous() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"12222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Remove("1"); err != nil {
		fmt.Println(err)
	}
	fmt.Println(l)
	// Output:
	// ambiguous item ID
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 12222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Uncomplete() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Complete("1"); err != nil {
		panic(err)
	}
	if err := l.Uncomplete("1"); err != nil {
		panic(err)
	}
	fmt.Println(l)
	// Output:
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Uncomplete_ambiguous() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"12222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Complete("11"); err != nil {
		panic(err)
	}
	if err := l.Uncomplete("1"); err != nil {
		fmt.Println(err)
	}
	fmt.Println(l)
	// Output:
	// ambiguous item ID
	// 11111111-1111-1111-1111-111111111111. [x] buy groceries
	// 12222222-2222-2222-2222-222222222222. [ ] write code
}

func TestCompleteWithSubtasks(t *testing.T) {
	tests := []struct {
		name              string
		setup             func(list *List) string // returns ID of item to complete
		wantParentState   ItemState
		wantSubtaskStates []ItemState
	}{
		{
			name: "completing parent completes all subtasks",
			setup: func(list *List) string {
				list.Add("Parent")
				parent := &list.Items[0]
				if err := list.AddSubtask(parent.ID, "Subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.ID, "Subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				return parent.ID
			},
			wantParentState:   ItemStateDone,
			wantSubtaskStates: []ItemState{ItemStateDone, ItemStateDone},
		},
		{
			name: "completing all subtasks completes parent",
			setup: func(list *List) string {
				list.Add("Parent")
				parent := &list.Items[0]
				if err := list.AddSubtask(parent.ID, "Subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.ID, "Subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.Complete(parent.Subtasks[0].ID); err != nil {
					t.Errorf("Complete() error = %v", err)
				}
				return parent.Subtasks[1].ID // complete the second subtask
			},
			wantParentState:   ItemStateDone,
			wantSubtaskStates: []ItemState{ItemStateDone, ItemStateDone},
		},
		{
			name: "completing some subtasks makes parent partially done",
			setup: func(list *List) string {
				list.Add("Parent")
				parent := &list.Items[0]
				if err := list.AddSubtask(parent.ID, "Subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.ID, "Subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				return parent.Subtasks[0].ID // complete only first subtask
			},
			wantParentState:   ItemStatePartiallyDone,
			wantSubtaskStates: []ItemState{ItemStateDone, ItemStateNotStarted},
		},
		{
			name: "completing parent with nested subtasks completes all",
			setup: func(list *List) string {
				list.Add("Parent")
				parent := &list.Items[0]
				if err := list.AddSubtask(parent.ID, "Subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.ID, "Subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.Subtasks[0].ID, "Sub-subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.Subtasks[1].ID, "Sub-subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				return parent.ID
			},
			wantParentState:   ItemStateDone,
			wantSubtaskStates: []ItemState{ItemStateDone, ItemStateDone},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			list := NewList()
			itemID := tt.setup(list)

			// Perform the completion
			if err := list.Complete(itemID); err != nil {
				t.Errorf("Complete() error = %v", err)
			}

			// Find the parent item
			var parent *Item
			for i := range list.Items {
				if len(list.Items[i].Subtasks) > 0 {
					parent = &list.Items[i]
					break
				}
			}
			if parent == nil {
				t.Fatal("parent task not found")
				return
			}

			// Verify parent state
			if parent.State != tt.wantParentState {
				t.Errorf("parent state = %v, want %v", parent.State, tt.wantParentState)
			}

			// Verify subtask states
			if len(parent.Subtasks) != len(tt.wantSubtaskStates) {
				t.Fatalf("got %d subtasks, want %d", len(parent.Subtasks), len(tt.wantSubtaskStates))
			}
			for i, subtask := range parent.Subtasks {
				if subtask.State != tt.wantSubtaskStates[i] {
					t.Errorf("subtask[%d] state = %v, want %v", i, subtask.State, tt.wantSubtaskStates[i])
				}
			}
		})
	}
}

func TestUndoWithSubtasks(t *testing.T) {
	tests := []struct {
		name              string
		setup             func(list *List) string // returns ID of item to undo
		wantParentState   ItemState
		wantSubtaskStates []ItemState
	}{
		{
			name: "undoing parent undoes all subtasks",
			setup: func(list *List) string {
				list.Add("Parent")
				parent := &list.Items[0]
				if err := list.AddSubtask(parent.ID, "Subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.ID, "Subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.Complete(parent.ID); err != nil {
					t.Errorf("Complete() error = %v", err)
				}
				return parent.ID
			},
			wantParentState:   ItemStateNotStarted,
			wantSubtaskStates: []ItemState{ItemStateNotStarted, ItemStateNotStarted},
		},
		{
			name: "undoing one subtask makes parent partially done",
			setup: func(list *List) string {
				list.Add("Parent")
				parent := &list.Items[0]
				if err := list.AddSubtask(parent.ID, "Subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.ID, "Subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.Complete(parent.ID); err != nil {
					t.Errorf("Complete() error = %v", err)
				}
				return parent.Subtasks[0].ID
			},
			wantParentState:   ItemStatePartiallyDone,
			wantSubtaskStates: []ItemState{ItemStateNotStarted, ItemStateDone},
		},
		{
			name: "undoing all subtasks undoes parent",
			setup: func(list *List) string {
				list.Add("Parent")
				parent := &list.Items[0]
				if err := list.AddSubtask(parent.ID, "Subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.ID, "Subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.Complete(parent.ID); err != nil {
					t.Errorf("Complete() error = %v", err)
				}
				if err := list.Undo(parent.Subtasks[0].ID); err != nil {
					t.Errorf("Undo() error = %v", err)
				}
				return parent.Subtasks[1].ID
			},
			wantParentState:   ItemStateNotStarted,
			wantSubtaskStates: []ItemState{ItemStateNotStarted, ItemStateNotStarted},
		},
		{
			name: "undoing parent with nested subtasks undoes all",
			setup: func(list *List) string {
				list.Add("Parent")
				parent := &list.Items[0]
				if err := list.AddSubtask(parent.ID, "Subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.ID, "Subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.Subtasks[0].ID, "Sub-subtask 1"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.AddSubtask(parent.Subtasks[1].ID, "Sub-subtask 2"); err != nil {
					t.Errorf("AddSubtask() error = %v", err)
				}
				if err := list.Complete(parent.ID); err != nil {
					t.Errorf("Complete() error = %v", err)
				}
				return parent.ID
			},
			wantParentState:   ItemStateNotStarted,
			wantSubtaskStates: []ItemState{ItemStateNotStarted, ItemStateNotStarted},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			list := NewList()
			itemID := tt.setup(list)

			// Perform the undo
			if err := list.Undo(itemID); err != nil {
				t.Errorf("Undo() error = %v", err)
			}

			// Find the parent item
			var parent *Item
			for i := range list.Items {
				if len(list.Items[i].Subtasks) > 0 {
					parent = &list.Items[i]
					break
				}
			}
			if parent == nil {
				t.Fatal("parent task not found")
				return
			}

			// Verify parent state
			if parent.State != tt.wantParentState {
				t.Errorf("parent state = %v, want %v", parent.State, tt.wantParentState)
			}

			// Verify subtask states
			if len(parent.Subtasks) != len(tt.wantSubtaskStates) {
				t.Fatalf("got %d subtasks, want %d", len(parent.Subtasks), len(tt.wantSubtaskStates))
			}
			for i, subtask := range parent.Subtasks {
				if subtask.State != tt.wantSubtaskStates[i] {
					t.Errorf("subtask[%d] state = %v, want %v", i, subtask.State, tt.wantSubtaskStates[i])
				}
			}
		})
	}
}
