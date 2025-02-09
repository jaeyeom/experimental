package core

import (
	"testing"
)

func TestMoveUp(t *testing.T) {
	tests := []struct {
		name    string
		setup   func() (*List, string) // returns list and ID to move
		wantErr error
		verify  func(*testing.T, *List)
	}{
		{
			name: "moving first root item up returns error",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("First")
				list.Add("Second")
				return list, list.Items[0].ID
			},
			wantErr: ErrNoSibling,
		},
		{
			name: "moving first subtask up returns error",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("Parent")
				if err := list.AddSubtask(list.Items[0].ID, "First Sub"); err != nil {
					return nil, ""
				}
				if err := list.AddSubtask(list.Items[0].ID, "Second Sub"); err != nil {
					return nil, ""
				}
				return list, list.Items[0].Subtasks[0].ID
			},
			wantErr: ErrNoSibling,
		},
		{
			name: "moving middle root item up succeeds",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("First")
				list.Add("Second")
				list.Add("Third")
				return list, list.Items[1].ID
			},
			verify: func(t *testing.T, list *List) {
				if len(list.Items) != 3 {
					t.Errorf("got %d items, want 3", len(list.Items))
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
			name: "moving middle subtask up succeeds",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("Parent")
				if err := list.AddSubtask(list.Items[0].ID, "First Sub"); err != nil {
					return nil, ""
				}
				if err := list.AddSubtask(list.Items[0].ID, "Second Sub"); err != nil {
					return nil, ""
				}
				if err := list.AddSubtask(list.Items[0].ID, "Third Sub"); err != nil {
					return nil, ""
				}
				return list, list.Items[0].Subtasks[1].ID
			},
			verify: func(t *testing.T, list *List) {
				subs := list.Items[0].Subtasks
				if len(subs) != 3 {
					t.Errorf("got %d subtasks, want 3", len(subs))
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
			name: "moving non-existent item returns error",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("First")
				return list, "non-existent-id"
			},
			wantErr: ErrItemNotFound,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			list, id := tt.setup()
			err := list.MoveUp(id)
			if err != tt.wantErr {
				t.Errorf("MoveUp() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.verify != nil {
				tt.verify(t, list)
			}
		})
	}
}

func TestMoveDown(t *testing.T) {
	tests := []struct {
		name    string
		setup   func() (*List, string) // returns list and ID to move
		wantErr error
		verify  func(*testing.T, *List)
	}{
		{
			name: "moving last root item down returns error",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("First")
				list.Add("Second")
				return list, list.Items[1].ID
			},
			wantErr: ErrNoSibling,
		},
		{
			name: "moving last subtask down returns error",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("Parent")
				if err := list.AddSubtask(list.Items[0].ID, "First Sub"); err != nil {
					return nil, ""
				}
				if err := list.AddSubtask(list.Items[0].ID, "Second Sub"); err != nil {
					return nil, ""
				}
				return list, list.Items[0].Subtasks[1].ID
			},
			wantErr: ErrNoSibling,
		},
		{
			name: "moving middle root item down succeeds",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("First")
				list.Add("Second")
				list.Add("Third")
				return list, list.Items[1].ID
			},
			verify: func(t *testing.T, list *List) {
				if len(list.Items) != 3 {
					t.Errorf("got %d items, want 3", len(list.Items))
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
			name: "moving middle subtask down succeeds",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("Parent")
				if err := list.AddSubtask(list.Items[0].ID, "First Sub"); err != nil {
					return nil, ""
				}
				if err := list.AddSubtask(list.Items[0].ID, "Second Sub"); err != nil {
					return nil, ""
				}
				if err := list.AddSubtask(list.Items[0].ID, "Third Sub"); err != nil {
					return nil, ""
				}
				return list, list.Items[0].Subtasks[1].ID
			},
			verify: func(t *testing.T, list *List) {
				subs := list.Items[0].Subtasks
				if len(subs) != 3 {
					t.Errorf("got %d subtasks, want 3", len(subs))
				}
				if subs[1].Description != "Third Sub" {
					t.Errorf("second subtask got %q, want 'Third Sub'", subs[1].Description)
				}
				if subs[2].Description != "Second Sub" {
					t.Errorf("third subtask got %q, want 'Second Sub'", subs[2].Description)
				}
			},
		},
		{
			name: "moving non-existent item returns error",
			setup: func() (*List, string) {
				list := NewList()
				list.Add("First")
				return list, "non-existent-id"
			},
			wantErr: ErrItemNotFound,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			list, id := tt.setup()
			err := list.MoveDown(id)
			if err != tt.wantErr {
				t.Errorf("MoveDown() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.verify != nil {
				tt.verify(t, list)
			}
		})
	}
}
