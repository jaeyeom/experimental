// Package core implements todo application business logic. It provides the core
// data structures and operations for managing todo items, including creating,
// completing, and removing items from a todo list.
package core

import (
	"errors"
	"fmt"
	"strings"

	"github.com/google/uuid"
)

// ItemState represents the completion state of a todo item.
type ItemState int

const (
	// ItemStateNotStarted indicates that the todo item has not been started.
	ItemStateNotStarted ItemState = iota
	// ItemStateDone indicates that the todo item and all its subtasks are completed.
	ItemStateDone
	// ItemStatePartiallyDone indicates that some subtasks are completed.
	ItemStatePartiallyDone
)

// Item represents a single todo item with its unique identifier, description,
// completion state, and optional subtasks.
type Item struct {
	ID          string    `json:"id"`
	Description string    `json:"description"`
	State       ItemState `json:"state"`
	ParentID    string    `json:"parent_id,omitempty"`
	Subtasks    []Item    `json:"subtasks,omitempty"`
}

// String returns a string representation of the todo item.
func (i *Item) String() string {
	return i.string("")
}

// string returns a string representation of the todo item with the given indentation.
func (i *Item) string(indent string) string {
	var state string
	switch i.State {
	case ItemStateDone:
		state = "[x]"
	case ItemStatePartiallyDone:
		state = "[-]"
	default:
		state = "[ ]"
	}
	result := fmt.Sprintf("%s%s. %s %s", indent, i.ID, state, i.Description)
	if len(i.Subtasks) > 0 {
		for _, subtask := range i.Subtasks {
			result += "\n" + subtask.string(indent+"  ")
		}
	}
	return result
}

// SetState sets the item's state and also updates the states of its subtasks.
// It does not update the state of the parent.
func (i *Item) SetState(s ItemState) {
	if s == i.State {
		return
	}
	i.State = s
	if s != ItemStatePartiallyDone {
		for idx := range i.Subtasks {
			i.Subtasks[idx].SetState(s)
		}
	}
}

// ValidateState updates its state based on the subtasks' states.
func (i *Item) ValidateState() {
	size := len(i.Subtasks)
	if size == 0 {
		return
	}
	done, notStarted := 0, 0

	for idx := range i.Subtasks {
		switch i.Subtasks[idx].State {
		case ItemStateDone:
			done++
		case ItemStateNotStarted:
			notStarted++
		}
	}
	if done == size {
		i.State = ItemStateDone
		return
	}
	if notStarted == size {
		i.State = ItemStateNotStarted
		return
	}
	i.State = ItemStatePartiallyDone
}

// List represents a collection of todo items with a function to generate new
// unique identifiers for items.
type List struct {
	Items []Item        `json:"items"`
	NewID func() string `json:"-"`
}

// ListOption defines a function type that can modify a List.
type ListOption func(*List)

// WithNewID returns a ListOption that sets the ID generation function for a
// List.
func WithNewID(newID func() string) ListOption {
	return func(l *List) {
		l.NewID = newID
	}
}

// NewList creates a new todo list with the provided options.
func NewList(opts ...ListOption) *List {
	l := &List{}
	for _, opt := range opts {
		opt(l)
	}
	return l
}

// String returns a string representation of the todo list.
func (l *List) String() string {
	var s []string
	for _, i := range l.Items {
		s = append(s, i.String())
	}
	return strings.Join(s, "\n")
}

// Add adds a new todo item to the list.
func (l *List) Add(item string) {
	if l.NewID == nil {
		l.NewID = func() string {
			return uuid.New().String()
		}
	}
	l.Items = append(l.Items, Item{
		ID:          l.NewID(),
		Description: item,
		State:       ItemStateNotStarted,
	})
}

// AddSubtask adds a new subtask to the specified parent task.
func (l *List) AddSubtask(parentID, description string) error {
	parent, err := l.FindItem(parentID)
	if err != nil {
		return err
	}

	if l.NewID == nil {
		l.NewID = func() string {
			return uuid.New().String()
		}
	}

	newItem := Item{
		ID:          l.NewID(),
		Description: description,
		State:       ItemStateNotStarted,
		ParentID:    parent.ID,
	}
	parent.Subtasks = append(parent.Subtasks, newItem)

	// If parent was done and we add a new subtask, it becomes partially done
	if parent.State == ItemStateDone {
		parent.State = ItemStatePartiallyDone
	}
	return nil
}

// updateParentState updates the state of a parent task based on its subtasks'
// states.
func (l *List) updateParentState(item *Item) {
	if len(item.Subtasks) == 0 {
		return
	}

	allDone := true
	anyDone := false

	for _, subtask := range item.Subtasks {
		if subtask.State == ItemStateDone {
			anyDone = true
		} else {
			allDone = false
		}
	}

	switch {
	case allDone:
		item.State = ItemStateDone
	case anyDone:
		item.State = ItemStatePartiallyDone
	default:
		item.State = ItemStateNotStarted
	}
}

var (
	// ErrItemNotFound is returned when a requested item does not exist in the list.
	ErrItemNotFound = errors.New("item not found")
	// ErrAmbiguousItem is returned when an item ID prefix matches multiple items.
	ErrAmbiguousItem = errors.New("ambiguous item ID")
	// ErrNoSibling is returned when trying to move an item but there is no sibling
	// in the requested direction (e.g., moving first item up or last item down).
	ErrNoSibling = errors.New("no sibling in that direction")
)

// findItemInSubtasks recursively searches for an item in the subtasks tree.
func (l *List) findItemInSubtasks(id string, items []Item) (*Item, error) {
	var found *Item
	for i := range items {
		if strings.HasPrefix(items[i].ID, id) {
			if found != nil {
				return nil, ErrAmbiguousItem
			}
			found = &items[i]
		}
		// Search in subtasks
		if len(items[i].Subtasks) > 0 {
			if subFound, err := l.findItemInSubtasks(id, items[i].Subtasks); err != nil {
				if err == ErrAmbiguousItem {
					return nil, err
				}
			} else if subFound != nil {
				if found != nil {
					return nil, ErrAmbiguousItem
				}
				found = subFound
			}
		}
	}
	return found, nil
}

// FindItem finds a todo item by its ID prefix.
func (l *List) FindItem(id string) (*Item, error) {
	found, err := l.findItemInSubtasks(id, l.Items)
	if err != nil {
		return nil, err
	}
	if found == nil {
		return nil, ErrItemNotFound
	}
	return found, nil
}

// Complete marks a task as done. If the task has subtasks, they will also be
// marked as done. If a task's subtasks are all completed, the parent task will
// be marked as done. If some but not all subtasks are completed, the parent
// task will be marked as partially done.
func (l *List) Complete(id string) error {
	item, err := l.FindItem(id)
	if err != nil {
		return err
	}

	item.SetState(ItemStateDone)
	if item.ParentID != "" {
		parent := l.FindByID(item.ParentID)
		if parent != nil {
			parent.ValidateState()
		}
	}

	return nil
}

// Undo marks a task as not started. If the task has subtasks, they will also be marked
// as not started. If a task's parent has other completed subtasks, the parent's state
// will be updated accordingly.
func (l *List) Undo(id string) error {
	item, err := l.FindItem(id)
	if err != nil {
		return err
	}

	// Set the state of this item and its subtasks to not started
	item.SetState(ItemStateNotStarted)

	// Update parent task state if this is a subtask
	if item.ParentID != "" {
		parent := l.FindByID(item.ParentID)
		if parent != nil {
			parent.ValidateState()
		}
	}

	return nil
}

// Uncomplete marks a todo item as incomplete by its ID prefix.
func (l *List) Uncomplete(id string) error {
	item, err := l.FindItem(id)
	if err != nil {
		return err
	}
	item.State = ItemStateNotStarted

	// If this item has a parent, update the parent's state
	if item.ParentID != "" {
		parent, err := l.FindItem(item.ParentID)
		if err != nil {
			return err
		}
		l.updateParentState(parent)
	}
	return nil
}

// Remove removes a todo item by its ID prefix.
func (l *List) Remove(id string) error {
	removed := -1
	for i := range l.Items {
		if strings.HasPrefix(l.Items[i].ID, id) {
			if removed != -1 {
				return ErrAmbiguousItem
			}
			removed = i
		}
	}
	if removed == -1 {
		return ErrItemNotFound
	}
	l.Items = append(l.Items[:removed], l.Items[removed+1:]...)
	return nil
}

// FindByID finds a todo item by its ID, including searching through subtasks.
func (l *List) FindByID(id string) *Item {
	// Helper function to search recursively through items and their subtasks
	var findRecursive func(items []Item) *Item
	findRecursive = func(items []Item) *Item {
		for i := range items {
			if items[i].ID == id {
				return &items[i]
			}
			if found := findRecursive(items[i].Subtasks); found != nil {
				return found
			}
		}
		return nil
	}
	return findRecursive(l.Items)
}

// findItemContainer finds an item and its container (either root list or parent's subtasks).
// It returns the container slice, index of the item, and whether the item was found.
func (l *List) findItemContainer(id string) (*[]Item, int, error) {
	// Check root items first
	var matches []struct {
		container *[]Item
		index     int
	}

	// Helper function to check if an ID matches the prefix
	isMatch := func(itemID string) bool {
		return strings.HasPrefix(itemID, id)
	}

	// Check root items
	for i, item := range l.Items {
		if isMatch(item.ID) {
			matches = append(matches, struct {
				container *[]Item
				index     int
			}{&l.Items, i})
		}
	}

	// If not found in root items, search in subtasks
	for i := range l.Items {
		var findInSubtasks func(items []Item) bool
		findInSubtasks = func(items []Item) bool {
			for j, item := range items {
				if isMatch(item.ID) {
					matches = append(matches, struct {
						container *[]Item
						index     int
					}{&items, j})
				}
				if len(item.Subtasks) > 0 && findInSubtasks(item.Subtasks) {
					return true
				}
			}
			return false
		}
		findInSubtasks(l.Items[i].Subtasks)
	}

	if len(matches) == 0 {
		return nil, 0, ErrItemNotFound
	}
	if len(matches) > 1 {
		return nil, 0, ErrAmbiguousItem
	}
	return matches[0].container, matches[0].index, nil
}

// move moves a todo item in the specified direction among its siblings.
// direction should be -1 for up or +1 for down.
func (l *List) move(id string, direction int) error {
	container, idx, err := l.findItemContainer(id)
	if err != nil {
		return err
	}

	// Check bounds based on direction
	newIdx := idx + direction
	if newIdx < 0 || newIdx >= len(*container) {
		return ErrNoSibling
	}

	// Swap with adjacent item
	(*container)[idx], (*container)[newIdx] = (*container)[newIdx], (*container)[idx]
	return nil
}

// MoveUp moves a todo item up among its siblings. If the item is the first sibling,
// it returns ErrNoSibling. If the item doesn't exist, it returns ErrItemNotFound.
func (l *List) MoveUp(id string) error {
	return l.move(id, -1)
}

// MoveDown moves a todo item down among its siblings. If the item is the last sibling,
// it returns ErrNoSibling. If the item doesn't exist, it returns ErrItemNotFound.
func (l *List) MoveDown(id string) error {
	return l.move(id, 1)
}
