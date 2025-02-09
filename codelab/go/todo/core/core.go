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
	// ItemStateIncomplete indicates that the todo item is not yet
	// completed.
	ItemStateIncomplete ItemState = iota
	// ItemStateComplete indicates that the todo item has been completed.
	ItemStateComplete
)

// Item represents a single todo item with its unique identifier, description,
// and completion state.
type Item struct {
	ID          string    `json:"id"`
	Description string    `json:"description"`
	State       ItemState `json:"state"`
}

// String returns a string representation of the todo item.
func (i *Item) String() string {
	if i.State == ItemStateComplete {
		return fmt.Sprintf("%s. [x] %s", i.ID, i.Description)
	}
	return fmt.Sprintf("%s. [ ] %s", i.ID, i.Description)
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
	})
}

var (
	// ErrItemNotFound is returned when a requested item does not exist in the list.
	ErrItemNotFound = errors.New("item not found")
	// ErrAmbiguousItem is returned when an item ID prefix matches multiple items.
	ErrAmbiguousItem = errors.New("item ambiguous")
)

// FindItem finds a todo item by its ID prefix.
func (l *List) FindItem(id string) (*Item, error) {
	var found *Item
	for i := range l.Items {
		if strings.HasPrefix(l.Items[i].ID, id) {
			if found != nil {
				return nil, ErrAmbiguousItem
			}
			found = &l.Items[i]
		}
	}
	if found == nil {
		return nil, ErrItemNotFound
	}
	return found, nil
}

// Complete marks a todo item as completed by its ID prefix.
func (l *List) Complete(id string) error {
	item, err := l.FindItem(id)
	if err != nil {
		return err
	}
	if item != nil {
		item.State = ItemStateComplete
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
	if removed != -1 {
		l.Items = append(l.Items[:removed], l.Items[removed+1:]...)
	}
	return nil
}
