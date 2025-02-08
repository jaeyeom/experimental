// Package core implementes todo application business logic.
package core

import (
	"errors"
	"fmt"
	"strings"

	"github.com/google/uuid"
)

type ItemState int

const (
	ItemStateIncomplete ItemState = iota
	ItemStateComplete
)

type Item struct {
	ID          string    `json:"id"`
	Description string    `json:"description"`
	State       ItemState `json:"state"`
}

func (i *Item) String() string {
	if i.State == ItemStateComplete {
		return fmt.Sprintf("%s. [x] %s", i.ID, i.Description)
	}
	return fmt.Sprintf("%s. [ ] %s", i.ID, i.Description)
}

type List struct {
	Items []Item        `json:"items"`
	NewID func() string `json:"-"`
}

type ListOption func(*List)

func WithNewID(newID func() string) ListOption {
	return func(l *List) {
		l.NewID = newID
	}
}

func NewList(opts ...ListOption) *List {
	l := &List{}
	for _, opt := range opts {
		opt(l)
	}
	return l
}

func (l *List) String() string {
	var s []string
	for _, i := range l.Items {
		s = append(s, i.String())
	}
	return strings.Join(s, "\n")
}

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
	ErrItemNotFound  = errors.New("item not found")
	ErrAmbiguousItem = errors.New("item ambiguous")
)

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
	l.Items = append(l.Items[:removed], l.Items[removed+1:]...)
	return nil
}
