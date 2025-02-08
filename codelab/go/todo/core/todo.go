// Package todo implementes todo application business logic.
package todo

import (
	"fmt"
	"strings"
)

type ItemState int

const (
	ItemStateIncomplete ItemState = iota
	ItemStateComplete
)

type Item struct {
	ID          int       `json:"id"`
	Description string    `json:"description"`
	State       ItemState `json:"state"`
}

func (i *Item) String() string {
	if i.State == ItemStateComplete {
		return fmt.Sprintf("%d. [x] %s", i.ID, i.Description)
	}
	return fmt.Sprintf("%d. [ ] %s", i.ID, i.Description)
}

type List struct {
	Items  []Item `json:"items"`
	LastID int    `json:"last_id"`
}

func NewList() *List {
	return &List{}
}

func (l *List) String() string {
	var s []string
	for _, i := range l.Items {
		s = append(s, i.String())
	}
	return strings.Join(s, "\n")
}

func (l *List) Add(item string) {
	l.LastID += 1
	l.Items = append(l.Items, Item{
		ID:          l.LastID,
		Description: item,
	})
}

func (l *List) FindItem(id int) *Item {
	for i := range l.Items {
		if l.Items[i].ID == id {
			return &l.Items[i]
		}
	}
	return nil
}

func (l *List) Complete(id int) {
	item := l.FindItem(id)
	if item != nil {
		item.State = ItemStateComplete
	}
}

func (l *List) Remove(id int) {
	for i := range l.Items {
		if l.Items[i].ID == id {
			l.Items = append(l.Items[:i], l.Items[i+1:]...)
			break
		}
	}
}
