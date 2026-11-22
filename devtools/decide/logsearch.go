package decide

import (
	"fmt"
	"sort"
	"strings"
	"time"
)

// SearchOptions selects entries of the decision log.
type SearchOptions struct {
	// Words must all appear (case-insensitive) in the question, answer
	// or outcome of an entry. No words match every entry.
	Words []string
	// Classes limits the item classes searched. Empty means decision and fact.
	Classes []string
	// All also returns superseded items.
	All bool
}

// DefaultLogClasses are the classes the decision log searches by default.
var DefaultLogClasses = []string{ClassDecision, ClassFact}

// LogTime is when an item became a log entry: its answer, else its
// completion, else its creation.
func LogTime(it *Item) time.Time {
	switch {
	case !it.AnsweredAt.IsZero():
		return it.AnsweredAt
	case !it.DoneAt.IsZero():
		return it.DoneAt
	}
	return it.CreatedAt
}

// isLogEntry reports whether it has a user answer or an owner outcome.
// Open and withdrawn items are not entries.
func isLogEntry(it *Item) bool {
	if it.State == StateWithdrawn {
		return false
	}
	return it.Answer != nil || (it.State == StateDone && it.Outcome != "")
}

// SearchLog returns the log entries of items that match opts, newest first.
// It never changes items.
func SearchLog(items []Item, opts SearchOptions) ([]Item, error) {
	classes := map[string]bool{}
	names := opts.Classes
	if len(names) == 0 {
		names = DefaultLogClasses
	}
	for _, raw := range names {
		class, err := ParseClass(raw)
		if err != nil {
			return nil, err
		}
		classes[class] = true
	}
	var out []Item
	for i := range items {
		it := &items[i]
		if !classes[it.EffectiveClass()] || !isLogEntry(it) {
			continue
		}
		if it.SupersededBy != "" && !opts.All {
			continue
		}
		if !matchesAll(it, opts.Words) {
			continue
		}
		out = append(out, cloneItem(it))
	}
	sort.SliceStable(out, func(i, j int) bool {
		ti, tj := LogTime(&out[i]), LogTime(&out[j])
		if !ti.Equal(tj) {
			return ti.After(tj)
		}
		ni, _ := parseID(out[i].ID)
		nj, _ := parseID(out[j].ID)
		return ni > nj
	})
	return out, nil
}

func matchesAll(it *Item, words []string) bool {
	hay := strings.ToLower(it.Question + "\n" + AnswerText(it) + "\n" + it.Outcome)
	for _, w := range words {
		if !strings.Contains(hay, strings.ToLower(w)) {
			return false
		}
	}
	return true
}

// Supersede records that item newID replaces item oldID, on both threads,
// and returns both items. It refuses an item superseding itself, an old
// item that is already superseded, and a link that would form a cycle.
func (l *Ledger) Supersede(oldID, newID, by string) (oldItem, newItem Item, err error) {
	err = l.locked(true, func(st *store) error {
		old, err := findItem(st, oldID)
		if err != nil {
			return err
		}
		newer, err := findItem(st, newID)
		if err != nil {
			return err
		}
		if old.ID == newer.ID {
			return fmt.Errorf("decide item %s cannot supersede itself", old.ID)
		}
		if old.SupersededBy != "" {
			return fmt.Errorf("decide item %s is already superseded by %s", old.ID, old.SupersededBy)
		}
		for cur := newer; cur.SupersededBy != ""; {
			if cur.SupersededBy == old.ID {
				return fmt.Errorf("decide item %s already supersedes %s", old.ID, newer.ID)
			}
			if cur, err = findItem(st, cur.SupersededBy); err != nil {
				return err
			}
		}
		if newer.Supersedes != "" {
			return fmt.Errorf("decide item %s already supersedes %s", newer.ID, newer.Supersedes)
		}
		old.SupersededBy = newer.ID
		newer.Supersedes = old.ID
		now := l.clock().UTC()
		suffix := ""
		if by != "" {
			suffix = " (by " + by + ")"
		}
		ensureMessages(old)
		old.Messages = append(old.Messages, Message{
			Author: "system", Time: now, By: by,
			Text: "superseded by " + newer.ID + suffix,
		})
		ensureMessages(newer)
		newer.Messages = append(newer.Messages, Message{
			Author: "system", Time: now, By: by,
			Text: "supersedes " + old.ID + suffix,
		})
		oldItem, newItem = *old, *newer
		return nil
	})
	return oldItem, newItem, err
}
