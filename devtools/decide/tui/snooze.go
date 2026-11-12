package tui

import (
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// snoozeEntry is one row of the snooze popup. A custom row has no time
// until its text parses. unsnooze clears the snooze.
type snoozeEntry struct {
	label    string
	at       time.Time
	custom   bool
	unsnooze bool
}

// beginSnooze opens the popup for the selected item. A snoozed item gets
// an unsnooze row first.
func (m *Model) beginSnooze() {
	it, ok := m.selectedItem()
	if !ok {
		return
	}
	m.snoozing = true
	m.snoozeItem = it
	m.snoozeCursor = 0
	m.snoozeCustom = ""
	m.snoozeErr = ""
}

func (m *Model) snoozeEntries() []snoozeEntry {
	var out []snoozeEntry
	if m.snoozeItem.Snoozed() {
		out = append(out, snoozeEntry{label: "unsnooze", unsnooze: true})
	}
	for _, p := range decide.SnoozePresets(m.now()) {
		out = append(out, snoozeEntry{label: p.Label, at: p.At})
	}
	return append(out, snoozeEntry{label: "custom", custom: true})
}

func (m *Model) moveSnooze(delta int) {
	n := len(m.snoozeEntries())
	m.snoozeCursor = max(min(m.snoozeCursor+delta, n-1), 0)
}

// snoozeKey drives the popup. j and k move, Enter picks, Esc cancels.
// Other printable keys type into the custom row and move there.
func (m *Model) snoozeKey(r rune) (bool, error) {
	entries := m.snoozeEntries()
	last := len(entries) - 1
	switch r {
	case '\x1b':
		m.snoozing = false
		return false, nil
	case 'j':
		m.moveSnooze(1)
		return false, nil
	case 'k':
		m.moveSnooze(-1)
		return false, nil
	case '\r':
		return false, m.pickSnooze(entries[m.snoozeCursor])
	case '\b', '\x7f':
		if m.snoozeCursor == last {
			m.snoozeCustom = dropLastRune(m.snoozeCustom)
			m.snoozeErr = ""
		}
		return false, nil
	}
	if r >= ' ' {
		m.snoozeCursor = last
		m.snoozeCustom += string(r)
		m.snoozeErr = ""
	}
	return false, nil
}

func (m *Model) pickSnooze(e snoozeEntry) error {
	id := m.snoozeItem.ID
	if e.unsnooze {
		m.snoozing = false
		if _, err := m.store().Unsnooze(id); err != nil {
			m.status = err.Error()
			return nil
		}
		m.status = "unsnoozed " + id
		return m.Reload()
	}
	at := e.at
	if e.custom {
		var err error
		at, err = decide.ParseSnooze(m.snoozeCustom, m.now())
		if err != nil {
			m.snoozeErr = err.Error()
			return nil
		}
	}
	m.snoozing = false
	updated, err := m.store().Snooze(id, at)
	if err != nil {
		m.status = err.Error()
		return nil
	}
	m.status = "snoozed " + id + " until " + m.wakeLabel(&updated)
	if m.mode == modeDetail {
		m.backToList()
	}
	return m.Reload()
}

// wakeLabel is the item's wake time in the inbox clock's zone.
func (m *Model) wakeLabel(it *decide.Item) string {
	if it.SnoozedUntil == nil {
		return ""
	}
	return decide.SnoozeLabel(it.SnoozedUntil.In(m.now().Location()))
}

// snoozeLines is the popup: the item, one row per choice, and an inline
// error for a custom time that does not parse.
func (m *Model) snoozeLines() []screenLine {
	lines := []screenLine{{text: plainText("snooze " + m.snoozeItem.ID)}}
	entries := m.snoozeEntries()
	labelW := 0
	for _, e := range entries {
		labelW = max(labelW, len(e.label))
	}
	loc := m.now().Location()
	for i, e := range entries {
		marker := "  "
		if i == m.snoozeCursor {
			marker = "> "
		}
		text := marker + e.label
		switch {
		case e.custom:
			text += ": " + m.snoozeCustom
			if i == m.snoozeCursor {
				text = marker + e.label + ": " + withCursor(m.snoozeCustom, len([]rune(m.snoozeCustom)))
			}
		case !e.unsnooze:
			text = marker + padRight(e.label, labelW) + "  " + decide.SnoozeLabel(e.at.In(loc))
		}
		lines = append(lines, screenLine{text: plainText(text)})
	}
	if m.snoozeErr != "" {
		lines = append(lines, screenLine{text: plainText(m.snoozeErr)})
	}
	return boxLines(lines)
}

const snoozeHint = "j/k move  enter picks  esc cancels  custom: 2h30m, HH:MM, YYYY-MM-DD HH:MM, mon 9:00"
