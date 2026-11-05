// Package tui is the keyboard inbox for decide.
//
// It lists one folder at a time, marks a row read on Enter, and reloads the
// ledger while it is open. It does not answer items.
package tui

import (
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

const (
	boldOn      = "\x1b[1m"
	boldOff     = "\x1b[0m"
	screenClear = "\x1b[H\x1b[2J"

	// DefaultInterval is how often an open inbox reloads the ledger.
	DefaultInterval = 250 * time.Millisecond
)

// Folders is the inbox order: open, answered, done, withdrawn.
var Folders = []struct {
	Name  string
	State string
}{
	{Name: "Inbox", State: decide.StateOpen},
	{Name: "Answered", State: decide.StateAnswered},
	{Name: "Done", State: decide.StateDone},
	{Name: "Withdrawn", State: decide.StateWithdrawn},
}

// Model is the inbox list. Tests drive it with Key and Reload.
type Model struct {
	Dir    string
	Now    func() time.Time
	folder int
	cursor int
	items  []decide.Item
	status string
}

// New returns an inbox positioned on the open folder.
func New(dir string) *Model {
	return &Model{Dir: dir, Now: time.Now}
}

// Reload reads the current folder from disk. The cursor stays on the same id
// when that id is still present.
func (m *Model) Reload() error {
	keep := ""
	if m.cursor >= 0 && m.cursor < len(m.items) {
		keep = m.items[m.cursor].ID
	}
	led, err := decide.Open(m.Dir)
	if err != nil {
		return fmt.Errorf("open decide ledger: %w", err)
	}
	all, err := led.List()
	if err != nil {
		return fmt.Errorf("list decide ledger: %w", err)
	}
	items, err := decide.Select(all, Folders[m.folder].State)
	if err != nil {
		return fmt.Errorf("select decide folder: %w", err)
	}
	m.items = items
	m.cursor = 0
	for i, it := range items {
		if it.ID == keep {
			m.cursor = i
			break
		}
	}
	return nil
}

// Key applies one key. h and l switch folders. j and k move. Enter marks the
// row read. q quits.
func (m *Model) Key(r rune) (bool, error) {
	switch r {
	case 'q':
		return true, nil
	case 'j':
		if m.cursor < len(m.items)-1 {
			m.cursor++
		}
	case 'k':
		if m.cursor > 0 {
			m.cursor--
		}
	case 'h':
		if m.folder > 0 {
			m.folder--
			m.cursor = 0
			if err := m.Reload(); err != nil {
				return false, err
			}
		}
	case 'l', '\t':
		if m.folder < len(Folders)-1 {
			m.folder++
			m.cursor = 0
			if err := m.Reload(); err != nil {
				return false, err
			}
		}
	case '\r', '\n':
		if err := m.openRow(); err != nil {
			return false, err
		}
	}
	return false, nil
}

func (m *Model) openRow() error {
	if len(m.items) == 0 {
		return nil
	}
	id := m.items[m.cursor].ID
	led, err := decide.Open(m.Dir)
	if err != nil {
		return fmt.Errorf("open decide ledger: %w", err)
	}
	if _, err := led.MarkRead(id); err != nil {
		return fmt.Errorf("mark decide item read: %w", err)
	}
	m.status = "opened " + id
	return m.Reload()
}

// View renders the folder bar and the rows. Unread rows are bold.
func (m *Model) View() string {
	var b strings.Builder
	for i, folder := range Folders {
		name := folder.Name
		if i == m.folder {
			name = "[" + name + "]"
		}
		if i > 0 {
			b.WriteByte(' ')
		}
		b.WriteString(name)
	}
	b.WriteByte('\n')
	if len(m.items) == 0 {
		b.WriteString("(empty)\n")
	}
	now := time.Now()
	if m.Now != nil {
		now = m.Now()
	}
	for i, it := range m.items {
		marker := "  "
		if i == m.cursor {
			marker = "> "
		}
		line := marker + rowText(it, now)
		if it.Unread {
			line = boldOn + line + boldOff
		}
		b.WriteString(line)
		b.WriteByte('\n')
	}
	if m.status != "" {
		b.WriteString(m.status)
		b.WriteByte('\n')
	}
	return b.String()
}

func rowText(it decide.Item, now time.Time) string {
	deadline := "-"
	if it.Deadline != nil {
		deadline = it.Deadline.Format(time.RFC3339)
	}
	question := strings.Join(strings.Fields(it.Question), " ")
	return fmt.Sprintf("%s %s %s %s %s", it.ID, it.Owner, question, age(now.Sub(it.CreatedAt)), deadline)
}

func age(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	switch {
	case d < time.Minute:
		return "now"
	case d < time.Hour:
		return fmt.Sprintf("%dm", int(d/time.Minute))
	case d < 24*time.Hour:
		return fmt.Sprintf("%dh", int(d/time.Hour))
	default:
		return fmt.Sprintf("%dd", int(d/(24*time.Hour)))
	}
}

// Frame is one screen, cleared before the view.
func Frame(view string) string {
	return screenClear + view
}

// WriteFrame writes one screen.
func WriteFrame(w io.Writer, view string) error {
	if _, err := io.WriteString(w, Frame(view)); err != nil {
		return fmt.Errorf("draw decide inbox: %w", err)
	}
	return nil
}
