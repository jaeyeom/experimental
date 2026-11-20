package tui

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// OrgctlRunner runs `orgctl` with args and returns its stdout.
type OrgctlRunner func(ctx context.Context, args []string) ([]byte, error)

// orgctlTimeout bounds one `orgctl tree --json`.
const orgctlTimeout = 5 * time.Second

// orgNode is one node of `orgctl tree --json`.
type orgNode struct {
	Label    string     `json:"label"`
	Kind     string     `json:"kind"`
	Max      int        `json:"max"`
	Children []*orgNode `json:"children"`
}

// orgRow is one row of the tree view. A row with contact -1 is a label:
// the operator, a role with no contact, a pool, or the other group.
type orgRow struct {
	prefix  string
	label   string
	contact int
}

func execOrgctl(ctx context.Context, args []string) ([]byte, error) {
	out, err := exec.CommandContext(ctx, "orgctl", args...).Output()
	if err != nil {
		var exit *exec.ExitError
		if errors.As(err, &exit) {
			if msg := strings.TrimSpace(string(exit.Stderr)); msg != "" {
				return nil, fmt.Errorf("orgctl: %s", msg)
			}
		}
		return nil, fmt.Errorf("orgctl: %w", err)
	}
	return out, nil
}

// toggleOrgTree switches Contacts between the flat list and the org
// tree. With ui.orgTree off it never runs orgctl. Turning the tree on
// reads `orgctl tree --json` again; a failure keeps the flat list with
// a note saying why.
func (m *Model) toggleOrgTree() {
	if !m.orgTreeOn {
		m.status = "the org tree is off; set ui.orgTree and ui.orgSpec"
		return
	}
	prev, hadPrev := m.selectedContact()
	m.orgView = !m.orgView
	m.orgRoot, m.orgNote = nil, ""
	if m.orgView {
		root, err := m.loadOrgTree()
		if err != nil {
			m.orgView = false
			m.orgNote = "org tree unavailable: " + err.Error()
		} else {
			m.orgRoot = root
		}
	}
	m.contactCursor = 0
	if hadPrev {
		for i, c := range m.visibleContacts() {
			if c.Name == prev.Name {
				m.contactCursor = i
			}
		}
	}
	m.revealContactCursor()
}

func (m *Model) loadOrgTree() (*orgNode, error) {
	if m.orgSpec == "" {
		return nil, errors.New("ui.orgSpec is not set")
	}
	run := m.orgRun
	if run == nil {
		run = execOrgctl
	}
	ctx, cancel := context.WithTimeout(context.Background(), orgctlTimeout)
	defer cancel()
	out, err := run(ctx, []string{"tree", "--json", m.orgSpec})
	if err != nil {
		return nil, err
	}
	var root orgNode
	if err := json.Unmarshal(out, &root); err != nil {
		return nil, fmt.Errorf("read orgctl tree --json: %w", err)
	}
	return &root, nil
}

// orgActive reports whether Contacts shows the tree.
func (m *Model) orgActive() bool {
	return m.orgView && m.orgRoot != nil
}

// orgLayout orders contacts by the tree and lists the rows to draw:
// the operator, then each role under its manager with its pools, then
// an "other" group of contacts the spec does not name.
func orgLayout(root *orgNode, contacts []contact) (ordered []contact, rows []orgRow) {
	byName := make(map[string]int, len(contacts))
	for i, c := range contacts {
		byName[c.Name] = i
	}
	placed := make([]bool, len(contacts))
	var walk func(n *orgNode, prefix, branch string)
	walk = func(n *orgNode, prefix, branch string) {
		row := orgRow{prefix: prefix + branch, label: n.Label, contact: -1}
		switch n.Kind {
		case "pool":
			row.label = fmt.Sprintf("[pool] %s (max %d)", n.Label, n.Max)
		case "role":
			if i, ok := byName[n.Label]; ok && !placed[i] {
				placed[i] = true
				row.contact = len(ordered)
				ordered = append(ordered, contacts[i])
			}
		}
		rows = append(rows, row)
		childPrefix := prefix
		switch branch {
		case "├── ":
			childPrefix += "│   "
		case "└── ":
			childPrefix += "    "
		}
		for i, child := range n.Children {
			b := "├── "
			if i == len(n.Children)-1 {
				b = "└── "
			}
			walk(child, childPrefix, b)
		}
	}
	walk(root, "", "")
	first := true
	for i, c := range contacts {
		if placed[i] {
			continue
		}
		if first {
			rows = append(rows, orgRow{label: "other", contact: -1})
			first = false
		}
		rows = append(rows, orgRow{prefix: "    ", contact: len(ordered)})
		ordered = append(ordered, c)
	}
	return ordered, rows
}

// orgCursorRow is the tree row of the contact under the cursor.
func (m *Model) orgCursorRow() int {
	_, rows := orgLayout(m.orgRoot, m.filteredContacts())
	for i, row := range rows {
		if row.contact == m.contactCursor {
			return i
		}
	}
	return m.contactCursor
}
