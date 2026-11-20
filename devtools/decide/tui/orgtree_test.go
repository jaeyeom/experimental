package tui

import (
	"context"
	"errors"
	"strings"
	"testing"
)

// orgTreeJSON is `orgctl tree --json` for a fictional desk: a lead with
// a clerk and a pool, and a night role with no contact.
const orgTreeJSON = `{"label":"user","kind":"root","children":[
  {"label":"desk/lead","kind":"role","children":[
    {"label":"desk/clerk","kind":"role","children":[]},
    {"label":"typists","kind":"pool","max":3,"children":[]}]},
  {"label":"desk/night","kind":"role","children":[]}]}`

type fakeOrgctl struct {
	out   string
	err   error
	calls [][]string
}

func (f *fakeOrgctl) run(_ context.Context, args []string) ([]byte, error) {
	f.calls = append(f.calls, args)
	return []byte(f.out), f.err
}

func orgContacts(t *testing.T, on bool, fake *fakeOrgctl) *Model {
	t.Helper()
	t.Setenv("NO_COLOR", "")
	m := openContacts(t, []string{"desk/clerk", "desk/lead", "ops/pager"}, `[
		{"name":"desk/lead","liveness":"alive"},
		{"name":"desk/clerk","liveness":"alive"},
		{"name":"ops/pager","liveness":"alive"}
	]`)
	m.orgTreeOn = on
	m.orgSpec = "/work/desk.json"
	if fake != nil {
		m.orgRun = fake.run
	}
	return m
}

func TestOrgTreeOffNeverRunsOrgctl(t *testing.T) {
	fake := &fakeOrgctl{out: orgTreeJSON}
	m := orgContacts(t, false, fake)
	press(t, m, "t")
	if len(fake.calls) != 0 {
		t.Fatalf("orgctl ran with the switch off: %v", fake.calls)
	}
	if !strings.Contains(m.status, "org tree is off") {
		t.Fatalf("status %q", m.status)
	}
	checkGolden(t, "contacts_org_off_120.golden", m.View())
}

func TestOrgTreeView(t *testing.T) {
	fake := &fakeOrgctl{out: orgTreeJSON}
	m := orgContacts(t, true, fake)
	press(t, m, "t")
	if len(fake.calls) != 1 || strings.Join(fake.calls[0], " ") != "tree --json /work/desk.json" {
		t.Fatalf("orgctl calls %v", fake.calls)
	}
	if got := strings.Join(contactNames(m.visibleContacts()), ","); got != "desk/lead,desk/clerk,ops/pager" {
		t.Fatalf("tree order %s", got)
	}
	view := m.View()
	for _, want := range []string{"user", "├── desk/lead", "│   ├── desk/clerk", "│   └── [pool] typists (max 3)", "└── desk/night", "other", "    ops/pager"} {
		if !strings.Contains(view, want) {
			t.Errorf("tree view lacks %q", want)
		}
	}
	checkGolden(t, "contacts_org_tree_120.golden", view)

	press(t, m, "jj")
	if c, _ := m.selectedContact(); c.Name != "ops/pager" {
		t.Fatalf("cursor on %s, want ops/pager in other", c.Name)
	}
	press(t, m, "t")
	if m.orgActive() || len(fake.calls) != 1 {
		t.Fatalf("t did not return to the list: active %v, calls %d", m.orgActive(), len(fake.calls))
	}
	if c, _ := m.selectedContact(); c.Name != "ops/pager" {
		t.Fatalf("cursor moved to %s", c.Name)
	}
}

func TestOrgTreeFailureFallsBackToTheList(t *testing.T) {
	fake := &fakeOrgctl{err: errors.New("orgctl: reportsTo cycle: a → b → a")}
	m := orgContacts(t, true, fake)
	press(t, m, "t")
	view := m.View()
	if m.orgActive() || !strings.Contains(view, "org tree unavailable: orgctl: reportsTo cycle") || !strings.Contains(view, "ops/pager") {
		t.Fatalf("failure did not fall back with a note\n%s", view)
	}
}

func TestOrgTreeMissingBinaryFallsBack(t *testing.T) {
	m := orgContacts(t, true, nil)
	t.Setenv("PATH", t.TempDir())
	press(t, m, "t")
	if m.orgActive() || !strings.Contains(m.orgNote, "org tree unavailable: orgctl:") || !strings.Contains(m.orgNote, "not found") {
		t.Fatalf("note %q", m.orgNote)
	}
	if !strings.Contains(m.View(), "org tree unavailable") {
		t.Fatal("the note is not shown")
	}
}
