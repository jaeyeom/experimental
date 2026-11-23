package baseline

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const (
	shaA = "689e0fb9ba69f79a977c4be1f3212e85e6a1fea3"
	shaB = "b7fb2dbc38fc4836ddbe2aacabc64a4dfd050afe"
	shaC = "0000000000000000000000000000000000000000000000000000000000000001"
)

func loadString(t *testing.T, body string) (*File, error) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "b.json")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return Load(path)
}

func TestLoadTransitionsValid(t *testing.T) {
	f, err := loadString(t, `{"version":1,"checker":"x","entries":{
		"a/migrations/0001_x.py":{"transitions":[
			{"before":"`+shaA+`","after":"`+shaB+`","reason":"fix","issue":"#1"},
			{"before":"`+shaB+`","after":"deleted","reason":"retire"},
			{"before":"`+shaC+`","after":"`+shaC+`","reason":"sha256"}]}}}`)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got := len(f.Entries["a/migrations/0001_x.py"].Transitions); got != 3 {
		t.Errorf("transitions = %d, want 3", got)
	}
}

func TestLoadTransitionsInvalid(t *testing.T) {
	tr := func(before, after, reason string) string {
		return `{"before":"` + before + `","after":"` + after + `","reason":"` + reason + `"}`
	}
	tests := []struct {
		name, entries, wantErr string
	}{
		{"glob key", `"a/**/x.sql":{"transitions":[` + tr(shaA, shaB, "r") + `]}`, "glob"},
		{"limits and transitions", `"a.sql":{"limits":{"max_bytes":1},"transitions":[` + tr(shaA, shaB, "r") + `]}`, "both"},
		{"bad before", `"a.sql":{"transitions":[` + tr("xyz", shaB, "r") + `]}`, "before"},
		{"deleted before", `"a.sql":{"transitions":[` + tr("deleted", shaB, "r") + `]}`, "before"},
		{"bad after", `"a.sql":{"transitions":[` + tr(shaA, "ABC", "r") + `]}`, "after"},
		{"uppercase hash", `"a.sql":{"transitions":[` + tr(strings.ToUpper(shaA), shaB, "r") + `]}`, "before"},
		{"empty reason", `"a.sql":{"transitions":[` + tr(shaA, shaB, "") + `]}`, "reason"},
		{"duplicate", `"a.sql":{"transitions":[` + tr(shaA, shaB, "r") + `,` + tr(shaA, shaB, "s") + `]}`, "duplicate"},
		{"unknown field", `"a.sql":{"transitions":[{"before":"` + shaA + `","after":"` + shaB + `","reason":"r","why":"x"}]}`, "unknown"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := loadString(t, `{"version":1,"checker":"x","entries":{`+tt.entries+`}}`)
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Errorf("Load error = %v, want containing %q", err, tt.wantErr)
			}
		})
	}
}

func TestMarshalSortsTransitions(t *testing.T) {
	f := &File{Version: CurrentVersion, Checker: "x", Entries: map[string]Entry{
		"a.sql": {Transitions: []Transition{
			{Before: shaB, After: TransitionDeleted, Reason: "second"},
			{Before: shaA, After: shaB, Reason: "first", Issue: "#2"},
		}},
	}}
	data, err := Marshal(f)
	if err != nil {
		t.Fatal(err)
	}
	want := `{
  "checker": "x",
  "entries": {
    "a.sql": {
      "transitions": [
        {
          "after": "` + shaB + `",
          "before": "` + shaA + `",
          "issue": "#2",
          "reason": "first"
        },
        {
          "after": "deleted",
          "before": "` + shaB + `",
          "reason": "second"
        }
      ]
    }
  },
  "version": 1
}
`
	if string(data) != want {
		t.Errorf("Marshal =\n%s\nwant\n%s", data, want)
	}
	got, err := loadString(t, string(data))
	if err != nil {
		t.Fatalf("round trip: %v", err)
	}
	if len(got.Entries["a.sql"].Transitions) != 2 {
		t.Errorf("round trip lost transitions: %+v", got.Entries)
	}
}

func TestTransitionEntryIsNotExempt(t *testing.T) {
	f := &File{Version: CurrentVersion, Entries: map[string]Entry{
		"a.sql": {Transitions: []Transition{{Before: shaA, After: shaB, Reason: "r"}}},
	}}
	e := NewEvaluator(f)
	if e.CheckExempt("a.sql") {
		t.Error("CheckExempt = true for transition-only entry, want false")
	}
	if r := e.CheckMax("a.sql", "max_bytes", 10, 5); r.Passed() || r.Exempt {
		t.Errorf("CheckMax = %+v, want failing non-exempt", r)
	}
}

func TestTransitionEntriesNeverStale(t *testing.T) {
	entry := Entry{Transitions: []Transition{{Before: shaA, After: TransitionDeleted, Reason: "r"}}}
	f := &File{Version: CurrentVersion, Checker: "x", Entries: map[string]Entry{
		"gone.sql": entry,
		"gone.md":  {},
	}}
	e := NewEvaluator(f)
	findings := e.StaleFindings(map[string]bool{})
	if len(findings) != 1 || findings[0].Key != "gone.md" {
		t.Errorf("StaleFindings = %+v, want only gone.md", findings)
	}
	for _, mode := range []UpdateMode{ModeUpdate, ModeTighten} {
		out := e.UpdatedFile(map[string]bool{}, mode)
		got, ok := out.Entries["gone.sql"]
		if !ok || len(got.Transitions) != 1 || got.Transitions[0] != entry.Transitions[0] {
			t.Errorf("mode %d: UpdatedFile entries = %+v, want gone.sql kept", mode, out.Entries)
		}
		if _, ok := out.Entries["gone.md"]; ok {
			t.Errorf("mode %d: gone.md kept, want dropped", mode)
		}
	}
}

func TestAllowsTransition(t *testing.T) {
	tr := Transition{Before: shaA, After: shaB, Reason: "r"}
	f := &File{Version: CurrentVersion, Entries: map[string]Entry{
		"a.sql":    {Transitions: []Transition{tr}},
		"**/*.sql": {},
	}}
	if got, ok := f.AllowsTransition("a.sql", shaA, shaB); !ok || got != tr {
		t.Errorf("AllowsTransition(a.sql, A, B) = %+v, %v", got, ok)
	}
	for _, c := range [][3]string{
		{"a.sql", shaB, shaA},
		{"a.sql", shaA, TransitionDeleted},
		{"b.sql", shaA, shaB},
	} {
		if _, ok := f.AllowsTransition(c[0], c[1], c[2]); ok {
			t.Errorf("AllowsTransition%v = true, want false", c)
		}
	}
	var nilFile *File
	if _, ok := nilFile.AllowsTransition("a.sql", shaA, shaB); ok {
		t.Error("nil file allowed transition")
	}
}

func TestAddTransition(t *testing.T) {
	f := &File{Version: CurrentVersion, Entries: map[string]Entry{}}
	tr := Transition{Before: shaA, After: shaB, Reason: "r"}
	if err := f.AddTransition("a.sql", tr); err != nil {
		t.Fatal(err)
	}
	if err := f.AddTransition("a.sql", Transition{Before: shaA, After: shaB, Reason: "again"}); err != nil {
		t.Fatal(err)
	}
	if err := f.AddTransition("a.sql", Transition{Before: shaB, After: TransitionDeleted, Reason: "r2"}); err != nil {
		t.Fatal(err)
	}
	if got := f.Entries["a.sql"].Transitions; len(got) != 2 || got[0].Reason != "r" {
		t.Errorf("transitions = %+v, want 2 with first reason kept", got)
	}
	for name, c := range map[string]struct {
		path string
		tr   Transition
	}{
		"glob":      {"*.sql", tr},
		"bad hash":  {"b.sql", Transition{Before: "x", After: shaB, Reason: "r"}},
		"no reason": {"b.sql", Transition{Before: shaA, After: shaB}},
		"traversal": {"../b.sql", tr},
	} {
		if err := f.AddTransition(c.path, c.tr); err == nil {
			t.Errorf("%s: AddTransition succeeded, want error", name)
		}
	}
	f.Entries["lim.sql"] = Entry{Limits: map[string]int64{"max_bytes": 1}}
	if err := f.AddTransition("lim.sql", tr); err == nil {
		t.Error("AddTransition onto limits entry succeeded, want error")
	}
}
