package decide

import (
	"path/filepath"
	"testing"
	"time"
)

// logLedger files and answers items at increasing times and returns the ledger.
func logLedger(t *testing.T) *Ledger {
	t.Helper()
	led, err := Open(filepath.Join(t.TempDir(), "decide"))
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { now = now.Add(time.Hour); return now })
	opts := []Option{{Key: "a", Label: "Alpha", Recommended: true}, {Key: "b", Label: "Beta"}}
	mk := func(class, q string, answer string) {
		t.Helper()
		it, err := led.Create(&Item{Question: q, Owner: "ada", Kind: KindRelay, Class: class, Options: opts, FreeText: true})
		if err != nil {
			t.Fatal(err)
		}
		if answer != "" {
			if _, err := led.Answer(it.ID, answer, ViaCLI); err != nil {
				t.Fatal(err)
			}
		}
	}
	mk(ClassDecision, "Which database for billing?", "a")           // D-001
	mk(ClassFact, "What is the staging hostname?", "stage.example") // Q-002
	mk(ClassDecision, "Pick a queue", "b")                          // D-003
	mk(ClassDecision, "Unanswered database question", "")           // D-004 open
	mk(ClassTask, "Rotate database password", "a")                  // T-005
	return led
}

func ids(items []Item) []string {
	var out []string
	for _, it := range items {
		out = append(out, it.ID)
	}
	return out
}

func equalIDs(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func search(t *testing.T, led *Ledger, opts SearchOptions) []string {
	t.Helper()
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	got, err := SearchLog(items, opts)
	if err != nil {
		t.Fatal(err)
	}
	return ids(got)
}

func TestSearchLogMatchesQuestionAnswerOutcomeNewestFirst(t *testing.T) {
	led := logLedger(t)
	if _, err := led.Done("D-003", "queue is NATS, deployed"); err != nil {
		t.Fatal(err)
	}
	if got := search(t, led, SearchOptions{Words: []string{"database"}}); !equalIDs(got, []string{"D-001"}) {
		t.Fatalf("question match = %v, want only answered D-001 (open and task excluded)", got)
	}
	if got := search(t, led, SearchOptions{Words: []string{"STAGE.example"}}); !equalIDs(got, []string{"Q-002"}) {
		t.Fatalf("answer match = %v", got)
	}
	if got := search(t, led, SearchOptions{Words: []string{"nats"}}); !equalIDs(got, []string{"D-003"}) {
		t.Fatalf("outcome match = %v", got)
	}
	// Every word must match; newest first.
	if got := search(t, led, SearchOptions{}); !equalIDs(got, []string{"D-003", "Q-002", "D-001"}) {
		t.Fatalf("all = %v, want newest first", got)
	}
	if got := search(t, led, SearchOptions{Words: []string{"queue", "database"}}); len(got) != 0 {
		t.Fatalf("both words = %v, want none", got)
	}
}

func TestSearchLogClasses(t *testing.T) {
	led := logLedger(t)
	if got := search(t, led, SearchOptions{Classes: []string{ClassFact}}); !equalIDs(got, []string{"Q-002"}) {
		t.Fatalf("fact only = %v", got)
	}
	if got := search(t, led, SearchOptions{Classes: []string{ClassTask}, Words: []string{"password"}}); !equalIDs(got, []string{"T-005"}) {
		t.Fatalf("task = %v", got)
	}
	if _, err := SearchLog(nil, SearchOptions{Classes: []string{"bogus"}}); err == nil {
		t.Fatal("bogus class accepted")
	}
}

func TestSupersedeHidesOldUnlessAll(t *testing.T) {
	led := logLedger(t)
	old, newer, err := led.Supersede("D-001", "D-003", "ada")
	if err != nil {
		t.Fatal(err)
	}
	if old.SupersededBy != "D-003" || newer.Supersedes != "D-001" {
		t.Fatalf("links = %q, %q", old.SupersededBy, newer.Supersedes)
	}
	for _, id := range []string{"D-001", "D-003"} {
		got, err := led.Get(id)
		if err != nil {
			t.Fatal(err)
		}
		if last := got.Messages[len(got.Messages)-1]; last.Author != "system" {
			t.Fatalf("%s last message = %+v, want system note", id, last)
		}
	}
	if got := search(t, led, SearchOptions{}); !equalIDs(got, []string{"D-003", "Q-002"}) {
		t.Fatalf("default = %v", got)
	}
	if got := search(t, led, SearchOptions{All: true}); !equalIDs(got, []string{"D-003", "Q-002", "D-001"}) {
		t.Fatalf("all = %v", got)
	}
	// Refusals: self, missing, already superseded, cycle.
	for _, c := range [][2]string{{"D-003", "D-003"}, {"D-003", "D-999"}, {"D-001", "Q-002"}, {"D-003", "D-001"}} {
		if _, _, err := led.Supersede(c[0], c[1], ""); err == nil {
			t.Fatalf("Supersede(%v) succeeded, want error", c)
		}
	}
}
