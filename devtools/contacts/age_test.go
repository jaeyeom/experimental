package contacts

import (
	"errors"
	"path/filepath"
	"testing"
)

// ageFixture: today is 2026-03-10 and retention is 30 days, so the cutoff
// is 2026-02-08.
func ageFixture(t *testing.T) (*Store, string) {
	t.Helper()
	s := newStore(t)
	ada := mustAdd(t, s, Person{Name: "Ada"})
	for _, e := range []struct{ date, channel, link string }{
		{"2026-01-03", "chat", "https://x/1"},
		{"2026-01-20", "email", "https://x/2"},
		{"2026-01-25", "chat", "https://x/3"},
		{"2026-02-01", "chat", "https://x/4"},
		{"2026-02-09", "chat", "https://x/5"}, // inside retention
	} {
		ev := Event{PersonID: ada, Date: e.date, Channel: e.channel, Link: e.link, Summary: "s " + e.link, Reporter: "ops"}
		if _, _, err := s.Report(ev, DefaultSummaryMax); err != nil {
			t.Fatal(err)
		}
	}
	for _, q := range []string{
		`INSERT INTO commitments (person_id, direction, text, reporter, created_at) VALUES (?, 'mine', 'open old', 'r', '2025-01-01T00:00:00Z')`,
		`INSERT INTO commitments (person_id, direction, text, reporter, created_at, status, closed_at) VALUES (?, 'mine', 'closed old', 'r', '2025-01-01T00:00:00Z', 'done', '2026-01-01T00:00:00Z')`,
		`INSERT INTO commitments (person_id, direction, text, reporter, created_at, status, closed_at) VALUES (?, 'mine', 'closed new', 'r', '2025-01-01T00:00:00Z', 'dropped', '2026-03-01T00:00:00Z')`,
		`INSERT INTO access_log (time, caller, command, person_id) VALUES ('2026-01-01T00:00:00Z', 'ops', 'history', ?)`,
		`INSERT INTO access_log (time, caller, command, person_id) VALUES ('2026-03-01T00:00:00Z', 'ops', 'history', ?)`,
	} {
		if _, err := s.db.Exec(q, ada); err != nil {
			t.Fatal(err)
		}
	}
	return s, ada
}

func TestAgeCompacts(t *testing.T) {
	s, ada := ageFixture(t)
	r, err := s.Age(AgeOptions{RetentionDays: 30})
	if err != nil {
		t.Fatal(err)
	}
	want := AgeResult{CompactedEvents: 4, Summaries: 2, DeletedCommitments: 1, DeletedAccessRows: 1}
	if r != want {
		t.Errorf("result = %+v, want %+v", r, want)
	}
	es, err := s.History(HistoryQuery{PersonID: ada})
	if err != nil {
		t.Fatal(err)
	}
	type row struct{ date, channel, link, summary string }
	var got []row
	for _, e := range es {
		got = append(got, row{e.Date, e.Channel, e.Link, e.Summary})
	}
	wantRows := []row{
		{"2026-02-09", "chat", "https://x/5", "s https://x/5"},
		{"2026-02-01", "chat", "https://x/4", "1 events, channels: chat"},
		{"2026-01-25", "chat,email", "https://x/3", "3 events, channels: chat, email"},
	}
	if len(got) != len(wantRows) {
		t.Fatalf("events = %+v", got)
	}
	for i := range got {
		if got[i] != wantRows[i] {
			t.Errorf("event %d = %+v, want %+v", i, got[i], wantRows[i])
		}
	}
	if n := count(t, s, `SELECT count(*) FROM commitments WHERE status = 'open'`); n != 1 {
		t.Errorf("open commitments = %d, want 1 (never aged)", n)
	}

	again, err := s.Age(AgeOptions{RetentionDays: 30})
	if err != nil || again.Changed() {
		t.Errorf("second run = %+v, %v; want no change", again, err)
	}
	// A late report into an aged month folds into its summary.
	late := Event{PersonID: ada, Date: "2026-01-10", Channel: "phone", Link: "https://x/6", Summary: "late", Reporter: "ops"}
	if _, _, err := s.Report(late, DefaultSummaryMax); err != nil {
		t.Fatal(err)
	}
	if _, err := s.Age(AgeOptions{RetentionDays: 30}); err != nil {
		t.Fatal(err)
	}
	var summary string
	if err := s.db.QueryRow(`SELECT summary FROM events WHERE date LIKE '2026-01-%'`).Scan(&summary); err != nil {
		t.Fatal(err)
	}
	if summary != "4 events, channels: chat, email, phone" {
		t.Errorf("merged summary = %q", summary)
	}
}

func TestAgeDrop(t *testing.T) {
	s, _ := ageFixture(t)
	r, err := s.Age(AgeOptions{RetentionDays: 30, Drop: true})
	if err != nil {
		t.Fatal(err)
	}
	if r.DroppedEvents != 4 || r.Summaries != 0 {
		t.Errorf("result = %+v", r)
	}
	if n := count(t, s, `SELECT count(*) FROM events`); n != 1 {
		t.Errorf("events left = %d, want 1", n)
	}
}

func TestAgeDryRunChangesNothing(t *testing.T) {
	s, _ := ageFixture(t)
	snapshot := func() [4]int {
		return [4]int{
			count(t, s, `SELECT count(*) FROM events`),
			count(t, s, `SELECT count(*) FROM commitments`),
			count(t, s, `SELECT count(*) FROM access_log`),
			count(t, s, `SELECT coalesce(sum(compacted), 0) FROM events`),
		}
	}
	before := snapshot()
	r, err := s.Age(AgeOptions{RetentionDays: 30, DryRun: true})
	if err != nil {
		t.Fatal(err)
	}
	if !r.Changed() {
		t.Error("dry run reported nothing to do")
	}
	if after := snapshot(); after != before {
		t.Errorf("dry run changed the store: %v -> %v", before, after)
	}
}

func TestAgeLocked(t *testing.T) {
	s, _ := ageFixture(t)
	unlock, err := lockFile(filepath.Join(s.dir, "age.lock"))
	if err != nil {
		t.Fatal(err)
	}
	if _, err := s.Age(AgeOptions{RetentionDays: 30}); !errors.Is(err, ErrAgeLocked) {
		t.Errorf("Age while locked = %v, want ErrAgeLocked", err)
	}
	unlock()
	if _, err := s.Age(AgeOptions{RetentionDays: 30}); err != nil {
		t.Errorf("Age after unlock = %v", err)
	}
}
