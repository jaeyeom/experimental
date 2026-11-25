package secretwatch

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestCheckCursorsFirstRunDedupeAndMasking(t *testing.T) {
	now := time.Date(2026, 11, 25, 12, 0, 0, 0, time.UTC)
	statePath := filepath.Join(t.TempDir(), "state.json")

	secretGH := "ghp_" + strings.Repeat("a1B2", 9)
	secretAWS := "AKIA" + "IOSFODNN7" + "EXMPL99"

	type journalFixture struct {
		entries []Entry
	}
	fixtures := map[JournalKind]*journalFixture{
		JournalUser:   {},
		JournalSystem: {},
	}
	var queries []Query
	stream := func(_ context.Context, q Query, emit func(Entry) error) (string, error) {
		queries = append(queries, q)
		fx := fixtures[q.Kind]
		afterFound := q.AfterCursor == ""
		var tail string
		for _, e := range fx.entries {
			if !afterFound {
				if e.Cursor == q.AfterCursor {
					afterFound = true
				}
				continue
			}
			tail = e.Cursor
			if err := emit(e); err != nil {
				return tail, err
			}
		}
		return tail, nil
	}

	var notifiedArgv [][]string
	var notifiedEnv [][]string
	cfg := Config{
		Notify: []string{"my-notify", "{rule}", "{journal}", "{unit}", "{process}", "{pid}", "{prefix}", "{cursor}", "{time}"},
	}

	runPass := func(at time.Time, since string) Report {
		st, err := LoadState(statePath)
		if err != nil {
			t.Fatal(err)
		}
		rep, next, err := Check(context.Background(), CheckInput{
			Config:  cfg,
			State:   st,
			Since:   since,
			Now:     at,
			BaseEnv: []string{"PATH=/usr/bin"},
			Stream:  stream,
			Notify: func(_ context.Context, argv, env []string) error {
				notifiedArgv = append(notifiedArgv, argv)
				notifiedEnv = append(notifiedEnv, env)
				return nil
			},
		})
		if err != nil {
			t.Fatal(err)
		}
		if err := SaveState(statePath, next); err != nil {
			t.Fatal(err)
		}
		return rep
	}

	// Run 1: first run starts from "now" when --since is omitted; historical entries
	// before now are not scanned (simulated by empty fixture on Run 1), and LastScan is saved.
	rep := runPass(now, "")
	if rep.ExitCode() != 0 || len(rep.Findings) != 0 {
		t.Fatalf("run 1 got %+v", rep)
	}
	if len(queries) != 2 || queries[0].Since != FormatJournalTime(now) || queries[0].AfterCursor != "" {
		t.Fatalf("run 1 queries = %+v, want Since=%q", queries, FormatJournalTime(now))
	}

	// Add new entries after Run 1.
	t1 := now.Add(1 * time.Minute)
	fixtures[JournalUser].entries = append(fixtures[JournalUser].entries,
		Entry{Cursor: "u1", Timestamp: t1, Unit: "deploy.service", Process: "curl", PID: "101", Message: "ok"},
		Entry{Cursor: "u2", Timestamp: t1.Add(time.Second), Unit: "deploy.service", Process: "curl", PID: "102", Message: "Authorization: Bearer " + secretGH},
	)
	fixtures[JournalSystem].entries = append(fixtures[JournalSystem].entries,
		Entry{Cursor: "s1", Timestamp: t1, Unit: "ansible.service", Process: "python3", PID: "202", Message: "AWS=" + secretAWS},
	)

	// Run 2: sees new entries, emits 2 findings, notifies once per finding, saves cursors.
	queries = nil
	rep = runPass(t1, "")
	if rep.ExitCode() != 1 || len(rep.Findings) != 2 {
		t.Fatalf("run 2 got exit=%d findings=%+v", rep.ExitCode(), rep.Findings)
	}
	if len(notifiedArgv) != 2 {
		t.Fatalf("notified count = %d, want 2", len(notifiedArgv))
	}

	// Run 3: uses AfterCursor u2 / s1 and sees only new lines (0 new findings).
	fixtures[JournalUser].entries = append(fixtures[JournalUser].entries,
		Entry{Cursor: "u3", Timestamp: t1.Add(2 * time.Minute), Unit: "deploy.service", Process: "curl", PID: "103", Message: "clean line"},
	)
	queries = nil
	rep = runPass(t1.Add(2*time.Minute), "")
	if rep.ExitCode() != 0 || len(rep.Findings) != 0 {
		t.Fatalf("run 3 got exit=%d findings=%+v", rep.ExitCode(), rep.Findings)
	}
	if queries[0].AfterCursor != "u2" || queries[1].AfterCursor != "s1" {
		t.Fatalf("run 3 cursors = (%q, %q), want (u2, s1)", queries[0].AfterCursor, queries[1].AfterCursor)
	}

	// Run 4: rerun overlapping u2 via --since does NOT re-alert (dedupe by cursor).
	rep = runPass(t1.Add(3*time.Minute), "1 hour ago")
	if rep.ExitCode() != 0 || len(rep.Findings) != 0 || len(notifiedArgv) != 2 {
		t.Fatalf("run 4 dedupe failed: exit=%d findings=%+v notified=%d", rep.ExitCode(), rep.Findings, len(notifiedArgv))
	}

	// Strict masking check across JSON report, human report, state file, and all notifier args/env.
	jsonOut, err := FormatReport(rep, true)
	if err != nil {
		t.Fatal(err)
	}
	stateBytes, err := os.ReadFile(statePath)
	if err != nil {
		t.Fatal(err)
	}
	haystacks := []string{jsonOut, string(stateBytes)}
	for _, argv := range notifiedArgv {
		haystacks = append(haystacks, strings.Join(argv, " "))
	}
	for _, env := range notifiedEnv {
		haystacks = append(haystacks, strings.Join(env, "\n"))
	}
	for _, h := range haystacks {
		assertMasked(t, h, secretGH)
		assertMasked(t, h, secretAWS)
	}
}

func TestCheckUnreadableSystemJournalSkippedWithNote(t *testing.T) {
	now := time.Date(2026, 11, 25, 12, 0, 0, 0, time.UTC)
	stream := func(_ context.Context, q Query, _ func(Entry) error) (string, error) {
		if q.Kind == JournalSystem {
			return "", fmt.Errorf("%w: No journal files were opened due to insufficient permissions", ErrJournalUnreadable)
		}
		return "u0", nil
	}

	rep, _, err := Check(context.Background(), CheckInput{
		Config: Config{},
		Now:    now,
		Stream: stream,
	})
	if err != nil {
		t.Fatal(err)
	}
	if rep.ExitCode() != 0 {
		t.Fatalf("ExitCode = %d, want 0 when system journal skipped", rep.ExitCode())
	}
	if len(rep.Journals) != 2 || !rep.Journals[1].Skipped || rep.Journals[1].Note == "" {
		t.Fatalf("expected skipped system journal with note, got %+v", rep.Journals)
	}
}

func TestCheckNotifyFailureRetriesAndExitsError(t *testing.T) {
	now := time.Date(2026, 11, 25, 12, 0, 0, 0, time.UTC)
	secret := "ghp_" + strings.Repeat("d4E5", 9)

	stream := func(_ context.Context, q Query, emit func(Entry) error) (string, error) {
		if q.Kind != JournalUser {
			return "", nil
		}
		if q.AfterCursor == "c1" {
			return "c1", nil
		}
		_ = emit(Entry{Cursor: "c1", Timestamp: now, Unit: "a.service", Message: secret})
		return "c1", nil
	}

	failOnce := true
	rep, st, err := Check(context.Background(), CheckInput{
		Config: Config{Notify: []string{"notify"}},
		Now:    now,
		Stream: stream,
		Notify: func(_ context.Context, _, _ []string) error {
			if failOnce {
				failOnce = false
				return errors.New("notify failed")
			}
			return nil
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	if rep.ExitCode() != 2 {
		t.Fatalf("ExitCode = %d, want 2 on notify error", rep.ExitCode())
	}
	if st.HasSeenCursor("c1") {
		t.Fatalf("cursor c1 must not be marked seen when notify failed")
	}
	if st.journal(JournalUser).Cursor != "" {
		t.Fatalf("cursor must not advance past failed notification, got %q", st.journal(JournalUser).Cursor)
	}

	// Second run retries notification and succeeds.
	rep2, st2, err := Check(context.Background(), CheckInput{
		Config: Config{Notify: []string{"notify"}},
		State:  st,
		Now:    now.Add(time.Minute),
		Stream: stream,
		Notify: func(_ context.Context, _, _ []string) error { return nil },
	})
	if err != nil {
		t.Fatal(err)
	}
	if rep2.ExitCode() != 1 || !st2.HasSeenCursor("c1") || st2.journal(JournalUser).Cursor != "c1" {
		t.Fatalf("retry failed: exit=%d st=%+v", rep2.ExitCode(), st2)
	}
}

func TestScanDoesNotModifyStateAndFiltersUnit(t *testing.T) {
	statePath := filepath.Join(t.TempDir(), "state.json")
	origState := State{Journals: map[JournalKind]JournalState{JournalUser: {Cursor: "orig-u"}}}
	if err := SaveState(statePath, origState); err != nil {
		t.Fatal(err)
	}
	beforeBytes, err := os.ReadFile(statePath)
	if err != nil {
		t.Fatal(err)
	}

	secret := "AIza" + "SyDa1b2C3d4E5f6G" + "7h8I9j0K1l2M3n4O5p6"
	var gotQueries []Query
	stream := func(_ context.Context, q Query, emit func(Entry) error) (string, error) {
		gotQueries = append(gotQueries, q)
		if q.Kind == JournalSystem {
			return "", fmt.Errorf("%w: permission denied", ErrJournalUnreadable)
		}
		_ = emit(Entry{Cursor: "c10", Timestamp: time.Date(2026, 11, 25, 11, 0, 0, 0, time.UTC), Unit: "other.service", Message: secret})
		_ = emit(Entry{Cursor: "c11", Timestamp: time.Date(2026, 11, 25, 11, 5, 0, 0, time.UTC), Unit: "target.service", Process: "app", PID: "77", Message: "key=" + secret})
		return "c11", nil
	}

	rep, err := Scan(context.Background(), ScanInput{
		Config: Config{},
		Since:  "2 hours ago",
		Until:  "1 hour ago",
		Unit:   "target.service",
		Stream: stream,
	})
	if err != nil {
		t.Fatal(err)
	}
	if rep.ExitCode() != 1 {
		t.Fatalf("ExitCode = %d, want 1", rep.ExitCode())
	}
	if len(rep.Findings) != 1 || rep.Findings[0].Cursor != "c11" || rep.Findings[0].Rule != "gcp-api-key" {
		t.Fatalf("unexpected findings: %+v", rep.Findings)
	}
	if gotQueries[0].Since != "2 hours ago" || gotQueries[0].Until != "1 hour ago" || gotQueries[0].Unit != "target.service" {
		t.Fatalf("unexpected query: %+v", gotQueries[0])
	}

	afterBytes, err := os.ReadFile(statePath)
	if err != nil {
		t.Fatal(err)
	}
	if string(beforeBytes) != string(afterBytes) {
		t.Fatalf("scan modified state file!\nbefore:\n%s\nafter:\n%s", beforeBytes, afterBytes)
	}

	human, err := FormatReport(rep, false)
	if err != nil {
		t.Fatal(err)
	}
	js, err := FormatReport(rep, true)
	if err != nil {
		t.Fatal(err)
	}
	assertMasked(t, human, secret)
	assertMasked(t, js, secret)
}

func TestFollowStreamsFindingsAndSkipsUnreadableSystem(t *testing.T) {
	now := time.Date(2026, 11, 25, 12, 0, 0, 0, time.UTC)
	secret := "xoxb-" + "123456789012-" + "AbCdEfGhIjKlMnOp"

	stream := func(_ context.Context, q Query, emit func(Entry) error) (string, error) {
		if !q.Follow {
			return "", errors.New("expected Follow=true")
		}
		if q.Kind == JournalSystem {
			return "", fmt.Errorf("%w: permission denied", ErrJournalUnreadable)
		}
		_ = emit(Entry{Cursor: "f1", Timestamp: now, Unit: "bot.service", Process: "bot", PID: "55", Message: secret})
		return "f1", nil
	}

	var gotFindings []Finding
	var gotNotes []string
	var saved []State
	rep, st, err := Follow(context.Background(), FollowInput{
		CheckInput: CheckInput{
			Config: Config{Notify: []string{"notify", "{rule}", "{prefix}"}},
			Now:    now,
			Stream: stream,
			Notify: func(_ context.Context, argv, _ []string) error {
				for _, a := range argv {
					assertMasked(t, a, secret)
				}
				return nil
			},
		},
		OnFinding: func(f Finding) { gotFindings = append(gotFindings, f) },
		OnNote:    func(_ JournalKind, note string) { gotNotes = append(gotNotes, note) },
		OnSave:    func(s State) error { saved = append(saved, s); return nil },
	})
	if err != nil {
		t.Fatal(err)
	}
	if rep.ExitCode() != 1 || len(gotFindings) != 1 || len(gotNotes) != 1 || len(saved) != 1 {
		t.Fatalf("follow got exit=%d findings=%d notes=%d saved=%d", rep.ExitCode(), len(gotFindings), len(gotNotes), len(saved))
	}
	if !st.HasSeenCursor("f1") || st.journal(JournalUser).Cursor != "f1" {
		t.Fatalf("unexpected follow state: %+v", st)
	}
}
