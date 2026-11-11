package decide

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"
)

// fakeLease answers `agentbus lease status` for one checkout.
type fakeLease struct {
	guarded string
	status  string
	err     error
	calls   [][]string
}

func (f *fakeLease) run(_ context.Context, args []string) ([]byte, error) {
	f.calls = append(f.calls, args)
	if f.err != nil {
		return nil, f.err
	}
	if slices.Contains(args, "--guarded") {
		return []byte(f.guarded), nil
	}
	return []byte(f.status), nil
}

const guardedRepo = `{"guarded": true, "checkout": "/repo"}`

var leaseCheckouts = []ConfigCheckout{{Name: "checkout", Path: "/repo"}}

func TestResourceViewReadsLease(t *testing.T) {
	deadline := time.Date(2026, 11, 11, 14, 20, 0, 0, time.Local)
	until := deadline.Format("15:04")
	tests := []struct {
		name   string
		status string
		avail  string
		line   string
	}{
		{"free", `{"checkout":"/repo","state":"free","queue":[]}`, ResourceFree, "checkout: free"},
		{"held", `{"checkout":"/repo","state":"held","holder":{"handle":"%7","name":"pm/foo","deadline":"` + deadline.Format(time.RFC3339) + `"},"queue":[]}`, ResourceBusy, "checkout: held by pm/foo until " + until},
		{"held by handle", `{"checkout":"/repo","state":"held","holder":{"handle":"%7","name":"","deadline":""},"queue":[]}`, ResourceBusy, "checkout: held by %7"},
		{"paused", `{"checkout":"/repo","state":"paused","holder":{"handle":"user","name":"","deadline":""},"queue":[]}`, ResourceBusy, "checkout: paused by user"},
		{"needs adopt", `{"checkout":"/repo","state":"needs-adopt","holder":{"handle":"%7","name":"pm/foo"},"queue":[],"backup_ref":"refs/agentbus/backup/1"}`, ResourceBusy, "checkout: needs adopt (backup refs/agentbus/backup/1)"},
		{"queue of 2", `{"checkout":"/repo","state":"held","holder":{"handle":"%7","name":"pm/foo"},"queue":[{"handle":"%8"},{"handle":"%9"}]}`, ResourceBusy, "checkout: held by pm/foo, 2 queued"},
		{"free with queue", `{"checkout":"/repo","state":"free","queue":[{"handle":"%8"},{"handle":"%9"}]}`, ResourceFree, "checkout: free, 2 queued"},
	}
	now := time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bus := &fakeLease{guarded: guardedRepo, status: tt.status}
			stored := []ResourceStatus{{Name: "checkout", Availability: ResourceFree, Note: "stale guess"}}
			rows := ResourceView(context.Background(), stored, leaseCheckouts, bus.run, now)
			if len(rows) != 1 {
				t.Fatalf("rows = %+v", rows)
			}
			row := rows[0]
			if row.Source != SourceLease || row.Guess || row.Availability != tt.avail || !row.UpdatedAt.Equal(now) {
				t.Fatalf("row = %+v", row)
			}
			if got := ResourceLine(row); got != tt.line {
				t.Fatalf("line = %q, want %q", got, tt.line)
			}
		})
	}
}

func TestResourceViewLeaseRowWithoutStoredRow(t *testing.T) {
	bus := &fakeLease{guarded: guardedRepo, status: `{"checkout":"/repo","state":"free","queue":[]}`}
	stored := []ResourceStatus{{Name: "zeta", Availability: ResourceBusy}}
	rows := ResourceView(context.Background(), stored, leaseCheckouts, bus.run, time.Now())
	if len(rows) != 2 || rows[0].Name != "checkout" || rows[0].Source != SourceLease || rows[1].Name != "zeta" || rows[1].Source != SourceStored {
		t.Fatalf("rows = %+v", rows)
	}
	want := [][]string{
		{"lease", "status", "--guarded", "--json", "--path", "/repo"},
		{"lease", "status", "--json", "--path", "/repo"},
	}
	if !slices.EqualFunc(bus.calls, want, slices.Equal[[]string]) {
		t.Fatalf("calls = %q", bus.calls)
	}
}

func TestResourceViewUnguardedShowsStoredRow(t *testing.T) {
	bus := &fakeLease{guarded: `{"guarded": false}`}
	stored := []ResourceStatus{{Name: "checkout", Availability: ResourceBusy, Note: "mine"}}
	rows := ResourceView(context.Background(), stored, leaseCheckouts, bus.run, time.Now())
	if len(rows) != 1 || rows[0].Source != SourceStored || rows[0].Guess {
		t.Fatalf("rows = %+v", rows)
	}
	if got := ResourceLine(rows[0]); got != "checkout: busy (mine)" {
		t.Fatalf("line = %q", got)
	}
	if len(bus.calls) != 1 {
		t.Fatalf("unguarded path called the daemon: %q", bus.calls)
	}
}

func TestResourceViewAgentbusMissingMarksGuess(t *testing.T) {
	bus := &fakeLease{err: errors.New("agentbus: executable file not found in $PATH")}
	stored := []ResourceStatus{
		{Name: "checkout", Availability: ResourceBusy, Note: "mine"},
		{Name: "other", Availability: ResourceFree},
	}
	rows := ResourceView(context.Background(), stored, leaseCheckouts, bus.run, time.Now())
	if len(rows) != 2 || rows[0].Source != SourceStored || !rows[0].Guess || rows[1].Guess {
		t.Fatalf("rows = %+v", rows)
	}
	if got := ResourceLine(rows[0]); got != "checkout: busy (mine) (guess)" {
		t.Fatalf("line = %q", got)
	}
	if got := ResourceNote(rows[0]); got != "mine (guess)" {
		t.Fatalf("note = %q", got)
	}
	if got := ResourceLine(rows[1]); got != "other: free" {
		t.Fatalf("line = %q", got)
	}
}

func TestResourceViewUnreachableDaemonMarksGuess(t *testing.T) {
	bus := &fakeLease{guarded: guardedRepo, status: "not json"}
	stored := []ResourceStatus{{Name: "checkout", Availability: ResourceFree}}
	rows := ResourceView(context.Background(), stored, leaseCheckouts, bus.run, time.Now())
	if len(rows) != 1 || !rows[0].Guess || ResourceLine(rows[0]) != "checkout: free (guess)" {
		t.Fatalf("rows = %+v", rows)
	}
}
