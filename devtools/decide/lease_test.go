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
	all     string
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
	if slices.Contains(args, "--all") {
		return []byte(f.all), nil
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

// allLeases is `agentbus lease status --all --json` on the hub: mac
// holds /repo, and the hub's own /repo is free.
const allLeases = `[
  {"checkout":"local:/repo","state":"free","queue":[],"takeover":"log"},
  {"checkout":"mac:/repo","state":"held","holder":{"handle":"%7","name":"pm/foo","since":"","deadline":"","why":""},"queue":[{"handle":"%8","name":"","urgent":false,"for":"","since":""}],"takeover":"log"}
]`

func TestResourceViewMachineReadsAllLeases(t *testing.T) {
	bus := &fakeLease{all: allLeases}
	stored := []ResourceStatus{{Name: "checkout", Availability: ResourceFree, Note: "stale guess"}}
	checkouts := []ConfigCheckout{{Name: "checkout", Machine: "mac", Path: "/repo"}}
	now := time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)
	rows := ResourceView(context.Background(), stored, checkouts, bus.run, now)
	if len(rows) != 1 || rows[0].Source != SourceLease || rows[0].Guess {
		t.Fatalf("rows = %+v", rows)
	}
	if got := ResourceLine(rows[0]); got != "checkout: held by pm/foo, 1 queued" {
		t.Fatalf("line = %q", got)
	}
	want := [][]string{{"lease", "status", "--all", "--json"}}
	if !slices.EqualFunc(bus.calls, want, slices.Equal[[]string]) {
		t.Fatalf("calls = %q", bus.calls)
	}
}

func TestResourceViewMachineReadsAllLeasesOnce(t *testing.T) {
	bus := &fakeLease{all: allLeases}
	checkouts := []ConfigCheckout{
		{Name: "mac", Machine: "mac", Path: "/repo"},
		{Name: "hub", Machine: "local", Path: "/repo"},
	}
	rows := ResourceView(context.Background(), nil, checkouts, bus.run, time.Now())
	if len(rows) != 2 || ResourceLine(rows[0]) != "hub: free" || ResourceLine(rows[1]) != "mac: held by pm/foo, 1 queued" {
		t.Fatalf("rows = %+v", rows)
	}
	if len(bus.calls) != 1 {
		t.Fatalf("calls = %q", bus.calls)
	}
}

func TestResourceViewMachineNoLeaseShowsStoredGuess(t *testing.T) {
	tests := []struct {
		name    string
		machine string
		path    string
	}{
		{"other machine holds the path", "linux", "/repo"},
		{"no lease for the key", "mac", "/other"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bus := &fakeLease{all: allLeases}
			stored := []ResourceStatus{{Name: "checkout", Availability: ResourceBusy, Note: "mine"}}
			checkouts := []ConfigCheckout{{Name: "checkout", Machine: tt.machine, Path: tt.path}}
			rows := ResourceView(context.Background(), stored, checkouts, bus.run, time.Now())
			if len(rows) != 1 || rows[0].Source != SourceStored || !rows[0].Guess {
				t.Fatalf("rows = %+v", rows)
			}
			if got := ResourceLine(rows[0]); got != "checkout: busy (mine) (guess)" {
				t.Fatalf("line = %q", got)
			}
		})
	}
}

func TestResourceViewMachineNoLeaseNoStoredRow(t *testing.T) {
	bus := &fakeLease{all: allLeases}
	checkouts := []ConfigCheckout{{Name: "checkout", Machine: "linux", Path: "/repo"}}
	if rows := ResourceView(context.Background(), nil, checkouts, bus.run, time.Now()); len(rows) != 0 {
		t.Fatalf("rows = %+v", rows)
	}
}

func TestResourceViewMachineAgentbusUnreachableMarksGuess(t *testing.T) {
	bus := &fakeLease{err: errors.New("agentbus: daemon not running")}
	stored := []ResourceStatus{{Name: "checkout", Availability: ResourceFree}}
	checkouts := []ConfigCheckout{{Name: "checkout", Machine: "mac", Path: "/repo"}}
	rows := ResourceView(context.Background(), stored, checkouts, bus.run, time.Now())
	if len(rows) != 1 || !rows[0].Guess || ResourceLine(rows[0]) != "checkout: free (guess)" {
		t.Fatalf("rows = %+v", rows)
	}
	if len(bus.calls) != 1 {
		t.Fatalf("calls = %q", bus.calls)
	}
}

func TestResourceViewMixedMachineAndLocal(t *testing.T) {
	bus := &fakeLease{guarded: guardedRepo, status: `{"checkout":"local:/repo","state":"free","queue":[]}`, all: allLeases}
	checkouts := []ConfigCheckout{
		{Name: "here", Path: "/repo"},
		{Name: "there", Machine: "mac", Path: "/repo"},
	}
	rows := ResourceView(context.Background(), nil, checkouts, bus.run, time.Now())
	if len(rows) != 2 || ResourceLine(rows[0]) != "here: free" || ResourceLine(rows[1]) != "there: held by pm/foo, 1 queued" {
		t.Fatalf("rows = %+v", rows)
	}
	want := [][]string{
		{"lease", "status", "--guarded", "--json", "--path", "/repo"},
		{"lease", "status", "--json", "--path", "/repo"},
		{"lease", "status", "--all", "--json"},
	}
	if !slices.EqualFunc(bus.calls, want, slices.Equal[[]string]) {
		t.Fatalf("calls = %q", bus.calls)
	}
}
