package daemon

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// logOnly switches the world's daemon to lease_takeover = "log".
func (w *checkinWorld) logOnly() *checkinWorld {
	w.srv.timing.Takeover = lease.TakeoverLog
	return w
}

// assertNoTakeover checks that alice still holds, bob still waits, and
// nobody was told about a takeover.
func (w *checkinWorld) assertNoTakeover(t *testing.T) {
	t.Helper()
	s := w.status(t)
	if s.State != lease.StateHeld || s.Holder == nil || s.Holder.Handle != ciAlice || len(s.Queue) != 1 || s.BackupRef != "" {
		t.Fatalf("status = %+v", s)
	}
	for _, text := range w.ad.Texts {
		if !strings.Contains(text, lease.CheckInText(ciKey)) {
			t.Fatalf("a message other than the check-in was posted: %q", text)
		}
	}
	if len(*w.notes) != 0 {
		t.Fatalf("notes = %q", *w.notes)
	}
	if n := len(w.events(t, eventlog.EventLeaseTakeover)); n != 0 {
		t.Fatalf("takeover events = %d", n)
	}
}

func TestLogOnlyIdleHolderIsNotTakenOverAndIsLoggedOnce(t *testing.T) {
	w := newCheckinWorld(t, true).logOnly()
	for _, m := range []int{0, 2, 5} {
		w.at(m, adapter.StateIdle)
	}
	w.assertNoTakeover(t)
	evs := w.events(t, eventlog.EventLeaseWouldTakeOver)
	if len(evs) != 1 {
		t.Fatalf("would-take-over events = %+v", evs)
	}
	if ev := evs[0]; ev.Actor != ciAlice || ev.Name != ciKey || !strings.Contains(ev.Reason, "idle and silent") || !strings.Contains(ev.Detail, ciBob) {
		t.Fatalf("would-take-over event = %+v", ev)
	}
	for _, m := range []int{7, 9, 11} {
		w.at(m, adapter.StateIdle)
	}
	w.assertNoTakeover(t)
	if n := len(w.events(t, eventlog.EventLeaseWouldTakeOver)); n != 1 {
		t.Fatalf("would-take-over events after three more intervals = %d", n)
	}
}

func TestLogOnlyGoneHolderIsNotTakenOverAndIsLoggedOnce(t *testing.T) {
	w := newCheckinWorld(t, true).logOnly()
	for _, m := range []int{-30, -28, -26, -24} {
		w.at(m, adapter.StateGone)
	}
	w.assertNoTakeover(t)
	evs := w.events(t, eventlog.EventLeaseWouldTakeOver)
	if len(evs) != 1 || evs[0].Reason != "pane gone" {
		t.Fatalf("would-take-over events = %+v", evs)
	}
}

func TestLogOnlyDoesNotCarryOutAPendingTakeover(t *testing.T) {
	w := newCheckinWorld(t, true)
	// A run with takeover on decided it and stopped before carrying it
	// out: mark it decided without taking over.
	if _, err := w.srv.leases.Observe(lease.Watch{Checkout: ciKey, Holder: ciAlice}, lease.PaneGone, w.srv.timing); err != nil {
		t.Fatal(err)
	}
	w.logOnly()
	for _, m := range []int{-30, -28, -26, -24} {
		w.at(m, adapter.StateGone)
	}
	w.assertNoTakeover(t)
	if n := len(w.events(t, eventlog.EventLeaseWouldTakeOver)); n != 1 {
		t.Fatalf("would-take-over events = %d", n)
	}
}

func TestLeaseStatusReportsTheTakeoverMode(t *testing.T) {
	for _, tt := range []struct {
		mode lease.TakeoverMode
		want string
	}{
		{"", "log"},
		{lease.TakeoverLog, "log"},
		{lease.TakeoverOn, "on"},
		{"maybe", "log"},
	} {
		dir := filepath.Join(t.TempDir(), "state")
		srv, err := newServer(context.Background(), dir, Config{LeaseTiming: lease.Timing{Takeover: tt.mode}}, eventlog.Open(dir))
		if err != nil {
			t.Fatal(err)
		}
		for _, all := range []bool{false, true} {
			resp := srv.lease(Request{Op: "lease-status", Lease: &lease.Args{Checkout: ciKey, All: all}})
			if !resp.OK || string(resp.Lease.Takeover) != tt.want {
				t.Errorf("mode %q all=%v: status = %+v, want takeover %q", tt.mode, all, resp, tt.want)
			}
		}
	}
}

func TestServeLogsTheTakeoverModeAtStart(t *testing.T) {
	for _, tt := range []struct {
		mode lease.TakeoverMode
		want string
	}{
		{"", "lease_takeover=log"},
		{lease.TakeoverOn, "lease_takeover=on"},
	} {
		dirs := paths.Dirs{
			SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
			StateDir:   filepath.Join(t.TempDir(), "state"),
		}
		stop := startWith(t, dirs, Config{LeaseTiming: lease.Timing{Takeover: tt.mode}})
		stop()
		lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: eventlog.EventStart})
		if err != nil {
			t.Fatal(err)
		}
		if len(lines) != 1 || !strings.Contains(lines[0], tt.want) {
			t.Fatalf("mode %q: start lines = %q, want %q", tt.mode, lines, tt.want)
		}
	}
}
