package daemon

import (
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// ciUser is a herdr pane that runs no agent: the user's terminal.
const ciUser = "herdr:local/default/w1:p5"

// withPanes lets the world's callers identify by HERDR_PANE_ID: p1 is
// alice, p2 bob, p3 carol, p5 the user's plain terminal.
func (w *checkinWorld) withPanes() *checkinWorld {
	w.ad.PaneHandles = map[string]string{"p1": ciAlice, "p2": ciBob, "p3": ciCarol, "p5": ciUser}
	w.ad.Reach[ciCarol] = adapter.StateWorking
	w.ad.Result[ciCarol] = adapter.ResultAccepted
	w.ad.Reach[ciUser] = adapter.StateNoAgent
	return w
}

func (w *checkinWorld) call(op, pane string, args lease.Args) Response {
	args.Checkout = ciKey
	var env []string
	if pane != "" {
		env = []string{"HERDR_PANE_ID=" + pane}
	}
	return w.srv.dispatch(Request{Op: op, Env: env, Lease: &args}, 0)
}

func TestPauseIsRefusedFromAnAgentPane(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	for _, op := range []string{"lease-pause", "lease-unshare", "lease-resume-begin", "lease-resume-abort", "lease-resume"} {
		resp := w.call(op, "p2", lease.Args{With: []string{"p3"}})
		if resp.OK || !strings.Contains(resp.Error, "runs an agent") {
			t.Errorf("%s from an agent pane = %+v", op, resp)
		}
	}
	if s := w.status(t); len(s.Queue) != 0 || s.Holder.Handle != ciAlice {
		t.Fatalf("status = %+v", s)
	}
}

func TestPauseFromAPlainPaneOrOutsideHerdrIsTheUser(t *testing.T) {
	for _, pane := range []string{"p5", ""} {
		w := newCheckinWorld(t, false).withPanes()
		if _, err := w.srv.leases.Release(ciAlice, lease.Args{Checkout: ciKey}, func(string) bool { return false }); err != nil {
			t.Fatal(err)
		}
		resp := w.call("lease-pause", pane, lease.Args{})
		if !resp.OK || resp.Lease.Result != lease.ResultPaused || resp.Lease.Caller != lease.UserHolder {
			t.Fatalf("pause from %q = %+v", pane, resp)
		}
		if s := w.status(t); s.State != lease.StatePaused || s.Holder.Handle != lease.UserHolder {
			t.Fatalf("status = %+v", s)
		}
	}
}

func TestPauseWhileAnAgentHoldsAsksItToRelease(t *testing.T) {
	w := newCheckinWorld(t, true).withPanes()
	resp := w.call("lease-pause", "p5", lease.Args{})
	if !resp.OK || resp.Lease.Result != lease.ResultQueued || resp.Lease.Position != 1 || resp.Lease.Asked != ciAlice {
		t.Fatalf("pause = %+v", resp)
	}
	if got := w.textsWith(lease.PauseAskText(ciKey)); len(got) != 1 {
		t.Fatalf("asks = %q (all %q)", got, w.ad.Texts)
	}
	if s := w.status(t); s.Queue[0].Handle != lease.UserHolder || s.Queue[1].Handle != ciBob {
		t.Fatalf("queue = %+v", s.Queue)
	}
	// Pausing again does not ask again.
	w.call("lease-pause", "p5", lease.Args{})
	if got := w.textsWith(lease.PauseAskText(ciKey)); len(got) != 1 {
		t.Fatalf("asks after a second pause = %q", got)
	}
}

func TestPauseWithResolvesPanesNamesAndHandles(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	if _, err := w.srv.leases.Release(ciAlice, lease.Args{Checkout: ciKey}, func(string) bool { return false }); err != nil {
		t.Fatal(err)
	}
	if _, err := w.srv.reg.Register(w.srv.ctx, []string{"HERDR_PANE_ID=p3"}, "ops/carol", "carol works on docs"); err != nil {
		t.Fatal(err)
	}
	resp := w.call("lease-pause", "p5", lease.Args{With: []string{"p2", "ops/carol", ciAlice}})
	if !resp.OK {
		t.Fatalf("pause --with = %+v", resp)
	}
	if got := resp.Lease.Lease.With; !slices.Equal(got, []string{ciBob, ciCarol, ciAlice}) {
		t.Fatalf("with = %v", got)
	}
	if r := w.call("lease-check", "p2", lease.Args{}); r.Lease.Result != lease.ResultAllowed {
		t.Fatalf("check by p2 = %+v", r)
	}
	if r := w.call("lease-release-begin", "p2", lease.Args{}); r.OK || !strings.Contains(r.Error, "only the user") {
		t.Fatalf("release by p2 = %+v", r)
	}
	if r := w.call("lease-unshare", "p5", lease.Args{With: []string{"p2"}}); !r.OK {
		t.Fatalf("unshare p2 = %+v", r)
	}
	if r := w.call("lease-check", "p2", lease.Args{}); r.Lease.Result != lease.ResultDenied {
		t.Fatalf("check by p2 after unshare = %+v", r)
	}
	if r := w.call("lease-pause", "p5", lease.Args{With: []string{"nobody"}}); r.OK || !strings.Contains(r.Error, "nobody") {
		t.Fatalf("pause --with an unknown pane = %+v", r)
	}
}

func TestReleaseToAQueuedPauseNotesTheUser(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	w.call("lease-pause", "p5", lease.Args{})
	before := len(w.ad.Texts)
	resp := w.call("lease-release", "p1", lease.Args{})
	if !resp.OK || resp.Lease.Granted != lease.UserHolder {
		t.Fatalf("release = %+v", resp)
	}
	if len(*w.notes) != 1 || (*w.notes)[0] != lease.PausedNote(ciKey) {
		t.Fatalf("notes = %q", *w.notes)
	}
	if len(w.ad.Texts) != before {
		t.Fatalf("a message was posted: %q", w.ad.Texts[before:])
	}
}

func TestTakeOverForAQueuedPauseNotesTheUserAndWakesNoPane(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	w.call("lease-pause", "p5", lease.Args{})
	for _, m := range []int{-40, -38, -35} {
		w.at(m, adapter.StateIdle)
	}
	s := w.status(t)
	if s.State != lease.StateNeedsAdopt || s.Holder == nil || s.Holder.Handle != lease.UserHolder {
		t.Fatalf("status = %+v", s)
	}
	if got := w.textsWith("[lease] granted"); len(got) != 0 {
		t.Fatalf("wakes = %q", got)
	}
	if len(*w.notes) != 1 || !strings.Contains((*w.notes)[0], "needs adopt") {
		t.Fatalf("notes = %q", *w.notes)
	}
}

func TestPausedHoldIsNeverCheckedInOnOrTakenOver(t *testing.T) {
	for _, mode := range []lease.TakeoverMode{lease.TakeoverLog, lease.TakeoverOn} {
		t.Run(string(mode), func(t *testing.T) {
			w := newCheckinWorld(t, false).withPanes()
			w.srv.timing.Takeover = mode
			if _, err := w.srv.leases.Release(ciAlice, lease.Args{Checkout: ciKey}, func(string) bool { return false }); err != nil {
				t.Fatal(err)
			}
			w.call("lease-pause", "p5", lease.Args{With: []string{"p3"}})
			w.call("lease-claim", "p2", lease.Args{For: "5m"})
			before := len(w.ad.Texts)
			for m := 0; m <= 60; m += 2 {
				w.clock.Set(ciT0().Add(time.Duration(m) * time.Minute))
				w.srv.checkLeases()
			}
			if s := w.status(t); s.State != lease.StatePaused || s.Holder.Handle != lease.UserHolder {
				t.Fatalf("status after an hour = %+v", s)
			}
			if len(w.ad.Texts) != before || len(*w.notes) != 0 {
				t.Fatalf("messages %q, notes %q", w.ad.Texts[before:], *w.notes)
			}
			for _, ev := range []string{eventlog.EventLeaseCheckIn, eventlog.EventLeaseTakeover, eventlog.EventLeaseWouldTakeOver} {
				if n := len(w.events(t, ev)); n != 0 {
					t.Fatalf("%s events = %d", ev, n)
				}
			}
		})
	}
}
