package daemon

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

const ciCarol = "herdr:local/default/w1:p3"

func (w *checkinWorld) status(t *testing.T) lease.Status {
	t.Helper()
	r, err := w.srv.leases.Status(lease.Args{Checkout: ciKey})
	if err != nil {
		t.Fatal(err)
	}
	return r.Leases[0]
}

func (w *checkinWorld) textsWith(prefix string) []string {
	var out []string
	for _, text := range w.ad.Texts {
		if strings.Contains(text, prefix) {
			out = append(out, text)
		}
	}
	return out
}

func TestTakeOverMessagesTheOldHolderWakesTheAdopterAndNotesTheUser(t *testing.T) {
	w := newCheckinWorld(t, true)
	// alice stays idle and silent past the grace, so its pane still gets
	// the message.
	for _, m := range []int{0, 2, 5} {
		w.at(m, adapter.StateIdle)
	}

	s := w.status(t)
	if s.State != lease.StateNeedsAdopt || s.Holder == nil || s.Holder.Handle != ciBob || !strings.HasPrefix(s.BackupRef, lease.BackupRefPrefix) {
		t.Fatalf("status after takeover = %+v", s)
	}
	if got := w.textsWith("[lease] taken over from you"); len(got) != 1 || !strings.Contains(got[0], lease.TakenOverText(ciKey, s.BackupRef)) {
		t.Fatalf("old holder messages = %q (all %q)", got, w.ad.Texts)
	}
	wake := w.textsWith("[lease] granted ")
	if len(wake) != 1 || !strings.Contains(wake[0], lease.AdoptWakeText(ciKey, ciAlice, s.BackupRef)) {
		t.Fatalf("adopter wake = %q", wake)
	}
	if len(*w.notes) != 1 || !strings.Contains((*w.notes)[0], ciKey) || !strings.Contains((*w.notes)[0], "needs adopt") {
		t.Fatalf("notes = %q", *w.notes)
	}
	// A second check does not take over again.
	w.at(7, adapter.StateIdle)
	if n := len(w.events(t, eventlog.EventLeaseTakeover)); n != 1 {
		t.Fatalf("takeovers = %d", n)
	}
}

func TestTakeOverWithoutANotifierSendsNoNote(t *testing.T) {
	w := newCheckinWorld(t, true)
	w.srv.notify = nil
	w.at(-30, adapter.StateGone)
	if s := w.status(t); s.State != lease.StateNeedsAdopt {
		t.Fatalf("state = %s", s.State)
	}
}

func TestCheckLeasesTakesOverAnIdleReleasingHolderAfterTheGrace(t *testing.T) {
	w := newCheckinWorld(t, true)
	if _, err := w.srv.leases.BeginRelease(ciAlice, lease.Args{Checkout: ciKey}); err != nil {
		t.Fatal(err)
	}
	for _, m := range []int{0, 2, 4} {
		w.at(m, adapter.StateIdle)
	}
	if n := len(w.events(t, eventlog.EventLeaseTakeover)); n != 0 {
		t.Fatalf("takeover within the grace: %d", n)
	}
	if w.checkIns() != 1 {
		t.Fatalf("check-ins = %d", w.checkIns())
	}
	w.at(5, adapter.StateIdle)
	if s := w.status(t); s.State != lease.StateNeedsAdopt || s.Holder.Handle != ciBob {
		t.Fatalf("status = %+v", s)
	}
}

func TestCheckLeasesTakesOverAGoneReleasingHolderAtOnce(t *testing.T) {
	w := newCheckinWorld(t, true)
	if _, err := w.srv.leases.BeginRelease(ciAlice, lease.Args{Checkout: ciKey}); err != nil {
		t.Fatal(err)
	}
	w.at(-30, adapter.StateGone)
	if s := w.status(t); s.State != lease.StateNeedsAdopt || s.Holder.Handle != ciBob {
		t.Fatalf("status = %+v", s)
	}
}

func TestAGoneAdopterPassesTheLeaseToTheNextWaiter(t *testing.T) {
	w := newCheckinWorld(t, true)
	w.ad.Reach[ciCarol] = adapter.StateWorking
	if _, err := w.srv.leases.Claim(ciCarol, lease.Args{Checkout: ciKey, For: "10m"}); err != nil {
		t.Fatal(err)
	}
	w.at(-30, adapter.StateGone)
	first := w.status(t)
	if first.Holder.Handle != ciBob {
		t.Fatalf("first adopter = %+v", first.Holder)
	}
	// bob's pane goes gone before it adopts.
	delete(w.ad.Reach, ciBob)
	w.srv.checkLeases()
	s := w.status(t)
	if s.State != lease.StateNeedsAdopt || s.Holder == nil || s.Holder.Handle != ciCarol || s.BackupRef != first.BackupRef {
		t.Fatalf("status after the adopter left = %+v", s)
	}
}

func TestTakeOverWithOnlyGoneWaitersLeavesItUnassigned(t *testing.T) {
	w := newCheckinWorld(t, true)
	delete(w.ad.Reach, ciBob)
	w.at(-30, adapter.StateGone)
	s := w.status(t)
	if s.State != lease.StateNeedsAdopt || s.Holder != nil || len(s.Queue) != 0 {
		t.Fatalf("status = %+v", s)
	}
	if len(*w.notes) != 1 || !strings.Contains((*w.notes)[0], "nobody is waiting") {
		t.Fatalf("notes = %q", *w.notes)
	}
}
