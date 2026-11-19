package jobs

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// fakeBus is the agent bus replies to each asked message.
type fakeBus struct{ replies map[string][]BusReply }

func (f *fakeBus) Replies(_ context.Context, msg string) ([]BusReply, error) {
	return f.replies[msg], nil
}

func (f *fakeBus) reply(msg, id, from string, addressed bool, text string) {
	f.replies[msg] = append(f.replies[msg], BusReply{ID: id, From: from, Addressed: addressed, Text: text})
}

// askArgs parks job id held by r1 on bus message m-1 sent to manager.
func askArgs(id string, items ...string) Args {
	return Args{ID: id, Runner: "r1", AskMsg: "m-1", AskTo: "manager", Saved: "park/" + id, Note: "design threads left", Items: items}
}

func askParked(t *testing.T, items ...string) (*Store, *clock, *memLog, Job) {
	t.Helper()
	s, c, lg := newStore(t, Config{})
	enq := Args{Kind: "comment", Subject: "o/r#1", Head: "abc"}
	if len(items) > 0 {
		enq.Items = append([]string{"c0"}, items...)
	}
	j := mustEnqueue(t, s, enq)
	mustClaim(t, s, "r1")
	r, err := s.Park(askArgs(j.ID, items...))
	if err != nil {
		t.Fatal(err)
	}
	if r.Child != nil {
		return s, c, lg, *r.Child
	}
	return s, c, lg, *r.Job
}

func TestParkOnABusMessageNeedsASavedRefAndOnlyOneTarget(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "comment", Subject: "o/r#1"})
	mustClaim(t, s, "r1")
	for name, a := range map[string]Args{
		"no saved ref":       {ID: j.ID, Runner: "r1", AskMsg: "m-1", AskTo: "manager", Note: "n"},
		"a stash":            {ID: j.ID, Runner: "r1", AskMsg: "m-1", AskTo: "manager", Saved: "refs/stash", Note: "n"},
		"no addressed name":  {ID: j.ID, Runner: "r1", AskMsg: "m-1", Saved: "park/x", Note: "n"},
		"a decide item, too": {ID: j.ID, Runner: "r1", AskMsg: "m-1", AskTo: "manager", Decide: "d9", Saved: "park/x", Note: "n"},
	} {
		if _, err := s.Park(a); err == nil {
			t.Errorf("%s: park accepted", name)
		}
	}
	r, err := s.Park(askArgs(j.ID))
	if err != nil {
		t.Fatal(err)
	}
	if got := r.Job; got.State != StateParked || got.AskMsg != "m-1" || got.AskTo != "manager" || got.Saved != "park/"+j.ID {
		t.Fatalf("parked = %+v", got)
	}
}

func TestReplyFromTheAddressedNameResumesWithItsText(t *testing.T) {
	for _, items := range [][]string{nil, {"c2"}} {
		s, _, _, j := askParked(t, items...)
		bus := &fakeBus{replies: map[string][]BusReply{}}
		RunAsk(context.Background(), s, bus)
		if got := show(t, s, j.ID); got.State == StateReady {
			t.Fatal("resumed with no reply")
		}
		bus.reply("m-1", "m-2", "manager", true, "use the old API")
		RunAsk(context.Background(), s, bus)
		got := show(t, s, j.ID)
		if got.State != StateReady || got.Note != "use the old API" || got.Answer != "use the old API" || got.Saved == "" {
			t.Fatalf("items %v: after the reply = %+v", items, got)
		}
		if last := got.History[len(got.History)-1]; last.Actor != "manager" {
			t.Fatalf("resume logged by %q, want the sender", last.Actor)
		}
	}
}

func TestReplyFromAnotherNameIsIgnoredAndLoggedOnce(t *testing.T) {
	s, _, lg, j := askParked(t)
	bus := &fakeBus{replies: map[string][]BusReply{}}
	bus.reply("m-1", "m-2", "intern", false, "just ship it")
	RunAsk(context.Background(), s, bus)
	RunAsk(context.Background(), s, bus)
	if got := show(t, s, j.ID); got.State != StateParked {
		t.Fatalf("another name's reply moved the job to %s", got.State)
	}
	if n := countEvents(lg, "m-2"); n != 1 {
		t.Fatalf("ignored reply logged %d times, want once", n)
	}
	if !strings.Contains(eventsText(lg), "intern") {
		t.Fatalf("ignored reply logged without its sender: %s", eventsText(lg))
	}
}

func TestRegatedJobWaitsForAVerifiedHumanAnswerOnly(t *testing.T) {
	s, _, lg, j := askParked(t, "c2")
	d := newFakeDecide()
	item, _ := d.Ask(context.Background(), "the manager cannot answer: which way?")
	if _, err := s.Regate(j.ID, ""); err == nil {
		t.Fatal("regate with no item accepted")
	}
	r, err := s.Regate(j.ID, item)
	if err != nil {
		t.Fatal(err)
	}
	if r.Job.ParkedOn != item || r.Job.ParkTarget() != "decision "+item {
		t.Fatalf("regated = %+v", r.Job)
	}
	if _, err := s.Regate(j.ID, "d7"); err == nil {
		t.Fatal("a second regate accepted")
	}

	bus := &fakeBus{replies: map[string][]BusReply{}}
	bus.reply("m-1", "m-2", "manager", true, "go ahead")
	RunAsk(context.Background(), s, bus)
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateGated {
		t.Fatalf("a bus reply after regate moved the job to %s", got.State)
	}
	if countEvents(lg, "m-2") != 1 {
		t.Fatalf("the ignored bus reply was not logged: %s", eventsText(lg))
	}
	if len(d.asked) != 1 {
		t.Fatalf("asked %q; a regated child needs no gate item", d.asked)
	}
	answer(d, item, "keep the old API", decide.ViaUI, "manager")
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateGated {
		t.Fatalf("an agent's answer resumed the regated job: %s", got.State)
	}
	answer(d, item, "keep the old API", decide.ViaUI, "")
	RunGate(context.Background(), s, d)
	got := show(t, s, j.ID)
	if got.State != StateReady || got.Answer != "keep the old API" {
		t.Fatalf("after the user's answer = %+v", got)
	}
}

func TestAskParkedChildGetsNoGateItem(t *testing.T) {
	s, _, _, j := askParked(t, "c2")
	d := newFakeDecide()
	RunGate(context.Background(), s, d)
	if len(d.asked) != 0 {
		t.Fatalf("asked %q for a child waiting on a bus reply", d.asked)
	}
	if got := show(t, s, j.ID); got.State != StateGated {
		t.Fatalf("child = %s", got.State)
	}
}

func TestParkTargetAndAge(t *testing.T) {
	s, c, _, j := askParked(t)
	c.t = c.t.Add(3 * time.Hour)
	got := show(t, s, j.ID)
	if got.ParkTarget() != "asked manager" {
		t.Fatalf("target = %q", got.ParkTarget())
	}
	if age := c.t.Sub(got.ParkedAt); age != 3*time.Hour {
		t.Fatalf("age = %s, want 3h", age)
	}
	if (Job{}).ParkTarget() != "" {
		t.Fatal("a job that is not parked has a target")
	}
}

func countEvents(lg *memLog, needle string) int {
	lg.mu.Lock()
	defer lg.mu.Unlock()
	n := 0
	for _, e := range lg.events {
		if strings.Contains(e.Detail+" "+e.Reason, needle) {
			n++
		}
	}
	return n
}

func eventsText(lg *memLog) string {
	lg.mu.Lock()
	defer lg.mu.Unlock()
	var b strings.Builder
	for _, e := range lg.events {
		b.WriteString(e.Actor + " " + e.Detail + " " + e.Reason + "\n")
	}
	return b.String()
}
