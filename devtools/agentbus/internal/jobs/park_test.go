package jobs

import (
	"context"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// parkArgs parks job id held by r1 on decide item d9 with a saved ref.
func parkArgs(id string, items ...string) Args {
	return Args{ID: id, Runner: "r1", Decide: "d9", Saved: "refs/heads/park/" + id, Note: "half the comments answered", Tab: "tiger/w1/p2", Items: items}
}

func TestParkNeedsTheHolderADecideItemASavedRefAndANote(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#1", Head: "abc"})
	mustClaim(t, s, "r1")
	for name, a := range map[string]Args{
		"another runner": {ID: j.ID, Runner: "r2", Decide: "d9", Saved: "park/x", Note: "n"},
		"no decide item": {ID: j.ID, Runner: "r1", Saved: "park/x", Note: "n"},
		"no saved ref":   {ID: j.ID, Runner: "r1", Decide: "d9", Note: "n"},
		"a bad ref":      {ID: j.ID, Runner: "r1", Decide: "d9", Saved: "park x", Note: "n"},
		"a stash":        {ID: j.ID, Runner: "r1", Decide: "d9", Saved: "refs/stash", Note: "n"},
		"no resume note": {ID: j.ID, Runner: "r1", Decide: "d9", Saved: "park/x"},
	} {
		if _, err := s.Park(a); err == nil {
			t.Errorf("%s: park accepted", name)
		}
	}
	if got := show(t, s, j.ID); got.State != StateRunning {
		t.Fatalf("refused parks moved the job to %s", got.State)
	}
}

func TestParkMovesARunningJobToParkedAndFreesItsLease(t *testing.T) {
	s, _, f := leaseStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#1", Head: "abc", Needs: Needs{Checkout: "/src/repo"}})
	mustClaimOn(t, s, "r1", "tiger")
	r, err := s.Park(parkArgs(j.ID))
	if err != nil {
		t.Fatal(err)
	}
	got := r.Job
	if got.State != StateParked || got.ParkedOn != "d9" || got.Saved != "refs/heads/park/"+j.ID ||
		got.Resume != "half the comments answered" || got.Tab != "tiger/w1/p2" || got.Holder != "" || got.Lease != "" {
		t.Fatalf("parked = %+v", got)
	}
	if r.Child != nil {
		t.Fatalf("a whole park made a child: %+v", r.Child)
	}
	if f.holder("tiger:/src/repo") != "" {
		t.Fatal("the park kept the checkout lease")
	}
	if got.unsettled() {
		t.Fatal("a parked mid-run job counts as a lost one")
	}
}

func TestPartialParkFinishesTheJobAndGatesAChildWithTheParkedItems(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#1", Head: "abc", Items: []string{"c1", "c2", "c3"}, Priority: 3})
	mustClaim(t, s, "r1")
	r, err := s.Park(parkArgs(j.ID, "c2", "c3"))
	if err != nil {
		t.Fatal(err)
	}
	if r.Job.State != StateDone {
		t.Fatalf("parent = %s, want done for the handled part", r.Job.State)
	}
	c := r.Child
	if c == nil || c.State != StateGated || c.Parent != j.ID || !slices.Equal(c.Items, []string{"c2", "c3"}) ||
		c.ParkedOn != "d9" || c.Saved != "refs/heads/park/"+j.ID || c.Tab != "tiger/w1/p2" ||
		c.Kind != "review" || c.Subject != "o/r#1" || c.Priority != 3 || c.Dedupe != "review o/r#1 parked on d9" {
		t.Fatalf("child = %+v", c)
	}
	// The child waits on its own item: no gate item, and it does not
	// block the subject's other jobs.
	other := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	if got := mustClaim(t, s, "r1"); got.ID != other.ID {
		t.Fatalf("claimed %s, want %s", got.ID, other.ID)
	}
	if _, err := s.Done(Args{ID: other.ID, Runner: "r1"}); err != nil {
		t.Fatal(err)
	}
	listed := mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#3", Items: []string{"c1"}})
	mustClaim(t, s, "r1")
	if _, err := s.Park(parkArgs(listed.ID, "c9")); err == nil {
		t.Fatal("parked an item the job does not list")
	}
}

func TestMovedHeadDoesNotRequeueParkedItems(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#1", Head: "abc", Items: []string{"c1", "c2"}})
	mustClaim(t, s, "r1")
	r, err := s.Park(parkArgs(j.ID, "c2"))
	if err != nil {
		t.Fatal(err)
	}
	dup, err := s.Enqueue(Args{Kind: "review", Subject: "o/r#1", Head: "def", Items: []string{"c2"}})
	if err != nil || !dup.Duplicate || dup.Job.ID != r.Child.ID {
		t.Fatalf("enqueue of only parked items at a new head = %+v, %v; want the child as duplicate", dup, err)
	}
	fresh := mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#1", Head: "def", Items: []string{"c2", "c3"}})
	if !slices.Equal(fresh.Items, []string{"c3"}) {
		t.Fatalf("new job items = %v, want only c3", fresh.Items)
	}

	// A whole park holds the job's subject for its kind.
	whole := mustEnqueue(t, s, Args{Kind: "fix-ci", Subject: "o/r#2", Head: "abc"})
	if r, err := s.Claim(Args{Runner: "r1", Kinds: []string{"fix-ci"}}); err != nil || r.Job == nil {
		t.Fatalf("claim = %+v, %v", r, err)
	}
	if _, err := s.Park(parkArgs(whole.ID)); err != nil {
		t.Fatal(err)
	}
	dup, err = s.Enqueue(Args{Kind: "fix-ci", Subject: "o/r#2", Head: "def"})
	if err != nil || !dup.Duplicate || dup.Job.ID != whole.ID {
		t.Fatalf("enqueue at a new head of a parked job = %+v, %v; want the parked job", dup, err)
	}
}

// answer is the user's answer to an agent's question in the inbox.
func answer(d *fakeDecide, id, text, via, by string) {
	it := d.items[id]
	it.Messages = append(it.Messages, decide.Message{Author: decide.Human, Text: text, Via: via, By: by})
	it.Answer = &decide.Answer{Value: text, AnsweredBy: decide.Human, Via: via}
	it.State = decide.StateAnswered
}

func TestUserAnswerResumesParkedJobsAndChildren(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	d := newFakeDecide()
	item, _ := d.Ask(context.Background(), "which way?")
	whole := mustEnqueue(t, s, Args{Kind: "fix-ci", Subject: "o/r#2"})
	mustClaim(t, s, "r1")
	a := parkArgs(whole.ID)
	a.Decide = item
	if _, err := s.Park(a); err != nil {
		t.Fatal(err)
	}
	part := mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#1", Items: []string{"c1"}})
	mustClaim(t, s, "r1")
	a = parkArgs(part.ID, "c1")
	a.Decide = item
	r, err := s.Park(a)
	if err != nil {
		t.Fatal(err)
	}
	child := r.Child.ID

	RunGate(context.Background(), s, d)
	if len(d.asked) != 1 {
		t.Fatalf("asked %q; a job parked on an item needs no gate item", d.asked)
	}
	answer(d, item, "use the old API", decide.ViaUI, "worker")
	RunGate(context.Background(), s, d)
	if got := show(t, s, whole.ID); got.State != StateParked {
		t.Fatalf("an agent's answer resumed the job: %s", got.State)
	}
	answer(d, item, "use the old API", decide.ViaUI, "")
	RunGate(context.Background(), s, d)
	for _, id := range []string{whole.ID, child} {
		got := show(t, s, id)
		if got.State != StateReady || got.ParkedOn != "" || got.Saved == "" || !strings.Contains(got.Note, item) {
			t.Fatalf("%s after the answer = %+v, want ready keeping its saved ref", id, got)
		}
	}
	if _, ok := d.done[item]; ok {
		t.Fatal("closed the agent's item; the resumed agent reads it")
	}
}

func TestResumedJobWaitsItsTurnForTheCheckout(t *testing.T) {
	s, _, f := leaseStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#1", Needs: Needs{Checkout: "/src/repo"}})
	mustClaimOn(t, s, "r1", "tiger")
	if _, err := s.Park(parkArgs(j.ID)); err != nil {
		t.Fatal(err)
	}
	f.refuse["tiger:/src/repo"] = true
	if err := s.Resume(j.ID, "d9", "answered", ""); err != nil {
		t.Fatal(err)
	}
	if r, _ := s.Claim(Args{Runner: "r1", Machine: "tiger"}); r.Job != nil {
		t.Fatalf("claimed a resumed job while its checkout is held: %+v", r.Job)
	}
	if got := show(t, s, j.ID); got.State != StateReady {
		t.Fatalf("resumed job = %s, want ready", got.State)
	}
	delete(f.refuse, "tiger:/src/repo")
	if got := mustClaimOn(t, s, "r1", "tiger"); got.ID != j.ID || got.Lease != "tiger:/src/repo" {
		t.Fatalf("claim once free = %+v", got)
	}
	if err := s.Resume(j.ID, "d9", "again", ""); err == nil {
		t.Fatal("resumed a running job")
	}
}
