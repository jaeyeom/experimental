package jobs

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// fakeDecide keeps items as decide does: an ask's first message carries
// its options, an answer is a human message plus the item's answer.
type fakeDecide struct {
	items map[string]*decide.Item
	asked []string
	done  map[string]string
	seq   int
}

func newFakeDecide() *fakeDecide {
	return &fakeDecide{items: map[string]*decide.Item{}, done: map[string]string{}}
}

var gateOptions = []decide.Option{{Key: OptionApprove, Label: "Run it"}, {Key: OptionReject, Label: "Reject"}}

func (f *fakeDecide) Ask(_ context.Context, question string) (string, error) {
	f.seq++
	id := fmt.Sprintf("d%d", f.seq)
	f.asked = append(f.asked, question)
	f.items[id] = &decide.Item{ID: id, State: decide.StateOpen, Owner: "agentbus", Messages: []decide.Message{
		{Author: "agentbus", Text: question, Options: gateOptions},
	}}
	return id, nil
}

func (f *fakeDecide) Show(_ context.Context, id string) (decide.Item, error) {
	it, ok := f.items[id]
	if !ok {
		return decide.Item{}, errors.New("no item")
	}
	return *it, nil
}

func (f *fakeDecide) Done(_ context.Context, id, outcome string) error {
	f.done[id] = outcome
	f.items[id].State = decide.StateDone
	return nil
}

// pick is an option answer: from the inbox (via ui), the command line
// (via cli), or an agent writing as the user (by set).
func (f *fakeDecide) pick(id, key, via, by string) {
	it := f.items[id]
	it.Messages = append(it.Messages, decide.Message{Author: decide.Human, Text: key, OptionKey: key, Via: via, By: by})
	it.Answer = &decide.Answer{Value: key, Key: key, AnsweredBy: decide.Human, Via: via}
	it.State = decide.StateAnswered
}

func (f *fakeDecide) note(id, text string) {
	it := f.items[id]
	it.Messages = append(it.Messages, decide.Message{Author: decide.Human, Text: text, Via: decide.ViaUI})
}

// question is a follow-up an agent asks on the item with decide reply.
func (f *fakeDecide) question(id string, keys ...string) {
	it := f.items[id]
	var opts []decide.Option
	for _, k := range keys {
		opts = append(opts, decide.Option{Key: k, Label: k})
	}
	it.Messages = append(it.Messages, decide.Message{Author: "agentbus", By: "worker", Text: "and?", Options: opts})
	it.State = decide.StateOpen
}

func askStore(t *testing.T) (*Store, *fakeDecide, Job) {
	t.Helper()
	s, _, _ := newStore(t, Config{})
	s.SetPolicy(Policy{Default: GateAsk})
	j := mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#1", Head: "abc"})
	d := newFakeDecide()
	RunGate(context.Background(), s, d)
	return s, d, j
}

func TestAskGateRaisesOneDecideItemAndStaysGated(t *testing.T) {
	s, d, j := askStore(t)
	if j.State != StateGated {
		t.Fatalf("enqueued state = %s, want gated", j.State)
	}
	RunGate(context.Background(), s, d)
	if len(d.asked) != 1 || !strings.Contains(d.asked[0], j.ID) || !strings.Contains(d.asked[0], "merge o/r#1") {
		t.Fatalf("asked = %q, want one item naming the job", d.asked)
	}
	got := show(t, s, j.ID)
	if got.State != StateGated || got.Decide != "d1" {
		t.Fatalf("job = %s decide %q, want gated d1", got.State, got.Decide)
	}
	if r, _ := s.Claim(Args{Runner: "r"}); r.Job != nil {
		t.Fatalf("claimed a gated job: %+v", r.Job)
	}
}

func TestAskGateHumanAnswerMakesTheJobReady(t *testing.T) {
	s, d, j := askStore(t)
	d.pick("d1", OptionApprove, decide.ViaUI, "")
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateReady {
		t.Fatalf("state = %s, want ready", got.State)
	}
	if !strings.HasPrefix(d.done["d1"], "approved") {
		t.Fatalf("decide done = %q, want the item closed as approved", d.done["d1"])
	}
	if c := mustClaim(t, s, "r"); c.ID != j.ID {
		t.Fatalf("claimed %s, want %s", c.ID, j.ID)
	}
}

func TestAskGateIgnoresAgentAnswers(t *testing.T) {
	for _, tt := range []struct{ name, via, by string }{
		{"agent as the user", decide.ViaUI, "worker"},
		{"command line", decide.ViaCLI, ""},
	} {
		s, d, j := askStore(t)
		d.pick("d1", OptionApprove, tt.via, tt.by)
		RunGate(context.Background(), s, d)
		if got := show(t, s, j.ID); got.State != StateGated {
			t.Errorf("%s: state = %s, want gated", tt.name, got.State)
		}
		if _, ok := d.done["d1"]; ok {
			t.Errorf("%s: decide item closed", tt.name)
		}
	}
}

func TestAskGateIgnoresAMessageWithoutAHumanUIAnswer(t *testing.T) {
	s, d, j := askStore(t)
	d.pick("d1", OptionApprove, decide.ViaUI, "")
	// The item's answer is not the user's own inbox answer.
	d.items["d1"].Answer.Via = decide.ViaCLI
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateGated {
		t.Fatalf("state = %s, want gated", got.State)
	}
}

func TestAskGateMultiQuestionNeedsEveryAnswer(t *testing.T) {
	s, d, j := askStore(t)
	d.question("d1", "x", "y")
	d.pick("d1", "x", decide.ViaUI, "")
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateGated {
		t.Fatalf("partial answer: state = %s, want gated", got.State)
	}
	// Answer the first question too: still each question needs its own
	// answer after it, so an answer to the follow-up does not count.
	d.pick("d1", OptionApprove, decide.ViaUI, "")
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateGated {
		t.Fatalf("first question unanswered before the follow-up: state = %s, want gated", got.State)
	}

	s, d, j = askStore(t)
	d.pick("d1", OptionApprove, decide.ViaUI, "")
	d.question("d1", "x", "y")
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateGated {
		t.Fatalf("follow-up unanswered: state = %s, want gated", got.State)
	}
	d.pick("d1", "y", decide.ViaUI, "")
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateReady {
		t.Fatalf("every question answered: state = %s, want ready", got.State)
	}
}

func TestAskGateRejectFailsTheJobWithTheNote(t *testing.T) {
	s, d, j := askStore(t)
	d.note("d1", "not during the release")
	d.pick("d1", OptionReject, decide.ViaUI, "")
	RunGate(context.Background(), s, d)
	got := show(t, s, j.ID)
	if got.State != StateFailed || !strings.Contains(got.Note, "not during the release") {
		t.Fatalf("job = %s %q, want failed with the note", got.State, got.Note)
	}
	if !strings.HasPrefix(d.done["d1"], "rejected") {
		t.Fatalf("decide done = %q, want the item closed as rejected", d.done["d1"])
	}
}

func TestAskGateRejectOnAFollowUpFails(t *testing.T) {
	s, d, j := askStore(t)
	d.pick("d1", OptionApprove, decide.ViaUI, "")
	d.question("d1", "x", OptionReject)
	d.pick("d1", OptionReject, decide.ViaUI, "")
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateFailed {
		t.Fatalf("state = %s, want failed", got.State)
	}
}

func TestAskGateAgentRejectIsIgnored(t *testing.T) {
	s, d, j := askStore(t)
	d.pick("d1", OptionReject, decide.ViaUI, "worker")
	RunGate(context.Background(), s, d)
	if got := show(t, s, j.ID); got.State != StateGated {
		t.Fatalf("state = %s, want gated", got.State)
	}
}

func TestPolicyGatesAtEnqueue(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	s.SetPolicy(Policy{Default: GateAuto, Kinds: map[string]Gate{"push": GateOff}, Floor: []string{"merge"}})
	if j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"}); j.State != StateReady {
		t.Fatalf("auto: state = %s, want ready", j.State)
	}
	if j := mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#2"}); j.State != StateGated {
		t.Fatalf("floor: state = %s, want gated", j.State)
	}
	_, err := s.Enqueue(Args{Kind: "push", Subject: "o/r#3"})
	if err == nil || !strings.Contains(err.Error(), "off") {
		t.Fatalf("off: err = %v, want a refusal", err)
	}
}

func TestNoPolicyAsksForEveryKind(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	s.SetPolicy(Policy{})
	if j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"}); j.State != StateGated {
		t.Fatalf("state = %s, want gated", j.State)
	}
}

func TestAskGateDoesNothingWithoutDecide(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	s.SetPolicy(Policy{})
	j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	RunGate(context.Background(), s, nil)
	if got := show(t, s, j.ID); got.State != StateGated || got.Decide != "" {
		t.Fatalf("job = %s %q, want gated with no item", got.State, got.Decide)
	}
}
