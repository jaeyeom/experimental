package outbox

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

type fakeApprover struct {
	items map[string]*decide.Item
	asks  []AskRequest
	done  map[string]string
}

func newFakeApprover() *fakeApprover {
	return &fakeApprover{items: map[string]*decide.Item{}, done: map[string]string{}}
}

func (f *fakeApprover) Ask(_ context.Context, req AskRequest) (string, error) {
	f.asks = append(f.asks, req)
	id := fmt.Sprintf("D-%d", len(f.asks))
	f.items[id] = &decide.Item{ID: id, State: decide.StateOpen}
	return id, nil
}

func (f *fakeApprover) Show(_ context.Context, id string) (decide.Item, error) {
	it, ok := f.items[id]
	if !ok {
		return decide.Item{}, errors.New("no such item")
	}
	return *it, nil
}

func (f *fakeApprover) Done(_ context.Context, id, outcome string) error {
	f.done[id] = outcome
	if it, ok := f.items[id]; ok {
		it.State = decide.StateDone
	}
	return nil
}

func (f *fakeApprover) pick(id, author, key, text, via, by string) {
	it := f.items[id]
	it.Messages = append(it.Messages, decide.Message{
		Author: author, Text: text, OptionKey: key, Via: via, By: by,
	})
	if author == decide.Human && by == "" {
		it.State = decide.StateAnswered
		val := text
		if key != "" {
			val = key
		}
		it.Answer = &decide.Answer{Value: val, Key: key, AnsweredBy: decide.Human, Via: via}
	}
}

type fakeExecutor struct {
	calls int
	err   error
	steps []string
}

func (e *fakeExecutor) Steps(_ context.Context, r Record) ([]Step, error) {
	targets := e.steps
	if len(targets) == 0 {
		targets = []string{r.Target}
	}
	out := make([]Step, 0, len(targets))
	for _, target := range targets {
		out = append(out, Step{
			Where: target,
			Run: func(context.Context) (string, error) {
				e.calls++
				if e.err != nil {
					return "", e.err
				}
				return fmt.Sprintf("https://ext/%s/%d", target, e.calls), nil
			},
		})
	}
	return out, nil
}

func TestEngineSubmitPollWithFakeExecutorAndTransport(t *testing.T) {
	store := NewFileStore(t.TempDir())
	dec := newFakeApprover()
	exec := &fakeExecutor{steps: []string{"step-1", "step-2"}}
	var notices []Notice
	eng := &Engine{
		Store:     store,
		Decide:    dec,
		Executor:  exec,
		Transport: TransportFunc(func(_ context.Context, n Notice) error { notices = append(notices, n); return nil }),
		Lock:      store.Lock,
	}

	ctx := context.Background()
	rec, err := eng.Submit(ctx, Draft{
		Kind:     "deploy",
		Target:   "prod-east",
		Owner:    "release-cli",
		Ref:      "rel-9",
		Question: "Deploy prod-east?",
	})
	if err != nil {
		t.Fatal(err)
	}
	if rec.Digest == "" || rec.DecideID != "D-1" {
		t.Fatalf("unexpected record: %+v", rec)
	}

	// CLI pick must not execute.
	dec.pick(rec.DecideID, decide.Human, "post", "post", decide.ViaCLI, "")
	if err := eng.Poll(ctx); err != nil {
		t.Fatal(err)
	}
	if exec.calls != 0 {
		t.Fatalf("CLI pick executed %d calls", exec.calls)
	}

	// Real UI pick executes once across repeated polls.
	dec.pick(rec.DecideID, decide.Human, "post", "post", decide.ViaUI, "")
	if err := eng.Poll(ctx); err != nil {
		t.Fatal(err)
	}
	if err := eng.Poll(ctx); err != nil {
		t.Fatal(err)
	}
	if exec.calls != 2 {
		t.Fatalf("exec calls = %d, want 2", exec.calls)
	}
	if len(notices) != 1 || notices[0].State != StateExecuted || !strings.Contains(notices[0].Message, "https://ext/step-2/2") {
		t.Fatalf("notices = %+v", notices)
	}
	if !strings.Contains(dec.done[rec.DecideID], "https://ext/step-1/1") {
		t.Errorf("decide done = %q", dec.done[rec.DecideID])
	}
}

func TestEngineDigestMismatchRefusesExecution(t *testing.T) {
	store := NewFileStore(t.TempDir())
	dec := newFakeApprover()
	exec := &fakeExecutor{}
	eng := &Engine{Store: store, Decide: dec, Executor: exec}

	ctx := context.Background()
	rec, err := eng.Submit(ctx, Draft{
		Kind:     "action",
		Target:   "svc",
		Owner:    "owner",
		Question: "Run clean action?",
		Verbatim: "echo clean",
	})
	if err != nil {
		t.Fatal(err)
	}

	rec.Verbatim = "echo tampered"
	if err := store.Save(rec); err != nil {
		t.Fatal(err)
	}
	dec.pick(rec.DecideID, decide.Human, "post", "post", decide.ViaUI, "")
	if err := eng.Poll(ctx); err != nil {
		t.Fatal(err)
	}
	if exec.calls != 0 {
		t.Fatalf("tampered record executed %d calls", exec.calls)
	}
	got, err := store.Get(rec.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateRejected || !strings.Contains(got.Outcome, "digest") {
		t.Fatalf("got = %+v, want StateRejected for digest mismatch", got)
	}
}

func TestEngineMidExecutionRestartBecomesUncertain(t *testing.T) {
	store := NewFileStore(t.TempDir())
	dec := newFakeApprover()
	exec := &fakeExecutor{}
	eng := &Engine{Store: store, Decide: dec, Executor: exec}

	ctx := context.Background()
	rec, err := eng.Submit(ctx, Draft{Kind: "action", Target: "svc", Owner: "owner", Question: "Run?"})
	if err != nil {
		t.Fatal(err)
	}
	idx := 0
	rec.State = StateExecuting
	rec.InFlight = &idx
	if err := store.Save(rec); err != nil {
		t.Fatal(err)
	}
	if err := eng.Poll(ctx); err != nil {
		t.Fatal(err)
	}
	if exec.calls != 0 {
		t.Fatalf("uncertain retry called executor %d times", exec.calls)
	}
	got, err := store.Get(rec.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateUncertain || got.InFlight != nil {
		t.Fatalf("got = %+v, want StateUncertain", got)
	}
}

func TestCommandExecutorAndCommandTransportScriptUsage(t *testing.T) {
	dir := t.TempDir()
	execLog := filepath.Join(dir, "exec.log")
	execScript := filepath.Join(dir, "run.sh")
	if err := os.WriteFile(execScript, []byte("#!/bin/sh\ncat > '"+execLog+"'\necho 'https://receipt/42'\n"), 0o700); err != nil {
		t.Fatal(err)
	}
	notifyLog := filepath.Join(dir, "notify.log")
	notifyScript := filepath.Join(dir, "notify.sh")
	if err := os.WriteFile(notifyScript, []byte("#!/bin/sh\nprintf '%s:%s\\n' \"$OUTBOX_OWNER\" \"$OUTBOX_STATE\" > '"+notifyLog+"'\ncat >> '"+notifyLog+"'\n"), 0o700); err != nil {
		t.Fatal(err)
	}

	store := NewFileStore(dir)
	dec := newFakeApprover()
	eng := &Engine{
		Store:     store,
		Decide:    dec,
		Executor:  CommandExecutor{Command: []string{execScript}},
		Transport: CommandTransport{Command: []string{notifyScript}},
	}
	ctx := context.Background()
	rec, err := eng.Submit(ctx, Draft{
		Kind:     "script",
		Target:   "db/migrate",
		Owner:    "ops-script",
		Question: "Apply migration?",
	})
	if err != nil {
		t.Fatal(err)
	}
	dec.pick(rec.DecideID, decide.Human, "post", "post", decide.ViaUI, "")
	if err := eng.Poll(ctx); err != nil {
		t.Fatal(err)
	}

	execPayload, err := os.ReadFile(execLog)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(execPayload), `"target": "db/migrate"`) && !strings.Contains(string(execPayload), `"target":"db/migrate"`) {
		t.Errorf("executor stdin = %q", string(execPayload))
	}
	notifyOut, err := os.ReadFile(notifyLog)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(notifyOut), "ops-script:executed") || !strings.Contains(string(notifyOut), "https://receipt/42") {
		t.Errorf("notify output = %q", string(notifyOut))
	}
}
