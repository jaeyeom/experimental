package dispatch

import (
	"context"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

func TestEvaluateOutOfScopeIsSkippedWithItsReason(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	pr.OutOfScope = "tab on local"
	got := Evaluate(Candidate{Repo: pr.Repo, Number: pr.Number, PR: &pr}, config.Defaults(), State{}, false, false, false, fixtureNow)
	if got.Action != ActionSkippedOutOfScope || got.Detail != "tab on local" {
		t.Fatalf("Evaluate = %+v, want skipped_out_of_scope (tab on local)", got)
	}
}

// heldHerdr is one idle agent whose prompt, when hold is set, waits for
// release after closing entered.
type heldHerdr struct {
	pane, tab string
	hold      bool
	entered   chan struct{}
	release   chan struct{}
}

func (h *heldHerdr) RequireMin(context.Context, string) error { return nil }

func (h *heldHerdr) AgentList(context.Context) ([]herdr.Agent, error) {
	return []herdr.Agent{idleAgent(h.pane, h.tab)}, nil
}

func (h *heldHerdr) Prompt(context.Context, string, string, []string, time.Duration) herdr.PromptOutcome {
	if h.hold {
		close(h.entered)
		<-h.release
	}
	return herdr.PromptOutcome{Status: herdr.PromptMatched}
}

func (h *heldHerdr) Wait(context.Context, string, []string, time.Duration) herdr.PromptOutcome {
	return herdr.PromptOutcome{Status: herdr.PromptMatched}
}

// TestRunScopedLiveRunsShareOneStateFile runs a live dispatch scoped to
// m2 while one scoped to local is mid-prompt. Both finish, and the state
// file keeps both entries and the one already there.
func TestRunScopedLiveRunsShareOneStateFile(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	cfg.DispatchTimeout = 5 * time.Second
	if err := SaveFile(store.Path, State{"acme/other#9": {DispatchedCommentIDs: []string{"c9"}, DispatchedAt: "2026-01-01T00:00:00Z"}}); err != nil {
		t.Fatal(err)
	}
	localPR := fixtureEligiblePR()
	remote := remotePR()
	remote.Number = 124
	localPR.OutOfScope, remote.OutOfScope = "", "tab on m2"
	localDoc := scan.Document{PRs: []scan.PR{localPR, remote}}
	localPR.OutOfScope, remote.OutOfScope = "tab on local", ""
	remoteDoc := scan.Document{PRs: []scan.PR{localPR, remote}}

	held := &heldHerdr{pane: "w2:pC", tab: "w2:tC", hold: true, entered: make(chan struct{}), release: make(chan struct{})}
	var wg sync.WaitGroup
	var localOut Document
	var localErr error
	wg.Add(1)
	go func() {
		defer wg.Done()
		localOut, localErr = Run(context.Background(), held, FileStore{Path: store.Path, Scope: "local"}, cfg, Request{Doc: localDoc}, fixtureNow)
	}()
	<-held.entered

	m2 := &heldHerdr{pane: "w3:p2", tab: "w3:t2"}
	remoteOut, remoteErr := Run(context.Background(), m2, FileStore{Path: store.Path, Scope: "m2"}, cfg, Request{
		Doc:    remoteDoc,
		Remote: func(string) Herdr { return m2 },
	}, fixtureNow)
	close(held.release)
	wg.Wait()

	if localErr != nil || remoteErr != nil {
		t.Fatalf("errors = %v, %v; want both runs to finish", localErr, remoteErr)
	}
	want := map[int]string{123: ActionDispatched, 124: ActionSkippedOutOfScope}
	for _, item := range localOut.Results {
		if item.Action != want[item.Number] {
			t.Fatalf("local results = %+v, want %v", localOut.Results, want)
		}
	}
	want = map[int]string{123: ActionSkippedOutOfScope, 124: ActionDispatched}
	for _, item := range remoteOut.Results {
		if item.Action != want[item.Number] {
			t.Fatalf("m2 results = %+v, want %v", remoteOut.Results, want)
		}
	}
	st, err := LoadFile(store.Path)
	if err != nil {
		t.Fatal(err)
	}
	for _, key := range []string{"acme/widgets#123", "acme/widgets#124", "acme/other#9"} {
		if _, ok := st[key]; !ok {
			t.Fatalf("state = %+v, missing %s", st, key)
		}
	}
}
