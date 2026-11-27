package dispatch

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// concurrentHerdr simulates multiple independent herdr panes running
// prompts concurrently and tracks peak in-flight concurrency and
// per-pane prompt counts.
type concurrentHerdr struct {
	mu           sync.Mutex
	inFlight     int
	maxInFlight  int
	promptCalls  map[string]int
	promptTexts  map[string]string
	onPrompt     func(ctx context.Context, pane, text string) herdr.PromptOutcome
	postStatuses map[string]string
}

func newConcurrentHerdr() *concurrentHerdr {
	return &concurrentHerdr{
		promptCalls:  make(map[string]int),
		promptTexts:  make(map[string]string),
		postStatuses: make(map[string]string),
	}
}

func (h *concurrentHerdr) setStatus(pane, status string) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.postStatuses[pane] = status
}

func (h *concurrentHerdr) RequireMin(context.Context, string) error { return nil }

func (h *concurrentHerdr) AgentList(context.Context) ([]herdr.Agent, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	var out []herdr.Agent
	for pane, status := range h.postStatuses {
		out = append(out, herdr.Agent{PaneID: pane, Agent: "codex", AgentStatus: status})
	}
	return out, nil
}

func (h *concurrentHerdr) Prompt(ctx context.Context, paneID, text string, _ []string, _ time.Duration) herdr.PromptOutcome {
	h.mu.Lock()
	h.inFlight++
	if h.inFlight > h.maxInFlight {
		h.maxInFlight = h.inFlight
	}
	h.promptCalls[paneID]++
	h.promptTexts[paneID] = text
	fn := h.onPrompt
	h.mu.Unlock()

	defer func() {
		h.mu.Lock()
		h.inFlight--
		h.mu.Unlock()
	}()

	if fn != nil {
		return fn(ctx, paneID, text)
	}
	return herdr.PromptOutcome{Status: herdr.PromptMatched, Agent: herdr.Agent{PaneID: paneID, AgentStatus: "idle"}}
}

func (h *concurrentHerdr) Wait(_ context.Context, paneID string, _ []string, _ time.Duration) herdr.PromptOutcome {
	return herdr.PromptOutcome{Status: herdr.PromptMatched, Agent: herdr.Agent{PaneID: paneID, AgentStatus: "idle"}}
}

func parallelCfg(t *testing.T, maxParallel int, checkout string) (config.Config, FileStore) {
	t.Helper()
	cfg, store := liveCfg(t)
	cfg.DispatchMaxParallel = maxParallel
	cfg.GateLease = "required"
	cfg.Checkout = checkout
	return cfg, store
}

func numberedEligiblePR(n int) scan.PR {
	pr := fixtureEligiblePR()
	pr.Number = n
	id := "PROJ-" + itoa(n)
	pr.Identifier = &id
	pr.Head = "fix-" + itoa(n)
	pr.HeadSHA = "sha-" + itoa(n)
	pr.URL = "https://github.com/acme/widgets/pull/" + itoa(n)
	pane := "w2:p" + itoa(n)
	pr.Tab = &scan.Tab{
		TabID:       "w2:t" + itoa(n),
		PaneID:      &pane,
		WorkspaceID: "w2",
		Label:       id,
		AgentStatus: "idle",
	}
	pr.BlockingComments = []scan.Comment{{
		ThreadID:  "PRRT_" + itoa(n),
		CommentID: "PRRC_" + itoa(n),
		Author:    "reviewer",
		Path:      "a.go",
		URL:       pr.URL + "#r1",
		Body:      "fix this",
	}}
	return pr
}

func TestParallelDispatchBoundedAndMaintainsWindow(t *testing.T) {
	t.Parallel()

	checkout := gitRepo(t)
	cfg, store := parallelCfg(t, 3, checkout)
	lease := &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}

	// 5 PRs, --parallel 3: first 3 block on releaseCh; once one releases, 4th starts immediately.
	started := make(chan string, 10)
	releaseCh := make(chan struct{})
	h := newConcurrentHerdr()
	for i := 1; i <= 5; i++ {
		h.postStatuses["w2:p"+itoa(i)] = "idle"
	}
	h.onPrompt = func(_ context.Context, pane, _ string) herdr.PromptOutcome {
		started <- pane
		<-releaseCh
		return herdr.PromptOutcome{Status: herdr.PromptMatched, Agent: herdr.Agent{PaneID: pane, AgentStatus: "idle"}}
	}

	prs := []scan.PR{
		numberedEligiblePR(1),
		numberedEligiblePR(2),
		numberedEligiblePR(3),
		numberedEligiblePR(4),
		numberedEligiblePR(5),
	}
	lgr := &ledger.Store{}
	ctx := WithProbes(context.Background(), Probes{Lease: lease})

	type runRes struct {
		doc Document
		err error
	}
	done := make(chan runRes, 1)
	go func() {
		doc, err := Run(ctx, h, store, cfg, Request{
			Doc:    scan.Document{PRs: prs},
			Ledger: lgr,
		}, fixtureNow)
		done <- runRes{doc: doc, err: err}
	}()

	// Wait until exactly 3 prompts have started concurrently.
	var firstBatch []string
	for len(firstBatch) < 3 {
		select {
		case p := <-started:
			firstBatch = append(firstBatch, p)
		case <-time.After(3 * time.Second):
			t.Fatalf("timed out waiting for 3 initial concurrent prompts; got %v", firstBatch)
		}
	}
	// Ensure a 4th does NOT start while 3 are still running.
	select {
	case extra := <-started:
		t.Fatalf("started 4th prompt %q while 3 in flight", extra)
	case <-time.After(20 * time.Millisecond):
	}

	// Unblock one worker; verify a 4th immediately takes its slot.
	releaseCh <- struct{}{}
	select {
	case p := <-started:
		if slices.Contains(firstBatch, p) {
			t.Fatalf("reused pane %q", p)
		}
	case <-time.After(3 * time.Second):
		t.Fatal("4th PR did not start after one of the first 3 finished")
	}

	// Unblock the remaining 4 workers.
	close(releaseCh)

	var res runRes
	select {
	case res = <-done:
	case <-time.After(5 * time.Second):
		t.Fatal("parallel Run did not complete")
	}
	if res.err != nil {
		t.Fatalf("Run() unexpected error: %v", res.err)
	}
	if h.maxInFlight != 3 {
		t.Fatalf("maxInFlight = %d, want 3", h.maxInFlight)
	}
	if len(res.doc.Results) != 5 {
		t.Fatalf("results len = %d, want 5", len(res.doc.Results))
	}
	for i, item := range res.doc.Results {
		if item.Action != ActionDispatched || item.Number != i+1 {
			t.Fatalf("result[%d] = %+v, want dispatched #%d", i, item, i+1)
		}
		if !strings.Contains(item.RenderedPrompt, "agentbus lease claim") || !strings.Contains(item.RenderedPrompt, "agentbus lease release") {
			t.Fatalf("result[%d] prompt missing lease instruction: %q", i, item.RenderedPrompt)
		}
	}
	if res.doc.Summary == nil || res.doc.Summary.MaxInFlight != 3 || res.doc.Summary.Finished != 5 || res.doc.Summary.TimedOut != 0 || res.doc.Summary.Blocked != 0 || res.doc.Summary.InFlight != 0 {
		t.Fatalf("summary = %+v", res.doc.Summary)
	}
	entries, err := lgr.Load()
	if err != nil || len(entries) != 5 {
		t.Fatalf("ledger entries = %d (%v), want 5", len(entries), err)
	}
	st := mustLoadState(t, store)
	for i := 1; i <= 5; i++ {
		if !st.Deduped("acme/widgets#"+itoa(i), []string{"PRRC_" + itoa(i)}) {
			t.Fatalf("missing dedupe state for #%d: %#v", i, st)
		}
	}
}

func TestParallelDispatchNeverTwoOnSamePRorTabAtOnce(t *testing.T) {
	t.Parallel()

	checkout := gitRepo(t)
	cfg, store := parallelCfg(t, 3, checkout)
	lease := &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}

	var overlap atomic.Int32
	var activeOnSameTab atomic.Int32
	h := newConcurrentHerdr()
	h.postStatuses["w2:p1"] = "idle"
	h.onPrompt = func(_ context.Context, pane, _ string) herdr.PromptOutcome {
		if n := activeOnSameTab.Add(1); n > 1 {
			overlap.Add(1)
		}
		time.Sleep(15 * time.Millisecond)
		activeOnSameTab.Add(-1)
		return herdr.PromptOutcome{Status: herdr.PromptMatched, Agent: herdr.Agent{PaneID: pane, AgentStatus: "idle"}}
	}

	// Duplicate --pr entry AND another PR pointing at the same pane/tab.
	pr1 := numberedEligiblePR(1)
	pr2 := numberedEligiblePR(2)
	pr2.Tab = pr1.Tab
	ctx := WithProbes(context.Background(), Probes{Lease: lease})
	got, err := Run(ctx, h, store, cfg, Request{
		Doc:   scan.Document{PRs: []scan.PR{pr1, pr2}},
		PRs:   []string{"acme/widgets#1", "acme/widgets#1", "acme/widgets#2"},
		Force: true,
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if overlap.Load() > 0 {
		t.Fatalf("detected %d overlapping dispatches on same PR/tab", overlap.Load())
	}
	if len(got.Results) != 3 {
		t.Fatalf("results len = %d, want 3", len(got.Results))
	}
	if h.promptCalls["w2:p1"] != 3 {
		t.Fatalf("prompt calls = %d, want 3 sequential runs with --force", h.promptCalls["w2:p1"])
	}
}

func TestParallelDispatchRequiresLease(t *testing.T) {
	t.Parallel()

	checkout := gitRepo(t)
	prs := []scan.PR{numberedEligiblePR(1)}

	for _, tc := range []struct {
		name      string
		gateLease string
		lease     LeaseSource
	}{
		{name: "gate_lease=auto", gateLease: "auto", lease: &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}},
		{name: "gate_lease=off", gateLease: "off", lease: &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}},
		{name: "nil LeaseSource", gateLease: "required", lease: nil},
		{name: "agentbus missing", gateLease: "required", lease: &fakeLease{guardedErr: ErrAgentbusMissing}},
		{name: "unguarded checkout", gateLease: "required", lease: &fakeLease{guarded: false}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg, store := parallelCfg(t, 3, checkout)
			cfg.GateLease = tc.gateLease
			h := newConcurrentHerdr()
			ctx := context.Background()
			if tc.lease != nil {
				ctx = WithProbes(ctx, Probes{Lease: tc.lease})
			}
			_, err := Run(ctx, h, store, cfg, Request{Doc: scan.Document{PRs: prs}}, fixtureNow)
			if err == nil {
				t.Fatalf("%s: expected error refusing parallel dispatch", tc.name)
			}
			if h.promptCalls["w2:p1"] != 0 {
				t.Fatalf("%s: dispatched despite missing lease", tc.name)
			}
		})
	}
}

func TestParallelGateAllowsDirtyWhenDispatchedHoldsLeaseAndBlocksOtherwise(t *testing.T) {
	t.Parallel()

	checkout := gitRepo(t)
	writeTestFile(t, filepath.Join(checkout, "a.txt"), "dirty\n")

	cfg, _ := parallelCfg(t, 3, checkout)
	dispatched := newDispatchedSet()
	dispatched.Add("", "w2:p1")

	// 1. Dirty checkout with dispatched lease holder DOES NOT block.
	leaseHeldByDispatched := &fakeLease{
		guarded: true,
		status: LeaseStatus{
			Checkout: "local:" + checkout,
			State:    "held",
			Holder:   &LeaseHolder{Handle: "herdr:local/default/w2:p1"},
		},
	}
	ctx := WithProbes(context.Background(), Probes{Lease: leaseHeldByDispatched})
	res, err := gateCheckWithPolicy(ctx, newConcurrentHerdr(), cfg, "", nil, dispatched.ContainsHolder)
	if err != nil {
		t.Fatalf("gate check error: %v", err)
	}
	if !res.Safe || res.Reason != "" || len(res.Busy) != 0 {
		t.Fatalf("dirty with dispatched holder = %+v, want safe", res)
	}

	// 2. Lease held by something NOT dispatched DOES block.
	leaseHeldByOther := &fakeLease{
		guarded: true,
		status: LeaseStatus{
			Checkout: "local:" + checkout,
			State:    "held",
			Holder:   &LeaseHolder{Handle: "herdr:local/default/w9:p9", Name: "pm/other"},
		},
	}
	ctxOther := WithProbes(context.Background(), Probes{Lease: leaseHeldByOther})
	res, err = gateCheckWithPolicy(ctxOther, newConcurrentHerdr(), cfg, "", nil, dispatched.ContainsHolder)
	if err != nil {
		t.Fatalf("gate check other error: %v", err)
	}
	if res.Safe || len(res.Busy) != 1 || res.Busy[0].Signal != SignalLease || !strings.Contains(res.Busy[0].Detail, "pm/other") {
		t.Fatalf("held by other = %+v, want lease hold", res)
	}

	// 3. Working non-dispatched tab when lease is free does NOT block parallel gate.
	leaseFree := &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}
	cleanCheckout := gitRepo(t)
	cfgClean, _ := parallelCfg(t, 3, cleanCheckout)
	hWorking := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w9:p9", "w9:t9")}}}
	res, err = gateCheckWithPolicy(WithProbes(context.Background(), Probes{Lease: leaseFree}), hWorking, cfgClean, "", nil, dispatched.ContainsHolder)
	if err != nil {
		t.Fatalf("gate check free clean error: %v", err)
	}
	if !res.Safe || len(res.Busy) != 0 {
		t.Fatalf("free clean parallel gate = %+v, want safe despite other working tabs", res)
	}

	// 4. Dirty checkout with NO lease holder (state=free) DOES block (ErrDirtyTree / gate_unsafe).
	cfgDirty, store := parallelCfg(t, 3, checkout)
	ctxFreeDirty := WithProbes(context.Background(), Probes{Lease: leaseFree})
	doc, err := Run(ctxFreeDirty, newConcurrentHerdr(), store, cfgDirty, Request{
		Doc: scan.Document{PRs: []scan.PR{numberedEligiblePR(1), numberedEligiblePR(2)}},
	}, fixtureNow)
	if !errors.Is(err, ErrDirtyTree) {
		t.Fatalf("free dirty Run() err = %v, want ErrDirtyTree", err)
	}
	var unsafeN, queuedN int
	for _, r := range doc.Results {
		switch r.Action {
		case ActionGateUnsafe:
			unsafeN++
		case ActionQueued:
			queuedN++
		}
	}
	if len(doc.Results) != 2 || unsafeN != 1 || queuedN != 1 {
		t.Fatalf("free dirty results = %+v, want 1 gate_unsafe + 1 queued", doc.Results)
	}
}

func TestParallelDispatchDirtyTreeDuringRunWhileFirstWorkerHoldsLease(t *testing.T) {
	t.Parallel()

	checkout := gitRepo(t)
	cfg, store := parallelCfg(t, 3, checkout)
	lease := &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}

	// PR 1 starts first while clean+free, claims the lease, and dirties the tree;
	// PR 2 must still be dispatched because PR 1 was dispatched by prsync.
	pr1Started := make(chan struct{})
	releaseAll := make(chan struct{})
	h := newConcurrentHerdr()
	h.postStatuses["w2:p1"] = "idle"
	h.postStatuses["w2:p2"] = "idle"
	h.onPrompt = func(_ context.Context, pane, _ string) herdr.PromptOutcome {
		if pane == "w2:p1" {
			lease.mu.Lock()
			lease.status = LeaseStatus{
				Checkout: "local:" + checkout,
				State:    "held",
				Holder:   &LeaseHolder{Handle: "herdr:local/default/w2:p1"},
			}
			lease.mu.Unlock()
			writeTestFile(t, filepath.Join(checkout, "a.txt"), "edited by p1\n")
			close(pr1Started)
		}
		<-releaseAll
		return herdr.PromptOutcome{Status: herdr.PromptMatched, Agent: herdr.Agent{PaneID: pane, AgentStatus: "idle"}}
	}

	cfg.DispatchMaxParallel = 2
	ctx := WithProbes(context.Background(), Probes{Lease: lease})
	type runRes struct {
		doc Document
		err error
	}
	done := make(chan runRes, 1)
	go func() {
		doc, err := Run(ctx, h, store, cfg, Request{
			Doc: scan.Document{PRs: []scan.PR{numberedEligiblePR(1), numberedEligiblePR(2), numberedEligiblePR(3)}},
		}, fixtureNow)
		done <- runRes{doc: doc, err: err}
	}()

	select {
	case <-pr1Started:
	case <-time.After(3 * time.Second):
		t.Fatal("timed out waiting for pr1 to dirty tree")
	}
	h.setStatus("w2:p3", "idle")
	close(releaseAll)

	res := <-done
	if res.err != nil {
		t.Fatalf("Run() err = %v", res.err)
	}
	for i, r := range res.doc.Results {
		if r.Action != ActionDispatched {
			t.Fatalf("result[%d] = %+v, want dispatched", i, r)
		}
	}
}

func TestParallelDispatchSummaryTimedOutAndBlocked(t *testing.T) {
	t.Parallel()

	checkout := gitRepo(t)
	cfg, store := parallelCfg(t, 3, checkout)
	cfg.WaitUntil = []string{"idle", "done", "blocked"}
	lease := &fakeLease{guarded: true, status: LeaseStatus{State: "free"}}

	h := newConcurrentHerdr()
	h.postStatuses["w2:p1"] = "idle"
	h.postStatuses["w2:p2"] = "blocked"
	h.postStatuses["w2:p3"] = "working"
	h.onPrompt = func(_ context.Context, pane, _ string) herdr.PromptOutcome {
		switch pane {
		case "w2:p2":
			return herdr.PromptOutcome{Status: herdr.PromptMatched, Agent: herdr.Agent{PaneID: pane, AgentStatus: "blocked"}}
		case "w2:p3":
			return herdr.PromptOutcome{Status: herdr.PromptStalled}
		default:
			return herdr.PromptOutcome{Status: herdr.PromptMatched, Agent: herdr.Agent{PaneID: pane, AgentStatus: "idle"}}
		}
	}

	var statusBuf bytes.Buffer
	ctx := WithStatusWriter(WithProbes(context.Background(), Probes{Lease: lease}), &statusBuf)
	doc, err := Run(ctx, h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{numberedEligiblePR(1), numberedEligiblePR(2), numberedEligiblePR(3)}},
	}, fixtureNow)
	if !errors.Is(err, ErrSettleTimeout) {
		t.Fatalf("err = %v, want ErrSettleTimeout", err)
	}
	if doc.Summary == nil {
		t.Fatal("Summary = nil, want RunSummary")
	}
	if doc.Summary.Finished != 1 || doc.Summary.Blocked != 1 || doc.Summary.TimedOut != 1 {
		t.Fatalf("Summary = %+v, want finished=1 blocked=1 timed_out=1", doc.Summary)
	}
	// Blocked PR must NOT stop #3 from running and must NOT write dedupe state.
	st := mustLoadState(t, store)
	if !st.Deduped("acme/widgets#1", []string{"PRRC_1"}) {
		t.Fatalf("missing #1 dedupe: %#v", st)
	}
	if _, ok := st["acme/widgets#2"]; ok {
		t.Fatalf("blocked #2 must not write dedupe state: %#v", st)
	}
	if !strings.Contains(statusBuf.String(), "finished=1") {
		t.Fatalf("status output missing summary: %q", statusBuf.String())
	}
}

type syncClock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *syncClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

type syncSleeper struct {
	clock *syncClock
}

func (s *syncSleeper) Sleep(ctx context.Context, d time.Duration) error {
	if err := ctx.Err(); err != nil {
		return fmt.Errorf("sync sleep: %w", err)
	}
	s.clock.mu.Lock()
	s.clock.now = s.clock.now.Add(d)
	s.clock.mu.Unlock()
	return nil
}

func TestParallelDispatchGateTimeoutExternalHolderAndRemoteLease(t *testing.T) {
	t.Parallel()

	t.Run("external holder times out gate", func(t *testing.T) {
		t.Parallel()
		checkout := gitRepo(t)
		cfg, store := parallelCfg(t, 2, checkout)
		cfg.GatePoll = time.Millisecond
		cfg.GateTimeout = 5 * time.Millisecond
		clock := &syncClock{now: fixtureNow}
		sleeper := &syncSleeper{clock: clock}
		lease := &fakeLease{
			guarded: true,
			status: LeaseStatus{
				Checkout: "local:" + checkout,
				State:    "held",
				Holder:   &LeaseHolder{Handle: "herdr:local/default/w9:p9", Name: "pm/foo"},
			},
		}
		h := newConcurrentHerdr()
		doc, err := Run(WithProbes(context.Background(), Probes{Lease: lease}), h, store, cfg, Request{
			Doc:     scan.Document{PRs: []scan.PR{numberedEligiblePR(1), numberedEligiblePR(2)}},
			Clock:   clock,
			Sleeper: sleeper,
		}, fixtureNow)
		if !errors.Is(err, ErrTimeout) {
			t.Fatalf("err = %v, want ErrTimeout", err)
		}
		if doc.Summary == nil || doc.Summary.TimedOut == 0 {
			t.Fatalf("summary = %+v, want TimedOut > 0", doc.Summary)
		}
		var timeoutN, queuedN int
		for _, r := range doc.Results {
			switch r.Action {
			case ActionGateTimeout:
				timeoutN++
			case ActionQueued:
				queuedN++
			default:
				t.Fatalf("unexpected action %q in %+v", r.Action, doc.Results)
			}
		}
		if timeoutN != 1 || queuedN != 1 {
			t.Fatalf("results = %+v, want 1 gate_timeout + 1 queued", doc.Results)
		}
	})

	t.Run("remote tab_machine parallel gate matches remote dispatched handle", func(t *testing.T) {
		t.Parallel()
		cfg := macCfg()
		cfg.DryRun = false
		cfg.DispatchMaxParallel = 2
		cfg.GateLease = "required"
		cfg.GatePoll = time.Millisecond
		cfg.GateTimeout = time.Second
		store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
		dispatched := newDispatchedSet()
		dispatched.Add("mac", "w3:p2")

		lease := &fakeLease{all: []LeaseStatus{{
			Checkout: macLeaseKey,
			State:    "held",
			Holder:   &LeaseHolder{Handle: "herdr:mac/s1/w3:p2"},
		}}}
		mac := fakeRemote{agents: []herdr.Agent{onMachine("mac", idleAgent("w3:p2", "w3:t2"))}}
		ctx := WithProbes(context.Background(), Probes{Remote: remotes(map[string]RemoteHerdr{"mac": mac}), Lease: lease})
		res, err := gateCheckWithPolicy(ctx, newConcurrentHerdr(), cfg, "", nil, dispatched.ContainsHolder)
		if err != nil {
			t.Fatalf("gate check error: %v", err)
		}
		if !res.Safe {
			t.Fatalf("remote dispatched holder blocked gate: %+v", res)
		}

		// Wrong machine handle on same pane still blocks.
		leaseWrong := &fakeLease{all: []LeaseStatus{{
			Checkout: macLeaseKey,
			State:    "held",
			Holder:   &LeaseHolder{Handle: "herdr:local/s1/w3:p2", Name: "other"},
		}}}
		res, err = gateCheckWithPolicy(WithProbes(context.Background(), Probes{Remote: remotes(map[string]RemoteHerdr{"mac": mac}), Lease: leaseWrong}), newConcurrentHerdr(), cfg, "", nil, dispatched.ContainsHolder)
		if err != nil {
			t.Fatalf("gate check error: %v", err)
		}
		if res.Safe || len(res.Busy) != 1 || res.Busy[0].Signal != SignalLease {
			t.Fatalf("wrong machine holder = %+v, want lease hold", res)
		}
		_ = store
	})
}
