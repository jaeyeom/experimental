package dispatch

import (
	"context"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

func TestSendPromptDoesNotSettleOnTransientIdleAfterBlocked(t *testing.T) {
	t.Parallel()

	cfg, c, req := settlePromptFixture()
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}
	h := &scriptHerdr{
		prompts: []herdr.PromptOutcome{{
			Status: herdr.PromptMatched,
			Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "idle"},
		}},
		postLists: [][]herdr.Agent{
			{idleAgent("w2:pC", "w2:tC")},
			{workingAgent("w2:pC", "w2:tC")},
			{seqAgent("w2:pC", "w2:tC", "done", 9)},
			{seqAgent("w2:pC", "w2:tC", "done", 9)},
			{seqAgent("w2:pC", "w2:tC", "done", 9)},
		},
		waits: []herdr.PromptOutcome{{
			Status: herdr.PromptMatched,
			Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "done"},
		}},
	}

	got := sendPrompt(context.Background(), h, cfg, c, req, clock, sleeper)
	if got.Action != ActionDispatched {
		t.Fatalf("action = %q, want dispatched after stable done (not the transient idle)", got.Action)
	}
	if h.waitN != 1 {
		t.Fatalf("Wait calls = %d, want 1 (transient idle after blocked must keep waiting)", h.waitN)
	}
}

func TestSendPromptSettleSequences(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		until      []string
		prompt     herdr.PromptOutcome
		postLists  [][]herdr.Agent
		waits      []herdr.PromptOutcome
		timeout    time.Duration
		wantAction string
		wantWait   int
	}{
		{
			name: "working to done is dispatched",
			prompt: herdr.PromptOutcome{
				Status: herdr.PromptMatched,
				Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "done"},
			},
			postLists: [][]herdr.Agent{
				{seqAgent("w2:pC", "w2:tC", "done", 2)},
				{seqAgent("w2:pC", "w2:tC", "done", 2)},
				{seqAgent("w2:pC", "w2:tC", "done", 2)},
			},
			wantAction: ActionDispatched,
		},
		{
			name: "idle that stays idle is dispatched after debounce",
			prompt: herdr.PromptOutcome{
				Status: herdr.PromptMatched,
				Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "idle"},
			},
			postLists: [][]herdr.Agent{
				{idleAgent("w2:pC", "w2:tC")},
				{idleAgent("w2:pC", "w2:tC")},
				{idleAgent("w2:pC", "w2:tC")},
			},
			wantAction: ActionDispatched,
		},
		{
			name: "working blocked idle working is not dispatched on transient idle",
			prompt: herdr.PromptOutcome{
				Status: herdr.PromptMatched,
				Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "idle"},
			},
			postLists: [][]herdr.Agent{
				{idleAgent("w2:pC", "w2:tC")},
				{workingAgent("w2:pC", "w2:tC")},
				{workingAgent("w2:pC", "w2:tC")},
				{workingAgent("w2:pC", "w2:tC")},
			},
			waits:      []herdr.PromptOutcome{{Status: herdr.PromptTimeout}},
			timeout:    5 * time.Millisecond,
			wantAction: ActionDispatchedTimeout,
			wantWait:   1,
		},
		{
			name:  "blocked until includes blocked is dispatched_blocked after debounce",
			until: []string{"idle", "done", "blocked"},
			prompt: herdr.PromptOutcome{
				Status: herdr.PromptMatched,
				Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "blocked"},
			},
			postLists: [][]herdr.Agent{
				{blockedAgent("w2:pC", "w2:tC")},
				{blockedAgent("w2:pC", "w2:tC")},
				{blockedAgent("w2:pC", "w2:tC")},
			},
			wantAction: ActionDispatchedBlocked,
		},
		{
			name:   "blocked without blocked in until waits to timeout",
			prompt: herdr.PromptOutcome{Status: herdr.PromptTimeout},
			postLists: [][]herdr.Agent{
				{blockedAgent("w2:pC", "w2:tC")},
				{blockedAgent("w2:pC", "w2:tC")},
			},
			waits:      []herdr.PromptOutcome{{Status: herdr.PromptTimeout}},
			timeout:    5 * time.Millisecond,
			wantAction: ActionDispatchedTimeout,
			wantWait:   1,
		},
		{
			name:       "agent disappears mid-wait is dispatched_timeout",
			prompt:     herdr.PromptOutcome{Status: herdr.PromptTimeout},
			postLists:  [][]herdr.Agent{{}},
			wantAction: ActionDispatchedTimeout,
		},
		{
			name:       "prompt stalled is dispatched_timeout without wait",
			prompt:     herdr.PromptOutcome{Status: herdr.PromptStalled},
			wantAction: ActionDispatchedTimeout,
		},
		{
			name: "prompt matched idle is still confirmed via agent list",
			prompt: herdr.PromptOutcome{
				Status: herdr.PromptMatched,
				Agent:  herdr.Agent{PaneID: "w2:pC", AgentStatus: "idle"},
			},
			postLists: [][]herdr.Agent{
				{idleAgent("w2:pC", "w2:tC")},
				{idleAgent("w2:pC", "w2:tC")},
				{idleAgent("w2:pC", "w2:tC")},
			},
			wantAction: ActionDispatched,
		},
		{
			name:   "prompt timeout then stable idle is dispatched",
			prompt: herdr.PromptOutcome{Status: herdr.PromptTimeout},
			postLists: [][]herdr.Agent{
				{idleAgent("w2:pC", "w2:tC")},
				{idleAgent("w2:pC", "w2:tC")},
				{idleAgent("w2:pC", "w2:tC")},
			},
			wantAction: ActionDispatched,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			cfg, c, req := settlePromptFixture()
			if len(tt.until) > 0 {
				cfg.WaitUntil = tt.until
			}
			if tt.timeout > 0 {
				cfg.DispatchTimeout = tt.timeout
			}
			clock := &fakeClock{now: fixtureNow}
			sleeper := &fakeSleeper{clock: clock}
			h := &scriptHerdr{
				prompts:   []herdr.PromptOutcome{tt.prompt},
				postLists: tt.postLists,
				waits:     tt.waits,
			}
			got := sendPrompt(context.Background(), h, cfg, c, req, clock, sleeper)
			if got.Action != tt.wantAction {
				t.Fatalf("action = %q, want %q", got.Action, tt.wantAction)
			}
			if h.waitN != tt.wantWait {
				t.Fatalf("Wait calls = %d, want %d", h.waitN, tt.wantWait)
			}
		})
	}
}

func TestGateDebounceTreatsBlockedAsBusyUnlikeSettleUntilIdle(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.ConcurrencyWaitOn = "any"
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}
	h := &scriptHerdr{lists: [][]herdr.Agent{
		{blockedAgent("w2:pX", "w2:tX")},
		{blockedAgent("w2:pX", "w2:tX")},
		{blockedAgent("w2:pX", "w2:tX")},
	}}
	_, err := Wait(context.Background(), h, cfg, "", nil, clock, sleeper)
	if err == nil {
		t.Fatal("gate Wait() error = nil, want timeout (blocked is busy)")
	}
}

func settlePromptFixture() (config.Config, Candidate, Request) {
	cfg := config.Defaults()
	cfg.DryRun = false
	cfg.GatePoll = time.Millisecond
	cfg.DispatchTimeout = time.Second
	pr := fixtureEligiblePR()
	return cfg, Candidate{Repo: pr.Repo, Number: pr.Number, PR: &pr}, Request{}
}
