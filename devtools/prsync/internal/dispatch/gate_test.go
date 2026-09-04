package dispatch

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

func TestCheckWorkingIsBusy(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pC", "w2:tC")}}}
	got, err := Check(context.Background(), h, "any", "", nil)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if got.Safe {
		t.Fatal("safe = true, want false while an agent is working")
	}
	if len(got.Busy) != 1 || got.Busy[0].PaneID != "w2:pC" || got.Busy[0].TabID != "w2:tC" {
		t.Fatalf("busy = %+v, want [{w2:pC w2:tC}]", got.Busy)
	}
}

func TestCheckIdleIsSafe(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	got, err := Check(context.Background(), h, "any", "", nil)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("safe = false, want true; busy=%+v", got.Busy)
	}
	if got.Busy == nil {
		t.Fatal("busy = nil, want empty slice")
	}
	if len(got.Busy) != 0 {
		t.Fatalf("busy = %+v, want empty", got.Busy)
	}
}

func TestCheckSelfExclusion(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pC", "w2:tC")}}}
	got, err := Check(context.Background(), h, "any", "w2:pC", nil)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("safe = false, want true after excluding runner pane; busy=%+v", got.Busy)
	}
}

func TestCheckUnsetRunnerDoesNotExclude(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pFocus", "w2:tFocus")}}}
	got, err := Check(context.Background(), h, "any", "", nil)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if got.Safe {
		t.Fatal("safe = true, want false when runner pane is unset")
	}
	if len(got.Busy) != 1 || got.Busy[0].PaneID != "w2:pFocus" {
		t.Fatalf("busy = %+v, want focused pane included", got.Busy)
	}
}

func TestCheckManagedIgnoresUnmatched(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pX", "w2:tOther")}}}
	matched := map[string]struct{}{"w2:tC": {}}
	got, err := Check(context.Background(), h, "managed", "", matched)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("safe = false, want true; managed must ignore unmatched working agents; busy=%+v", got.Busy)
	}
}

func TestCheckManagedBusyWhenMatched(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pC", "w2:tC")}}}
	matched := map[string]struct{}{"w2:tC": {}}
	got, err := Check(context.Background(), h, "managed", "", matched)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if got.Safe {
		t.Fatal("safe = true, want false when a matched tab is working")
	}
}

func TestCheckBlockedIsBusy(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{blockedAgent("w2:pC", "w2:tC")}}}
	got, err := Check(context.Background(), h, "any", "", nil)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if got.Safe {
		t.Fatal("safe = true, want false while an agent is blocked awaiting the user")
	}
	if len(got.Busy) != 1 || got.Busy[0].PaneID != "w2:pC" || got.Busy[0].TabID != "w2:tC" {
		t.Fatalf("busy = %+v, want [{w2:pC w2:tC}]", got.Busy)
	}
}

func TestCheckBlockedSelfExclusion(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{blockedAgent("w2:pC", "w2:tC")}}}
	got, err := Check(context.Background(), h, "any", "w2:pC", nil)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("safe = false, want true after excluding runner pane; busy=%+v", got.Busy)
	}
}

func TestCheckManagedBusyWhenMatchedBlocked(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{blockedAgent("w2:pC", "w2:tC")}}}
	matched := map[string]struct{}{"w2:tC": {}}
	got, err := Check(context.Background(), h, "managed", "", matched)
	if err != nil {
		t.Fatalf("Check() unexpected error: %v", err)
	}
	if got.Safe {
		t.Fatal("safe = true, want false when a matched tab is blocked")
	}
}

func TestCheckNonWorkingStatusesAreSafe(t *testing.T) {
	t.Parallel()

	for _, status := range []string{"idle", "done", "unknown", "none"} {
		t.Run(status, func(t *testing.T) {
			t.Parallel()
			h := &scriptHerdr{lists: [][]herdr.Agent{{{
				PaneID: "w2:pC", TabID: "w2:tC", Agent: "codex", AgentStatus: status,
			}}}}
			got, err := Check(context.Background(), h, "any", "", nil)
			if err != nil {
				t.Fatalf("Check() unexpected error: %v", err)
			}
			if !got.Safe {
				t.Fatalf("status %s: safe = false, want true; busy=%+v", status, got.Busy)
			}
		})
	}
}

func TestCheckRequireMinErrors(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		err  error
	}{
		{name: "not installed", err: herdr.ErrNotInstalled},
		{name: "unsupported", err: herdr.ErrUnsupported},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			h := &scriptHerdr{minErr: tc.err}
			_, err := Check(context.Background(), h, "any", "", nil)
			if !errors.Is(err, tc.err) {
				t.Fatalf("Check() error = %v, want %v", err, tc.err)
			}
		})
	}
}

func TestCheckAgentListError(t *testing.T) {
	t.Parallel()

	want := errors.New("agent list boom")
	h := &scriptHerdr{lists: [][]herdr.Agent{{}}, listErrs: []error{want}}
	_, err := Check(context.Background(), h, "any", "", nil)
	if !errors.Is(err, want) {
		t.Fatalf("Check() error = %v, want %v", err, want)
	}
}

func TestMatchedTabsIncludesNilPane(t *testing.T) {
	t.Parallel()

	doc := scan.Document{PRs: []scan.PR{
		{Tab: &scan.Tab{TabID: "w2:tC"}},
		{Tab: nil},
		{Tab: &scan.Tab{TabID: "w2:tD", PaneID: strPtr("w2:pD")}},
	}}
	got := MatchedTabs(doc)
	if _, ok := got["w2:tC"]; !ok {
		t.Fatalf("matched = %v, want w2:tC (nil pane_id still counts)", got)
	}
	if _, ok := got["w2:tD"]; !ok {
		t.Fatalf("matched = %v, want w2:tD", got)
	}
	if len(got) != 2 {
		t.Fatalf("matched = %v, want 2 tabs", got)
	}
}

func TestWaitFlipsWorkingToIdle(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{
		{workingAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
	}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	sleeper := &fakeSleeper{clock: clock}
	cfg := config.Defaults()
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond

	got, err := Wait(context.Background(), h, cfg, "", nil, clock, sleeper)
	if err != nil {
		t.Fatalf("Wait() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("safe = false after flip, busy=%+v", got.Busy)
	}
	if h.n != 1+settleDebouncePolls {
		t.Fatalf("AgentList calls = %d, want %d (busy then %d idle)", h.n, 1+settleDebouncePolls, settleDebouncePolls)
	}
	if sleeper.n != settleDebouncePolls {
		t.Fatalf("Sleep calls = %d, want %d", sleeper.n, settleDebouncePolls)
	}
}

func TestWaitFlipsBlockedToIdle(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{
		{blockedAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
	}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	sleeper := &fakeSleeper{clock: clock}
	cfg := config.Defaults()
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond

	got, err := Wait(context.Background(), h, cfg, "", nil, clock, sleeper)
	if err != nil {
		t.Fatalf("Wait() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("safe = false after flip, busy=%+v", got.Busy)
	}
	if h.n != 1+settleDebouncePolls {
		t.Fatalf("AgentList calls = %d, want %d (blocked then %d idle)", h.n, 1+settleDebouncePolls, settleDebouncePolls)
	}
	if sleeper.n != settleDebouncePolls {
		t.Fatalf("Sleep calls = %d, want %d", sleeper.n, settleDebouncePolls)
	}
}

func TestWaitDebouncesSingleIdleAfterWorking(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{
		{workingAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
		{workingAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
		{idleAgent("w2:pC", "w2:tC")},
	}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	sleeper := &fakeSleeper{clock: clock}
	cfg := config.Defaults()
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond

	got, err := Wait(context.Background(), h, cfg, "", nil, clock, sleeper)
	if err != nil {
		t.Fatalf("Wait() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("safe = false after debounce, busy=%+v", got.Busy)
	}
	if h.n != 6 {
		t.Fatalf("AgentList calls = %d, want 6 (single idle after working is not safe)", h.n)
	}
}

func TestWaitIdleMustHoldDebouncePolls(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	sleeper := &fakeSleeper{clock: clock}
	cfg := config.Defaults()
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond

	got, err := Wait(context.Background(), h, cfg, "", nil, clock, sleeper)
	if err != nil {
		t.Fatalf("Wait() unexpected error: %v", err)
	}
	if !got.Safe {
		t.Fatalf("safe = false, want true; busy=%+v", got.Busy)
	}
	if h.n != settleDebouncePolls {
		t.Fatalf("AgentList calls = %d, want %d (empty busy set must hold)", h.n, settleDebouncePolls)
	}
}

func TestWaitTimeoutStaysBusy(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pC", "w2:tC")}}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	sleeper := &fakeSleeper{clock: clock}
	cfg := config.Defaults()
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 3 * time.Millisecond

	got, err := Wait(context.Background(), h, cfg, "", nil, clock, sleeper)
	if !errors.Is(err, ErrTimeout) {
		t.Fatalf("Wait() error = %v, want ErrTimeout", err)
	}
	if got.Safe {
		t.Fatal("safe = true on timeout, want false")
	}
}

func TestWaitHonorsContextCancelDuringSleep(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pC", "w2:tC")}}}
	clock := &fakeClock{now: time.Unix(0, 0).UTC()}
	ctx, cancel := context.WithCancel(context.Background())
	sleeper := &fakeSleeper{clock: clock, before: cancel}
	cfg := config.Defaults()
	cfg.GatePoll = time.Millisecond
	cfg.GateTimeout = 50 * time.Millisecond

	_, err := Wait(ctx, h, cfg, "", nil, clock, sleeper)
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Wait() error = %v, want context.Canceled", err)
	}
}

type scriptHerdr struct {
	minErr      error
	lists       [][]herdr.Agent
	listErrs    []error
	n           int
	prompts     []herdr.PromptOutcome
	promptN     int
	sawUntil    []string
	lastPane    string
	sincePrompt int
	settling    bool
	postLists   [][]herdr.Agent
}

func (s *scriptHerdr) RequireMin(context.Context, string) error {
	s.settling = false
	s.sincePrompt = 0
	return s.minErr
}

func (s *scriptHerdr) Prompt(_ context.Context, pane, _ string, until []string, _ time.Duration) herdr.PromptOutcome {
	s.lastPane = pane
	s.settling = true
	s.sincePrompt = 0
	s.sawUntil = append([]string(nil), until...)
	if s.promptN < len(s.prompts) {
		out := s.prompts[s.promptN]
		s.promptN++
		return out
	}
	s.promptN++
	return herdr.PromptOutcome{Status: herdr.PromptMatched}
}

func (s *scriptHerdr) AgentList(context.Context) ([]herdr.Agent, error) {
	if s.settling {
		s.sincePrompt++
		if len(s.postLists) > 0 {
			i := s.sincePrompt - 1
			if i >= len(s.postLists) {
				i = len(s.postLists) - 1
			}
			return s.postLists[i], nil
		}
		pane := s.lastPane
		if pane == "" {
			pane = "w2:pC"
		}
		status := "idle"
		if s.sincePrompt == 1 {
			status = "working"
		}
		return []herdr.Agent{seqAgent(pane, "w2:tC", status, s.sincePrompt+1)}, nil
	}
	i := s.n
	s.n++
	if i < len(s.listErrs) && s.listErrs[i] != nil {
		return nil, s.listErrs[i]
	}
	if len(s.lists) == 0 {
		return nil, errors.New("no agent lists configured")
	}
	if i >= len(s.lists) {
		i = len(s.lists) - 1
	}
	return s.lists[i], nil
}

type fakeClock struct {
	now time.Time
}

func (c *fakeClock) Now() time.Time { return c.now }

type fakeSleeper struct {
	clock  *fakeClock
	n      int
	before func()
}

func (s *fakeSleeper) Sleep(ctx context.Context, d time.Duration) error {
	s.n++
	if s.before != nil {
		s.before()
	}
	if err := ctx.Err(); err != nil {
		return fmt.Errorf("sleep: %w", err)
	}
	s.clock.now = s.clock.now.Add(d)
	return nil
}

func workingAgent(paneID, tabID string) herdr.Agent {
	return herdr.Agent{PaneID: paneID, TabID: tabID, Agent: "codex", AgentStatus: "working"}
}

func idleAgent(paneID, tabID string) herdr.Agent {
	return seqAgent(paneID, tabID, "idle", 0)
}

func seqAgent(paneID, tabID, status string, seq int) herdr.Agent {
	return herdr.Agent{
		PaneID:         paneID,
		TabID:          tabID,
		Agent:          "codex",
		AgentStatus:    status,
		StateChangeSeq: seq,
	}
}

func blockedSettlePost(paneID, tabID string) [][]herdr.Agent {
	working := seqAgent(paneID, tabID, "working", 2)
	blocked := seqAgent(paneID, tabID, "blocked", 3)
	return [][]herdr.Agent{{working}, {blocked}, {blocked}, {blocked}}
}

func blockedAgent(paneID, tabID string) herdr.Agent {
	return herdr.Agent{PaneID: paneID, TabID: tabID, Agent: "codex", AgentStatus: "blocked"}
}

func strPtr(s string) *string { return &s }
