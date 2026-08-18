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

func TestCheckNonWorkingStatusesAreSafe(t *testing.T) {
	t.Parallel()

	for _, status := range []string{"idle", "done", "blocked", "unknown", "none"} {
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
	if h.n != 2 {
		t.Fatalf("AgentList calls = %d, want 2 (busy then idle)", h.n)
	}
	if sleeper.n != 1 {
		t.Fatalf("Sleep calls = %d, want 1", sleeper.n)
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
	minErr   error
	lists    [][]herdr.Agent
	listErrs []error
	n        int
	prompts  []herdr.PromptOutcome
	promptN  int
	sawUntil []string
}

func (s *scriptHerdr) RequireMin(context.Context, string) error { return s.minErr }

func (s *scriptHerdr) Prompt(_ context.Context, _, _ string, until []string, _ time.Duration) herdr.PromptOutcome {
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
	return herdr.Agent{PaneID: paneID, TabID: tabID, Agent: "codex", AgentStatus: "idle"}
}

func strPtr(s string) *string { return &s }
