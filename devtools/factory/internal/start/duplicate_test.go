package start

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestSecondStartRefusesActiveClaim(t *testing.T) {
	t.Parallel()
	for _, state := range []store.ClaimState{store.Claimed, store.Started} {
		for _, force := range []bool{false, true} {
			h := newHarness(t)
			if err := h.st.InsertClaim(context.Background(), store.Claim{
				Source:      "jira:acme",
				ID:          "PRJ-123",
				ClaimedAt:   fixedNow.Add(-5 * time.Minute),
				Machine:     "workstation",
				TabID:       "w1:t1",
				WorkspaceID: "w1",
				State:       state,
				UpdatedAt:   fixedNow.Add(-5 * time.Minute),
			}); err != nil {
				t.Fatal(err)
			}
			opts := applyOpt
			if force {
				opts = forceOpt
			}
			err := h.start(h.snap, okGate, opts)
			var oe *OutcomeError
			if !errors.As(err, &oe) || oe.ExitStatus() != ExitRefused {
				t.Fatalf("state=%s force=%t: err = %v", state, force, err)
			}
			msg := err.Error()
			for _, want := range []string{"workstation", "2026-11-01T11:55:00Z", "w1:t1", "factory release PRJ-123", "adopt"} {
				if !strings.Contains(msg, want) {
					t.Fatalf("state=%s force=%t: err = %q, want %q", state, force, msg, want)
				}
			}
			assertCalls(t, h.log, nil)
			claims, cerr := h.st.Claims(context.Background())
			if cerr != nil || len(claims) != 1 {
				t.Fatalf("claims = %+v, err = %v", claims, cerr)
			}
		}
	}
}

func TestSecondStartRefusesLiveHerdrTabWithoutSnapshot(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	// Snapshot has no tabs, but live herdr has the tab.
	h.snap.Tabs = nil
	h.herdr.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123", AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	err := h.start(h.snap, okGate, applyOpt)
	var oe *OutcomeError
	if !errors.As(err, &oe) || oe.ExitStatus() != ExitRefused {
		t.Fatalf("err = %v", err)
	}
	if !strings.Contains(err.Error(), `tab "PRJ-123" already exists`) {
		t.Fatalf("err = %v", err)
	}
	assertCalls(t, h.log, []string{"Tabs"})
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestConcurrentStartsOneWins(t *testing.T) {
	t.Parallel()
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("close store: %v", err)
		}
	})

	it := jiraItem()
	src := &fake.Source{
		SourceName: "jira:acme",
		Account:    "ada",
		Items:      map[string]source.Item{it.ID: it},
	}

	makeStarter := func(out *bytes.Buffer, log *[]string) *Starter {
		return &Starter{
			Src:      &loggedSource{inner: src, log: log},
			Cfg:      jiraCfg(),
			Kind:     "jira",
			Defaults: config.Defaults{AgentKind: "claude"},
			Store:    st,
			Herdr: &fakeHerdr{
				log:   log,
				found: true,
				ws:    herdr.Workspace{ID: "w1", Label: "factory", AgentStatus: herdr.AgentStatusIdle},
				tab:   herdr.Tab{ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123", AgentStatus: herdr.AgentStatusIdle, PaneCount: 1},
				pane:  herdr.Pane{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1"},
			},
			Hostname: "laptop",
			Now:      func() time.Time { return fixedNow },
			Out:      out,
			Caller:   "w1:p9",
			Stash:    func(context.Context, string) (int, error) { return 0, nil },
			Sleep:    func(context.Context, time.Duration) error { return nil },
		}
	}

	snap := ready.Snapshot{
		Me:         "ada",
		Workspaces: []herdr.Workspace{{ID: "w1", Label: "factory", AgentStatus: herdr.AgentStatusIdle}},
		Items:      map[string]source.Item{it.ID: it},
	}

	var out1, out2 bytes.Buffer
	var log1, log2 []string
	s1 := makeStarter(&out1, &log1)
	s2 := makeStarter(&out2, &log2)

	readyCh := make(chan struct{})
	var wg sync.WaitGroup
	errs := make([]error, 2)
	for i, st := range []*Starter{s1, s2} {
		wg.Add(1)
		go func(idx int, starter *Starter) {
			defer wg.Done()
			<-readyCh
			errs[idx] = starter.Start(context.Background(), it.ID, snap, okGate, applyOpt)
		}(i, st)
	}
	close(readyCh)
	wg.Wait()

	var wins, refuses int
	for _, e := range errs {
		if e == nil {
			wins++
			continue
		}
		var oe *OutcomeError
		if errors.As(e, &oe) && oe.ExitStatus() == ExitRefused {
			refuses++
			continue
		}
		t.Fatalf("unexpected error: %v", e)
	}
	if wins != 1 || refuses != 1 {
		t.Fatalf("wins = %d, refuses = %d, errs = %v", wins, refuses, errs)
	}
	active, err := st.ActiveClaims(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(active) != 1 || active[0].State != store.Started {
		t.Fatalf("active claims = %+v", active)
	}
}

func TestLaunchFailureAfterNoTransitionClaimLeavesTrackerUntouched(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	// Item is already in claim_status and assigned to ada, so Claim posts no change.
	h.starter.Cfg.Claim = config.Claim{
		RequireLeaf:       true,
		RequireUnblocked:  true,
		SourceContainment: true,
		Assign:            true,
		OwnershipRule:     config.OwnershipUnassignedOrMe,
		MarkInProgress:    true,
		InProgressRule:    config.InProgressAllow,
		Status:            "In Progress",
	}
	it := jiraItem()
	it.State = "In Progress"
	it.Assignee = "ada"
	h.snap.Items[it.ID] = it
	h.src.inner.Items[it.ID] = it
	h.herdr.startErr = errors.New("agent down")

	err := h.start(h.snap, okGate, applyOpt)
	var oe *OutcomeError
	if !errors.As(err, &oe) || oe.ExitStatus() != ExitClaimKept {
		t.Fatalf("err = %v, want ExitClaimKept", err)
	}
	assertNoRelease(t, h.src)
	if len(h.herdr.closedTabs) != 1 || h.herdr.closedTabs[0] != "w1:t1" {
		t.Fatalf("closedTabs = %v", h.herdr.closedTabs)
	}
	got, gerr := h.st.LatestClaim(context.Background(), "jira:acme", "PRJ-123")
	if gerr != nil || got.State != store.Failed {
		t.Fatalf("claim = %+v, err = %v", got, gerr)
	}
	if live := h.src.inner.Items[it.ID]; live.State != "In Progress" || live.Assignee != "ada" {
		t.Fatalf("tracker item modified: %+v", live)
	}
}

func TestOutcomesAndJSON(t *testing.T) {
	t.Parallel()

	t.Run("started", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		if err := h.start(h.snap, okGate, Options{Apply: true, JSON: true}); err != nil {
			t.Fatal(err)
		}
		var got Outcome
		if err := json.Unmarshal(h.out.Bytes(), &got); err != nil {
			t.Fatalf("json = %q: %v", h.out.String(), err)
		}
		if got.Status != OutcomeStarted || got.ExitCode != ExitStarted || got.TabID != "w1:t1" || got.ClaimTime != "2026-11-01T12:00:00Z" {
			t.Fatalf("outcome = %+v", got)
		}
	})

	t.Run("refused", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		err := h.start(h.snap, gate.Result{OK: false, Reasons: []string{"held"}}, Options{Apply: true, JSON: true})
		var oe *OutcomeError
		if !errors.As(err, &oe) || oe.ExitStatus() != ExitRefused {
			t.Fatalf("err = %v", err)
		}
		var got Outcome
		if jerr := json.Unmarshal(h.out.Bytes(), &got); jerr != nil {
			t.Fatalf("json = %q: %v", h.out.String(), jerr)
		}
		if got.Status != OutcomeRefused || got.ExitCode != ExitRefused || !strings.Contains(got.Reason, "held") {
			t.Fatalf("outcome = %+v", got)
		}
	})

	t.Run("failed_claim_kept", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.herdr.promptErrs = []error{errors.New("prompt boom")}
		err := h.start(h.snap, okGate, Options{Apply: true, JSON: true})
		var oe *OutcomeError
		if !errors.As(err, &oe) || oe.ExitStatus() != ExitClaimKept {
			t.Fatalf("err = %v", err)
		}
		var got Outcome
		if jerr := json.Unmarshal(h.out.Bytes(), &got); jerr != nil {
			t.Fatalf("json = %q: %v", h.out.String(), jerr)
		}
		if got.Status != OutcomeFailedClaimKept || got.ExitCode != ExitClaimKept || !strings.Contains(got.NextStep, "factory release PRJ-123") {
			t.Fatalf("outcome = %+v", got)
		}
	})

	t.Run("failed_released", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.herdr.startErr = errors.New("agent down")
		err := h.start(h.snap, okGate, Options{Apply: true, JSON: true})
		var oe *OutcomeError
		if !errors.As(err, &oe) || oe.ExitStatus() != ExitReleased {
			t.Fatalf("err = %v", err)
		}
		var got Outcome
		if jerr := json.Unmarshal(h.out.Bytes(), &got); jerr != nil {
			t.Fatalf("json = %q: %v", h.out.String(), jerr)
		}
		if got.Status != OutcomeFailedReleased || got.ExitCode != ExitReleased || !strings.Contains(got.NextStep, "factory start PRJ-123 --apply") {
			t.Fatalf("outcome = %+v", got)
		}
	})
}
