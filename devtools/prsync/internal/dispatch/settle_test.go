package dispatch

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

func TestWaitForSettleIgnoresPreSendIdle(t *testing.T) {
	t.Parallel()

	baseline := seqAgent("w2:pC", "w2:tC", "idle", 1)
	h := &scriptHerdr{lists: [][]herdr.Agent{{baseline}}}
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}

	_, err := waitForSettle(context.Background(), h, "w2:pC", baseline, []string{"idle", "done"}, 50*time.Millisecond, time.Millisecond, clock, sleeper)
	if !errors.Is(err, errSettleTimeout) {
		t.Fatalf("error = %v, want errSettleTimeout (pre-send idle is not settled)", err)
	}
}

func TestWaitForSettleFlapDoesNotSettleBeforeWorking(t *testing.T) {
	t.Parallel()

	baseline := seqAgent("w2:pC", "w2:tC", "idle", 1)
	h := &scriptHerdr{lists: [][]herdr.Agent{
		{seqAgent("w2:pC", "w2:tC", "idle", 1)},
		{seqAgent("w2:pC", "w2:tC", "blocked", 2)},
		{seqAgent("w2:pC", "w2:tC", "idle", 3)},
		{seqAgent("w2:pC", "w2:tC", "idle", 3)},
		{seqAgent("w2:pC", "w2:tC", "idle", 3)},
		{seqAgent("w2:pC", "w2:tC", "blocked", 4)},
		{seqAgent("w2:pC", "w2:tC", "working", 5)},
		{seqAgent("w2:pC", "w2:tC", "idle", 6)},
		{seqAgent("w2:pC", "w2:tC", "idle", 6)},
		{seqAgent("w2:pC", "w2:tC", "idle", 6)},
	}}
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}

	got, err := waitForSettle(context.Background(), h, "w2:pC", baseline, []string{"idle", "done"}, time.Second, time.Millisecond, clock, sleeper)
	if err != nil {
		t.Fatalf("waitForSettle() unexpected error: %v", err)
	}
	if got.AgentStatus != "idle" || got.StateChangeSeq != 6 {
		t.Fatalf("settled = %+v, want idle seq 6 after working", got)
	}
	if h.n < 8 {
		t.Fatalf("AgentList calls = %d, want flap idle samples skipped until working", h.n)
	}
}

func TestWaitForSettleSeqAdvanceWithoutBlockedIsEnough(t *testing.T) {
	t.Parallel()

	baseline := seqAgent("w2:pC", "w2:tC", "idle", 1)
	settled := seqAgent("w2:pC", "w2:tC", "idle", 4)
	h := &scriptHerdr{lists: [][]herdr.Agent{
		{settled},
		{settled},
		{settled},
	}}
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}

	got, err := waitForSettle(context.Background(), h, "w2:pC", baseline, []string{"idle", "done"}, time.Second, time.Millisecond, clock, sleeper)
	if err != nil {
		t.Fatalf("waitForSettle() unexpected error: %v", err)
	}
	if got.AgentStatus != "idle" || got.StateChangeSeq != 4 {
		t.Fatalf("settled = %+v, want idle seq 4 (missed working)", got)
	}
}

func TestWaitForSettleDebouncesSingleIdleAfterWorking(t *testing.T) {
	t.Parallel()

	baseline := seqAgent("w2:pC", "w2:tC", "idle", 1)
	h := &scriptHerdr{lists: [][]herdr.Agent{
		{seqAgent("w2:pC", "w2:tC", "working", 2)},
		{seqAgent("w2:pC", "w2:tC", "idle", 3)},
		{seqAgent("w2:pC", "w2:tC", "working", 4)},
		{seqAgent("w2:pC", "w2:tC", "idle", 5)},
		{seqAgent("w2:pC", "w2:tC", "idle", 5)},
		{seqAgent("w2:pC", "w2:tC", "idle", 5)},
	}}
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}

	got, err := waitForSettle(context.Background(), h, "w2:pC", baseline, []string{"idle", "done"}, time.Second, time.Millisecond, clock, sleeper)
	if err != nil {
		t.Fatalf("waitForSettle() unexpected error: %v", err)
	}
	if got.StateChangeSeq != 5 {
		t.Fatalf("settled seq = %d, want 5 (single idle after working is not enough)", got.StateChangeSeq)
	}
}

func TestWaitForSettleBaselineWorkingHonorsBlockedUntil(t *testing.T) {
	t.Parallel()

	// herdr --until idle can return after the working sample; the settle
	// baseline is then already working and the next polls are blocked.
	baseline := seqAgent("w2:pC", "w2:tC", "working", 2)
	blocked := seqAgent("w2:pC", "w2:tC", "blocked", 3)
	h := &scriptHerdr{lists: [][]herdr.Agent{
		{blocked},
		{blocked},
		{blocked},
	}}
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}

	got, err := waitForSettle(context.Background(), h, "w2:pC", baseline, []string{"idle", "done", "blocked"}, time.Second, time.Millisecond, clock, sleeper)
	if err != nil {
		t.Fatalf("waitForSettle() unexpected error: %v", err)
	}
	if got.AgentStatus != "blocked" {
		t.Fatalf("settled status = %q, want blocked (baseline working arms the watch)", got.AgentStatus)
	}
}

func TestWaitForSettleBlockedUntilIsDispatchedBlocked(t *testing.T) {
	t.Parallel()

	baseline := seqAgent("w2:pC", "w2:tC", "idle", 1)
	blocked := seqAgent("w2:pC", "w2:tC", "blocked", 3)
	h := &scriptHerdr{lists: [][]herdr.Agent{
		{seqAgent("w2:pC", "w2:tC", "working", 2)},
		{blocked},
		{blocked},
		{blocked},
	}}
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}

	got, err := waitForSettle(context.Background(), h, "w2:pC", baseline, []string{"idle", "done", "blocked"}, time.Second, time.Millisecond, clock, sleeper)
	if err != nil {
		t.Fatalf("waitForSettle() unexpected error: %v", err)
	}
	if got.AgentStatus != "blocked" {
		t.Fatalf("settled status = %q, want blocked", got.AgentStatus)
	}
}

func TestWaitForSettleRevisionWhenSeqAbsent(t *testing.T) {
	t.Parallel()

	baseline := herdr.Agent{PaneID: "w2:pC", TabID: "w2:tC", AgentStatus: "idle", Revision: 10}
	settled := herdr.Agent{PaneID: "w2:pC", TabID: "w2:tC", AgentStatus: "idle", Revision: 11}
	h := &scriptHerdr{lists: [][]herdr.Agent{
		{settled},
		{settled},
		{settled},
	}}
	clock := &fakeClock{now: fixtureNow}
	sleeper := &fakeSleeper{clock: clock}

	got, err := waitForSettle(context.Background(), h, "w2:pC", baseline, []string{"idle", "done"}, time.Second, time.Millisecond, clock, sleeper)
	if err != nil {
		t.Fatalf("waitForSettle() unexpected error: %v", err)
	}
	if got.Revision != 11 {
		t.Fatalf("settled revision = %d, want 11", got.Revision)
	}
}
