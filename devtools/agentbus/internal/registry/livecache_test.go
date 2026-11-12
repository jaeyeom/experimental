package registry

import (
	"context"
	"errors"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
)

// slowAdapter is a fake whose liveness calls can sleep, block, or fail,
// and are counted.
type slowAdapter struct {
	*fake.Adapter
	mu     sync.Mutex
	delay  time.Duration
	gate   chan struct{}
	failOn string
	states atomic.Int32
	invs   atomic.Int32
}

func (a *slowAdapter) wait() {
	a.mu.Lock()
	delay, gate := a.delay, a.gate
	a.mu.Unlock()
	time.Sleep(delay)
	if gate != nil {
		<-gate
	}
}

func (a *slowAdapter) set(fn func()) {
	a.mu.Lock()
	defer a.mu.Unlock()
	fn()
}

func (a *slowAdapter) State(ctx context.Context, handle string) (adapter.State, error) {
	a.states.Add(1)
	a.wait()
	a.mu.Lock()
	failOn := a.failOn
	a.mu.Unlock()
	if failOn != "" && strings.HasPrefix(handle, failOn) {
		return "", errors.New("machine unreachable")
	}
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.Adapter.State(ctx, handle) //nolint:wrapcheck // a test fake passes the fake through
}

func (a *slowAdapter) Inventory(ctx context.Context) ([]adapter.Agent, error) {
	a.invs.Add(1)
	a.wait()
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.Adapter.Inventory(ctx) //nolint:wrapcheck // a test fake passes the fake through
}

type clock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *clock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *clock) advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = c.now.Add(d)
}

const (
	localHandle  = "herdr:local/default/w1:p1"
	remoteHandle = "herdr:linux2/default/w1:p2"
)

// openSlow opens a registry with ops/local and ops/remote registered
// and the liveness cache filled by a fresh list.
func openSlow(t *testing.T) (*Registry, *slowAdapter, *clock) {
	t.Helper()
	ad := &slowAdapter{Adapter: &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{localHandle: adapter.StateIdle, remoteHandle: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": localHandle, "p2": remoteHandle},
	}}
	reg, err := Open(t.TempDir(), []adapter.Adapter{ad}, nil)
	if err != nil {
		t.Fatal(err)
	}
	clk := &clock{now: time.Date(2026, 11, 12, 9, 0, 0, 0, time.UTC)}
	reg.Now = clk.Now
	ctx := context.Background()
	if _, err := reg.Register(ctx, envPane("p1"), "ops/local", "local work"); err != nil {
		t.Fatal(err)
	}
	if _, err := reg.Register(ctx, envPane("p2"), "ops/remote", "remote work"); err != nil {
		t.Fatal(err)
	}
	if _, err := reg.List(ctx); err != nil {
		t.Fatal(err)
	}
	return reg, ad, clk
}

func livenessOf(rows []Row) map[string]string {
	out := map[string]string{}
	for _, row := range rows {
		out[row.Name] = row.Liveness
	}
	return out
}

// waitRefreshed waits until no background liveness refresh runs.
func waitRefreshed(t *testing.T, reg *Registry) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for time.Now().Before(deadline) {
		reg.mu.Lock()
		busy := reg.liveBusy
		reg.mu.Unlock()
		if !busy {
			return
		}
		time.Sleep(time.Millisecond)
	}
	t.Fatal("background refresh did not finish")
}

func TestListCachedAnswersWithoutAdapterCalls(t *testing.T) {
	reg, ad, clk := openSlow(t)
	ad.set(func() { ad.delay = time.Second })
	ad.states.Store(0)
	ad.invs.Store(0)
	clk.advance(3 * time.Second)

	start := time.Now()
	rows := reg.ListCached(context.Background())
	if elapsed := time.Since(start); elapsed > 100*time.Millisecond {
		t.Fatalf("cached list took %s", elapsed)
	}
	if got := livenessOf(rows); got["ops/local"] != "alive" || got["ops/remote"] != "alive" {
		t.Fatalf("cached liveness = %v", got)
	}
	for _, row := range rows {
		if age := clk.Now().Sub(row.Observed); age != 3*time.Second {
			t.Fatalf("%s observed %s ago, want 3s", row.Name, age)
		}
	}
	if s, i := ad.states.Load(), ad.invs.Load(); s != 0 || i != 0 {
		t.Fatalf("cached list made %d state and %d inventory calls", s, i)
	}
}

func TestListCachedStartsOneBackgroundRefreshWhenStale(t *testing.T) {
	reg, ad, clk := openSlow(t)
	gate := make(chan struct{})
	ad.set(func() {
		ad.gate = gate
		ad.Reach[localHandle] = adapter.StateBlocked
	})
	ad.states.Store(0)
	clk.advance(DefaultListMaxAge + time.Second)

	var wg sync.WaitGroup
	for range 2 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			rows := reg.ListCached(context.Background())
			if got := livenessOf(rows); got["ops/local"] != "alive" {
				t.Errorf("stale list liveness = %v, want the cached alive", got)
			}
		}()
	}
	wg.Wait()
	close(gate)
	waitRefreshed(t, reg)

	// One refresh probes each of the two handles once.
	if n := ad.states.Load(); n != 2 {
		t.Fatalf("state calls = %d, want 2 from one refresh", n)
	}
	rows := reg.ListCached(context.Background())
	if got := livenessOf(rows); got["ops/local"] != "blocked" {
		t.Fatalf("refreshed liveness = %v", got)
	}
	for _, row := range rows {
		if !row.Observed.Equal(clk.Now()) {
			t.Fatalf("%s observed at %s, want %s", row.Name, row.Observed, clk.Now())
		}
	}
	if n := ad.states.Load(); n != 2 {
		t.Fatalf("a fresh cache started another refresh: %d state calls", n)
	}
}

func TestListFreshProbesAndUpdatesTheCache(t *testing.T) {
	reg, ad, clk := openSlow(t)
	ad.set(func() { ad.Reach[localHandle] = adapter.StateGone })
	ad.states.Store(0)
	clk.advance(time.Second)

	rows, err := reg.List(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if ad.states.Load() == 0 {
		t.Fatal("fresh list made no state calls")
	}
	if got := livenessOf(rows); got["ops/local"] != "gone" {
		t.Fatalf("fresh liveness = %v", got)
	}
	cached := reg.ListCached(context.Background())
	if got := livenessOf(cached); got["ops/local"] != "gone" {
		t.Fatalf("cache after fresh list = %v", got)
	}
	if !cached[0].Observed.Equal(clk.Now()) {
		t.Fatalf("observed = %s, want %s", cached[0].Observed, clk.Now())
	}
}

func TestBackgroundRefreshMarksOnlyTheUnreachableMachine(t *testing.T) {
	reg, ad, clk := openSlow(t)
	ad.set(func() { ad.failOn = "herdr:linux2/" })
	clk.advance(DefaultListMaxAge + time.Second)

	reg.ListCached(context.Background())
	waitRefreshed(t, reg)
	got := livenessOf(reg.ListCached(context.Background()))
	if got["ops/remote"] != "unreachable" || got["ops/local"] != "alive" {
		t.Fatalf("liveness = %v, want only ops/remote unreachable", got)
	}
}

func TestInventoryAndDeliveryUpdateTheCache(t *testing.T) {
	reg, ad, clk := openSlow(t)
	ad.set(func() { ad.Reach[localHandle] = adapter.StateBlocked })
	clk.advance(time.Second)
	reg.pollOnce(context.Background())
	got := livenessOf(reg.ListCached(context.Background()))
	if got["ops/local"] != "blocked" {
		t.Fatalf("after inventory = %v", got)
	}

	clk.advance(time.Second)
	reg.ObserveLiveness(remoteHandle, "unreachable")
	rows := reg.ListCached(context.Background())
	if got := livenessOf(rows); got["ops/remote"] != "unreachable" {
		t.Fatalf("after delivery = %v", got)
	}
	for _, row := range rows {
		if row.Name == "ops/remote" && !row.Observed.Equal(clk.Now()) {
			t.Fatalf("observed = %s, want %s", row.Observed, clk.Now())
		}
	}
}

func TestListCachedNeverObservedIsUnreachable(t *testing.T) {
	reg := openReg(t, &fake.Adapter{AdapterName: "herdr"})
	reg.mu.Lock()
	reg.names["ops/old"] = record{Handle: "herdr:local/default/w9:p9", When: "old"}
	reg.mu.Unlock()
	rows := reg.ListCached(context.Background())
	if len(rows) != 1 || rows[0].Liveness != "unreachable" || !rows[0].Observed.IsZero() {
		t.Fatalf("rows = %+v", rows)
	}
	waitRefreshed(t, reg)
}
