package daemon

import (
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/leasecache"
)

// withCache gives the server a lease hook cache on the world's clock
// and caches pane as allowed on the checkout now.
func (w *checkinWorld) withCache(t *testing.T, pane string) *checkinWorld {
	t.Helper()
	w.srv.leaseCache = leasecache.Cache{Dir: t.TempDir(), Now: w.clock.Now}
	if err := w.srv.leaseCache.Put(ciKey, pane, w.clock.Now()); err != nil {
		t.Fatal(err)
	}
	if !w.srv.leaseCache.Allowed(ciKey, pane) {
		t.Fatal("cache did not take the entry")
	}
	return w
}

func TestTakeOverDropsTheHookCache(t *testing.T) {
	w := newCheckinWorld(t, true)
	// The check that cached alice ran just before its pane went.
	w.clock.Set(ciT0().Add(10*time.Minute - 10*time.Second))
	w.withCache(t, "p1")
	w.at(-30, adapter.StateGone)
	if s := w.status(t); s.State != lease.StateNeedsAdopt {
		t.Fatalf("state = %s", s.State)
	}
	if w.srv.leaseCache.Allowed(ciKey, "p1") {
		t.Fatal("the old holder is still cached after takeover")
	}
}

func TestReleaseDropsTheHookCache(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes().withCache(t, "p1")
	if r := w.call("lease-release-begin", "p1", lease.Args{}); !r.OK {
		t.Fatalf("release-begin = %+v", r)
	}
	if r := w.call("lease-release", "p1", lease.Args{}); !r.OK {
		t.Fatalf("release = %+v", r)
	}
	if w.srv.leaseCache.Allowed(ciKey, "p1") {
		t.Fatal("the holder is still cached after release")
	}
}

func TestUnshareAndResumeDropTheHookCache(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	if _, err := w.srv.leases.Release(ciAlice, lease.Args{Checkout: ciKey}, func(string) bool { return false }); err != nil {
		t.Fatal(err)
	}
	if r := w.call("lease-pause", "p5", lease.Args{With: []string{"p2", "p3"}}); !r.OK {
		t.Fatalf("pause = %+v", r)
	}
	w.withCache(t, "p2")
	if r := w.call("lease-unshare", "p5", lease.Args{With: []string{"p2"}}); !r.OK {
		t.Fatalf("unshare = %+v", r)
	}
	if w.srv.leaseCache.Allowed(ciKey, "p2") {
		t.Fatal("an unshared pane is still cached")
	}
	w.withCache(t, "p3")
	if r := w.call("lease-resume-begin", "p5", lease.Args{}); !r.OK {
		t.Fatalf("resume-begin = %+v", r)
	}
	if r := w.call("lease-resume", "p5", lease.Args{}); !r.OK {
		t.Fatalf("resume = %+v", r)
	}
	if w.srv.leaseCache.Allowed(ciKey, "p3") {
		t.Fatal("a pane the pause was shared with is still cached after resume")
	}
}
