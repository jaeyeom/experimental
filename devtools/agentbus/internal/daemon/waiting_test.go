package daemon

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

func TestQueuedClaimTellsTheHolderOnce(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	if r := w.call("lease-claim", "p2", lease.Args{For: "30m", Why: "x"}); !r.OK || r.Lease.Result != lease.ResultQueued {
		t.Fatalf("claim = %+v", r)
	}
	got := w.textsWith("waiting for " + ciKey)
	if len(got) != 1 || !strings.Contains(got[0], "1 waiting") || !strings.Contains(got[0], ciBob) || !strings.Contains(got[0], "(x, for 30m)") {
		t.Fatalf("notices = %q (all %q)", got, w.ad.Texts)
	}
	if evs := w.events(t, eventlog.EventLeaseNotifyWaiting); len(evs) != 1 || evs[0].Actor != ciAlice {
		t.Fatalf("events = %+v", evs)
	}

	w.call("lease-claim", "p2", lease.Args{For: "45m", Why: "x"})
	if got := w.textsWith("waiting for " + ciKey); len(got) != 1 {
		t.Fatalf("notices after a repeat claim = %q", got)
	}

	w.call("lease-claim", "p3", lease.Args{For: "10m"})
	got = w.textsWith("waiting for " + ciKey)
	if len(got) != 2 || !strings.Contains(got[1], "2 waiting") || !strings.Contains(got[1], ciCarol) {
		t.Fatalf("notices after carol = %q", got)
	}
}

func TestQueuedPauseSendsOnlyTheAsk(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	w.call("lease-pause", "p5", lease.Args{})
	if got := w.textsWith(lease.PauseAskText(ciKey)); len(got) != 1 {
		t.Fatalf("asks = %q", got)
	}
	if got := w.textsWith("waiting for " + ciKey); len(got) != 0 {
		t.Fatalf("notices = %q", got)
	}
}
