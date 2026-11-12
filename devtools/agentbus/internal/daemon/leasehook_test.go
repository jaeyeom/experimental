package daemon

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

func hookArgs(tool, target string) lease.Args {
	return lease.Args{Hook: true, Tool: tool, Target: target}
}

func TestLeaseCheckFromTheHookLogsADenial(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	resp := w.call("lease-check", "p2", hookArgs("Edit", "sub/a.txt"))
	if !resp.OK || resp.Lease.Result != lease.ResultDenied {
		t.Fatalf("bob's hook check = %+v", resp)
	}
	evs := w.events(t, eventlog.EventLeaseHookDeny)
	if len(evs) != 1 {
		t.Fatalf("lease_hook_deny events = %+v, want one", evs)
	}
	ev := evs[0]
	if ev.Name != ciKey || ev.Actor != ciBob {
		t.Fatalf("event = %+v", ev)
	}
	for _, want := range []string{"tool Edit", "target sub/a.txt", "holder " + ciAlice, "state held"} {
		if !strings.Contains(ev.Detail, want) {
			t.Errorf("detail %q lacks %q", ev.Detail, want)
		}
	}
	if strings.Contains(ev.Detail, "machine") {
		t.Errorf("a local caller's detail names a machine: %q", ev.Detail)
	}
}

func TestLeaseCheckFromTheHookLogsNoAllow(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	resp := w.call("lease-check", "p1", hookArgs("Bash", "git switch"))
	if !resp.OK || resp.Lease.Result != lease.ResultAllowed {
		t.Fatalf("alice's hook check = %+v", resp)
	}
	if evs := w.events(t, eventlog.EventLeaseHookDeny); len(evs) != 0 {
		t.Fatalf("an allow logged %+v", evs)
	}
}

func TestPlainLeaseCheckLogsNoDenial(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	resp := w.call("lease-check", "p2", lease.Args{})
	if !resp.OK || resp.Lease.Result != lease.ResultDenied {
		t.Fatalf("bob's plain check = %+v", resp)
	}
	if evs := w.events(t, eventlog.EventLeaseHookDeny); len(evs) != 0 {
		t.Fatalf("a plain check logged %+v", evs)
	}
}

func TestLeaseHookDenialSurvivesAFailingEventLog(t *testing.T) {
	w := newCheckinWorld(t, false).withPanes()
	blocker := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(blocker, nil, 0o600); err != nil {
		t.Fatal(err)
	}
	w.srv.log = eventlog.Open(blocker)
	resp := w.call("lease-check", "p2", hookArgs("Edit", "a.txt"))
	if !resp.OK || resp.Lease.Result != lease.ResultDenied || resp.Lease.Lease == nil || resp.Lease.Lease.Holder.Handle != ciAlice {
		t.Fatalf("hook check with a broken log = %+v", resp)
	}
}

func TestHubLogsTheCallersMachineOnAHookDenial(t *testing.T) {
	herdrtest.Install(t).Write("local.agents", `{"result":{"agents":[]}}`)
	dirs := hubDirs(t)
	ad := &herdr.Adapter{Machines: []string{"linux2"}, Timeout: time.Second}
	stop := startWith(t, dirs, Config{Adapters: []adapter.Adapter{ad}, HubCaller: herdr.RemoteCaller})
	defer stop()

	const linux2 = "herdr:linux2/default/w1:p1"
	args := hookArgs("Edit", "a.txt")
	args.Checkout = "linux2:/x"
	resp := call(t, dirs.HubSocketPath, Request{Op: "lease-check", Protocol: ProtocolVersion, Handle: linux2, Lease: &args})
	if !resp.OK || resp.Lease.Result != lease.ResultDenied {
		t.Fatalf("hook check by linux2 = %+v", resp)
	}
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Name: "linux2:/x", Event: eventlog.EventLeaseHookDeny})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) != 1 {
		t.Fatalf("lease_hook_deny lines = %q, want one", lines)
	}
	ev, err := eventlog.Parse(lines[0])
	if err != nil {
		t.Fatal(err)
	}
	if ev.Actor != linux2 {
		t.Fatalf("event = %+v", ev)
	}
	for _, want := range []string{"machine linux2", "tool Edit", "target a.txt", "holder none", "state free"} {
		if !strings.Contains(ev.Detail, want) {
			t.Errorf("detail %q lacks %q", ev.Detail, want)
		}
	}
}
