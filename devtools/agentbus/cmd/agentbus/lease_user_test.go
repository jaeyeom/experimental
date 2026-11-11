package main

import (
	"encoding/json"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// userPane makes p5 the user's plain terminal pane.
func (w *leaseWorld) userPane() *leaseWorld {
	w.ad.Reach[leaseUserPane] = adapter.StateNoAgent
	w.ad.PaneHandles["p5"] = leaseUserPane
	return w
}

func TestLeasePauseIsRefusedFromAnAgentPaneAndGrantedFromAPlainOne(t *testing.T) {
	w := newLeaseWorld(t).userPane()
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	code, _, stderr := w.lease(t, "p1", "pause")
	if code != 1 || !strings.Contains(stderr, "is for the user") {
		t.Fatalf("pause from an agent pane = %d %q", code, stderr)
	}
	if code, _, stderr := w.lease(t, "p1", "resume"); code != 1 || !strings.Contains(stderr, "is for the user") {
		t.Fatalf("resume from an agent pane = %d %q", code, stderr)
	}
	out := w.mustLease(t, "p5", "pause")
	if !strings.Contains(out, "paused local:"+w.repo) {
		t.Fatalf("pause = %q", out)
	}
	status := w.mustLease(t, "p1", "status")
	if !strings.Contains(status, ": paused") || !strings.Contains(status, "holder: user") || strings.Contains(status, "bid ends") {
		t.Fatalf("status:\n%s", status)
	}
	if code, _, stderr := w.lease(t, "p1", "check"); code != 1 || !strings.Contains(stderr, "paused by the user") {
		t.Fatalf("check under the pause = %d %q", code, stderr)
	}
}

func TestLeasePauseWhileAnAgentHoldsAsksItToRelease(t *testing.T) {
	w := newLeaseWorld(t).userPane()
	stop := sync.OnceFunc(startDaemon(t, w.dirs, w.cfg))
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p2", "claim", "--for", "20m", "--urgent")
	out := w.mustLease(t, "p5", "pause")
	if !strings.Contains(out, "queued, position 1") || !strings.Contains(out, "asked "+leaseAlice) {
		t.Fatalf("pause = %q", out)
	}
	var st lease.Status
	if err := json.Unmarshal([]byte(w.mustLease(t, "p1", "status", "--json")), &st); err != nil {
		t.Fatal(err)
	}
	if st.Queue[0].Handle != lease.UserHolder || st.Queue[1].Handle != leaseBob {
		t.Fatalf("queue = %+v", st.Queue)
	}
	stop()
	if len(w.ad.Texts) != 1 || !strings.Contains(w.ad.Texts[0], lease.PauseAskText("local:"+w.repo)) {
		t.Fatalf("deliveries = %q", w.ad.Texts)
	}
}

func TestLeasePauseWithSharesTheHoldUntilUnshare(t *testing.T) {
	w := newLeaseWorld(t).userPane()
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p5", "pause", "--with", "p2")
	if code, _, _ := w.lease(t, "p2", "check"); code != 0 {
		t.Fatalf("check by p2 = %d", code)
	}
	if code, _, _ := w.lease(t, "p3", "check"); code != 1 {
		t.Fatalf("check by p3 = %d", code)
	}
	for _, sub := range [][]string{{"release"}, {"extend", "--for", "10m"}} {
		if code, _, stderr := w.lease(t, "p2", sub...); code != 1 || !strings.Contains(stderr, "only the user") {
			t.Fatalf("%s by p2 = %d %q", sub[0], code, stderr)
		}
	}
	if code, _, _ := w.lease(t, "p2", "resume"); code != 1 {
		t.Fatalf("resume by p2 = %d", code)
	}
	w.mustLease(t, "p5", "pause", "--with", "p3")
	js := w.mustLease(t, "p5", "status", "--json")
	var st lease.Status
	if err := json.Unmarshal([]byte(js), &st); err != nil || !slices.Equal(st.With, []string{leaseBob, leaseCarol}) {
		t.Fatalf("status --json = %s, %v", js, err)
	}
	if text := w.mustLease(t, "p5", "status"); !strings.Contains(text, "with: "+leaseBob+", "+leaseCarol) {
		t.Fatalf("status:\n%s", text)
	}
	if out := w.mustLease(t, "p5", "unshare", "p2"); !strings.Contains(out, "unshared") {
		t.Fatalf("unshare = %q", out)
	}
	if code, _, _ := w.lease(t, "p2", "check"); code != 1 {
		t.Fatalf("check by p2 after unshare = %d", code)
	}
	if code, _, stderr := w.lease(t, "p5", "unshare"); code != 2 || !strings.Contains(stderr, "name") {
		t.Fatalf("unshare of nobody = %d %q", code, stderr)
	}
	events := strings.Join(w.leaseEvents(t), "\n")
	for _, want := range []string{eventlog.EventLeasePause, eventlog.EventLeaseWith, eventlog.EventLeaseUnshare} {
		if !strings.Contains(events, want+" "+lease.UserHolder) {
			t.Errorf("events are missing %s:\n%s", want, events)
		}
	}
}

func TestLeaseResumeRefusesADirtyTree(t *testing.T) {
	w := newLeaseWorld(t).userPane()
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p5", "pause")
	writeFile(t, filepath.Join(w.repo, "a.txt"), []byte("changed\n"))
	for _, args := range [][]string{{"resume"}, {"resume", "--as-is"}} {
		code, _, stderr := w.lease(t, "p5", args...)
		if code != 1 || !strings.Contains(stderr, "a.txt") {
			t.Fatalf("%v with a dirty tree = %d %q", args, code, stderr)
		}
	}
	if status := w.mustLease(t, "p5", "status"); !strings.Contains(status, ": paused") {
		t.Fatalf("status after a refused resume:\n%s", status)
	}
}

func TestLeaseResumeAsIsLeavesTheBranchAndGrantsTheNextWaiter(t *testing.T) {
	w := newLeaseWorld(t).userPane()
	stop := sync.OnceFunc(startDaemon(t, w.dirs, w.cfg))
	defer stop()
	w.mustLease(t, "p5", "pause", "--with", "p3")
	w.mustLease(t, "p2", "claim", "--for", "20m")
	w.git(t, w.repo, "switch", "-q", "-c", "feature")
	out := w.mustLease(t, "p5", "resume", "--as-is")
	if !strings.Contains(out, "resumed local:"+w.repo+" on feature") || !strings.Contains(out, "granted to "+leaseBob) {
		t.Fatalf("resume --as-is = %q", out)
	}
	if branch := w.git(t, w.repo, "branch", "--show-current"); branch != "feature" {
		t.Fatalf("branch = %q", branch)
	}
	if code, _, _ := w.lease(t, "p2", "check"); code != 0 {
		t.Fatalf("check by the new holder = %d", code)
	}
	if code, _, _ := w.lease(t, "p3", "check"); code != 1 {
		t.Fatalf("check by a --with pane after resume = %d", code)
	}
	events := strings.Join(w.leaseEvents(t), "\n")
	if !strings.Contains(events, eventlog.EventLeaseResume+" "+lease.UserHolder) {
		t.Fatalf("events:\n%s", events)
	}
	stop()
	if len(w.ad.Texts) != 1 || !strings.Contains(w.ad.Texts[0], "[lease] granted local:"+w.repo) {
		t.Fatalf("deliveries = %q", w.ad.Texts)
	}
}

func TestLeaseResumeSwitchesToTheDefaultBranch(t *testing.T) {
	w := newLeaseWorld(t).userPane()
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p5", "pause")
	w.git(t, w.repo, "switch", "-q", "-c", "feature")
	out := w.mustLease(t, "p5", "resume")
	if !strings.Contains(out, "on main") || !strings.Contains(out, "the checkout is free") {
		t.Fatalf("resume = %q", out)
	}
	w.assertCleanOnMain(t)
}

func TestLeaseResumeAfterTheUserAdopted(t *testing.T) {
	noProcs(t)
	w := newLeaseWorld(t).userPane()
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.dirty(t)
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p5", "adopt")
	out := w.mustLease(t, "p5", "resume")
	if !strings.Contains(out, "granted to "+leaseAlice) {
		t.Fatalf("resume after adopt = %q", out)
	}
}

func TestLeaseResumeWhileQueuedWithdrawsThePause(t *testing.T) {
	w := newLeaseWorld(t).userPane()
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p5", "pause")
	writeFile(t, filepath.Join(w.repo, "a.txt"), []byte("alice's work\n"))
	out := w.mustLease(t, "p5", "resume")
	if !strings.Contains(out, "withdrew") {
		t.Fatalf("resume while queued = %q", out)
	}
	if status := w.mustLease(t, "p1", "status"); strings.Contains(status, "user") {
		t.Fatalf("status:\n%s", status)
	}
	if got := w.git(t, w.repo, "status", "--porcelain"); got == "" {
		t.Fatal("withdrawing the pause touched the tree")
	}
}
