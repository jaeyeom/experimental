package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func (w *leaseWorld) git(t *testing.T, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir, "-c", "user.name=t", "-c", "user.email=t@example.com"}, args...)...) //nolint:gosec // G204: test git in a temp repo
	cmd.Env = w.base
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v: %v\n%s", args, err, out)
	}
	return strings.TrimSpace(string(out))
}

// pushUpstream lands a commit adding file on origin's main from another
// clone.
func (w *leaseWorld) pushUpstream(t *testing.T, file string) {
	t.Helper()
	other := filepath.Join(t.TempDir(), "other")
	w.git(t, w.repo, "clone", "-q", w.origin, other)
	writeFile(t, filepath.Join(other, file), []byte("upstream\n"))
	w.git(t, other, "add", file)
	w.git(t, other, "commit", "-q", "-m", "upstream")
	w.git(t, other, "push", "-q", "origin", "main")
}

func (w *leaseWorld) leaseEvents(t *testing.T) []string {
	t.Helper()
	lines, err := eventlog.Open(w.dirs.StateDir).Read(eventlog.Filter{Name: "local:" + w.repo})
	if err != nil {
		t.Fatal(err)
	}
	var out []string
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		out = append(out, ev.Event+" "+ev.Actor)
	}
	return out
}

func TestLeaseReleaseSwitchesPullsAndWakesTheNextWaiter(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p2", "claim", "--for", "20m", "--why", "issue 624")

	// The holder works on a feature branch and leaves an unpushed commit.
	w.git(t, w.repo, "switch", "-q", "-c", "feature")
	writeFile(t, filepath.Join(w.repo, "f.txt"), []byte("f\n"))
	w.git(t, w.repo, "add", "f.txt")
	w.git(t, w.repo, "commit", "-q", "-m", "feature")
	w.pushUpstream(t, "up.txt")

	out := w.mustLease(t, "p1", "release")
	for _, want := range []string{"released local:" + w.repo, "on main", "granted to " + leaseBob} {
		if !strings.Contains(out, want) {
			t.Errorf("release output is missing %q:\n%s", want, out)
		}
	}
	if branch := w.git(t, w.repo, "branch", "--show-current"); branch != "main" {
		t.Fatalf("branch after release = %q", branch)
	}
	if _, err := os.Stat(filepath.Join(w.repo, "up.txt")); err != nil {
		t.Fatalf("release did not pull: %v", err)
	}
	status := w.mustLease(t, "p1", "status")
	if !strings.Contains(status, "held") || !strings.Contains(status, "holder: "+leaseBob) {
		t.Fatalf("status after release:\n%s", status)
	}
	if code, _, _ := w.lease(t, "p2", "check"); code != 0 {
		t.Fatalf("new holder check = %d", code)
	}
	if len(w.ad.Texts) != 1 {
		t.Fatalf("deliveries = %q", w.ad.Texts)
	}
	wake := w.ad.Texts[0]
	if !strings.Contains(wake, "[lease] granted local:"+w.repo+" until ") || !strings.Contains(wake, "(for: issue 624). Release with: agentbus lease release") {
		t.Fatalf("wake message = %q", wake)
	}
	events := strings.Join(w.leaseEvents(t), "\n")
	for _, want := range []string{eventlog.EventLeaseRelease + " " + leaseAlice, eventlog.EventLeaseGrant + " " + leaseBob} {
		if !strings.Contains(events, want) {
			t.Errorf("events are missing %q:\n%s", want, events)
		}
	}
}

func TestLeaseReleaseRefusesADirtyTree(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p2", "claim", "--for", "20m")
	writeFile(t, filepath.Join(w.repo, "a.txt"), []byte("changed\n"))
	writeFile(t, filepath.Join(w.repo, "new.txt"), []byte("new\n"))

	code, _, stderr := w.lease(t, "p1", "release")
	if code != 1 || !strings.Contains(stderr, "not clean") || !strings.Contains(stderr, "a.txt") || !strings.Contains(stderr, "new.txt") {
		t.Fatalf("dirty release = %d %q", code, stderr)
	}
	if status := w.mustLease(t, "p1", "status"); !strings.Contains(status, "held") || !strings.Contains(status, "holder: "+leaseAlice) {
		t.Fatalf("status after refused release:\n%s", status)
	}
	if len(w.ad.Texts) != 0 {
		t.Fatalf("deliveries = %q", w.ad.Texts)
	}
}

func TestLeaseReleaseRefusesAMidRebaseTree(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	if err := os.Mkdir(filepath.Join(w.repo, ".git", "rebase-merge"), 0o755); err != nil {
		t.Fatal(err)
	}
	code, _, stderr := w.lease(t, "p1", "release")
	if code != 1 || !strings.Contains(stderr, "rebase in progress") {
		t.Fatalf("mid-rebase release = %d %q", code, stderr)
	}
	if status := w.mustLease(t, "p1", "status"); !strings.Contains(status, ": held") {
		t.Fatalf("status after refused release:\n%s", status)
	}
}

func TestLeaseReleaseStillReleasesWhenThePullFails(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p2", "claim", "--for", "20m")
	if err := os.Rename(w.origin, w.origin+".unreachable"); err != nil {
		t.Fatal(err)
	}

	code, stdout, stderr := w.lease(t, "p1", "release")
	if code != 0 || !strings.Contains(stderr, "pull failed") || !strings.Contains(stdout, "granted to "+leaseBob) {
		t.Fatalf("release with a failed pull = %d\nstdout %s\nstderr %s", code, stdout, stderr)
	}
	if status := w.mustLease(t, "p1", "status"); !strings.Contains(status, "holder: "+leaseBob) {
		t.Fatalf("status after release:\n%s", status)
	}
}

func TestLeaseReleaseSkipsAGoneWaiter(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p4", "claim", "--for", "20m")
	w.mustLease(t, "p3", "claim", "--for", "10m")

	out := w.mustLease(t, "p1", "release")
	if !strings.Contains(out, "skipped "+leaseDave+" (pane gone)") || !strings.Contains(out, "granted to "+leaseCarol) {
		t.Fatalf("release = %q", out)
	}
	status := w.mustLease(t, "p1", "status")
	if !strings.Contains(status, "holder: "+leaseCarol) || strings.Contains(status, leaseDave) {
		t.Fatalf("status after release:\n%s", status)
	}
	events := strings.Join(w.leaseEvents(t), "\n")
	if !strings.Contains(events, eventlog.EventLeaseSkip+" "+leaseDave) {
		t.Fatalf("events are missing the skip:\n%s", events)
	}
}

func TestLeaseReleaseWithNoWaiterFreesTheCheckout(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	out := w.mustLease(t, "p1", "release", "--json")
	if !strings.Contains(out, `"result": "released"`) || !strings.Contains(out, `"state": "free"`) {
		t.Fatalf("release --json = %s", out)
	}
}

func TestLeaseReleaseByANonHolderIsRefused(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p2", "claim", "--for", "20m")
	w.git(t, w.repo, "switch", "-q", "-c", "feature")

	code, _, stderr := w.lease(t, "p2", "release")
	if code != 1 || !strings.Contains(stderr, "only the holder") {
		t.Fatalf("non-holder release = %d %q", code, stderr)
	}
	if branch := w.git(t, w.repo, "branch", "--show-current"); branch != "feature" {
		t.Fatalf("a refused release switched the branch to %q", branch)
	}
	if status := w.mustLease(t, "p1", "status"); !strings.Contains(status, ": held") || !strings.Contains(status, "holder: "+leaseAlice) {
		t.Fatalf("status after refused release:\n%s", status)
	}
}

func TestLeaseReleaseWithoutOriginHeadKeepsTheHold(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.git(t, w.repo, "remote", "set-head", "origin", "--delete")

	code, _, stderr := w.lease(t, "p1", "release")
	if code != 1 || !strings.Contains(stderr, "origin/HEAD") {
		t.Fatalf("release without origin/HEAD = %d %q", code, stderr)
	}
	if status := w.mustLease(t, "p1", "status"); !strings.Contains(status, ": held") {
		t.Fatalf("status after a failed switch:\n%s", status)
	}
}
