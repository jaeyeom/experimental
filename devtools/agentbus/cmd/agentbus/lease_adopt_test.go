package main

import (
	"errors"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/checkout"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// leaseUserPane is a herdr pane that runs no agent: the user.
const leaseUserPane = "herdr:local/default/w1:p5"

// noProcs swaps adopt's process lister for one that sees nothing, so a
// test does not depend on what else runs on the machine.
func noProcs(t *testing.T) {
	t.Helper()
	old := listProcs
	listProcs = func() ([]checkout.Proc, error) { return nil, nil }
	t.Cleanup(func() { listProcs = old })
}

// dirty leaves a modified, a staged, and an untracked file in the repo.
func (w *leaseWorld) dirty(t *testing.T) {
	t.Helper()
	writeFile(t, filepath.Join(w.repo, "a.txt"), []byte("modified\n"))
	writeFile(t, filepath.Join(w.repo, "staged.txt"), []byte("staged\n"))
	w.git(t, w.repo, "add", "staged.txt")
	writeFile(t, filepath.Join(w.repo, "untracked.txt"), []byte("untracked\n"))
}

// assertCleanOnMain checks the checkout is clean on main.
func (w *leaseWorld) assertCleanOnMain(t *testing.T) {
	t.Helper()
	if branch := w.git(t, w.repo, "branch", "--show-current"); branch != "main" {
		t.Fatalf("branch = %q", branch)
	}
	if st := w.git(t, w.repo, "status", "--porcelain", "--untracked-files=all"); st != "" {
		t.Fatalf("checkout is not clean:\n%s", st)
	}
}

func (w *leaseWorld) backupRefs(t *testing.T) []string {
	t.Helper()
	return strings.Fields(w.git(t, w.repo, "for-each-ref", "--format=%(refname)", "refs/lease-backup/"))
}

func TestLeaseTakeoverThenAdoptSavesEverythingAndCleans(t *testing.T) {
	noProcs(t)
	w := newLeaseWorld(t)
	ticks := make(chan time.Time)
	w.cfg.LeaseTicks = ticks
	w.cfg.LeaseTiming.Takeover = lease.TakeoverOn
	// The test stops the daemon before it reads the deliveries.
	stop := sync.OnceFunc(startDaemon(t, w.dirs, w.cfg))
	defer stop()
	// dave's pane is gone; bob waits.
	w.mustLease(t, "p4", "claim", "--for", "40m")
	w.mustLease(t, "p2", "claim", "--for", "20m")
	w.git(t, w.repo, "switch", "-q", "-c", "feature")
	w.dirty(t)
	stashBefore := w.git(t, w.repo, "stash", "list")

	ticks <- time.Now()
	deadline := time.Now().Add(5 * time.Second)
	for !strings.Contains(w.mustLease(t, "p2", "status", "--json"), `"state": "needs-adopt"`) {
		if time.Now().After(deadline) {
			t.Fatal("no takeover after a tick")
		}
		time.Sleep(20 * time.Millisecond)
	}
	status := w.mustLease(t, "p2", "status", "--json")
	if !strings.Contains(status, `"handle": "`+leaseBob+`"`) || !strings.Contains(status, `"backup_ref": "refs/lease-backup/`) {
		t.Fatalf("status after takeover = %s", status)
	}
	if code, _, stderr := w.lease(t, "p2", "check"); code != 1 || !strings.Contains(stderr, "needs adopt") {
		t.Fatalf("check before adopt = %d %q", code, stderr)
	}
	if code, _, stderr := w.lease(t, "p3", "adopt"); code != 1 || !strings.Contains(stderr, "needs adopt by") {
		t.Fatalf("adopt by a non-adopter = %d %q", code, stderr)
	}
	if st := w.git(t, w.repo, "status", "--porcelain"); st == "" {
		t.Fatal("a refused adopt cleaned the tree")
	}

	out := w.mustLease(t, "p2", "adopt")
	refs := w.backupRefs(t)
	if len(refs) != 1 || !strings.HasSuffix(refs[0], "-herdr-local-default-w1-p4") {
		t.Fatalf("backup refs = %v", refs)
	}
	if !strings.Contains(out, "adopted local:"+w.repo+" on main") || !strings.Contains(out, "saved leftovers to "+refs[0]) {
		t.Fatalf("adopt output = %q", out)
	}
	for file, want := range map[string]string{"a.txt": "modified", "staged.txt": "staged", "untracked.txt": "untracked"} {
		if got := w.git(t, w.repo, "show", refs[0]+":"+file); got != want {
			t.Errorf("backup %s = %q, want %q", file, got, want)
		}
	}
	if msg := w.git(t, w.repo, "log", "-1", "--format=%B", refs[0]); !strings.Contains(msg, "branch: feature") {
		t.Errorf("backup message = %q", msg)
	}
	w.assertCleanOnMain(t)
	if got := w.git(t, w.repo, "stash", "list"); got != stashBefore {
		t.Fatalf("stash list changed: %q", got)
	}
	if code, _, _ := w.lease(t, "p2", "check"); code != 0 {
		t.Fatalf("check after adopt = %d", code)
	}
	text := w.mustLease(t, "p2", "status")
	if !strings.Contains(text, ": held") || !strings.Contains(text, "backups:\n  "+refs[0]) {
		t.Fatalf("status after adopt:\n%s", text)
	}
	if js := w.mustLease(t, "p2", "status", "--json"); strings.Contains(js, "backup_ref") {
		t.Fatalf("status --json after adopt still has backup_ref: %s", js)
	}
	events := strings.Join(w.leaseEvents(t), "\n")
	for _, want := range []string{eventlog.EventLeaseTakeover + " " + leaseDave, eventlog.EventLeaseAdopt + " " + leaseBob} {
		if !strings.Contains(events, want) {
			t.Errorf("events are missing %q:\n%s", want, events)
		}
	}
	stop()
	wake := strings.Join(w.ad.Texts, "\n")
	if !strings.Contains(wake, "as needs-adopt") || !strings.Contains(wake, "agentbus lease adopt") {
		t.Fatalf("adopter wake = %q", wake)
	}
}

// rebaseConflict leaves the repo stopped in a rebase of feature onto
// main.
func (w *leaseWorld) rebaseConflict(t *testing.T) {
	t.Helper()
	commit := func(text string) {
		writeFile(t, filepath.Join(w.repo, "a.txt"), []byte(text))
		w.git(t, w.repo, "commit", "-q", "-am", text)
	}
	w.git(t, w.repo, "switch", "-q", "-c", "feature")
	commit("feature\n")
	w.git(t, w.repo, "switch", "-q", "main")
	commit("main\n")
	w.git(t, w.repo, "switch", "-q", "feature")
	cmd := exec.Command("git", "-C", w.repo, "-c", "user.name=t", "-c", "user.email=t@example.com", "rebase", "main") //nolint:gosec // G204: test git in a temp repo
	cmd.Env = w.base
	if err := cmd.Run(); err == nil {
		t.Fatal("rebase did not stop")
	}
}

func TestLeaseAdoptRecordsAndAbortsARebase(t *testing.T) {
	noProcs(t)
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.rebaseConflict(t)
	stashBefore := w.git(t, w.repo, "stash", "list")
	if out := w.mustLease(t, "p1", "claim", "--for", "40m"); !strings.Contains(out, "needs-adopt") {
		t.Fatalf("claim of a mid-rebase tree = %q", out)
	}
	w.mustLease(t, "p1", "adopt")
	refs := w.backupRefs(t)
	if len(refs) == 0 {
		t.Fatal("no backup ref")
	}
	msg := w.git(t, w.repo, "log", "-1", "--format=%B", refs[0])
	if !strings.Contains(msg, "operation: rebase") || !strings.Contains(msg, "refs/heads/feature") {
		t.Fatalf("backup message does not record the rebase:\n%s", msg)
	}
	if _, err := os.Stat(filepath.Join(w.repo, ".git", "rebase-merge")); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("rebase still in progress: %v", err)
	}
	w.assertCleanOnMain(t)
	if got := w.git(t, w.repo, "stash", "list"); got != stashBefore {
		t.Fatalf("stash list changed: %q", got)
	}
}

func TestLeaseAdoptIsBlockedByASleepInTheCheckoutUntilForced(t *testing.T) {
	if _, err := exec.LookPath("sleep"); err != nil {
		t.Skip("sleep is not installed")
	}
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.dirty(t)
	w.mustLease(t, "p1", "claim", "--for", "40m")

	sleep := exec.Command("sleep", "60")
	sleep.Dir = w.repo
	if err := sleep.Start(); err != nil {
		t.Fatal(err)
	}
	defer func() {
		_ = sleep.Process.Kill()
		_ = sleep.Wait()
	}()
	// An interactive shell in the checkout does not block.
	if _, err := exec.LookPath("bash"); err == nil {
		shell := exec.Command("bash", "--noprofile", "--norc", "-i")
		shell.Dir = w.repo
		stdin, _ := shell.StdinPipe()
		shell.Stdout, shell.Stderr = io.Discard, io.Discard
		if err := shell.Start(); err != nil {
			t.Fatal(err)
		}
		defer func() {
			stdin.Close()
			_ = shell.Process.Kill()
			_ = shell.Wait()
		}()
	}

	label := "sleep (pid " + strconv.Itoa(sleep.Process.Pid) + ")"
	code, _, stderr := w.lease(t, "p1", "adopt")
	if code != 1 || !strings.Contains(stderr, label) || !strings.Contains(stderr, "--force") {
		t.Fatalf("adopt with a sleep = %d %q", code, stderr)
	}
	if strings.Contains(stderr, "bash") {
		t.Fatalf("an interactive bash blocked adopt: %q", stderr)
	}
	if status := w.mustLease(t, "p1", "status", "--json"); !strings.Contains(status, `"state": "needs-adopt"`) || !strings.Contains(status, leaseAlice) {
		t.Fatalf("status after a refused adopt = %s", status)
	}
	if st := w.git(t, w.repo, "status", "--porcelain"); st == "" || len(w.backupRefs(t)) != 0 {
		t.Fatalf("a refused adopt touched the tree: %q, refs %v", st, w.backupRefs(t))
	}

	code, stdout, stderr := w.lease(t, "p1", "adopt", "--force")
	if code != 0 || !strings.Contains(stderr, label) || !strings.Contains(stdout, "adopted ") {
		t.Fatalf("adopt --force = %d\nstdout %s\nstderr %s", code, stdout, stderr)
	}
	w.assertCleanOnMain(t)
	events := strings.Join(w.leaseEventDetails(t), "\n")
	if !strings.Contains(events, "forced past: "+label) {
		t.Fatalf("events do not record the force:\n%s", events)
	}
}

func TestLeaseAdoptByTheUserFromAPlainPane(t *testing.T) {
	noProcs(t)
	w := newLeaseWorld(t)
	w.ad.Reach[leaseUserPane] = adapter.StateNoAgent
	w.ad.PaneHandles["p5"] = leaseUserPane
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.dirty(t)
	w.mustLease(t, "p1", "claim", "--for", "40m")

	out := w.mustLease(t, "p5", "adopt")
	if !strings.Contains(out, "adopted ") {
		t.Fatalf("user adopt = %q", out)
	}
	status := w.mustLease(t, "p5", "status", "--json")
	if !strings.Contains(status, `"state": "held"`) || !strings.Contains(status, `"handle": "user"`) || !strings.Contains(status, `"handle": "`+leaseAlice+`"`) {
		t.Fatalf("status after the user adopted = %s", status)
	}
	w.assertCleanOnMain(t)
}

func TestLeaseAdoptOfAHeldCheckoutIsRefused(t *testing.T) {
	noProcs(t)
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	writeFile(t, filepath.Join(w.repo, "new.txt"), []byte("mine\n"))
	code, _, stderr := w.lease(t, "p1", "adopt")
	if code != 1 || !strings.Contains(stderr, "does not need adopt") {
		t.Fatalf("adopt of a held checkout = %d %q", code, stderr)
	}
	if _, err := os.Stat(filepath.Join(w.repo, "new.txt")); err != nil {
		t.Fatalf("a refused adopt removed work: %v", err)
	}
}

func (w *leaseWorld) leaseEventDetails(t *testing.T) []string {
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
		out = append(out, ev.Event+" "+ev.Actor+" "+ev.Detail+" "+ev.Reason)
	}
	return out
}
