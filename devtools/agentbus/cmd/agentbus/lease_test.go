package main

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

const (
	leaseAlice = "herdr:local/default/w1:p1"
	leaseBob   = "herdr:local/default/w1:p2"
	leaseCarol = "herdr:local/default/w1:p3"
)

// leaseWorld is a temp git repo listed in lease_checkouts, a fake herdr
// with three panes, and the env to run agentbus as each pane.
type leaseWorld struct {
	repo string
	base []string
	dirs paths.Dirs
	cfg  daemon.Config
}

func newLeaseWorld(t *testing.T) *leaseWorld {
	t.Helper()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}
	repo, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	configHome := t.TempDir()
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"XDG_CONFIG_HOME=" + configHome,
		"HOME=" + t.TempDir(),
		"PATH=" + os.Getenv("PATH"),
		"GIT_CONFIG_NOSYSTEM=1",
		"GIT_CONFIG_GLOBAL=" + os.DevNull,
	}
	git := func(args ...string) {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", repo}, args...)...) //nolint:gosec // G204: test git in a temp repo
		cmd.Env = base
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %v\n%s", args, err, out)
		}
	}
	git("init", "-q", "-b", "main")
	writeFile(t, filepath.Join(repo, "a.txt"), []byte("a\n"))
	git("add", "a.txt")
	git("-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-q", "-m", "init")
	writeFile(t, filepath.Join(configHome, "agentbus", "config.toml"), []byte(`lease_checkouts = ["`+repo+`"]`+"\n"))

	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			leaseAlice: adapter.StateWorking,
			leaseBob:   adapter.StateWorking,
			leaseCarol: adapter.StateWorking,
		},
		PaneHandles: map[string]string{"p1": leaseAlice, "p2": leaseBob, "p3": leaseCarol},
	}
	now := time.Date(2026, 11, 10, 9, 0, 0, 0, time.UTC)
	return &leaseWorld{
		repo: repo,
		base: base,
		dirs: dirs,
		cfg:  daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour, Now: func() time.Time { return now }},
	}
}

func (w *leaseWorld) as(pane string) []string {
	return append(append([]string{}, w.base...), "HERDR_PANE_ID="+pane)
}

func (w *leaseWorld) lease(t *testing.T, pane string, args ...string) (int, string, string) {
	t.Helper()
	full := append([]string{"lease", args[0], "--path", w.repo}, args[1:]...)
	return runCmd(full, w.as(pane))
}

func (w *leaseWorld) mustLease(t *testing.T, pane string, args ...string) string {
	t.Helper()
	code, stdout, stderr := w.lease(t, pane, args...)
	if code != 0 {
		t.Fatalf("lease %v as %s = %d\nstdout %s\nstderr %s", args, pane, code, stdout, stderr)
	}
	return stdout
}

func TestLeaseClaimQueueLeaveExtendCheck(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()

	if out := w.mustLease(t, "p1", "claim", "--for", "40m", "--why", "issue 623"); !strings.HasPrefix(out, "granted until ") {
		t.Fatalf("first claim = %q", out)
	}
	if out := w.mustLease(t, "p2", "claim", "--for", "20m"); !strings.HasPrefix(out, "queued, position 1 (holder "+leaseAlice+", bid ends ") {
		t.Fatalf("second claim = %q", out)
	}
	if out := w.mustLease(t, "p3", "claim", "--for", "5m", "--urgent"); !strings.HasPrefix(out, "queued, position 1 ") {
		t.Fatalf("urgent claim = %q", out)
	}
	if out := w.mustLease(t, "p2", "claim", "--for", "25m"); !strings.HasPrefix(out, "queued, position 2 ") {
		t.Fatalf("reclaim = %q", out)
	}
	status := w.mustLease(t, "p1", "status")
	for _, want := range []string{"held", "why: issue 623", "1. " + leaseCarol + ", for 5m", "urgent", "2. " + leaseBob + ", for 25m"} {
		if !strings.Contains(status, want) {
			t.Errorf("status is missing %q:\n%s", want, status)
		}
	}

	if code, _, stderr := w.lease(t, "p2", "extend", "--for", "1h"); code != 1 || !strings.Contains(stderr, "only the holder") {
		t.Fatalf("non-holder extend = %d %q", code, stderr)
	}
	if out := w.mustLease(t, "p1", "extend", "--for", "1h"); !strings.HasPrefix(out, "extended until ") {
		t.Fatalf("extend = %q", out)
	}

	if code, _, stderr := w.lease(t, "p1", "check"); code != 0 || stderr != "" {
		t.Fatalf("holder check = %d %q", code, stderr)
	}
	code, _, stderr := w.lease(t, "p2", "check")
	if code != 1 || strings.Count(stderr, "\n") != 1 || !strings.Contains(stderr, "is held by "+leaseAlice) || !strings.Contains(stderr, "agentbus lease claim") {
		t.Fatalf("other check = %d %q", code, stderr)
	}

	w.mustLease(t, "p2", "leave")
	if status := w.mustLease(t, "p1", "status"); strings.Contains(status, leaseBob) {
		t.Fatalf("status after leave:\n%s", status)
	}
	if code, _, _ := w.lease(t, "p2", "leave"); code != 1 {
		t.Fatalf("second leave = %d", code)
	}
}

func TestLeaseCheckUnguardedPathMakesNoDaemonCall(t *testing.T) {
	w := newLeaseWorld(t)
	noDaemon(t)
	outside := t.TempDir()
	code, stdout, stderr := runCmd([]string{"lease", "check", "--path", outside}, w.as("p1"))
	if code != 0 || stdout != "" || stderr != "" {
		t.Fatalf("unguarded check = %d %q %q", code, stdout, stderr)
	}
	// A repository nested in a guarded checkout is not guarded either.
	nested := filepath.Join(w.repo, "nested")
	cmd := exec.Command("git", "init", "-q", nested)
	cmd.Env = w.base
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git init: %v\n%s", err, out)
	}
	code, stdout, _ = runCmd([]string{"lease", "check", "--json", "--path", filepath.Join(nested, "f.go")}, w.as("p1"))
	if code != 0 || !strings.Contains(stdout, `"allowed": true`) || !strings.Contains(stdout, `"guarded": false`) {
		t.Fatalf("nested check = %d %q", code, stdout)
	}
	// Without any config, every path is unguarded.
	code, _, _ = runCmd([]string{"lease", "check", "--path", w.repo}, []string{"HOME=" + t.TempDir()})
	if code != 0 {
		t.Fatalf("no-config check = %d", code)
	}
}

func TestLeaseRefusesAnUnguardedClaim(t *testing.T) {
	w := newLeaseWorld(t)
	noDaemon(t)
	code, _, stderr := runCmd([]string{"lease", "claim", "--for", "1m", "--path", t.TempDir()}, w.as("p1"))
	if code != 1 || !strings.Contains(stderr, "lease_checkouts") {
		t.Fatalf("unguarded claim = %d %q", code, stderr)
	}
	code, _, stderr = runCmd([]string{"lease", "claim", "--path", w.repo}, w.as("p1"))
	if code != 2 || !strings.Contains(stderr, "--for is required") {
		t.Fatalf("claim without --for = %d %q", code, stderr)
	}
}

func TestLeaseDirtyFreeCheckoutIsGrantedAsNeedsAdopt(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	writeFile(t, filepath.Join(w.repo, "a.txt"), []byte("changed\n"))
	out := w.mustLease(t, "p1", "claim", "--for", "40m")
	if !strings.Contains(out, "needs-adopt") {
		t.Fatalf("dirty claim = %q", out)
	}
	if status := w.mustLease(t, "p1", "status", "--json"); !strings.Contains(status, `"state": "needs-adopt"`) {
		t.Fatalf("status = %s", status)
	}
	if code, _, stderr := w.lease(t, "p1", "check"); code != 1 || !strings.Contains(stderr, "needs adopt") {
		t.Fatalf("needs-adopt check = %d %q", code, stderr)
	}
}

func TestLeaseStateSurvivesRestart(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p2", "claim", "--for", "20m")
	w.mustLease(t, "p3", "claim", "--for", "5m", "--urgent")
	before := w.mustLease(t, "p1", "status", "--json")
	stop()

	w.cfg.Now = func() time.Time { return time.Date(2026, 11, 10, 10, 0, 0, 0, time.UTC) }
	stop = startDaemon(t, w.dirs, w.cfg)
	defer stop()
	if after := w.mustLease(t, "p1", "status", "--json"); after != before {
		t.Fatalf("after restart\n%s\nwant\n%s", after, before)
	}
	if code, _, _ := w.lease(t, "p1", "check"); code != 0 {
		t.Fatalf("holder check after restart = %d", code)
	}
}

func TestLeaseStatusJSONGolden(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	mustCmd(t, []string{"register", "ops/alice", "--when", "testing the lease"}, w.as("p1"))
	w.mustLease(t, "p1", "claim", "--for", "40m", "--why", "issue 623")
	w.mustLease(t, "p2", "claim", "--for", "20m")
	w.mustLease(t, "p3", "claim", "--for", "1h30m", "--urgent")

	one := w.mustLease(t, "p1", "status", "--json")
	all, _ := mustCmd(t, []string{"lease", "status", "--all", "--json"}, w.as("p1"))
	got := strings.ReplaceAll(one+all, w.repo, "/home/u/src/repo")
	golden := filepath.Join("testdata", "lease_status.golden")
	if os.Getenv("UPDATE_GOLDEN") != "" {
		writeFile(t, golden, []byte(got))
	}
	want, err := os.ReadFile(golden)
	if err != nil {
		t.Fatal(err)
	}
	if got != string(want) {
		t.Fatalf("status --json =\n%s\nwant\n%s", got, want)
	}
}

func TestLeaseRefusesHubModeForAGuardedPath(t *testing.T) {
	w := newLeaseWorld(t)
	noDaemon(t)
	env := append(w.as("p1"), hubEnv+"=me@hub")
	code, _, stderr := runCmd([]string{"lease", "claim", "--for", "1m", "--path", w.repo}, env)
	if code != 1 || !strings.Contains(stderr, "hub mode") {
		t.Fatalf("hub claim = %d %q", code, stderr)
	}
	code, _, _ = runCmd([]string{"lease", "check", "--path", t.TempDir()}, env)
	if code != 0 {
		t.Fatalf("hub unguarded check = %d", code)
	}
}

func TestLeaseHelp(t *testing.T) {
	noDaemon(t)
	var stdout strings.Builder
	if code := cmdLease(context.Background(), nil, helpEnv, &stdout, &stdout); code != 0 {
		t.Fatalf("lease = %d", code)
	}
	for _, want := range []string{"claim --for", "--urgent", "extend", "leave", "status [--all]", "check", "--path", "--json"} {
		if !strings.Contains(stdout.String(), want) {
			t.Errorf("lease help is missing %q", want)
		}
	}
}
