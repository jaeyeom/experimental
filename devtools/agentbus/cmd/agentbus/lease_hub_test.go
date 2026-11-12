package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall/sshtest"
)

const (
	hubAlice = "herdr:linux2/default/w1:p1"
	hubBob   = "herdr:linux2/default/w1:p2"
)

// hubLeaseWorld is a guarded temp repo on linux2, a machine in hub mode,
// with a fake ssh to the hub and a fake herdr that answers both for
// linux2's own panes and for the hub's --machine linux2 calls.
type hubLeaseWorld struct {
	*leaseWorld
	ssh    *sshtest.Fake
	herdr  *herdrtest.Fake
	tunnel string
}

func newHubLeaseWorld(t *testing.T, configLines ...string) *hubLeaseWorld {
	t.Helper()
	sshFake := sshtest.Install(t)
	sshFake.InstallTool("agentbus")
	herdrFake := herdrtest.Install(t)
	herdrFake.Write("local.agents", agentsJSON("w1:p1=idle", "w1:p2=idle"))
	herdrFake.Write("linux2.agents", agentsJSON("w1:p1=idle", "w1:p2=idle"))
	w := newLeaseWorld(t)
	configHome := envGet(w.base, "XDG_CONFIG_HOME")
	lines := append([]string{`hub = "me@hub"`, `machine = "linux2"`, `lease_checkouts = ["` + w.repo + `"]`}, configLines...)
	writeFile(t, filepath.Join(configHome, "agentbus", "config.toml"), []byte(strings.Join(lines, "\n")+"\n"))
	return &hubLeaseWorld{leaseWorld: w, ssh: sshFake, herdr: herdrFake, tunnel: w.dirs.TunnelSocketPath}
}

// startHubDaemon runs the hub's daemon with its hub socket at linux2's
// tunnel path and returns the hub's own environment.
func (h *hubLeaseWorld) startHubDaemon(t *testing.T) []string {
	t.Helper()
	return h.startHub(t, h.tunnel)
}

// startHubOverSSH runs the hub's daemon with the tunnel down and puts
// this test binary first on PATH as the hub's agentbus, so the fake ssh
// runs the real `agentbus lease forward` against that daemon, in the
// hub's own environment, as an ssh login would.
func (h *hubLeaseWorld) startHubOverSSH(t *testing.T) []string {
	t.Helper()
	hubEnv := h.startHub(t, "")
	h.ssh.Reachable()
	exe, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	words := []string{"exec", "env", "-i", `PATH="$PATH"`, `AGENTBUS_AS_HANDLE="$AGENTBUS_AS_HANDLE"`, "AGENTBUS_TEST_HOOK=1", hubcall.Quote("SSH_CONNECTION=10.0.0.2 5000 10.0.0.1 22")}
	for _, kv := range hubEnv {
		words = append(words, hubcall.Quote(kv))
	}
	words = append(words, hubcall.Quote(exe), `"$@"`)
	dir := t.TempDir()
	script := "#!/bin/sh\n" + strings.Join(words, " ") + "\n"
	if err := os.WriteFile(filepath.Join(dir, "agentbus"), []byte(script), 0o755); err != nil { //nolint:gosec // test executable
		t.Fatal(err)
	}
	t.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	return hubEnv
}

// startHub runs the hub's daemon, with its hub socket at hubSocket when
// set, and returns the hub's own environment.
func (h *hubLeaseWorld) startHub(t *testing.T, hubSocket string) []string {
	t.Helper()
	env := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	if hubSocket != "" {
		dirs.HubSocketPath = hubSocket
	}
	ad := &herdr.Adapter{Machines: []string{"linux2"}, Timeout: 5 * time.Second}
	stop := startDaemon(t, dirs, daemon.Config{
		Adapters:     []adapter.Adapter{ad},
		PollInterval: time.Hour,
		HubCaller:    herdr.RemoteCaller,
	})
	t.Cleanup(stop)
	return env
}

func (h *hubLeaseWorld) usedSSH() bool {
	return h.ssh.Read("argv") != ""
}

// hubStatus is the hub's status of every checkout.
func hubStatus(t *testing.T, hubEnv []string) []lease.Status {
	t.Helper()
	out, _ := mustCmd(t, []string{"lease", "status", "--all", "--json"}, hubEnv)
	var st []lease.Status
	if err := json.Unmarshal([]byte(out), &st); err != nil {
		t.Fatalf("status --all --json = %s: %v", out, err)
	}
	return st
}

func TestLeaseHubTunnelClaimAndReleaseRunGitHere(t *testing.T) {
	h := newHubLeaseWorld(t)
	hubEnv := h.startHubDaemon(t)
	key := "linux2:" + h.repo

	if out := h.mustLease(t, "w1:p1", "claim", "--for", "40m"); !strings.HasPrefix(out, "granted until ") {
		t.Fatalf("claim = %q", out)
	}
	if out := h.mustLease(t, "w1:p2", "claim", "--for", "20m"); !strings.HasPrefix(out, "queued, position 1") {
		t.Fatalf("second claim = %q", out)
	}
	st := hubStatus(t, hubEnv)
	if len(st) != 1 || st[0].Checkout != key || st[0].Holder == nil || st[0].Holder.Handle != hubAlice {
		t.Fatalf("hub status = %+v", st)
	}

	h.git(t, h.repo, "switch", "-q", "-c", "feature")
	writeFile(t, filepath.Join(h.repo, "f.txt"), []byte("f\n"))
	h.git(t, h.repo, "add", "f.txt")
	h.git(t, h.repo, "commit", "-q", "-m", "feature")
	h.pushUpstream(t, "up.txt")

	out := h.mustLease(t, "w1:p1", "release")
	for _, want := range []string{"switched feature -> main\n", "pulled: ", "released; granted to " + hubBob} {
		if !strings.Contains(out, want) {
			t.Errorf("release output is missing %q:\n%s", want, out)
		}
	}
	if branch := h.git(t, h.repo, "branch", "--show-current"); branch != "main" {
		t.Fatalf("branch after release = %q", branch)
	}
	if _, err := os.Stat(filepath.Join(h.repo, "up.txt")); err != nil {
		t.Fatalf("release did not pull here: %v", err)
	}
	if st := hubStatus(t, hubEnv); st[0].Holder == nil || st[0].Holder.Handle != hubBob {
		t.Fatalf("hub status after release = %+v", st)
	}
	if h.usedSSH() {
		t.Fatalf("a tunnelled lease ran ssh: %q", h.ssh.Lines("argv"))
	}
}

func TestLeaseHubPauseFromAnAgentPaneIsRefusedOverTheTunnel(t *testing.T) {
	h := newHubLeaseWorld(t)
	h.startHubDaemon(t)
	code, _, stderr := h.lease(t, "w1:p1", "pause")
	if code != 1 || !strings.Contains(stderr, "is for the user") {
		t.Fatalf("pause from an agent pane = %d %q", code, stderr)
	}
}

// plainPane makes w1:p3 a plain terminal pane, where no agent runs, as
// both linux2's herdr and the hub's --machine linux2 calls see it.
func (h *hubLeaseWorld) plainPane() *hubLeaseWorld {
	plain := strings.Replace(agentsJSON("w1:p1=idle", "w1:p2=idle", "w1:p3=idle"), `"agent":"codex","agent_status":"idle","terminal_title":"title w1:p3"`, `"agent":"","agent_status":"","terminal_title":"title w1:p3"`, 1)
	h.herdr.Write("local.agents", plain)
	h.herdr.Write("linux2.agents", plain)
	return h
}

// hubRoutes are the two ways a lease command on linux2 reaches the hub:
// the tunnel, and ssh to `agentbus lease forward` with the tunnel down.
var hubRoutes = []struct {
	name  string
	start func(h *hubLeaseWorld, t *testing.T) []string
	ssh   bool
}{
	{"tunnel", (*hubLeaseWorld).startHubDaemon, false},
	{"ssh", (*hubLeaseWorld).startHubOverSSH, true},
}

func (h *hubLeaseWorld) assertRoute(t *testing.T, ssh bool) {
	t.Helper()
	if h.usedSSH() != ssh {
		t.Fatalf("used ssh = %v, want %v: %q", h.usedSSH(), ssh, h.ssh.Lines("argv"))
	}
	if ssh && h.ssh.Read("agentbus.args") != "" {
		t.Fatalf("ssh ran the fake hub agentbus, not the real one")
	}
}

func TestLeaseHubPauseFromAPlainPaneOverSSH(t *testing.T) {
	h := newHubLeaseWorld(t).plainPane()
	hubEnv := h.startHubOverSSH(t)
	if out := h.mustLease(t, "w1:p3", "pause", "--with", "w1:p2"); !strings.Contains(out, "paused linux2:"+h.repo) {
		t.Fatalf("pause = %q", out)
	}
	st := hubStatus(t, hubEnv)
	if st[0].Holder == nil || st[0].Holder.Handle != lease.UserHolder || !slices.Equal(st[0].With, []string{hubBob}) {
		t.Fatalf("hub status = %+v", st)
	}
	h.assertRoute(t, true)
}

func TestLeaseHubResumeFromAPlainPane(t *testing.T) {
	for _, route := range hubRoutes {
		t.Run(route.name, func(t *testing.T) {
			h := newHubLeaseWorld(t).plainPane()
			hubEnv := route.start(h, t)
			h.mustLease(t, "w1:p3", "pause")
			if out := h.mustLease(t, "w1:p2", "claim", "--for", "20m"); !strings.HasPrefix(out, "queued, position 1") {
				t.Fatalf("claim under the pause = %q", out)
			}
			h.git(t, h.repo, "switch", "-q", "-c", "feature")
			h.pushUpstream(t, "up.txt")

			out := h.mustLease(t, "w1:p3", "resume")
			for _, want := range []string{"switched feature -> main\n", "pulled: ", "resumed; granted to " + hubBob} {
				if !strings.Contains(out, want) {
					t.Errorf("resume output is missing %q:\n%s", want, out)
				}
			}
			if branch := h.git(t, h.repo, "branch", "--show-current"); branch != "main" {
				t.Fatalf("branch after resume = %q", branch)
			}
			if _, err := os.Stat(filepath.Join(h.repo, "up.txt")); err != nil {
				t.Fatalf("resume did not pull here: %v", err)
			}
			if st := hubStatus(t, hubEnv); st[0].Holder == nil || st[0].Holder.Handle != hubBob {
				t.Fatalf("hub status after resume = %+v", st)
			}

			// With nobody waiting, the checkout becomes free.
			h.mustLease(t, "w1:p2", "release")
			h.mustLease(t, "w1:p3", "pause")
			if out := h.mustLease(t, "w1:p3", "resume"); !strings.Contains(out, "resumed; checkout free") {
				t.Fatalf("resume with nobody waiting = %q", out)
			}
			if st := hubStatus(t, hubEnv); st[0].State != lease.StateFree || st[0].Holder != nil {
				t.Fatalf("hub status after the second resume = %+v", st)
			}
			h.assertRoute(t, route.ssh)
		})
	}
}

func TestLeaseHubAdoptFromAPlainPane(t *testing.T) {
	noProcs(t)
	for _, route := range hubRoutes {
		t.Run(route.name, func(t *testing.T) {
			h := newHubLeaseWorld(t).plainPane()
			hubEnv := route.start(h, t)
			h.dirty(t)
			if out := h.mustLease(t, "w1:p1", "claim", "--for", "40m"); !strings.Contains(out, "needs-adopt") {
				t.Fatalf("claim of a dirty tree = %q", out)
			}

			if out := h.mustLease(t, "w1:p3", "adopt"); !strings.Contains(out, "adopted linux2:"+h.repo) {
				t.Fatalf("user adopt = %q", out)
			}
			st := hubStatus(t, hubEnv)
			if st[0].State != lease.StateHeld || st[0].Holder == nil || st[0].Holder.Handle != lease.UserHolder || len(st[0].Queue) == 0 || st[0].Queue[0].Handle != hubAlice {
				t.Fatalf("hub status after the user adopted = %+v", st)
			}
			h.assertCleanOnMain(t)
			if len(h.backupRefs(t)) == 0 {
				t.Fatal("adopt saved no backup ref here")
			}
			h.assertRoute(t, route.ssh)
		})
	}
}

func TestLeaseHubUserOpsFromAnAgentPaneAreRefused(t *testing.T) {
	noProcs(t)
	for _, route := range hubRoutes {
		t.Run(route.name, func(t *testing.T) {
			h := newHubLeaseWorld(t)
			hubEnv := route.start(h, t)
			for _, op := range []string{"pause", "resume"} {
				if code, _, stderr := h.lease(t, "w1:p1", op); code != 1 || !strings.Contains(stderr, "lease "+op+" is for the user") {
					t.Fatalf("%s from an agent pane = %d %q", op, code, stderr)
				}
			}
			h.dirty(t)
			h.mustLease(t, "w1:p1", "claim", "--for", "40m")
			// bob is not the assigned adopter, and not the user.
			if code, _, stderr := h.lease(t, "w1:p2", "adopt"); code != 1 || !strings.Contains(stderr, "needs adopt by "+hubAlice) {
				t.Fatalf("adopt by another agent = %d %q", code, stderr)
			}
			if st := h.git(t, h.repo, "status", "--porcelain"); st == "" || len(h.backupRefs(t)) != 0 {
				t.Fatalf("a refused adopt touched the tree: %q, refs %v", st, h.backupRefs(t))
			}
			// alice, the assigned adopter, may adopt.
			if out := h.mustLease(t, "w1:p1", "adopt"); !strings.Contains(out, "adopted ") {
				t.Fatalf("adopt by the adopter = %q", out)
			}
			if st := hubStatus(t, hubEnv); st[0].State != lease.StateHeld || st[0].Holder == nil || st[0].Holder.Handle != hubAlice {
				t.Fatalf("hub status after the adopter adopted = %+v", st)
			}
			h.assertRoute(t, route.ssh)
		})
	}
}

func TestLeaseHubPauseFromAPlainPaneOverTheTunnel(t *testing.T) {
	h := newHubLeaseWorld(t).plainPane()
	hubEnv := h.startHubDaemon(t)

	// A pane id the CLI cannot place here is not looked up on the hub.
	code, _, stderr := h.lease(t, "w1:p3", "pause", "--with", "w9:p9")
	if code != 1 || !strings.Contains(stderr, "--with w9:p9") {
		t.Fatalf("pause --with an unknown pane = %d %q", code, stderr)
	}
	if out := h.mustLease(t, "w1:p3", "pause", "--with", "w1:p2"); !strings.Contains(out, "paused linux2:"+h.repo) {
		t.Fatalf("pause = %q", out)
	}
	st := hubStatus(t, hubEnv)
	if st[0].Holder == nil || st[0].Holder.Handle != lease.UserHolder || !slices.Equal(st[0].With, []string{hubBob}) {
		t.Fatalf("hub status = %+v", st)
	}
	if code, _, _ := h.lease(t, "w1:p2", "check"); code != checkAllowed {
		t.Fatalf("check by the shared pane = %d", code)
	}
	if h.usedSSH() {
		t.Fatalf("a tunnelled pause ran ssh: %q", h.ssh.Lines("argv"))
	}
}

func TestLeaseHubHookEnforcesOverTheTunnel(t *testing.T) {
	h := newHubLeaseWorld(t)
	h.startHubDaemon(t)
	h.mustLease(t, "w1:p1", "claim", "--for", "40m")
	file := filepath.Join(h.repo, "a.txt")

	decision, reason, _ := h.edit(t, "w1:p2", file)
	assertDenied(t, "a non-holder's edit", decision, reason)
	if !strings.Contains(reason, "held by "+hubAlice) {
		t.Fatalf("reason = %q", reason)
	}
	decision, reason, stderr := h.edit(t, "w1:p1", file)
	assertAllowed(t, "the holder's edit", decision, reason)
	if stderr != "" {
		t.Fatalf("holder's hook stderr = %q", stderr)
	}
	if h.usedSSH() {
		t.Fatalf("the hook ran ssh: %q", h.ssh.Lines("argv"))
	}
}

// The hub logs a hook denial with the caller's machine, over the
// tunnel and over ssh.
func TestLeaseHubHookDenialNamesTheCallersMachine(t *testing.T) {
	for _, route := range hubRoutes {
		h := newHubLeaseWorld(t)
		hubEnv := route.start(h, t)
		decision, reason, _ := h.bash(t, "w1:p2", "git switch -c x", h.repo)
		assertDenied(t, route.name+": a free checkout's git switch", decision, reason)
		h.assertRoute(t, route.ssh)
		hubDirs, err := paths.FromEnv(hubEnv)
		if err != nil {
			t.Fatal(err)
		}
		evs := hookDenials(t, hubDirs.StateDir, "linux2:"+h.repo)
		if len(evs) != 1 || evs[0].Actor != hubBob {
			t.Fatalf("%s: lease_hook_deny events = %+v, want one by %s", route.name, evs, hubBob)
		}
		for _, want := range []string{"machine linux2", "tool Bash", "target git switch", "state free"} {
			if !strings.Contains(evs[0].Detail, want) {
				t.Errorf("%s: detail %q lacks %q", route.name, evs[0].Detail, want)
			}
		}
	}
}

// cannedReply makes the fake hub agentbus answer every call with r.
func (h *hubLeaseWorld) cannedReply(t *testing.T, r lease.Reply) {
	t.Helper()
	data, err := json.Marshal(daemon.Response{OK: true, Lease: &r})
	if err != nil {
		t.Fatal(err)
	}
	h.ssh.Write("agentbus.stdout", string(data)+"\n")
}

// forwarded is the request the fake hub agentbus last read on stdin.
func (h *hubLeaseWorld) forwarded(t *testing.T) daemon.Request {
	t.Helper()
	var req daemon.Request
	if err := json.Unmarshal([]byte(h.ssh.Read("agentbus.stdin")), &req); err != nil {
		t.Fatalf("hub stdin %q: %v", h.ssh.Read("agentbus.stdin"), err)
	}
	return req
}

func TestLeaseHubSSHReleaseRunsGitHereAndSendsOnlyFacts(t *testing.T) {
	h := newHubLeaseWorld(t)
	h.ssh.Reachable()
	key := "linux2:" + h.repo
	h.cannedReply(t, lease.Reply{Result: lease.ResultReleased, Lease: &lease.Status{Checkout: key, State: lease.StateFree}})
	h.git(t, h.repo, "switch", "-q", "-c", "feature")
	// A pull that cannot reach origin is a fact the hub records.
	h.git(t, h.repo, "remote", "set-url", "origin", filepath.Join(t.TempDir(), "missing.git"))

	code, out, stderr := h.lease(t, "w1:p1", "release")
	if code != 0 || !strings.Contains(out, "switched feature -> main\npull failed: ") || !strings.Contains(out, "released; checkout free") {
		t.Fatalf("release = %d\nstdout %s\nstderr %s", code, out, stderr)
	}
	if branch := h.git(t, h.repo, "branch", "--show-current"); branch != "main" {
		t.Fatalf("branch after release = %q", branch)
	}
	if got := h.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"lease", "forward"}) {
		t.Fatalf("hub args = %q", got)
	}
	if remote := h.ssh.Lines("argv"); len(remote) == 0 || strings.Contains(remote[len(remote)-1], "git") {
		t.Fatalf("the hub was asked to run %q", remote)
	}
	req := h.forwarded(t)
	if req.Op != "lease-release" || req.Lease == nil || req.Lease.Checkout != key || req.Lease.PullError == "" || len(req.Env) != 0 {
		t.Fatalf("forwarded request = %+v", req)
	}
	if got := h.ssh.Read("agentbus.handle"); got != hubAlice {
		t.Fatalf("hub caller = %q", got)
	}
}

func TestLeaseHubSSHClaimSendsTheTreeAndTheMachineKey(t *testing.T) {
	h := newHubLeaseWorld(t)
	h.ssh.Reachable()
	key := "linux2:" + h.repo
	h.cannedReply(t, lease.Reply{Result: lease.ResultGranted, Lease: &lease.Status{
		Checkout: key, State: lease.StateNeedsAdopt,
		Holder: &lease.HolderView{Handle: hubAlice, Deadline: "2026-11-10T09:40:00Z"},
	}})
	writeFile(t, filepath.Join(h.repo, "a.txt"), []byte("dirty\n"))
	out := h.mustLease(t, "w1:p1", "claim", "--for", "40m")
	if !strings.Contains(out, "needs-adopt") {
		t.Fatalf("claim = %q", out)
	}
	req := h.forwarded(t)
	if req.Op != "lease-claim" || req.Lease.Checkout != key || req.Lease.For != "40m" || req.Lease.Tree == "" {
		t.Fatalf("forwarded request = %+v", req)
	}
}

func TestLeaseHubPauseWithNamesThisMachinesPanes(t *testing.T) {
	h := newHubLeaseWorld(t)
	h.ssh.Reachable()
	key := "linux2:" + h.repo
	h.cannedReply(t, lease.Reply{Result: lease.ResultPaused, Lease: &lease.Status{Checkout: key, State: lease.StatePaused}})
	// The user, outside herdr.
	code, _, stderr := runCmd([]string{"lease", "pause", "--path", h.repo, "--with", "w1:p2", "--with", "ops/x"}, h.base)
	if code != 0 {
		t.Fatalf("pause = %d %q", code, stderr)
	}
	req := h.forwarded(t)
	if req.Op != "lease-pause" || !slices.Equal(req.Lease.With, []string{hubBob, "ops/x"}) {
		t.Fatalf("forwarded request = %+v", req)
	}
	if got := h.ssh.Read("agentbus.handle"); got != "" {
		t.Fatalf("the user's pause carried caller %q", got)
	}
}

func TestLeaseHubUnguardedCheckMakesNoSSHCall(t *testing.T) {
	h := newHubLeaseWorld(t)
	code, _, stderr := runCmd([]string{"lease", "check", "--path", t.TempDir()}, h.as("w1:p1"))
	if code != checkAllowed || h.usedSSH() {
		t.Fatalf("unguarded check = %d %q, ssh %q", code, stderr, h.ssh.Lines("argv"))
	}
	decision, reason, _ := h.edit(t, "w1:p1", filepath.Join(t.TempDir(), "x.txt"))
	assertAllowed(t, "an unguarded edit", decision, reason)
	if h.usedSSH() {
		t.Fatalf("an unguarded hook ran ssh: %q", h.ssh.Lines("argv"))
	}
}

func TestLeaseHubUnreachable(t *testing.T) {
	tests := []struct {
		name  string
		setup func(h *hubLeaseWorld)
		cause string
	}{
		{"down", func(h *hubLeaseWorld) { h.ssh.Write("down", ""); h.ssh.Unreachable() }, "host unreachable"},
		{"locked", func(h *hubLeaseWorld) { h.ssh.Write("locked", ""); h.ssh.Reachable() }, "authentication locked"},
		{"hang", func(h *hubLeaseWorld) { h.ssh.Write("hang", ""); h.ssh.Reachable() }, "authentication locked"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := newHubLeaseWorld(t, `remote_timeout = "1s"`)
			tt.setup(h)

			start := time.Now()
			code, _, stderr := h.lease(t, "w1:p1", "check")
			if took := time.Since(start); took > 3500*time.Millisecond {
				t.Fatalf("check took %s", took)
			}
			if code != checkAllowed || !strings.Contains(stderr, tt.cause) || !strings.Contains(stderr, "allowing") {
				t.Fatalf("check = %d %q", code, stderr)
			}

			start = time.Now()
			decision, reason, stderr := h.edit(t, "w1:p1", filepath.Join(h.repo, "a.txt"))
			if took := time.Since(start); took > 3500*time.Millisecond {
				t.Fatalf("hook took %s", took)
			}
			assertAllowed(t, "an edit with the hub "+tt.name, decision, reason)
			if !strings.Contains(stderr, tt.cause) || !strings.Contains(stderr, "allowing") {
				t.Fatalf("hook stderr = %q", stderr)
			}

			if tt.name == "hang" {
				return
			}
			code, _, stderr = h.lease(t, "w1:p1", "claim", "--for", "40m")
			if code != 1 || !strings.Contains(stderr, "hub me@hub unreachable: "+tt.cause) {
				t.Fatalf("claim = %d %q", code, stderr)
			}
		})
	}
}

func TestLeaseForwardRunsOnTheHubDaemon(t *testing.T) {
	h := newHubLeaseWorld(t)
	hubEnv := h.startHubDaemon(t)
	remote := append(append([]string{}, hubEnv...), "SSH_CONNECTION=10.0.0.2 5000 10.0.0.1 22", herdr.AsHandleEnv+"="+hubAlice)
	forward := func(env []string, req string) daemon.Response {
		t.Helper()
		code, out, stderr := runCmdIn([]string{"lease", "forward"}, env, strings.NewReader(req))
		if code != 0 {
			t.Fatalf("forward = %d %q", code, stderr)
		}
		var resp daemon.Response
		if err := json.Unmarshal([]byte(out), &resp); err != nil {
			t.Fatalf("forward stdout %q: %v", out, err)
		}
		return resp
	}
	resp := forward(remote, `{"op":"lease-claim","lease":{"checkout":"linux2:/src/repo","for":"40m"}}`)
	if !resp.OK || resp.Lease.Result != lease.ResultGranted || resp.Lease.Caller != hubAlice {
		t.Fatalf("forwarded claim = %+v", resp)
	}
	// AGENTBUS_AS_HANDLE is trusted only over ssh.
	local := append(append([]string{}, hubEnv...), herdr.AsHandleEnv+"="+hubBob)
	if resp := forward(local, `{"op":"lease-claim","lease":{"checkout":"linux2:/src/repo","for":"40m"}}`); resp.OK {
		t.Fatalf("claim without ssh = %+v", resp)
	}
	code, _, stderr := runCmdIn([]string{"lease", "forward"}, remote, strings.NewReader(`{"op":"send"}`))
	if code != 1 || !strings.Contains(stderr, "lease-") {
		t.Fatalf("forward of send = %d %q", code, stderr)
	}
}

func TestLeaseForwardRefusesAnotherMachinesCheckout(t *testing.T) {
	// w1:p3 is a plain terminal pane: its caller is the user.
	h := newHubLeaseWorld(t).plainPane()
	hubEnv := h.startHubDaemon(t)
	forward := func(handle, req string) daemon.Response {
		t.Helper()
		env := append(append([]string{}, hubEnv...), "SSH_CONNECTION=10.0.0.2 5000 10.0.0.1 22", herdr.AsHandleEnv+"="+handle)
		code, out, stderr := runCmdIn([]string{"lease", "forward"}, env, strings.NewReader(req))
		if code != 0 {
			t.Fatalf("forward = %d %q", code, stderr)
		}
		var resp daemon.Response
		if err := json.Unmarshal([]byte(out), &resp); err != nil {
			t.Fatalf("forward stdout %q: %v", out, err)
		}
		return resp
	}
	if resp := forward(hubAlice, `{"op":"lease-claim","lease":{"checkout":"laptop:/x","for":"40m"}}`); resp.OK || !strings.Contains(resp.Error, "not on machine linux2") {
		t.Fatalf("claim of laptop:/x = %+v", resp)
	}
	if resp := forward(hubAlice, `{"op":"lease-claim","lease":{"checkout":"linux2:/x","for":"40m"}}`); !resp.OK || resp.Lease.Result != lease.ResultGranted {
		t.Fatalf("claim of linux2:/x = %+v", resp)
	}
	// The user in a plain pane on linux2 is held to linux2's checkouts too.
	if resp := forward("herdr:linux2/default/w1:p3", `{"op":"lease-pause","lease":{"checkout":"laptop:/x"}}`); resp.OK || !strings.Contains(resp.Error, "not on machine linux2") {
		t.Fatalf("pause of laptop:/x from a plain pane = %+v", resp)
	}
	if resp := forward("herdr:laptop/default/w1:p1", `{"op":"lease-claim","lease":{"checkout":"laptop:/x","for":"40m"}}`); !resp.OK || resp.Lease.Result != lease.ResultGranted {
		t.Fatalf("laptop's claim of laptop:/x = %+v", resp)
	}
	resp := forward(hubAlice, `{"op":"lease-status","lease":{"all":true}}`)
	if !resp.OK || len(resp.Lease.Leases) != 2 || resp.Lease.Leases[0].Checkout != "laptop:/x" || resp.Lease.Leases[1].Checkout != "linux2:/x" {
		t.Fatalf("status --all from linux2 = %+v", resp)
	}
}
