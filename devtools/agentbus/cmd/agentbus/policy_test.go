package main

import (
	"bytes"
	"context"
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall/sshtest"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// policyPanes are three claude panes on the client "mac".
var policyPanes = []string{"w1:p1", "w1:p4", "w1:p5"}

// policyAgents is the local agent list with the given panes idle and
// the rest working.
func policyAgents(idle ...string) string {
	rows := make([]string, 0, len(policyPanes))
	for _, pane := range policyPanes {
		status := "working"
		if slices.Contains(idle, pane) {
			status = "idle"
		}
		session := "sess-" + strings.TrimPrefix(pane, "w1:p")
		rows = append(rows, `{"pane_id":"`+pane+`","tab_id":"w1:t1","agent":"claude","agent_status":"`+status+`","agent_session":{"value":"`+session+`"}}`)
	}
	return `{"result":{"agents":[` + strings.Join(rows, ",") + `]}}`
}

// policyMem is the client's available memory: base plus 400 MiB per
// hibernation the fake hub has seen ended.
type policyMem struct {
	base atomic.Int64
	bus  *hibBus
}

func (m *policyMem) Available(context.Context) (int64, error) {
	ends := 0
	for _, op := range m.bus.ops() {
		if op == "hibernate-end" {
			ends++
		}
	}
	return m.base.Load() + int64(ends)*400*mib, nil
}

// setupPolicy is a hub-mode client "mac" with hibernate_policy on
// under 1 GiB until 1.5 GiB, three idle-capable claude panes, and a
// fake hub on the tunnel that has never polled this machine: its
// hibernate-state has names but no seen times.
func setupPolicy(t *testing.T) (*herdrtest.Fake, *hibBus, *policyMem, func(time.Time), []string) {
	t.Helper()
	fake, bus, env := setupHibernate(t)
	writeFile(t, filepath.Join(envGet(env, "HOME"), ".config", "agentbus", "config.toml"), []byte(
		"hub = \"me@hub\"\nmachine = \"mac\"\nhibernate_policy = \"on\"\n"+
			"hibernate_below = \"1GiB\"\nhibernate_target = \"1.5GiB\"\nhibernate_min_idle = \"30m\"\n"))
	bus.names = map[string][]string{
		"herdr:mac/sess-1/w1:p1": {"ops/a"},
		"herdr:mac/sess-4/w1:p4": {"ops/b"},
		"herdr:mac/sess-5/w1:p5": {"ops/c"},
	}
	bus.seen = nil
	fake.Write("local.agents", policyAgents())
	for i, pane := range policyPanes {
		pid := 100 * (i + 1)
		fake.Write("local.process-info."+pane, processInfoJSON(pane, pid))
		fake.Write("local.after-keys."+pane, processInfoJSON(pane, 1))
	}
	hibernateProcs = func(context.Context) ([]procinfo.Process, error) {
		return []procinfo.Process{
			p(1, 0, 3*mib, 5*time.Hour, "-zsh"),
			p(100, 1, 400*mib, 3*time.Hour, "claude"),
			p(200, 1, 400*mib, 3*time.Hour, "claude"),
			p(300, 1, 400*mib, 3*time.Hour, "claude"),
		}, nil
	}
	mem := &policyMem{bus: bus}
	mem.base.Store(4 * 1024 * mib)
	ticks, ticked := make(chan time.Time), make(chan struct{})
	oldMem, oldTicks, oldTicked := policyMemory, policyTicks, policyTicked
	t.Cleanup(func() { policyMemory, policyTicks, policyTicked = oldMem, oldTicks, oldTicked })
	policyMemory = func() procinfo.Memory { return mem }
	policyTicks = func(time.Duration) (<-chan time.Time, func()) { return ticks, func() {} }
	policyTicked = func() { ticked <- struct{}{} }
	// step ticks at now and returns once that tick is done.
	step := func(now time.Time) {
		ticks <- now
		<-ticked
	}
	return fake, bus, mem, step, env
}

// startPolicy runs `agentbus hibernate --policy` until the returned
// stop is called, which returns its exit code and stderr.
func startPolicy(t *testing.T, env []string) func() (int, string) {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	var stdout, stderr bytes.Buffer
	done := make(chan int)
	go func() {
		done <- run(ctx, []string{"hibernate", "--policy"}, env, strings.NewReader(""), &stdout, &stderr)
	}()
	return func() (int, string) {
		cancel()
		code := <-done
		return code, stderr.String()
	}
}

func TestPolicyRunnerHibernatesThroughTheHubInIdleOrder(t *testing.T) {
	fake, bus, mem, step, env := setupPolicy(t)
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Dir(dirs.TunnelSocketPath), 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, dirs.TunnelSocketPath, nil)
	env = append(env, "HERDR_PANE_ID=w1:p4")
	stop := startPolicy(t, env)

	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	// p5 goes idle first, then p1, then p4: the runner's own clock, not
	// the hub's, says how long each has been idle.
	fake.Write("local.agents", policyAgents("w1:p5"))
	step(t0)
	fake.Write("local.agents", policyAgents("w1:p5", "w1:p1"))
	step(t0.Add(5 * time.Minute))
	fake.Write("local.agents", policyAgents("w1:p5", "w1:p1", "w1:p4"))
	step(t0.Add(10 * time.Minute))
	if ops := bus.ops(); slices.Contains(ops, "hibernate-state") {
		t.Fatalf("hub ops = %q before memory was low", ops)
	}
	mem.base.Store(900 * mib)
	step(t0.Add(41 * time.Minute))
	code, stderr := stop()
	if code != 0 {
		t.Fatalf("exit %d, stderr %q", code, stderr)
	}

	var begun []string
	for _, req := range bus.reqs {
		if req.Op == "hibernate-begin" {
			begun = append(begun, req.Hibernate.Record.Handle)
		}
	}
	// p5 is idle 41m and p1 36m; p4, idle 31m, is left: 900 MiB + two
	// stops of 400 MiB reach 1.5 GiB.
	if want := []string{"herdr:mac/sess-5/w1:p5", "herdr:mac/sess-1/w1:p1"}; !slices.Equal(begun, want) {
		t.Fatalf("hibernated %q, want %q; stderr %q", begun, want, stderr)
	}
	for _, socket := range bus.socket {
		if socket != dirs.TunnelSocketPath {
			t.Fatalf("dialled %s, want only the tunnel to the hub", socket)
		}
	}
	if strings.Count(stderr, "policy_hibernate") != 2 || !strings.Contains(stderr, "ops/c") || !strings.Contains(stderr, "ops/a") {
		t.Fatalf("stderr = %q, want two policy_hibernate events", stderr)
	}
}

func TestPolicyRunnerWithTheHubUnreachableStopsNothing(t *testing.T) {
	fake, _, mem, step, env := setupPolicy(t)
	dialDaemon = func(_ context.Context, socket string, req daemon.Request) (daemon.Response, error) {
		t.Errorf("dialled %s for %s with no tunnel", socket, req.Op)
		return daemon.Response{}, errors.New("no daemon")
	}
	ssh := sshtest.Install(t)
	ssh.Write("down", "")
	ssh.Unreachable()
	mem.base.Store(500 * mib)
	stop := startPolicy(t, env)

	t0 := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	fake.Write("local.agents", policyAgents(policyPanes...))
	step(t0)
	step(t0.Add(time.Hour))
	code, stderr := stop()
	if code != 0 {
		t.Fatalf("exit %d, stderr %q", code, stderr)
	}
	for _, argv := range fake.Argv() {
		if strings.HasPrefix(argv, "pane send-") || strings.HasPrefix(argv, "pane run") {
			t.Fatalf("typed into a pane with the hub down: %q", argv)
		}
	}
	if !strings.Contains(stderr, "hub me@hub") || !strings.Contains(stderr, "nothing more hibernated until the next tick") {
		t.Fatalf("stderr = %q, want why nothing was hibernated", stderr)
	}
}

func TestPolicyRunnerNeedsHubMode(t *testing.T) {
	_, _, env := setupHibernate(t)
	code, _, stderr := runCmd([]string{"hibernate", "--policy"}, env)
	if code != 1 || !strings.Contains(stderr, "agentbusd") {
		t.Fatalf("hibernate --policy = %d, %q, want agentbusd named as the runner here", code, stderr)
	}
	if code, _, _ := runCmd([]string{"hibernate", "--policy", "ops/x"}, env); code != 2 {
		t.Fatalf("hibernate --policy ops/x = %d, want 2", code)
	}
}

func TestPolicyRunnerOffExitsCleanly(t *testing.T) {
	_, _, _, _, env := setupPolicy(t)
	writeFile(t, filepath.Join(envGet(env, "HOME"), ".config", "agentbus", "config.toml"), []byte("hub = \"me@hub\"\nmachine = \"mac\"\n"))
	code, _, stderr := runCmd([]string{"hibernate", "--policy"}, env)
	if code != 0 || !strings.Contains(stderr, "hibernate_policy=off") {
		t.Fatalf("hibernate --policy = %d, %q", code, stderr)
	}
}

func TestPolicyUnitTexts(t *testing.T) {
	vars := [][2]string{{"HOME", "/Users/me"}, {"PATH", "/opt/bin:/usr/bin"}}
	plist, err := policyBody("darwin", "/opt/bin/agentbus", "/state/agentbus/hibernate-policy.log", vars)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"<string>" + policyLaunchdLabel + "</string>",
		"<string>/opt/bin/agentbus</string>\n    <string>hibernate</string>\n    <string>--policy</string>",
		"<key>SuccessfulExit</key>\n    <false/>",
		"<key>StandardErrorPath</key>\n  <string>/state/agentbus/hibernate-policy.log</string>",
		"<string>/opt/bin:/usr/bin</string>",
	} {
		if !strings.Contains(plist, want) {
			t.Fatalf("plist missing %s\n%s", want, plist)
		}
	}
	unit, err := policyBody("linux", "/opt/bin/agentbus", "/state/agentbus/hibernate-policy.log", vars)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"Description=agentbus hibernation policy for this hub-mode client\n",
		"ExecStart=/opt/bin/agentbus hibernate --policy\n",
		"Restart=on-failure\n",
		"Environment=PATH=/opt/bin:/usr/bin\n",
	} {
		if !strings.Contains(unit, want) {
			t.Fatalf("unit missing %s\n%s", want, unit)
		}
	}
}

func TestHubInstallWritesThePolicyUnitOnlyWhenThePolicyIsOn(t *testing.T) {
	if runtime.GOOS != "darwin" && runtime.GOOS != "linux" {
		t.Skip("install supports launchd and systemd")
	}
	h := startHub(t, `hub = "me@hub"`, `machine = "mac"`, `hub_socket = "/run/user/1000/agentbus/hub.sock"`,
		`hibernate_policy = "dry-run"`, `hibernate_below = "1GiB"`)
	env := append(append([]string{}, h.env...), "AGENTBUS_INSTALL_NO_LOAD=1")
	code, stdout, stderr := h.runEnv(t, env, "", "install")
	if code != 0 {
		t.Fatalf("install = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	home := strings.TrimPrefix(env[0], "HOME=")
	tunnel, err := tunnelPath(runtime.GOOS, home, env)
	if err != nil {
		t.Fatal(err)
	}
	policy, err := policyPath(runtime.GOOS, home, env)
	if err != nil {
		t.Fatal(err)
	}
	if stdout != "wrote "+tunnel+"\nwrote "+policy+"\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	data, err := os.ReadFile(policy)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(data), "hibernate") || !strings.Contains(string(data), "--policy") {
		t.Fatalf("policy unit = %s", data)
	}
}
