package main

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall/sshtest"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

const mib = 1024 * 1024

// memAgents is a local herdr agent list: an idle claude in w1:p1, a
// working codex in w1:p2, and a plain shell in w1:p3.
const memAgents = `{"result":{"agents":[
  {"pane_id":"w1:p1","tab_id":"w1:t1","agent":"claude","agent_status":"idle","agent_session":{"agent":"claude","kind":"id","value":"sess-1"}},
  {"pane_id":"w1:p2","tab_id":"w1:t2","agent":"codex","agent_status":"working","agent_session":{"value":"sess-2"}},
  {"pane_id":"w1:p3","tab_id":"w1:t3","agent":"","agent_status":"","agent_session":{"value":""}}
]}}`

const memTabs = `{"result":{"tabs":[
  {"tab_id":"w1:t1","label":"inbox"},{"tab_id":"w1:t2","label":"build"},{"tab_id":"w1:t3","label":"shell"}
]}}`

func processInfoJSON(pane string, pids ...int) string {
	procs := make([]string, len(pids))
	for i, pid := range pids {
		procs[i] = `{"pid":` + itoa(pid) + `,"name":"agent","argv":["agent"],"cwd":"/src"}`
	}
	return `{"result":{"process_info":{"pane_id":"` + pane + `","shell_pid":1,"foreground_process_group_id":` +
		itoa(pids[0]) + `,"foreground_processes":[` + strings.Join(procs, ",") + `]}}}`
}

func itoa(n int) string {
	b, _ := json.Marshal(n)
	return string(b)
}

type fakeTable []procinfo.Process

func (f fakeTable) List(context.Context) ([]procinfo.Process, error) { return f, nil }

type fakeMemory int64

func (f fakeMemory) Available(context.Context) (int64, error) { return int64(f), nil }

func p(pid, ppid int, rss int64, age time.Duration, argv ...string) procinfo.Process {
	return procinfo.Process{PID: pid, PPID: ppid, RSS: rss, Age: age, Name: argv[0], Argv: argv}
}

// memTable gives w1:p1 (pid 100) a tree of 471 MiB, larger than w1:p2's
// single 400 MiB process, though its own root is smaller. pid 101 is an
// old job; 103 is young and 104 a helper.
var memTable = fakeTable{
	p(1, 0, 5*mib, 48*time.Hour, "launchd"),
	p(100, 1, 300*mib, 3*time.Hour, "claude"),
	p(101, 100, 50*mib, time.Hour, "sh", "-c", "bazel build //..."),
	p(102, 101, 100*mib, time.Hour, "bazel", "build", "//..."),
	p(103, 100, 1*mib, 2*time.Second, "sh", "-c", "ls"),
	p(104, 100, 20*mib, time.Hour, "node", "/opt/github-mcp-server/index.js"),
	p(200, 1, 400*mib, 2*time.Hour, "codex"),
	p(300, 1, 3*mib, 5*time.Hour, "-zsh"),
}

// fakeMemBus fakes the daemon: ops/x is on w1:p1, and w1:p2 holds a lease
// that w1:p1 is queued for. Its idle tracker has no times.
func fakeMemBus(ctx context.Context, socket string, req daemon.Request) (daemon.Response, error) {
	return fakeMemBusSeen(nil)(ctx, socket, req)
}

// fakeMemBusSeen is fakeMemBus with the idle tracker's times seen.
func fakeMemBusSeen(seen map[string]hibernate.Seen) func(context.Context, string, daemon.Request) (daemon.Response, error) {
	return func(_ context.Context, _ string, req daemon.Request) (daemon.Response, error) {
		if req.Op != "hibernate-state" {
			return daemon.Response{Error: "unexpected op " + req.Op}, nil
		}
		return daemon.Response{OK: true, Hibernate: &hibernate.Reply{
			Names: map[string][]string{"herdr:local/sess-1/w1:p1": {"ops/x"}},
			Seen:  seen,
			Leases: []lease.Status{{
				Checkout: "local:/src/repo",
				State:    lease.StateHeld,
				Holder:   &lease.HolderView{Handle: "herdr:local/sess-2/w1:p2"},
				Queue:    []lease.WaiterView{{Handle: "herdr:local/sess-1/w1:p1"}},
			}},
		}}, nil
	}
}

func setupMem(t *testing.T, dial func(context.Context, string, daemon.Request) (daemon.Response, error)) (*herdrtest.Fake, []string) {
	t.Helper()
	fake := herdrtest.Install(t)
	fake.Write("local.agents", memAgents)
	fake.Write("local.tabs", memTabs)
	fake.Write("local.process-info.w1:p1", processInfoJSON("w1:p1", 100))
	fake.Write("local.process-info.w1:p2", processInfoJSON("w1:p2", 200))
	fake.Write("local.process-info.w1:p3", processInfoJSON("w1:p3", 300))
	oldDial, oldProcs, oldMem := dialDaemon, memProcs, memMemory
	t.Cleanup(func() { dialDaemon, memProcs, memMemory = oldDial, oldProcs, oldMem })
	dialDaemon = dial
	memProcs = func() procinfo.Table { return memTable }
	memMemory = func() procinfo.Memory { return fakeMemory(8 * 1024 * mib) }
	env := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	return fake, env
}

type memJSON struct {
	Agents []struct {
		Pane       string           `json:"pane"`
		Tab        string           `json:"tab"`
		Handle     string           `json:"handle"`
		Names      *[]string        `json:"names"`
		State      string           `json:"state"`
		IdleSince  string           `json:"idle_since"`             //nolint:tagliatelle // mem JSON shape
		IdleSecs   *int64           `json:"idle_seconds"`           //nolint:tagliatelle // mem JSON shape
		LowerBound bool             `json:"idle_since_lower_bound"` //nolint:tagliatelle // mem JSON shape
		RSSBytes   int64            `json:"rss_bytes"`              //nolint:tagliatelle // mem JSON shape
		Jobs       []map[string]any `json:"jobs"`
		Leases     *[]struct {
			Checkout string `json:"checkout"`
			Role     string `json:"role"`
		} `json:"leases"`
		Session string `json:"session"`
	} `json:"agents"`
	TotalRSSBytes  int64          `json:"total_rss_bytes"` //nolint:tagliatelle // mem JSON shape
	AvailableBytes int64          `json:"available_bytes"` //nolint:tagliatelle // mem JSON shape
	Machine        string         `json:"machine"`
	States         map[string]int `json:"states"`
	Notes          []string       `json:"notes"`
}

func TestMemJSON(t *testing.T) {
	fake, env := setupMem(t, fakeMemBus)
	stdout, _ := mustCmd(t, []string{"mem", "--json"}, env)
	var got memJSON
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("decode %q: %v", stdout, err)
	}
	if len(got.Agents) != 2 {
		t.Fatalf("agents = %+v, want two agent panes and no shell pane", got.Agents)
	}
	first, second := got.Agents[0], got.Agents[1]
	if first.Pane != "w1:p1" || second.Pane != "w1:p2" {
		t.Fatalf("order = %s, %s; want the larger tree first", first.Pane, second.Pane)
	}
	if first.RSSBytes != 471*mib || second.RSSBytes != 400*mib {
		t.Fatalf("rss = %d, %d; want the whole tree summed", first.RSSBytes, second.RSSBytes)
	}
	if first.Tab != "inbox" || first.Handle != "herdr:local/sess-1/w1:p1" || first.Session != "sess-1" || first.State != "idle" || first.IdleSince != "unknown" {
		t.Fatalf("first = %+v", first)
	}
	if first.Names == nil || !slices.Equal(*first.Names, []string{"ops/x"}) || second.Names == nil || len(*second.Names) != 0 {
		t.Fatalf("names = %v, %v", first.Names, second.Names)
	}
	if len(first.Jobs) != 1 || first.Jobs[0]["pid"] != float64(101) || first.Jobs[0]["age_seconds"] != float64(3600) || first.Jobs[0]["command"] != `sh -c "bazel build //..."` {
		t.Fatalf("jobs = %v, want only the old non-helper child", first.Jobs)
	}
	if second.Jobs == nil || len(second.Jobs) != 0 {
		t.Fatalf("jobs = %v, want []", second.Jobs)
	}
	if first.Leases == nil || len(*first.Leases) != 1 || (*first.Leases)[0].Role != "queued" || (*first.Leases)[0].Checkout != "local:/src/repo" {
		t.Fatalf("leases = %+v", first.Leases)
	}
	if second.Leases == nil || len(*second.Leases) != 1 || (*second.Leases)[0].Role != "holder" {
		t.Fatalf("leases = %+v", second.Leases)
	}
	if got.TotalRSSBytes != first.RSSBytes+second.RSSBytes {
		t.Fatalf("total = %d, want the sum of the rows", got.TotalRSSBytes)
	}
	if got.AvailableBytes != 8*1024*mib || got.Machine == "" || got.States["idle"] != 1 || got.States["working"] != 1 || len(got.Notes) != 0 {
		t.Fatalf("footer = %+v", got)
	}
	var infos, lists int
	for _, argv := range fake.Argv() {
		switch {
		case strings.HasPrefix(argv, "pane process-info"):
			infos++
			if strings.Contains(argv, "w1:p3") {
				t.Fatalf("process-info for the shell pane: %q", argv)
			}
		case argv == "agent list":
			lists++
		}
	}
	if lists != 1 || infos != 2 {
		t.Fatalf("herdr calls: %d agent list, %d process-info; want 1 and 2", lists, infos)
	}
}

func TestMemText(t *testing.T) {
	_, env := setupMem(t, fakeMemBus)
	stdout, _ := mustCmd(t, []string{"mem"}, env)
	lines := strings.Split(strings.TrimSpace(stdout), "\n")
	if len(lines) < 4 {
		t.Fatalf("output = %q", stdout)
	}
	for _, want := range []string{"PANE", "NAMES", "STATE", "RSS", "JOBS", "LEASE"} {
		if !strings.Contains(lines[0], want) {
			t.Fatalf("header %q lacks %s", lines[0], want)
		}
	}
	for _, want := range []string{"w1:p1", "inbox", "ops/x", "idle", "471.0 MiB", "queued local:/src/repo"} {
		if !strings.Contains(lines[1], want) {
			t.Fatalf("row %q lacks %q", lines[1], want)
		}
	}
	if !strings.Contains(lines[2], "w1:p2") || !strings.Contains(lines[2], "400.0 MiB") || !strings.Contains(lines[2], "holds local:/src/repo") {
		t.Fatalf("row = %q", lines[2])
	}
	if strings.Contains(stdout, "w1:p3") {
		t.Fatalf("output lists the shell pane: %q", stdout)
	}
	footer := lines[len(lines)-1]
	for _, want := range []string{"871.0 MiB", "2 agents", "idle 1", "working 1", "8.0 GiB available"} {
		if !strings.Contains(footer, want) {
			t.Fatalf("footer %q lacks %q", footer, want)
		}
	}
}

func TestMemFooterShowsTheHibernationPolicy(t *testing.T) {
	_, env := setupMem(t, fakeMemBus)
	stdout, _ := mustCmd(t, []string{"mem"}, env)
	if !strings.Contains(stdout, "available; hibernate policy off\n") {
		t.Fatalf("no config: output = %q, want the policy off", stdout)
	}
	writeFile(t, filepath.Join(envGet(env, "HOME"), ".config", "agentbus", "config.toml"), []byte("hibernate_policy = \"on\"\nhibernate_below = \"1.5GiB\"\nhibernate_target = \"3GiB\"\n"))
	stdout, _ = mustCmd(t, []string{"mem"}, env)
	if !strings.Contains(stdout, "hibernate policy on (below 1.5 GiB, target 3.0 GiB, min idle 30m0s)") {
		t.Fatalf("output = %q, want the mode and thresholds", stdout)
	}
	stdout, _ = mustCmd(t, []string{"mem", "--json"}, env)
	var got struct {
		Policy struct {
			Mode        string `json:"mode"`
			BelowBytes  int64  `json:"below_bytes"`      //nolint:tagliatelle // mem JSON shape
			TargetBytes int64  `json:"target_bytes"`     //nolint:tagliatelle // mem JSON shape
			MinIdle     int64  `json:"min_idle_seconds"` //nolint:tagliatelle // mem JSON shape
		} `json:"hibernate_policy"` //nolint:tagliatelle // mem JSON shape
	}
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatal(err)
	}
	if got.Policy.Mode != "on" || got.Policy.BelowBytes != 1536*mib || got.Policy.TargetBytes != 3072*mib || got.Policy.MinIdle != 1800 {
		t.Fatalf("hibernate_policy = %+v", got.Policy)
	}
}

func TestMemDaemonDown(t *testing.T) {
	_, env := setupMem(t, func(context.Context, string, daemon.Request) (daemon.Response, error) {
		return daemon.Response{}, errors.New("connect: no such file")
	})
	stdout, _ := mustCmd(t, []string{"mem"}, env)
	if !strings.Contains(stdout, "w1:p1") || !strings.Contains(stdout, "w1:p2") {
		t.Fatalf("output = %q, want the rows", stdout)
	}
	header := strings.SplitN(stdout, "\n", 2)[0]
	if strings.Contains(header, "NAMES") || strings.Contains(header, "LEASE") {
		t.Fatalf("header = %q, want no names or lease column", header)
	}
	if !strings.Contains(stdout, "note: ") || !strings.Contains(stdout, "daemon") {
		t.Fatalf("output = %q, want a note about the daemon", stdout)
	}

	stdout, _ = mustCmd(t, []string{"mem", "--json"}, env)
	var got memJSON
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatal(err)
	}
	if len(got.Agents) != 2 || got.Agents[0].Names != nil || got.Agents[0].Leases != nil || len(got.Notes) != 1 {
		t.Fatalf("json = %s, want rows without names or leases and one note", stdout)
	}
	if got.Agents[0].IdleSince != "unknown" || got.Agents[0].IdleSecs != nil {
		t.Fatalf("idle = %+v, want unknown without the daemon", got.Agents[0])
	}
}

// memRow is the text row of pane.
func memRow(t *testing.T, stdout, pane string) string {
	t.Helper()
	for _, line := range strings.Split(stdout, "\n") {
		if strings.HasPrefix(line, pane+" ") {
			return line
		}
	}
	t.Fatalf("no row for %s in %q", pane, stdout)
	return ""
}

func TestMemIdleSinceFromTheDaemon(t *testing.T) {
	_, env := setupMem(t, fakeMemBusSeen(map[string]hibernate.Seen{
		"herdr:local/sess-1/w1:p1": {Status: "idle", For: 3 * time.Hour},
		"herdr:local/sess-2/w1:p2": {Status: "working", For: time.Hour},
	}))
	stdout := mustOut(t, []string{"mem"}, env)
	if row := memRow(t, stdout, "w1:p1"); !strings.Contains(row, " 3h0m ") {
		t.Fatalf("w1:p1 = %q, want idle 3h0m", row)
	}
	if row := memRow(t, stdout, "w1:p2"); !strings.Contains(row, " unknown ") {
		t.Fatalf("w1:p2 = %q, want a working row unknown", row)
	}
	got := decodeMem(t, mustOut(t, []string{"mem", "--json"}, env))
	first := got.Agents[0]
	since, err := time.Parse(time.RFC3339, first.IdleSince)
	if err != nil || first.IdleSecs == nil || *first.IdleSecs != 10800 || first.LowerBound {
		t.Fatalf("w1:p1 = %+v, want idle_seconds 10800 and an RFC 3339 idle_since", first)
	}
	if ago := time.Since(since); ago < 3*time.Hour-time.Minute || ago > 3*time.Hour+time.Minute {
		t.Fatalf("idle_since = %s, want about 3h ago", since)
	}
	if second := got.Agents[1]; second.IdleSince != "unknown" || second.IdleSecs != nil {
		t.Fatalf("w1:p2 = %+v, want unknown", second)
	}
}

func TestMemIdleSinceBoundedByTheDaemonStart(t *testing.T) {
	_, env := setupMem(t, fakeMemBusSeen(map[string]hibernate.Seen{
		"herdr:local/sess-1/w1:p1": {Status: "idle", For: 40 * time.Minute, LowerBound: true},
	}))
	if row := memRow(t, mustOut(t, []string{"mem"}, env), "w1:p1"); !strings.Contains(row, " >=40m ") {
		t.Fatalf("w1:p1 = %q, want >=40m", row)
	}
	first := decodeMem(t, mustOut(t, []string{"mem", "--json"}, env)).Agents[0]
	if !first.LowerBound || first.IdleSecs == nil || *first.IdleSecs != 2400 {
		t.Fatalf("w1:p1 = %+v, want idle_since_lower_bound and idle_seconds 2400", first)
	}
}

func TestMemHerdrDown(t *testing.T) {
	fake, env := setupMem(t, fakeMemBus)
	fake.Write("local.down", "")
	code, _, stderr := runCmd([]string{"mem"}, env)
	if code == 0 || !strings.Contains(stderr, "herdr") {
		t.Fatalf("mem = %d, stderr %q; want a herdr error", code, stderr)
	}
}

// memHubList is the hub's `list --json`: ops/x on this machine (mac),
// ops/y on another machine, and ops/z on the hub's own pane with the
// same session and pane ids as this machine's w1:p2.
const memHubList = `[
  {"name":"ops/x","when":"1m","liveness":"alive","transport":"herdr","handle":"herdr:mac/sess-1/w1:p1"},
  {"name":"ops/y","when":"1m","liveness":"alive","transport":"herdr","handle":"herdr:other/sess-1/w1:p1"},
  {"name":"ops/z","when":"1m","liveness":"alive","transport":"herdr","handle":"herdr:local/sess-2/w1:p2"}
]`

// memHubLeases is the hub's `lease status --all --json`: this machine's
// w1:p2 holds mac:/src/repo, and other machines' panes hold or wait for
// checkouts too.
const memHubLeases = `[
  {"checkout":"mac:/src/repo","state":"held","holder":{"handle":"herdr:mac/sess-2/w1:p2"},"queue":[{"handle":"herdr:other/sess-1/w1:p1"}],"takeover":"ask"},
  {"checkout":"local:/hub/repo","state":"held","holder":{"handle":"herdr:local/sess-1/w1:p1"}}
]`

// setupMemHub runs mem on machine "mac" in hub mode against a fake hub
// over ssh whose agentbus prints memHubList and memHubLeases. Any local
// daemon call fails the test.
func setupMemHub(t *testing.T) (*sshtest.Fake, []string) {
	t.Helper()
	_, env := setupMem(t, func(_ context.Context, socket string, req daemon.Request) (daemon.Response, error) {
		t.Errorf("mem in hub mode dialed %s for %s", socket, req.Op)
		return daemon.Response{}, errors.New("no daemon")
	})
	writeFile(t, filepath.Join(envGet(env, "HOME"), ".config", "agentbus", "config.toml"), []byte("hub = \"me@hub\"\nmachine = \"mac\"\n"))
	ssh := sshtest.Install(t)
	dir := t.TempDir()
	writeFile(t, filepath.Join(dir, "list.json"), []byte(memHubList))
	writeFile(t, filepath.Join(dir, "leases.json"), []byte(memHubLeases))
	script := "#!/bin/sh\n" +
		"case \"$*\" in\n" +
		"'list --json') cat " + filepath.Join(dir, "list.json") + ";;\n" +
		"'lease status --all --json') cat " + filepath.Join(dir, "leases.json") + ";;\n" +
		"*) echo \"unexpected $*\" >&2; exit 2;;\n" +
		"esac\n"
	if err := os.WriteFile(filepath.Join(dir, "agentbus"), []byte(script), 0o755); err != nil { //nolint:gosec // test executable
		t.Fatal(err)
	}
	t.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	return ssh, env
}

func decodeMem(t *testing.T, stdout string) memJSON {
	t.Helper()
	var got memJSON
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("decode %q: %v", stdout, err)
	}
	return got
}

func mustOut(t *testing.T, args, env []string) string {
	t.Helper()
	stdout, _ := mustCmd(t, args, env)
	return stdout
}

func TestMemHubNamesAndLeasesOverSSH(t *testing.T) {
	ssh, env := setupMemHub(t)
	ssh.Reachable()
	got := decodeMem(t, mustOut(t, []string{"mem", "--json"}, env))
	if len(got.Agents) != 2 || len(got.Notes) != 0 {
		t.Fatalf("mem = %+v, want two rows and no note", got)
	}
	first, second := got.Agents[0], got.Agents[1]
	if first.Pane != "w1:p1" || first.Names == nil || !slices.Equal(*first.Names, []string{"ops/x"}) {
		t.Fatalf("w1:p1 names = %v, want only this machine's ops/x", first.Names)
	}
	if first.Leases == nil || len(*first.Leases) != 0 {
		t.Fatalf("w1:p1 leases = %+v, want none: the hub's own pane holds local:/hub/repo", first.Leases)
	}
	if second.Pane != "w1:p2" || second.Names == nil || len(*second.Names) != 0 {
		t.Fatalf("w1:p2 names = %v, want none: ops/z is the hub's own pane", second.Names)
	}
	if second.Leases == nil || len(*second.Leases) != 1 || (*second.Leases)[0].Role != "holder" || (*second.Leases)[0].Checkout != "mac:/src/repo" {
		t.Fatalf("w1:p2 leases = %+v, want it to hold mac:/src/repo", second.Leases)
	}

	stdout := mustOut(t, []string{"mem"}, env)
	header := strings.SplitN(stdout, "\n", 2)[0]
	if !strings.Contains(header, "NAMES") || !strings.Contains(header, "LEASE") {
		t.Fatalf("header = %q, want names and lease columns", header)
	}
	if !strings.Contains(stdout, "ops/x") || !strings.Contains(stdout, "holds mac:/src/repo") ||
		strings.Contains(stdout, "ops/y") || strings.Contains(stdout, "ops/z") || strings.Contains(stdout, "note:") {
		t.Fatalf("output = %q", stdout)
	}
}

func TestMemHubUnreachable(t *testing.T) {
	ssh, env := setupMemHub(t)
	ssh.Write("down", "")
	ssh.Unreachable()
	start := time.Now()
	stdout := mustOut(t, []string{"mem"}, env)
	if took := time.Since(start); took > 5*time.Second {
		t.Fatalf("mem took %s", took)
	}
	if !strings.Contains(stdout, "w1:p1") || !strings.Contains(stdout, "w1:p2") {
		t.Fatalf("output = %q, want the rows", stdout)
	}
	header := strings.SplitN(stdout, "\n", 2)[0]
	if strings.Contains(header, "NAMES") || strings.Contains(header, "LEASE") {
		t.Fatalf("header = %q, want no names or lease column", header)
	}
	if !strings.Contains(stdout, "note: ") || !strings.Contains(stdout, "me@hub") {
		t.Fatalf("output = %q, want a note naming the hub", stdout)
	}
}

func TestMemHubShowsThisMachinesPolicy(t *testing.T) {
	ssh, env := setupMemHub(t)
	ssh.Write("down", "")
	ssh.Unreachable()
	writeFile(t, filepath.Join(envGet(env, "HOME"), ".config", "agentbus", "config.toml"),
		[]byte("hub = \"me@hub\"\nmachine = \"mac\"\nhibernate_policy = \"on\"\nhibernate_below = \"1GiB\"\n"))
	stdout := mustOut(t, []string{"mem"}, env)
	if !strings.Contains(stdout, "hibernate policy on (below 1.0 GiB, target 1.0 GiB, min idle 30m0s)") || strings.Contains(stdout, "does not run") {
		t.Fatalf("output = %q, want the configured policy: agentbus hibernate --policy runs it here", stdout)
	}
}

func TestMemHubDeadline(t *testing.T) {
	ssh, env := setupMemHub(t)
	ssh.Write("hang", "")
	ssh.Reachable()
	env = append(env, "AGENTBUS_HOOK_TIMEOUT=500ms")
	start := time.Now()
	got := decodeMem(t, mustOut(t, []string{"mem", "--json"}, env))
	if took := time.Since(start); took > 2500*time.Millisecond {
		t.Fatalf("mem took %s, want the hook deadline", took)
	}
	if len(got.Agents) != 2 || got.Agents[0].Names != nil || len(got.Notes) != 1 || !strings.Contains(got.Notes[0], "me@hub") {
		t.Fatalf("mem = %+v, want rows without names and a note naming the hub", got)
	}
}

// savePolicySeen saves an idle tracker to the policy runner's file: the
// runner saw w1:p1 idle 3h ago, and w1:p2 idle with an older seq.
func savePolicySeen(t *testing.T, env []string) {
	t.Helper()
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(dirs.StateDir, 0o755); err != nil {
		t.Fatal(err)
	}
	now := time.Now()
	var tr hibernate.IdleTracker
	tr.Observe(now.Add(-4*time.Hour), []adapter.Agent{{Handle: "herdr:local/sess-1/w1:p1", Status: "working"}})
	tr.Observe(now.Add(-3*time.Hour), []adapter.Agent{
		{Handle: "herdr:local/sess-1/w1:p1", Status: "idle"},
		{Handle: "herdr:local/sess-2/w1:p2", Status: "idle", StateSeq: 9},
	})
	if err := tr.Save(filepath.Join(dirs.StateDir, policySeenFile), now); err != nil {
		t.Fatal(err)
	}
}

func TestMemHubIdleSinceFromThePolicyRunner(t *testing.T) {
	ssh, env := setupMemHub(t)
	ssh.Reachable()
	stdout := mustOut(t, []string{"mem"}, env)
	if row := memRow(t, stdout, "w1:p1"); !strings.Contains(row, " unknown ") {
		t.Fatalf("w1:p1 = %q, want unknown with no policy runner", row)
	}
	savePolicySeen(t, env)
	stdout = mustOut(t, []string{"mem"}, env)
	if row := memRow(t, stdout, "w1:p1"); !strings.Contains(row, " 3h0m ") {
		t.Fatalf("w1:p1 = %q, want the runner's 3h0m", row)
	}
	got := decodeMem(t, mustOut(t, []string{"mem", "--json"}, env))
	if first := got.Agents[0]; first.IdleSecs == nil || *first.IdleSecs != 10800 || first.LowerBound {
		t.Fatalf("w1:p1 = %+v, want idle_seconds 10800", first)
	}
	// w1:p2 is working now, not what the runner saw.
	if second := got.Agents[1]; second.IdleSince != "unknown" {
		t.Fatalf("w1:p2 = %+v, want unknown", second)
	}
}

func TestMemHubOverTheTunnel(t *testing.T) {
	_, env := setupMem(t, nil)
	writeFile(t, filepath.Join(envGet(env, "HOME"), ".config", "agentbus", "config.toml"), []byte("hub = \"me@hub\"\nmachine = \"mac\"\n"))
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	// A file stands in for the tunnel socket; the fake dial answers on it.
	writeFile(t, dirs.TunnelSocketPath, nil)
	var sockets []string
	dialDaemon = func(_ context.Context, socket string, req daemon.Request) (daemon.Response, error) {
		sockets = append(sockets, socket)
		switch req.Op {
		case "ping":
			return daemon.Response{OK: true, Protocol: daemon.ProtocolVersion}, nil
		case "hibernate-state":
			return daemon.Response{OK: true, Hibernate: &hibernate.Reply{
				Names: map[string][]string{
					"herdr:mac/sess-1/w1:p1":   {"ops/x"},
					"herdr:other/sess-2/w1:p2": {"ops/y"},
				},
				// The hub's tracker is not this machine's: mem leaves
				// it out.
				Seen: map[string]hibernate.Seen{"herdr:mac/sess-1/w1:p1": {Status: "idle", For: time.Hour}},
				Leases: []lease.Status{{
					Checkout: "mac:/src/repo",
					State:    lease.StateHeld,
					Holder:   &lease.HolderView{Handle: "herdr:mac/sess-2/w1:p2"},
				}},
			}}, nil
		}
		return daemon.Response{Error: "unexpected op " + req.Op}, nil
	}
	env = append(env, "HERDR_PANE_ID=w1:p2")
	got := decodeMem(t, mustOut(t, []string{"mem", "--json"}, env))
	if len(got.Agents) != 2 || len(got.Notes) != 0 {
		t.Fatalf("mem = %+v", got)
	}
	first, second := got.Agents[0], got.Agents[1]
	if first.Names == nil || !slices.Equal(*first.Names, []string{"ops/x"}) || second.Names == nil || len(*second.Names) != 0 {
		t.Fatalf("names = %v, %v", first.Names, second.Names)
	}
	if second.Leases == nil || len(*second.Leases) != 1 || (*second.Leases)[0].Checkout != "mac:/src/repo" {
		t.Fatalf("leases = %+v", second.Leases)
	}
	if first.IdleSince != "unknown" {
		t.Fatalf("w1:p1 = %+v, want unknown: the hub does not track this machine", first)
	}
	for _, socket := range sockets {
		if socket != dirs.TunnelSocketPath {
			t.Fatalf("dialled %s, want only the tunnel", socket)
		}
	}
}

func TestMemRejectsArguments(t *testing.T) {
	_, env := setupMem(t, fakeMemBus)
	if code, _, _ := runCmd([]string{"mem", "extra"}, env); code != 2 {
		t.Fatalf("mem extra = %d, want 2", code)
	}
}
