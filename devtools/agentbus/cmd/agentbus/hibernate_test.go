package main

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

const hibRule = "──────────────────────────────"

// fakeHibClock moves only when slept on.
type fakeHibClock struct{ now time.Time }

func (c *fakeHibClock) Now() time.Time { return c.now }

func (c *fakeHibClock) Sleep(_ context.Context, d time.Duration) error {
	c.now = c.now.Add(d)
	return nil
}

// hibBus fakes the daemon's hibernate ops and records the requests.
type hibBus struct {
	mu     sync.Mutex
	names  map[string][]string
	seen   map[string]hibernate.Seen
	reqs   []daemon.Request
	socket []string
}

func (b *hibBus) dial(_ context.Context, socket string, req daemon.Request) (daemon.Response, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.reqs = append(b.reqs, req)
	b.socket = append(b.socket, socket)
	switch req.Op {
	case "ping":
		return daemon.Response{OK: true, Protocol: daemon.ProtocolVersion}, nil
	case "hibernate-state":
		return daemon.Response{OK: true, Hibernate: &hibernate.Reply{Names: b.names, Seen: b.seen}}, nil
	case "hibernate-begin":
		rec := *req.Hibernate.Record
		rec.Status = hibernate.StatusStopping
		return daemon.Response{OK: true, Hibernate: &hibernate.Reply{Record: &rec}}, nil
	case "hibernate-end", "hibernate-refused":
		return daemon.Response{OK: true, Hibernate: &hibernate.Reply{}}, nil
	}
	return daemon.Response{Error: "unexpected op " + req.Op}, nil
}

func (b *hibBus) ops() []string {
	b.mu.Lock()
	defer b.mu.Unlock()
	var out []string
	for _, r := range b.reqs {
		out = append(out, r.Op)
	}
	return out
}

func (b *hibBus) find(op string) (daemon.Request, bool) {
	b.mu.Lock()
	defer b.mu.Unlock()
	for _, r := range b.reqs {
		if r.Op == op {
			return r, true
		}
	}
	return daemon.Request{}, false
}

// setupHibernate is memAgents with an empty Claude Code composer on
// screen; w1:p1's claude (pid 100) exits on Enter.
func setupHibernate(t *testing.T) (*herdrtest.Fake, *hibBus, []string) {
	t.Helper()
	fake := herdrtest.Install(t)
	fake.Write("local.agents", memAgents)
	fake.Write("local.tabs", memTabs)
	fake.Write("local.screen", "⏺ Done.\n"+hibRule+"\n❯ \n"+hibRule+"\n  [Opus] repo\n")
	fake.Write("local.process-info.w1:p1", `{"result":{"process_info":{"shell_pid":1,"foreground_process_group_id":100,"foreground_processes":[{"pid":100,"name":"2.1.319","argv":["claude","--resume"],"cwd":"/src"}]}}}`)
	fake.Write("local.after-keys.w1:p1", processInfoJSON("w1:p1", 1))
	fake.Write("local.process-info.w1:p2", processInfoJSON("w1:p2", 200))
	bus := &hibBus{
		names: map[string][]string{"herdr:local/sess-1/w1:p1": {"ops/x"}},
		seen: map[string]hibernate.Seen{
			"herdr:local/sess-1/w1:p1": {Status: "idle", For: 3 * time.Hour},
			"herdr:local/sess-2/w1:p2": {Status: "working", For: time.Minute},
		},
	}
	oldDial, oldProcs, oldCwd, oldClock := dialDaemon, hibernateProcs, hibernateCwd, hibernateClock
	t.Cleanup(func() { dialDaemon, hibernateProcs, hibernateCwd, hibernateClock = oldDial, oldProcs, oldCwd, oldClock })
	dialDaemon = bus.dial
	hibernateProcs = func(context.Context) ([]procinfo.Process, error) {
		return []procinfo.Process{
			p(1, 0, 3*mib, 5*time.Hour, "-zsh"),
			p(100, 1, 300*mib, 3*time.Hour, "claude"),
			p(104, 100, 20*mib, time.Hour, "node", "/opt/github-mcp-server/index.js"),
			p(200, 1, 400*mib, 2*time.Hour, "codex"),
		}, nil
	}
	hibernateCwd = func(context.Context, int) (string, error) { return "/elsewhere", nil }
	hibernateClock = &fakeHibClock{now: time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)}
	env := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	return fake, bus, env
}

func TestHibernateStopsAnIdleAgent(t *testing.T) {
	fake, bus, env := setupHibernate(t)
	stdout, _ := mustCmd(t, []string{"hibernate", "ops/x"}, env)
	if stdout != "hibernated ops/x (320.0 MiB freed)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	begin, ok := bus.find("hibernate-begin")
	if !ok {
		t.Fatalf("ops = %q", bus.ops())
	}
	rec := begin.Hibernate.Record
	if rec.Handle != "herdr:local/sess-1/w1:p1" || rec.Session != "sess-1" || rec.Cwd != "/src" || !slices.Equal(rec.Argv, []string{"claude", "--resume"}) || rec.Kind != "claude" {
		t.Fatalf("record = %+v", rec)
	}
	end, _ := bus.find("hibernate-end")
	if end.Hibernate.Status != hibernate.StatusHibernated {
		t.Fatalf("end = %+v", end.Hibernate)
	}
	var inputs []string
	for _, argv := range fake.Argv() {
		if strings.HasPrefix(argv, "pane send-") || strings.HasPrefix(argv, "pane run") {
			inputs = append(inputs, argv)
		}
	}
	if len(inputs) != 3 || inputs[0] != "pane send-text w1:p1 /exit" || inputs[1] != "pane send-keys w1:p1 enter" ||
		!strings.HasPrefix(inputs[2], "pane run w1:p1 printf") || !strings.Contains(inputs[2], "agentbus: hibernated ops/x; wakes on the next message") {
		t.Fatalf("pane inputs = %q", inputs)
	}
}

func TestHibernateIdleOverDryRunJSON(t *testing.T) {
	fake, bus, env := setupHibernate(t)
	stdout, _ := mustCmd(t, []string{"hibernate", "--idle-over", "2h", "--dry-run", "--json"}, env)
	var got []hibernate.Result
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("decode %q: %v", stdout, err)
	}
	if len(got) != 1 || got[0].Name != "ops/x" || got[0].Outcome != hibernate.OutcomeWould || got[0].RSSBytes != 320*mib {
		t.Fatalf("results = %+v", got)
	}
	if ops := bus.ops(); !slices.Equal(ops, []string{"hibernate-state"}) {
		t.Fatalf("daemon ops = %q, want only the state read", ops)
	}
	for _, argv := range fake.Argv() {
		if strings.HasPrefix(argv, "pane send-") || strings.HasPrefix(argv, "pane run") {
			t.Fatalf("dry run typed into a pane: %q", argv)
		}
	}
}

func TestHibernateRefusalExitsNonZero(t *testing.T) {
	fake, bus, env := setupHibernate(t)
	fake.Write("local.screen", "⏺ Done.\n"+hibRule+"\n❯ not sent yet\n"+hibRule+"\n")
	code, stdout, _ := runCmd([]string{"hibernate", "ops/x"}, env)
	if code != 1 || stdout != "refused ops/x: unsent text in the composer\n" {
		t.Fatalf("hibernate = %d, %q", code, stdout)
	}
	refused, ok := bus.find("hibernate-refused")
	if !ok || refused.Hibernate.Handle != "herdr:local/sess-1/w1:p1" {
		t.Fatalf("refused = %+v, ops %q", refused, bus.ops())
	}
}

func TestHibernateUsage(t *testing.T) {
	_, _, env := setupHibernate(t)
	for _, args := range [][]string{
		{"hibernate"},
		{"hibernate", "--idle-over", "2h", "ops/x"},
		{"hibernate", "--idle-over", "soon"},
		{"hibernate", "--bogus"},
	} {
		if code, _, _ := runCmd(args, env); code != 2 {
			t.Errorf("%q = %d, want 2", args, code)
		}
	}
}

func TestHibernateInHubModeTalksToTheHubWithMachineHandles(t *testing.T) {
	_, bus, env := setupHibernate(t)
	home := envGet(env, "HOME")
	writeFile(t, filepath.Join(home, ".config", "agentbus", "config.toml"), []byte("hub = \"me@hub\"\nmachine = \"mac\"\n"))
	bus.names = map[string][]string{"herdr:mac/sess-1/w1:p1": {"ops/x"}}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Dir(dirs.TunnelSocketPath), 0o755); err != nil {
		t.Fatal(err)
	}
	// A file stands in for the tunnel socket; the fake dial answers on it.
	writeFile(t, dirs.TunnelSocketPath, nil)
	env = append(env, "HERDR_PANE_ID=w1:p2")
	stdout, _ := mustCmd(t, []string{"hibernate", "ops/x"}, env)
	if stdout != "hibernated ops/x (320.0 MiB freed)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	begin, _ := bus.find("hibernate-begin")
	if begin.Hibernate.Record.Handle != "herdr:mac/sess-1/w1:p1" || begin.Handle != "herdr:mac/sess-2/w1:p2" {
		t.Fatalf("begin = record %q from %q", begin.Hibernate.Record.Handle, begin.Handle)
	}
	for _, socket := range bus.socket {
		if socket != dirs.TunnelSocketPath {
			t.Fatalf("dialled %s, want only the tunnel", socket)
		}
	}
}

func TestLeaseForwardPassesHibernateOps(t *testing.T) {
	_, bus, env := setupHibernate(t)
	req := `{"op":"hibernate-refused","hibernate":{"handle":"herdr:mac/s1/w1:p1","reasons":["agent is working"]}}`
	code, stdout, stderr := runCmdIn([]string{"lease", "forward"}, env, strings.NewReader(req))
	if code != 0 {
		t.Fatalf("lease forward = %d, %q", code, stderr)
	}
	got, ok := bus.find("hibernate-refused")
	if !ok || got.Hibernate == nil || got.Hibernate.Handle != "herdr:mac/s1/w1:p1" {
		t.Fatalf("forwarded = %+v", got)
	}
	if !strings.Contains(stdout, `"ok":true`) {
		t.Fatalf("stdout = %q", stdout)
	}
}
