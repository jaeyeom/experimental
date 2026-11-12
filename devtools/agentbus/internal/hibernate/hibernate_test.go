package hibernate

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

const mib = 1024 * 1024

const (
	alice = "herdr:local/s1/w1:p1"
	bob   = "herdr:local/s2/w1:p2"
)

func t0() time.Time { return time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC) }

// fakeClock moves only when Sleep is called.
type fakeClock struct{ now time.Time }

func (c *fakeClock) Now() time.Time { return c.now }

func (c *fakeClock) Sleep(_ context.Context, d time.Duration) error {
	c.now = c.now.Add(d)
	return nil
}

// fakePanes is a local herdr. A pane in exits drops its agent process
// from the foreground once Enter is pressed in it.
type fakePanes struct {
	agents  []adapter.Agent
	infos   map[string]herdr.ProcessInfo
	screens map[string]string
	exits   map[string]bool
	calls   []string
}

func (f *fakePanes) Inventory(context.Context) ([]adapter.Agent, error) { return f.agents, nil }

func (f *fakePanes) ProcessInfo(_ context.Context, pane string) (herdr.ProcessInfo, error) {
	f.calls = append(f.calls, "process-info "+pane)
	info, ok := f.infos[pane]
	if !ok {
		return herdr.ProcessInfo{}, errors.New("pane_not_found")
	}
	return info, nil
}

func (f *fakePanes) Screen(_ context.Context, pane string) (string, error) {
	f.calls = append(f.calls, "screen "+pane)
	return f.screens[pane], nil
}

func (f *fakePanes) SendText(_ context.Context, pane, text string) error {
	f.calls = append(f.calls, "send-text "+pane+" "+text)
	return nil
}

func (f *fakePanes) SendKeys(_ context.Context, pane string, keys ...string) error {
	f.calls = append(f.calls, "send-keys "+pane+" "+strings.Join(keys, " "))
	if f.exits[pane] && slices.Contains(keys, "enter") {
		f.infos[pane] = herdr.ProcessInfo{ShellPID: 1, ForegroundProcessGroupID: 1, ForegroundProcesses: []herdr.ForegroundProcess{{PID: 1, Name: "zsh", Argv: []string{"-zsh"}, Cwd: "/src/repo"}}}
	}
	return nil
}

func (f *fakePanes) RunInPane(_ context.Context, pane, command string) error {
	f.calls = append(f.calls, "run "+pane+" "+command)
	return nil
}

// inputs are the calls that type into a pane.
func (f *fakePanes) inputs() []string {
	var out []string
	for _, c := range f.calls {
		if strings.HasPrefix(c, "send-") || strings.HasPrefix(c, "run ") {
			out = append(out, c)
		}
	}
	return out
}

type fakeBus struct {
	state   Reply
	begun   []Record
	ended   []string
	refused []string
}

func (b *fakeBus) State(context.Context) (Reply, error) { return b.state, nil }

func (b *fakeBus) Begin(_ context.Context, rec Record) (Record, error) {
	rec.Status = StatusStopping
	b.begun = append(b.begun, rec)
	return rec, nil
}

func (b *fakeBus) End(_ context.Context, handle, status, reason string) error {
	b.ended = append(b.ended, handle+" "+status+" "+reason)
	return nil
}

func (b *fakeBus) Refused(_ context.Context, handle string, reasons []string) error {
	b.refused = append(b.refused, handle+": "+strings.Join(reasons, "; "))
	return nil
}

func claudeScreen(box string) string {
	const rule = "──────────────────────────────"
	return "⏺ Done.\n" + rule + "\n" + box + "\n" + rule + "\n  [Opus] repo\n"
}

func info(pid int, cwd string, argv ...string) herdr.ProcessInfo {
	return herdr.ProcessInfo{ShellPID: 1, ForegroundProcessGroupID: pid, ForegroundProcesses: []herdr.ForegroundProcess{{PID: pid, Name: "2.1.319", Argv: argv, Cwd: cwd}}}
}

type world struct {
	panes *fakePanes
	bus   *fakeBus
	clock *fakeClock
	procs []procinfo.Process
	cwds  map[int]string
	h     *Hibernator
}

// newWorld has alice, an idle claude named ops/x in w1:p1 (pid 100,
// 300 MiB with a 20 MiB helper), and bob, an idle claude named ops/y in
// w1:p2 (pid 200). Each pane has its own shell: pid 1 for w1:p1, pid 2
// for w1:p2. The daemon has seen both idle for three hours.
func newWorld() *world {
	w := &world{
		panes: &fakePanes{
			agents: []adapter.Agent{
				{Handle: alice, PaneID: "w1:p1", Kind: "claude", Status: "idle", SessionID: "s1"},
				{Handle: bob, PaneID: "w1:p2", Kind: "claude", Status: "idle", SessionID: "s2"},
			},
			infos: map[string]herdr.ProcessInfo{
				"w1:p1": info(100, "/src/repo", "claude", "--model", "opus"),
				"w1:p2": func() herdr.ProcessInfo {
					i := info(200, "/src/other", "claude")
					i.ShellPID = 2
					return i
				}(),
			},
			screens: map[string]string{"w1:p1": claudeScreen("❯ "), "w1:p2": claudeScreen("❯ ")},
			exits:   map[string]bool{"w1:p1": true, "w1:p2": true},
		},
		bus: &fakeBus{state: Reply{
			Names: map[string][]string{alice: {"ops/x"}, bob: {"ops/y"}},
			Seen: map[string]Seen{
				alice: {Status: "idle", For: 3 * time.Hour},
				bob:   {Status: "idle", For: 3 * time.Hour},
			},
		}},
		clock: &fakeClock{now: t0()},
		procs: []procinfo.Process{
			{PID: 1, PPID: 0, Name: "zsh", Argv: []string{"-zsh"}, Age: -1},
			{PID: 100, PPID: 1, Name: "claude", Argv: []string{"claude"}, RSS: 300 * mib, Age: 3 * time.Hour},
			{PID: 101, PPID: 100, Name: "node", Argv: []string{"node", "/opt/github-mcp-server/index.js"}, RSS: 20 * mib, Age: time.Hour},
			{PID: 2, PPID: 0, Name: "zsh", Argv: []string{"-zsh"}, Age: -1},
			{PID: 200, PPID: 2, Name: "claude", Argv: []string{"claude"}, RSS: 100 * mib, Age: 3 * time.Hour},
		},
		cwds: map[int]string{},
	}
	w.h = &Hibernator{
		Panes: w.panes,
		Bus:   w.bus,
		Procs: func(context.Context) ([]procinfo.Process, error) { return w.procs, nil },
		Cwd: func(_ context.Context, pid int) (string, error) {
			if c, ok := w.cwds[pid]; ok {
				return c, nil
			}
			return "", errors.New("no cwd")
		},
		Clock: w.clock,
	}
	return w
}

func (w *world) run(t *testing.T, req Request) []Result {
	t.Helper()
	results, err := w.h.Run(context.Background(), req)
	if err != nil {
		t.Fatal(err)
	}
	return results
}

func TestHibernatesAnIdleAgentAndKeepsItsPane(t *testing.T) {
	w := newWorld()
	results := w.run(t, Request{Targets: []string{"ops/x"}})
	if len(results) != 1 || results[0].Outcome != OutcomeHibernated {
		t.Fatalf("results = %+v", results)
	}
	if got, want := results[0].Line(), "hibernated ops/x (320.0 MiB freed)"; got != want {
		t.Fatalf("line = %q, want %q", got, want)
	}
	if len(w.bus.begun) != 1 {
		t.Fatalf("begun = %+v", w.bus.begun)
	}
	rec := w.bus.begun[0]
	if rec.Handle != alice || rec.Kind != "claude" || rec.Session != "s1" || rec.Cwd != "/src/repo" ||
		!slices.Equal(rec.Argv, []string{"claude", "--model", "opus"}) || !slices.Equal(rec.Names, []string{"ops/x"}) ||
		rec.RSSBytes != 320*mib || !rec.At.Equal(t0()) || rec.Pane != "w1:p1" {
		t.Fatalf("record = %+v", rec)
	}
	wantInputs := []string{
		"send-text w1:p1 /exit",
		"send-keys w1:p1 enter",
		`run w1:p1 printf '%s\n' 'agentbus: hibernated ops/x; wakes on the next message or '\''agentbus wake ops/x'\'''`,
	}
	if got := w.panes.inputs(); !slices.Equal(got, wantInputs) {
		t.Fatalf("inputs = %q, want %q", got, wantInputs)
	}
	if !slices.Equal(w.bus.ended, []string{alice + " hibernated "}) {
		t.Fatalf("ended = %q", w.bus.ended)
	}
	if len(w.bus.refused) != 0 {
		t.Fatalf("refused = %q", w.bus.refused)
	}
}

func TestAHandleTargetWorksToo(t *testing.T) {
	w := newWorld()
	results := w.run(t, Request{Targets: []string{bob}})
	if len(results) != 1 || results[0].Outcome != OutcomeHibernated || results[0].Name != "ops/y" {
		t.Fatalf("results = %+v", results)
	}
}

func TestEachRefusalOnItsOwn(t *testing.T) {
	for _, tt := range []struct {
		name  string
		setup func(w *world)
		want  string
	}{
		{"working", func(w *world) { w.panes.agents[0].Status = "working" }, "agent is working"},
		{"blocked", func(w *world) { w.panes.agents[0].Status = "blocked" }, "agent is blocked"},
		{"dialog on screen", func(w *world) {
			w.panes.screens["w1:p1"] = "Do you want to proceed?\n ❯ 1. Yes\n   2. No\n"
		}, "pending dialog: permission prompt"},
		{"background job", func(w *world) {
			w.procs = append(w.procs, procinfo.Process{PID: 102, PPID: 100, Name: "sh", Argv: []string{"sh", "-c", "sleep 600"}, Age: time.Minute})
		}, "background job pid 102 sh -c \"sleep 600\""},
		{"own build tool in the cwd", func(w *world) {
			w.procs = append(w.procs, procinfo.Process{PID: 300, PPID: 1, Name: "bazel", Argv: []string{"bazel", "test", "//..."}, Age: time.Minute})
			w.cwds[300] = "/src/repo/sub"
		}, "own build tool pid 300 bazel test //... in /src/repo/sub"},
		{"own build tool under the agent", func(w *world) {
			w.procs = append(w.procs, procinfo.Process{PID: 300, PPID: 100, Name: "go", Argv: []string{"go", "test", "./..."}, Age: time.Minute})
			w.cwds[300] = "/src/repo"
		}, "own build tool pid 300 go test ./... in /src/repo"},
		{"checkout build while holding the lease", func(w *world) {
			otherBuild(w)
			w.bus.state.Leases = []lease.Status{{Checkout: "local:/src/repo", Holder: &lease.HolderView{Handle: alice}}}
		}, "checkout build pid 300 bazel test //... in /src/repo/sub (agent holds the lease)"},
		{"checkout build while queued for the lease", func(w *world) {
			otherBuild(w)
			w.bus.state.Leases = []lease.Status{{Checkout: "local:/src/repo", Holder: &lease.HolderView{Handle: bob}, Queue: []lease.WaiterView{{Handle: alice}}}}
		}, "checkout build pid 300 bazel test //... in /src/repo/sub (agent is queued for the lease)"},
		{"unsent text", func(w *world) { w.panes.screens["w1:p1"] = claudeScreen("❯ half a thought") }, "unsent text in the composer"},
		{"unknown kind", func(w *world) { w.panes.agents[0].Kind = "codex" }, "cannot read the composer of a codex agent"},
		{"held lease", func(w *world) {
			w.bus.state.Leases = []lease.Status{{Checkout: "local:/src/repo", Holder: &lease.HolderView{Handle: alice}}}
		}, "holds the lease on local:/src/repo"},
		{"queued lease", func(w *world) {
			w.bus.state.Leases = []lease.Status{{Checkout: "local:/src/repo", Holder: &lease.HolderView{Handle: bob}, Queue: []lease.WaiterView{{Handle: alice}}}}
		}, "queued for the lease on local:/src/repo"},
		{"exempt name", func(w *world) { w.h.Exempt = []string{"ops/*"} }, `exempt by hibernate_exempt "ops/*"`},
	} {
		t.Run(tt.name, func(t *testing.T) {
			w := newWorld()
			tt.setup(w)
			results := w.run(t, Request{Targets: []string{"ops/x"}})
			if len(results) != 1 || results[0].Outcome != OutcomeRefused {
				t.Fatalf("results = %+v", results)
			}
			if !slices.Contains(results[0].Reasons, tt.want) {
				t.Fatalf("reasons = %q, want %q", results[0].Reasons, tt.want)
			}
			if !strings.HasPrefix(results[0].Line(), "refused ops/x: ") {
				t.Fatalf("line = %q", results[0].Line())
			}
			if len(w.bus.begun) != 0 || len(w.panes.inputs()) != 0 {
				t.Fatalf("stopped anyway: begun %+v, inputs %q", w.bus.begun, w.panes.inputs())
			}
			if len(w.bus.refused) != 1 || !strings.Contains(w.bus.refused[0], tt.want) {
				t.Fatalf("refused events = %q", w.bus.refused)
			}
		})
	}
}

// otherBuild runs a bazel build in alice's checkout from bob's pane.
func otherBuild(w *world) {
	w.procs = append(w.procs, procinfo.Process{PID: 300, PPID: 2, Name: "bazel", Argv: []string{"bazel", "test", "//..."}, Age: time.Minute})
	w.cwds[300] = "/src/repo/sub"
}

func TestAnotherPanesBuildInTheCheckoutDoesNotBlock(t *testing.T) {
	for _, dryRun := range []bool{true, false} {
		w := newWorld()
		otherBuild(w)
		results := w.run(t, Request{Targets: []string{"ops/x"}, DryRun: dryRun})
		want := OutcomeHibernated
		if dryRun {
			want = OutcomeWould
		}
		if len(results) != 1 || results[0].Outcome != want {
			t.Fatalf("dry run %v: results = %+v", dryRun, results)
		}
		if dryRun && results[0].Line() != "would hibernate ops/x (320.0 MiB)" {
			t.Fatalf("line = %q", results[0].Line())
		}
	}
}

func TestAnotherPanesBuildDoesNotBlockTheIdlePolicyPick(t *testing.T) {
	w := newWorld()
	otherBuild(w)
	results := w.run(t, Request{IdleOver: 2 * time.Hour, DryRun: true})
	if len(results) != 2 || results[0].Outcome != OutcomeWould || results[1].Outcome != OutcomeWould {
		t.Fatalf("results = %+v", results)
	}
}

func TestRefusalNamesEveryFailedCheck(t *testing.T) {
	w := newWorld()
	w.panes.screens["w1:p1"] = claudeScreen("❯ draft")
	w.h.Exempt = []string{alice}
	results := w.run(t, Request{Targets: []string{"ops/x"}})
	if got := results[0].Reasons; len(got) != 2 {
		t.Fatalf("reasons = %q, want the draft and the exemption", got)
	}
}

func TestUnknownTargetIsRefused(t *testing.T) {
	w := newWorld()
	results := w.run(t, Request{Targets: []string{"ops/nobody"}})
	if len(results) != 1 || results[0].Outcome != OutcomeRefused || results[0].Name != "ops/nobody" {
		t.Fatalf("results = %+v", results)
	}
}

func TestIdleOverPicksOnlyAgentsIdleLongEnough(t *testing.T) {
	w := newWorld()
	w.bus.state.Seen[bob] = Seen{Status: "idle", For: time.Hour}
	results := w.run(t, Request{IdleOver: 2 * time.Hour})
	if len(results) != 1 || results[0].Handle != alice || results[0].Outcome != OutcomeHibernated {
		t.Fatalf("results = %+v", results)
	}
}

func TestIdleOverSkipsAgentsTheDaemonHasNotSeen(t *testing.T) {
	w := newWorld()
	delete(w.bus.state.Seen, bob)
	results := w.run(t, Request{IdleOver: 2 * time.Hour, DryRun: true})
	if len(results) != 1 || results[0].Handle != alice {
		t.Fatalf("results = %+v", results)
	}
}

func TestDryRunChangesNothing(t *testing.T) {
	w := newWorld()
	w.panes.screens["w1:p2"] = claudeScreen("❯ draft")
	results := w.run(t, Request{IdleOver: 2 * time.Hour, DryRun: true})
	if len(results) != 2 {
		t.Fatalf("results = %+v", results)
	}
	if got, want := results[0].Line(), "would hibernate ops/x (320.0 MiB)"; got != want {
		t.Fatalf("line = %q, want %q", got, want)
	}
	if results[1].Outcome != OutcomeRefused {
		t.Fatalf("bob = %+v", results[1])
	}
	if len(w.bus.begun) != 0 || len(w.bus.ended) != 0 || len(w.bus.refused) != 0 || len(w.panes.inputs()) != 0 {
		t.Fatalf("dry run changed: begun %+v ended %q refused %q inputs %q", w.bus.begun, w.bus.ended, w.bus.refused, w.panes.inputs())
	}
}

func TestStopTimeoutKeepsTheRecordAndKillsNothing(t *testing.T) {
	w := newWorld()
	w.panes.exits["w1:p1"] = false
	results := w.run(t, Request{Targets: []string{"ops/x"}})
	if len(results) != 1 || results[0].Outcome != OutcomeFailed {
		t.Fatalf("results = %+v", results)
	}
	if !strings.Contains(results[0].Line(), "did not exit within 20s") {
		t.Fatalf("line = %q", results[0].Line())
	}
	if len(w.bus.ended) != 1 || !strings.HasPrefix(w.bus.ended[0], alice+" "+StatusStopFailed+" ") {
		t.Fatalf("ended = %q", w.bus.ended)
	}
	if got := w.panes.inputs(); !slices.Equal(got, []string{"send-text w1:p1 /exit", "send-keys w1:p1 enter"}) {
		t.Fatalf("inputs = %q, want only the exit input", got)
	}
	if elapsed := w.clock.now.Sub(t0()); elapsed < DefaultStopTimeout {
		t.Fatalf("waited %s, want at least %s", elapsed, DefaultStopTimeout)
	}
}

func TestHubModeUsesTheMachineHandle(t *testing.T) {
	w := newWorld()
	hubAlice := "herdr:mac/s1/w1:p1"
	w.h.Machine = "mac"
	w.bus.state.Names = map[string][]string{hubAlice: {"ops/x"}}
	w.bus.state.Seen = map[string]Seen{hubAlice: {Status: "idle", For: 3 * time.Hour}}
	results := w.run(t, Request{Targets: []string{"ops/x"}})
	if len(results) != 1 || results[0].Outcome != OutcomeHibernated || results[0].Handle != hubAlice {
		t.Fatalf("results = %+v", results)
	}
	if w.bus.begun[0].Handle != hubAlice {
		t.Fatalf("record handle = %q", w.bus.begun[0].Handle)
	}
}

func TestAlreadyHibernatedIsRefused(t *testing.T) {
	w := newWorld()
	w.panes.agents = w.panes.agents[1:]
	w.bus.state.Records = []Record{{Handle: alice, Status: StatusHibernated}}
	results := w.run(t, Request{Targets: []string{"ops/x"}})
	if len(results) != 1 || !slices.Contains(results[0].Reasons, "already hibernated") {
		t.Fatalf("results = %+v", results)
	}
}
