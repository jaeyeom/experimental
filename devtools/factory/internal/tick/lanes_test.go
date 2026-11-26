package tick

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const macCheckout = "/path/on/mac/checkout"

// laneEnv is a tickEnv with a local linux lane and a remote mac lane.
type laneEnv struct {
	*tickEnv
	mac      *fakeHerd
	macDown  bool     // The mac machine does not answer.
	machines []string // Machines Engine.Machine was asked for.
}

// downHerd is a herdr machine that does not answer.
type downHerd struct{ *fakeHerd }

var errMacDown = &herdr.RunError{Bin: "herdr", Err: errors.New("timed out after 15s")}

func (d downHerd) Tabs(context.Context) ([]herdr.Tab, error)   { return nil, errMacDown }
func (d downHerd) Panes(context.Context) ([]herdr.Pane, error) { return nil, errMacDown }
func (d downHerd) Workspaces(context.Context) ([]herdr.Workspace, error) {
	return nil, errMacDown
}

func newLaneEnv(t *testing.T, items []source.Item, apply bool, linuxMax int) *laneEnv {
	t.Helper()
	env := &laneEnv{tickEnv: newTickEnv(t, items, apply, false)}
	env.mac = &fakeHerd{workspaces: []herdr.Workspace{{ID: "w1", Label: "factory", AgentStatus: herdr.AgentStatusIdle}}}
	cfg := env.eng.Config
	cfg.Lanes = []config.Lane{
		{Name: "linux", Checkout: cfg.Sources[0].Cwd, MaxWorkers: linuxMax, Labels: []string{"needs-linux"}},
		{Name: "mac", Machine: "mac1", Checkout: macCheckout, MaxWorkers: 2, Labels: []string{"needs-mac"}},
	}
	cfg.LaneOrder = config.LaneOrder{Default: []string{"linux", "mac"}}
	env.eng.Machine = func(machine string) (LaneMachine, error) {
		env.machines = append(env.machines, machine)
		if machine != "mac1" {
			return LaneMachine{}, fmt.Errorf("machine: expected mac1, found %q", machine)
		}
		var sessions Sessions = env.mac
		if env.macDown {
			sessions = downHerd{env.mac}
		}
		return LaneMachine{
			Sessions: sessions, Closer: env.mac, Agent: env.mac,
			Why: func(_ context.Context, err error) string { return "host unreachable: " + err.Error() },
		}, nil
	}
	return env
}

func labeled(id string, labels ...string) source.Item {
	it := readyItem(id, "Ship "+id)
	it.Labels = labels
	return it
}

func seedLaneClaim(t *testing.T, env *laneEnv, id, lane, tabID string) {
	t.Helper()
	at := env.now.Add(-time.Hour)
	if err := env.st.InsertClaim(context.Background(), store.Claim{
		Source: "jira:acme", ID: id, ClaimedAt: at, Machine: "test-host",
		TabID: tabID, WorkspaceID: "w1", State: store.Started, UpdatedAt: at, Lane: lane,
	}); err != nil {
		t.Fatal(err)
	}
}

func TestTickLanesOneStartPerLaneByLabel(t *testing.T) {
	env := newLaneEnv(t, []source.Item{labeled("PRJ-1", "needs-mac"), labeled("PRJ-2", "needs-linux")}, true, 2)
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	out := env.out.String()
	for _, want := range []string{
		"started PRJ-2 in factory/PRJ-2 (lane linux)\n",
		"started PRJ-1 in factory/PRJ-1 (lane mac)\n",
	} {
		if !strings.Contains(out, want) {
			t.Fatalf("stdout = %q, want %q", out, want)
		}
	}
	if got := env.herd.createdTabs(); !slices.Equal(got, []string{"PRJ-2"}) {
		t.Fatalf("local tabs = %v", got)
	}
	if got := env.mac.createdTabs(); !slices.Equal(got, []string{"PRJ-1"}) {
		t.Fatalf("mac tabs = %v", got)
	}
	if c := claimRow(t, env.st, "PRJ-1"); c.Lane != "mac" || c.State != store.Started || c.StashCount != nil {
		t.Fatalf("PRJ-1 claim = %+v", c)
	}
	if c := claimRow(t, env.st, "PRJ-2"); c.Lane != "linux" || c.State != store.Started {
		t.Fatalf("PRJ-2 claim = %+v", c)
	}
	assertLaneRuns(t, env, map[string]string{
		"started PRJ-2 in factory/PRJ-2 (lane linux)": outcomeStarted,
		"started PRJ-1 in factory/PRJ-1 (lane mac)":   outcomeStarted,
	})
}

func TestTickLanesFullLaneGetsNoStart(t *testing.T) {
	env := newLaneEnv(t, []source.Item{labeled("PRJ-1", "needs-mac"), labeled("PRJ-2", "needs-linux"), labeled("PRJ-9", "needs-linux")}, true, 1)
	seedLaneClaim(t, env, "PRJ-9", "linux", "")
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	out := env.out.String()
	if !strings.Contains(out, "lane linux: gate: ") || !strings.Contains(out, "started PRJ-1 in factory/PRJ-1 (lane mac)\n") {
		t.Fatalf("stdout = %q", out)
	}
	if strings.Contains(out, "started PRJ-2") || len(env.herd.createdTabs()) != 0 {
		t.Fatalf("full linux lane started: stdout = %q tabs = %v", out, env.herd.createdTabs())
	}
}

func TestTickLanesSkipsUnreachableLane(t *testing.T) {
	env := newLaneEnv(t, []source.Item{labeled("PRJ-1"), labeled("PRJ-2", "needs-mac")}, true, 2)
	env.macDown = true
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	out := env.out.String()
	want := "lane mac: skipped: host unreachable: " + errMacDown.Error() + "\n"
	if !strings.Contains(out, "started PRJ-1 in factory/PRJ-1 (lane linux)\n") || !strings.Contains(out, want) {
		t.Fatalf("stdout = %q, want %q", out, want)
	}
	if len(env.mac.createdTabs()) != 0 {
		t.Fatalf("mac tabs = %v", env.mac.createdTabs())
	}
	assertLaneRuns(t, env, map[string]string{
		"started PRJ-1 in factory/PRJ-1 (lane linux)": outcomeStarted,
		strings.TrimSuffix(want, "\n"):                outcomeSkipped,
	})
}

func TestTickLanesMaxInFlightCapsStarts(t *testing.T) {
	env := newLaneEnv(t, []source.Item{labeled("PRJ-1", "needs-linux"), labeled("PRJ-2", "needs-mac")}, true, 2)
	env.eng.Config.Defaults.MaxInFlight = 1
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	out := env.out.String()
	if !strings.Contains(out, "started PRJ-1 in factory/PRJ-1 (lane linux)\n") ||
		!strings.Contains(out, "lane mac: gate: max_in_flight 1 reached by this tick's starts\n") {
		t.Fatalf("stdout = %q", out)
	}
	if len(env.mac.createdTabs()) != 0 || len(env.machines) != 0 {
		t.Fatalf("mac was used: tabs = %v machines = %v", env.mac.createdTabs(), env.machines)
	}
}

func TestSettleLanesClosesTabOnEachMachine(t *testing.T) {
	env := newLaneEnv(t, []source.Item{readyItem("PRJ-1", "a"), readyItem("PRJ-2", "b")}, true, 2)
	env.herd.tabs = []herdr.Tab{{ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusIdle}}
	env.mac.tabs = []herdr.Tab{{ID: "w1:t7", WorkspaceID: "w1", Label: "PRJ-2", AgentStatus: herdr.AgentStatusDone}}
	seedLaneClaim(t, env, "PRJ-1", "linux", "w1:t1")
	seedLaneClaim(t, env, "PRJ-2", "mac", "w1:t7")
	one, two := pullRequest("[PRJ-1] a", true), pullRequest("[PRJ-2] b", true)
	two.Number, two.URL = 2, "https://example.test/pull/2"
	env.pulls.merged = []gh.PR{one, two}
	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	for _, id := range []string{"PRJ-1", "PRJ-2"} {
		if c := claimRow(t, env.st, id); c.State != store.Done {
			t.Fatalf("%s claim = %+v", id, c)
		}
	}
	if got := env.herd.closedTabs(); !slices.Equal(got, []string{"w1:t1"}) {
		t.Fatalf("local closed = %v", got)
	}
	if got := env.mac.closedTabs(); !slices.Equal(got, []string{"w1:t7"}) {
		t.Fatalf("mac closed = %v", got)
	}
	if !slices.Equal(env.machines, []string{"mac1"}) {
		t.Fatalf("machines = %v, want [mac1]", env.machines)
	}
	out := env.out.String()
	for _, want := range []string{"closed tab PRJ-1 (w1:t1) on lane linux\n", "closed tab PRJ-2 (w1:t7) on lane mac\n"} {
		if !strings.Contains(out, want) {
			t.Fatalf("stdout = %q, want %q", out, want)
		}
	}
	runs, err := env.st.Runs(context.Background(), 10)
	if err != nil {
		t.Fatal(err)
	}
	var settled []string
	for _, run := range runs {
		if run.Command == settleCommand && run.Outcome == outcomeSettled {
			settled = append(settled, run.DetailJSON)
		}
	}
	if len(settled) != 2 || !strings.Contains(strings.Join(settled, " "), `settled PRJ-2 (lane mac)`) ||
		!strings.Contains(strings.Join(settled, " "), `settled PRJ-1 (lane linux)`) {
		t.Fatalf("settle runs = %v", settled)
	}
}

func TestSettleLanesUnreachableMachineStillSettles(t *testing.T) {
	env := newLaneEnv(t, []source.Item{readyItem("PRJ-2", "b")}, true, 2)
	env.macDown = true
	seedLaneClaim(t, env, "PRJ-2", "mac", "w1:t7")
	env.pulls.merged = []gh.PR{pullRequest("[PRJ-2] b", true)}
	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if c := claimRow(t, env.st, "PRJ-2"); c.State != store.Done {
		t.Fatalf("claim = %+v", c)
	}
	out := env.out.String()
	if !strings.HasPrefix(out, "lane mac: tab for PRJ-2 not checked: host unreachable: ") || !strings.Contains(out, errMacDown.Error()) {
		t.Fatalf("stdout = %q", out)
	}
	if len(env.mac.closedTabs()) != 0 {
		t.Fatalf("mac closed = %v", env.mac.closedTabs())
	}
}

func TestSettleLanesUnknownLaneIsAnError(t *testing.T) {
	env := newLaneEnv(t, []source.Item{readyItem("PRJ-2", "b")}, true, 2)
	seedLaneClaim(t, env, "PRJ-2", "gone", "w1:t7")
	env.pulls.merged = []gh.PR{pullRequest("[PRJ-2] b", true)}
	err := env.eng.Settle(context.Background(), nil)
	if err == nil || !strings.Contains(err.Error(), `claim lane "gone": expected a configured [[lane]], found none`) {
		t.Fatalf("err = %v", err)
	}
}

// assertLaneRuns checks that each summary has a tick run with that outcome.
func assertLaneRuns(t *testing.T, env *laneEnv, want map[string]string) {
	t.Helper()
	runs, err := env.st.Runs(context.Background(), 20)
	if err != nil {
		t.Fatal(err)
	}
	got := map[string]string{}
	for _, run := range runs {
		if run.Command != runCommand {
			continue
		}
		for summary := range want {
			if strings.Contains(run.DetailJSON, `"summary":"`+summary+`"`) {
				got[summary] = run.Outcome
			}
		}
	}
	for summary, outcome := range want {
		if got[summary] != outcome {
			t.Fatalf("run %q outcome = %q, want %q (runs %+v)", summary, got[summary], outcome, runs)
		}
	}
}
