package main

import (
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr/herdrtest"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

const remoteTarget = "herdr:m2/default/w2:p3"

// agentsJSON is a herdr agent list of unfocused codex panes in session
// default. Each pane is "pane=status".
func agentsJSON(panes ...string) string {
	items := make([]string, 0, len(panes))
	for _, entry := range panes {
		pane, status, _ := strings.Cut(entry, "=")
		items = append(items, fmt.Sprintf(
			`{"pane_id":%q,"tab_id":"w2:t1","agent":"codex","agent_status":%q,"terminal_title":%q,"focused":false,"agent_session":{"value":"default"}}`,
			pane, status, "title "+pane))
	}
	return `{"result":{"agents":[` + strings.Join(items, ",") + `]}}`
}

type remoteBus struct {
	base []string
	dirs paths.Dirs
	fake *herdrtest.Fake
}

// startRemoteBus runs a daemon with the live herdr adapter against a fake
// herdr. Local has w1:p1 and w1:p2; machine m2 has w2:p3. All are idle.
func startRemoteBus(t *testing.T, timeout, poll time.Duration) *remoteBus {
	t.Helper()
	fake := herdrtest.Install(t)
	fake.Write("local.agents", agentsJSON("w1:p1=idle", "w1:p2=idle"))
	fake.Write("m2.agents", agentsJSON("w2:p3=idle"))
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	ad := &herdr.Adapter{Machines: []string{"m2"}, Timeout: timeout}
	stop := startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: poll})
	t.Cleanup(stop)
	return &remoteBus{base: base, dirs: dirs, fake: fake}
}

func (b *remoteBus) env(pane string) []string {
	return append(append([]string{}, b.base...), "HERDR_PANE_ID="+pane)
}

func sendID(t *testing.T, env []string, target, body string) (string, string) {
	t.Helper()
	out, _ := mustCmd(t, []string{"send", target, body}, env)
	id, state, ok := strings.Cut(strings.TrimSpace(out), " ")
	if !ok {
		t.Fatalf("send = %q", out)
	}
	return id, state
}

func TestRemoteMessageHeldWhileMachineUnreachable(t *testing.T) {
	bus := startRemoteBus(t, 5*time.Second, time.Hour)
	bus.fake.Write("m2.down", "")
	id, state := sendID(t, bus.env("w1:p1"), remoteTarget, "remote-body")
	want := "held: machine m2 unreachable: " + remotecall.CauseHostUnreachable
	if state != want {
		t.Fatalf("send state = %q, want %q", state, want)
	}
	status, _ := mustCmd(t, []string{"status", id}, bus.base)
	if !strings.Contains(status, "state: "+want) {
		t.Fatalf("status = %q", status)
	}
	if logHas(t, bus.dirs, eventlog.EventRetry, id) {
		t.Fatal("an unreachable machine counted as a retry")
	}
	for _, argv := range bus.fake.Argv() {
		if strings.Contains(argv, "agent prompt") {
			t.Fatalf("pasted while m2 was down: %q", argv)
		}
	}

	bus.fake.Remove("m2.down")
	status, _ = mustCmd(t, []string{"status", id}, bus.base)
	if !strings.Contains(status, "state: delivered") {
		t.Fatalf("status after m2 came back = %q", status)
	}
	if !textsContain(bus.fake.Argv(), "--machine m2 agent prompt w2:p3 ") {
		t.Fatal("delivery did not use --machine m2")
	}
	if logHas(t, bus.dirs, eventlog.EventRetry, id) {
		t.Fatal("an unreachable machine counted as a retry")
	}
}

func TestHungMachineDoesNotHoldUpLocalDelivery(t *testing.T) {
	bus := startRemoteBus(t, 3*time.Second, 50*time.Millisecond)
	bus.fake.Write("local.agents", agentsJSON("w1:p1=idle", "w1:p2=blocked"))
	localID, state := sendID(t, bus.env("w1:p1"), "herdr:local/default/w1:p2", "local-body")
	if state != "held: blocked" {
		t.Fatalf("local send state = %q", state)
	}

	bus.fake.Write("m2.hang", "")
	start := time.Now()
	remoteID, state := sendID(t, bus.env("w1:p1"), remoteTarget, "remote-body")
	if took := time.Since(start); took > 8*time.Second {
		t.Fatalf("send to a hung machine took %s", took)
	}
	if want := "held: machine m2 unreachable: " + remotecall.CauseAuthLocked; state != want {
		t.Fatalf("remote send state = %q, want %q", state, want)
	}

	// The poller is now stuck on m2 for up to 3s per call. The local
	// pane unblocks, and its message must go out well before that.
	bus.fake.Write("local.agents", agentsJSON("w1:p1=idle", "w1:p2=idle"))
	deadline := time.Now().Add(2 * time.Second)
	for !logHas(t, bus.dirs, eventlog.EventDelivered, localID) {
		if time.Now().After(deadline) {
			t.Fatal("local message was not delivered while m2 hung")
		}
		time.Sleep(20 * time.Millisecond)
	}
	if logHas(t, bus.dirs, eventlog.EventDelivered, remoteID) {
		t.Fatal("remote message was delivered while m2 hung")
	}
}

func TestSearchIncludesConfiguredMachines(t *testing.T) {
	bus := startRemoteBus(t, 5*time.Second, 50*time.Millisecond)
	out, stderr := mustCmd(t, []string{"search"}, bus.base)
	handles := map[string]bool{}
	for _, row := range parseRows(out) {
		handles[row["handle"]] = true
	}
	for _, want := range []string{"herdr:local/default/w1:p1", "herdr:local/default/w1:p2", remoteTarget} {
		if !handles[want] {
			t.Fatalf("search is missing %s:\n%s", want, out)
		}
	}
	if stderr != "" {
		t.Fatalf("search stderr = %q", stderr)
	}

	// The poller refreshes the inventory in the background.
	bus.fake.Write("m2.down", "")
	var code int
	deadline := time.Now().Add(3 * time.Second)
	for {
		code, out, stderr = runCmd([]string{"search", "title"}, bus.base)
		if code != 0 {
			t.Fatalf("search with m2 down exited %d: %s", code, stderr)
		}
		if stderr != "" || time.Now().After(deadline) {
			break
		}
		time.Sleep(20 * time.Millisecond)
	}
	rows := parseRows(out)
	if len(rows) != 2 || strings.Contains(out, "herdr:m2/") {
		t.Fatalf("search with m2 down = %q", out)
	}
	if want := "agentbus: machine m2 unreachable: " + remotecall.CauseHostUnreachable + "\n"; stderr != want {
		t.Fatalf("search stderr = %q, want %q", stderr, want)
	}
}

func TestNameOnUnreachableMachineIsNotReleased(t *testing.T) {
	bus := startRemoteBus(t, 5*time.Second, time.Hour)
	mustCmd(t, []string{"register", "eng/remote", "--when", "runs on m2"}, bus.env("w1:p1"))
	mustCmd(t, []string{"release", "eng/remote", "--to", remoteTarget}, bus.env("w1:p1"))

	bus.fake.Write("m2.down", "")
	code, _, stderr := runCmd([]string{"register", "eng/remote", "--when", "takeover"}, bus.env("w1:p2"))
	if code == 0 || !strings.Contains(stderr, "held by "+remoteTarget) {
		t.Fatalf("register over an unreachable holder = %d %q", code, stderr)
	}

	// Reachable, with the pane gone: the name is free.
	bus.fake.Remove("m2.down")
	bus.fake.Write("m2.agents", agentsJSON())
	mustCmd(t, []string{"register", "eng/remote", "--when", "takeover"}, bus.env("w1:p2"))
}
