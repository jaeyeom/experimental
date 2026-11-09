package main

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func TestRegistryCommandsSurviveRestart(t *testing.T) {
	runtime := shortRuntime(t)
	stateHome := t.TempDir()
	home := t.TempDir()
	base := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + stateHome,
		"HOME=" + home,
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	holder := "herdr:local/default/w1:p1"
	claimer := "herdr:local/default/w1:p2"
	third := "herdr:local/default/w1:p3"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			holder:  adapter.StateIdle,
			claimer: adapter.StateIdle,
			third:   adapter.StateIdle,
		},
		PaneHandles: map[string]string{"p1": holder, "p2": claimer, "p3": third},
	}
	var stop func()
	t.Cleanup(func() {
		if stop != nil {
			stop()
			stop = nil
		}
	})
	stop = startBus(t, dirs, ad)
	env := func(pane string) []string {
		if pane == "" {
			return append([]string{}, base...)
		}
		return append(append([]string{}, base...), "HERDR_PANE_ID="+pane)
	}
	if code, _, stderr := runCmd([]string{"register", "human/ops", "--when", "nope"}, env("p1")); code == 0 || !strings.Contains(stderr, "human:* is reserved") {
		t.Fatalf("human register = %d %q", code, stderr)
	}
	if code, _, stderr := runCmd([]string{"register", "ops/inbox", "--when", "nope"}, env("")); code == 0 || !strings.Contains(stderr, "outside herdr") || !strings.Contains(stderr, "cannot register") {
		t.Fatalf("outside register = %d %q", code, stderr)
	}

	stdout, _ := mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews"}, env("p1"))
	if !strings.Contains(stdout, "registered ops/inbox") {
		t.Fatalf("stdout = %q", stdout)
	}
	assertRegistryFields(t, dirs.StateDir)
	if code, _, stderr := runCmd([]string{"release", "ops/inbox"}, env("p2")); code == 0 || !strings.Contains(stderr, "only the holder") {
		t.Fatalf("non-holder release = %d %q", code, stderr)
	}
	mustCmd(t, []string{"describe", "--when", "updated"}, env("p1"))
	who, _ := mustCmd(t, []string{"whoami"}, env("p1"))
	if !strings.Contains(who, "ops/inbox") || !strings.Contains(who, "when: updated") {
		t.Fatalf("whoami = %q", who)
	}

	alive := "ops/inbox is held by " + holder + " (alive); message them with: agentbus send ops/inbox"
	if code, _, stderr := runCmd([]string{"register", "ops/inbox", "--when", "steal"}, env("p2")); code == 0 || !strings.Contains(stderr, alive) {
		t.Fatalf("live refusal = %d %q", code, stderr)
	}
	ad.Reach[holder] = adapter.StateUnknown
	unreachable := "ops/inbox is held by " + holder + " (unreachable); message them with: agentbus send ops/inbox"
	if code, _, stderr := runCmd([]string{"register", "ops/inbox", "--when", "steal"}, env("p2")); code == 0 || !strings.Contains(stderr, unreachable) {
		t.Fatalf("unreachable refusal = %d %q", code, stderr)
	}
	who, _ = mustCmd(t, []string{"whoami"}, env("p1"))
	if !strings.Contains(who, "ops/inbox") {
		t.Fatalf("holder lost the name while unreachable: %q", who)
	}

	ad.Reach[holder] = adapter.StateIdle
	stdout, _ = mustCmd(t, []string{"release", "ops/inbox", "--to", claimer}, env("p1"))
	if !strings.Contains(stdout, "handed off ops/inbox to "+claimer) {
		t.Fatalf("handoff stdout = %q", stdout)
	}
	thirdRefusal := "ops/inbox is held by " + claimer + " (alive); message them with: agentbus send ops/inbox"
	if code, _, stderr := runCmd([]string{"register", "ops/inbox", "--when", "steal"}, env("p3")); code == 0 || !strings.Contains(stderr, thirdRefusal) {
		t.Fatalf("third refusal = %d %q", code, stderr)
	}
	who, _ = mustCmd(t, []string{"whoami"}, env("p2"))
	if !strings.Contains(who, "ops/inbox") || !strings.Contains(who, "when: updated") {
		t.Fatalf("claimer whoami = %q", who)
	}

	ad.Reach[claimer] = adapter.StateGone
	mustCmd(t, []string{"register", "ops/inbox", "--when", "again"}, env("p1"))
	stop()
	stop = nil
	stop = startBus(t, dirs, ad)
	who, _ = mustCmd(t, []string{"whoami"}, env("p1"))
	if !strings.Contains(who, "ops/inbox") || !strings.Contains(who, "when: again") {
		t.Fatalf("whoami after restart = %q", who)
	}
	stdout, _ = mustCmd(t, []string{"release", "ops/inbox"}, env("p1"))
	if !strings.Contains(stdout, "released ops/inbox") {
		t.Fatalf("release stdout = %q", stdout)
	}
	who, _ = mustCmd(t, []string{"whoami"}, env("p1"))
	if strings.TrimSpace(who) != holder {
		t.Fatalf("whoami after release = %q", who)
	}

	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: eventlog.EventClaimRefused})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) < 3 {
		t.Fatalf("claim_refused lines = %v", lines)
	}
	handoffs, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: eventlog.EventHandoff})
	if err != nil {
		t.Fatal(err)
	}
	if len(handoffs) != 1 {
		t.Fatalf("handoff lines = %v", handoffs)
	}
}

func assertRegistryFields(t *testing.T, stateDir string) {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(stateDir, "registry.json"))
	if err != nil {
		t.Fatal(err)
	}
	var doc struct {
		Names map[string]map[string]json.RawMessage `json:"names"`
	}
	if err := json.Unmarshal(data, &doc); err != nil {
		t.Fatal(err)
	}
	fields := doc.Names["ops/inbox"]
	if len(fields) != 2 {
		t.Fatalf("registry fields = %v", fields)
	}
	if _, ok := fields["handle"]; !ok {
		t.Fatalf("registry fields = %v", fields)
	}
	if _, ok := fields["when"]; !ok {
		t.Fatalf("registry fields = %v", fields)
	}
}

func mustCmd(t *testing.T, args, env []string) (string, string) {
	t.Helper()
	code, stdout, stderr := runCmd(args, env)
	if code != 0 {
		t.Fatalf("run %q = %d\nstdout %s\nstderr %s", args, code, stdout, stderr)
	}
	return stdout, stderr
}

func runCmd(args, env []string) (int, string, string) {
	return runCmdIn(args, env, strings.NewReader(""))
}

func runCmdIn(args, env []string, stdin io.Reader) (int, string, string) {
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), args, env, stdin, &stdout, &stderr)
	return code, stdout.String(), stderr.String()
}

func startBus(t *testing.T, dirs paths.Dirs, ad adapter.Adapter) func() {
	t.Helper()
	return startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}})
}

func startDaemon(t *testing.T, dirs paths.Dirs, cfg daemon.Config) func() {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	errc := make(chan error, 1)
	go func() {
		errc <- daemon.Serve(ctx, dirs, cfg)
	}()
	deadline := time.Now().Add(2 * time.Second)
	for {
		_, err := client.Call(context.Background(), dirs.SocketPath, daemon.Request{Op: "ping"})
		if err == nil {
			break
		}
		if time.Now().After(deadline) {
			cancel()
			t.Fatalf("daemon did not listen: %v", err)
		}
		time.Sleep(10 * time.Millisecond)
	}
	return func() {
		cancel()
		select {
		case err := <-errc:
			if err != nil {
				t.Errorf("Serve: %v", err)
			}
		case <-time.After(2 * time.Second):
			t.Error("Serve did not return")
		}
	}
}

func shortRuntime(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "ab")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}
