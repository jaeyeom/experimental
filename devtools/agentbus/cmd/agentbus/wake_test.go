package main

import (
	"context"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// cmdWaker counts the wakes the daemon asks for.
type cmdWaker struct {
	mu   sync.Mutex
	recs []hibernate.Record
}

func (w *cmdWaker) Wake(_ context.Context, rec hibernate.Record) error {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.recs = append(w.recs, rec)
	return nil
}

func (w *cmdWaker) count() int {
	w.mu.Lock()
	defer w.mu.Unlock()
	return len(w.recs)
}

// startWakeBus runs a daemon with ops/x hibernated in w1:p1.
func startWakeBus(t *testing.T) ([]string, *cmdWaker) {
	t.Helper()
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	const alice = "herdr:local/s1/w1:p1"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{alice: adapter.StateIdle},
		Result:      map[string]adapter.Result{alice: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": alice},
	}
	waker := &cmdWaker{}
	stop := startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour, Waker: waker})
	t.Cleanup(stop)
	mustCmd(t, []string{"register", "ops/x", "--when", "reviews"}, append(append([]string{}, base...), "HERDR_PANE_ID=p1"))
	ctx := context.Background()
	rec := &hibernate.Record{Handle: alice, Pane: "w1:p1", Kind: "claude", Session: "s1", Cwd: "/src", Argv: []string{"claude"}}
	for _, req := range []daemon.Request{
		{Op: "hibernate-begin", Hibernate: &hibernate.Args{Record: rec}},
		{Op: "hibernate-end", Hibernate: &hibernate.Args{Handle: alice, Status: hibernate.StatusHibernated}},
	} {
		if resp, err := client.Call(ctx, dirs.SocketPath, req); err != nil || !resp.OK {
			t.Fatalf("%s = %+v, %v", req.Op, resp, err)
		}
	}
	return base, waker
}

func TestWakeCommandRelaunchesWithNoMessage(t *testing.T) {
	env, waker := startWakeBus(t)
	out, _ := mustCmd(t, []string{"wake", "ops/x"}, env)
	if strings.TrimSpace(out) != "woke ops/x (herdr:local/s1/w1:p1)" {
		t.Fatalf("wake = %q", out)
	}
	if waker.count() != 1 {
		t.Fatalf("relaunched %d times", waker.count())
	}
	listed, _ := mustCmd(t, []string{"list"}, env)
	if strings.Contains(listed, "hibernated") {
		t.Fatalf("list after the wake = %q", listed)
	}
	code, _, stderr := runCmd([]string{"wake", "ops/x"}, env)
	if code != 1 || !strings.Contains(stderr, "not hibernated") {
		t.Fatalf("second wake = %d %q", code, stderr)
	}
}

func TestWakeCommandUsage(t *testing.T) {
	for _, args := range [][]string{{"wake"}, {"wake", "a", "b"}, {"wake", "--now"}} {
		if code, _, stderr := runCmd(args, nil); code != 2 || !strings.Contains(stderr, "usage: agentbus wake <name|handle>") {
			t.Errorf("%q = %d %q", args, code, stderr)
		}
	}
}

func TestSendToAHibernatedNameSaysItIsWaking(t *testing.T) {
	env, waker := startWakeBus(t)
	out, stderr := mustCmd(t, []string{"send", "ops/x", "hello"}, env)
	if !strings.Contains(out, "held: waking") || !strings.Contains(stderr, "waking it") {
		t.Fatalf("send = %q / %q", out, stderr)
	}
	id, _, _ := strings.Cut(strings.TrimSpace(out), " ")
	deadline := time.Now().Add(5 * time.Second)
	for {
		status, _ := mustCmd(t, []string{"status", id}, env)
		if strings.Contains(status, "state: delivered") {
			break
		}
		if time.Now().After(deadline) {
			t.Fatalf("status = %q, want delivered after the wake", status)
		}
		time.Sleep(10 * time.Millisecond)
	}
	if waker.count() != 1 {
		t.Fatalf("relaunched %d times", waker.count())
	}
}

func TestWakeRunsOnTheHub(t *testing.T) {
	if !tunnelCommands["wake"] || localCommands["wake"] {
		t.Fatal("wake must go to the hub's daemon in hub mode")
	}
}
