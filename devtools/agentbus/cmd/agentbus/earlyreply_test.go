package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// awaitDelivered waits until status shows id delivered. status flushes
// the queue, so a paste that started after the reply has finished.
func awaitDelivered(t *testing.T, env []string, id string) {
	t.Helper()
	status, _ := mustCmd(t, []string{"status", id}, env)
	if !strings.Contains(status, "state: delivered") {
		t.Fatalf("status %s = %q, want delivered", id, status)
	}
}

func messageCount(t *testing.T, env []string) int {
	t.Helper()
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	data, err := os.ReadFile(filepath.Join(dirs.StateDir, "messages.json"))
	if err != nil {
		t.Fatal(err)
	}
	var file struct {
		Messages []json.RawMessage `json:"messages"`
	}
	if err := json.Unmarshal(data, &file); err != nil {
		t.Fatal(err)
	}
	return len(file.Messages)
}

func hintedClientID(t *testing.T, stderr string) string {
	t.Helper()
	const needle = "check: agentbus status "
	i := strings.Index(stderr, needle)
	if i < 0 {
		t.Fatalf("stderr has no status hint: %q", stderr)
	}
	id, _, _ := strings.Cut(stderr[i+len(needle):], "\n")
	id = strings.TrimSpace(id)
	if id == "" {
		t.Fatalf("empty client id in %q", stderr)
	}
	return id
}

func TestSendAnswersBeforeASlowPaste(t *testing.T) {
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	sender := "herdr:local/default/w1:p1"
	reviewer := "herdr:local/default/w1:p2"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sender: adapter.StateIdle, reviewer: adapter.StateIdle},
		Result:      map[string]adapter.Result{sender: adapter.ResultAccepted, reviewer: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sender, "p2": reviewer},
		InventoryAgents: []adapter.Agent{
			{Handle: sender, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: reviewer, PaneID: "p2", SessionID: "default", Status: "idle"},
		},
		OnDeliver: func(string) { time.Sleep(time.Second) },
	}
	stop := startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer stop()
	env := func(pane string) []string {
		return append(append([]string{}, base...), "HERDR_PANE_ID="+pane)
	}
	mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews"}, env("p1"))
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, env("p2"))

	ctx, cancel := context.WithTimeout(context.Background(), 300*time.Millisecond)
	defer cancel()
	start := time.Now()
	var stdout, stderr bytes.Buffer
	code := run(ctx, []string{"send", "eng/reviewer", "slow-body"}, env("p1"), strings.NewReader(""), &stdout, &stderr)
	if elapsed := time.Since(start); code != 0 || !strings.Contains(stdout.String(), "queued for delivery") || elapsed >= 700*time.Millisecond {
		t.Fatalf("send = %d in %s stdout %q stderr %q", code, elapsed, stdout.String(), stderr.String())
	}
	id, _, ok := strings.Cut(strings.TrimSpace(stdout.String()), " ")
	if !ok || !strings.HasPrefix(id, "m-") {
		t.Fatalf("send = %q", stdout.String())
	}
	awaitDelivered(t, base, id)
	if ad.Calls != 1 || messageCount(t, base) != 1 {
		t.Fatalf("pastes = %d messages = %d", ad.Calls, messageCount(t, base))
	}

	ctx, cancel = context.WithTimeout(context.Background(), 300*time.Millisecond)
	defer cancel()
	start = time.Now()
	stdout.Reset()
	stderr.Reset()
	code = run(ctx, []string{"reply", id, "ack"}, env("p2"), strings.NewReader(""), &stdout, &stderr)
	if elapsed := time.Since(start); code != 0 || !strings.Contains(stdout.String(), "queued for delivery") || elapsed >= 700*time.Millisecond {
		t.Fatalf("reply = %d in %s stdout %q stderr %q", code, elapsed, stdout.String(), stderr.String())
	}
	replyID, _, ok := strings.Cut(strings.TrimSpace(stdout.String()), " ")
	if !ok || !strings.HasPrefix(replyID, "m-") || replyID == id {
		t.Fatalf("reply = %q", stdout.String())
	}
	awaitDelivered(t, base, replyID)
	if ad.Calls != 2 || messageCount(t, base) != 2 {
		t.Fatalf("pastes = %d messages = %d", ad.Calls, messageCount(t, base))
	}
}

func TestHibernatedSendReturnsHeldBeforePaste(t *testing.T) {
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
	pasted := make(chan struct{})
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{alice: adapter.StateIdle},
		Result:      map[string]adapter.Result{alice: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": alice},
		OnDeliver: func(string) {
			close(pasted)
			time.Sleep(200 * time.Millisecond)
		},
	}
	stop := startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer stop()
	env := append(append([]string{}, base...), "HERDR_PANE_ID=p1")
	mustCmd(t, []string{"register", "ops/x", "--when", "reviews"}, env)
	rec := &hibernate.Record{Handle: alice, Pane: "w1:p1", Kind: "claude", Session: "s1", Cwd: "/src", Argv: []string{"claude"}}
	for _, req := range []daemon.Request{
		{Op: "hibernate-begin", Hibernate: &hibernate.Args{Record: rec}},
		{Op: "hibernate-end", Hibernate: &hibernate.Args{Handle: alice, Status: hibernate.StatusHibernated}},
	} {
		resp, err := client.Call(context.Background(), dirs.SocketPath, req)
		if err != nil || !resp.OK {
			t.Fatalf("%s = %+v, %v", req.Op, resp, err)
		}
	}

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	start := time.Now()
	var stdout, stderr bytes.Buffer
	code := run(ctx, []string{"send", "ops/x", "wake-body"}, env, strings.NewReader(""), &stdout, &stderr)
	if elapsed := time.Since(start); code != 0 || !strings.Contains(stdout.String(), "held: hibernated") || elapsed >= 700*time.Millisecond {
		t.Fatalf("send = %d in %s stdout %q stderr %q", code, elapsed, stdout.String(), stderr.String())
	}
	select {
	case <-pasted:
		t.Fatal("pasted into a hibernated pane")
	case <-time.After(200 * time.Millisecond):
	}
	if ad.Calls != 0 {
		t.Fatalf("pastes = %d", ad.Calls)
	}
}

func TestSendWaitStillWaitsForDelivered(t *testing.T) {
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	sender := "herdr:local/default/w1:p1"
	reviewer := "herdr:local/default/w1:p2"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sender: adapter.StateIdle, reviewer: adapter.StateIdle},
		Result:      map[string]adapter.Result{sender: adapter.ResultAccepted, reviewer: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sender, "p2": reviewer},
		InventoryAgents: []adapter.Agent{
			{Handle: sender, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: reviewer, PaneID: "p2", SessionID: "default", Status: "idle"},
		},
		OnDeliver: func(string) { time.Sleep(400 * time.Millisecond) },
	}
	stop := startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer stop()
	env := append(append([]string{}, base...), "HERDR_PANE_ID=p1")
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, append(append([]string{}, base...), "HERDR_PANE_ID=p2"))

	start := time.Now()
	out, _ := mustCmd(t, []string{"send", "--wait", "eng/reviewer", "wait-body"}, env)
	elapsed := time.Since(start)
	if !strings.Contains(out, "delivered") || strings.Contains(out, "queued for delivery") || elapsed < 300*time.Millisecond {
		t.Fatalf("wait send = %q in %s", out, elapsed)
	}
	if ad.Calls != 1 {
		t.Fatalf("pastes = %d", ad.Calls)
	}
}

func TestUncertainStatusFindsTheStoredSendAndReply(t *testing.T) {
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	sender := "herdr:local/default/w1:p1"
	reviewer := "herdr:local/default/w1:p2"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sender: adapter.StateIdle, reviewer: adapter.StateIdle},
		Result:      map[string]adapter.Result{sender: adapter.ResultAccepted, reviewer: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sender, "p2": reviewer},
		InventoryAgents: []adapter.Agent{
			{Handle: sender, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: reviewer, PaneID: "p2", SessionID: "default", Status: "idle"},
		},
	}
	stop := startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer stop()
	env := func(pane string) []string {
		return append(append([]string{}, base...), "HERDR_PANE_ID="+pane)
	}
	mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews"}, env("p1"))
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, env("p2"))

	orig := dialDaemon
	dialDaemon = func(ctx context.Context, socket string, req daemon.Request) (daemon.Response, error) {
		resp, err := orig(ctx, socket, req)
		if req.Op == "send" || req.Op == "reply" {
			return daemon.Response{}, client.MarkSent(errors.New("read response: dropped"))
		}
		return resp, err
	}
	t.Cleanup(func() { dialDaemon = orig })

	code, stdout, stderr := runCmd([]string{"send", "eng/reviewer", "stored-body"}, env("p1"))
	if code == 0 || stdout != "" || !strings.Contains(stderr, "agentbus log --since 1m --name eng/reviewer") {
		t.Fatalf("send = %d stdout %q stderr %q", code, stdout, stderr)
	}
	sendID := hintedClientID(t, stderr)
	shown, _ := mustCmd(t, []string{"status", sendID}, base)
	if !strings.Contains(shown, "client_id: "+sendID) || !strings.Contains(shown, "id: m-") {
		t.Fatalf("status by client id = %q", shown)
	}
	msgID := statusMessageID(t, shown)
	body, _ := mustCmd(t, []string{"show", msgID}, base)
	if !strings.Contains(body, "stored-body") {
		t.Fatalf("show = %q", body)
	}

	code, stdout, stderr = runCmd([]string{"reply", msgID, "stored-reply"}, env("p2"))
	if code == 0 || stdout != "" || !strings.Contains(stderr, "agentbus log --since 1m --name ops/inbox") {
		t.Fatalf("reply = %d stdout %q stderr %q", code, stdout, stderr)
	}
	replyID := hintedClientID(t, stderr)
	shown, _ = mustCmd(t, []string{"status", replyID}, base)
	if !strings.Contains(shown, "client_id: "+replyID) {
		t.Fatalf("reply status = %q", shown)
	}
	replyMsg := statusMessageID(t, shown)
	body, _ = mustCmd(t, []string{"show", replyMsg}, base)
	if !strings.Contains(body, "stored-reply") || !strings.Contains(body, "re: "+msgID) {
		t.Fatalf("reply show = %q", body)
	}
}

func statusMessageID(t *testing.T, status string) string {
	t.Helper()
	for _, line := range strings.Split(status, "\n") {
		id, ok := strings.CutPrefix(line, "id: ")
		if ok && strings.HasPrefix(id, "m-") {
			return id
		}
	}
	t.Fatalf("status has no message id: %q", status)
	return ""
}
