package main

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func TestQueueHoldsThenDeliversAndSurvivesRestart(t *testing.T) {
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
	reviewer := "herdr:local/default/w1:p2"
	bare := "herdr:local/default/w9:p9"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			holder:   adapter.StateIdle,
			reviewer: adapter.StateBlocked,
			bare:     adapter.StateNoAgent,
		},
		Result: map[string]adapter.Result{
			holder:   adapter.ResultAccepted,
			reviewer: adapter.ResultAccepted,
		},
		PaneHandles: map[string]string{"p1": holder, "p2": reviewer},
		InventoryAgents: []adapter.Agent{
			{Handle: holder, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: reviewer, PaneID: "p2", SessionID: "default", Status: "blocked"},
		},
	}
	cfg := daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour}
	stop := startDaemon(t, dirs, cfg)
	env := func(pane string) []string {
		return append(append([]string{}, base...), "HERDR_PANE_ID="+pane)
	}
	mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews"}, env("p1"))
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, env("p2"))

	if code, _, stderr := runCmd([]string{"send", "missing/name", "nope"}, env("p1")); code == 0 || !strings.Contains(stderr, "try agentbus search") {
		t.Fatalf("unknown name = %d %q", code, stderr)
	}
	bareSent, _ := mustCmd(t, []string{"send", bare, "bare-shell-body"}, env("p1"))
	if !strings.Contains(bareSent, "held: no-agent") || len(ad.Texts) != 0 {
		t.Fatalf("bare send = %q texts %q", bareSent, ad.Texts)
	}

	first, _ := mustCmd(t, []string{"send", "eng/reviewer", "first-body"}, env("p1"))
	second, _ := mustCmd(t, []string{"send", "eng/reviewer", "second-body"}, env("p1"))
	if !strings.Contains(first, "held: blocked") || !strings.Contains(second, "held: waiting for an earlier message") {
		t.Fatalf("queued = %q / %q", first, second)
	}
	if len(ad.Texts) != 0 {
		t.Fatalf("pasted while blocked: %q", ad.Texts)
	}
	firstID, _, ok := strings.Cut(strings.TrimSpace(first), " ")
	if !ok {
		t.Fatalf("first send = %q", first)
	}
	listed, _ := mustCmd(t, []string{"list"}, base)
	if !strings.Contains(listed, "held "+firstID) || !strings.Contains(listed, "waited") || !strings.Contains(listed, "name: ops/inbox") {
		t.Fatalf("list = %q", listed)
	}
	status, _ := mustCmd(t, []string{"status", firstID}, base)
	if !strings.Contains(status, "state: held: blocked") || !strings.Contains(status, "waited:") {
		t.Fatalf("status = %q", status)
	}
	if !logHas(t, dirs, eventlog.EventHeld, "waited") {
		t.Fatal("held log has no waited detail")
	}

	ad.Reach[reviewer] = adapter.StateIdle
	status, _ = mustCmd(t, []string{"status", firstID}, base)
	if !strings.Contains(status, "state: delivered") {
		t.Fatalf("status after idle = %q", status)
	}
	if len(ad.Texts) != 2 || !strings.Contains(ad.Texts[0], "first-body") || !strings.Contains(ad.Texts[1], "second-body") {
		t.Fatalf("delivery order = %q", ad.Texts)
	}

	ad.Reach[reviewer] = adapter.StateBlocked
	expired, _ := mustCmd(t, []string{"send", "--ttl", "200ms", "--wait", "eng/reviewer", "ttl-body"}, env("p1"))
	expiredID, expiredState, ok := strings.Cut(strings.TrimSpace(expired), " ")
	if !ok || expiredState != "expired" {
		t.Fatalf("ttl send = %q", expired)
	}
	if textsContain(ad.Texts, "ttl-body") || !textsContain(ad.Texts, "message "+expiredID+" to eng/reviewer expired") {
		t.Fatalf("ttl texts = %q", ad.Texts)
	}
	if !logHas(t, dirs, eventlog.EventExpired, "ttl") {
		t.Fatal("expired log missing")
	}

	held, _ := mustCmd(t, []string{"send", "eng/reviewer", "survive-body"}, env("p1"))
	heldID, heldState, ok := strings.Cut(strings.TrimSpace(held), " ")
	if !ok || !strings.HasPrefix(heldState, "held:") {
		t.Fatalf("survive send = %q", held)
	}
	stop()
	before := len(ad.Texts)
	stop = startDaemon(t, dirs, cfg)
	defer stop()
	status, _ = mustCmd(t, []string{"status", heldID}, base)
	if !strings.Contains(status, "state: held:") || len(ad.Texts) != before || textsContain(ad.Texts, "survive-body") {
		t.Fatalf("after restart status %q texts %q", status, ad.Texts)
	}
	ad.Reach[reviewer] = adapter.StateIdle
	status, _ = mustCmd(t, []string{"status", heldID}, base)
	if !strings.Contains(status, "state: delivered") || !textsContain(ad.Texts, "survive-body") {
		t.Fatalf("delivered after restart = %q texts %q", status, ad.Texts)
	}

	ad.Result[reviewer] = adapter.ResultRefusedRetryable
	code, stdout, stderr := runCmd([]string{"send", "eng/reviewer", "refused-body"}, env("p1"))
	if code != 0 || !strings.Contains(stdout, "held: transport refused") {
		t.Fatalf("refusal = %d %q %q", code, stdout, stderr)
	}
}

func logHas(t *testing.T, dirs paths.Dirs, event, text string) bool {
	t.Helper()
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: event})
	if err != nil {
		t.Fatal(err)
	}
	return strings.Contains(strings.Join(lines, "\n"), text)
}

func textsContain(texts []string, fragment string) bool {
	for _, text := range texts {
		if strings.Contains(text, fragment) {
			return true
		}
	}
	return false
}
