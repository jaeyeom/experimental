package main

import (
	"encoding/json"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func TestMergedEnvelopeIDs(t *testing.T) {
	header := "[agentbus m-1a2b from ops/inbox → eng/reviewer · 10:04]\nbody\n"
	for _, tc := range []struct {
		name   string
		prompt string
		want   []string
	}{
		{"no envelope", "git pull --ff-only", nil},
		{"clean delivery", header, nil},
		{"clean after blank", "\n " + header, nil},
		{"clean quoting another", header + "see [agentbus m-99 from x → y · 09:00]", nil},
		{"draft first", "! git pull --ff-only" + header, []string{"m-1a2b"}},
		{"draft first multi-line", "line one\nline two " + header, []string{"m-1a2b"}},
		{"two merged", "draft " + header + "[agentbus m-3c from a → b · 10:05]\n" + header, []string{"m-1a2b", "m-3c"}},
		{"spike text", "my half-typed draft [agentbus m-42 from eng/reviewer] please review", []string{"m-42"}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := mergedEnvelopeIDs(tc.prompt); !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("mergedEnvelopeIDs(%q) = %q, want %q", tc.prompt, got, tc.want)
			}
		})
	}
}

func TestPromptHookBlocksMergedDraftAndRequeues(t *testing.T) {
	dirs, env := hookEnv(t)
	if code, _, stderr := runCmd([]string{"install-hooks"}, env); code != 0 {
		t.Fatalf("install-hooks = %d, stderr %s", code, stderr)
	}
	script := filepath.Join(homeOf(t, env), ".claude", "hooks", promptHookScriptName)

	sender := "herdr:local/default/w1:p1"
	target := "herdr:local/default/w1:p2"
	var mu sync.Mutex
	draft := false
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sender: adapter.StateIdle, target: adapter.StateIdle},
		Result:      map[string]adapter.Result{sender: adapter.ResultAccepted, target: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sender, "p2": target},
		InventoryAgents: []adapter.Agent{
			{Handle: sender, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: target, PaneID: "p2", SessionID: "default", Status: "idle"},
		},
		Composer: func(string) (bool, string, error) {
			mu.Lock()
			defer mu.Unlock()
			if draft {
				return false, adapter.HoldDraft, nil
			}
			return true, "", nil
		},
	}
	stop := startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer stop()
	paneEnv := func(pane string) []string {
		return append(append([]string{}, env...), "HERDR_PANE_ID="+pane)
	}
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, paneEnv("p2"))
	sent, _ := mustCmd(t, []string{"send", "eng/reviewer", "merged-body"}, paneEnv("p1"))
	id, state, _ := strings.Cut(strings.TrimSpace(sent), " ")
	if state != "queued for delivery" {
		t.Fatalf("send = %q", sent)
	}
	awaitDelivered(t, env, id)
	if len(ad.Texts) != 1 {
		t.Fatalf("send = %q texts %q", sent, ad.Texts)
	}
	envelope := ad.Texts[0]

	wrapper := t.TempDir()
	writeAgentbusWrapper(t, wrapper)
	childEnv := commandEnv(env, wrapper)
	hookInput := func(prompt string) string {
		data, err := json.Marshal(map[string]string{
			"session_id":      "sess-merge",
			"hook_event_name": "UserPromptSubmit",
			"prompt":          prompt,
		})
		if err != nil {
			t.Fatal(err)
		}
		return string(data)
	}

	// A clean delivery and a plain prompt pass with no output.
	for _, prompt := range []string{envelope, "just a question"} {
		code, stdout, stderr := runScript(t, script, hookInput(prompt), childEnv)
		if code != 0 || stdout != "" {
			t.Fatalf("prompt %q = %d\nstdout %q\nstderr %q", prompt, code, stdout, stderr)
		}
	}
	if logHas(t, dirs, eventlog.EventRequeued, id) {
		t.Fatal("a clean prompt re-queued the message")
	}

	// The user's draft came first: block, and the bus re-queues the message.
	mu.Lock()
	draft = true
	mu.Unlock()
	code, stdout, stderr := runScript(t, script, hookInput("! git pull --ff-only"+envelope), childEnv)
	if code != 0 {
		t.Fatalf("merged prompt = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}
	var decision struct {
		Decision string `json:"decision"`
		Reason   string `json:"reason"`
	}
	if err := json.Unmarshal([]byte(stdout), &decision); err != nil {
		t.Fatalf("decode %q: %v", stdout, err)
	}
	if decision.Decision != "block" {
		t.Fatalf("decision = %#v", decision)
	}
	for _, want := range []string{"draft", "merged", "[agentbus " + id, "Delete", "re-queued"} {
		if !strings.Contains(decision.Reason, want) {
			t.Fatalf("reason %q lacks %q", decision.Reason, want)
		}
	}
	if !logHas(t, dirs, eventlog.EventRequeued, id) {
		t.Fatal("requeue was not logged")
	}
	status, _ := mustCmd(t, []string{"status", id}, env)
	if !strings.Contains(status, "state: held: "+adapter.HoldDraft) || len(ad.Texts) != 1 {
		t.Fatalf("after block: status %q texts %q", status, ad.Texts)
	}

	// The user deletes the envelope and submits; the bus delivers again.
	mu.Lock()
	draft = false
	mu.Unlock()
	status, _ = mustCmd(t, []string{"status", id}, env)
	if !strings.Contains(status, "state: delivered") || len(ad.Texts) != 2 || !strings.Contains(ad.Texts[1], "merged-body") {
		t.Fatalf("redelivery: status %q texts %q", status, ad.Texts)
	}

	// An id the bus does not know still blocks, and the reason says the
	// copy in the input box is the only one.
	code, stdout, _ = runScript(t, script, hookInput("draft [agentbus m-00000000 from x → y · 10:00]\nhi\n"), childEnv)
	if code != 0 || !strings.Contains(stdout, `"block"`) || !strings.Contains(stdout, "could not re-queue m-00000000") {
		t.Fatalf("unknown id = %d %q", code, stdout)
	}
}

func TestPromptHookBlocksWhenBusIsDown(t *testing.T) {
	_, env := hookEnv(t)
	if code, _, stderr := runCmd([]string{"install-hooks"}, env); code != 0 {
		t.Fatalf("install-hooks = %d, stderr %s", code, stderr)
	}
	script := filepath.Join(homeOf(t, env), ".claude", "hooks", promptHookScriptName)
	wrapper := t.TempDir()
	writeAgentbusWrapper(t, wrapper)
	stdin := `{"session_id":"s","prompt":"draft [agentbus m-ab from x → y · 10:00]\nhi\n"}`
	code, stdout, stderr := runScript(t, script, stdin, commandEnv(env, wrapper))
	if code != 0 || !strings.Contains(stdout, `"block"`) || !strings.Contains(stdout, "could not re-queue m-ab") {
		t.Fatalf("hook = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}

	// Without agentbus on PATH the prompt goes through unchanged.
	code, stdout, _ = runScript(t, script, stdin, commandEnv(env, t.TempDir()))
	if code != 0 || stdout != "" {
		t.Fatalf("missing agentbus = %d %q", code, stdout)
	}
}
