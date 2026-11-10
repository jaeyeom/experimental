package main

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

const hookInput = `{"session_id":"sess-1","tool_name":"SendMessage","tool_input":{"to":"researcher","message":"hi"}}`

func fakeHookDial(t *testing.T, dial func(context.Context, daemon.Request) (daemon.Response, error)) {
	t.Helper()
	old := dialDaemon
	t.Cleanup(func() { dialDaemon = old })
	dialDaemon = func(ctx context.Context, _ string, req daemon.Request) (daemon.Response, error) {
		return dial(ctx, req)
	}
}

func runHook(t *testing.T, env []string, stdin string) (int, string) {
	t.Helper()
	code, stdout, stderr := runCmdIn([]string{"hook"}, env, strings.NewReader(stdin))
	if stderr != "" {
		t.Fatalf("hook stderr = %q", stderr)
	}
	return code, stdout
}

func TestHookHintsTheBusNameWithoutADecision(t *testing.T) {
	_, env := hookEnv(t)
	var got daemon.Request
	fakeHookDial(t, func(_ context.Context, req daemon.Request) (daemon.Response, error) {
		got = req
		return daemon.Response{OK: true, Name: "coord/primary"}, nil
	})
	code, stdout := runHook(t, env, hookInput)
	if code != 0 {
		t.Fatalf("code = %d", code)
	}
	if got.Op != "bypass" || got.Name != "researcher" || got.Body != "hi" || got.Session != "sess-1" {
		t.Fatalf("request = %+v", got)
	}
	assertHint(t, stdout, "coord/primary")
}

func TestHookPrintsNothingWhenTheTargetDoesNotResolve(t *testing.T) {
	_, env := hookEnv(t)
	for name, dial := range map[string]func(context.Context, daemon.Request) (daemon.Response, error){
		"unresolved": func(context.Context, daemon.Request) (daemon.Response, error) {
			return daemon.Response{OK: true}, nil
		},
		"refused": func(context.Context, daemon.Request) (daemon.Response, error) {
			return daemon.Response{Error: "unknown op", Name: "coord/primary"}, nil
		},
		"unreachable": func(context.Context, daemon.Request) (daemon.Response, error) {
			return daemon.Response{Name: "coord/primary"}, errors.New("connection refused")
		},
	} {
		fakeHookDial(t, dial)
		if code, stdout := runHook(t, env, hookInput); code != 0 || stdout != "" {
			t.Fatalf("%s: code %d, stdout %q", name, code, stdout)
		}
	}
}

func TestHookGivesUpAtTheTimeout(t *testing.T) {
	_, env := hookEnv(t)
	fakeHookDial(t, func(ctx context.Context, _ daemon.Request) (daemon.Response, error) {
		<-ctx.Done()
		return daemon.Response{OK: true, Name: "coord/primary"}, ctx.Err()
	})
	start := time.Now()
	code, stdout := runHook(t, env, hookInput)
	if elapsed := time.Since(start); elapsed > hookTimeout+500*time.Millisecond {
		t.Fatalf("hook took %v", elapsed)
	}
	if code != 0 || stdout != "" {
		t.Fatalf("code %d, stdout %q", code, stdout)
	}
}

func TestBypassLogsTheResolvedBusName(t *testing.T) {
	dirs, env := hookEnv(t)
	holder := "herdr:local/default/w1:p1"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{holder: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": holder},
	}
	stop := startBus(t, dirs, ad)
	defer stop()
	mustCmd(t, []string{"register", "coord/primary", "--when", "coordinates"}, append(append([]string{}, env...), "HERDR_PANE_ID=p1"))

	code, stdout := runHook(t, env, `{"session_id":"sess-1","tool_input":{"to":"`+holder+`","message":"hi"}}`)
	if code != 0 {
		t.Fatalf("code = %d", code)
	}
	assertHint(t, stdout, "coord/primary")
	if code, stdout := runHook(t, env, hookInput); code != 0 || stdout != "" {
		t.Fatalf("unresolved: code %d, stdout %q", code, stdout)
	}

	events := bypassEvents(t, dirs.StateDir)
	if len(events) != 2 {
		t.Fatalf("bypass events = %#v", events)
	}
	if events[0]["name"] != holder || events[0]["resolved"] != "coord/primary" {
		t.Fatalf("resolved event = %#v", events[0])
	}
	if _, ok := events[1]["resolved"]; ok || events[1]["name"] != "researcher" {
		t.Fatalf("unresolved event = %#v", events[1])
	}
	logged, _ := mustCmd(t, []string{"log", "--event", "bypass"}, env)
	if !strings.Contains(logged, `resolved="coord/primary"`) {
		t.Fatalf("log = %q", logged)
	}
}

func assertHint(t *testing.T, stdout, name string) {
	t.Helper()
	var doc map[string]any
	if err := json.Unmarshal([]byte(stdout), &doc); err != nil {
		t.Fatalf("stdout %q is not hook JSON: %v", stdout, err)
	}
	if _, ok := doc["decision"]; ok {
		t.Fatalf("hook output has a decision: %q", stdout)
	}
	out, _ := doc["hookSpecificOutput"].(map[string]any)
	if len(out) != 2 || out["hookEventName"] != "PreToolUse" {
		t.Fatalf("hookSpecificOutput = %#v", out)
	}
	want := "This recipient is " + name + " on agentbus. Next time use: agentbus send " + name + ` "..."`
	if out["additionalContext"] != want {
		t.Fatalf("additionalContext = %q, want %q", out["additionalContext"], want)
	}
}
