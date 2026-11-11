package herdr

import (
	"context"
	"strings"
	"testing"
)

func recordingAdapter(calls *[]string, stdout string, code int) *Adapter {
	return &Adapter{Run: func(_ context.Context, args []string) (string, string, int, error) {
		*calls = append(*calls, strings.Join(args, " "))
		if code != 0 {
			return "", `{"error":{"code":"pane_not_found"}}`, code, nil
		}
		return stdout, "", 0, nil
	}}
}

func TestPaneInputCallShapes(t *testing.T) {
	var calls []string
	ad := recordingAdapter(&calls, `{"result":{}}`, 0)
	ctx := context.Background()
	if err := ad.SendText(ctx, "w1:p2", "/exit"); err != nil {
		t.Fatal(err)
	}
	if err := ad.SendKeys(ctx, "w1:p2", "enter"); err != nil {
		t.Fatal(err)
	}
	if err := ad.RunInPane(ctx, "w1:p2", "echo hi"); err != nil {
		t.Fatal(err)
	}
	want := []string{
		"pane send-text w1:p2 /exit",
		"pane send-keys w1:p2 enter",
		"pane run w1:p2 echo hi",
	}
	if strings.Join(calls, "\n") != strings.Join(want, "\n") {
		t.Fatalf("calls = %q, want %q", calls, want)
	}
}

func TestPaneInputReportsHerdrErrors(t *testing.T) {
	var calls []string
	ad := recordingAdapter(&calls, "", 1)
	ctx := context.Background()
	for name, err := range map[string]error{
		"send-text": ad.SendText(ctx, "w9:p9", "/exit"),
		"send-keys": ad.SendKeys(ctx, "w9:p9", "enter"),
		"run":       ad.RunInPane(ctx, "w9:p9", "true"),
	} {
		if err == nil || !strings.Contains(err.Error(), "pane_not_found") {
			t.Errorf("%s error = %v", name, err)
		}
	}
}

func TestScreenReadsTheVisibleScreen(t *testing.T) {
	var calls []string
	ad := recordingAdapter(&calls, "screen text\n", 0)
	got, err := ad.Screen(context.Background(), "w1:p2")
	if err != nil || got != "screen text\n" {
		t.Fatalf("Screen() = %q, %v", got, err)
	}
	if calls[0] != "agent read w1:p2 --source visible" {
		t.Fatalf("ran %q", calls[0])
	}
}

func TestComposerOf(t *testing.T) {
	for _, tt := range []struct {
		name, kind, screen string
		empty, ok          bool
	}{
		{"claude empty", "claude", claudeScreen("❯ "), true, true},
		{"claude draft", "claude", claudeScreen("❯ half a thought"), false, true},
		{"claude unreadable", "claude", "", false, false},
		{"unknown kind", "codex", claudeScreen("❯ "), false, false},
	} {
		t.Run(tt.name, func(t *testing.T) {
			empty, ok := ComposerOf(tt.kind, tt.screen)
			if empty != tt.empty || ok != tt.ok {
				t.Fatalf("ComposerOf = %v, %v; want %v, %v", empty, ok, tt.empty, tt.ok)
			}
		})
	}
}

func TestPendingDialog(t *testing.T) {
	permission := "⏺ Bash(rm -rf build)\n" + rule + "\n Bash command\n\n   rm -rf build\n\n Do you want to proceed?\n ❯ 1. Yes\n   2. No, and tell Claude what to do differently (esc)\n"
	question := "☐ Scope\n\nWhich packages?\n\n❯ 1. Only agentbus\n  2. Everything\n\nEnter to select · ↑/↓ to navigate · Esc to cancel\n"
	for _, tt := range []struct {
		name, kind, screen string
		want               bool
	}{
		{"permission", "claude", permission, true},
		{"question", "claude", question, true},
		{"idle", "claude", claudeScreen("❯ "), false},
		{"other kind", "codex", permission, false},
	} {
		t.Run(tt.name, func(t *testing.T) {
			if _, got := PendingDialog(tt.kind, tt.screen); got != tt.want {
				t.Fatalf("PendingDialog = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestExitInput(t *testing.T) {
	text, keys, ok := ExitInput("claude")
	if !ok || text != "/exit" || len(keys) != 1 || keys[0] != "enter" {
		t.Fatalf("ExitInput(claude) = %q, %q, %v", text, keys, ok)
	}
	if _, _, ok := ExitInput("codex"); ok {
		t.Fatal("ExitInput(codex) is known")
	}
}

func TestInventoryCarriesTheStateChangeSeq(t *testing.T) {
	ad := &Adapter{Run: func(_ context.Context, args []string) (string, string, int, error) {
		if args[0] == "tab" {
			return tabList, "", 0, nil
		}
		return `{"result":{"agents":[{"pane_id":"w2:p3","tab_id":"w2:t1","agent":"claude","agent_status":"idle","state_change_seq":42,"agent_session":{"value":"s"}}]}}`, "", 0, nil
	}}
	agents, err := ad.Inventory(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(agents) != 1 || agents[0].StateSeq != 42 {
		t.Fatalf("Inventory() = %+v", agents)
	}
}
