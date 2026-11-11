package herdr

import (
	"context"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

const agentList = `{
  "result": {
    "agents": [
      {
        "pane_id": "w2:p3",
        "tab_id": "w2:t1",
        "agent": "claude",
        "agent_status": "idle",
        "terminal_title": "shell",
        "agent_session": {"value": "default"}
      }
    ]
  }
}`

const tabList = `{
  "result": {
    "tabs": [
      {"tab_id": "w2:t1", "label": "work"}
    ]
  }
}`

func TestHerdrMapsRecordedCommands(t *testing.T) {
	var prompts [][]string
	ad := &Adapter{Run: func(_ context.Context, args []string) (string, string, int, error) {
		switch args[0] + " " + args[1] {
		case "agent list":
			return agentList, "", 0, nil
		case "tab list":
			return tabList, "", 0, nil
		case "agent prompt":
			prompts = append(prompts, append([]string(nil), args...))
			if strings.Contains(args[3], "blocked") {
				return "", "agent_blocked", 1, nil
			}
			return `{"result":{}}`, "", 0, nil
		default:
			t.Fatalf("unexpected command %q", args)
			return "", "", 1, nil
		}
	}}
	if err := ad.SelfTest(context.Background()); err != nil {
		t.Fatal(err)
	}
	handle, err := ad.Identify(context.Background(), []string{"HERDR_PANE_ID=w2:p3"})
	if err != nil {
		t.Fatal(err)
	}
	if handle != "herdr:local/default/w2:p3" {
		t.Fatalf("handle = %q", handle)
	}
	got, err := ad.Inventory(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].TabLabel != "work" || got[0].Title != "shell" || got[0].Kind != "claude" {
		t.Fatalf("inventory = %+v", got)
	}
	state, err := ad.State(context.Background(), handle)
	if err != nil {
		t.Fatal(err)
	}
	if state != adapter.StateIdle {
		t.Fatalf("state = %q", state)
	}
	result, err := ad.Deliver(context.Background(), handle, "hello")
	if err != nil {
		t.Fatal(err)
	}
	if result != adapter.ResultAccepted {
		t.Fatalf("deliver = %q", result)
	}
	if len(prompts) != 1 || strings.Join(prompts[0], " ") != "agent prompt w2:p3 hello" {
		t.Fatalf("prompt args = %q", prompts)
	}
	refused, err := ad.Deliver(context.Background(), handle, "blocked please")
	if err != nil {
		t.Fatal(err)
	}
	if refused != adapter.ResultRefusedRetryable {
		t.Fatalf("blocked deliver = %q", refused)
	}
	gone, err := ad.State(context.Background(), "herdr:local/default/missing")
	if err != nil {
		t.Fatal(err)
	}
	if gone != adapter.StateGone {
		t.Fatalf("missing pane = %q", gone)
	}
}

const rule = "──────────────────────────────────────────"

// claudeScreen is a Claude Code screen as `herdr agent read --source
// visible` prints it: history, the input box between two rules, and the
// status lines below it. Claude Code pads the marker with a no-break space.
func claudeScreen(box ...string) string {
	lines := []string{"⏺ Done. The tests pass.", "", rule}
	lines = append(lines, box...)
	lines = append(lines, rule, "  [Opus 5.5] 14% | repo", "  ⏵⏵ auto mode on (shift+tab to cycle)")
	return strings.Join(lines, "\n") + "\n"
}

func TestClaudeComposerFixtures(t *testing.T) {
	tests := []struct {
		name   string
		screen string
		empty  bool
		ok     bool
	}{
		{"empty box", claudeScreen("❯ "), true, true},
		{"empty box with cursor", claudeScreen("❯  "), true, true},
		{"one line", claudeScreen("❯ please hold on"), false, true},
		{"multi line", claudeScreen("❯ first line", "  second line"), false, true},
		{"multi line blank first", claudeScreen("❯ ", "  second line"), false, true},
		{"shell mode draft", claudeScreen("! git pull --ff-only"), false, true},
		{"shell mode empty", claudeScreen("! "), false, true},
		{"previous prompt above an empty box", "❯ earlier prompt\n" + claudeScreen("❯ "), true, true},
		{"unknown layout", "Do you want to proceed?\n❯ 1. Yes\n  2. No\n", false, false},
		{"no marker in box", claudeScreen("  some menu"), false, false},
		{"empty screen", "", false, false},
		{"plain space empty", claudeScreen("❯ "), true, true},
		{"plain space draft", claudeScreen("❯ hi"), false, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			empty, ok := claudeComposerEmpty(tt.screen)
			if empty != tt.empty || ok != tt.ok {
				t.Fatalf("claudeComposerEmpty = %v, %v; want %v, %v", empty, ok, tt.empty, tt.ok)
			}
		})
	}
}

func TestComposerEmpty(t *testing.T) {
	const handle = "herdr:local/default/w2:p3"
	tests := []struct {
		name    string
		kind    string
		focused string
		screen  string
		readErr bool
		empty   bool
		reason  string
		wantErr bool
	}{
		{name: "claude empty and focused", kind: "claude", focused: "true", screen: claudeScreen("❯ "), empty: true},
		{name: "claude draft", kind: "claude", focused: "false", screen: claudeScreen("❯ ! git pull"), reason: adapter.HoldDraft},
		{name: "claude unparseable", kind: "claude", focused: "false", screen: "garbage\n", reason: adapter.HoldDraftUnknown},
		{name: "claude read fails", kind: "claude", focused: "false", readErr: true, reason: adapter.HoldDraftUnknown, wantErr: true},
		{name: "unknown kind focused", kind: "codex", focused: "true", reason: adapter.HoldFocused},
		{name: "unknown kind unfocused", kind: "codex", focused: "false", empty: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			list := `{"result":{"agents":[{"pane_id":"w2:p3","agent":"` + tt.kind +
				`","agent_status":"idle","focused":` + tt.focused + `,"agent_session":{"value":"default"}}]}}`
			ad := &Adapter{Run: func(_ context.Context, args []string) (string, string, int, error) {
				switch strings.Join(args, " ") {
				case "agent list":
					return list, "", 0, nil
				case "agent read w2:p3 --source visible":
					if tt.readErr {
						return "", "boom", 1, nil
					}
					return tt.screen, "", 0, nil
				default:
					t.Fatalf("unexpected command %q", args)
					return "", "", 1, nil
				}
			}}
			empty, reason, err := ad.ComposerEmpty(context.Background(), handle)
			if empty != tt.empty || reason != tt.reason || (err != nil) != tt.wantErr {
				t.Fatalf("ComposerEmpty = %v, %q, %v; want %v, %q, err %v", empty, reason, err, tt.empty, tt.reason, tt.wantErr)
			}
		})
	}
}

func TestSelfTestFailureIsTheCommandError(t *testing.T) {
	ad := &Adapter{Run: func(context.Context, []string) (string, string, int, error) {
		return "", "not running", 1, nil
	}}
	err := ad.SelfTest(context.Background())
	if err == nil || !strings.Contains(err.Error(), "not running") {
		t.Fatalf("self-test error = %v", err)
	}
}

// paneProcessInfoJSON is the shape `herdr pane process-info` prints.
const paneProcessInfoJSON = `{"id":"cli:pane:process_info","result":{"process_info":{
  "foreground_process_group_id":94342,
  "foreground_processes":[
    {"argv":["caffeinate","-i"],"argv0":"caffeinate","cmdline":"caffeinate -i","cwd":"/src/repo","name":"caffeinate","pid":30296},
    {"argv":["/usr/local/bin/claude"],"argv0":"claude","cmdline":"/usr/local/bin/claude","cwd":"/src/repo","name":"2.1.319","pid":94342}
  ],
  "pane_id":"w2:pC","shell_pid":61296},"type":"pane_process_info"}}`

func TestProcessInfo(t *testing.T) {
	var calls [][]string
	ad := &Adapter{Run: func(_ context.Context, args []string) (string, string, int, error) {
		calls = append(calls, args)
		switch args[len(args)-1] {
		case "w9:p9":
			return "", `{"error":{"code":"pane_not_found"}}`, 1, nil
		case "w9:pE":
			return `{"result":{}}`, "", 0, nil
		}
		return paneProcessInfoJSON, "", 0, nil
	}}
	got, err := ad.ProcessInfo(context.Background(), "w2:pC")
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(calls[0], " ") != "pane process-info --pane w2:pC" {
		t.Fatalf("ran %q", calls[0])
	}
	if got.ShellPID != 61296 || got.ForegroundProcessGroupID != 94342 || len(got.ForegroundProcesses) != 2 {
		t.Fatalf("ProcessInfo() = %+v", got)
	}
	if p := got.ForegroundProcesses[1]; p.PID != 94342 || p.Name != "2.1.319" || p.Cwd != "/src/repo" || len(p.Argv) != 1 {
		t.Fatalf("foreground process = %+v", p)
	}
	if pids := got.ForegroundPIDs(); len(pids) != 2 || pids[0] != 30296 || pids[1] != 94342 {
		t.Fatalf("ForegroundPIDs() = %v", pids)
	}
	if _, err := ad.ProcessInfo(context.Background(), "w9:p9"); err == nil || !strings.Contains(err.Error(), "pane_not_found") {
		t.Fatalf("ProcessInfo(missing pane) error = %v", err)
	}
	if _, err := ad.ProcessInfo(context.Background(), "w9:pE"); err == nil {
		t.Fatal("ProcessInfo(no process_info) = nil error")
	}
}
