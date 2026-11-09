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
	if _, err := ad.Deliver(context.Background(), "herdr:remote/default/w2:p3", "no"); err == nil {
		t.Fatal("remote handle was accepted")
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
