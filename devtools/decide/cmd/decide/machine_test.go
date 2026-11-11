package main

import (
	"bytes"
	"encoding/json"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

const herdrAgentsW1P3 = `{"result":{"agents":[{"pane_id":"w1:p3","agent":"claude","agent_session":{"value":"s"}}]}}`

func TestHubAskPassesTheCallerHandle(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub", "machine": "m2"}`)
	herdr := herdrtest.Install(t)
	herdr.Write("local.agents", herdrAgentsW1P3)
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	var stderr bytes.Buffer
	if code := run([]string{"ask", "--owner", "ada", "q"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("ask = %d, stderr %q", code, stderr.String())
	}
	if env := fake.Lines("decide.env"); !slices.Contains(env, "DECIDE_AS_HANDLE=herdr:m2/s/w1:p3") {
		t.Fatalf("hub env = %q", env)
	}
}

func TestHubMachineEnvOverridesConfig(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub", "machine": "other"}`)
	herdr := herdrtest.Install(t)
	herdr.Write("local.agents", herdrAgentsW1P3)
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	t.Setenv("DECIDE_MACHINE", "m2")
	var stderr bytes.Buffer
	if code := run([]string{"ask", "--owner", "ada", "q"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("ask = %d, stderr %q", code, stderr.String())
	}
	if env := fake.Lines("decide.env"); !slices.Contains(env, "DECIDE_AS_HANDLE=herdr:m2/s/w1:p3") {
		t.Fatalf("hub env = %q", env)
	}
}

func TestHubAskNeedsAMachine(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub"}`)
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	t.Setenv("DECIDE_MACHINE", "")
	var stderr bytes.Buffer
	code := run([]string{"ask", "--owner", "ada", "q"}, &bytes.Buffer{}, &stderr)
	if code == 0 || !strings.Contains(stderr.String(), `hub mode needs "machine"`) {
		t.Fatalf("ask = %d, stderr %q", code, stderr.String())
	}
	if fake.Read("argv") != "" {
		t.Fatal("ask ran ssh without a caller")
	}
}

func TestHubNonAskSendsNoCaller(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub", "machine": "m2"}`)
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	if code := run([]string{"list"}, &bytes.Buffer{}, &bytes.Buffer{}); code != 0 {
		t.Fatalf("list = %d", code)
	}
	for _, kv := range fake.Lines("decide.env") {
		if strings.HasPrefix(kv, "DECIDE_AS_HANDLE=") {
			t.Fatalf("list passed %q", kv)
		}
	}
}

// onHub makes this process look like a decide command the hub runs for
// the hub-mode caller handle.
func onHub(t *testing.T, handle string) {
	t.Helper()
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("SSH_CONNECTION", "10.0.0.2 5000 10.0.0.1 22")
	t.Setenv("DECIDE_AS_HANDLE", handle)
}

func askJSON(t *testing.T, extra ...string) decide.Item {
	t.Helper()
	args := []string{"ask", "--owner", "ada", "--kind", "relay", "--option", "a:Alpha", "--option", "b:Beta", "--rec", "a", "--json"}
	args = append(args, extra...)
	args = append(args, "q")
	stdout, stderr, code := runCLI(args...)
	if code != 0 {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	var item decide.Item
	if err := json.Unmarshal([]byte(stdout), &item); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	return item
}

func TestHubAskStoresTheCallerPaneAndMachine(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	item := askJSON(t)
	if item.NotifyPane != "p3" || item.NotifyMachine != "m2" {
		t.Fatalf("notify = %q on %q, want p3 on m2", item.NotifyPane, item.NotifyMachine)
	}
}

func TestHubAskKeepsAnExplicitPaneOnTheCallerMachine(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	item := askJSON(t, "--notify-pane", "p7")
	if item.NotifyPane != "p7" || item.NotifyMachine != "m2" {
		t.Fatalf("notify = %q on %q, want p7 on m2", item.NotifyPane, item.NotifyMachine)
	}
	bus := askJSON(t, "--notify-pane", "agentbus:ops")
	if bus.NotifyPane != "agentbus:ops" || bus.NotifyMachine != "" {
		t.Fatalf("bus notify = %q on %q", bus.NotifyPane, bus.NotifyMachine)
	}
}

func TestHubAskTrustsTheCallerOnlyOverSSH(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	t.Setenv("SSH_CONNECTION", "")
	_, stderr, code := runCLI("ask", "--owner", "ada", "--kind", "relay", "--option", "a:A", "--option", "b:B", "--rec", "a", "q")
	if code == 0 || !strings.Contains(stderr, "DECIDE_AS_HANDLE is only trusted over ssh") {
		t.Fatalf("ask = %d, stderr %q", code, stderr)
	}
}

func TestLocalAskLeavesTheMachineEmpty(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_AS_HANDLE", "")
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	item := askJSON(t)
	if item.NotifyPane != "" || item.NotifyMachine != "" {
		t.Fatalf("notify = %q on %q, want unset", item.NotifyPane, item.NotifyMachine)
	}
	item = askJSON(t, "--notify-pane", "p7")
	if item.NotifyPane != "p7" || item.NotifyMachine != "" {
		t.Fatalf("notify = %q on %q, want p7 locally", item.NotifyPane, item.NotifyMachine)
	}
}

func TestHubNotifyExportsTheMachine(t *testing.T) {
	dump := setupNotifyDump(t)
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("SSH_CONNECTION", "x")
	t.Setenv("DECIDE_AS_HANDLE", "herdr:m2/s/p3")
	item := askJSON(t)
	if _, stderr, code := runCLI("answer", item.ID, "a"); code != 0 {
		t.Fatalf("answer = %d\n%s", code, stderr)
	}
	if got := readDump(t, dump, "DECIDE_MACHINE"); got != "m2" {
		t.Fatalf("DECIDE_MACHINE = %q, want m2", got)
	}
	if got := readDump(t, dump, "DECIDE_PANE"); got != "p3" {
		t.Fatalf("DECIDE_PANE = %q, want p3", got)
	}
}

func TestComposeSendsToTheItemMachine(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	item := askJSON(t)
	herdr := herdrtest.Install(t)
	if _, stderr, code := runCLI("send", "--id", item.ID, "please edit"); code != 0 {
		t.Fatalf("send --id = %d\n%s", code, stderr)
	}
	argv := herdr.Argv()
	want := "--machine m2 agent prompt p3 [decide " + item.ID + " from human]"
	if len(argv) == 0 || argv[0] != want {
		t.Fatalf("herdr argv = %q, want first line %q", argv, want)
	}
}

func TestComposeReportsAnUnreachableMachine(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	item := askJSON(t)
	herdr := herdrtest.Install(t)
	herdr.Write("m2.down", "")
	_, stderr, code := runCLI("send", "--id", item.ID, "please edit")
	if code == 0 || !strings.Contains(stderr, "machine m2 unreachable: host unreachable") {
		t.Fatalf("send --id = %d, stderr %q", code, stderr)
	}
}
