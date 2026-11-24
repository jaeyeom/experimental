package main

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

func writeDecideConfig(t *testing.T, body string) string {
	t.Helper()
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	dir := filepath.Join(cfg, "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.json"), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return state
}

// scriptedBus replaces agentbusRun. list is the stdout of `list --json`.
// A send returns sendOut, or sendErr when that is set.
func scriptedBus(t *testing.T, list []byte, sendOut string, sendErr error) *[][]string {
	t.Helper()
	var calls [][]string
	prev := agentbusRun
	agentbusRun = func(_ context.Context, args []string) ([]byte, error) {
		calls = append(calls, append([]string(nil), args...))
		if len(args) == 0 {
			return nil, errors.New("agentbus: empty argv")
		}
		switch args[0] {
		case "list":
			return list, nil
		case "send":
			if sendErr != nil {
				return nil, sendErr
			}
			return []byte(sendOut), nil
		default:
			return nil, errors.New("unexpected agentbus " + strings.Join(args, " "))
		}
	}
	t.Cleanup(func() { agentbusRun = prev })
	return &calls
}

func TestSendToAgentbusNameDeliversOverAgentbus(t *testing.T) {
	state := writeDecideConfig(t, `{
		"recipients": {"agentbus": true},
		"sendTemplate": "missing-pane-cmd {pane}"
	}`+"\n")
	list := []byte(`[{"name":"coord/primary","handle":"box/pane-9","liveness":"alive"}]`)
	calls := scriptedBus(t, list, "m-7f3a queued\n", nil)

	stdout, stderr, code := runCLI("send", "--to", "coord/primary", "hello bus")
	if code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	items := showThreads(t)
	if len(items) != 1 {
		t.Fatalf("items = %+v", items)
	}
	it := items[0]
	if stdout != "sent to coord/primary as "+it.ID+"\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if it.Owner != "coord/primary" || it.NotifyPane != "agentbus:coord/primary" || it.BusMessage != "m-7f3a" || it.Messages[0].Via != decide.ViaCLI || it.Messages[0].Text != "hello bus" {
		t.Fatalf("thread = %+v", it)
	}
	if len(*calls) != 2 || strings.Join((*calls)[0], " ") != "list --json" {
		t.Fatalf("calls = %q", *calls)
	}
	send := (*calls)[1]
	wantNote := decide.DeliveryNote(it.ID, "human", "hello bus")
	if len(send) != 3 || send[0] != "send" || send[1] != "coord/primary" || send[2] != wantNote {
		t.Fatalf("send argv = %q", send)
	}
	history, err := os.ReadFile(filepath.Join(state, "decide", decide.SendsName))
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(history), `"pane":"agentbus:coord/primary"`) || !strings.Contains(string(history), `"text":"hello bus"`) {
		t.Fatalf("history = %s", history)
	}
}

func TestSendAgentbusOffRejectsBusName(t *testing.T) {
	writeDecideConfig(t, `{"sendTemplate":"true {pane}"}`+"\n")
	calls := scriptedBus(t, []byte(`[{"name":"coord/primary","liveness":"alive"}]`), "m-1\n", nil)

	_, stderr, code := runCLI("send", "--to", "coord/primary", "hello")
	if code != 2 || !strings.Contains(stderr, `decide send recipient "coord/primary" is unknown`) {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	if len(*calls) != 0 {
		t.Fatalf("agentbus ran with the gate off: %q", *calls)
	}
	if items := showThreads(t); len(items) != 0 {
		t.Fatalf("items = %+v", items)
	}
}

func TestSendUnknownNameExits2(t *testing.T) {
	writeDecideConfig(t, `{"recipients":{"agentbus":true},"sendTemplate":"true {pane}"}`+"\n")
	calls := scriptedBus(t, []byte(`[{"name":"coord/primary","liveness":"alive"}]`), "m-1\n", nil)

	_, stderr, code := runCLI("send", "--to", "nope", "hello")
	if code != 2 || !strings.Contains(stderr, `decide send recipient "nope" is unknown`) {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	if len(*calls) != 1 || strings.Join((*calls)[0], " ") != "list --json" {
		t.Fatalf("calls = %q", *calls)
	}
	if items := showThreads(t); len(items) != 0 {
		t.Fatalf("items = %+v", items)
	}
}

func TestSendPrintsTheNewThreadID(t *testing.T) {
	writeDecideConfig(t, `{"targets":[{"name":"alpha","pane":"pane-a"}],"sendTemplate":"true {pane}"}`+"\n")

	stdout, stderr, code := runCLI("send", "--to", "alpha", "hello")
	if code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	items := showThreads(t)
	if len(items) != 1 || items[0].NotifyPane != "pane-a" || items[0].BusMessage != "" {
		t.Fatalf("thread = %+v", items)
	}
	if stdout != "sent to alpha as "+items[0].ID+"\n" {
		t.Fatalf("stdout = %q", stdout)
	}
}

func TestSendJSONIncludesTheNewThreadID(t *testing.T) {
	writeDecideConfig(t, `{"targets":[{"name":"alpha","pane":"pane-a"}],"sendTemplate":"true {pane}"}`+"\n")

	stdout, stderr, code := runCLI("send", "--to", "alpha", "--json", "hello")
	if code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	var sent struct {
		Name string `json:"name"`
		Text string `json:"text"`
		ID   string `json:"id"`
	}
	if err := json.Unmarshal([]byte(stdout), &sent); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	items := showThreads(t)
	if len(items) != 1 || sent.Name != "alpha" || sent.Text != "hello" || sent.ID != items[0].ID || sent.ID == "" {
		t.Fatalf("sent = %+v items = %+v", sent, items)
	}
}

func TestSendConfigTargetStillUsesThePaneTemplate(t *testing.T) {
	state := writeDecideConfig(t, `{
		"recipients": {"agentbus": true},
		"targets": [{"name": "alpha", "pane": "pane-a"}],
		"sendTemplate": "sentinel {pane}"
	}`+"\n")
	bin := filepath.Join(state, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	logPath := filepath.Join(state, "argv.log")
	markPath := filepath.Join(state, "thread-at-delivery")
	if err := os.WriteFile(filepath.Join(bin, "sentinel"), []byte(sendCaptureScript(logPath, markPath)), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
	calls := scriptedBus(t, []byte(`[{"name":"coord/primary","handle":"box/other","liveness":"alive"}]`), "m-1\n", nil)

	stdout, stderr, code := runCLI("send", "--to", "alpha", "hello")
	if code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	items := showThreads(t)
	if len(items) != 1 || items[0].NotifyPane != "pane-a" || items[0].BusMessage != "" || items[0].Messages[0].Via != decide.ViaCLI {
		t.Fatalf("thread = %+v", items)
	}
	if stdout != "sent to alpha as "+items[0].ID+"\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if len(*calls) != 1 || (*calls)[0][0] != "list" {
		t.Fatalf("calls = %q", *calls)
	}
	raw, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatal(err)
	}
	parts := strings.Split(strings.TrimRight(string(raw), "\x00"), "\x00")
	if len(parts) != 2 || parts[0] != "pane-a" || parts[1] != decide.DeliveryNote(items[0].ID, "human", "hello") {
		t.Fatalf("argv = %#v", parts)
	}
}

func TestSendAgentbusPaneUsesAgentbusNotHerdr(t *testing.T) {
	state := writeDecideConfig(t, `{
		"recipients": {"agentbus": true},
		"targets": [{"name": "team/desk", "pane": "agentbus:team/desk"}],
		"sendTemplate": "missing-pane-cmd {pane}"
	}`+"\n")
	list := []byte(`[{"name":"team/desk","handle":"box/other","liveness":"alive"}]`)
	calls := scriptedBus(t, list, "m-7f3a queued\n", nil)

	stdout, stderr, code := runCLI("send", "--to", "team/desk", "hello")
	if code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	items := showThreads(t)
	if len(items) != 1 {
		t.Fatalf("items = %+v", items)
	}
	it := items[0]
	if stdout != "sent to team/desk as "+it.ID+"\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if it.Owner != "team/desk" || it.NotifyPane != "agentbus:team/desk" || it.BusMessage != "m-7f3a" || it.Messages[0].Via != decide.ViaCLI || it.Messages[0].Text != "hello" {
		t.Fatalf("thread = %+v", it)
	}
	var sends [][]string
	for _, call := range *calls {
		if len(call) > 0 && call[0] == "send" {
			sends = append(sends, call)
		}
	}
	wantNote := decide.DeliveryNote(it.ID, "human", "hello")
	if len(sends) != 1 || len(sends[0]) != 3 || sends[0][1] != "team/desk" || sends[0][2] != wantNote {
		t.Fatalf("send argv = %q", sends)
	}
	history, err := os.ReadFile(filepath.Join(state, "decide", decide.SendsName))
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(history), `"pane":"agentbus:team/desk"`) || !strings.Contains(string(history), `"text":"hello"`) {
		t.Fatalf("history = %s", history)
	}
}

func TestSendTemplateFailureDropsTheThreadAndKeepsStderr(t *testing.T) {
	state := writeDecideConfig(t, `{
		"targets": [{"name": "alpha", "pane": "pane-a"}],
		"sendTemplate": "fail-send {pane}"
	}`+"\n")
	bin := filepath.Join(state, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	script := "#!/bin/sh\necho 'pane is gone' >&2\nexit 1\n"
	if err := os.WriteFile(filepath.Join(bin, "fail-send"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))

	_, stderr, code := runCLI("send", "--to", "alpha", "hello")
	if code == 0 || !strings.Contains(stderr, "pane is gone") {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	if items := showThreads(t); len(items) != 0 {
		t.Fatalf("items = %+v", items)
	}
}

func TestSendDropsConfigTargetHeldByBusHandle(t *testing.T) {
	writeDecideConfig(t, `{
		"recipients": {"agentbus": true},
		"targets": [{"name": "alpha", "pane": "pane-a"}],
		"sendTemplate": "missing-pane-cmd {pane}"
	}`+"\n")
	list := []byte(`[{"name":"coord/primary","handle":"box/pane-a","liveness":"alive"}]`)
	calls := scriptedBus(t, list, "m-9\n", nil)

	_, stderr, code := runCLI("send", "--to", "alpha", "hello")
	if code != 2 || !strings.Contains(stderr, `decide send recipient "alpha" is unknown`) {
		t.Fatalf("held target = %d\n%s", code, stderr)
	}
	if items := showThreads(t); len(items) != 0 {
		t.Fatalf("held target left a thread: %+v", items)
	}

	stdout, stderr, code := runCLI("send", "--to", "coord/primary", "hello")
	if code != 0 {
		t.Fatalf("bus name = %d\n%s", code, stderr)
	}
	items := showThreads(t)
	if len(items) != 1 || items[0].NotifyPane != "agentbus:coord/primary" || items[0].BusMessage != "m-9" || items[0].Messages[0].Via != decide.ViaCLI {
		t.Fatalf("thread = %+v", items)
	}
	if stdout != "sent to coord/primary as "+items[0].ID+"\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	var sends int
	for _, call := range *calls {
		if len(call) > 0 && call[0] == "send" {
			sends++
		}
	}
	if sends != 1 {
		t.Fatalf("calls = %q", *calls)
	}
}

func TestSendAgentbusListFailureIsNotUnknown(t *testing.T) {
	writeDecideConfig(t, `{
		"recipients": {"agentbus": true},
		"targets": [{"name": "alpha", "pane": "pane-a"}],
		"sendTemplate": "true {pane}"
	}`+"\n")
	prev := agentbusRun
	agentbusRun = func(_ context.Context, args []string) ([]byte, error) {
		if len(args) > 0 && args[0] == "list" {
			return nil, errors.New("daemon down")
		}
		return nil, errors.New("unexpected agentbus " + strings.Join(args, " "))
	}
	t.Cleanup(func() { agentbusRun = prev })

	_, stderr, code := runCLI("send", "--to", "alpha", "hello")
	if code != 1 || !strings.Contains(stderr, "daemon down") || strings.Contains(stderr, "unknown") {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	if items := showThreads(t); len(items) != 0 {
		t.Fatalf("items = %+v", items)
	}
}

func TestSendBusFailureDropsTheThread(t *testing.T) {
	writeDecideConfig(t, `{"recipients":{"agentbus":true}}`+"\n")
	scriptedBus(t, []byte(`[{"name":"coord/primary","liveness":"alive"}]`), "", errors.New("agentbus: no such name"))

	_, stderr, code := runCLI("send", "--to", "coord/primary", "hello")
	if code != 1 || !strings.Contains(stderr, "agentbus: no such name") {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	if items := showThreads(t); len(items) != 0 {
		t.Fatalf("items = %+v", items)
	}
}

func TestHubSendToAgentbusName(t *testing.T) {
	t.Setenv("HERDR_PANE_ID", "")
	_, state := setupHub(t, `{"hub":"me@hub"}`)
	hubDir, bin, _ := hubDecide(t, `{
		"recipients": {"agentbus": true},
		"sendTemplate": "missing-pane-cmd {pane}"
	}`+"\n")
	logPath := filepath.Join(bin, "agentbus.log")
	script := "#!/bin/sh\n" +
		"printf '%s\\n' \"$@\" >> " + hubcall.Quote(logPath) + "\n" +
		`if [ "$1" = "list" ]; then printf '%s\n' '[{"name":"coord/primary","handle":"box/p1","liveness":"alive"}]'; exit 0; fi` + "\n" +
		`if [ "$1" = "send" ]; then printf '%s\n' 'm-9 queued'; exit 0; fi` + "\n" +
		"exit 1\n"
	if err := os.WriteFile(filepath.Join(bin, "agentbus"), []byte(script), 0o755); err != nil { //nolint:gosec // test agentbus script
		t.Fatal(err)
	}

	stdout, stderr, code := runCLI("send", "--to", "coord/primary", "hello hub")
	if code != 0 {
		t.Fatalf("send = %d\n%s\n%s", code, stdout, stderr)
	}
	led, err := decide.Open(hubDir)
	if err != nil {
		t.Fatal(err)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 1 {
		t.Fatalf("hub items = %+v", items)
	}
	it := items[0]
	if stdout != "sent to coord/primary as "+it.ID+"\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if it.NotifyPane != "agentbus:coord/primary" || it.BusMessage != "m-9" || it.Messages[0].Via != decide.ViaCLI || it.Messages[0].Text != "hello hub" {
		t.Fatalf("hub thread = %+v", it)
	}
	logged, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(logged), "send\ncoord/primary\n") {
		t.Fatalf("agentbus log = %q", logged)
	}
	assertNoLocalState(t, state)
}

func TestSendVerbatimToAgentbusCarriesVerbatimInNote(t *testing.T) {
	writeDecideConfig(t, `{
		"recipients": {"agentbus": true},
		"sendTemplate": "missing-pane-cmd {pane}"
	}`+"\n")
	list := []byte(`[{"name":"coord/primary","handle":"box/pane-9","liveness":"alive"}]`)
	calls := scriptedBus(t, list, "m-8a1b queued\n", nil)

	longText := "Browser question\n" + strings.Repeat("x", 200)
	_, stderr, code := runCLI("send", "--to", "coord/primary", "--verbatim", longText)
	if code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	items := showThreads(t)
	if len(items) != 1 || items[0].Verbatim != longText || items[0].Messages[0].Verbatim != longText || items[0].Messages[0].Text != "" {
		t.Fatalf("items = %+v", items)
	}
	var sends [][]string
	for _, call := range *calls {
		if len(call) > 0 && call[0] == "send" {
			sends = append(sends, call)
		}
	}
	if len(sends) != 1 || !strings.Contains(sends[0][2], longText) {
		t.Fatalf("bus sends = %q", sends)
	}
}

func TestSendIDDeliversToCommandTarget(t *testing.T) {
	dir := t.TempDir()
	stdinFile := filepath.Join(dir, "stdin.json")
	script := filepath.Join(dir, "mytool")
	if err := os.WriteFile(script, []byte("#!/bin/sh\ncat > '"+stdinFile+"'\n"), 0o755); err != nil { //nolint:gosec // test helper script
		t.Fatal(err)
	}
	cfgBytes, err := json.Marshal(decide.Config{
		SendTemplate: "false {pane}",
		Targets: []decide.ConfigTarget{
			{Name: "mytool", Command: []string{script}},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	writeDecideConfig(t, string(cfgBytes)+"\n")

	stdout, stderr, code := runCLI("ask", "--owner", "mytool", "--class", "decision", "--kind", "relay", "--notify-pane", "target:mytool",
		"--option", "y:Yes", "--option", "n:No", "--rec", "y", "Post to #eng?")
	if code != 0 {
		t.Fatalf("ask = %d\n%s\n%s", code, stdout, stderr)
	}
	id := strings.TrimSpace(stdout)

	_, stderr, code = runCLI("send", "--id", id, "hold off until noon")
	if code != 0 {
		t.Fatalf("send --id = %d\n%s", code, stderr)
	}
	data, err := os.ReadFile(stdinFile)
	if err != nil {
		t.Fatal(err)
	}
	var got decide.Item
	if err := json.Unmarshal(data, &got); err != nil {
		t.Fatalf("unmarshal stdin JSON: %v", err)
	}
	if got.ID != id || len(got.Messages) < 2 || got.Messages[len(got.Messages)-1].Text != "hold off until noon" {
		t.Fatalf("stdin item = %+v", got)
	}
}
