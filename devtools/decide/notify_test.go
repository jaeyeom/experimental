package decide

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

func TestNotifyRoutesAgentbusWithoutTheTemplate(t *testing.T) {
	dir := t.TempDir()
	marker := filepath.Join(dir, "template-ran")
	writeConfig(t, `{"notifyTemplate":"touch `+marker+`","notifyPane":"pane"}`)
	args := filepath.Join(dir, "args")
	installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' \"$1\" \"$2\" \"$3\" \"$4\" \"$5\" > '"+args+"'\nprintf '%s\\n' 'm-1 delivered'\n")

	item := &Item{
		ID:         "D-7",
		NotifyPane: "agentbus:ops/x",
		Answer:     &Answer{Value: "ship", AnsweredBy: "human"},
	}
	var out strings.Builder
	if err := Notify(item, &out); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(marker); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("notify template ran for an agentbus target")
	}
	got := readText(t, args)
	note := DeliveryNote("D-7", "human", "ship")
	want := "send\nops/x\n" + note + "\n--client-id\n" + DeliveryKey(item) + "\n"
	if got != want {
		t.Fatalf("agentbus args = %q, want %q", got, want)
	}
}

func TestNotifyAgentbusFailure(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"true"}`)
	installProgram(t, "agentbus", "#!/bin/sh\necho 'agentbus: down' >&2\nexit 1\n")
	item := &Item{ID: "D-7", NotifyPane: "agentbus:ops/x", Answer: &Answer{Value: "ship", AnsweredBy: "human"}}
	if err := Notify(item, &strings.Builder{}); err == nil {
		t.Fatal("failing agentbus send returned nil")
	}
}

func TestNotifyAndOwnerShareAcceptedBusSend(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"false {pane}"}`)
	item := &Item{ID: "D-7", NotifyPane: "agentbus:ops/x", Answer: &Answer{Value: "ship", AnsweredBy: "human"}}
	cases := []struct {
		name    string
		script  string
		wantErr bool
	}{
		{name: "queued", script: "#!/bin/sh\nprintf '%s\\n' 'm-1 queued for delivery'\n"},
		{name: "held exit 3", script: "#!/bin/sh\nprintf '%s\\n' 'm-1 held: hibernated'\necho 'agentbus: held' >&2\nexit 3\n"},
		{name: "exit 1", script: "#!/bin/sh\necho 'agentbus: down' >&2\nexit 1\n", wantErr: true},
		{name: "error json", script: "#!/bin/sh\nprintf '%s\\n' '{\"error\":{\"code\":\"agent_not_found\"}}'\n", wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			installProgram(t, "agentbus", tc.script)
			nerr := Notify(item, &strings.Builder{})
			oerr := NotifyOwner("agentbus:ops/x", "", "D-7", "hello")
			if (nerr != nil) != tc.wantErr || (oerr != nil) != tc.wantErr {
				t.Fatalf("notify err %v, owner err %v, want error %v", nerr, oerr, tc.wantErr)
			}
		})
	}
}

func TestNotifyAgentbusHeldAndQueued(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"true"}`)
	item := &Item{ID: "D-7", NotifyPane: "agentbus:ops/x", Answer: &Answer{Value: "ship", AnsweredBy: "human"}}
	for _, state := range []string{"held: hibernated", "queued", "queued for delivery"} {
		t.Run(state, func(t *testing.T) {
			installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' 'm-1 "+state+"'\n")
			if err := Notify(item, &strings.Builder{}); err != nil {
				t.Fatalf("state %q: %v", state, err)
			}
		})
	}
}

func TestNotifyTemplateErrorJSON(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"printf '%s' '{\"error\":{\"code\":\"agent_not_found\"}}'"}`)
	item := &Item{ID: "D-7", NotifyPane: "w1:p1", Answer: &Answer{Value: "ship", AnsweredBy: "human"}}
	err := Notify(item, &strings.Builder{})
	if err == nil || !strings.Contains(err.Error(), "agent_not_found") {
		t.Fatalf("err = %v", err)
	}
}

func TestNotifyRemotePaneUsesMachine(t *testing.T) {
	dir := t.TempDir()
	marker := filepath.Join(dir, "template-ran")
	writeConfig(t, `{"notifyTemplate":"touch `+marker+`"}`)
	herdr := herdrtest.Install(t)
	item := &Item{
		ID:            "D-7",
		NotifyPane:    "p3",
		NotifyMachine: "m2",
		Answer:        &Answer{Value: "ship", AnsweredBy: "human"},
	}
	if err := Notify(item, &strings.Builder{}); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(marker); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("notify template ran for a remote pane")
	}
	header, _, _ := strings.Cut(DeliveryNote("D-7", "human", "ship"), "\n")
	want := "--machine m2 agent prompt p3 " + header
	argv := herdr.Argv()
	if len(argv) == 0 || argv[0] != want {
		t.Fatalf("herdr argv = %q, want %q", argv, want)
	}
}

func TestNotifyRemotePaneFailure(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"true"}`)
	item := &Item{
		ID:            "D-7",
		NotifyPane:    "p3",
		NotifyMachine: "m2",
		Answer:        &Answer{Value: "ship", AnsweredBy: "human"},
	}
	t.Run("unreachable", func(t *testing.T) {
		herdr := herdrtest.Install(t)
		herdr.Write("m2.down", "")
		err := Notify(item, &strings.Builder{})
		if err == nil || !strings.Contains(err.Error(), "m2") {
			t.Fatalf("err = %v", err)
		}
	})
	t.Run("error json", func(t *testing.T) {
		herdr := herdrtest.Install(t)
		herdr.Write("m2.prompt", `{"error":{"code":"agent_not_found"}}`)
		err := Notify(item, &strings.Builder{})
		if err == nil || !strings.Contains(err.Error(), "agent_not_found") {
			t.Fatalf("err = %v", err)
		}
	})
}

func TestNotifyLocalPaneRunsTheTemplate(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"printf ok"}`)
	herdr := herdrtest.Install(t)
	item := &Item{ID: "D-7", NotifyPane: "w1:p1", Answer: &Answer{Value: "ship", AnsweredBy: "human"}}
	var out strings.Builder
	if err := Notify(item, &out); err != nil {
		t.Fatal(err)
	}
	if out.String() != "ok" {
		t.Fatalf("stdout = %q", out.String())
	}
	if argv := herdr.Argv(); len(argv) != 0 {
		t.Fatalf("local pane ran herdr: %q", argv)
	}
}

func TestNotifyPaneTemplateUnchanged(t *testing.T) {
	writeConfig(t, `{"notifyTemplate":"printf ok"}`)
	item := &Item{ID: "D-7", NotifyPane: "w1:p1", Answer: &Answer{Value: "ship", AnsweredBy: "human"}}
	var out strings.Builder
	if err := Notify(item, &out); err != nil {
		t.Fatal(err)
	}
	if out.String() != "ok" {
		t.Fatalf("stdout = %q", out.String())
	}
}

func TestRunSendTreatsErrorJSONAsFailure(t *testing.T) {
	script := filepath.Join(t.TempDir(), "herdr")
	body := "#!/bin/sh\nprintf '%s\\n' '{\"error\":{\"code\":\"agent_not_found\"}}'\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	err := RunSend([]string{script, "agent", "prompt", "w1:p1", "note"})
	if err == nil || !strings.Contains(err.Error(), "agent_not_found") {
		t.Fatalf("err = %v", err)
	}
}

func TestRunSendKeepsASuccessfulCommand(t *testing.T) {
	script := filepath.Join(t.TempDir(), "herdr")
	if err := os.WriteFile(script, []byte("#!/bin/sh\nprintf ok\n"), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	if err := RunSend([]string{script, "agent", "prompt", "w1:p1", "note"}); err != nil {
		t.Fatal(err)
	}
}

func TestNotifyOwnerRoutesAgentbus(t *testing.T) {
	dir := t.TempDir()
	writeConfig(t, `{"sendTemplate":"false {pane}"}`)
	args := filepath.Join(dir, "args")
	installProgram(t, "agentbus", "#!/bin/sh\nprintf '%s\\n' \"$1\" \"$2\" \"$3\" > '"+args+"'\nprintf '%s\\n' 'm-1 delivered'\n")
	if err := NotifyOwner("agentbus:ops/x", "mac", "D-7", "hello"); err != nil {
		t.Fatal(err)
	}
	got := readText(t, args)
	note := DeliveryNote("D-7", "human", "hello")
	if got != "send\nops/x\n"+note+"\n" {
		t.Fatalf("agentbus args = %q", got)
	}
}

func installProgram(t *testing.T, name, script string) {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(bin, name), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func readText(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}
