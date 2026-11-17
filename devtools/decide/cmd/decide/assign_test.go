package main

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// busSendLog puts an agentbus on PATH that logs each send's name and
// accepts it. It returns the log path.
func busSendLog(t *testing.T) string {
	t.Helper()
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("DECIDE_AS_HANDLE", "")
	bin := filepath.Join(t.TempDir(), "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	logPath := filepath.Join(t.TempDir(), "bus.log")
	script := "#!/bin/sh\nprintf '%s\\n' \"$2\" >> '" + logPath + "'\nprintf '%s\\n' 'm-1 delivered'\n"
	if err := os.WriteFile(filepath.Join(bin, "agentbus"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
	return logPath
}

func busSends(t *testing.T, logPath string) []string {
	t.Helper()
	data, err := os.ReadFile(logPath)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		t.Fatal(err)
	}
	return strings.Fields(string(data))
}

func TestAssignSetsAndClearsTheWorker(t *testing.T) {
	busSendLog(t)
	id := askRelay(t, []string{"--notify-pane", "agentbus:ops/owner"}, "q")
	if _, stderr, code := runCLI("assign", id, "bea"); code != 0 {
		t.Fatalf("assign = %d\n%s", code, stderr)
	}
	got := showJSON(t, id)
	if got.Owner != "ada" || got.Worker != "bea" || got.WorkerNotifyPane != "agentbus:bea" {
		t.Fatalf("show = owner %q worker %q pane %q", got.Owner, got.Worker, got.WorkerNotifyPane)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.Author != "system" || last.Text != "assigned: bea is working on it" {
		t.Fatalf("event = %+v", last)
	}
	stdout, _, _ := runCLI("show", id)
	if !strings.Contains(stdout, "owner: ada\nworking on it: bea\n") {
		t.Fatalf("show text =\n%s", stdout)
	}
	listOut, _, _ := runCLI("list", "--json")
	if !strings.Contains(listOut, `"worker": "bea"`) || !strings.Contains(listOut, `"workerNotifyPane": "agentbus:bea"`) {
		t.Fatalf("list json =\n%s", listOut)
	}
	if _, stderr, code := runCLI("assign", id, "--clear"); code != 0 {
		t.Fatalf("assign --clear = %d\n%s", code, stderr)
	}
	got = showJSON(t, id)
	if got.Worker != "" || got.WorkerNotifyPane != "" {
		t.Fatalf("cleared = worker %q pane %q", got.Worker, got.WorkerNotifyPane)
	}
	if last := got.Messages[len(got.Messages)-1]; last.Text != "unassigned: bea is no longer working on it" {
		t.Fatalf("event = %+v", last)
	}
	stdout, _, _ = runCLI("show", id)
	if strings.Contains(stdout, "working on it:") {
		t.Fatalf("show text after clear =\n%s", stdout)
	}
}

func TestAssignNotifyPaneAndUsage(t *testing.T) {
	busSendLog(t)
	id := askRelay(t, nil, "q")
	if _, stderr, code := runCLI("assign", id, "bea", "--notify-pane", "p7"); code != 0 {
		t.Fatalf("assign = %d\n%s", code, stderr)
	}
	if got := showJSON(t, id); got.Worker != "bea" || got.WorkerNotifyPane != "p7" {
		t.Fatalf("show = worker %q pane %q", got.Worker, got.WorkerNotifyPane)
	}
	for _, args := range [][]string{
		{"assign", id},
		{"assign", id, "bea", "--clear"},
		{"assign", id, "--clear", "--notify-pane", "p7"},
	} {
		if _, _, code := runCLI(args...); code != 2 {
			t.Fatalf("%q = %d, want 2", args, code)
		}
	}
}

func TestSendIDWithAWorkerWarnsAndRoutes(t *testing.T) {
	const warning = "This reply goes to ada (owner); bea is working on it."
	cases := []struct {
		name string
		to   []string
		want []string
		warn bool
	}{
		{name: "no flag", want: []string{"ops/owner"}, warn: true},
		{name: "owner", to: []string{"--to", "owner"}, want: []string{"ops/owner"}},
		{name: "worker", to: []string{"--to", "worker"}, want: []string{"bea"}},
		{name: "both", to: []string{"--to", "both"}, want: []string{"ops/owner", "bea"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			logPath := busSendLog(t)
			id := askRelay(t, []string{"--notify-pane", "agentbus:ops/owner"}, "q")
			if _, stderr, code := runCLI("assign", id, "bea"); code != 0 {
				t.Fatalf("assign = %d\n%s", code, stderr)
			}
			args := append([]string{"send", "--id", id}, tc.to...)
			_, stderr, code := runCLI(append(args, "follow up")...)
			if code != 0 {
				t.Fatalf("send = %d\n%s", code, stderr)
			}
			if strings.Contains(stderr, warning) != tc.warn {
				t.Fatalf("stderr = %q, warn %v", stderr, tc.warn)
			}
			if got := busSends(t, logPath); strings.Join(got, ",") != strings.Join(tc.want, ",") {
				t.Fatalf("sends = %q, want %q", got, tc.want)
			}
			if got := showJSON(t, id); len(got.Delivery) != len(tc.want) {
				t.Fatalf("delivery = %+v", got.Delivery)
			}
		})
	}
}

func TestSendIDWithoutAWorkerIsUnchanged(t *testing.T) {
	logPath := busSendLog(t)
	id := askRelay(t, []string{"--notify-pane", "agentbus:ops/owner"}, "q")
	_, stderr, code := runCLI("send", "--id", id, "follow up")
	if code != 0 || stderr != "" {
		t.Fatalf("send = %d stderr %q", code, stderr)
	}
	if got := busSends(t, logPath); len(got) != 1 || got[0] != "ops/owner" {
		t.Fatalf("sends = %q", got)
	}
	_, stderr, code = runCLI("send", "--id", id, "--to", "worker", "again")
	if code == 0 || !strings.Contains(stderr, "no worker") {
		t.Fatalf("--to worker without one = %d\n%s", code, stderr)
	}
	_, stderr, code = runCLI("send", "--id", id, "--to", "alpha", "again")
	if code != 2 || !strings.Contains(stderr, "owner") {
		t.Fatalf("--to alpha = %d\n%s", code, stderr)
	}
	if got := busSends(t, logPath); len(got) != 1 {
		t.Fatalf("refused sends ran: %q", got)
	}
}

func TestAnswerNotifyEnvHasTheWorker(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("DECIDE_AS_HANDLE", "")
	t.Setenv("DECIDE_WORKER", "stale")
	logPath := filepath.Join(state, "notify.log")
	installNotifier(t, logPath, "exit 0")
	writeNotifyConfig(t, `notifier "w=$DECIDE_WORKER"`, "default-pane")
	plain := askRelay(t, nil, "plain")
	worked := askRelay(t, nil, "worked")
	if _, stderr, code := runCLI("assign", worked, "bea"); code != 0 {
		t.Fatalf("assign = %d\n%s", code, stderr)
	}
	for _, id := range []string{plain, worked} {
		if _, stderr, code := runCLI("answer", id, "a"); code != 0 {
			t.Fatalf("answer = %d\n%s", code, stderr)
		}
	}
	got := notifyLog(t, logPath)
	if len(got) != 2 || got[0] != "w=" || got[1] != "w=bea" {
		t.Fatalf("notify log = %#v", got)
	}
}
