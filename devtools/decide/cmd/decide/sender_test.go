package main

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

// lastMessage is the newest message of id, read with show --json.
func lastMessage(t *testing.T, id string) decide.Message {
	t.Helper()
	got := showJSON(t, id)
	if len(got.Messages) == 0 {
		t.Fatalf("%s has no messages", id)
	}
	return got.Messages[len(got.Messages)-1]
}

func replyAndSend(t *testing.T) (reply, send decide.Message) {
	t.Helper()
	writeSendConfig(t, os.Getenv("XDG_CONFIG_HOME"))
	id := askRelay(t, []string{"--notify-pane", "p1"}, "post it?")
	if _, stderr, code := runCLI("reply", id, "posted"); code != 0 {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
	reply = lastMessage(t, id)
	if _, stderr, code := runCLI("send", "--id", id, "thanks"); code != 0 {
		t.Fatalf("send --id = %d\n%s", code, stderr)
	}
	return reply, lastMessage(t, id)
}

func TestReplyAndSendRecordTheAgentbusName(t *testing.T) {
	inHerdrPane(t)
	fakeWhoami(t, "ops/inbox\nwhen: x\nops/triage\nwhen: y\n", nil)
	reply, send := replyAndSend(t)
	if reply.Author != "ada" || reply.By != "ops/inbox" || reply.Via != "" {
		t.Fatalf("reply = %+v", reply)
	}
	if send.Author != "human" || send.By != "ops/inbox" || send.Via != decide.ViaCLI {
		t.Fatalf("send --id = %+v", send)
	}
}

func TestReplyWithoutANameRecordsThePaneHandle(t *testing.T) {
	inHerdrPane(t)
	fakeWhoami(t, "herdr:m1/s/w1:p3\n", nil)
	if reply, _ := replyAndSend(t); reply.By != "herdr:m1/s/w1:p3" {
		t.Fatalf("reply = %+v", reply)
	}
	fakeWhoami(t, "", errors.New("no agentbus"))
	if reply, _ := replyAndSend(t); reply.By != "w1:p3" {
		t.Fatalf("reply without agentbus = %+v", reply)
	}
}

func TestReplyOutsideHerdrRecordsNoSender(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	calls := fakeWhoami(t, "ops/inbox\nwhen: x\n", nil)
	reply, send := replyAndSend(t)
	if reply.By != "" || send.By != "" || *calls != 0 {
		t.Fatalf("reply = %+v, send = %+v, whoami calls %d", reply, send, *calls)
	}
}

func TestHubRecordsTheClientSender(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	t.Setenv("HERDR_PANE_ID", "")
	calls := fakeWhoami(t, "", errors.New("the hub must not run agentbus"))
	t.Setenv("DECIDE_BUS_NAMES", "ops/inbox,ops/triage")
	if reply, send := replyAndSend(t); reply.By != "ops/inbox" || send.By != "ops/inbox" {
		t.Fatalf("reply = %+v, send = %+v", reply, send)
	}
	t.Setenv("DECIDE_BUS_NAMES", "")
	if reply, _ := replyAndSend(t); reply.By != "herdr:m2/s/p3" {
		t.Fatalf("reply without names = %+v", reply)
	}
	if *calls != 0 {
		t.Fatalf("hub ran agentbus whoami %d times", *calls)
	}
}

func TestHubReplySendsTheCaller(t *testing.T) {
	for _, args := range [][]string{{"reply", "D-1", "posted"}, {"send", "--id", "D-1", "thanks"}} {
		fake, _ := setupHub(t, `{"hub": "me@hub", "machine": "m2"}`)
		herdr := herdrtest.Install(t)
		herdr.Write("local.agents", herdrAgentsW1P3)
		herdr.Write("local.panes", herdrPanesW1P3)
		herdr.Write("local.tabs", herdrTabsW1T2)
		t.Setenv("HERDR_PANE_ID", "w1:p3")
		fakeWhoami(t, "ops/inbox\nwhen: x\n", nil)
		var stderr bytes.Buffer
		if code := run(args, &bytes.Buffer{}, &stderr); code != 0 {
			t.Fatalf("%q = %d, stderr %q", args, code, stderr.String())
		}
		env := fake.Lines("decide.env")
		for _, want := range []string{"DECIDE_AS_HANDLE=herdr:m2/s/w1:p3", "DECIDE_BUS_NAMES=ops/inbox"} {
			if !slices.Contains(env, want) {
				t.Fatalf("%q hub env = %q, want %s", args, env, want)
			}
		}
	}
}

func TestHubReplyWithoutAMachineWarnsAndRuns(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub"}`)
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	t.Setenv("DECIDE_MACHINE", "")
	var stderr bytes.Buffer
	if code := run([]string{"reply", "D-1", "posted"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("reply = %d, stderr %q", code, stderr.String())
	}
	if !strings.Contains(stderr.String(), "records no sender") {
		t.Fatalf("stderr = %q", stderr.String())
	}
	for _, kv := range fake.Lines("decide.env") {
		if strings.HasPrefix(kv, "DECIDE_AS_HANDLE=") {
			t.Fatalf("reply passed %q", kv)
		}
	}
}

func TestShowNamesTheSender(t *testing.T) {
	inHerdrPane(t)
	fakeWhoami(t, "ops/inbox\nwhen: x\n", nil)
	writeSendConfig(t, os.Getenv("XDG_CONFIG_HOME"))
	id := askRelay(t, nil, "post it?")
	if _, stderr, code := runCLI("reply", id, "posted"); code != 0 {
		t.Fatalf("reply = %d\n%s", code, stderr)
	}
	stdout, stderr, code := runCLI("show", id)
	if code != 0 || !strings.Contains(stdout, " ada (via ops/inbox) posted\n") {
		t.Fatalf("show = %d\n%s\n%s", code, stdout, stderr)
	}
}

func TestSendToRecordsVia(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	dir := filepath.Join(cfg, "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body := []byte(`{"targets":[{"name":"alpha","pane":"pane-a"}],"sendTemplate":"true {pane}"}` + "\n")
	if err := os.WriteFile(filepath.Join(dir, "config.json"), body, 0o600); err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		flags []string
		want  string
	}{
		{nil, decide.ViaCLI},
		{[]string{"--via", "ui"}, decide.ViaUI},
	} {
		args := append(append([]string{"send", "--to", "alpha"}, tc.flags...), "hello")
		if _, stderr, code := runCLI(args...); code != 0 {
			t.Fatalf("%q = %d\n%s", args, code, stderr)
		}
		threads := showThreads(t)
		newest := threads[0]
		for _, it := range threads {
			if it.ID > newest.ID {
				newest = it
			}
		}
		if m := newest.Messages[0]; m.Via != tc.want {
			t.Fatalf("%q first message = %+v, want via %s", args, m, tc.want)
		}
	}
}
