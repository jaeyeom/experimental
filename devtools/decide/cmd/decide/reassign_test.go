package main

import (
	"errors"
	"io/fs"
	"os"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

func TestReassignBothFlagsAndShowJSON(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	item := askJSON(t, "--notify-pane", "p1")
	if item.NotifyPane != "p1" || item.NotifyMachine != "m2" || item.Owner != "ada" {
		t.Fatalf("ask = owner %q pane %q machine %q", item.Owner, item.NotifyPane, item.NotifyMachine)
	}
	t.Setenv("DECIDE_AS_HANDLE", "herdr:m9/s/p4")
	if _, stderr, code := runCLI("reassign", item.ID, "--owner", "bea", "--notify-pane", "p9"); code != 0 {
		t.Fatalf("reassign = %d\n%s", code, stderr)
	}
	got := showJSON(t, item.ID)
	if got.Owner != "bea" || got.NotifyPane != "p9" || got.NotifyMachine != "m9" {
		t.Fatalf("show = owner %q pane %q machine %q", got.Owner, got.NotifyPane, got.NotifyMachine)
	}
	if got.Answer != nil || got.State != decide.StateOpen {
		t.Fatalf("answer changed: state %s answer %+v", got.State, got.Answer)
	}
	if len(got.Messages) < 2 || got.Messages[0].Text != "q" {
		t.Fatalf("history = %+v", got.Messages)
	}
	last := got.Messages[len(got.Messages)-1]
	const want = "reassigned: owner ada → bea; notify pane p1 → p9 (by herdr:m9/s/p4)"
	if last.Author != "system" || last.Text != want {
		t.Fatalf("event = author %q text %q", last.Author, last.Text)
	}
}

func TestReassignOneFlagLeavesTheOther(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	item := askJSON(t, "--notify-pane", "p1")
	if _, stderr, code := runCLI("reassign", item.ID, "--owner", "bea"); code != 0 {
		t.Fatalf("owner = %d\n%s", code, stderr)
	}
	got := showJSON(t, item.ID)
	if got.Owner != "bea" || got.NotifyPane != "p1" || got.NotifyMachine != "m2" {
		t.Fatalf("owner only = owner %q pane %q machine %q", got.Owner, got.NotifyPane, got.NotifyMachine)
	}
	if _, stderr, code := runCLI("reassign", item.ID, "--notify-pane", "p9"); code != 0 {
		t.Fatalf("pane = %d\n%s", code, stderr)
	}
	got = showJSON(t, item.ID)
	if got.Owner != "bea" || got.NotifyPane != "p9" || got.NotifyMachine != "m2" {
		t.Fatalf("pane only = owner %q pane %q machine %q", got.Owner, got.NotifyPane, got.NotifyMachine)
	}
}

func TestReassignRequiresAFlag(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	id := askRelay(t, nil, "q")
	_, stderr, code := runCLI("reassign", id)
	if code != 2 || !strings.Contains(stderr, "--owner") {
		t.Fatalf("reassign = %d\n%s", code, stderr)
	}
	if showJSON(t, id).Owner != "ada" {
		t.Fatal("reassign with no flags changed the item")
	}
}

func TestReassignRefusesDoneAndWithdrawn(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	done := askRelay(t, nil, "done item")
	if _, stderr, code := runCLI("done", done, "finished"); code != 0 {
		t.Fatalf("done = %d\n%s", code, stderr)
	}
	_, stderr, code := runCLI("reassign", done, "--owner", "bea")
	if code == 0 || !strings.Contains(stderr, "done") {
		t.Fatalf("reassign done = %d\n%s", code, stderr)
	}
	withdrawn := askRelay(t, nil, "withdrawn item")
	if _, stderr, code := runCLI("withdraw", withdrawn, "nope"); code != 0 {
		t.Fatalf("withdraw = %d\n%s", code, stderr)
	}
	_, stderr, code = runCLI("reassign", withdrawn, "--notify-pane", "p9")
	if code == 0 || !strings.Contains(stderr, "withdrawn") {
		t.Fatalf("reassign withdrawn = %d\n%s", code, stderr)
	}
}

func TestReassignAnswerNotifiesTheNewPane(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("DECIDE_AS_HANDLE", "")
	logPath := state + "/notify.log"
	installNotifier(t, logPath, "exit 0")
	writeNotifyConfig(t, `notifier "$DECIDE_OWNER" "$DECIDE_PANE"`, "default-pane")
	id := askRelay(t, []string{"--notify-pane", "old-pane"}, "q")
	if _, stderr, code := runCLI("reassign", id, "--owner", "bea", "--notify-pane", "new-pane"); code != 0 {
		t.Fatalf("reassign = %d\n%s", code, stderr)
	}
	if _, err := os.Stat(logPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("reassign ran the notifier: %v", err)
	}
	if _, stderr, code := runCLI("answer", id, "a"); code != 0 {
		t.Fatalf("answer = %d\n%s", code, stderr)
	}
	got := notifyLog(t, logPath)
	if len(got) != 1 || got[0] != "bea new-pane" {
		t.Fatalf("notify log = %#v", got)
	}
}

func TestReassignDoesNotResendAQueuedNotification(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("DECIDE_AS_HANDLE", "")
	logPath := state + "/notify.log"
	installNotifier(t, logPath, "exit 0")
	writeNotifyConfig(t, `notifier "$DECIDE_PANE"`, "default-pane")
	id := askRelay(t, []string{"--notify-pane", "old-pane"}, "q")
	if _, stderr, code := runCLI("answer", id, "a"); code != 0 {
		t.Fatalf("answer = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("reassign", id, "--notify-pane", "new-pane"); code != 0 {
		t.Fatalf("reassign = %d\n%s", code, stderr)
	}
	got := notifyLog(t, logPath)
	if len(got) != 1 || got[0] != "old-pane" {
		t.Fatalf("notify log = %#v, want only the original pane", got)
	}
}

func TestReassignHumanReplyNotifiesTheNewPane(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("DECIDE_AS_HANDLE", "")
	t.Setenv("HERDR_PANE_ID", "")
	herdr := herdrtest.Install(t)
	id := askRelay(t, []string{"--notify-pane", "old-pane"}, "q")
	if _, stderr, code := runCLI("reassign", id, "--notify-pane", "new-pane"); code != 0 {
		t.Fatalf("reassign = %d\n%s", code, stderr)
	}
	if argv := herdr.Argv(); len(argv) != 0 {
		t.Fatalf("reassign notified %#v", argv)
	}
	if _, stderr, code := runCLI("send", "--id", id, "the reply"); code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	argv := strings.Join(herdr.Argv(), "\n")
	if !strings.Contains(argv, "agent prompt new-pane") || strings.Contains(argv, "old-pane") {
		t.Fatalf("herdr argv = %q", argv)
	}
}

func TestReassignClearPaneRoutesNothing(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("DECIDE_AS_HANDLE", "")
	t.Setenv("HERDR_PANE_ID", "")
	logPath := state + "/notify.log"
	installNotifier(t, logPath, "exit 0")
	writeNotifyConfig(t, `notifier "$DECIDE_PANE"`, "default-pane")
	herdr := herdrtest.Install(t)
	id := askRelay(t, []string{"--notify-pane", "old-pane"}, "q")
	if _, stderr, code := runCLI("reassign", id, "--notify-pane", ""); code != 0 {
		t.Fatalf("reassign = %d\n%s", code, stderr)
	}
	got := showJSON(t, id)
	if got.NotifyPane != "" || got.NotifyMachine != "" || !got.NotifyClosed {
		t.Fatalf("cleared = pane %q machine %q closed %v", got.NotifyPane, got.NotifyMachine, got.NotifyClosed)
	}
	if _, stderr, code := runCLI("answer", id, "a"); code != 0 {
		t.Fatalf("answer = %d\n%s", code, stderr)
	}
	if _, err := os.Stat(logPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("cleared pane ran the notifier: %v", err)
	}
	open := askRelay(t, []string{"--notify-pane", "old-pane"}, "reply")
	if _, stderr, code := runCLI("reassign", open, "--notify-pane", ""); code != 0 {
		t.Fatalf("reassign open = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("send", "--id", open, "hello"); code != 0 {
		t.Fatalf("send = %d\n%s", code, stderr)
	}
	if argv := herdr.Argv(); len(argv) != 0 {
		t.Fatalf("cleared pane notified %#v", argv)
	}
}

func TestHubReassignPassesTheCallerHandle(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub", "machine": "m2"}`)
	herdr := herdrtest.Install(t)
	herdr.Write("local.agents", herdrAgentsW1P3)
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	var stderr strings.Builder
	if code := run([]string{"reassign", "D-1", "--notify-pane", "p9"}, &strings.Builder{}, &stderr); code != 0 {
		t.Fatalf("reassign = %d, stderr %q", code, stderr.String())
	}
	env := fake.Lines("decide.env")
	found := false
	for _, kv := range env {
		if kv == "DECIDE_AS_HANDLE=herdr:m2/s/w1:p3" {
			found = true
		}
	}
	if !found {
		t.Fatalf("hub env = %q", env)
	}
}

func TestReassignDocs(t *testing.T) {
	_, stderr, code := runCLI()
	if code != 2 || !strings.Contains(stderr, "decide reassign <id>") || !strings.Contains(stderr, "owning tab is retired") {
		t.Fatalf("usage = %d\n%s", code, stderr)
	}
	stdout, skillErr, skillCode := runCLI("skill")
	if skillCode != 0 || !strings.Contains(stdout, "owning tab is retired") {
		t.Fatalf("skill = %d\n%s\n%s", skillCode, skillErr, stdout)
	}
}
