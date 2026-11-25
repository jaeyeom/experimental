package main

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestAgentDoneOnUnreadItemStaysInTheInbox(t *testing.T) {
	useState(t)
	id := askRelay(t, nil, "result ready")
	_, stderr, code := runCLI("done", id, "shipped")
	if code != 0 {
		t.Fatalf("done = %d\n%s", code, stderr)
	}
	if !strings.Contains(stderr, "stays in the inbox until the user reads it") {
		t.Fatalf("stderr = %q", stderr)
	}
	got := showJSON(t, id)
	if got.State != decide.StateDoneUnread || got.Outcome != "shipped" || !got.Unread {
		t.Fatalf("item = state %s outcome %q unread %v", got.State, got.Outcome, got.Unread)
	}
	stdout, stderr, code := runCLI("list")
	if code != 0 || !strings.Contains(stdout, id) {
		t.Fatalf("list = %d %q\n%s", code, stdout, stderr)
	}
	if _, stderr, code := runCLI("done", "--via", "ui", id, "archived"); code != 0 {
		t.Fatalf("ui done = %d\n%s", code, stderr)
	}
	got = showJSON(t, id)
	if got.State != decide.StateDone || got.Outcome != "shipped" {
		t.Fatalf("archived item = state %s outcome %q", got.State, got.Outcome)
	}
}

func TestDoneViaUIClosesAtOnceAndRejectsUnknownVia(t *testing.T) {
	useState(t)
	id := askRelay(t, nil, "unread")
	if _, stderr, code := runCLI("done", "--via", "bot", id, "x"); code != 2 {
		t.Fatalf("done --via bot = %d\n%s", code, stderr)
	}
	_, stderr, code := runCLI("done", "--via", "ui", id, "closed")
	if code != 0 || strings.Contains(stderr, "stays in the inbox") {
		t.Fatalf("ui done = %d\n%s", code, stderr)
	}
	if got := showJSON(t, id); got.State != decide.StateDone || got.Outcome != "closed" {
		t.Fatalf("item = state %s outcome %q", got.State, got.Outcome)
	}
}

func TestHubDoneViaUIKeepsTheFlagWithLocalText(t *testing.T) {
	fake, state := setupHub(t, `{"hub": "me@hub"}`)
	path := writeBody(t, "archived\n")
	var stderr strings.Builder
	if code := run([]string{"done", "--via", "ui", "D-1", "--text-file", path}, &strings.Builder{}, &stderr); code != 0 {
		t.Fatalf("done = %d\n%s", code, stderr.String())
	}
	if got := fake.Read("decide.args"); got != "done\n--via\nui\nD-1\narchived\n" {
		t.Fatalf("hub args = %q", got)
	}
	assertNoLocalState(t, state)
}

func TestCLIAnswerOnClosedItemIsRefused(t *testing.T) {
	useState(t)
	out, stderr, code := runCLI("ask", "--class", "decision", "--owner", "ada", "--kind", "relay", "--option", "y:Yes", "--option", "n:No", "--rec", "y", "post it?")
	if code != 0 {
		t.Fatalf("ask = %d\n%s", code, stderr)
	}
	id := strings.TrimSpace(out)
	if _, stderr, code := runCLI("done", id, "Not posted: cancelled."); code != 0 {
		t.Fatalf("done = %d\n%s", code, stderr)
	}
	_, stderr, code = runCLI("answer", id, "y")
	if code == 0 {
		t.Fatalf("answer on done-unread succeeded, want non-zero exit")
	}
	if !strings.Contains(stderr, "already closed by ada") {
		t.Fatalf("stderr = %q, want already closed by ada", stderr)
	}
	got := showJSON(t, id)
	if got.State != decide.StateDoneUnread || got.Answer != nil || len(got.Delivery) != 0 {
		t.Fatalf("item = state %s answer %+v delivery %+v", got.State, got.Answer, got.Delivery)
	}
}
