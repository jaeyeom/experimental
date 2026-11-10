package main

import (
	"strings"
	"testing"
)

func TestSnoozeHidesFromListAndShowsUntil(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	id := askRelay(t, nil, "snooze me")
	other := askRelay(t, nil, "keep me")

	stdout, stderr, code := runCLI("snooze", id, "1h")
	if code != 0 {
		t.Fatalf("snooze code = %d\n%s", code, stderr)
	}
	if !strings.Contains(stdout, "snoozed until: ") {
		t.Fatalf("snooze stdout =\n%s", stdout)
	}
	item := showJSON(t, id)
	if item.SnoozedUntil == nil || item.SnoozedAt == nil {
		t.Fatalf("show --json = %+v", item)
	}
	raw, _, _ := runCLI("show", "--json", id)
	if !strings.Contains(raw, `"snoozedUntil"`) {
		t.Fatalf("show --json lacks snoozedUntil\n%s", raw)
	}

	list, _, code := runCLI("list")
	if code != 0 || strings.Contains(list, id) || !strings.Contains(list, other) {
		t.Fatalf("list code %d =\n%s", code, list)
	}
	snoozed, _, code := runCLI("list", "--snoozed")
	if code != 0 || !strings.Contains(snoozed, id) || strings.Contains(snoozed, other) || !strings.Contains(snoozed, "snoozed until ") {
		t.Fatalf("list --snoozed code %d =\n%s", code, snoozed)
	}

	stdout, stderr, code = runCLI("unsnooze", id)
	if code != 0 || strings.Contains(stdout, "snoozed until") {
		t.Fatalf("unsnooze code %d\n%s%s", code, stdout, stderr)
	}
	list, _, _ = runCLI("list")
	if !strings.Contains(list, id) {
		t.Fatalf("list after unsnooze =\n%s", list)
	}
}

func TestReplyWakesASnoozedItemFromTheCLI(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	id := askRelay(t, nil, "wake me")
	if _, stderr, code := runCLI("snooze", id, "mon 9:00"); code != 0 {
		t.Fatalf("snooze code = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("reply", id, "news"); code != 0 {
		t.Fatalf("reply code = %d\n%s", code, stderr)
	}
	item := showJSON(t, id)
	if item.SnoozedUntil != nil || !item.Unread {
		t.Fatalf("after reply = %+v", item)
	}
}

func TestSnoozeRejectsBadInput(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	id := askRelay(t, nil, "bad")
	if _, _, code := runCLI("snooze", id, "soon"); code == 0 {
		t.Fatal("snooze soon succeeded")
	}
	if _, _, code := runCLI("snooze", id); code == 0 {
		t.Fatal("snooze without a time succeeded")
	}
	if _, _, code := runCLI("done", id, "ok"); code != 0 {
		t.Fatal("done failed")
	}
	if _, stderr, code := runCLI("snooze", id, "1h"); code == 0 || !strings.Contains(stderr, "done") {
		t.Fatalf("snooze done code %d\n%s", code, stderr)
	}
}
