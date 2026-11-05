package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestAskPrintsSequentialIDsAndStoresNotifyPane(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("XDG_STATE_HOME", dir)
	sentinel := filepath.Join(dir, "spawned")
	bin := filepath.Join(dir, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	script := "#!/bin/sh\ntouch '" + sentinel + "'\n"
	if err := os.WriteFile(filepath.Join(bin, "notify"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin)

	first := askRelay(t, nil, "first question")
	if first != "D-001" {
		t.Fatalf("first id = %q, want D-001", first)
	}
	second := askRelay(t, nil, "second question")
	if second != "D-002" {
		t.Fatalf("second id = %q, want D-002", second)
	}

	stdout, stderr, code := runCLI(
		"ask", "--owner", "ada", "--kind", "relay",
		"--option", "a:Alpha", "--option", "b:Beta", "--rec", "a",
		"--json", "--notify-pane", "pane-9",
		"third question",
	)
	if code != 0 {
		t.Fatalf("ask --json code = %d\n%s", code, stderr)
	}
	var item decide.Item
	if err := json.Unmarshal([]byte(stdout), &item); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if item.ID != "D-003" {
		t.Fatalf("json id = %q, want D-003", item.ID)
	}
	if item.NotifyPane != "pane-9" {
		t.Fatalf("notifyPane = %q", item.NotifyPane)
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("notify command was spawned: %v", err)
	}
}

func TestListOrdersByDeadlineThenNewest(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("XDG_STATE_HOME", dir)

	later := askRelay(t, []string{"--deadline", "2026-12-02T00:00:00Z"}, "later deadline")
	sooner := askRelay(t, []string{"--deadline", "2026-12-01T00:00:00Z"}, "sooner deadline")
	older := askRelay(t, nil, "older open")
	newer := askRelay(t, nil, "newer open")
	answered := askRelay(t, nil, "answered item")
	if code := answer(t, answered, "a"); code != 0 {
		t.Fatalf("answer code = %d", code)
	}

	stdout, stderr, code := runCLI("list", "--state", "open")
	if code != 0 {
		t.Fatalf("list code = %d\n%s", code, stderr)
	}
	got := idsOf(stdout)
	want := []string{sooner, later, newer, older}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("list order = %v, want %v\n%s", got, want, stdout)
	}

	defOut, defErr, defCode := runCLI("list")
	if defCode != 0 {
		t.Fatalf("default list code = %d\n%s", defCode, defErr)
	}
	if defOut != stdout {
		t.Fatalf("default list = %q, want %q", defOut, stdout)
	}
	for _, id := range got {
		if id == answered {
			t.Fatalf("open list includes answered %s", answered)
		}
	}
}

func TestAnswerRelayFreeTextAndTypeIn(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("XDG_STATE_HOME", dir)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	id := askRelay(t, nil, "pick one")
	if got := showJSON(t, id); !got.FreeText {
		t.Fatal("relay free text is not the default")
	}
	const text = "not one of the options"
	if _, stderr, code := runCLI("answer", id, text); code != 0 {
		t.Fatalf("free text answer code = %d\n%s", code, stderr)
	}
	item := showJSON(t, id)
	if item.State != decide.StateAnswered || item.Answer == nil || item.Answer.Value != text {
		t.Fatalf("show answer = %+v", item.Answer)
	}

	punct := askRelay(t, nil, "keep marks")
	raw := `say "hi" $(uname)`
	if _, stderr, code := runCLI("answer", punct, raw); code != 0 {
		t.Fatalf("punctuation answer code = %d\n%s", code, stderr)
	}
	if got := showJSON(t, punct); got.Answer == nil || got.Answer.Value != raw {
		t.Fatalf("show answer = %+v", got.Answer)
	}

	only := askRelay(t, []string{"--options-only"}, "keys only")
	if got := showJSON(t, only); got.FreeText {
		t.Fatal("--options-only still allowed free text")
	}
	if _, stderr, code := runCLI("answer", only, text); code == 0 {
		t.Fatalf("options-only accepted free text\n%s", stderr)
	}
	if got := showJSON(t, only); got.State != decide.StateOpen || got.Answer != nil {
		t.Fatalf("options-only item changed: %+v", got)
	}

	free := askRelay(t, []string{"--free-text"}, "or type")
	if _, stderr, code := runCLI("answer", free, "custom words"); code != 0 {
		t.Fatalf("free text answer code = %d\n%s", code, stderr)
	}
	got := showJSON(t, free)
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "custom words" || got.Answer.AnsweredBy != "human" {
		t.Fatalf("free text answer = %+v", got.Answer)
	}

	keyed := askRelay(t, nil, "use the key")
	if _, stderr, code := runCLI("answer", keyed, "b"); code != 0 {
		t.Fatalf("option answer code = %d\n%s", code, stderr)
	}
	keyedItem := showJSON(t, keyed)
	if keyedItem.Answer == nil || keyedItem.Answer.Value != "b" || keyedItem.Answer.AnsweredBy != "human" {
		t.Fatalf("option answer = %+v", keyedItem.Answer)
	}

	typeIn := askTypeIn(t, "type this", "the box")
	if _, stderr, code := runCLI("answer", typeIn, "typed"); code == 0 {
		t.Fatalf("answer on type-in succeeded\n%s", stderr)
	}
	if got := showJSON(t, typeIn); got.State != decide.StateOpen || got.Answer != nil {
		t.Fatalf("type-in changed: %+v", got)
	}
}

func TestAskOptionsOnlyConfigFlagWins(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	writeOptionsOnlyConfig(t)

	id := askRelay(t, nil, "config default")
	if got := showJSON(t, id); got.FreeText {
		t.Fatal("config options-only still allowed free text")
	}
	if _, stderr, code := runCLI("answer", id, "not one of the options"); code == 0 {
		t.Fatalf("config options-only accepted free text\n%s", stderr)
	}

	on := askRelay(t, []string{"--free-text"}, "flag wins")
	if got := showJSON(t, on); !got.FreeText {
		t.Fatal("--free-text did not override the config default")
	}
	raw := `say "hi" $(uname)`
	if _, stderr, code := runCLI("answer", on, raw); code != 0 {
		t.Fatalf("free text answer code = %d\n%s", code, stderr)
	}
	if got := showJSON(t, on); got.Answer == nil || got.Answer.Value != raw {
		t.Fatalf("show answer = %+v", got.Answer)
	}
}

func TestAnswerRunsNotifyTemplate(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	logPath := filepath.Join(state, "notify.log")
	installNotifier(t, logPath, "exit 0")
	writeNotifyConfig(t, "notifier {id} {owner} {pane} '{answer}'", "default-pane")

	plain := askRelay(t, nil, "use the default pane")
	if _, err := os.Stat(logPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("ask ran the notify template")
	}
	if _, stderr, code := runCLI("answer", plain, "b"); code != 0 {
		t.Fatalf("answer code = %d\n%s", code, stderr)
	}
	if got := notifyLog(t, logPath); len(got) != 1 || got[0] != plain+" ada default-pane b" {
		t.Fatalf("notify log = %#v", got)
	}

	overridden := askRelay(t, []string{"--notify-pane", "pane-9"}, "use the item pane")
	if _, stderr, code := runCLI("answer", overridden, "a"); code != 0 {
		t.Fatalf("answer code = %d\n%s", code, stderr)
	}
	got := notifyLog(t, logPath)
	if len(got) != 2 || got[0] != plain+" ada default-pane b" || got[1] != overridden+" ada pane-9 a" {
		t.Fatalf("notify log = %#v", got)
	}
	if showJSON(t, plain).NotifyPane != "" || showJSON(t, overridden).NotifyPane != "pane-9" {
		t.Fatal("notify pane leaked onto the other item")
	}
}

func TestNotifyFailureKeepsAnswer(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	installNotifier(t, filepath.Join(state, "notify.log"), "exit 1")
	writeNotifyConfig(t, "notifier {id}", "pane")

	id := askRelay(t, nil, "keep me")
	stdout, stderr, code := runCLI("answer", id, "b")
	if code == 0 {
		t.Fatalf("failing notify succeeded\n%s", stdout)
	}
	if !strings.Contains(stderr, "notify") {
		t.Fatalf("stderr = %q", stderr)
	}
	got := showJSON(t, id)
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "b" {
		t.Fatalf("answer was not kept: %+v", got)
	}
}

func TestDoneAndWithdrawSkipNotify(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	logPath := filepath.Join(state, "notify.log")
	installNotifier(t, logPath, "exit 0")
	writeNotifyConfig(t, "notifier {id}", "pane")

	typeIn := askTypeIn(t, "ship the note", "the form")
	if _, stderr, code := runCLI("done", typeIn, "typed it"); code != 0 {
		t.Fatalf("done code = %d\n%s", code, stderr)
	}
	relay := askRelay(t, nil, "withdraw me")
	if _, stderr, code := runCLI("withdraw", relay, "not needed"); code != 0 {
		t.Fatalf("withdraw code = %d\n%s", code, stderr)
	}
	if _, err := os.Stat(logPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("done or withdraw ran notify: %v", err)
	}
	if showJSON(t, typeIn).State != decide.StateDone {
		t.Fatal("type-in was not done")
	}
}

func TestWithdrawAndDoneStoreText(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("XDG_STATE_HOME", dir)

	left := askRelay(t, nil, "withdraw me")
	if _, stderr, code := runCLI("withdraw", left, "not needed"); code != 0 {
		t.Fatalf("withdraw code = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("withdraw", left, "again"); code == 0 {
		t.Fatalf("second withdraw succeeded\n%s", stderr)
	}
	got := showJSON(t, left)
	if got.State != decide.StateWithdrawn || got.Reason != "not needed" {
		t.Fatalf("withdrawn item = %+v", got)
	}

	doneID := askTypeIn(t, "ship the note", "the form")
	if _, stderr, code := runCLI("done", doneID, "typed it"); code != 0 {
		t.Fatalf("done code = %d\n%s", code, stderr)
	}
	if _, stderr, code := runCLI("done", doneID, "again"); code == 0 {
		t.Fatalf("second done succeeded\n%s", stderr)
	}
	finished := showJSON(t, doneID)
	if finished.State != decide.StateDone || finished.Outcome != "typed it" {
		t.Fatalf("done item = %+v", finished)
	}
	if _, _, code := runCLI("withdraw", doneID, "too late"); code == 0 {
		t.Fatal("withdraw of a done item succeeded")
	}
	if _, stderr, code := runCLI("withdraw", left, ""); code == 0 {
		t.Fatalf("empty reason succeeded\n%s", stderr)
	}
}

func TestShowPrintsTypeInTextAndWhere(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("XDG_STATE_HOME", dir)
	id := askTypeIn(t, "exact phrase", "the reply box")
	stdout, stderr, code := runCLI("show", id)
	if code != 0 {
		t.Fatalf("show code = %d\n%s", code, stderr)
	}
	for _, needle := range []string{
		"id: " + id,
		"question: what should I type",
		"typeText: exact phrase",
		"where: the reply box",
		"kind: type-in",
		"owner: ada",
	} {
		if !strings.Contains(stdout, needle) {
			t.Fatalf("show missing %q\n%s", needle, stdout)
		}
	}
}

func askRelay(t *testing.T, extra []string, question string) string {
	t.Helper()
	args := relayArgs(question)
	if len(extra) > 0 {
		args = append(args[:len(args)-1], append(extra, question)...)
	}
	stdout, stderr, code := runCLI(args...)
	if code != 0 {
		t.Fatalf("ask %q code = %d\n%s", question, code, stderr)
	}
	return strings.TrimSpace(stdout)
}

func relayArgs(question string) []string {
	return []string{
		"ask", "--owner", "ada", "--kind", "relay",
		"--option", "a:Alpha", "--option", "b:Beta", "--rec", "a",
		question,
	}
}

func askTypeIn(t *testing.T, text, where string) string {
	t.Helper()
	stdout, stderr, code := runCLI(
		"ask", "--owner", "ada", "--kind", "type-in",
		"--type-text", text, "--where", where,
		"what should I type",
	)
	if code != 0 {
		t.Fatalf("ask type-in code = %d\n%s", code, stderr)
	}
	return strings.TrimSpace(stdout)
}

func answer(t *testing.T, id, value string) int {
	t.Helper()
	_, stderr, code := runCLI("answer", id, value)
	if code != 0 {
		t.Log(stderr)
	}
	return code
}

func showJSON(t *testing.T, id string) decide.Item {
	t.Helper()
	stdout, stderr, code := runCLI("show", "--json", id)
	if code != 0 {
		t.Fatalf("show --json code = %d\n%s", code, stderr)
	}
	return decodeItem(t, stdout)
}

func decodeItem(t *testing.T, raw string) decide.Item {
	t.Helper()
	var item decide.Item
	if err := json.Unmarshal([]byte(raw), &item); err != nil {
		t.Fatalf("json: %v\n%s", err, raw)
	}
	return item
}

func idsOf(out string) []string {
	var ids []string
	for _, line := range strings.Split(strings.TrimSpace(out), "\n") {
		if line == "" {
			continue
		}
		ids = append(ids, strings.Split(line, "\t")[0])
	}
	return ids
}

func installNotifier(t *testing.T, logPath, tail string) {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	script := "#!/bin/sh\nprintf '%s\\n' \"$*\" >> '" + logPath + "'\n" + tail + "\n"
	if err := os.WriteFile(filepath.Join(bin, "notifier"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func writeOptionsOnlyConfig(t *testing.T) {
	t.Helper()
	dir := filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body := []byte("{\"optionsOnly\": true}\n")
	if err := os.WriteFile(filepath.Join(dir, "config.json"), body, 0o600); err != nil {
		t.Fatal(err)
	}
}

func writeNotifyConfig(t *testing.T, template, pane string) {
	t.Helper()
	dir := filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body, err := json.Marshal(fileConfig{NotifyTemplate: template, NotifyPane: pane})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.json"), append(body, '\n'), 0o600); err != nil {
		t.Fatal(err)
	}
}

func notifyLog(t *testing.T, path string) []string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return strings.Split(strings.TrimSpace(string(data)), "\n")
}

func runCLI(args ...string) (string, string, int) {
	var stdout, stderr bytes.Buffer
	code := run(args, &stdout, &stderr)
	return stdout.String(), stderr.String(), code
}
