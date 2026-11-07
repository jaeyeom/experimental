package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// TestMain points config and state at temporary directories so a CLI test
// cannot run the live notify command. A test may still override either variable.
func TestMain(m *testing.M) {
	cfg, err := os.MkdirTemp("", "decide-cli-config")
	if err != nil {
		fmt.Fprintf(os.Stderr, "cli config dir: %v\n", err)
		os.Exit(1)
	}
	state, err := os.MkdirTemp("", "decide-cli-state")
	if err != nil {
		fmt.Fprintf(os.Stderr, "cli state dir: %v\n", err)
		os.Exit(1)
	}
	if err := os.Setenv("XDG_CONFIG_HOME", cfg); err != nil {
		fmt.Fprintf(os.Stderr, "cli config env: %v\n", err)
		os.Exit(1)
	}
	if err := os.Setenv("XDG_STATE_HOME", state); err != nil {
		fmt.Fprintf(os.Stderr, "cli state env: %v\n", err)
		os.Exit(1)
	}
	code := m.Run()
	_ = os.RemoveAll(cfg)
	_ = os.RemoveAll(state)
	os.Exit(code)
}

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

func TestAnswerWithoutNotifyTemplateDoesNotSpawn(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("XDG_STATE_HOME", dir)
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	if err := os.MkdirAll(filepath.Join(cfg, "decide"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(cfg, "decide", "config.json"), []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	sentinel := filepath.Join(dir, "spawned")
	bin := filepath.Join(dir, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	script := "#!/bin/sh\ntouch '" + sentinel + "'\n"
	if err := os.WriteFile(filepath.Join(bin, "sh"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))

	id := askRelay(t, nil, "no notify")
	if code := answer(t, id, "a"); code != 0 {
		t.Fatalf("answer code = %d", code)
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("notify command was spawned: %v", err)
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
	writeNotifyConfig(t, `notifier "$DECIDE_ID" "$DECIDE_OWNER" "$DECIDE_PANE" "$DECIDE_ANSWER"`, "default-pane")

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

func TestNotifyPassesAnswerInEnvironment(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	logPath := filepath.Join(state, "notify.log")
	installEnvNotifier(t, logPath)
	writeNotifyConfig(t, "notifier", "default-pane")

	raw := `say "hi" $(uname)`
	id := askRelay(t, []string{"--notify-pane", "pane-9"}, "unsafe answer")
	if _, stderr, code := runCLI("answer", id, raw); code != 0 {
		t.Fatalf("answer code = %d\n%s", code, stderr)
	}
	got := notifyLog(t, logPath)
	want := []string{id, "ada", "pane-9", raw}
	if len(got) != len(want) {
		t.Fatalf("notify log = %#v", got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("notify log = %#v", got)
		}
	}
	if showJSON(t, id).Answer == nil || showJSON(t, id).Answer.Value != raw {
		t.Fatal("answer was not kept")
	}
}

func TestParentDecideEnvDoesNotOverrideNotify(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_ID", "stale")
	t.Setenv("DECIDE_ANSWER", "stale")
	t.Setenv("DECIDE_OWNER", "stale")
	t.Setenv("DECIDE_PANE", "stale")
	logPath := filepath.Join(state, "notify.log")
	installEnvNotifier(t, logPath)
	writeNotifyConfig(t, "notifier", "default-pane")

	id := askRelay(t, []string{"--notify-pane", "pane-9"}, "parent env")
	if _, stderr, code := runCLI("answer", id, "saved"); code != 0 {
		t.Fatalf("answer code = %d\n%s", code, stderr)
	}
	got := notifyLog(t, logPath)
	want := []string{id, "ada", "pane-9", "saved"}
	if len(got) != len(want) {
		t.Fatalf("notify log = %#v", got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("notify log = %#v, want %#v", got, want)
		}
	}
}

func TestNotifyExportsQuestionAndLabel(t *testing.T) {
	t.Run("option", func(t *testing.T) {
		dump := setupNotifyDump(t)
		id := askChoice(t, "Ship the change?\ndetails", "--notify-pane", "pane-9")
		if _, stderr, code := runCLI("answer", id, "a"); code != 0 {
			t.Fatalf("answer code = %d\n%s", code, stderr)
		}
		item := showJSON(t, id)
		if item.Answer == nil || item.Answer.Value != "a" {
			t.Fatalf("answer = %+v", item.Answer)
		}
		assertNotifyDump(t, dump, item, "Ship the change?", "Ship it")
	})

	t.Run("free text", func(t *testing.T) {
		dump := setupNotifyDump(t)
		raw := "say \"hi\"\n$(uname)"
		id := askChoice(t, "first line only\nsecond line", "--free-text", "--notify-pane", "pane-9")
		if _, stderr, code := runCLI("answer", id, raw); code != 0 {
			t.Fatalf("answer code = %d\n%s", code, stderr)
		}
		item := showJSON(t, id)
		if item.Answer == nil || item.Answer.Value != raw {
			t.Fatalf("answer = %+v", item.Answer)
		}
		assertNotifyDump(t, dump, item, "first line only", "")
		if strings.Contains(readDump(t, dump, "DECIDE_QUESTION"), "second line") {
			t.Fatal("question included the next line")
		}
	})

	t.Run("long question", func(t *testing.T) {
		dump := setupNotifyDump(t)
		head := strings.Repeat("Q", 199) + "한" + "TAIL"
		id := askChoice(t, head+"\nnext-line", "--notify-pane", "pane-9")
		if _, stderr, code := runCLI("answer", id, "a"); code != 0 {
			t.Fatalf("answer code = %d\n%s", code, stderr)
		}
		want := strings.Repeat("Q", 199) + "한"
		if utf8.RuneCountInString(want) != 200 {
			t.Fatalf("question rune count %d", utf8.RuneCountInString(want))
		}
		item := showJSON(t, id)
		assertNotifyDump(t, dump, item, want, "Ship it")
	})
}

func setupNotifyDump(t *testing.T) string {
	t.Helper()
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	for _, name := range []string{
		"DECIDE_ID", "DECIDE_ANSWER", "DECIDE_ANSWER_LABEL", "DECIDE_OWNER",
		"DECIDE_PANE", "DECIDE_KIND", "DECIDE_QUESTION", "DECIDE_ANSWERED_AT",
		"DECIDE_ANSWERED_BY",
	} {
		t.Setenv(name, "stale")
	}
	dump := t.TempDir()
	var b strings.Builder
	b.WriteString("#!/bin/sh\n")
	for _, name := range []string{
		"DECIDE_ID", "DECIDE_ANSWER", "DECIDE_ANSWER_LABEL", "DECIDE_OWNER",
		"DECIDE_PANE", "DECIDE_KIND", "DECIDE_QUESTION", "DECIDE_ANSWERED_AT",
		"DECIDE_ANSWERED_BY",
	} {
		fmt.Fprintf(&b, "printf '%%s' \"$%s\" > '%s'\n", name, filepath.Join(dump, name))
	}
	fmt.Fprintf(&b, "printf '%%s' \"$1\" > '%s'\n", filepath.Join(dump, "arg"))
	installNotifierScript(t, b.String())
	writeNotifyConfig(t, "notifier '{answer}'", "default-pane")
	return dump
}

func askChoice(t *testing.T, question string, extra ...string) string {
	t.Helper()
	args := []string{
		"ask", "--owner", "ada", "--kind", "relay",
		"--option", "a:Ship it", "--option", "b:Later", "--rec", "a",
	}
	args = append(args, extra...)
	args = append(args, question)
	stdout, stderr, code := runCLI(args...)
	if code != 0 {
		t.Fatalf("ask code = %d\n%s", code, stderr)
	}
	return strings.TrimSpace(stdout)
}

func assertNotifyDump(t *testing.T, dump string, item decide.Item, question, label string) {
	t.Helper()
	want := map[string]string{
		"DECIDE_ID":           item.ID,
		"DECIDE_ANSWER":       item.Answer.Value,
		"DECIDE_ANSWER_LABEL": label,
		"DECIDE_OWNER":        "ada",
		"DECIDE_PANE":         "pane-9",
		"DECIDE_KIND":         "relay",
		"DECIDE_QUESTION":     question,
		"DECIDE_ANSWERED_AT":  item.AnsweredAt.Format(time.RFC3339),
		"DECIDE_ANSWERED_BY":  "human",
		"arg":                 "{answer}",
	}
	for name, value := range want {
		if got := readDump(t, dump, name); got != value {
			t.Fatalf("%s = %q, want %q", name, got, value)
		}
	}
}

func readDump(t *testing.T, dump, name string) string {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(dump, name))
	if err != nil {
		t.Fatalf("read %s: %v", name, err)
	}
	return string(data)
}

func TestNotifyFailureKeepsAnswer(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	installNotifier(t, filepath.Join(state, "notify.log"), "exit 1")
	writeNotifyConfig(t, `notifier "$DECIDE_ID"`, "pane")

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
	writeNotifyConfig(t, `notifier "$DECIDE_ID"`, "pane")

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
	installNotifierScript(t, "#!/bin/sh\nprintf '%s\\n' \"$*\" >> '"+logPath+"'\n"+tail+"\n")
}

func installEnvNotifier(t *testing.T, logPath string) {
	t.Helper()
	script := "#!/bin/sh\n" +
		"printf '%s\\n' \"$DECIDE_ID\" >> '" + logPath + "'\n" +
		"printf '%s\\n' \"$DECIDE_OWNER\" >> '" + logPath + "'\n" +
		"printf '%s\\n' \"$DECIDE_PANE\" >> '" + logPath + "'\n" +
		"printf '%s\\n' \"$DECIDE_ANSWER\" >> '" + logPath + "'\n"
	installNotifierScript(t, script)
}

func installNotifierScript(t *testing.T, script string) {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(bin, "notifier"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func TestLoadConfigComposeTargets(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body := []byte("{\"targets\":[{\"name\":\"inbox\",\"pane\":\"pane-9\"}],\"sendTemplate\":\"herdr agent prompt {pane}\"}\n")
	if err := os.WriteFile(filepath.Join(dir, "config.json"), body, 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	targets := inboxTargets(cfg)
	if len(targets) != 1 || targets[0].Name != "inbox" || targets[0].Pane != "pane-9" {
		t.Fatalf("targets = %+v", targets)
	}
	if cfg.SendTemplate != "herdr agent prompt {pane}" {
		t.Fatalf("template = %q", cfg.SendTemplate)
	}
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
	body, err := json.Marshal(decide.Config{NotifyTemplate: template, NotifyPane: pane})
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

func TestSendKeepsTheNoteAsOneArgument(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	bin := filepath.Join(state, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	logPath := filepath.Join(state, "argv.log")
	script := "#!/bin/sh\nfor arg in \"$@\"; do printf '%s' \"$arg\" >> '" + logPath + "'; printf '\\0' >> '" + logPath + "'; done\n"
	if err := os.WriteFile(filepath.Join(bin, "sentinel"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
	dir := filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	body := []byte("{\"targets\":[{\"name\":\"alpha\",\"pane\":\"pane-a\"}],\"sendTemplate\":\"sentinel {pane}\"}\n")
	if err := os.WriteFile(filepath.Join(dir, "config.json"), body, 0o600); err != nil {
		t.Fatal(err)
	}

	const note = "hello\nthere"
	stdout, stderr, code := runCLI("send", "--to", "alpha", "--json", note)
	if code != 0 {
		t.Fatalf("send code = %d\n%s", code, stderr)
	}
	var sent struct {
		Name string `json:"name"`
		Text string `json:"text"`
	}
	if err := json.Unmarshal([]byte(stdout), &sent); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if sent.Name != "alpha" || sent.Text != note {
		t.Fatalf("sent = %+v", sent)
	}
	raw, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatal(err)
	}
	parts := bytes.Split(raw, []byte{0})
	if len(parts) > 0 && len(parts[len(parts)-1]) == 0 {
		parts = parts[:len(parts)-1]
	}
	if len(parts) != 2 || string(parts[0]) != "pane-a" || string(parts[1]) != note {
		t.Fatalf("argv = %#v", parts)
	}

	if _, _, code = runCLI("send", "--to", "missing", "hi"); code == 0 {
		t.Fatal("unknown recipient was accepted")
	}
	if _, err := os.Stat(filepath.Join(state, "decide", decide.SendsName)); err != nil {
		t.Fatal(err)
	}
	started := showThreads(t)
	if len(started) != 1 || len(started[0].Messages) != 1 || started[0].Messages[0].Author != "human" || started[0].Messages[0].Text != note || started[0].Owner != "alpha" {
		t.Fatalf("thread = %+v", started)
	}
}

func TestDraftReplyReturnsToInbox(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	id := askDraft(t)
	if code := answer(t, id, "draft"); code != 0 {
		t.Fatalf("answer code = %d", code)
	}
	openList, _, code := runCLI("list")
	if code != 0 || strings.Contains(openList, id) {
		t.Fatalf("answered item stayed in the inbox\n%s", openList)
	}
	const draft = "Hello \"there\"\nplease send this draft"
	stdout, stderr, code := runCLI(
		"reply", id,
		"--option", "send:send",
		"--option", "edit:edit",
		"--option", "don't send:don't send",
		"--rec", "send",
		draft,
	)
	if code != 0 {
		t.Fatalf("reply code = %d\n%s\n%s", code, stderr, stdout)
	}
	got := showJSON(t, id)
	if got.State != decide.StateOpen || !got.Unread {
		t.Fatalf("state %s unread %v", got.State, got.Unread)
	}
	if got.Question != "what should I send?" || got.Answer == nil || got.Answer.Value != "draft" || got.NotifyPane != "pane-9" {
		t.Fatalf("ask was dropped: %+v", got)
	}
	if !optionKeys(got.Options, "send", "edit", "don't send") || !recommendedKey(got.Options, "send") {
		t.Fatalf("options = %+v", got.Options)
	}
	if !hasMessage(got.Messages, "ada", "what should I send?") || !hasMessage(got.Messages, "human", "draft") || !hasMessage(got.Messages, "ada", draft) {
		t.Fatalf("messages = %+v", got.Messages)
	}
	inbox, _, code := runCLI("list")
	if code != 0 || !strings.Contains(inbox, id) {
		t.Fatalf("thread did not return to the inbox\n%s", inbox)
	}
	if code := answer(t, id, "send"); code != 0 {
		t.Fatalf("answer send code = %d", code)
	}
	sent := showJSON(t, id)
	if sent.State != decide.StateAnswered || sent.Answer == nil || sent.Answer.Value != "send" || sent.Answer.AnsweredBy != "human" {
		t.Fatalf("send = %+v", sent.Answer)
	}
	last := sent.Messages[len(sent.Messages)-1]
	if last.Author != "human" || last.Text != "send" {
		t.Fatalf("send message = %+v", last)
	}

	id2 := askDraft(t)
	if code := answer(t, id2, "draft"); code != 0 {
		t.Fatal(code)
	}
	if _, stderr, code := runCLI(
		"reply", id2,
		"--option", "send:send",
		"--option", "edit:edit",
		"--option", "don't send:don't send",
		"--rec", "send",
		draft,
	); code != 0 {
		t.Fatalf("reply code = %d\n%s", code, stderr)
	}
	bin := filepath.Join(state, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	logPath := filepath.Join(state, "argv.log")
	script := "#!/bin/sh\nfor arg in \"$@\"; do printf '%s' \"$arg\" >> '" + logPath + "'; printf '\\0' >> '" + logPath + "'; done\n"
	if err := os.WriteFile(filepath.Join(bin, "sentinel"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin)
	cfgDir := filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide")
	if err := os.MkdirAll(cfgDir, 0o755); err != nil {
		t.Fatal(err)
	}
	body := []byte("{\"sendTemplate\":\"sentinel {pane}\"}\n")
	if err := os.WriteFile(filepath.Join(cfgDir, "config.json"), body, 0o600); err != nil {
		t.Fatal(err)
	}
	const edits = "shorten the first line"
	if _, stderr, code := runCLI("send", "--id", id2, edits); code != 0 {
		t.Fatalf("send --id code = %d\n%s", code, stderr)
	}
	edited := showJSON(t, id2)
	if edited.State != decide.StateOpen || !optionKeys(edited.Options, "send", "edit", "don't send") {
		t.Fatalf("edits changed the pending answer: state %s options %+v", edited.State, edited.Options)
	}
	editMsg := edited.Messages[len(edited.Messages)-1]
	if editMsg.Author != "human" || editMsg.Text != edits {
		t.Fatalf("edit message = %+v", editMsg)
	}
	if !hasMessage(edited.Messages, "ada", draft) {
		t.Fatalf("draft was dropped: %+v", edited.Messages)
	}
	raw, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatal(err)
	}
	parts := bytes.Split(raw, []byte{0})
	if len(parts) > 0 && len(parts[len(parts)-1]) == 0 {
		parts = parts[:len(parts)-1]
	}
	if len(parts) != 2 || string(parts[0]) != "pane-9" || string(parts[1]) != id2+"\n"+edits {
		t.Fatalf("argv = %#v", parts)
	}
}

func askDraft(t *testing.T) string {
	t.Helper()
	stdout, stderr, code := runCLI(
		"ask", "--owner", "ada", "--kind", "relay",
		"--option", "draft:draft a message for me",
		"--option", "later:Later",
		"--rec", "later",
		"--notify-pane", "pane-9",
		"what should I send?",
	)
	if code != 0 {
		t.Fatalf("ask code = %d\n%s", code, stderr)
	}
	return strings.TrimSpace(stdout)
}

func showThreads(t *testing.T) []decide.Item {
	t.Helper()
	dir, err := decide.DefaultDir()
	if err != nil {
		t.Fatal(err)
	}
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	return items
}

func hasMessage(msgs []decide.Message, author, text string) bool {
	for _, msg := range msgs {
		if msg.Author == author && msg.Text == text {
			return true
		}
	}
	return false
}

func optionKeys(opts []decide.Option, keys ...string) bool {
	if len(opts) != len(keys) {
		return false
	}
	for i := range keys {
		if opts[i].Key != keys[i] {
			return false
		}
	}
	return true
}

func recommendedKey(opts []decide.Option, key string) bool {
	for _, opt := range opts {
		if opt.Key == key {
			return opt.Recommended
		}
	}
	return false
}

func TestTargetsJSONOmitsTemplates(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	const secret = "notify-template-secret"
	body := []byte(`{"notifyTemplate":"` + secret + `","sendTemplate":"send-template-secret","targets":[{"name":"alpha","pane":"pane-a"},{"name":"beta","pane":"pane-b"}]}` + "\n")
	if err := os.WriteFile(filepath.Join(dir, "config.json"), body, 0o600); err != nil {
		t.Fatal(err)
	}

	stdout, stderr, code := runCLI("targets", "--json")
	if code != 0 {
		t.Fatalf("targets code = %d\n%s", code, stderr)
	}
	if strings.Contains(stdout, secret) || strings.Contains(stdout, "send-template-secret") {
		t.Fatalf("templates leaked\n%s", stdout)
	}
	var targets []decide.ConfigTarget
	if err := json.Unmarshal([]byte(stdout), &targets); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if len(targets) != 2 || targets[0].Name != "alpha" || targets[0].Pane != "pane-a" || targets[1].Name != "beta" {
		t.Fatalf("targets = %+v", targets)
	}

	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	stdout, stderr, code = runCLI("targets", "--json")
	if code != 0 {
		t.Fatalf("empty code = %d\n%s", code, stderr)
	}
	if stdout != "[]\n" {
		t.Fatalf("empty targets = %q", stdout)
	}
}

func TestStatusSetListAndClearAgent(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	stdout, stderr, code := runCLI("status", "set", "--agent", "alpha", "fixing the inbox")
	if code != 0 {
		t.Fatalf("set code = %d\n%s", code, stderr)
	}
	if stdout != "alpha working fixing the inbox\n" {
		t.Fatalf("set out = %q", stdout)
	}

	_, stderr, code = runCLI("status", "set", "--agent", "beta", "--state", "blocked", "needs a look")
	if code != 0 {
		t.Fatalf("set beta code = %d\n%s", code, stderr)
	}
	stdout, stderr, code = runCLI("status", "set", "--agent", "beta", "still blocked")
	if code != 0 {
		t.Fatalf("keep state code = %d\n%s", code, stderr)
	}
	if stdout != "beta blocked still blocked\n" {
		t.Fatalf("kept state = %q", stdout)
	}

	_, _, code = runCLI("status", "set", "--agent", "gamma", "--state", "asleep", "nope")
	if code == 0 {
		t.Fatal("invalid state was accepted")
	}

	stdout, stderr, code = runCLI("status", "list")
	if code != 0 {
		t.Fatalf("list code = %d\n%s", code, stderr)
	}
	if !strings.Contains(stdout, "alpha working now fixing the inbox") || !strings.Contains(stdout, "beta blocked now still blocked") || strings.Contains(stdout, "gamma") {
		t.Fatalf("list = %q", stdout)
	}

	stdout, stderr, code = runCLI("status", "list", "--json")
	if code != 0 {
		t.Fatalf("json code = %d\n%s", code, stderr)
	}
	var board struct {
		Agents []decide.AgentStatus `json:"agents"`
	}
	if err := json.Unmarshal([]byte(stdout), &board); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if len(board.Agents) != 2 || board.Agents[0].Name != "alpha" || board.Agents[0].UpdatedAt.IsZero() || board.Agents[1].State != "blocked" {
		t.Fatalf("agents = %+v", board.Agents)
	}

	_, stderr, code = runCLI("status", "clear", "--agent", "alpha")
	if code != 0 {
		t.Fatalf("clear code = %d\n%s", code, stderr)
	}
	stdout, stderr, code = runCLI("status", "list", "--json")
	if code != 0 {
		t.Fatalf("list after clear code = %d\n%s", code, stderr)
	}
	if err := json.Unmarshal([]byte(stdout), &board); err != nil {
		t.Fatal(err)
	}
	if len(board.Agents) != 1 || board.Agents[0].Name != "beta" {
		t.Fatalf("after clear = %+v", board.Agents)
	}
	if _, _, code = runCLI("status", "clear", "--agent", "alpha"); code == 0 {
		t.Fatal("clearing a missing agent succeeded")
	}
}

func TestStatusResourceFreeBusyAndClear(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	stdout, stderr, code := runCLI("status", "set", "--resource", "workspace", "free", "paused")
	if code != 0 {
		t.Fatalf("set code = %d\n%s", code, stderr)
	}
	if stdout != "workspace: free (paused)\n" {
		t.Fatalf("set out = %q", stdout)
	}
	if _, stderr, code = runCLI("status", "set", "--resource", "workspace", "maybe"); code == 0 {
		t.Fatalf("invalid availability accepted\n%s", stderr)
	}

	stdout, stderr, code = runCLI("status", "list")
	if code != 0 {
		t.Fatalf("list code = %d\n%s", code, stderr)
	}
	if !strings.Contains(stdout, "workspace: free (paused)") {
		t.Fatalf("list = %q", stdout)
	}

	stdout, stderr, code = runCLI("status", "list", "--json")
	if code != 0 {
		t.Fatalf("json code = %d\n%s", code, stderr)
	}
	var board struct {
		Resources []decide.ResourceStatus `json:"resources"`
	}
	if err := json.Unmarshal([]byte(stdout), &board); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if len(board.Resources) != 1 || board.Resources[0].Availability != "free" || board.Resources[0].Note != "paused" || board.Resources[0].UpdatedAt.IsZero() {
		t.Fatalf("resources = %+v", board.Resources)
	}

	if _, stderr, code = runCLI("status", "clear", "--resource", "workspace"); code != 0 {
		t.Fatalf("clear code = %d\n%s", code, stderr)
	}
	stdout, _, code = runCLI("status", "list")
	if code != 0 || strings.Contains(stdout, "workspace") {
		t.Fatalf("after clear %d %q", code, stdout)
	}
}

func runCLI(args ...string) (string, string, int) {
	var stdout, stderr bytes.Buffer
	code := run(args, &stdout, &stderr)
	return stdout.String(), stderr.String(), code
}

func TestSkill(t *testing.T) {
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	stdout, stderr, code := runCLI("skill")
	if code != 0 {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	if !strings.HasPrefix(stdout, "---\nname: decide\n") {
		t.Fatalf("stdout does not start with frontmatter:\n%s", stdout)
	}
	if !strings.Contains(stdout, "description:") {
		t.Fatalf("stdout missing description:\n%s", stdout)
	}
	for _, phrase := range []string{
		"ask the user for a decision, send a note, or post status",
		"Run `decide` with no arguments",
		"Every item has options, includes `--rec`, and usually allows `--free-text`.",
		"`--notify-pane` routes the answer to the owning agent's pane.",
		"verify it with `decide show --json`: `state` is `answered` and `answeredBy` is `human`.",
		"Close an item with `decide done` and an outcome.",
		"A follow-up is a reply in the existing thread, not a new item.",
		"A human reply notifies the owner.",
		"`decide reply` posts the agent's progress or result.",
		"A reply with options asks a follow-up question.",
		"A reply without options is a notification the user can archive.",
		`Reply in the same thread when the user picks "draft a message for me"`,
		"full draft text",
		"`send`, `edit`, and `don't send`",
		"The user answers or requests edits inside decide.",
		"`decide status set` posts the agent state, and posts a shared resource as free or busy.",
		"Deadlines are RFC3339.",
		"An inbox answer may not count as approval for an outward action, such as posting to chat, writing to a tracker, or merging, when that agent's own check requires a typed approval.",
		"type this in tab X",
		"include the exact text.",
	} {
		if !strings.Contains(stdout, phrase) {
			t.Fatalf("skill missing %q\n%s", phrase, stdout)
		}
	}

	dir := filepath.Join(cfg, "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.json"), []byte("{"), 0o600); err != nil {
		t.Fatal(err)
	}
	again, againErr, againCode := runCLI("skill")
	if againCode != 0 || again != stdout || againErr != "" {
		t.Fatalf("config changed skill: exit %d\nstderr=%s\nstdout=%s", againCode, againErr, again)
	}

	extraOut, extraErr, extraCode := runCLI("skill", "extra")
	if extraCode != 2 {
		t.Fatalf("extra exit = %d, want 2, stderr = %s", extraCode, extraErr)
	}
	if strings.Contains(extraOut, "name: decide") || strings.Contains(extraErr, "name: decide") {
		t.Fatalf("skill was printed\nstdout=%s\nstderr=%s", extraOut, extraErr)
	}

	usageOut, usageErr, usageCode := runCLI()
	if usageCode != 2 {
		t.Fatalf("usage exit = %d, stderr = %s", usageCode, usageErr)
	}
	if !strings.Contains(usageErr, "\n  decide skill\n") {
		t.Fatalf("usage does not list skill:\n%s%s", usageOut, usageErr)
	}
}
