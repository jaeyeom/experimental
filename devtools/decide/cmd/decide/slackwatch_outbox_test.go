package main

import (
	"context"
	"encoding/json"
	"errors"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

// TestSlackwatchAskKeepsOptions runs the real decide CLI the way slackwatch
// asks, then a second sender's plain reply. Options stay. An older decide
// that cleared them is covered by editing the ledger and letting slackwatch
// re-attach through decide reply.
func TestSlackwatchAskKeepsOptions(t *testing.T) {
	state := t.TempDir()
	cfgDir := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", cfgDir)
	t.Setenv("HERDR_PANE_ID", "w1:p2")
	t.Setenv("DECIDE_AS_HANDLE", "herdr:laptop/s/w1:p2")
	t.Setenv("DECIDE_TEST_CHILD", "1")

	notes := filepath.Join(t.TempDir(), "notes")
	store, err := slackwatch.OpenStore(filepath.Join(t.TempDir(), "slackwatch.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { store.Close() })
	quoted := "'" + strings.ReplaceAll(notes, "'", `'\''`) + "'"
	swCfg := slackwatch.Config{
		FallbackTarget: "fallback",
		NotifyTemplate: `printf '%s\n' "$SLACKWATCH_MESSAGE" >> ` + quoted,
		Post: slackwatch.PostConfig{
			Mode: slackwatch.PostModeHandoff, Agent: "poster", ExpectUser: "UPOSTER",
		},
	}
	d := slackwatch.DecideCLI{Run: slackwatch.ExecDecide([]string{os.Args[0]}), Owner: slackwatch.DefaultDecideOwner}
	ob := &slackwatch.Outbox{
		Store: store, Config: swCfg, Decide: d,
		Notifier: &slackwatch.Notifier{Store: store, Config: swCfg, Logger: slog.New(slog.DiscardHandler)},
		Now:      time.Now, Logger: slog.New(slog.DiscardHandler),
	}
	rec, err := ob.Submit(context.Background(), slackwatch.Draft{
		Channel: "C1", Text: "Ship the note.", Owner: "agent-1", Purpose: "announce",
	})
	if err != nil {
		t.Fatal(err)
	}
	item := loadDecide(t, state, rec.DecideID)
	if !item.OwnerRepliesOnly {
		t.Fatal("ask did not set ownerRepliesOnly")
	}
	if item.NotifyPane != "" {
		t.Fatalf("notifyPane = %q, want empty", item.NotifyPane)
	}
	if !optionKeys(item.Options, "post", "drop") {
		t.Fatalf("options = %+v", item.Options)
	}

	stdout, stderr, code := runDecideChild(t, rec.DecideID, "other-agent")
	if code == 0 {
		t.Fatalf("second sender was accepted\n%s\n%s", stdout, stderr)
	}
	if !strings.Contains(stderr, "accepts replies only from its owner "+slackwatch.DefaultDecideOwner) {
		t.Fatalf("stderr = %q", stderr)
	}
	if got := loadDecide(t, state, rec.DecideID); !optionKeys(got.Options, "post", "drop") || len(got.Messages) != 1 {
		t.Fatalf("second sender changed the item: options %+v messages %d", got.Options, len(got.Messages))
	}

	clearDecideOptions(t, state, rec.DecideID)
	if err := ob.Poll(context.Background()); err != nil {
		t.Fatal(err)
	}
	if err := ob.Poll(context.Background()); err != nil {
		t.Fatal(err)
	}
	got := loadDecide(t, state, rec.DecideID)
	if !optionKeys(got.Options, "post", "drop") {
		t.Fatalf("options after re-attach = %+v", got.Options)
	}
	if len(got.Messages) != 2 {
		t.Fatalf("messages = %d, want the ask and one re-attach", len(got.Messages))
	}
	last := got.Messages[len(got.Messages)-1]
	if last.By != slackwatch.DefaultDecideOwner {
		t.Fatalf("re-attach by = %q, want the owner", last.By)
	}
	notice, err := os.ReadFile(notes)
	if err != nil {
		t.Fatal(err)
	}
	want := "outbox " + strconv.FormatInt(rec.ID, 10) + ": decide item lost its options; re-attached\n"
	if string(notice) != want {
		t.Fatalf("notice = %q, want %q", notice, want)
	}
}

func loadDecide(t *testing.T, state, id string) decide.Item {
	t.Helper()
	led, err := decide.Open(filepath.Join(state, "decide"))
	if err != nil {
		t.Fatal(err)
	}
	item, err := led.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	return item
}

func clearDecideOptions(t *testing.T, state, id string) {
	t.Helper()
	path := filepath.Join(state, "decide", "ledger.json")
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	var doc struct {
		Next  int           `json:"next"`
		Items []decide.Item `json:"items"`
	}
	if err := json.Unmarshal(data, &doc); err != nil {
		t.Fatal(err)
	}
	var found bool
	for i := range doc.Items {
		if doc.Items[i].ID == id {
			doc.Items[i].Options = nil
			found = true
		}
	}
	if !found {
		t.Fatalf("ledger has no %s", id)
	}
	out, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, append(out, '\n'), 0o600); err != nil {
		t.Fatal(err)
	}
}

// runDecideChild runs decide reply as a sender other than the owner.
func runDecideChild(t *testing.T, id, sender string) (string, string, int) {
	t.Helper()
	dir := t.TempDir()
	script := filepath.Join(dir, "agentbus")
	body := "#!/bin/sh\nprintf '%s\\nwhen: other\\n' " + sender + "\n"
	if err := os.WriteFile(script, []byte(body), 0o700); err != nil { //nolint:gosec // G306: the fake agentbus must be executable.
		t.Fatal(err)
	}
	cmd := exec.Command(os.Args[0], "reply", id, "plain reply from someone else") //nolint:gosec // G204: the test re-execs itself; no shell.
	cmd.Env = append(withoutEnv("HERDR_PANE_ID", "DECIDE_AS_HANDLE", "SSH_CONNECTION", "DECIDE_BUS_NAMES", "DECIDE_TAB_LABEL", "PATH"),
		"HERDR_PANE_ID=w9:p9",
		"PATH="+dir,
	)
	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()
	code := 0
	if err != nil {
		var exit *exec.ExitError
		if !errors.As(err, &exit) {
			t.Fatal(err)
		}
		code = exit.ExitCode()
	}
	return stdout.String(), stderr.String(), code
}

// withoutEnv is the process environment with keys removed. A later value
// does not override an earlier one, so a child that must drop a variable
// has to omit it.
func withoutEnv(keys ...string) []string {
	drop := map[string]bool{}
	for _, k := range keys {
		drop[k] = true
	}
	var env []string
	for _, kv := range os.Environ() {
		key, _, _ := strings.Cut(kv, "=")
		if drop[key] {
			continue
		}
		env = append(env, kv)
	}
	return env
}
