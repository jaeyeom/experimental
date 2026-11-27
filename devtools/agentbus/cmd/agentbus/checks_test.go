package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lastturn"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

type syncClock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *syncClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *syncClock) Advance(d time.Duration) {
	c.mu.Lock()
	c.now = c.now.Add(d)
	c.mu.Unlock()
}

func checkWorld(t *testing.T, ad *fake.Adapter, clk *syncClock, configTOML string) func(string) []string {
	t.Helper()
	xdg := t.TempDir()
	if configTOML != "" {
		cfgDir := filepath.Join(xdg, "agentbus")
		if err := os.MkdirAll(cfgDir, 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(cfgDir, "config.toml"), []byte(configTOML), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"XDG_CONFIG_HOME=" + xdg,
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	cfg := daemon.Config{
		Adapters:     []adapter.Adapter{ad},
		PollInterval: time.Hour,
	}
	if clk != nil {
		cfg.Now = clk.Now
	}
	stop := startDaemon(t, dirs, cfg)
	t.Cleanup(stop)
	return func(pane string) []string {
		out := append([]string{}, base...)
		if pane != "" {
			out = append(out, "HERDR_PANE_ID="+pane)
		}
		return out
	}
}

func TestHeldCommandFiltersAndExitCodes(t *testing.T) {
	t.Parallel()
	clk := &syncClock{now: time.Date(2026, 11, 27, 4, 0, 0, 0, time.UTC)}
	const (
		sender   = "herdr:local/s1/w1:p1"
		receiver = "herdr:local/s1/w1:p2"
		other    = "herdr:local/s1/w1:p3"
	)
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			sender:   adapter.StateIdle,
			receiver: adapter.StateIdle,
			other:    adapter.StateBlocked,
		},
		Result: map[string]adapter.Result{
			sender:   adapter.ResultAccepted,
			receiver: adapter.ResultAccepted,
		},
		PaneHandles: map[string]string{"p1": sender, "p2": receiver, "p3": other},
	}
	env := checkWorld(t, ad, clk, "")
	mustCmd(t, []string{"register", "ops/worker", "--when", "sending"}, env("p1"))
	mustCmd(t, []string{"register", "ops/inbox", "--when", "inbox"}, env("p2"))
	mustCmd(t, []string{"register", "ops/blocked", "--when", "blocked"}, env("p3"))

	// Empty initially -> exit 0, JSON [].
	code, stdout, stderr := runCmd([]string{"held", "--json"}, env(""))
	if code != 0 || strings.TrimSpace(stdout) != "[]" || stderr != "" {
		t.Fatalf("empty held --json = %d, %q, %q", code, stdout, stderr)
	}
	code, stdout, stderr = runCmd([]string{"held"}, env(""))
	if code != 0 || stdout != "" || stderr != "" {
		t.Fatalf("empty held = %d, %q, %q", code, stdout, stderr)
	}

	// Put a draft hold on ops/inbox -> first message gets "user draft in composer",
	// second message gets "waiting for an earlier message".
	ad.ComposerText = "user typing"
	out1, _ := mustCmd(t, []string{"send", "ops/inbox", "first draft-held"}, env("p1"))
	mustCmd(t, []string{"status", firstID(t, out1)}, env(""))
	clk.Advance(5 * time.Minute)
	out2, _ := mustCmd(t, []string{"send", "ops/inbox", "second ordering-held"}, env("p1"))
	mustCmd(t, []string{"status", firstID(t, out2)}, env(""))
	clk.Advance(10 * time.Minute)
	out3, _ := mustCmd(t, []string{"send", "ops/blocked", "blocked target"}, env("p1"))
	mustCmd(t, []string{"status", firstID(t, out3)}, env(""))
	clk.Advance(2 * time.Minute)

	// All 3 held -> exit 1.
	code, stdout, stderr = runCmd([]string{"held", "--json"}, env(""))
	if code != 1 || stderr != "" {
		t.Fatalf("held --json = %d, stderr %q", code, stderr)
	}
	var all []mailbox.HeldMessage
	if err := json.Unmarshal([]byte(stdout), &all); err != nil {
		t.Fatalf("decode held --json: %v\n%s", err, stdout)
	}
	if len(all) != 3 {
		t.Fatalf("len(all) = %d, want 3: %+v", len(all), all)
	}
	if all[0].ReasonCode != "draft" || all[1].ReasonCode != "ordering" || all[2].ReasonCode != "blocked" {
		t.Fatalf("reason codes = %+v", all)
	}

	// --not-reason draft excludes the draft-held message.
	code, stdout, _ = runCmd([]string{"held", "--not-reason", "draft", "--json"}, env(""))
	if code != 1 {
		t.Fatalf("held --not-reason draft = %d", code)
	}
	var noDraft []mailbox.HeldMessage
	if err := json.Unmarshal([]byte(stdout), &noDraft); err != nil {
		t.Fatal(err)
	}
	if len(noDraft) != 2 || noDraft[0].ReasonCode != "ordering" || noDraft[1].ReasonCode != "blocked" {
		t.Fatalf("noDraft = %+v", noDraft)
	}

	// --older-than 15m only matches the 17m-old draft hold.
	code, stdout, _ = runCmd([]string{"held", "--older-than", "15m", "--json"}, env(""))
	if code != 1 {
		t.Fatalf("held --older-than 15m = %d", code)
	}
	var older []mailbox.HeldMessage
	if err := json.Unmarshal([]byte(stdout), &older); err != nil {
		t.Fatal(err)
	}
	if len(older) != 1 || older[0].ReasonCode != "draft" {
		t.Fatalf("older = %+v", older)
	}

	// --older-than 15m --not-reason draft matches nothing -> exit 0!
	code, stdout, _ = runCmd([]string{"held", "--older-than", "15m", "--not-reason", "draft", "--json"}, env(""))
	if code != 0 || strings.TrimSpace(stdout) != "[]" {
		t.Fatalf("older + not-reason = %d, %q", code, stdout)
	}

	// --reason and --to filters.
	code, stdout, _ = runCmd([]string{"held", "--reason", "ordering", "--to", "ops/inbox"}, env(""))
	if code != 1 || !strings.Contains(stdout, "ops/inbox") || !strings.Contains(stdout, "[ordering]") {
		t.Fatalf("text held = %d, %q", code, stdout)
	}

	// Usage error -> exit 2.
	if code, _, _ := runCmd([]string{"held", "--older-than", "bogus"}, env("")); code != 2 {
		t.Fatalf("bad --older-than = %d, want 2", code)
	}

	// Bus down -> exit 2 without starting daemon.
	downEnv := []string{"XDG_RUNTIME_DIR=" + shortRuntime(t), "XDG_STATE_HOME=" + t.TempDir(), "HOME=" + t.TempDir()}
	if code, _, _ := runCmd([]string{"held"}, downEnv); code != 2 {
		t.Fatalf("down bus held = %d, want 2", code)
	}
}

func TestAsksCommandRulesReplyClearingAndAge(t *testing.T) {
	t.Parallel()
	clk := &syncClock{now: time.Date(2026, 11, 27, 4, 0, 0, 0, time.UTC)}
	const (
		sender   = "herdr:local/s1/w1:p1"
		receiver = "herdr:local/s1/w1:p2"
	)
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sender: adapter.StateIdle, receiver: adapter.StateIdle},
		Result:      map[string]adapter.Result{sender: adapter.ResultAccepted, receiver: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sender, "p2": receiver},
	}
	env := checkWorld(t, ad, clk, "")
	mustCmd(t, []string{"register", "ops/worker", "--when", "sending"}, env("p1"))
	mustCmd(t, []string{"register", "ops/inbox", "--when", "inbox"}, env("p2"))

	// FYI message (no --expect-reply, no ?) -> exit 0.
	mustCmd(t, []string{"send", "--wait", "ops/inbox", "FYI build is green."}, env("p1"))
	code, stdout, _ := runCmd([]string{"asks", "--json"}, env(""))
	if code != 0 || strings.TrimSpace(stdout) != "[]" {
		t.Fatalf("asks after FYI = %d, %q", code, stdout)
	}

	// Send explicit ask and question-mark ask at different ages.
	explicitOut, _ := mustCmd(t, []string{"send", "--wait", "--expect-reply", "ops/inbox", "Confirm you saw this\nLine two"}, env("p1"))
	explicitID := firstID(t, explicitOut)
	clk.Advance(10 * time.Minute)
	questionOut, _ := mustCmd(t, []string{"send", "--wait", "ops/inbox", "Is the queue clear?"}, env("p1"))
	questionID := firstID(t, questionOut)
	clk.Advance(2 * time.Minute)

	// Both asks present -> exit 1.
	code, stdout, _ = runCmd([]string{"asks", "--json"}, env(""))
	if code != 1 {
		t.Fatalf("asks = %d, %q", code, stdout)
	}
	var asks []mailbox.Ask
	if err := json.Unmarshal([]byte(stdout), &asks); err != nil {
		t.Fatal(err)
	}
	if len(asks) != 2 {
		t.Fatalf("asks = %+v, want 2", asks)
	}
	if asks[0].ID != explicitID || asks[0].Rule != "explicit" || asks[0].FirstLine != "Confirm you saw this" {
		t.Fatalf("asks[0] = %+v", asks[0])
	}
	if asks[1].ID != questionID || asks[1].Rule != "question-mark" {
		t.Fatalf("asks[1] = %+v", asks[1])
	}

	// --unanswered 5m excludes the 2m-old question and keeps the 12m-old explicit ask.
	code, stdout, _ = runCmd([]string{"asks", "--unanswered", "5m", "--from", "ops/worker", "--to", "ops/inbox", "--json"}, env(""))
	if code != 1 {
		t.Fatalf("asks --unanswered 5m = %d", code)
	}
	var older []mailbox.Ask
	if err := json.Unmarshal([]byte(stdout), &older); err != nil {
		t.Fatal(err)
	}
	if len(older) != 1 || older[0].ID != explicitID {
		t.Fatalf("older = %+v", older)
	}

	// Reply clears each ask.
	mustCmd(t, []string{"reply", explicitID, "confirmed"}, env("p2"))
	code, stdout, _ = runCmd([]string{"asks", "--unanswered", "5m"}, env(""))
	if code != 0 || stdout != "" {
		t.Fatalf("asks after reply to explicit = %d, %q", code, stdout)
	}
	mustCmd(t, []string{"reply", questionID, "yes"}, env("p2"))
	code, stdout, _ = runCmd([]string{"asks", "--json"}, env(""))
	if code != 0 || strings.TrimSpace(stdout) != "[]" {
		t.Fatalf("asks after all replied = %d, %q", code, stdout)
	}

	// Bus down -> exit 2.
	downEnv := []string{"XDG_RUNTIME_DIR=" + shortRuntime(t), "XDG_STATE_HOME=" + t.TempDir(), "HOME=" + t.TempDir()}
	if code, _, _ := runCmd([]string{"asks"}, downEnv); code != 2 {
		t.Fatalf("down bus asks = %d, want 2", code)
	}
}

func TestDialogsCommandBuiltinConfiguredIdleAndErrors(t *testing.T) {
	t.Parallel()
	const (
		pPerm   = "herdr:local/s1/w1:p1"
		pIdle   = "herdr:local/s1/w1:p2"
		pCustom = "herdr:m2/s1/w2:p1"
		rule    = "──────────────────────────────"
	)
	ad := &fake.Adapter{
		AdapterName: "herdr",
		InventoryAgents: []adapter.Agent{
			{Handle: pPerm, PaneID: "w1:p1", Kind: "claude", Status: "blocked"},
			{Handle: pIdle, PaneID: "w1:p2", Kind: "claude", Status: "idle"},
			{Handle: pCustom, PaneID: "w2:p1", Kind: "codex", Status: "idle"},
		},
		PaneHandles: map[string]string{"w1:p1": pPerm, "w1:p2": pIdle, "w2:p1": pCustom},
		Reach:       map[string]adapter.State{pPerm: adapter.StateBlocked, pIdle: adapter.StateIdle, pCustom: adapter.StateIdle},
		PaneScreens: map[string]string{
			pPerm:   "⏺ Bash(rm -rf build)\nDo you want to proceed?\n❯ 1. Yes\n",
			pIdle:   "Earlier question: Do you want to proceed? Approve [y/N]?\n" + rule + "\n❯ \n" + rule + "\n",
			pCustom: "Waiting for confirmation\nApprove [y/N]?\n",
		},
	}
	env := checkWorld(t, ad, nil, "dialog_rules = {\"confirm\" = 'Approve \\[y/N\\]'}\n")
	mustCmd(t, []string{"register", "ops/perm", "--when", "perm"}, env("w1:p1"))

	code, stdout, stderr := runCmd([]string{"dialogs", "--json"}, env(""))
	if code != 1 || stderr != "" {
		t.Fatalf("dialogs --json = %d, stderr %q", code, stderr)
	}
	var entries []daemon.DialogEntry
	if err := json.Unmarshal([]byte(stdout), &entries); err != nil {
		t.Fatal(err)
	}
	if len(entries) != 2 {
		t.Fatalf("entries = %+v, want 2 (idle screen must not false-positive)", entries)
	}
	if entries[0].Pane != "w1:p1" || entries[0].Name != "ops/perm" || entries[0].Machine != "local" || entries[0].Rule != "permission prompt" {
		t.Fatalf("entries[0] = %+v", entries[0])
	}
	if entries[1].Pane != "w2:p1" || entries[1].Machine != "m2" || entries[1].Rule != "confirm" || entries[1].Text != "Approve [y/N]?" {
		t.Fatalf("entries[1] = %+v", entries[1])
	}

	// --machine filter
	code, stdout, _ = runCmd([]string{"dialogs", "--machine", "m2"}, env(""))
	if code != 1 || !strings.Contains(stdout, "m2/w2:p1") || strings.Contains(stdout, "w1:p1") {
		t.Fatalf("dialogs --machine m2 = %d, %q", code, stdout)
	}

	// --machine with no dialogs -> exit 0
	code, stdout, _ = runCmd([]string{"dialogs", "--machine", "m3", "--json"}, env(""))
	if code != 0 || strings.TrimSpace(stdout) != "[]" {
		t.Fatalf("dialogs --machine m3 = %d, %q", code, stdout)
	}

	// Clear dialogs -> only idle remains -> exit 0!
	ad.PaneScreens[pPerm] = rule + "\n❯ \n" + rule + "\n"
	ad.PaneScreens[pCustom] = "> \n"
	code, stdout, _ = runCmd([]string{"dialogs", "--json"}, env(""))
	if code != 0 || strings.TrimSpace(stdout) != "[]" {
		t.Fatalf("dialogs idle only = %d, %q", code, stdout)
	}

	// Invalid config regex -> exit 2!
	badCfgDir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(badCfgDir, "agentbus"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(badCfgDir, "agentbus", "config.toml"), []byte(`dialog_rules = ["[unclosed"]`+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	badEnv := []string{"XDG_CONFIG_HOME=" + badCfgDir}
	for _, kv := range env("") {
		if !strings.HasPrefix(kv, "XDG_CONFIG_HOME=") {
			badEnv = append(badEnv, kv)
		}
	}
	if code, _, stderr := runCmd([]string{"dialogs"}, badEnv); code != 2 || !strings.Contains(stderr, "invalid regex") {
		t.Fatalf("bad regex dialogs = %d, stderr %q", code, stderr)
	}

	// Bus down -> exit 2!
	downEnv := []string{"XDG_RUNTIME_DIR=" + shortRuntime(t), "XDG_STATE_HOME=" + t.TempDir(), "HOME=" + t.TempDir()}
	if code, _, _ := runCmd([]string{"dialogs"}, downEnv); code != 2 {
		t.Fatalf("down bus dialogs = %d, want 2", code)
	}
}

func TestCheckCommandsHubUnreachableExitsTwo(t *testing.T) {
	t.Parallel()
	s := newTunnelSetup(t)
	s.ssh.Write("down", "")
	s.ssh.Unreachable()
	for _, cmd := range []string{"held", "asks", "dialogs"} {
		code, _, stderr := s.run(t, s.env, "", cmd)
		if code != 2 || !strings.Contains(stderr, "me@hub") {
			t.Fatalf("%s hub unreachable = %d, stderr %q; want 2", cmd, code, stderr)
		}
	}
}

func TestLoginExpiredHoldShowsDetailInStatusHeldAndLog(t *testing.T) {
	t.Parallel()
	const (
		sender   = "herdr:local/s1/w1:p1"
		receiver = "herdr:local/s1/w1:p2"
	)
	var turnsMu sync.Mutex
	turns := map[string][]lastturn.Turn{}
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sender: adapter.StateIdle, receiver: adapter.StateIdle},
		Result:      map[string]adapter.Result{sender: adapter.ResultAccepted, receiver: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sender, "p2": receiver},
	}
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"XDG_CONFIG_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	stop := startDaemon(t, dirs, daemon.Config{
		Adapters:     []adapter.Adapter{ad},
		PollInterval: time.Hour,
		Transcript: func(ag adapter.Agent) ([]lastturn.Turn, bool) {
			turnsMu.Lock()
			defer turnsMu.Unlock()
			tList, ok := turns[ag.Handle]
			return tList, ok
		},
	})
	t.Cleanup(stop)
	env := func(pane string) []string {
		out := append([]string{}, base...)
		if pane != "" {
			out = append(out, "HERDR_PANE_ID="+pane)
		}
		return out
	}
	mustCmd(t, []string{"register", "ops/sender", "--when", "sending"}, env("p1"))
	mustCmd(t, []string{"register", "ops/target", "--when", "target"}, env("p2"))

	// Seed empty transcript for receiver and send a first message that gets an auth error.
	turnsMu.Lock()
	turns[receiver] = []lastturn.Turn{}
	turnsMu.Unlock()
	out1, _ := mustCmd(t, []string{"send", "--wait", "ops/target", "first"}, env("p1"))
	id1 := firstID(t, out1)

	turnsMu.Lock()
	turns[receiver] = []lastturn.Turn{
		{Role: "user", Text: "[agentbus " + id1 + "]\nfirst", Source: "/tmp/s1.jsonl", Line: 1},
		{Role: "assistant", Text: "Login expired · Please run /login", IsAPIErrorMessage: true, Model: "<synthetic>", Source: "/tmp/s1.jsonl", Line: 2},
	}
	turnsMu.Unlock()

	// Next message triggers verification of id1 (entering outage) and gets held with held_detail.
	out2, _ := mustCmd(t, []string{"send", "ops/target", "second"}, env("p1"))
	id2 := firstID(t, out2)

	statusOut, _ := mustCmd(t, []string{"status", id2}, base)
	if !strings.Contains(statusOut, "state: held: target login expired") || !strings.Contains(statusOut, "held_detail: ") || !strings.Contains(statusOut, "s1.jsonl:2 (turn 2)") {
		t.Fatalf("status output = %q", statusOut)
	}

	logOut, _ := mustCmd(t, []string{"log", "--event", "held"}, base)
	if !strings.Contains(logOut, "held_detail=") || !strings.Contains(logOut, "s1.jsonl:2 (turn 2)") {
		t.Fatalf("log --event held output = %q", logOut)
	}

	code, heldOut, _ := runCmd([]string{"held", "--reason", "login-expired"}, base)
	if code != 1 || !strings.Contains(heldOut, "s1.jsonl:2 (turn 2)") {
		t.Fatalf("held = %d, %q", code, heldOut)
	}
}
