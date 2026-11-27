package config

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"
)

func TestPolicyConfigDefaultsAndLoad(t *testing.T) {
	t.Parallel()
	def := Defaults()
	if def.Policy.CatchUp != PolicyOff || def.Policy.Rebase != PolicyOff ||
		def.Policy.NitReply != PolicyOff || def.Policy.CIFix != PolicyOff ||
		def.Policy.QueueFix != PolicyOff || def.Policy.BotRereview != PolicyOff {
		t.Fatalf("default policy modes should all be off: %+v", def.Policy)
	}
	if !def.Policy.RequireIdle || def.Policy.MaxPerRun != 1 || !def.Policy.SkipDrafts || !def.Policy.SkipStacked || !def.Policy.SkipOtherMachine {
		t.Fatalf("unexpected default guards: %+v", def.Policy)
	}
	if def.Policy.AgentBackend != BackendHerdr || def.Policy.LeaseProvider != LeaseOff {
		t.Fatalf("unexpected default backend/lease: %+v", def.Policy)
	}

	dir := t.TempDir()
	cfgPath := filepath.Join(dir, "prsync.config")
	body := `
policy_catch_up=dry-run
policy_rebase=auto
policy_nit_reply=auto
policy_ci_fix=dry-run
policy_queue_fix=auto
bot_rereview=auto
bot_rereview_login=coderabbitai[bot]
bot_rereview_body=@coderabbitai review
policy_dispatch_conflicting=true
guard_require_idle=false
guard_max_per_run=3
guard_daily_cap=10
guard_skip_drafts=false
guard_hold_labels=hold, do-not-merge
guard_skip_stacked=false
guard_default_branches=main, develop
guard_skip_other_machine=false
guard_command=test -f /tmp/ok
nit_reply_bots=coderabbitai, gemini-code-assist
nit_reply_patterns=(?i)nit:, (?i)non-blocking
dispatch_ledger_file=/tmp/ledger.jsonl
agent_backend=command
agent_backend_cmd=/usr/local/bin/my-mux
lease_provider=file
lease_name=my-repo
lease_timeout=20m
lease_dir=/tmp/leases
`
	if err := os.WriteFile(cfgPath, []byte(body), 0o600); err != nil {
		t.Fatalf("write config: %v", err)
	}
	cfg, err := Load(cfgPath)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	p := cfg.Policy
	if p.CatchUp != PolicyDryRun || p.Rebase != PolicyAuto || p.NitReply != PolicyAuto || p.CIFix != PolicyDryRun || p.QueueFix != PolicyAuto || p.BotRereview != PolicyAuto {
		t.Fatalf("modes = %+v", p)
	}
	if p.BotRereviewLogin != "coderabbitai[bot]" || p.BotRereviewBody != "@coderabbitai review" {
		t.Fatalf("bot_rereview = %+v", p)
	}
	if !p.DispatchConflicting || p.RequireIdle || p.MaxPerRun != 3 || p.DailyCap != 10 || p.SkipDrafts || p.SkipStacked || p.SkipOtherMachine {
		t.Fatalf("guards = %+v", p)
	}
	if !slices.Equal(p.HoldLabels, []string{"hold", "do-not-merge"}) {
		t.Fatalf("HoldLabels = %v", p.HoldLabels)
	}
	if !slices.Equal(p.DefaultBranches, []string{"main", "develop"}) {
		t.Fatalf("DefaultBranches = %v", p.DefaultBranches)
	}
	if !slices.Equal(p.NitReplyBots, []string{"coderabbitai", "gemini-code-assist"}) {
		t.Fatalf("NitReplyBots = %v", p.NitReplyBots)
	}
	if len(p.NitReplyPatterns) != 2 {
		t.Fatalf("NitReplyPatterns len = %d", len(p.NitReplyPatterns))
	}
	if p.GuardCommand != "test -f /tmp/ok" || p.LedgerFile != "/tmp/ledger.jsonl" {
		t.Fatalf("guard/ledger = %+v", p)
	}
	if p.AgentBackend != BackendCommand || p.AgentBackendCmd != "/usr/local/bin/my-mux" {
		t.Fatalf("backend = %+v", p)
	}
	if p.LeaseProvider != LeaseFile || p.LeaseName != "my-repo" || p.LeaseTimeout != 20*time.Minute || p.LeaseDir != "/tmp/leases" {
		t.Fatalf("lease = %+v", p)
	}
}

func TestPolicyConfigValidationErrors(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		body string
	}{
		{name: "invalid policy mode", body: "policy_rebase=always\n"},
		{name: "invalid agent backend", body: "agent_backend=tmux\n"},
		{name: "command backend without cmd", body: "agent_backend=command\n"},
		{name: "invalid lease provider", body: "lease_provider=redis\n"},
		{name: "command lease without claim cmd", body: "lease_provider=command\n"},
		{name: "invalid lease timeout", body: "lease_timeout=0s\n"},
		{name: "invalid nit regex", body: "nit_reply_patterns=[invalid\n"},
		{name: "invalid bot_rereview mode", body: "bot_rereview=always\n"},
		{name: "bot_rereview dry-run missing login", body: "bot_rereview=dry-run\nbot_rereview_body=@coderabbitai review\n"},
		{name: "bot_rereview dry-run missing body", body: "bot_rereview=dry-run\nbot_rereview_login=coderabbitai\n"},
		{name: "bot_rereview auto missing login", body: "bot_rereview=auto\nbot_rereview_body=@coderabbitai review\n"},
		{name: "bot_rereview auto missing body", body: "bot_rereview=auto\nbot_rereview_login=coderabbitai\n"},
		{name: "policy_bot_rereview auto missing login", body: "policy_bot_rereview=auto\nbot_rereview_body=@coderabbitai review\n"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfgPath := filepath.Join(t.TempDir(), "prsync.config")
			if err := os.WriteFile(cfgPath, []byte(tc.body), 0o600); err != nil {
				t.Fatalf("write config: %v", err)
			}
			if _, err := Load(cfgPath); err == nil {
				t.Fatalf("Load() succeeded, want error for %s", tc.name)
			}
		})
	}
}
