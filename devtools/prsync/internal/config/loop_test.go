package config

import (
	"errors"
	"path/filepath"
	"slices"
	"testing"
	"time"
)

func TestLoopDefaults(t *testing.T) {
	t.Parallel()

	cfg := Defaults()
	if len(cfg.LoopPreRoundCmd) != 0 {
		t.Fatalf("LoopPreRoundCmd = %v, want empty", cfg.LoopPreRoundCmd)
	}
	if cfg.LoopPreRoundTimeout != 30*time.Second {
		t.Fatalf("LoopPreRoundTimeout = %s, want 30s", cfg.LoopPreRoundTimeout)
	}
	if cfg.LoopGatePause != 5*time.Minute {
		t.Fatalf("LoopGatePause = %s, want 5m", cfg.LoopGatePause)
	}
	if cfg.LoopGatePauseMax != time.Hour {
		t.Fatalf("LoopGatePauseMax = %s, want 1h", cfg.LoopGatePauseMax)
	}
	if cfg.LoopWakeAgentbusName != "" {
		t.Fatalf("LoopWakeAgentbusName = %q, want empty", cfg.LoopWakeAgentbusName)
	}
	if cfg.AgentbusBin != "agentbus" {
		t.Fatalf("AgentbusBin = %q, want agentbus", cfg.AgentbusBin)
	}
}

func TestLoadLoopGateKeys(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)

	path := filepath.Join(t.TempDir(), "prsync.config")
	mustWrite(t, path,
		`loop_pre_round_cmd="~/bin/check-quota --min 10 --label 'model quota'"`+"\n"+
			"loop_pre_round_timeout=45s\n"+
			"loop_gate_pause=2m\n"+
			"loop_gate_pause_max=20m\n"+
			"loop_wake_agentbus_name=prsync-wake\n"+
			"agentbus_bin=~/bin/agentbus\n",
	)
	got, err := Load(path)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	wantCmd := []string{
		filepath.Join(home, "bin/check-quota"),
		"--min",
		"10",
		"--label",
		"model quota",
	}
	if !slices.Equal(got.LoopPreRoundCmd, wantCmd) {
		t.Fatalf("LoopPreRoundCmd = %q, want %q", got.LoopPreRoundCmd, wantCmd)
	}
	if got.LoopPreRoundTimeout != 45*time.Second {
		t.Fatalf("LoopPreRoundTimeout = %s, want 45s", got.LoopPreRoundTimeout)
	}
	if got.LoopGatePause != 2*time.Minute {
		t.Fatalf("LoopGatePause = %s, want 2m", got.LoopGatePause)
	}
	if got.LoopGatePauseMax != 20*time.Minute {
		t.Fatalf("LoopGatePauseMax = %s, want 20m", got.LoopGatePauseMax)
	}
	if got.LoopWakeAgentbusName != "prsync-wake" {
		t.Fatalf("LoopWakeAgentbusName = %q, want prsync-wake", got.LoopWakeAgentbusName)
	}
	if wantBin := filepath.Join(home, "bin/agentbus"); got.AgentbusBin != wantBin {
		t.Fatalf("AgentbusBin = %q, want %q", got.AgentbusBin, wantBin)
	}
}

func TestLoadLoopGateRejectsBadValues(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct{ body, key string }{
		{"loop_pre_round_cmd=/bin/check \"unclosed\n", "loop_pre_round_cmd"},
		{"loop_pre_round_cmd=/bin/check 'unclosed\n", "loop_pre_round_cmd"},
		{"loop_pre_round_timeout=soon\n", "loop_pre_round_timeout"},
		{"loop_pre_round_timeout=0s\n", "loop_pre_round_timeout"},
		{"loop_pre_round_timeout=-5s\n", "loop_pre_round_timeout"},
		{"loop_gate_pause=soon\n", "loop_gate_pause"},
		{"loop_gate_pause=0s\n", "loop_gate_pause"},
		{"loop_gate_pause=-1m\n", "loop_gate_pause"},
		{"loop_gate_pause_max=soon\n", "loop_gate_pause_max"},
		{"loop_gate_pause_max=0s\n", "loop_gate_pause_max"},
		{"loop_gate_pause_max=-1m\n", "loop_gate_pause_max"},
		{"agentbus_bin=\n", "agentbus_bin"},
	} {
		path := filepath.Join(t.TempDir(), "prsync.config")
		mustWrite(t, path, tc.body)
		var keyErr *KeyError
		if _, err := Load(path); !errors.As(err, &keyErr) || keyErr.Key != tc.key {
			t.Errorf("Load(%q) error = %v, want %s key error", tc.body, err, tc.key)
		}
	}
}
