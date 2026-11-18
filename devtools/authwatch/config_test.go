package authwatch

import (
	"testing"
	"time"
)

func TestParseConfigDefaults(t *testing.T) {
	cfg, err := ParseConfig([]byte(""))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.WarnWithin != 72*time.Hour {
		t.Errorf("WarnWithin = %v", cfg.WarnWithin)
	}
	if cfg.CriticalWithin != 24*time.Hour {
		t.Errorf("CriticalWithin = %v", cfg.CriticalWithin)
	}
	if cfg.FailingAfter != 10*time.Minute {
		t.Errorf("FailingAfter = %v", cfg.FailingAfter)
	}
	if len(cfg.Profiles) != 1 || cfg.Profiles[0].Name != "default" {
		t.Errorf("Profiles = %+v", cfg.Profiles)
	}
}

func TestParseConfigCustomAndValidation(t *testing.T) {
	raw := []byte(`
warn_within = "48h"
critical_within = "12h"
failing_after = "5m"
use_cli_status = true
cli_command = "my-claude"
notify = ["notify-send", "{profile}", "{level}"]

[[profiles]]
name = "work"
config_dir = "~/.claude-work"

[[profiles]]
name = "personal"
config_dir = "~/.claude-personal"
`)
	cfg, err := ParseConfig(raw)
	if err != nil {
		t.Fatal(err)
	}
	if cfg.WarnWithin != 48*time.Hour || cfg.CriticalWithin != 12*time.Hour || cfg.FailingAfter != 5*time.Minute {
		t.Errorf("unexpected thresholds: %+v", cfg)
	}
	if !cfg.UseCLIStatus || cfg.CLICommand != "my-claude" {
		t.Errorf("unexpected cli status settings: %+v", cfg)
	}
	if len(cfg.Profiles) != 2 {
		t.Fatalf("Profiles len = %d", len(cfg.Profiles))
	}

	badConfigs := []string{
		`warn_within = "10h"
critical_within = "20h"`,
		`notify = ["echo", ""]`,
		`[[profiles]]
name = ""`,
		`[[profiles]]
name = "dup"
[[profiles]]
name = "dup"`,
	}
	for i, bad := range badConfigs {
		if _, err := ParseConfig([]byte(bad)); err == nil {
			t.Errorf("case %d: expected error for bad config", i)
		}
	}
}
