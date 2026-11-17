package ghoutbox

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestParseConfig(t *testing.T) {
	cfg, err := ParseConfig([]byte(`
data_dir = "drafts"
notify_pane = "w1:p2"
decide_command = ["ssh", "host", "decide"]
poll_interval = "1m"

[lint]
defaults = false
agentbus_names = false
agent_pattern = '\b(?:pm|ops)/[a-z-]+\b'
patterns = ['TICKET-\d+']
phrases = ["secret plan"]
`))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.DataDir != "drafts" || cfg.NotifyPane != "w1:p2" || strings.Join(cfg.DecideCommand, " ") != "ssh host decide" ||
		cfg.PollInterval != time.Minute || cfg.GHCommand[0] != "gh" || cfg.AgentbusCommand[0] != "agentbus" {
		t.Errorf("cfg = %+v", cfg)
	}
	if cfg.Lint.Defaults || cfg.Lint.AgentbusNames || len(cfg.Lint.Patterns) != 1 || len(cfg.Lint.Phrases) != 1 || cfg.Lint.AgentPattern == "" {
		t.Errorf("lint = %+v", cfg.Lint)
	}
}

func TestParseConfigErrors(t *testing.T) {
	for _, in := range []string{
		`unknown = 1`,
		`gh_command = []`,
		`poll_interval = "soon"`,
		"[lint]\npatterns = ['(']",
	} {
		if _, err := ParseConfig([]byte(in)); err == nil {
			t.Errorf("%q: want an error", in)
		}
	}
}

func TestLoadConfigDefaults(t *testing.T) {
	dir := t.TempDir()
	env := []string{"HOME=" + dir, "XDG_STATE_HOME=" + filepath.Join(dir, "state")}
	cfg, err := LoadConfig(filepath.Join(dir, "missing.toml"), env)
	if err != nil {
		t.Fatal(err)
	}
	if cfg.DataDir != filepath.Join(dir, "state", "gh-outbox") || !cfg.Lint.Defaults || !cfg.Lint.AgentbusNames {
		t.Errorf("cfg = %+v", cfg)
	}
	path := filepath.Join(dir, "config.toml")
	if err := os.WriteFile(path, []byte(`data_dir = "d"`), 0o600); err != nil {
		t.Fatal(err)
	}
	if cfg, err = LoadConfig(path, env); err != nil || cfg.DataDir != filepath.Join(dir, "d") {
		t.Errorf("DataDir = %q err = %v", cfg.DataDir, err)
	}
	if p, _ := DefaultConfigPath([]string{"HOME=/h"}); p != "/h/.config/gh-outbox/config.toml" {
		t.Errorf("DefaultConfigPath = %q", p)
	}
	if p, _ := DefaultDataDir([]string{"HOME=/h"}); p != "/h/.local/state/gh-outbox" {
		t.Errorf("DefaultDataDir = %q", p)
	}
}
