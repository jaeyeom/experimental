package ghask

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

const validConfig = `
listen = "127.0.0.1:8765"
repos = ["octo/demo"]
ask_command = ["asker", "--file", "{file}", "--pr", "{pr}", "--repo", "{repo}"]
id_pattern = 'thread-([0-9a-f]+)'
followup_command = ["follow", "--id", "{id}", "--file", "{file}", "--pr", "{pr}", "--repo", "{repo}"]
status_command = ["status", "{id}"]
`

func TestParseConfigDefaults(t *testing.T) {
	cfg, err := ParseConfig([]byte(validConfig))
	if err != nil {
		t.Fatalf("ParseConfig() error = %v", err)
	}
	if cfg.Listen != "127.0.0.1:8765" {
		t.Errorf("Listen = %q", cfg.Listen)
	}
	if cfg.RateLimit != DefaultRateLimit {
		t.Errorf("RateLimit = %d, want %d", cfg.RateLimit, DefaultRateLimit)
	}
	if cfg.CommandTimeout != DefaultCommandTimeout {
		t.Errorf("CommandTimeout = %v, want %v", cfg.CommandTimeout, DefaultCommandTimeout)
	}
	if got := cfg.IDPattern.String(); got != "thread-([0-9a-f]+)" {
		t.Errorf("IDPattern = %q", got)
	}
	if cfg.Reuse != ReusePerPR {
		t.Errorf("Reuse = %q, want %q", cfg.Reuse, ReusePerPR)
	}
	if len(cfg.FollowupCommand) != 9 || len(cfg.StatusCommand) != 2 {
		t.Errorf("FollowupCommand = %q, StatusCommand = %q", cfg.FollowupCommand, cfg.StatusCommand)
	}
}

func TestParseConfigReuseNone(t *testing.T) {
	data := strings.NewReplacer(
		`followup_command = ["follow", "--id", "{id}", "--file", "{file}", "--pr", "{pr}", "--repo", "{repo}"]`, "",
		`status_command = ["status", "{id}"]`, "",
	).Replace(validConfig) + "reuse = \"none\"\n"
	cfg, err := ParseConfig([]byte(data))
	if err != nil {
		t.Fatalf("ParseConfig() error = %v", err)
	}
	if cfg.Reuse != ReuseNone || cfg.FollowupCommand != nil || cfg.StatusCommand != nil {
		t.Errorf("got reuse %q followup %q status %q", cfg.Reuse, cfg.FollowupCommand, cfg.StatusCommand)
	}
}

func TestParseConfigListenDefault(t *testing.T) {
	data := strings.Replace(validConfig, `listen = "127.0.0.1:8765"`, "", 1)
	cfg, err := ParseConfig([]byte(data))
	if err != nil {
		t.Fatalf("ParseConfig() error = %v", err)
	}
	if cfg.Listen != DefaultListen {
		t.Errorf("Listen = %q, want %q", cfg.Listen, DefaultListen)
	}
}

func TestParseConfigOverrides(t *testing.T) {
	data := validConfig + "rate_limit = 3\ncommand_timeout = \"5m\"\n"
	cfg, err := ParseConfig([]byte(data))
	if err != nil {
		t.Fatalf("ParseConfig() error = %v", err)
	}
	if cfg.RateLimit != 3 || cfg.CommandTimeout != 5*time.Minute {
		t.Errorf("got rate %d timeout %v", cfg.RateLimit, cfg.CommandTimeout)
	}
}

func TestParseConfigErrors(t *testing.T) {
	tests := []struct {
		name    string
		replace [2]string
		extra   string
		wantErr string
	}{
		{name: "non-loopback listen", replace: [2]string{"127.0.0.1:8765", "0.0.0.0:8765"}, wantErr: "127.0.0.1"},
		{name: "localhost listen", replace: [2]string{"127.0.0.1:8765", "localhost:8765"}, wantErr: "127.0.0.1"},
		{name: "no repos", replace: [2]string{`repos = ["octo/demo"]`, ""}, wantErr: "repos"},
		{name: "bad repo", replace: [2]string{`"octo/demo"`, `"octo"`}, wantErr: "repo"},
		{name: "empty command", replace: [2]string{`ask_command = ["asker", "--file", "{file}", "--pr", "{pr}", "--repo", "{repo}"]`, `ask_command = []`}, wantErr: "ask_command"},
		{name: "payload placeholder", replace: [2]string{`"{repo}"]`, `"{question}"]`}, wantErr: "{question}"},
		{name: "no id pattern", replace: [2]string{`id_pattern = 'thread-([0-9a-f]+)'`, ""}, wantErr: "id_pattern"},
		{name: "bad id pattern", replace: [2]string{`'thread-([0-9a-f]+)'`, `'('`}, wantErr: "id_pattern"},
		{name: "id in ask_command", replace: [2]string{`"{repo}"]`, `"{id}"]`}, wantErr: "ask_command: unknown placeholder {id}"},
		{name: "bad reuse", extra: "reuse = \"always\"\n", wantErr: "reuse"},
		{name: "per-pr without followup", replace: [2]string{`followup_command = ["follow", "--id", "{id}", "--file", "{file}", "--pr", "{pr}", "--repo", "{repo}"]`, ""}, wantErr: "followup_command"},
		{name: "followup payload placeholder", replace: [2]string{`"follow", "--id"`, `"follow", "{question}"`}, wantErr: "followup_command: unknown placeholder {question}"},
		{name: "status file placeholder", replace: [2]string{`["status", "{id}"]`, `["status", "{id}", "{file}"]`}, wantErr: "status_command: unknown placeholder {file}"},
		{name: "empty status command", replace: [2]string{`["status", "{id}"]`, `[]`}, wantErr: "status_command"},
		{name: "unknown key", extra: "colour = 1\n", wantErr: "colour"},
		{name: "negative rate", extra: "rate_limit = -1\n", wantErr: "rate_limit"},
		{name: "bad timeout", extra: "command_timeout = \"soon\"\n", wantErr: "command_timeout"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			data := validConfig
			if tc.replace[0] != "" {
				data = strings.Replace(data, tc.replace[0], tc.replace[1], 1)
			}
			data += tc.extra
			_, err := ParseConfig([]byte(data))
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("ParseConfig() error = %v, want containing %q", err, tc.wantErr)
			}
		})
	}
}

func TestLoadConfigTokenFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "config.toml")
	if err := os.WriteFile(path, []byte(validConfig), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err := LoadConfig(path)
	if err != nil {
		t.Fatalf("LoadConfig() error = %v", err)
	}
	if want := filepath.Join(dir, "token"); cfg.TokenFile != want {
		t.Errorf("TokenFile = %q, want %q", cfg.TokenFile, want)
	}

	if err := os.WriteFile(path, []byte(validConfig+"token_file = \"sub/secret\"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err = LoadConfig(path)
	if err != nil {
		t.Fatalf("LoadConfig() error = %v", err)
	}
	if want := filepath.Join(dir, "sub", "secret"); cfg.TokenFile != want {
		t.Errorf("TokenFile = %q, want %q", cfg.TokenFile, want)
	}
}

func TestDefaultConfigPath(t *testing.T) {
	tests := []struct {
		name string
		env  []string
		want string
	}{
		{name: "xdg", env: []string{"XDG_CONFIG_HOME=/x", "HOME=/h"}, want: "/x/gh-ask/config.toml"},
		{name: "home", env: []string{"HOME=/h"}, want: "/h/.config/gh-ask/config.toml"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := DefaultConfigPath(tc.env)
			if err != nil {
				t.Fatalf("DefaultConfigPath() error = %v", err)
			}
			if got != tc.want {
				t.Errorf("DefaultConfigPath() = %q, want %q", got, tc.want)
			}
		})
	}
	if _, err := DefaultConfigPath(nil); err == nil {
		t.Error("DefaultConfigPath(nil) error = nil, want error")
	}
}
