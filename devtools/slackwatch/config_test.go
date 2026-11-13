package slackwatch

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

func writeFile(t *testing.T, name, data string, mode os.FileMode) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), name)
	if err := os.WriteFile(path, []byte(data), mode); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, mode); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestLoadConfigDefaults(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", "/xdg/state")
	path := writeFile(t, "config.json", `{"notifyTemplate": "echo hi"}`, 0o600)
	cfg, err := LoadConfig(path)
	if err != nil {
		t.Fatal(err)
	}
	if cfg.NotifyTemplate != "echo hi" {
		t.Errorf("NotifyTemplate = %q", cfg.NotifyTemplate)
	}
	if cfg.NotifySelf {
		t.Error("NotifySelf default should be false")
	}
	if cfg.SweepInterval.Duration != 6*time.Hour {
		t.Errorf("SweepInterval = %v", cfg.SweepInterval)
	}
	if cfg.QuietExpiry.Duration != 336*time.Hour {
		t.Errorf("QuietExpiry = %v", cfg.QuietExpiry)
	}
	if cfg.ReopenWindow.Duration != 720*time.Hour {
		t.Errorf("ReopenWindow = %v", cfg.ReopenWindow)
	}
	if cfg.ExcerptLimit != 300 {
		t.Errorf("ExcerptLimit = %d", cfg.ExcerptLimit)
	}
	if cfg.NotifyTimeout.Duration != 30*time.Second {
		t.Errorf("NotifyTimeout = %v", cfg.NotifyTimeout)
	}
	if cfg.NotifyCheckCommand != "" {
		t.Errorf("NotifyCheckCommand = %q", cfg.NotifyCheckCommand)
	}
	if len(cfg.NotifyRetryAfter) != 2 || cfg.NotifyRetryAfter[0].Duration != 2*time.Second || cfg.NotifyRetryAfter[1].Duration != 5*time.Second {
		t.Errorf("NotifyRetryAfter = %+v", cfg.NotifyRetryAfter)
	}
	if cfg.UnownedGrace.Duration != 10*time.Second {
		t.Errorf("UnownedGrace = %v", cfg.UnownedGrace)
	}
	if cfg.ChannelCacheTTL.Duration != DefaultChannelCacheTTL {
		t.Errorf("ChannelCacheTTL = %v", cfg.ChannelCacheTTL)
	}
	if cfg.ChannelLookupWait.Duration != DefaultChannelLookupWait {
		t.Errorf("ChannelLookupWait = %v", cfg.ChannelLookupWait)
	}
	if want := "/xdg/state/slackwatch/slackwatch.db"; cfg.DBPath != want {
		t.Errorf("DBPath = %q, want %q", cfg.DBPath, want)
	}
}

func TestLoadConfigMissingFileUsesDefaults(t *testing.T) {
	cfg, err := LoadConfig(filepath.Join(t.TempDir(), "missing.json"))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.ExcerptLimit != 300 {
		t.Errorf("ExcerptLimit = %d", cfg.ExcerptLimit)
	}
}

func TestLoadConfigOverrides(t *testing.T) {
	path := writeFile(t, "config.json", `{
		"notifyTemplate": "notify",
		"livenessTemplate": "alive",
		"notifyTimeout": "10s",
		"notifyCheckCommand": "agentbus status",
		"notifyRetryAfter": ["1s", "250ms"],
		"targets": {"ops": "pane:1"},
		"fallbackTarget": "ops",
		"footerPattern": "^-- (\\S+)$",
		"notifySelf": true,
		"sweepInterval": "90m",
		"quietExpiry": "1h30m",
		"reopenWindow": "48h",
		"unownedGrace": "3s",
		"excerptLimit": 50,
		"dbPath": "/tmp/x.db",
		"tokenFile": "/tmp/tokens",
		"outbox": "ops/outbox"
	}`, 0o600)
	cfg, err := LoadConfig(path)
	if err != nil {
		t.Fatal(err)
	}
	if cfg.SweepInterval.Duration != 90*time.Minute {
		t.Errorf("SweepInterval = %v", cfg.SweepInterval)
	}
	if cfg.QuietExpiry.Duration != 90*time.Minute {
		t.Errorf("QuietExpiry = %v", cfg.QuietExpiry)
	}
	if cfg.ReopenWindow.Duration != 48*time.Hour {
		t.Errorf("ReopenWindow = %v", cfg.ReopenWindow)
	}
	if cfg.NotifyTimeout.Duration != 10*time.Second {
		t.Errorf("NotifyTimeout = %v", cfg.NotifyTimeout)
	}
	if cfg.NotifyCheckCommand != "agentbus status" {
		t.Errorf("NotifyCheckCommand = %q", cfg.NotifyCheckCommand)
	}
	if len(cfg.NotifyRetryAfter) != 2 || cfg.NotifyRetryAfter[0].Duration != time.Second || cfg.NotifyRetryAfter[1].Duration != 250*time.Millisecond {
		t.Errorf("NotifyRetryAfter = %+v", cfg.NotifyRetryAfter)
	}
	if cfg.UnownedGrace.Duration != 3*time.Second {
		t.Errorf("UnownedGrace = %v", cfg.UnownedGrace)
	}
	if !cfg.NotifySelf || cfg.ExcerptLimit != 50 || cfg.DBPath != "/tmp/x.db" ||
		cfg.TokenFile != "/tmp/tokens" || cfg.Targets["ops"] != "pane:1" ||
		cfg.FallbackTarget != "ops" || cfg.LivenessTemplate != "alive" ||
		cfg.Outbox != "ops/outbox" {
		t.Errorf("cfg = %+v", cfg)
	}
	if cfg.FooterPattern.Regexp == nil || !cfg.FooterPattern.MatchString("-- ops") {
		t.Errorf("FooterPattern = %v", cfg.FooterPattern)
	}
}

func TestLoadConfigInbound(t *testing.T) {
	path := writeFile(t, "config.json", `{
		"inbound": {"target": "bus-in", "ignoreAuthors": ["UBOT"]}
	}`, 0o600)
	cfg, err := LoadConfig(path)
	if err != nil {
		t.Fatal(err)
	}
	if cfg.Inbound == nil || cfg.Inbound.Target != "bus-in" ||
		!reflect.DeepEqual(cfg.Inbound.Kinds, []string{"dm", "mention"}) ||
		!reflect.DeepEqual(cfg.Inbound.IgnoreAuthors, []string{"UBOT"}) ||
		len(cfg.Inbound.IgnoreChannels) != 0 {
		t.Errorf("inbound = %+v", cfg.Inbound)
	}
	for name, body := range map[string]string{
		"missing target": `{"inbound": {}}`,
		"blank target":   `{"inbound": {"target": "  "}}`,
		"bad kind":       `{"inbound": {"target": "bus", "kinds": ["dm", "ping"]}}`,
		"unknown":        `{"inbound": {"target": "bus", "extra": true}}`,
	} {
		t.Run(name, func(t *testing.T) {
			path := writeFile(t, "config.json", body, 0o600)
			if _, err := LoadConfig(path); err == nil {
				t.Fatal("expected error")
			}
		})
	}
}

func TestLoadConfigRejectsUnknownKeys(t *testing.T) {
	path := writeFile(t, "config.json", `{"notifyTemplate": "x", "botToken": "xoxb"}`, 0o600)
	_, err := LoadConfig(path)
	if err == nil || !strings.Contains(err.Error(), "botToken") {
		t.Fatalf("err = %v, want unknown key error", err)
	}
}

func TestLoadConfigRejectsBadValues(t *testing.T) {
	for name, body := range map[string]string{
		"duration":      `{"sweepInterval": "6 hours"}`,
		"durationType":  `{"sweepInterval": 6}`,
		"footerPattern": `{"footerPattern": "("}`,
		"trailing":      `{} {}`,
	} {
		t.Run(name, func(t *testing.T) {
			path := writeFile(t, "config.json", body, 0o600)
			if _, err := LoadConfig(path); err == nil {
				t.Fatal("expected error")
			}
		})
	}
}

func TestConfigPath(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", "/xdg/config")
	t.Setenv("SLACKWATCH_CONFIG", "")
	if got, want := ConfigPath(""), "/xdg/config/slackwatch/config.json"; got != want {
		t.Errorf("default = %q, want %q", got, want)
	}
	t.Setenv("SLACKWATCH_CONFIG", "/env/config.json")
	if got := ConfigPath(""); got != "/env/config.json" {
		t.Errorf("env = %q", got)
	}
	if got := ConfigPath("/flag/config.json"); got != "/flag/config.json" {
		t.Errorf("flag = %q", got)
	}
}

func TestCheckTokenFile(t *testing.T) {
	open := writeFile(t, "tokens-open", "secret", 0o644)
	err := CheckTokenFile(open)
	if err == nil {
		t.Fatal("0644 token file should be rejected")
	}
	if strings.Contains(err.Error(), "secret") {
		t.Errorf("error leaks token: %v", err)
	}
	private := writeFile(t, "tokens-private", "secret", 0o600)
	if err := CheckTokenFile(private); err != nil {
		t.Fatalf("0600 token file rejected: %v", err)
	}
	if err := CheckTokenFile(""); err == nil {
		t.Fatal("empty tokenFile should be rejected")
	}
	if err := CheckTokenFile(filepath.Join(t.TempDir(), "missing")); err == nil {
		t.Fatal("missing token file should be rejected")
	}
}

func TestLoadTokens(t *testing.T) {
	good := writeFile(t, "tokens", `{"appToken": "xapp-1", "userToken": "xoxp-2"}`, 0o600)
	got, err := LoadTokens(good)
	if err != nil {
		t.Fatal(err)
	}
	if got.AppToken != "xapp-1" || got.UserToken != "xoxp-2" {
		t.Errorf("tokens = %+v", got)
	}
	for name, body := range map[string]string{
		"raw":     "xoxp-secret",
		"missing": `{"appToken": "xapp-secret"}`,
		"unknown": `{"appToken": "xapp-secret", "userToken": "xoxp-secret", "bot": "x"}`,
	} {
		_, err := LoadTokens(writeFile(t, "tokens-"+name, body, 0o600))
		if err == nil {
			t.Errorf("%s: accepted", name)
			continue
		}
		if strings.Contains(err.Error(), "secret") {
			t.Errorf("%s: error leaks token: %v", name, err)
		}
	}
}
