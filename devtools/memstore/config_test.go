package memstore

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestConfigPath(t *testing.T) {
	tests := []struct {
		name     string
		flagPath string
		env      map[string]string
		want     string
		wantErr  bool
	}{
		{
			name:     "flag overrides everything",
			flagPath: "/custom/flag.toml",
			env: map[string]string{
				"MEMSTORE_CONFIG": "/env/config.toml",
				"XDG_CONFIG_HOME": "/xdg",
				"HOME":            "/home/u",
			},
			want: "/custom/flag.toml",
		},
		{
			name: "MEMSTORE_CONFIG overrides XDG and HOME",
			env: map[string]string{
				"MEMSTORE_CONFIG": "/env/config.toml",
				"XDG_CONFIG_HOME": "/xdg",
				"HOME":            "/home/u",
			},
			want: "/env/config.toml",
		},
		{
			name: "XDG_CONFIG_HOME used when set",
			env: map[string]string{
				"XDG_CONFIG_HOME": "/xdg",
				"HOME":            "/home/u",
			},
			want: "/xdg/memstore/config.toml",
		},
		{
			name: "falls back to HOME/.config",
			env: map[string]string{
				"HOME": "/home/u",
			},
			want: "/home/u/.config/memstore/config.toml",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			getenv := func(k string) string { return tc.env[k] }
			got, err := ConfigPath(tc.flagPath, getenv)
			if (err != nil) != tc.wantErr {
				t.Fatalf("ConfigPath(%q) error = %v, wantErr %v", tc.flagPath, err, tc.wantErr)
			}
			if got != tc.want {
				t.Errorf("ConfigPath(%q) = %q, want %q", tc.flagPath, got, tc.want)
			}
		})
	}
}

func TestLoadConfigDefaultsOnMissingFile(t *testing.T) {
	env := map[string]string{"HOME": "/home/u"}
	getenv := func(k string) string { return env[k] }

	cfg, err := LoadConfig(filepath.Join(t.TempDir(), "missing.toml"), getenv)
	if err != nil {
		t.Fatalf("LoadConfig missing file: %v", err)
	}
	if cfg.RetentionDays != DefaultRetentionDays {
		t.Errorf("RetentionDays = %d, want %d", cfg.RetentionDays, DefaultRetentionDays)
	}
	if len(cfg.Vaults) != 0 || len(cfg.Profiles) != 0 {
		t.Errorf("expected empty vaults/profiles, got %+v", cfg)
	}
}

func TestLoadConfigValid(t *testing.T) {
	env := map[string]string{"HOME": "/home/u"}
	getenv := func(k string) string { return env[k] }

	tomlData := `
retention_days = 90

[vaults.shared]
path = "~/mem/shared.db"

[vaults.work]
path = "/var/mem/work.db"
residency = "cloud-inbox"
contacts_config = "~/.config/contacts/config.toml"

[vaults.cloud]
path = "/var/mem/cloud.db"
residency = "cloud-hosted"

[profiles.work-agent]
vaults = ["shared", "work"]
compartments = ["general", "engineering"]
max_sensitivity = "normal"
capabilities = ["read", "write-episodic", "propose-fact"]
`
	path := filepath.Join(t.TempDir(), "config.toml")
	if err := os.WriteFile(path, []byte(tomlData), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg, err := LoadConfig(path, getenv)
	if err != nil {
		t.Fatalf("LoadConfig: %v", err)
	}
	if cfg.RetentionDays != 90 {
		t.Errorf("RetentionDays = %d, want 90", cfg.RetentionDays)
	}

	shared := cfg.Vaults["shared"]
	if shared.Name != "shared" {
		t.Errorf("shared.Name = %q, want shared", shared.Name)
	}
	if shared.Path != "/home/u/mem/shared.db" {
		t.Errorf("shared.Path = %q, want /home/u/mem/shared.db", shared.Path)
	}
	if shared.Residency != ResidencyLocalOnly {
		t.Errorf("shared.Residency = %q, want %q", shared.Residency, ResidencyLocalOnly)
	}

	work := cfg.Vaults["work"]
	if work.Residency != ResidencyCloudInbox {
		t.Errorf("work.Residency = %q, want %q", work.Residency, ResidencyCloudInbox)
	}
	if work.ContactsConfig != "/home/u/.config/contacts/config.toml" {
		t.Errorf("work.ContactsConfig = %q, want /home/u/.config/contacts/config.toml", work.ContactsConfig)
	}

	prof := cfg.Profiles["work-agent"]
	if prof.Name != "work-agent" {
		t.Errorf("prof.Name = %q, want work-agent", prof.Name)
	}
	if prof.MaxSensitivity != SensitivityNormal {
		t.Errorf("prof.MaxSensitivity = %q, want %q", prof.MaxSensitivity, SensitivityNormal)
	}
}

func TestLoadConfigValidationErrors(t *testing.T) {
	env := map[string]string{"HOME": "/home/u"}
	getenv := func(k string) string { return env[k] }

	tests := []struct {
		name    string
		toml    string
		wantSub string
	}{
		{
			name:    "invalid toml syntax",
			toml:    `[vaults.bad`,
			wantSub: "parse config",
		},
		{
			name:    "negative retention_days",
			toml:    `retention_days = -5`,
			wantSub: "retention_days",
		},
		{
			name: "empty vault path",
			toml: `
[vaults.shared]
residency = "local-only"
`,
			wantSub: "path",
		},
		{
			name: "unknown residency",
			toml: `
[vaults.shared]
path = "/tmp/shared.db"
residency = "mars-only"
`,
			wantSub: "residency",
		},
		{
			name: "unknown sensitivity",
			toml: `
[vaults.shared]
path = "/tmp/shared.db"
[profiles.p]
vaults = ["shared"]
compartments = ["general"]
max_sensitivity = "top-secret"
capabilities = ["read"]
`,
			wantSub: "sensitivity",
		},
		{
			name: "empty sensitivity",
			toml: `
[vaults.shared]
path = "/tmp/shared.db"
[profiles.p]
vaults = ["shared"]
compartments = ["general"]
capabilities = ["read"]
`,
			wantSub: "sensitivity",
		},
		{
			name: "unknown capability",
			toml: `
[vaults.shared]
path = "/tmp/shared.db"
[profiles.p]
vaults = ["shared"]
compartments = ["general"]
max_sensitivity = "normal"
capabilities = ["read", "nuke"]
`,
			wantSub: "capability",
		},
		{
			name: "profile references undefined vault",
			toml: `
[vaults.shared]
path = "/tmp/shared.db"
[profiles.p]
vaults = ["shared", "missing"]
compartments = ["general"]
max_sensitivity = "normal"
capabilities = ["read"]
`,
			wantSub: "undefined vault",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "config.toml")
			if err := os.WriteFile(path, []byte(tc.toml), 0o600); err != nil {
				t.Fatal(err)
			}
			_, err := LoadConfig(path, getenv)
			if err == nil {
				t.Fatalf("LoadConfig succeeded, want error containing %q", tc.wantSub)
			}
			if !strings.Contains(err.Error(), tc.wantSub) {
				t.Errorf("LoadConfig error = %q, want substring %q", err.Error(), tc.wantSub)
			}
		})
	}
}
