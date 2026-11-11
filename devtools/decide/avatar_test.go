package decide

import (
	"os"
	"path/filepath"
	"testing"
)

func TestMatchAvatar(t *testing.T) {
	rules := []AvatarRule{
		{Match: "coord/*", File: "glob.txt"},
		{Match: "pr-*", File: "pr.webp"},
		{Match: "coord/primary", File: "exact.png"},
		{Match: "pr-*", File: "second.txt"},
		{Match: "[", File: "bad.txt"},
	}
	tests := []struct {
		name string
		want string
		ok   bool
	}{
		{"coord/primary", "exact.png", true},
		{"coord/other", "glob.txt", true},
		{"coordx", "", false},
		{"pr-12", "pr.webp", true},
		{"[", "bad.txt", true},
		{"other", "", false},
	}
	for _, tt := range tests {
		got, ok := MatchAvatar(rules, tt.name)
		if ok != tt.ok || got.File != tt.want {
			t.Errorf("MatchAvatar(%q) = %q, %v; want %q, %v", tt.name, got.File, ok, tt.want, tt.ok)
		}
	}
}

func TestLoadConfigAvatars(t *testing.T) {
	cfgHome := t.TempDir()
	home := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfgHome)
	t.Setenv("HOME", home)
	dir := filepath.Join(cfgHome, "decide")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	data := `{
  "avatars": [
    {"match": "a", "file": "~/avatars/a.png"},
    {"match": "b", "file": "avatars/b.txt"},
    {"match": "c", "file": "/abs/c.webp"}
  ],
  "avatarDefault": true,
  "avatarMono": "hide"
}`
	if err := os.WriteFile(filepath.Join(dir, "config.json"), []byte(data), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if !cfg.AvatarDefault {
		t.Error("AvatarDefault = false, want true")
	}
	if cfg.AvatarMono != AvatarMonoHide {
		t.Errorf("AvatarMono = %q, want %q", cfg.AvatarMono, AvatarMonoHide)
	}
	want := []string{
		filepath.Join(home, "avatars", "a.png"),
		filepath.Join(dir, "avatars", "b.txt"),
		"/abs/c.webp",
	}
	if len(cfg.Avatars) != len(want) {
		t.Fatalf("Avatars = %+v", cfg.Avatars)
	}
	for i, w := range want {
		if cfg.Avatars[i].File != w {
			t.Errorf("Avatars[%d].File = %q, want %q", i, cfg.Avatars[i].File, w)
		}
	}
}

func TestLoadConfigAvatarDefaultOff(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if cfg.AvatarDefault || len(cfg.Avatars) != 0 || cfg.AvatarMono != "" {
		t.Errorf("cfg = %+v, want no avatars", cfg)
	}
}
