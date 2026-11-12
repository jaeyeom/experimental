package decide

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// configFile is the config path writeConfig set up.
func configFile(t *testing.T) string {
	t.Helper()
	return filepath.Join(os.Getenv("XDG_CONFIG_HOME"), "decide", "config.json")
}

func readFile(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}

func TestSetAvatarRuleAddsExactRuleFirst(t *testing.T) {
	original := `{"zeta": 1, "avatars": [{"match": "ops/*", "file": "/a.txt"}], "alpha": {"b": 2, "a": 1}}`
	writeConfig(t, original)
	rules, err := SetAvatarRule("ops/x", AvatarRule{Builtin: "owl"})
	if err != nil {
		t.Fatal(err)
	}
	want := `{
  "zeta": 1,
  "avatars": [
    {
      "match": "ops/x",
      "builtin": "owl"
    },
    {
      "match": "ops/*",
      "file": "/a.txt"
    }
  ],
  "alpha": {
    "b": 2,
    "a": 1
  }
}
`
	if got := readFile(t, configFile(t)); got != want {
		t.Fatalf("config =\n%s\nwant\n%s", got, want)
	}
	if got := readFile(t, configFile(t)+".bak"); got != original {
		t.Fatalf(".bak = %q, want the original", got)
	}
	if len(rules) != 2 || rules[0] != (AvatarRule{Match: "ops/x", Builtin: "owl"}) {
		t.Fatalf("rules = %+v", rules)
	}
	if r, _ := MatchAvatar(rules, "ops/x"); r.Builtin != "owl" {
		t.Fatalf("ops/x matches %+v", r)
	}
}

func TestSetAvatarRuleNoConfig(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	if _, err := SetAvatarRule("ops/x", AvatarRule{Builtin: "owl"}); err != nil {
		t.Fatal(err)
	}
	want := "{\n  \"avatars\": [\n    {\n      \"match\": \"ops/x\",\n      \"builtin\": \"owl\"\n    }\n  ]\n}\n"
	if got := readFile(t, configFile(t)); got != want {
		t.Fatalf("config =\n%s", got)
	}
	if _, err := os.Stat(configFile(t) + ".bak"); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf(".bak of a missing config: %v", err)
	}
}

func TestSetAvatarRuleReplacesExactFile(t *testing.T) {
	writeConfig(t, `{"avatars": [
  {"match": "ops/*", "builtin": "cat"},
  {"match": "ops/x", "file": "x.png", "note": "keep"},
  {"match": "*", "file": "all.txt"}
]}`)
	if _, err := SetAvatarRule("ops/x", AvatarRule{Builtin: "owl"}); err != nil {
		t.Fatal(err)
	}
	want := `{
  "avatars": [
    {
      "match": "ops/*",
      "builtin": "cat"
    },
    {
      "match": "ops/x",
      "note": "keep",
      "builtin": "owl"
    },
    {
      "match": "*",
      "file": "all.txt"
    }
  ]
}
`
	if got := readFile(t, configFile(t)); got != want {
		t.Fatalf("config =\n%s\nwant\n%s", got, want)
	}
}

func TestSetAvatarRuleDefaultRemovesExactRule(t *testing.T) {
	writeConfig(t, `{"avatars": [{"match": "ops/x", "builtin": "owl"}, {"match": "ops/*", "builtin": "cat"}]}`)
	rules, err := SetAvatarRule("ops/x", AvatarRule{})
	if err != nil {
		t.Fatal(err)
	}
	if len(rules) != 1 || rules[0].Match != "ops/*" {
		t.Fatalf("rules = %+v", rules)
	}
	if got := readFile(t, configFile(t)); strings.Contains(got, `"ops/x"`) {
		t.Fatalf("exact rule kept:\n%s", got)
	}
}

func TestSetAvatarRuleHide(t *testing.T) {
	writeConfig(t, `{"avatars": [{"match": "ops/x", "builtin": "owl"}]}`)
	rules, err := SetAvatarRule("ops/x", AvatarRule{Hide: true})
	if err != nil {
		t.Fatal(err)
	}
	if len(rules) != 1 || rules[0] != (AvatarRule{Match: "ops/x", Hide: true}) {
		t.Fatalf("rules = %+v", rules)
	}
	want := "{\n  \"avatars\": [\n    {\n      \"match\": \"ops/x\",\n      \"hide\": true\n    }\n  ]\n}\n"
	if got := readFile(t, configFile(t)); got != want {
		t.Fatalf("config =\n%s", got)
	}
}

func TestSetAvatarRuleBrokenConfigIsUntouched(t *testing.T) {
	for _, data := range []string{
		`{"avatars": [`,
		`{"avatars": {"match": "x"}}`,
		`[]`,
		`{"a": 1} {"b": 2}`,
		`{"avatars": [{"match": "x", "file": "a", "builtin": "owl"}]}`,
	} {
		writeConfig(t, data)
		if _, err := SetAvatarRule("ops/x", AvatarRule{Builtin: "owl"}); err == nil {
			t.Fatalf("%s: no error", data)
		}
		if got := readFile(t, configFile(t)); got != data {
			t.Fatalf("%s: config changed to %q", data, got)
		}
		if _, err := os.Stat(configFile(t) + ".bak"); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("%s: .bak written", data)
		}
	}
}

func TestSetAvatarRuleReadOnlyConfigIsUntouched(t *testing.T) {
	data := `{"avatars": []}`
	writeConfig(t, data)
	if err := os.Chmod(configFile(t), 0o400); err != nil {
		t.Fatal(err)
	}
	if _, err := SetAvatarRule("ops/x", AvatarRule{Builtin: "owl"}); err == nil {
		t.Fatal("no error for a read-only config")
	}
	if got := readFile(t, configFile(t)); got != data {
		t.Fatalf("config changed to %q", got)
	}
}

func TestSetAvatarRuleFollowsSymlink(t *testing.T) {
	cfgHome := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfgHome)
	target := filepath.Join(t.TempDir(), "real.json")
	if err := os.WriteFile(target, []byte(`{"keep": true}`), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(cfgHome, "decide"), 0o755); err != nil {
		t.Fatal(err)
	}
	link := configFile(t)
	if err := os.Symlink(target, link); err != nil {
		t.Fatal(err)
	}
	if _, err := SetAvatarRule("ops/x", AvatarRule{Builtin: "owl"}); err != nil {
		t.Fatal(err)
	}
	if fi, err := os.Lstat(link); err != nil || fi.Mode()&os.ModeSymlink == 0 {
		t.Fatalf("link replaced: %v %v", fi, err)
	}
	got := readFile(t, target)
	if !strings.Contains(got, `"keep": true`) || !strings.Contains(got, `"builtin": "owl"`) {
		t.Fatalf("target =\n%s", got)
	}
}

func TestSetAvatarRuleBackupOncePerSession(t *testing.T) {
	original := `{"avatars": []}`
	writeConfig(t, original)
	for _, id := range []string{"owl", "cat"} {
		if _, err := SetAvatarRule("ops/x", AvatarRule{Builtin: id}); err != nil {
			t.Fatal(err)
		}
	}
	if got := readFile(t, configFile(t)+".bak"); got != original {
		t.Fatalf(".bak = %q, want the original", got)
	}
}

func TestLoadConfigAvatarHide(t *testing.T) {
	writeConfig(t, `{"avatars": [{"match": "ops/x", "hide": true}]}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if r, ok := MatchAvatar(cfg.Avatars, "ops/x"); !ok || !r.Hide {
		t.Fatalf("rule = %+v, %v", r, ok)
	}
}
