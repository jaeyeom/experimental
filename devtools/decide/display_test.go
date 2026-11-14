package decide

import (
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// hubFile loads data as this machine's config file.
func hubFile(t *testing.T, data string) ConfigFile {
	t.Helper()
	writeConfig(t, data)
	f, err := LoadConfigFile()
	if err != nil {
		t.Fatal(err)
	}
	return f
}

func configDir(t *testing.T) string {
	t.Helper()
	path, ok := configPath()
	if !ok {
		t.Fatal("no config path")
	}
	return filepath.Dir(path)
}

func TestDisplaySettingsExportOnlyDisplayKeys(t *testing.T) {
	f := hubFile(t, `{
		"hub": "me@hub", "hubTimeout": "5s", "machine": "mac", "hubMachine": "hub",
		"openCommand": ["my-open"], "checkouts": [{"name": "repo", "path": "/src/repo"}],
		"notifyTemplate": "notify-me", "notifyPane": "w1:p1", "sendTemplate": "herdr send {pane}",
		"optionsOnly": true, "colors": {"human": "2"}, "ui": {"utc": true},
		"avatarDefault": true, "avatarMono": "hide", "recipients": {"agentbus": true},
		"recipientsTimeout": "5s", "targets": [{"name": "builder", "pane": "w2:p3"}],
		"hyperlinks": false, "hiddenContacts": ["zed"], "avatars": [{"match": "ada", "builtin": "owl"}]
	}`)
	got := f.Display()
	var keys []string
	for k := range got.Keys {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	want := []string{"avatarDefault", "avatarMono", "avatars", "hiddenContacts", "hyperlinks", "recipients", "recipientsTimeout", "targets"}
	if !slices.Equal(keys, want) {
		t.Fatalf("keys = %q, want %q", keys, want)
	}
	data, err := json.Marshal(got)
	if err != nil {
		t.Fatal(err)
	}
	for _, secret := range []string{"openCommand", "me@hub", "hubMachine", "checkouts", "/src/repo", "notify-me", "herdr send", "my-open", "machine", "utc"} {
		if strings.Contains(string(data), secret) {
			t.Fatalf("display settings carry %q: %s", secret, data)
		}
	}
}

func TestDisplaySettingsCarryAvatarFileBytes(t *testing.T) {
	f := hubFile(t, `{"avatars": [
		{"match": "ada", "file": "ada.txt"},
		{"match": "bob", "file": "gone.png"},
		{"match": "big", "file": "big.txt"},
		{"match": "owl", "builtin": "owl"}
	]}`)
	dir := configDir(t)
	if err := os.WriteFile(filepath.Join(dir, "ada.txt"), []byte("AB"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "big.txt"), make([]byte, MaxHubAvatarSize+1), 0o600); err != nil {
		t.Fatal(err)
	}
	got := f.Display()
	var rules []AvatarRule
	if err := json.Unmarshal(got.Keys["avatars"], &rules); err != nil {
		t.Fatal(err)
	}
	files := []string{rules[0].File, rules[1].File, rules[2].File}
	if !slices.Equal(files, []string{"hub:0.txt", "hub:1.png", "hub:2.txt"}) || rules[3].Builtin != "owl" {
		t.Fatalf("rules = %+v", rules)
	}
	if string(got.Files["hub:0.txt"].Data) != "AB" || got.Files["hub:0.txt"].Error != "" {
		t.Fatalf("ada file = %+v", got.Files["hub:0.txt"])
	}
	if e := got.Files["hub:1.png"]; e.Data != nil || !strings.Contains(e.Error, "not found") {
		t.Fatalf("missing file = %+v", e)
	}
	if e := got.Files["hub:2.txt"]; e.Data != nil || !strings.Contains(e.Error, "256 KB") {
		t.Fatalf("big file = %+v", e)
	}
	data, _ := json.Marshal(got)
	if strings.Contains(string(data), dir) {
		t.Fatalf("display settings carry the config path: %s", data)
	}
}

func TestMergeDisplayUsesTheHubForUnsetKeys(t *testing.T) {
	hub := hubFile(t, `{"avatars": [{"match": "ada", "builtin": "owl"}], "avatarDefault": true}`).Display()
	local := hubFile(t, `{"hub": "me@hub"}`)
	eff, err := MergeDisplay(local, hub)
	if err != nil {
		t.Fatal(err)
	}
	if !eff.Config.AvatarDefault || len(eff.Config.Avatars) != 1 || eff.Config.Avatars[0].Builtin != "owl" {
		t.Fatalf("config = %+v", eff.Config)
	}
	if eff.Config.Hub != "me@hub" {
		t.Fatalf("lost the local hub: %+v", eff.Config)
	}
	if eff.Sources["avatars"] != SourceHub || eff.Sources["avatarDefault"] != SourceHub || eff.Sources["hub"] != SourceLocal {
		t.Fatalf("sources = %v", eff.Sources)
	}
	if got := eff.Summary(); got != "display settings: hub (2 keys), local overrides: none" {
		t.Fatalf("summary = %q", got)
	}
}

func TestMergeDisplayLocalValueWins(t *testing.T) {
	hub := hubFile(t, `{"avatars": [{"match": "ada", "builtin": "owl"}], "avatarDefault": true, "hyperlinks": false}`).Display()
	local := hubFile(t, `{"avatarDefault": false}`)
	eff, err := MergeDisplay(local, hub)
	if err != nil {
		t.Fatal(err)
	}
	if eff.Config.AvatarDefault || len(eff.Config.Avatars) != 1 || eff.Config.HyperlinksOn() {
		t.Fatalf("config = %+v", eff.Config)
	}
	if eff.Sources["avatarDefault"] != SourceLocal || eff.Sources["avatars"] != SourceHub {
		t.Fatalf("sources = %v", eff.Sources)
	}
	if got := eff.Summary(); got != "display settings: hub (3 keys), local overrides: avatarDefault" {
		t.Fatalf("summary = %q", got)
	}
}

func TestMergeDisplayKeepsLocalAvatarsWhole(t *testing.T) {
	hub := hubFile(t, `{"avatars": [{"match": "ada", "builtin": "owl"}, {"match": "bob", "builtin": "fox"}]}`).Display()
	local := hubFile(t, `{"avatars": [{"match": "bob", "builtin": "cat"}]}`)
	eff, err := MergeDisplay(local, hub)
	if err != nil {
		t.Fatal(err)
	}
	if len(eff.Config.Avatars) != 1 || eff.Config.Avatars[0].Builtin != "cat" || eff.Files != nil {
		t.Fatalf("avatars = %+v, files %v", eff.Config.Avatars, eff.Files)
	}
}

func TestMergeDisplayIgnoresKeysOffTheAllowlist(t *testing.T) {
	hub := DisplaySettings{Keys: map[string]json.RawMessage{
		"openCommand": json.RawMessage(`["evil"]`),
		"hub":         json.RawMessage(`"elsewhere"`),
		"avatarMono":  json.RawMessage(`"hide"`),
	}}
	eff, err := MergeDisplay(hubFile(t, `{}`), hub)
	if err != nil {
		t.Fatal(err)
	}
	if len(eff.Config.OpenCommand) != 0 || eff.Config.Hub != "" || eff.Config.AvatarMono != "hide" {
		t.Fatalf("config = %+v", eff.Config)
	}
	if _, ok := eff.Sources["openCommand"]; ok {
		t.Fatalf("sources = %v", eff.Sources)
	}
}

func TestMergeDisplayKeepsHubAvatarNames(t *testing.T) {
	writeConfig(t, `{"avatars": [{"match": "ada", "file": "ada.txt"}]}`)
	if err := os.WriteFile(filepath.Join(configDir(t), "ada.txt"), []byte("AB"), 0o600); err != nil {
		t.Fatal(err)
	}
	f, err := LoadConfigFile()
	if err != nil {
		t.Fatal(err)
	}
	eff, err := MergeDisplay(hubFile(t, `{}`), f.Display())
	if err != nil {
		t.Fatal(err)
	}
	if eff.Config.Avatars[0].File != "hub:0.txt" || string(eff.Files["hub:0.txt"].Data) != "AB" {
		t.Fatalf("avatars = %+v, files %v", eff.Config.Avatars, eff.Files)
	}
}
