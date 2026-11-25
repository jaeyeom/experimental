package decide

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoadConfigKeysValidation(t *testing.T) {
	t.Run("unknown action", func(t *testing.T) {
		dir := t.TempDir()
		t.Setenv("XDG_CONFIG_HOME", dir)
		writeTestConfig(t, dir, `{"ui":{"keys":{"nope.action":["x"]}}}`)
		_, err := LoadConfig()
		if err == nil || !strings.Contains(err.Error(), "nope.action") {
			t.Fatalf("err = %v, want unknown action name", err)
		}
	})

	t.Run("bad key", func(t *testing.T) {
		dir := t.TempDir()
		t.Setenv("XDG_CONFIG_HOME", dir)
		writeTestConfig(t, dir, `{"ui":{"keys":{"detail.reply":["notakey"]}}}`)
		_, err := LoadConfig()
		if err == nil || !strings.Contains(err.Error(), "detail.reply") || !strings.Contains(err.Error(), "notakey") {
			t.Fatalf("err = %v, want action and bad key", err)
		}
	})

	t.Run("conflict in one view", func(t *testing.T) {
		dir := t.TempDir()
		t.Setenv("XDG_CONFIG_HOME", dir)
		writeTestConfig(t, dir, `{"ui":{"keys":{"list.archive":["j"]}}}`)
		_, err := LoadConfig()
		if err == nil || !strings.Contains(err.Error(), "list.next") || !strings.Contains(err.Error(), "list.archive") || !strings.Contains(err.Error(), `"j"`) {
			t.Fatalf("err = %v, want conflict naming both actions and key", err)
		}
	})

	t.Run("prefix clash", func(t *testing.T) {
		dir := t.TempDir()
		t.Setenv("XDG_CONFIG_HOME", dir)
		writeTestConfig(t, dir, `{"ui":{"keys":{"list.next":["g"]}}}`)
		_, err := LoadConfig()
		if err == nil || !strings.Contains(err.Error(), `"g"`) || !strings.Contains(err.Error(), "list.next") || !strings.Contains(err.Error(), "goto.") {
			t.Fatalf("err = %v, want prefix clash naming key and actions", err)
		}
	})

	t.Run("editor prefix clash", func(t *testing.T) {
		dir := t.TempDir()
		t.Setenv("XDG_CONFIG_HOME", dir)
		writeTestConfig(t, dir, `{"ui":{"keys":{"edit.killLine":["ctrl+x"]}}}`)
		_, err := LoadConfig()
		if err == nil || !strings.Contains(err.Error(), "ctrl+x") || !strings.Contains(err.Error(), "edit.killLine") || !strings.Contains(err.Error(), "edit.external") {
			t.Fatalf("err = %v, want editor prefix clash", err)
		}
	})

	t.Run("valid rebind and unbind", func(t *testing.T) {
		dir := t.TempDir()
		t.Setenv("XDG_CONFIG_HOME", dir)
		writeTestConfig(t, dir, `{"ui":{"keys":{"detail.reply":["r","R"],"list.snooze":[],"edit.external":["C-x C-e","ctrl+g"]}}}`)
		cfg, err := LoadConfig()
		if err != nil {
			t.Fatalf("LoadConfig: %v", err)
		}
		km, err := ResolveKeyMap(cfg.UI.Keys)
		if err != nil {
			t.Fatalf("ResolveKeyMap: %v", err)
		}
		if got := km.ConfigKeys("detail.reply"); len(got) != 2 || got[0] != "r" || got[1] != "R" {
			t.Fatalf("detail.reply = %v", got)
		}
		if got := km.ConfigKeys("list.snooze"); len(got) != 0 {
			t.Fatalf("list.snooze = %v, want empty", got)
		}
		if got := km.ConfigKeys("edit.external"); len(got) != 2 || got[0] != "ctrl+x ctrl+e" || got[1] != "ctrl+g" {
			t.Fatalf("edit.external = %v", got)
		}
	})
}

func TestFormatKeyMapListsEveryAction(t *testing.T) {
	km, err := ResolveKeyMap(map[string][]string{
		"detail.reply": {"r", "R"},
		"list.snooze":  {},
	})
	if err != nil {
		t.Fatalf("ResolveKeyMap: %v", err)
	}
	text := FormatKeyMapText(km)
	for _, d := range ActionCatalog() {
		if !strings.Contains(text, d.Name) {
			t.Errorf("text missing action %q", d.Name)
		}
	}
	if !strings.Contains(text, "(unbound)") {
		t.Errorf("text missing (unbound) for empty binding:\n%s", text)
	}

	raw, err := FormatKeyMapJSON(km)
	if err != nil {
		t.Fatalf("FormatKeyMapJSON: %v", err)
	}
	var parsed struct {
		UI struct {
			Keys map[string][]string `json:"keys"`
		} `json:"ui"`
	}
	if err := json.Unmarshal(raw, &parsed); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	for _, d := range ActionCatalog() {
		if _, ok := parsed.UI.Keys[d.Name]; !ok {
			t.Errorf("JSON missing action %q", d.Name)
		}
	}
	if got := parsed.UI.Keys["detail.reply"]; len(got) != 2 || got[0] != "r" || got[1] != "R" {
		t.Errorf("JSON detail.reply = %v", got)
	}
	if got := parsed.UI.Keys["list.snooze"]; len(got) != 0 {
		t.Errorf("JSON list.snooze = %v, want empty", got)
	}
}

func TestReadmeKeyTableMatchesDefaultKeyMap(t *testing.T) {
	data, err := os.ReadFile("docs/inbox.org")
	if err != nil {
		t.Fatalf("read docs/inbox.org: %v", err)
	}
	want := FormatReadmeKeyTable(DefaultKeyMap())
	if !strings.Contains(string(data), want) {
		t.Fatalf("docs/inbox.org key table does not match FormatReadmeKeyTable(DefaultKeyMap()); expected:\n%s", want)
	}
}

func writeTestConfig(t *testing.T, xdgDir, content string) {
	t.Helper()
	path := filepath.Join(xdgDir, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
}

func TestPrimaryButtonString(t *testing.T) {
	cases := []struct {
		name      string
		overrides map[string][]string
		action    string
		wantUTF8  string
		wantASCII string
	}{
		{
			name:      "default compose.send",
			action:    "compose.send",
			wantUTF8:  "^⏎",
			wantASCII: "C-Enter",
		},
		{
			name:      "remapped to ctrl+g",
			overrides: map[string][]string{"compose.send": {"ctrl+g"}, "edit.external": {"ctrl+x ctrl+e"}},
			action:    "compose.send",
			wantUTF8:  "^G",
			wantASCII: "C-g",
		},
		{
			name:      "remapped to enter",
			overrides: map[string][]string{"compose.send": {"enter"}},
			action:    "compose.send",
			wantUTF8:  "⏎",
			wantASCII: "Enter",
		},
		{
			name:      "unbound action",
			overrides: map[string][]string{"compose.send": {}},
			action:    "compose.send",
			wantUTF8:  "",
			wantASCII: "",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			km, err := ResolveKeyMap(tc.overrides)
			if err != nil {
				t.Fatalf("ResolveKeyMap: %v", err)
			}
			if got := km.PrimaryButtonString(tc.action, false); got != tc.wantUTF8 {
				t.Errorf("UTF8 = %q, want %q", got, tc.wantUTF8)
			}
			if got := km.PrimaryButtonString(tc.action, true); got != tc.wantASCII {
				t.Errorf("ASCII = %q, want %q", got, tc.wantASCII)
			}
		})
	}
}

func TestShortestButtonString(t *testing.T) {
	cases := []struct {
		name      string
		overrides map[string][]string
		action    string
		wantUTF8  string
		wantASCII string
	}{
		{
			name:      "default edit.external picks ctrl+g over ctrl+x ctrl+e",
			action:    "edit.external",
			wantUTF8:  "^G",
			wantASCII: "C-g",
		},
		{
			name:      "custom single chord",
			overrides: map[string][]string{"edit.external": {"ctrl+x ctrl+e"}},
			action:    "edit.external",
			wantUTF8:  "^X ^E",
			wantASCII: "C-x C-e",
		},
		{
			name:      "custom single key",
			overrides: map[string][]string{"edit.external": {"ctrl+t"}},
			action:    "edit.external",
			wantUTF8:  "^T",
			wantASCII: "C-t",
		},
		{
			name:      "unbound action",
			overrides: map[string][]string{"edit.external": {}},
			action:    "edit.external",
			wantUTF8:  "",
			wantASCII: "",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			km, err := ResolveKeyMap(tc.overrides)
			if err != nil {
				t.Fatalf("ResolveKeyMap: %v", err)
			}
			if got := km.ShortestButtonString(tc.action, false); got != tc.wantUTF8 {
				t.Errorf("UTF8 = %q, want %q", got, tc.wantUTF8)
			}
			if got := km.ShortestButtonString(tc.action, true); got != tc.wantASCII {
				t.Errorf("ASCII = %q, want %q", got, tc.wantASCII)
			}
		})
	}
}
