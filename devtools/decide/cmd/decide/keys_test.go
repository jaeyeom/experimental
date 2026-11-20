package main

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestCmdKeysAndPrintKeys(t *testing.T) {
	cfgDir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfgDir)
	cfgPath := filepath.Join(cfgDir, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, []byte(`{"ui":{"keys":{"detail.reply":["r","R"]}}}`), 0o600); err != nil {
		t.Fatal(err)
	}

	var stdout, stderr bytes.Buffer
	if code := run([]string{"keys"}, &stdout, &stderr); code != 0 {
		t.Fatalf("decide keys failed (%d): %s", code, stderr.String())
	}
	out := stdout.String()
	for _, d := range decide.ActionCatalog() {
		if !strings.Contains(out, d.Name) {
			t.Errorf("decide keys missing %q", d.Name)
		}
	}
	if !strings.Contains(out, "r, R") {
		t.Errorf("decide keys missing rebound detail.reply:\n%s", out)
	}

	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"keys", "--json"}, &stdout, &stderr); code != 0 {
		t.Fatalf("decide keys --json failed (%d): %s", code, stderr.String())
	}
	var parsed struct {
		UI struct {
			Keys map[string][]string `json:"keys"`
		} `json:"ui"`
	}
	if err := json.Unmarshal(stdout.Bytes(), &parsed); err != nil {
		t.Fatalf("unmarshal keys --json: %v", err)
	}
	if got := parsed.UI.Keys["detail.reply"]; len(got) != 2 || got[0] != "r" || got[1] != "R" {
		t.Errorf("JSON detail.reply = %v", got)
	}

	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"inbox", "--print-keys"}, &stdout, &stderr); code != 0 {
		t.Fatalf("decide inbox --print-keys failed (%d): %s", code, stderr.String())
	}
	if stdout.String() != out {
		t.Errorf("inbox --print-keys output differs from decide keys")
	}
}
