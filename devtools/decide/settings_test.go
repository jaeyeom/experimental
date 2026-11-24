package decide

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestSetConfigValuePreservesUnknownKeysOrderAndWritesOneBackup(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "config.json")
	orig := "{\n  \"customTop\": 123,\n  \"ui\": {\n    \"customUI\": \"keep\",\n    \"utc\": false\n  },\n  \"tailKey\": \"end\"\n}\n"
	if err := os.WriteFile(path, []byte(orig), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg, err := SetConfigValueAt(path, "ui.utc", true)
	if err != nil {
		t.Fatalf("SetConfigValueAt(ui.utc): %v", err)
	}
	if !cfg.UI.UTC {
		t.Fatalf("cfg.UI.UTC = false, want true")
	}

	gotBytes, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	got := string(gotBytes)
	for _, want := range []string{`"customTop": 123`, `"customUI": "keep"`, `"utc": true`, `"tailKey": "end"`} {
		if !strings.Contains(got, want) {
			t.Errorf("written config missing %q:\n%s", want, got)
		}
	}
	if strings.Index(got, `"customTop"`) > strings.Index(got, `"ui"`) ||
		strings.Index(got, `"ui"`) > strings.Index(got, `"tailKey"`) {
		t.Errorf("top-level key order changed:\n%s", got)
	}
	if strings.Index(got, `"customUI"`) > strings.Index(got, `"utc"`) {
		t.Errorf("ui key order changed:\n%s", got)
	}

	bakBytes, err := os.ReadFile(path + ".bak")
	if err != nil {
		t.Fatalf("read backup: %v", err)
	}
	if string(bakBytes) != orig {
		t.Fatalf("backup = %q, want original %q", string(bakBytes), orig)
	}

	// Second write in the same process must keep the original .bak.
	if _, err := SetConfigValueAt(path, "hyperlinks", false); err != nil {
		t.Fatalf("second SetConfigValueAt: %v", err)
	}
	bakBytes2, err := os.ReadFile(path + ".bak")
	if err != nil {
		t.Fatal(err)
	}
	if string(bakBytes2) != orig {
		t.Fatalf("second write overwrote backup: got %q, want %q", string(bakBytes2), orig)
	}
}

func TestSetConfigValueRejectsInvalidFileAndLeavesUnchanged(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "config.json")
	bad := `{"ui": {broken`
	if err := os.WriteFile(path, []byte(bad), 0o600); err != nil {
		t.Fatal(err)
	}

	if _, err := SetConfigValueAt(path, "ui.utc", true); err == nil {
		t.Fatal("expected error for invalid config.json, got nil")
	}
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != bad {
		t.Fatalf("invalid file was modified: got %q, want %q", string(got), bad)
	}
	if _, err := os.Stat(path + ".bak"); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("unexpected backup created for invalid file: err=%v", err)
	}
}

func TestSetConfigValueRejectsInvalidNewValue(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "config.json")
	orig := "{}\n"
	if err := os.WriteFile(path, []byte(orig), 0o600); err != nil {
		t.Fatal(err)
	}

	if _, err := SetConfigValueAt(path, "ui.answerAction", "bogus"); err == nil {
		t.Fatal("expected validation error for bogus answerAction")
	}
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != orig {
		t.Fatalf("file changed on invalid value: got %q, want %q", string(got), orig)
	}
}
