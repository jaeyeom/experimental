package baseline

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestLoadMissingFile(t *testing.T) {
	path := filepath.Join(t.TempDir(), "does-not-exist.json")
	f, err := Load(path)
	if err != nil {
		t.Fatalf("Load(%q) unexpected error: %v", path, err)
	}
	if f.Version != CurrentVersion {
		t.Errorf("Version = %d, want %d", f.Version, CurrentVersion)
	}
	if f.Entries == nil {
		t.Error("Entries is nil, want non-nil empty map")
	}
	if len(f.Entries) != 0 {
		t.Errorf("len(Entries) = %d, want 0", len(f.Entries))
	}
}

func TestMarshalCanonical(t *testing.T) {
	f := &File{
		Version: CurrentVersion,
		Checker: "agents-md-size",
		Entries: map[string]Entry{
			"third_party/generated/AGENTS.md": {},
			"devtools/setup-dev/ansible/AGENTS.md": {
				Limits: map[string]int64{
					"max_cascading_bytes": 18432,
					"max_bytes":           5120,
				},
				Reason: "Legacy Ansible playbook instructions pending split",
				Issue:  "#1069",
			},
		},
	}

	gotBytes, err := Marshal(f)
	if err != nil {
		t.Fatalf("Marshal() error: %v", err)
	}
	got := string(gotBytes)

	want := `{
  "checker": "agents-md-size",
  "entries": {
    "devtools/setup-dev/ansible/AGENTS.md": {
      "issue": "#1069",
      "limits": {
        "max_bytes": 5120,
        "max_cascading_bytes": 18432
      },
      "reason": "Legacy Ansible playbook instructions pending split"
    },
    "third_party/generated/AGENTS.md": {}
  },
  "version": 1
}
`
	if got != want {
		t.Errorf("Marshal() mismatch:\ngot:\n%s\nwant:\n%s", got, want)
	}
	if !strings.HasSuffix(got, "\n") {
		t.Error("Marshal() output missing trailing newline")
	}
}

func TestSaveAndLoadRoundTrip(t *testing.T) {
	path := filepath.Join(t.TempDir(), "nested", ".baselines", "agents-md-size.json")
	orig := &File{
		Version: CurrentVersion,
		Checker: "agents-md-size",
		Entries: map[string]Entry{
			"**/testdata/**": {
				Reason: "Test fixtures are exempt from size limits",
			},
			"devtools/pkg/AGENTS.md": {
				Limits: map[string]int64{"max_bytes": 4096},
				Issue:  "#1071",
			},
		},
	}

	if err := Save(path, orig); err != nil {
		t.Fatalf("Save(%q) error: %v", path, err)
	}

	loaded, err := Load(path)
	if err != nil {
		t.Fatalf("Load(%q) error: %v", path, err)
	}
	if !reflect.DeepEqual(loaded, orig) {
		t.Errorf("round-trip mismatch:\ngot:  %+v\nwant: %+v", loaded, orig)
	}
}

func TestSaveNilFile(t *testing.T) {
	path := filepath.Join(t.TempDir(), "nil.json")
	if err := Save(path, nil); err == nil {
		t.Fatal("Save(nil) expected error, got nil")
	}
}

func TestLoadValidationErrors(t *testing.T) {
	tests := []struct {
		name    string
		content string
	}{
		{
			name:    "invalid JSON",
			content: `{not json`,
		},
		{
			name:    "trailing JSON data",
			content: `{"version": 1, "checker": "x", "entries": {}} {"extra": 1}`,
		},
		{
			name:    "unsupported version 2",
			content: `{"version": 2, "checker": "x", "entries": {}}`,
		},
		{
			name:    "unsupported version 0",
			content: `{"version": 0, "checker": "x", "entries": {}}`,
		},
		{
			name:    "unknown top-level field",
			content: `{"version": 1, "checker": "x", "entries": {}, "extra": true}`,
		},
		{
			name:    "unknown entry field",
			content: `{"version": 1, "checker": "x", "entries": {"a/b.go": {"limit": 10}}}`,
		},
		{
			name:    "absolute path key",
			content: `{"version": 1, "checker": "x", "entries": {"/etc/passwd": {}}}`,
		},
		{
			name:    "parent traversal key",
			content: `{"version": 1, "checker": "x", "entries": {"../secret": {}}}`,
		},
		{
			name:    "nested parent traversal key",
			content: `{"version": 1, "checker": "x", "entries": {"a/../b": {}}}`,
		},
		{
			name:    "backslash key",
			content: `{"version": 1, "checker": "x", "entries": {"a\\b.go": {}}}`,
		},
		{
			name:    "leading dot-slash key",
			content: `{"version": 1, "checker": "x", "entries": {"./a/b.go": {}}}`,
		},
		{
			name:    "trailing slash key",
			content: `{"version": 1, "checker": "x", "entries": {"a/b/": {}}}`,
		},
		{
			name:    "double slash key",
			content: `{"version": 1, "checker": "x", "entries": {"a//b.go": {}}}`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "baseline.json")
			if err := os.WriteFile(path, []byte(tc.content), 0o600); err != nil {
				t.Fatalf("write fixture: %v", err)
			}
			if _, err := Load(path); err == nil {
				t.Fatalf("Load() expected error for %s, got nil", tc.name)
			}
		})
	}
}
