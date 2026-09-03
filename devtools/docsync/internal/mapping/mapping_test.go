package mapping

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoadValid(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	path := write(t, dir, "docsync.yml", `
version: 1
rules:
  - match: ["internal/auth/**/*.go"]
    docs:
      - path: docs/api-reference.md
        section: "## Authentication"
        why: "token TTL + claim shape"
  - match: ["config/schema.go"]
    docs:
      - path: docs/configuration.md
        why: "documented option list"
`)
	m, err := Load(path)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if m.Version != 1 {
		t.Fatalf("Version = %d, want 1", m.Version)
	}
	if m.Root != dir {
		t.Fatalf("Root = %q, want %q", m.Root, dir)
	}
	if len(m.Rules) != 2 {
		t.Fatalf("len(Rules) = %d, want 2", len(m.Rules))
	}
	if m.Rules[0].Docs[0].Section != "## Authentication" {
		t.Fatalf("section = %q", m.Rules[0].Docs[0].Section)
	}
	if m.Rules[1].Match[0] != "config/schema.go" {
		t.Fatalf("match = %q", m.Rules[1].Match[0])
	}
}

func TestLoadStripsLeadingSlashOnMatch(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
version: 1
rules:
  - match: ["/config/schema.go"]
    docs:
      - path: docs/configuration.md
`)
	m, err := Load(path)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if m.Rules[0].Match[0] != "config/schema.go" {
		t.Fatalf("match = %q, want config/schema.go", m.Rules[0].Match[0])
	}
}

func TestLoadUnknownKey(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
version: 1
rules:
  - match: ["a.go"]
    sections: oops
    docs:
      - path: docs/a.md
`)
	_, err := Load(path)
	if err == nil {
		t.Fatal("Load() error = nil, want unknown-key error")
	}
	if !strings.Contains(err.Error(), "sections") {
		t.Fatalf("error = %v, want it to mention sections", err)
	}
}

func TestLoadWrongVersion(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
version: 2
rules:
  - match: ["a.go"]
    docs:
      - path: docs/a.md
`)
	_, err := Load(path)
	if err == nil {
		t.Fatal("Load() error = nil, want version error")
	}
	if !strings.Contains(err.Error(), "version") {
		t.Fatalf("error = %v, want it to mention version", err)
	}
}

func TestLoadMissingVersion(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
rules:
  - match: ["a.go"]
    docs:
      - path: docs/a.md
`)
	_, err := Load(path)
	if err == nil {
		t.Fatal("Load() error = nil, want version error")
	}
}

func TestLoadEmptyMatch(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
version: 1
rules:
  - match: []
    docs:
      - path: docs/a.md
`)
	_, err := Load(path)
	if err == nil {
		t.Fatal("Load() error = nil")
	}
	if !strings.Contains(err.Error(), "rule[0].match must be non-empty") {
		t.Fatalf("error = %v", err)
	}
}

func TestLoadEmptyDocs(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
version: 1
rules:
  - match: ["a.go"]
    docs: []
`)
	_, err := Load(path)
	if err == nil {
		t.Fatal("Load() error = nil")
	}
	if !strings.Contains(err.Error(), "rule[0].docs must be non-empty") {
		t.Fatalf("error = %v", err)
	}
}

func TestLoadEmptyDocPath(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
version: 1
rules:
  - match: ["a.go"]
    docs:
      - path: ""
`)
	_, err := Load(path)
	if err == nil {
		t.Fatal("Load() error = nil")
	}
	if !strings.Contains(err.Error(), "rule[0].docs[0].path must be non-empty") {
		t.Fatalf("error = %v", err)
	}
}

func TestLoadInvalidGlobBareDoublestar(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
version: 1
rules:
  - match: ["ok.go"]
    docs:
      - path: docs/a.md
  - match: ["x.go"]
    docs:
      - path: docs/b.md
  - match: ["y.go"]
    docs:
      - path: docs/c.md
  - match: ["src/**.go"]
    docs:
      - path: docs/d.md
`)
	_, err := Load(path)
	if err == nil {
		t.Fatal("Load() error = nil")
	}
	if !strings.Contains(err.Error(), `rule[3].match "src/**.go" is not a valid glob (bare ** must be a path segment)`) {
		t.Fatalf("error = %v", err)
	}
}

func TestSchemaIssuesCollectsMultiple(t *testing.T) {
	t.Parallel()
	path := write(t, t.TempDir(), "docsync.yml", `
version: 1
rules:
  - match: []
    docs:
      - path: docs/a.md
  - match: ["src/**.go"]
    docs:
      - path: ""
`)
	m, err := ParseFile(path)
	if err != nil {
		t.Fatalf("ParseFile() error = %v", err)
	}
	issues := m.SchemaIssues()
	if len(issues) < 3 {
		t.Fatalf("issues = %#v, want at least 3", issues)
	}
}

func TestFindWalksUp(t *testing.T) {
	t.Parallel()
	root := t.TempDir()
	write(t, root, "docsync.yml", "version: 1\nrules: []\n")
	nested := filepath.Join(root, "a", "b")
	if err := os.MkdirAll(nested, 0o755); err != nil {
		t.Fatal(err)
	}
	got, err := Find(nested)
	if err != nil {
		t.Fatalf("Find() error = %v", err)
	}
	want := filepath.Join(root, "docsync.yml")
	if got != want {
		t.Fatalf("Find() = %q, want %q", got, want)
	}
}

func TestFindNotFound(t *testing.T) {
	t.Parallel()
	_, err := Find(t.TempDir())
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Find() error = %v, want ErrNotFound", err)
	}
}

func TestRelativize(t *testing.T) {
	t.Parallel()
	root := t.TempDir()
	inside := filepath.Join(root, "internal", "auth", "token.go")
	rel, ok := Relativize(root, inside)
	if !ok || rel != "internal/auth/token.go" {
		t.Fatalf("Relativize(inside) = %q, %v", rel, ok)
	}
	outside := filepath.Join(filepath.Dir(root), "other", "x.go")
	if _, ok := Relativize(root, outside); ok {
		t.Fatal("Relativize(outside) ok = true, want false")
	}
}

func TestLintDocsMissingAndEmptyMatch(t *testing.T) {
	t.Parallel()
	root := t.TempDir()
	if err := os.MkdirAll(filepath.Join(root, "docs"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(root, "docs", "ok.md"), []byte("# ok\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(root, "present.go"), []byte("package p\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	path := write(t, root, "docsync.yml", `
version: 1
rules:
  - match: ["present.go"]
    docs:
      - path: docs/ok.md
  - match: ["present.go"]
    docs:
      - path: docs/old-name.md
  - match: ["legacy/*.go"]
    docs:
      - path: docs/ok.md
`)
	m, err := Load(path)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	issues := m.Lint()
	var errs, warns int
	var joined strings.Builder
	for _, iss := range issues {
		joined.WriteString(string(iss.Severity) + " " + iss.Message + "\n")
		switch iss.Severity {
		case SeverityError:
			errs++
		case SeverityWarn:
			warns++
		}
	}
	got := joined.String()
	if errs < 1 || warns < 1 {
		t.Fatalf("errs=%d warns=%d issues=%s", errs, warns, got)
	}
	if !strings.Contains(got, `rule[1].docs[0].path "docs/old-name.md" does not exist`) {
		t.Fatalf("issues = %s", got)
	}
	if !strings.Contains(got, `rule[2].match ["legacy/*.go"] matches no files currently tracked`) {
		t.Fatalf("issues = %s", got)
	}
}

func write(t *testing.T, dir, name, body string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(strings.TrimSpace(body)+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}
