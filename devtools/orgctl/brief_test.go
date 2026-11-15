package orgctl

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func briefSample() (Role, Spec) {
	role := Role{
		Cwd: "/work/acme/lead",
		Brief: Brief{
			File: "briefs/lead.md",
			Text: "Keep the desk moving.\r\nAsk before changing a plan.",
		},
	}
	org := Spec{
		Rules: []Rule{
			{ID: "plain-words", Text: "Write so a new teammate can follow the note."},
			{ID: "no-secrets", Text: "Never write down a secret value."},
		},
	}
	return role, org
}

const wantBrief = `Keep the desk moving.
Ask before changing a plan.

no-secrets
Never write down a secret value.

plain-words
Write so a new teammate can follow the note.
`

func TestRenderIsByteIdentical(t *testing.T) {
	role, org := briefSample()
	first := Render(role, org)
	second := Render(role, org)
	if first != second {
		t.Fatalf("renders differ:\n%s\n---\n%s", first, second)
	}
	if first != wantBrief {
		t.Fatalf("render = %q", first)
	}
	role.Brief.Text = "Keep the desk moving.\nAsk before changing a plan."
	if Render(role, org) != first {
		t.Fatal("line endings were not normalized")
	}

	role, org = briefSample()
	role.Rules = []string{"plain-words"}
	only := Render(role, org)
	if strings.Contains(only, "no-secrets") || !strings.Contains(only, "plain-words") {
		t.Fatalf("selected rules = %q", only)
	}
	role.Rules = []string{}
	none := Render(role, org)
	if strings.Contains(none, "no-secrets") || strings.Contains(none, "plain-words") {
		t.Fatalf("empty rules = %q", none)
	}
}

func TestChangedRuleChangesHash(t *testing.T) {
	role, org := briefSample()
	before := Hash(role, org)
	org.Rules[0].Text = "Write in another way."
	after := Hash(role, org)
	if before == after {
		t.Fatal("hash did not change")
	}
}

func TestCompareEditedFile(t *testing.T) {
	role, org := briefSample()
	dir := t.TempDir()
	path := filepath.Join(dir, role.Brief.File)
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	body := []byte("someone edited this\n")
	if err := os.WriteFile(path, body, 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, 0o444); err != nil {
		t.Fatal(err)
	}
	got, err := Compare(role, org, dir)
	if err != nil {
		t.Fatal(err)
	}
	if got != "edited in place" {
		t.Fatalf("status = %q", got)
	}
	after, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(after) != string(body) {
		t.Fatal("brief file changed")
	}
}

func TestCompareMissingFile(t *testing.T) {
	role, org := briefSample()
	dir := t.TempDir()
	got, err := Compare(role, org, dir)
	if err != nil {
		t.Fatal(err)
	}
	if got != "missing" {
		t.Fatalf("status = %q", got)
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 0 {
		t.Fatalf("dir entries = %v", entries)
	}
}

func TestCompareEmptyTextIsUnmanaged(t *testing.T) {
	role, org := briefSample()
	role.Brief.Text = ""
	dir := t.TempDir()
	path := filepath.Join(dir, role.Brief.File)
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	body := []byte("hand written\n")
	if err := os.WriteFile(path, body, 0o600); err != nil {
		t.Fatal(err)
	}
	got, err := Compare(role, org, dir)
	if err != nil {
		t.Fatal(err)
	}
	if got != BriefUnmanaged {
		t.Fatalf("status = %q, want unmanaged", got)
	}
	after, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(after) != string(body) {
		t.Fatal("brief file changed")
	}

	missing := Role{Brief: Brief{File: "briefs/gone.md"}}
	got, err = Compare(missing, org, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	if got != BriefMissing {
		t.Fatalf("absent = %q, want missing", got)
	}
}

func TestCompareExpandsHome(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	role, org := briefSample()
	role.Brief.File = "~/briefs/x.md"
	path := filepath.Join(home, "briefs", "x.md")
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(Render(role, org)), 0o600); err != nil {
		t.Fatal(err)
	}
	got, err := Compare(role, org, "/unused")
	if err != nil || got != BriefOK {
		t.Fatalf("tilde file = %q, %v", got, err)
	}

	role.Cwd = "~/proj"
	role.Brief.File = "BRIEF.md"
	rel := filepath.Join(home, "proj", "BRIEF.md")
	if err := os.MkdirAll(filepath.Dir(rel), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(rel, []byte(Render(role, org)), 0o600); err != nil {
		t.Fatal(err)
	}
	got, err = Compare(role, org, role.Cwd)
	if err != nil || got != BriefOK {
		t.Fatalf("relative file = %q, %v", got, err)
	}
}

func TestCompareMatchingFile(t *testing.T) {
	role, org := briefSample()
	dir := t.TempDir()
	path := filepath.Join(dir, role.Brief.File)
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(Render(role, org)), 0o600); err != nil {
		t.Fatal(err)
	}
	got, err := Compare(role, org, dir)
	if err != nil {
		t.Fatal(err)
	}
	if got != "ok" {
		t.Fatalf("status = %q", got)
	}
}
