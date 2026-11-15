package main

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"
)

func TestReadmeMatchesRenameBehavior(t *testing.T) {
	readme := readmeText(t)
	for _, want := range []string{
		"plan file",
		"docs directory",
		"plan.md",
		"docs/project/PROJ-123.md",
		"docs/project/PROJ-123-login-flow.md",
		"in place",
		"no dry run",
		"clean Git tree",
		"OLD-KEY-*.md",
		"subdirectory",
		"go install github.com/jaeyeom/experimental/devtools/rename-jira-keys/cmd/rename-jira-keys@latest",
		"//devtools/rename-jira-keys/cmd/rename-jira-keys",
		"//devtools/rename-jira-keys/...",
		"1.26",
		"--old",
		"--old-key",
		"--new",
		"--new-key",
		"-p",
		"--plan-file",
		"-d",
		"--docs-dir",
	} {
		if !strings.Contains(readme, want) {
			t.Errorf("README lacks %q", want)
		}
	}
	for _, gone := range []string{
		"test_jira_rename",
		"1.16",
		"Implementation Notes",
		"//devtools/rename-jira-keys:rename-jira-keys",
		"//devtools/rename-jira-keys:all",
	} {
		if strings.Contains(readme, gone) {
			t.Errorf("README still mentions %q", gone)
		}
	}
	command := strings.Index(readme, "rename-jira-keys --old PROJ-123 --new ACME-10")
	install := strings.Index(readme, "* Install")
	if command < 0 || install < 0 || command > install {
		t.Fatal("quick start command must appear before Install")
	}
	example := exampleAfter(t, readme, command)
	if !strings.Contains(example, "Summary: Updated 3 files") {
		t.Fatalf("quick start example has no summary:\n%s", example)
	}

	dir := t.TempDir()
	docs := filepath.Join(dir, "docs", "project")
	if err := os.MkdirAll(docs, 0o755); err != nil {
		t.Fatal(err)
	}
	write := func(path, body string) {
		t.Helper()
		if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	write(filepath.Join(dir, "plan.md"), "See PROJ-123.\n")
	write(filepath.Join(docs, "PROJ-123.md"), "# PROJ-123\n")
	write(filepath.Join(docs, "PROJ-123-login-flow.md"), "# PROJ-123 login\n")
	t.Chdir(dir)

	oldStdout := os.Stdout
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	os.Stdout = w
	runErr := runRename(Config{
		OldKey:   "PROJ-123",
		NewKey:   "ACME-10",
		PlanFile: "plan.md",
		DocsDir:  "docs/project",
	})
	_ = w.Close()
	os.Stdout = oldStdout
	if runErr != nil {
		t.Fatal(runErr)
	}
	var buf bytes.Buffer
	if _, err := io.Copy(&buf, r); err != nil {
		t.Fatal(err)
	}
	if strings.TrimRight(buf.String(), "\n") != strings.TrimRight(example, "\n") {
		t.Fatalf("quick start output differs\ngot:\n%s\nwant:\n%s", buf.String(), example)
	}
}

func exampleAfter(t *testing.T, doc string, from int) string {
	t.Helper()
	re := regexp.MustCompile(`(?s)#\+BEGIN_EXAMPLE\n(.*?)\n#\+END_EXAMPLE`)
	loc := re.FindStringSubmatchIndex(doc[from:])
	if loc == nil {
		t.Fatal("no example after the quick start command")
	}
	return doc[from+loc[2] : from+loc[3]]
}

func readmeText(t *testing.T) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "..", "..", "README.org"))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools/rename-jira-keys/README.org"))
	}
	for _, path := range candidates {
		body, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		return string(body)
	}
	t.Fatal("README.org not found")
	return ""
}
