package linters_test

import (
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"
)

func TestReadmeRequiredPhrases(t *testing.T) {
	readme := readTreeFile(t, "README.org")
	org := strings.Index(readme, "\n** org-lint\n")
	if org < 0 {
		t.Fatal("README has no ** org-lint section")
	}
	head := readme[:org]
	for _, want := range []string{
		"#+TITLE: Linters",
		"six linters",
		"py-sibling-import",
		"subdirectory=devtools/linters/pylinters",
		"migration-immutability",
		"go install github.com/jaeyeom/experimental/devtools/linters/cmd/migration-immutability@latest",
		"org-lint",
		"agents-md-size",
		"unnecessary-interface-assertion-linter",
		"pkgvar",
		"make lint",
		"go vet",
		"[[file:../devcheck/README.org][devcheck]]",
		"[[file:../setup-dev/README.org][setup-dev]]",
		"1.26",
		"//nolint:pkgvar // set with -ldflags -X",
		"var maxRetries = 3",
		"regexp.MustCompile",
		"loadConfig",
		"os.Getenv",
		"example.com/config.Load",
		"set no trust level",
		"(low)",
		"(nil)",
		"(high)",
		"sample.org:3: (low) Link to non-existent local file \"missing.org\"",
		"sample.org:7: (nil) Duplicate CUSTOM_ID property \"same\"",
		"sample.org:15: (high) \"nowhere\" not a supported value for #+ATTR_ORG keyword attribute \":align\".",
		"sample.go:10:5: package-level variable can be a constant",
		"sample.go:12:5: package-level initializer calls an impure function or receives from a channel",
		"sample.go:14:5: package-level initializer discards its result",
		"bad.go:9:1: Unnecessary interface assertion",
		"go install github.com/jaeyeom/experimental/devtools/linters/cmd/agents-md-size@latest",
		"go install github.com/jaeyeom/experimental/devtools/linters/cmd/org-lint@latest",
		"go install github.com/jaeyeom/experimental/devtools/linters/cmd/pkgvar@latest",
		"go install github.com/jaeyeom/experimental/devtools/linters/cmd/unnecessary-interface-assertion-linter@latest",
		"go vet -vettool=$(which pkgvar) ./...",
		"./ensure.sh org-lint",
		"exits 0",
		"exits 1",
		"exits 2",
		"exits 3",
	} {
		if !strings.Contains(head, want) && !strings.Contains(readme, want) {
			t.Errorf("README lacks %q", want)
		}
	}
	for _, want := range []string{
		"org-lint",
		"agents-md-size",
		"unnecessary-interface-assertion-linter",
		"pkgvar",
		"py-sibling-import",
		"* Index",
	} {
		if !strings.Contains(head, want) {
			t.Errorf("first screen lacks %q", want)
		}
	}
	for _, gone := range []string{
		"1.16",
		"dart-mode",
		"a name keeps the result",
		"*** Features",
		"#+TITLE: Org Linters",
		"go install ./cmd/org-lint",
		"go install ./devtools/linters/cmd/org-lint",
	} {
		if strings.Contains(readme, gone) {
			t.Errorf("README still mentions %q", gone)
		}
	}
	if pureWord.MatchString(readme) {
		t.Error(`README uses the undefined word "pure"`)
	}
	const defaults = "errors.New,fmt.Errorf,regexp.MustCompile,strings.Join,strings.Repeat,strings.ReplaceAll,strings.Split"
	if got := strings.Count(readme, defaults); got != 1 {
		t.Errorf("default allowlist appears %d times, want 1", got)
	}
	if got := strings.Count(readme, defaults+",example.com/config.Load"); got != 1 {
		t.Errorf("allowlist example that adds a function appears %d times, want 1", got)
	}
	if got := strings.Count(readme, "go install github.com/jaeyeom/experimental/devtools/linters/cmd/org-lint@latest"); got != 1 {
		t.Errorf("org-lint install command appears %d times, want 1", got)
	}
}

func TestReadmeQuotesLinterDiagnostics(t *testing.T) {
	readme := readTreeFile(t, "README.org")
	analyzer := readTreeFile(t, "pkgvar/analyzer.go")
	for _, name := range []string{"diagConst", "diagDiscard", "diagImpure"} {
		msg := constString(t, analyzer, name)
		if !strings.Contains(readme, msg) {
			t.Errorf("README lacks %s %q", name, msg)
		}
	}
	linter := readTreeFile(t, "unnecessaryinterfaceassertion/linter.go")
	msg := messageString(t, linter)
	if !strings.Contains(readme, msg) {
		t.Errorf("README lacks interface diagnostic %q", msg)
	}
}

func TestReadmeIndexTableIsRectangular(t *testing.T) {
	readme := readTreeFile(t, "README.org")
	org := strings.Index(readme, "\n** org-lint\n")
	if org < 0 {
		t.Fatal("README has no ** org-lint section")
	}
	var widths []int
	for _, line := range strings.Split(readme[:org], "\n") {
		if strings.HasPrefix(line, "|") {
			widths = append(widths, len(line))
		}
	}
	if len(widths) < 7 {
		t.Fatalf("index table has %d rows, want header, separator, and five tools", len(widths))
	}
	for i, width := range widths {
		if width != widths[0] {
			t.Errorf("index table line %d has width %d, want %d", i+1, width, widths[0])
		}
	}
}

var pureWord = regexp.MustCompile(`(?:^|[^A-Za-z])pure(?:[^A-Za-z]|$)`)

func constString(t *testing.T, src, name string) string {
	t.Helper()
	re := regexp.MustCompile(`(?m)^\s*` + name + `\s+=\s+"([^"]+)"`)
	match := re.FindStringSubmatch(src)
	if match == nil {
		t.Fatalf("const %s not found", name)
	}
	return match[1]
}

func messageString(t *testing.T, src string) string {
	t.Helper()
	re := regexp.MustCompile(`Message:\s+"([^"]+)"`)
	match := re.FindStringSubmatch(src)
	if match == nil {
		t.Fatal("Message field not found")
	}
	return match[1]
}

func readTreeFile(t *testing.T, rel string) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), rel))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "linters", rel))
	}
	for _, path := range candidates {
		body, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		return string(body)
	}
	t.Fatalf("file %s not found", rel)
	return ""
}
