package unnecessaryinterfaceassertion_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/unnecessaryinterfaceassertion"
)

func TestLintFlagsUnnecessaryInterfaceAssertionFixture(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not available")
	}
	setupGoPackagesEnv(t)

	linter := unnecessaryinterfaceassertion.New()
	linter.Dir = testdataDir(t)

	issues, err := linter.Lint([]string{"./..."})
	if err != nil {
		t.Fatalf("Lint() returned error: %v", err)
	}
	if len(issues) != 1 {
		t.Fatalf("expected 1 issue, got %d: %v", len(issues), issues)
	}
	got := issues[0]
	if !strings.Contains(filepath.ToSlash(got.FilePath), "unnecessary/unnecessary.go") {
		t.Errorf("FilePath = %q, want path containing unnecessary/unnecessary.go", got.FilePath)
	}
	if got.Message != "Unnecessary interface assertion" {
		t.Errorf("Message = %q, want %q", got.Message, "Unnecessary interface assertion")
	}
	if got.ToolName != "unnecessary-interface-assertion-linter" {
		t.Errorf("ToolName = %q, want unnecessary-interface-assertion-linter", got.ToolName)
	}
	if got.Line <= 0 {
		t.Errorf("Line = %d, want a positive line number", got.Line)
	}
}

// setupGoPackagesEnv makes packages.Load work in the Bazel sandbox without
// downloading a toolchain into t.TempDir() (those files are not writable, so
// cleanup fails on GitHub Actions coverage runs).
func setupGoPackagesEnv(t *testing.T) {
	t.Helper()
	t.Setenv("GOPACKAGESDRIVER", "off")
	t.Setenv("GOTOOLCHAIN", "local")
	t.Setenv("GOCACHE", writableTempDir(t))
	gopath := writableTempDir(t)
	t.Setenv("GOPATH", gopath)
	t.Setenv("GOMODCACHE", filepath.Join(gopath, "pkg", "mod"))
}

func writableTempDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	t.Cleanup(func() { chmodRecursively(dir) })
	return dir
}

func chmodRecursively(dir string) {
	_ = os.Chmod(dir, 0o755)
	entries, err := os.ReadDir(dir)
	if err != nil {
		return
	}
	for _, e := range entries {
		if e.Type()&os.ModeSymlink != 0 {
			continue
		}
		path := filepath.Join(dir, e.Name())
		if e.IsDir() {
			chmodRecursively(path)
			continue
		}
		_ = os.Chmod(path, 0o644)
	}
}

func testdataDir(t *testing.T) string {
	t.Helper()
	var candidates []string
	_, file, _, ok := runtime.Caller(0)
	if ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "testdata"))
	}
	candidates = append(candidates, "testdata")
	for _, dir := range candidates {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err != nil {
			continue
		}
		if _, err := os.Stat(filepath.Join(dir, "unnecessary", "unnecessary.go")); err != nil {
			continue
		}
		abs, err := filepath.Abs(dir)
		if err != nil {
			t.Fatal(err)
		}
		return abs
	}
	t.Fatal("testdata directory with go.mod not found")
	return ""
}
