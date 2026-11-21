// Package analysistestenv prepares the environment for go/analysis analysistest
// under Bazel's strict action env and sandbox.
package analysistestenv

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// Reporter forwards analysistest failures without implementing testing.TB,
// so analysistest skips its go-tool GOROOT check under rules_go.
type Reporter struct{ T *testing.T }

// Errorf reports a test error.
func (r Reporter) Errorf(format string, args ...any) {
	r.T.Helper()
	r.T.Errorf(format, args...)
}

// Fatalf reports a fatal test error.
func (r Reporter) Fatalf(format string, args ...any) {
	r.T.Helper()
	r.T.Fatalf(format, args...)
}

// Prepare sets environment variables and PATH so analysistest can invoke go,
// and returns the absolute path to the testdata directory containing marker.
func Prepare(t *testing.T, pkgDir, marker string) string {
	t.Helper()
	t.Setenv("GOTOOLCHAIN", "local")
	t.Setenv("GOPACKAGESDRIVER", "off")
	t.Setenv("CGO_ENABLED", "0")
	if root := os.Getenv("GOROOT"); root != "" {
		if _, err := os.Stat(root); err != nil {
			if err := os.Unsetenv("GOROOT"); err != nil {
				t.Fatal(err)
			}
		}
	}
	if os.Getenv("TEST_TMPDIR") != "" {
		t.Setenv("GOCACHE", t.TempDir())
		t.Setenv("GOMODCACHE", t.TempDir())
		t.Setenv("GOTMPDIR", t.TempDir())
		t.Setenv("HOME", t.TempDir())
	}
	ensureGoOnPath(t)
	return testdataDir(t, pkgDir, marker)
}

func ensureGoOnPath(t *testing.T) {
	t.Helper()
	if bin, err := exec.LookPath("go"); err == nil && goMinorMatches(bin, runtime.Version()) {
		return
	}
	bin, root, err := findSDKGo(runtime.Version())
	if err != nil {
		t.Fatalf("go command not found: %v", err)
	}
	t.Setenv("GOROOT", root)
	t.Setenv("PATH", filepath.Dir(bin)+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func goMinorMatches(bin, runtimeVersion string) bool {
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	out, err := exec.Command(bin, "env", "GOVERSION").Output() //nolint:gosec // G204: trusted go binary
	if err != nil {
		return false
	}
	return minorVersion(strings.TrimSpace(string(out))) == minorVersion(runtimeVersion)
}

func minorVersion(v string) string {
	v = strings.TrimPrefix(v, "go")
	major, rest, ok := strings.Cut(v, ".")
	if !ok || major == "" {
		return ""
	}
	minor, _, _ := strings.Cut(rest, ".")
	if minor == "" {
		return ""
	}
	return major + "." + minor
}

func findSDKGo(runtimeVersion string) (string, string, error) {
	var roots []string
	for _, key := range []string{"RUNFILES_DIR", "TEST_SRCDIR"} {
		if v := os.Getenv(key); v != "" {
			roots = append(roots, v)
		}
	}
	if wd, err := os.Getwd(); err == nil {
		roots = append(roots, wd)
	}
	for _, root := range roots {
		for _, pattern := range []string{
			filepath.Join(root, "bin", "go"),
			filepath.Join(root, "*", "bin", "go"),
			filepath.Join(root, "*", "*", "bin", "go"),
		} {
			matches, err := filepath.Glob(pattern)
			if err != nil {
				return "", "", fmt.Errorf("glob %s: %w", pattern, err)
			}
			for _, candidate := range matches {
				info, err := os.Stat(candidate)
				if err != nil || info.IsDir() {
					continue
				}
				goroot := filepath.Dir(filepath.Dir(candidate))
				if _, err := os.Stat(filepath.Join(goroot, "src")); err != nil {
					continue
				}
				if goMinorMatches(candidate, runtimeVersion) {
					return candidate, goroot, nil
				}
			}
		}
	}
	return "", "", fmt.Errorf("no go %s in runfiles %v", minorVersion(runtimeVersion), roots)
}

func testdataDir(t *testing.T, pkgDir, marker string) string {
	t.Helper()
	candidates := []string{
		"testdata",
		filepath.Join(pkgDir, "testdata"),
	}
	for _, dir := range candidates {
		if _, err := os.Stat(filepath.Join(dir, marker)); err != nil {
			continue
		}
		abs, err := filepath.Abs(dir)
		if err != nil {
			t.Fatal(err)
		}
		return abs
	}
	t.Fatal("testdata directory not found")
	return ""
}
