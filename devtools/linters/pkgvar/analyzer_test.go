package pkgvar_test

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/pkgvar"
	"golang.org/x/tools/go/analysis/analysistest"
)

func TestDefaultAllowlist(t *testing.T) {
	flag := pkgvar.Analyzer.Flags.Lookup("allowlist")
	if flag == nil {
		t.Fatal("allowlist flag is not defined")
		return
	}
	const want = "errors.New,fmt.Errorf,regexp.MustCompile,strings.Join,strings.Repeat,strings.ReplaceAll,strings.Split"
	if flag.DefValue != want {
		t.Fatalf("default allowlist = %q, want %q", flag.DefValue, want)
	}
}

func TestPkgVar(t *testing.T) {
	setAllowlist(t, "errors.New,fmt.Errorf,regexp.MustCompile,strings.Join,strings.Repeat,strings.ReplaceAll,strings.Split")
	analysistest.Run(reporter{t}, prepareAnalysis(t), pkgvar.Analyzer, "p")
}

func TestAllowlistRejectsBadEntry(t *testing.T) {
	setAllowlist(t, "not-a-function")
	got := &capture{}
	analysistest.Run(got, prepareAnalysis(t), pkgvar.Analyzer, "p")
	if !strings.Contains(strings.Join(got.errs, "\n"), "invalid allowlist entry") {
		t.Fatalf("errors = %q, want invalid allowlist entry", got.errs)
	}
}

// Tests share Analyzer.Flags, so they must not call t.Parallel.
func setAllowlist(t *testing.T, value string) {
	t.Helper()
	flag := pkgvar.Analyzer.Flags.Lookup("allowlist")
	if flag == nil {
		t.Fatal("allowlist flag is not defined")
		return
	}
	prev := flag.Value.String()
	if err := flag.Value.Set(value); err != nil {
		t.Fatalf("set allowlist: %v", err)
	}
	t.Cleanup(func() {
		if err := flag.Value.Set(prev); err != nil {
			t.Errorf("restore allowlist: %v", err)
		}
	})
}

// reporter forwards analysistest failures. It is not a testing.TB.
// rules_go links binaries with GOROOT set to the dummy value "GOROOT",
// and analysistest skips its go-tool GOROOT check for non-TB reporters.
type reporter struct{ t *testing.T }

func (r reporter) Errorf(format string, args ...any) {
	r.t.Helper()
	r.t.Errorf(format, args...)
}

func (r reporter) Fatalf(format string, args ...any) {
	r.t.Helper()
	r.t.Fatalf(format, args...)
}

type capture struct {
	errs []string
}

func (c *capture) Errorf(format string, args ...any) {
	c.errs = append(c.errs, fmt.Sprintf(format, args...))
}

func (c *capture) Fatalf(format string, args ...any) {
	c.errs = append(c.errs, fmt.Sprintf(format, args...))
}

func prepareAnalysis(t *testing.T) string {
	t.Helper()
	t.Setenv("GOTOOLCHAIN", "local")
	t.Setenv("GOPACKAGESDRIVER", "off")
	// net/http pulls cgo on darwin. The Bazel sandbox has no C compiler.
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
	return testdataDir(t)
}

// ensureGoOnPath keeps the go command on the same minor version as the test
// binary. A newer go writes export data that golang.org/x/tools v0.40 cannot
// import, and analysistest then skips the package.
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
	cmd := exec.Command(bin, "env", "GOVERSION") //nolint:gosec // G702: fixed argv, trusted go binary
	cmd.Env = withoutEnv(os.Environ(), "GOROOT")
	out, err := cmd.Output()
	if err != nil {
		return false
	}
	return minorVersion(strings.TrimSpace(string(out))) == minorVersion(runtimeVersion)
}

func withoutEnv(env []string, key string) []string {
	prefix := key + "="
	out := make([]string, 0, len(env))
	for _, kv := range env {
		if strings.HasPrefix(kv, prefix) {
			continue
		}
		out = append(out, kv)
	}
	return out
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
	seen := map[string]struct{}{}
	var mismatched []string
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
				if _, ok := seen[candidate]; ok {
					continue
				}
				seen[candidate] = struct{}{}
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
				mismatched = append(mismatched, candidate)
			}
		}
	}
	return "", "", fmt.Errorf("no go %s in runfiles %v (found %v)", minorVersion(runtimeVersion), roots, mismatched)
}

func testdataDir(t *testing.T) string {
	t.Helper()
	var candidates []string
	_, file, _, ok := runtime.Caller(0)
	if ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "testdata"))
	}
	candidates = append(candidates,
		"testdata",
		filepath.Join("devtools", "linters", "pkgvar", "testdata"),
	)
	marker := filepath.Join("src", "p", "p.go")
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
