// Package detector_test provides tests for the detector package.
package detector_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/detector"
)

func TestUnnecessaryInterfaceAssertionLinter(t *testing.T) {
	// This test requires the Go toolchain to be fully functional because packages.Load
	// internally uses 'go list'. Skip if running in Bazel sandbox as the module
	// resolution doesn't work correctly for dynamically created test modules.
	if os.Getenv("TEST_TMPDIR") != "" {
		t.Skip("Skipping test: packages.Load doesn't work correctly in Bazel sandbox")
	}

	// Also skip if Go toolchain is not available.
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("Skipping test: Go toolchain not available")
	}

	linter := detector.NewUnnecessaryInterfaceAssertionLinter()

	// Use TEST_TMPDIR if available (Bazel), otherwise use default temp directory.
	tempBase := os.Getenv("TEST_TMPDIR")
	if tempBase == "" {
		tempBase = os.TempDir()
	}

	// Create a temporary module directory.
	tmpDir, err := os.MkdirTemp(tempBase, "testmodule")
	if err != nil {
		t.Fatalf("failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create a go.mod file.
	if err := os.WriteFile(filepath.Join(tmpDir, "go.mod"), []byte("module testmodule"), 0o600); err != nil {
		t.Fatalf("failed to create go.mod: %v", err)
	}

	// --- Test Case 1: Unnecessary Assertion ---
	unnecessaryDir := filepath.Join(tmpDir, "unnecessary")
	if err := os.Mkdir(unnecessaryDir, 0o755); err != nil {
		t.Fatalf("failed to create unnecessary dir: %v", err)
	}
	unnecessaryCase := `
package unnecessary

type UnusedInterface interface {
	DoSomething()
}

type UnusedType struct{}

func (t *UnusedType) DoSomething() {}

var _ UnusedInterface = (*UnusedType)(nil)
`
	if err := os.WriteFile(filepath.Join(unnecessaryDir, "unnecessary.go"), []byte(unnecessaryCase), 0o600); err != nil {
		t.Fatalf("failed to write unnecessary.go: %v", err)
	}

	// --- Test Case 2: Necessary Assertion (Polymorphic usage) ---
	necessaryDir := filepath.Join(tmpDir, "necessary")
	if err := os.Mkdir(necessaryDir, 0o755); err != nil {
		t.Fatalf("failed to create necessary dir: %v", err)
	}
	necessaryCase := `
package necessary

import "fmt"

type UsedInterface interface {
	DoSomethingElse()
}

type UsedType struct{}

func (t *UsedType) DoSomethingElse() {
	fmt.Println("Hello")
}

var _ UsedInterface = (*UsedType)(nil)

func UseTheInterface(i UsedInterface) {
	i.DoSomethingElse()
}
`
	if err := os.WriteFile(filepath.Join(necessaryDir, "necessary.go"), []byte(necessaryCase), 0o600); err != nil {
		t.Fatalf("failed to write necessary.go: %v", err)
	}

	// --- Test Case 3: Necessary Assertion (Multiple Implementations) ---
	multipleDir := filepath.Join(tmpDir, "multiple")
	if err := os.Mkdir(multipleDir, 0o755); err != nil {
		t.Fatalf("failed to create multiple dir: %v", err)
	}
	multipleImplsCase := `
package multiple

type MultiInterface interface {
	DoLotsOfThings()
}

type MultiType1 struct{}
func (t *MultiType1) DoLotsOfThings() {}

type MultiType2 struct{}
func (t *MultiType2) DoLotsOfThings() {}

var _ MultiInterface = (*MultiType1)(nil)
var _ MultiInterface = (*MultiType2)(nil)
`
	if err := os.WriteFile(filepath.Join(multipleDir, "multiple.go"), []byte(multipleImplsCase), 0o600); err != nil {
		t.Fatalf("failed to write multiple.go: %v", err)
	}

	// Set the directory for package loading instead of changing working directory.
	linter.Dir = tmpDir

	issues, err := linter.Lint([]string{"./..."})
	if err != nil {
		t.Fatalf("Lint() returned error: %v", err)
	}

	// We expect only one issue from the unnecessary/unnecessary.go file.
	if len(issues) != 1 {
		t.Errorf("expected 1 issue, got %d: %v", len(issues), issues)
	}
	if len(issues) > 0 {
		if !strings.Contains(issues[0], "unnecessary/unnecessary.go") {
			t.Errorf("expected issue to contain 'unnecessary/unnecessary.go', got: %s", issues[0])
		}
		if !strings.Contains(issues[0], "Unnecessary interface assertion") {
			t.Errorf("expected issue to contain 'Unnecessary interface assertion', got: %s", issues[0])
		}
	}
}
