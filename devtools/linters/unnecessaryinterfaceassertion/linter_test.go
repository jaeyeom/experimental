package unnecessaryinterfaceassertion_test

import (
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/internal/analysistestenv"
	"github.com/jaeyeom/experimental/devtools/linters/unnecessaryinterfaceassertion"
)

func TestLintFlagsUnnecessaryInterfaceAssertionFixture(t *testing.T) {
	linter := unnecessaryinterfaceassertion.New()
	linter.Dir = analysistestenv.Prepare(t, filepath.Join("devtools", "linters", "unnecessaryinterfaceassertion"), "go.mod")

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
