package runner

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

const golangciLintFixture = `{
  "Issues": [
    {
      "FromLinter": "gosec",
      "Text": "Potential file inclusion via variable",
      "Severity": "error",
      "Pos": {
        "Filename": "broken.go",
        "Offset": 120,
        "Line": 3,
        "Column": 10
      }
    }
  ]
}`

const ruffFixture = `[
  {
    "code": "F401",
    "message": "os imported but unused",
    "filename": "unused.py",
    "location": {"row": 1, "column": 8},
    "end_location": {"row": 1, "column": 11}
  }
]`

func TestParseIssues_GolangciLintJSON(t *testing.T) {
	issues := ParseIssues("golangci-lint", golangciLintFixture, "")
	if len(issues) != 1 {
		t.Fatalf("issues = %d, want 1", len(issues))
	}
	got := issues[0]
	if got.FilePath != "broken.go" {
		t.Errorf("FilePath = %q, want broken.go", got.FilePath)
	}
	if got.Line != 3 || got.Column != 10 {
		t.Errorf("pos = %d:%d, want 3:10", got.Line, got.Column)
	}
	if got.Code != "gosec" {
		t.Errorf("Code = %q, want gosec", got.Code)
	}
	if got.ToolName != "golangci-lint" {
		t.Errorf("ToolName = %q, want golangci-lint", got.ToolName)
	}
	if got.Severity != config.SeverityError {
		t.Errorf("Severity = %q, want error", got.Severity)
	}
	if got.Message == "" {
		t.Error("Message is empty")
	}
}

func TestParseIssues_RuffJSON(t *testing.T) {
	issues := ParseIssues("ruff", ruffFixture, "")
	if len(issues) != 1 {
		t.Fatalf("issues = %d, want 1", len(issues))
	}
	got := issues[0]
	if got.FilePath != "unused.py" {
		t.Errorf("FilePath = %q, want unused.py", got.FilePath)
	}
	if got.Line != 1 || got.Column != 8 {
		t.Errorf("pos = %d:%d, want 1:8", got.Line, got.Column)
	}
	if got.EndLine != 1 || got.EndColumn != 11 {
		t.Errorf("end = %d:%d, want 1:11", got.EndLine, got.EndColumn)
	}
	if got.Code != "F401" {
		t.Errorf("Code = %q, want F401", got.Code)
	}
	if got.ToolName != "ruff" {
		t.Errorf("ToolName = %q, want ruff", got.ToolName)
	}
}

func TestParseIssues_UnknownToolReturnsNone(t *testing.T) {
	issues := ParseIssues("make", "some raw lint output\nfile.go:1: boom\n", "stderr")
	if len(issues) != 0 {
		t.Fatalf("issues = %v, want none for unknown tools", issues)
	}
}

func TestParseIssues_EmptyOutput(t *testing.T) {
	if issues := ParseIssues("golangci-lint", "", ""); len(issues) != 0 {
		t.Errorf("empty golangci-lint output produced %d issues", len(issues))
	}
	if issues := ParseIssues("ruff", "[]", ""); len(issues) != 0 {
		t.Errorf("empty ruff output produced %d issues", len(issues))
	}
}

func TestParseIssues_InvalidJSONReturnsNone(t *testing.T) {
	if issues := ParseIssues("golangci-lint", "not json", "also not json"); len(issues) != 0 {
		t.Errorf("invalid JSON produced %d issues", len(issues))
	}
}

func TestParseIssues_UsesStderrWhenStdoutEmpty(t *testing.T) {
	issues := ParseIssues("ruff", "", ruffFixture)
	if len(issues) != 1 {
		t.Fatalf("issues = %d, want 1 from stderr", len(issues))
	}
	if issues[0].FilePath != "unused.py" {
		t.Errorf("FilePath = %q, want unused.py", issues[0].FilePath)
	}
}
