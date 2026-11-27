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

const ifaceAssertFixture = `[
  {
    "filePath": "unnecessary/unnecessary.go",
    "line": 11,
    "column": 1,
    "severity": "warning",
    "message": "Unnecessary interface assertion",
    "toolName": "unnecessary-interface-assertion-linter"
  }
]`

func TestParseIssues_IfaceAssertJSON(t *testing.T) {
	issues := ParseIssues("unnecessary-interface-assertion-linter", ifaceAssertFixture, "")
	if len(issues) != 1 {
		t.Fatalf("issues = %d, want 1", len(issues))
	}
	got := issues[0]
	if got.FilePath != "unnecessary/unnecessary.go" {
		t.Errorf("FilePath = %q, want unnecessary/unnecessary.go", got.FilePath)
	}
	if got.Line != 11 || got.Column != 1 {
		t.Errorf("pos = %d:%d, want 11:1", got.Line, got.Column)
	}
	if got.Message != "Unnecessary interface assertion" {
		t.Errorf("Message = %q", got.Message)
	}
	if got.ToolName != "unnecessary-interface-assertion-linter" {
		t.Errorf("ToolName = %q", got.ToolName)
	}
	if got.Severity != config.SeverityWarning {
		t.Errorf("Severity = %q, want warning", got.Severity)
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

func TestParseIssues_GolangciLintV2TrailingStats(t *testing.T) {
	v2Out := golangciLintFixture + "\n1 issues:\n* gosec: 1\n"
	issues := ParseIssues("golangci-lint", v2Out, "")
	if len(issues) != 1 {
		t.Fatalf("issues = %d, want 1 when trailing stats present", len(issues))
	}
	if issues[0].Code != "gosec" || issues[0].FilePath != "broken.go" {
		t.Errorf("unexpected issue: %+v", issues[0])
	}
}

func TestParseIssues_RuffSeverity(t *testing.T) {
	payload := `[{"code":"W291","message":"trailing whitespace","filename":"a.py","severity":"warning","location":{"row":2,"column":5},"end_location":{"row":2,"column":6}}]`
	issues := ParseIssues("ruff", payload, "")
	if len(issues) != 1 {
		t.Fatalf("issues = %d, want 1", len(issues))
	}
	if issues[0].Severity != config.SeverityWarning {
		t.Errorf("Severity = %q, want warning", issues[0].Severity)
	}
}

func TestParseIssues_EslintJSON(t *testing.T) {
	payload := `[
  {"filePath": "/repo/ok.ts", "messages": []},
  {"filePath": "/repo/bad.ts", "messages": [
    {"ruleId": "no-unused-vars", "severity": 2, "message": "'x' is defined but never used.", "line": 3, "column": 7, "endLine": 3, "endColumn": 8},
    {"ruleId": "no-console", "severity": 1, "message": "Unexpected console statement.", "line": 5, "column": 1}
  ]}
]`
	issues := ParseIssues("eslint", payload, "")
	if len(issues) != 2 {
		t.Fatalf("issues = %d, want 2", len(issues))
	}
	if issues[0].Severity != config.SeverityError || issues[0].Code != "no-unused-vars" || issues[0].ToolName != "eslint" {
		t.Errorf("issue[0] = %+v", issues[0])
	}
	if issues[1].Severity != config.SeverityWarning || issues[1].Code != "no-console" {
		t.Errorf("issue[1] = %+v", issues[1])
	}
}

func TestParseIssues_BiomeJSON(t *testing.T) {
	payload := `The json reporter is experimental.
{"summary":{"errors":1},"diagnostics":[{"severity":"error","message":"This comparison uses the same expression.","category":"lint/suspicious/noSelfCompare","location":{"path":"bad.js","start":{"line":2,"column":5},"end":{"line":2,"column":11}}}]}
lint ━━━━━━`
	issues := ParseIssues("biome", payload, "")
	if len(issues) != 1 {
		t.Fatalf("issues = %d, want 1", len(issues))
	}
	got := issues[0]
	if got.FilePath != "bad.js" || got.Line != 2 || got.Column != 5 || got.Code != "lint/suspicious/noSelfCompare" || got.ToolName != "biome" {
		t.Errorf("got = %+v", got)
	}
}
