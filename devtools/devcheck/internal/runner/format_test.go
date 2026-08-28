package runner

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestWrite_PromptIncludesIssueLocationAndNextSteps(t *testing.T) {
	var buf bytes.Buffer
	if err := Write(&buf, FormatPrompt, lintFailureReport()); err != nil {
		t.Fatalf("Write() error = %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"DevCheck Report",
		"Build System: make",
		"Languages: go",
		"broken.go:3",
		"gosec",
		"Next Steps",
		"Requires attention",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("prompt missing %q\n%s", want, out)
		}
	}
}

func TestWrite_JSONIncludesToolsAndIssues(t *testing.T) {
	var buf bytes.Buffer
	if err := Write(&buf, FormatJSON, lintFailureReport()); err != nil {
		t.Fatalf("Write() error = %v", err)
	}
	var payload jsonReport
	if err := json.Unmarshal(buf.Bytes(), &payload); err != nil {
		t.Fatalf("invalid JSON: %v\n%s", err, buf.String())
	}
	if len(payload.Tools) == 0 {
		t.Fatal("JSON tools is empty")
	}
	if len(payload.Issues) != 1 {
		t.Fatalf("JSON issues = %d, want 1", len(payload.Issues))
	}
	if payload.Issues[0].FilePath != "broken.go" {
		t.Errorf("issue filePath = %q, want broken.go", payload.Issues[0].FilePath)
	}
	if !payload.Failed {
		t.Error("JSON failed = false, want true")
	}
}

func TestWrite_SummaryListsCommands(t *testing.T) {
	var buf bytes.Buffer
	if err := Write(&buf, FormatSummary, lintFailureReport()); err != nil {
		t.Fatalf("Write() error = %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "golangci-lint") {
		t.Errorf("summary missing command\n%s", out)
	}
	if !strings.Contains(out, "failed") && !strings.Contains(out, "FAIL") {
		t.Errorf("summary missing failure status\n%s", out)
	}
}

func TestWrite_DryRunPrintsCommands(t *testing.T) {
	report := &Report{
		Project: &config.ProjectConfig{
			BuildSystem: config.BuildSystemBazel,
			Languages:   []config.Language{config.LanguageGo},
		},
		DryRun: true,
		Tools: []ToolResult{
			{
				Tool: PlannedTool{
					Type: config.ToolTypeFormat,
					Config: executor.ToolConfig{
						Command: "bazel",
						Args:    []string{"run", "//tools:format"},
					},
				},
				DryRun: true,
			},
		},
	}
	var buf bytes.Buffer
	if err := Write(&buf, FormatPrompt, report); err != nil {
		t.Fatalf("Write() error = %v", err)
	}
	if !strings.Contains(buf.String(), "bazel run //tools:format") {
		t.Errorf("dry-run prompt missing command\n%s", buf.String())
	}
}

func TestWrite_UnknownFormat(t *testing.T) {
	var buf bytes.Buffer
	err := Write(&buf, OutputFormat("xml"), lintFailureReport())
	if err == nil {
		t.Fatal("Write() error = nil, want unknown format")
	}
}

func lintFailureReport() *Report {
	return &Report{
		Project: &config.ProjectConfig{
			RootPath:    "/repo",
			BuildSystem: config.BuildSystemMake,
			Languages:   []config.Language{config.LanguageGo},
		},
		Tools: []ToolResult{
			{
				Tool: PlannedTool{
					Type: config.ToolTypeLint,
					Config: executor.ToolConfig{
						Command: "golangci-lint",
						Args:    []string{"run", "--output.json.path=stdout"},
					},
				},
				Result: &executor.ExecutionResult{
					Command:   "golangci-lint",
					ExitCode:  1,
					StartTime: time.Now(),
					EndTime:   time.Now(),
				},
				Issues: []config.Issue{{
					FilePath: "broken.go",
					Line:     3,
					Column:   10,
					Severity: config.SeverityError,
					Message:  "Potential file inclusion via variable",
					Code:     "gosec",
					ToolName: "golangci-lint",
				}},
			},
		},
		Issues: []config.Issue{{
			FilePath: "broken.go",
			Line:     3,
			Column:   10,
			Severity: config.SeverityError,
			Message:  "Potential file inclusion via variable",
			Code:     "gosec",
			ToolName: "golangci-lint",
		}},
	}
}

type jsonReport struct {
	Tools  []json.RawMessage `json:"tools"`
	Issues []config.Issue    `json:"issues"`
	Failed bool              `json:"failed"`
}
