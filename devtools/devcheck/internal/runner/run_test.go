package runner

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestRun_DryRunDoesNotExecute(t *testing.T) {
	project := bazelGoProject()
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("bazel", true)
	mock.DefaultResult = &executor.ExecutionResult{
		Command:   "bazel",
		ExitCode:  0,
		StartTime: time.Now(),
		EndTime:   time.Now(),
	}

	report, err := Run(context.Background(), project, Options{DryRun: true}, mock)
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if len(mock.CallHistory) != 0 {
		t.Fatalf("dry-run executed %d commands, want 0", len(mock.CallHistory))
	}
	if len(report.Tools) != 3 {
		t.Fatalf("tools = %d, want 3", len(report.Tools))
	}
	for _, tool := range report.Tools {
		if !tool.DryRun {
			t.Errorf("%s DryRun = false, want true", tool.Tool.Type)
		}
		if !strings.Contains(tool.Tool.Display(), "bazel") {
			t.Errorf("display %q, want bazel command", tool.Tool.Display())
		}
	}
	if report.Failed() {
		t.Error("dry-run report should not fail")
	}
}

func TestRun_RecordsSuccessAndFailure(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("gofumpt")},
			config.ToolTypeLint:   {tool("golangci-lint")},
		},
	}
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("gofumpt", true)
	mock.SetAvailableCommand("golangci-lint", true)
	mock.ExpectCommand("gofumpt").
		WillReturn(&executor.ExecutionResult{
			Command: "gofumpt", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()
	mock.ExpectCommand("golangci-lint").
		WillReturn(&executor.ExecutionResult{
			Command:   "golangci-lint",
			ExitCode:  1,
			Output:    golangciLintFixture,
			StartTime: time.Now(),
			EndTime:   time.Now(),
		}, nil).Once().Build()

	report, err := Run(context.Background(), project, Options{}, mock)
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if !report.Failed() {
		t.Fatal("Failed() = false, want true on lint failure")
	}
	if len(report.Issues) != 1 {
		t.Fatalf("issues = %d, want 1", len(report.Issues))
	}
	if report.Issues[0].FilePath != "broken.go" {
		t.Errorf("issue file = %q, want broken.go", report.Issues[0].FilePath)
	}
}

func TestRun_ChangedOnlyRequiresGit(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		HasGit:   false,
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("gofumpt")},
		},
	}
	mock := availableExec(t, "gofumpt").(*executor.MockExecutor)

	_, err := Run(context.Background(), project, Options{ChangedOnly: true}, mock)
	if err == nil {
		t.Fatal("Run() error = nil, want git repo required")
	}
	if !strings.Contains(strings.ToLower(err.Error()), "git") {
		t.Errorf("error %q, want to mention git", err)
	}
}

func TestRun_ChangedOnlyAppendsMatchingFiles(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		HasGit:   true,
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("gofumpt")},
		},
	}
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("gofumpt", true)
	mock.SetAvailableCommand("git", true)
	mock.ExpectCommand("git").
		WillReturn(&executor.ExecutionResult{
			Command: "git", ExitCode: 0, Output: "main.go\nREADME.md\n",
			StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()
	mock.ExpectCommand("git").
		WillReturn(&executor.ExecutionResult{
			Command: "git", ExitCode: 0, Output: "",
			StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()
	mock.ExpectCommand("gofumpt").
		WillReturn(&executor.ExecutionResult{
			Command: "gofumpt", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()

	report, err := Run(context.Background(), project, Options{ChangedOnly: true}, mock)
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if len(report.Tools) != 1 {
		t.Fatalf("tools = %d, want 1", len(report.Tools))
	}
	if !containsAll(report.Tools[0].Tool.Config.Args, "main.go") {
		t.Errorf("args = %v, want main.go", report.Tools[0].Tool.Config.Args)
	}
	if containsAll(report.Tools[0].Tool.Config.Args, "README.md") {
		t.Errorf("args = %v, README.md should not be passed to gofumpt", report.Tools[0].Tool.Config.Args)
	}
}

func TestRun_AttachesRawOutputWhenUnparsed(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeLint: {tool("make", "lint")},
		},
	}
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("make", true)
	mock.ExpectCommand("make").
		WillReturn(&executor.ExecutionResult{
			Command: "make", ExitCode: 1, Output: "lint failed on foo.go",
			Stderr: "error details", StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()

	report, err := Run(context.Background(), project, Options{}, mock)
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if len(report.Issues) != 0 {
		t.Errorf("parsed issues = %d, want 0 for make", len(report.Issues))
	}
	if !strings.Contains(report.Tools[0].RawOutput, "lint failed on foo.go") {
		t.Errorf("RawOutput = %q, want make stdout", report.Tools[0].RawOutput)
	}
}

func TestRun_ExecutionErrorFailsReport(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("gofumpt")},
		},
	}
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("gofumpt", true)
	mock.ExpectCommand("gofumpt").
		WillReturn(nil, context.DeadlineExceeded).Once().Build()

	report, err := Run(context.Background(), project, Options{}, mock)
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if !report.Failed() {
		t.Fatal("Failed() = false, want true on execution error")
	}
	if report.Tools[0].Error == nil {
		t.Fatal("tool error is nil")
	}
}
