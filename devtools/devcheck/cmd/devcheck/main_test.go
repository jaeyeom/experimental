package main

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestHelpListsDocumentedFlags(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-h"}, &stdout, &stderr, executor.NewMockExecutor())
	if code != 0 {
		t.Fatalf("help exit = %d, want 0", code)
	}
	out := stdout.String() + stderr.String()
	for _, name := range []string{
		"-dry-run", "-n", "-verbose", "-v", "-filter", "-format", "-changed-only", "-force-fallback",
	} {
		if !strings.Contains(out, name) {
			t.Errorf("help missing %s\n%s", name, out)
		}
	}
	if strings.Contains(out, "-demo") {
		t.Errorf("help still advertises -demo\n%s", out)
	}
}

func TestDryRunPrintsCommandsAndExitsZero(t *testing.T) {
	dir := fixtureDir(t, "go.mod", "main.go", "MODULE.bazel", "Makefile")
	if err := os.WriteFile(filepath.Join(dir, "Makefile"), []byte("format:\nlint:\ntest:\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("make", true)

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-n", dir}, &stdout, &stderr, mock)
	if code != 0 {
		t.Fatalf("dry-run exit = %d, stderr=%s", code, stderr.String())
	}
	if len(mock.CallHistory) != 0 {
		t.Fatalf("dry-run executed %d commands", len(mock.CallHistory))
	}
	out := stdout.String()
	for _, want := range []string{
		"make format",
		"make lint",
		"make test",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("dry-run missing %q\n%s", want, out)
		}
	}
	for _, notWant := range []string{
		"bazel run //tools:format",
		"bazel run //tools:lint",
	} {
		if strings.Contains(out, notWant) {
			t.Errorf("dry-run unexpectedly listed %q\n%s", notWant, out)
		}
	}
}

func TestDryRunUsesBazelWhenFormatAndLintTargetsExist(t *testing.T) {
	dir := fixtureDir(t, "go.mod", "main.go", "MODULE.bazel", "Makefile", "tools/BUILD.bazel")
	if err := os.WriteFile(filepath.Join(dir, "Makefile"), []byte("format:\nlint:\ntest:\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "tools/BUILD.bazel"), []byte(
		"sh_binary(name = \"format\", srcs = [\"format.sh\"])\nsh_binary(name = \"lint\", srcs = [\"lint.sh\"])\n",
	), 0o600); err != nil {
		t.Fatal(err)
	}
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("bazel", true)

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-n", dir}, &stdout, &stderr, mock)
	if code != 0 {
		t.Fatalf("dry-run exit = %d, stderr=%s", code, stderr.String())
	}
	out := stdout.String()
	for _, want := range []string{
		"bazel run //tools:format",
		"bazel run //tools:lint",
		"bazel test //...",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("dry-run missing %q\n%s", want, out)
		}
	}
}

func TestFilterFormatRunsOnlyFormatters(t *testing.T) {
	dir := fixtureDir(t, "go.mod", "main.go")
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("gofumpt", true)
	mock.SetAvailableCommand("golangci-lint", true)
	mock.SetAvailableCommand("go", true)
	mock.ExpectCommand("gofumpt").
		WillReturn(&executor.ExecutionResult{
			Command: "gofumpt", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-filter=format", dir}, &stdout, &stderr, mock)
	if code != 0 {
		t.Fatalf("exit = %d, stderr=%s", code, stderr.String())
	}
	commands := executedCommands(mock)
	if len(commands) != 1 || commands[0] != "gofumpt" {
		t.Fatalf("executed %v, want [gofumpt]", commands)
	}
}

func TestFormatJSONIsValidAndIncludesToolsAndIssues(t *testing.T) {
	dir := fixtureDir(t, "go.mod", "main.go")
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("golangci-lint", true)
	mock.ExpectCommand("golangci-lint").
		WillReturn(&executor.ExecutionResult{
			Command: "golangci-lint", ExitCode: 1, Output: golangciLintJSON,
			StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-filter=lint", "-format=json", dir}, &stdout, &stderr, mock)
	if code == 0 {
		t.Fatal("lint failure exit = 0, want non-zero")
	}
	var payload struct {
		Tools  []map[string]any `json:"tools"`
		Issues []config.Issue   `json:"issues"`
	}
	if err := json.Unmarshal(stdout.Bytes(), &payload); err != nil {
		t.Fatalf("invalid JSON: %v\n%s", err, stdout.String())
	}
	if len(payload.Tools) == 0 {
		t.Fatal("JSON tools is empty")
	}
	if len(payload.Issues) != 1 || payload.Issues[0].FilePath != "broken.go" {
		t.Fatalf("JSON issues = %+v, want broken.go", payload.Issues)
	}
}

func TestLintFixtureExitsNonZeroAndNamesFile(t *testing.T) {
	dir := fixtureDir(t, "go.mod", "broken.go")
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("golangci-lint", true)
	mock.ExpectCommand("golangci-lint").
		WillReturn(&executor.ExecutionResult{
			Command: "golangci-lint", ExitCode: 1, Output: golangciLintJSON,
			StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-filter=lint", dir}, &stdout, &stderr, mock)
	if code == 0 {
		t.Fatal("lint fixture exit = 0, want non-zero")
	}
	if !strings.Contains(stdout.String(), "broken.go") {
		t.Errorf("prompt missing broken.go\n%s", stdout.String())
	}
}

func TestChangedOnlyWithoutGitErrors(t *testing.T) {
	dir := fixtureDir(t, "go.mod", "main.go")
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("gofumpt", true)

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-changed-only", "-filter=format", dir}, &stdout, &stderr, mock)
	if code == 0 {
		t.Fatal("exit = 0, want non-zero without git")
	}
	if !strings.Contains(strings.ToLower(stderr.String()+stdout.String()), "git") {
		t.Errorf("error should mention git: %s%s", stdout.String(), stderr.String())
	}
}

func TestInvalidFilter(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-filter=docs"}, &stdout, &stderr, executor.NewMockExecutor())
	if code == 0 {
		t.Fatal("invalid filter exit = 0")
	}
}

func TestInvalidFormat(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-format=xml"}, &stdout, &stderr, executor.NewMockExecutor())
	if code == 0 {
		t.Fatal("invalid format exit = 0")
	}
}

func TestFilterRepeatableAndCommaSeparated(t *testing.T) {
	dir := fixtureDir(t, "go.mod", "main.go")
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("gofumpt", true)
	mock.SetAvailableCommand("golangci-lint", true)
	mock.SetDefaultBehavior(&executor.ExecutionResult{
		Command: "ok", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now(),
	}, nil)

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"-filter=format,lint", dir}, &stdout, &stderr, mock)
	if code != 0 {
		t.Fatalf("comma filter exit = %d, stderr=%s", code, stderr.String())
	}
	got := executedCommands(mock)
	if !contains(got, "gofumpt") || !contains(got, "golangci-lint") || contains(got, "go") {
		t.Errorf("executed %v, want gofumpt and golangci-lint only", got)
	}

	mock2 := executor.NewMockExecutor()
	mock2.SetAvailableCommand("gofumpt", true)
	mock2.SetAvailableCommand("golangci-lint", true)
	mock2.SetDefaultBehavior(&executor.ExecutionResult{
		Command: "ok", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now(),
	}, nil)
	stdout.Reset()
	stderr.Reset()
	code = run(context.Background(), []string{"-filter=format", "-filter=lint", dir}, &stdout, &stderr, mock2)
	if code != 0 {
		t.Fatalf("repeatable filter exit = %d, stderr=%s", code, stderr.String())
	}
	got = executedCommands(mock2)
	if !contains(got, "gofumpt") || !contains(got, "golangci-lint") {
		t.Errorf("repeatable executed %v", got)
	}
}

const golangciLintJSON = `{
  "Issues": [
    {
      "FromLinter": "gosec",
      "Text": "Potential file inclusion via variable",
      "Severity": "error",
      "Pos": {"Filename": "broken.go", "Line": 3, "Column": 10}
    }
  ]
}`

func fixtureDir(t *testing.T, files ...string) string {
	t.Helper()
	dir := t.TempDir()
	for _, file := range files {
		path := filepath.Join(dir, file)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte("module fixture\n"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	return dir
}

func executedCommands(mock *executor.MockExecutor) []string {
	var commands []string
	for _, call := range mock.GetCallHistory() {
		commands = append(commands, call.Config.Command)
	}
	return commands
}

func contains(got []string, want string) bool {
	for _, g := range got {
		if g == want {
			return true
		}
	}
	return false
}
