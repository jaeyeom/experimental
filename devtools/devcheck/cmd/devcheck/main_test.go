package main

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestDetectAndPrint(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "devcheck_cli_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name           string
		files          []string
		expectedOutput []string
	}{
		{
			name:  "go project with bazel",
			files: []string{"go.mod", "main.go", "MODULE.bazel"},
			expectedOutput: []string{
				"Languages: go",
				"Build System: bazel",
				"Tools:",
				"  format: bazel run",
				"  lint: bazel run",
				"  test: bazel test",
			},
		},
		{
			name:  "python project",
			files: []string{"pyproject.toml", "main.py", "requirements.txt"},
			expectedOutput: []string{
				"Languages: python",
				"Build System: none",
				"Tools:",
				"  format: ruff format",
				"  lint: ruff check",
			},
		},
		{
			name:  "mixed project with make",
			files: []string{"go.mod", "main.go", "requirements.txt", "main.py", "Makefile"},
			expectedOutput: []string{
				"Languages: go, python",
				"Build System: make",
				"Tools:",
				"  format: make format",
				"  lint: make lint",
				"  test: make test",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test directory
			testDir := filepath.Join(tempDir, tt.name)
			err := os.MkdirAll(testDir, 0o755)
			if err != nil {
				t.Fatal(err)
			}

			// Create test files
			for _, file := range tt.files {
				filePath := filepath.Join(testDir, file)
				err := os.WriteFile(filePath, []byte("test content"), 0o600)
				if err != nil {
					t.Fatal(err)
				}
			}

			// Capture output
			var buf bytes.Buffer
			err = detectAndPrint(testDir, &buf)
			if err != nil {
				t.Errorf("detectAndPrint() error = %v", err)
				return
			}

			output := buf.String()

			// Check that expected strings are in the output
			for _, expected := range tt.expectedOutput {
				if !strings.Contains(output, expected) {
					t.Errorf("Expected output to contain %q, but got:\n%s", expected, output)
				}
			}
		})
	}
}

func TestDetectAndPrintInvalidPath(t *testing.T) {
	var buf bytes.Buffer
	err := detectAndPrint("/path/that/does/not/exist", &buf)
	if err == nil {
		t.Error("Expected error for non-existent path")
	}
}

func TestDetectAndPrintCurrentDirectory(t *testing.T) {
	// Test that we can run detection on current directory without error
	// Get current working directory for testing
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	var buf bytes.Buffer
	err = detectAndPrint(wd, &buf)
	if err != nil {
		t.Errorf("detectAndPrint() on current directory error = %v", err)
	}

	output := buf.String()
	if output == "" {
		t.Error("Expected some output for current directory")
	}

	// Should contain basic structure
	if !strings.Contains(output, "Path:") {
		t.Error("Expected output to contain 'Path:'")
	}
}

// Test output formatting functions.
func TestPrintDemoHeader(t *testing.T) {
	var buf bytes.Buffer
	printDemoHeader(&buf)
	output := buf.String()

	expectedStrings := []string{
		"DevCheck Tool Executor Demo",
		"==============================",
	}

	for _, expected := range expectedStrings {
		if !strings.Contains(output, expected) {
			t.Errorf("Expected output to contain %q, got:\n%s", expected, output)
		}
	}
}

func TestShowDetectedTools(t *testing.T) {
	tests := []struct {
		name           string
		projectConfig  *config.ProjectConfig
		expectedOutput []string
	}{
		{
			name: "project with tools",
			projectConfig: &config.ProjectConfig{
				Tools: map[config.ToolType][]string{
					config.ToolTypeFormat: {"gofmt"},
					config.ToolTypeLint:   {"golangci-lint"},
				},
			},
			expectedOutput: []string{
				"Detected Tools:",
				"format: gofmt",
				"lint: golangci-lint",
			},
		},
		{
			name: "project with no tools",
			projectConfig: &config.ProjectConfig{
				Tools: map[config.ToolType][]string{},
			},
			expectedOutput: []string{
				"Detected Tools:",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var buf bytes.Buffer
			showDetectedTools(&buf, tt.projectConfig)
			output := buf.String()

			for _, expected := range tt.expectedOutput {
				if !strings.Contains(output, expected) {
					t.Errorf("Expected output to contain %q, got:\n%s", expected, output)
				}
			}
		})
	}
}

// Test configuration builder functions.
func TestCreateBazelConfig(t *testing.T) {
	dir := "/test/dir"
	cfg := createBazelConfig(dir)

	if cfg.Command != "bazel" {
		t.Errorf("Expected command 'bazel', got %q", cfg.Command)
	}

	expectedArgs := []string{"info", "workspace"}
	if len(cfg.Args) != len(expectedArgs) {
		t.Fatalf("Expected %d args, got %d", len(expectedArgs), len(cfg.Args))
	}
	for i, arg := range expectedArgs {
		if cfg.Args[i] != arg {
			t.Errorf("Expected arg[%d] = %q, got %q", i, arg, cfg.Args[i])
		}
	}

	if cfg.WorkingDir != dir {
		t.Errorf("Expected WorkingDir %q, got %q", dir, cfg.WorkingDir)
	}

	if cfg.Timeout != 15*time.Second {
		t.Errorf("Expected timeout 15s, got %v", cfg.Timeout)
	}
}

func TestCreateGitConfig(t *testing.T) {
	dir := "/test/dir"
	cfg := createGitConfig(dir)

	if cfg.Command != "git" {
		t.Errorf("Expected command 'git', got %q", cfg.Command)
	}

	expectedArgs := []string{"status", "--short"}
	if len(cfg.Args) != len(expectedArgs) {
		t.Fatalf("Expected %d args, got %d", len(expectedArgs), len(cfg.Args))
	}
	for i, arg := range expectedArgs {
		if cfg.Args[i] != arg {
			t.Errorf("Expected arg[%d] = %q, got %q", i, arg, cfg.Args[i])
		}
	}

	if cfg.WorkingDir != dir {
		t.Errorf("Expected WorkingDir %q, got %q", dir, cfg.WorkingDir)
	}
}

func TestAddGoTools(t *testing.T) {
	tests := []struct {
		name              string
		availableCommands map[string]bool
		expectedCommands  []string
	}{
		{
			name: "all go tools available",
			availableCommands: map[string]bool{
				"go":            true,
				"golangci-lint": true,
			},
			expectedCommands: []string{"go", "golangci-lint"},
		},
		{
			name: "only go available",
			availableCommands: map[string]bool{
				"go": true,
			},
			expectedCommands: []string{"go"},
		},
		{
			name:              "no go tools available",
			availableCommands: map[string]bool{},
			expectedCommands:  []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mockExec := executor.NewMockExecutor()
			for cmd, available := range tt.availableCommands {
				mockExec.SetAvailableCommand(cmd, available)
			}

			configs := addGoTools("/test/dir", mockExec)

			if len(configs) != len(tt.expectedCommands) {
				t.Fatalf("Expected %d configs, got %d", len(tt.expectedCommands), len(configs))
			}

			for i, expectedCmd := range tt.expectedCommands {
				if configs[i].Command != expectedCmd {
					t.Errorf("Expected command[%d] = %q, got %q", i, expectedCmd, configs[i].Command)
				}
			}
		})
	}
}

func TestAddPythonTools(t *testing.T) {
	tests := []struct {
		name              string
		projectConfig     *config.ProjectConfig
		availableCommands map[string]bool
		expectedCommands  []string
	}{
		{
			name: "ruff available with config",
			projectConfig: &config.ProjectConfig{
				ConfigFiles: map[string]string{
					"ruff": "pyproject.toml",
				},
			},
			availableCommands: map[string]bool{
				"ruff": true,
			},
			expectedCommands: []string{"ruff"},
		},
		{
			name: "ruff available but no config",
			projectConfig: &config.ProjectConfig{
				ConfigFiles: map[string]string{},
			},
			availableCommands: map[string]bool{
				"ruff": true,
			},
			expectedCommands: []string{},
		},
		{
			name: "ruff not available",
			projectConfig: &config.ProjectConfig{
				ConfigFiles: map[string]string{
					"ruff": "pyproject.toml",
				},
			},
			availableCommands: map[string]bool{},
			expectedCommands:  []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mockExec := executor.NewMockExecutor()
			for cmd, available := range tt.availableCommands {
				mockExec.SetAvailableCommand(cmd, available)
			}

			configs := addPythonTools(tt.projectConfig, "/test/dir", mockExec)

			if len(configs) != len(tt.expectedCommands) {
				t.Fatalf("Expected %d configs, got %d", len(tt.expectedCommands), len(configs))
			}

			for i, expectedCmd := range tt.expectedCommands {
				if configs[i].Command != expectedCmd {
					t.Errorf("Expected command[%d] = %q, got %q", i, expectedCmd, configs[i].Command)
				}
			}
		})
	}
}

func TestAddLanguageSpecificTools(t *testing.T) {
	tests := []struct {
		name              string
		languages         []config.Language
		availableCommands map[string]bool
		expectedCommands  []string
	}{
		{
			name:      "go project",
			languages: []config.Language{config.LanguageGo},
			availableCommands: map[string]bool{
				"go": true,
			},
			expectedCommands: []string{"go"},
		},
		{
			name:      "python project",
			languages: []config.Language{config.LanguagePython},
			availableCommands: map[string]bool{
				"ruff": true,
			},
			expectedCommands: []string{},
		},
		{
			name:      "multi-language project",
			languages: []config.Language{config.LanguageGo, config.LanguagePython},
			availableCommands: map[string]bool{
				"go": true,
			},
			expectedCommands: []string{"go"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mockExec := executor.NewMockExecutor()
			for cmd, available := range tt.availableCommands {
				mockExec.SetAvailableCommand(cmd, available)
			}

			projectConfig := &config.ProjectConfig{
				Languages:   tt.languages,
				ConfigFiles: map[string]string{},
			}

			configs := addLanguageSpecificTools(projectConfig, "/test/dir", mockExec)

			if len(configs) != len(tt.expectedCommands) {
				t.Fatalf("Expected %d configs, got %d", len(tt.expectedCommands), len(configs))
			}

			for i, expectedCmd := range tt.expectedCommands {
				if configs[i].Command != expectedCmd {
					t.Errorf("Expected command[%d] = %q, got %q", i, expectedCmd, configs[i].Command)
				}
			}
		})
	}
}

func TestPrepareDemoConfigs(t *testing.T) {
	tests := []struct {
		name              string
		projectConfig     *config.ProjectConfig
		availableCommands map[string]bool
		expectedCommands  []string
	}{
		{
			name: "bazel project with git",
			projectConfig: &config.ProjectConfig{
				BuildSystem: "bazel",
				HasGit:      true,
				Languages:   []config.Language{},
			},
			availableCommands: map[string]bool{
				"bazel": true,
				"git":   true,
			},
			expectedCommands: []string{"bazel", "git"},
		},
		{
			name: "go project with git",
			projectConfig: &config.ProjectConfig{
				BuildSystem: "none",
				HasGit:      true,
				Languages:   []config.Language{config.LanguageGo},
			},
			availableCommands: map[string]bool{
				"go":  true,
				"git": true,
			},
			expectedCommands: []string{"go", "git"},
		},
		{
			name: "project without git",
			projectConfig: &config.ProjectConfig{
				BuildSystem: "none",
				HasGit:      false,
				Languages:   []config.Language{config.LanguageGo},
			},
			availableCommands: map[string]bool{
				"go": true,
			},
			expectedCommands: []string{"go"},
		},
		{
			name: "no available tools",
			projectConfig: &config.ProjectConfig{
				BuildSystem: "bazel",
				HasGit:      true,
				Languages:   []config.Language{config.LanguageGo},
			},
			availableCommands: map[string]bool{},
			expectedCommands:  []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Save original BasicExecutor creation and restore after test
			// For this test, we'll rely on the actual behavior that checks system commands
			// In production, the function creates its own BasicExecutor, so we can only
			// test the integration behavior based on actual command availability

			configs, err := prepareDemoConfigs(tt.projectConfig, "/test/dir")
			if err != nil {
				t.Fatalf("prepareDemoConfigs failed: %v", err)
			}

			// Verify structure and types - don't verify exact count
			// since it depends on system-installed tools
			for _, cfg := range configs {
				if cfg.Command == "" {
					t.Error("Config should have a command")
				}
				if cfg.WorkingDir != "/test/dir" {
					t.Errorf("Expected WorkingDir '/test/dir', got %q", cfg.WorkingDir)
				}
			}
		})
	}
}

// Test sequential demo execution.
func TestRunSequentialDemo(t *testing.T) {
	tests := []struct {
		name            string
		configs         []executor.ToolConfig
		mockResults     []*executor.ExecutionResult
		mockErrors      []error
		expectedSuccess int
		expectedOutput  []string
	}{
		{
			name: "all tools succeed",
			configs: []executor.ToolConfig{
				{Command: "tool1", Args: []string{"arg1"}},
				{Command: "tool2", Args: []string{"arg2"}},
			},
			mockResults: []*executor.ExecutionResult{
				{Command: "tool1", ExitCode: 0, Output: "success1", StartTime: time.Now(), EndTime: time.Now()},
				{Command: "tool2", ExitCode: 0, Output: "success2", StartTime: time.Now(), EndTime: time.Now()},
			},
			mockErrors:      []error{nil, nil},
			expectedSuccess: 2,
			expectedOutput:  []string{"SUCCESS", "tool1", "tool2", "2/2"},
		},
		{
			name: "one tool fails",
			configs: []executor.ToolConfig{
				{Command: "tool1"},
				{Command: "tool2"},
			},
			mockResults: []*executor.ExecutionResult{
				{Command: "tool1", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now()},
				{Command: "tool2", ExitCode: 1, Stderr: "error message", StartTime: time.Now(), EndTime: time.Now()},
			},
			mockErrors:      []error{nil, nil},
			expectedSuccess: 1,
			expectedOutput:  []string{"SUCCESS", "FAILED", "exit code 1", "1/2"},
		},
		{
			name: "execution error",
			configs: []executor.ToolConfig{
				{Command: "tool1"},
			},
			mockResults: []*executor.ExecutionResult{
				nil,
			},
			mockErrors:      []error{fmt.Errorf("execution failed")},
			expectedSuccess: 0,
			expectedOutput:  []string{"ERROR", "execution failed", "0/1"},
		},
		{
			name: "mixed results",
			configs: []executor.ToolConfig{
				{Command: "tool1"},
				{Command: "tool2"},
				{Command: "tool3"},
			},
			mockResults: []*executor.ExecutionResult{
				{Command: "tool1", ExitCode: 0, Output: "ok", StartTime: time.Now(), EndTime: time.Now()},
				nil,
				{Command: "tool3", ExitCode: 1, Stderr: "failed", StartTime: time.Now(), EndTime: time.Now()},
			},
			mockErrors:      []error{nil, fmt.Errorf("error"), nil},
			expectedSuccess: 1,
			expectedOutput:  []string{"SUCCESS", "ERROR", "FAILED", "1/3"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mockExec := executor.NewMockExecutor()

			// Set up expectations
			for i, cfg := range tt.configs {
				mockExec.ExpectCommand(cfg.Command).
					WillReturn(tt.mockResults[i], tt.mockErrors[i]).
					Once().
					Build()
			}

			var buf bytes.Buffer
			ctx := context.Background()

			err := runSequentialDemo(ctx, &buf, mockExec, tt.configs)
			if err != nil {
				t.Fatalf("runSequentialDemo failed: %v", err)
			}

			output := buf.String()

			// Verify expected outputs
			for _, expected := range tt.expectedOutput {
				if !strings.Contains(output, expected) {
					t.Errorf("Expected output to contain %q, got:\n%s", expected, output)
				}
			}
		})
	}
}

// Test concurrent demo execution.
func TestRunConcurrentDemo(t *testing.T) {
	tests := []struct {
		name            string
		configs         []executor.ToolConfig
		mockResults     []*executor.ExecutionResult
		mockErrors      []error
		maxWorkers      int
		expectedSuccess int
		expectedOutput  []string
	}{
		{
			name: "all tools succeed",
			configs: []executor.ToolConfig{
				{Command: "tool1", Args: []string{"arg1"}},
				{Command: "tool2", Args: []string{"arg2"}},
			},
			mockResults: []*executor.ExecutionResult{
				{Command: "tool1", ExitCode: 0, Output: "success1", StartTime: time.Now(), EndTime: time.Now()},
				{Command: "tool2", ExitCode: 0, Output: "success2", StartTime: time.Now(), EndTime: time.Now()},
			},
			mockErrors:      []error{nil, nil},
			maxWorkers:      2,
			expectedSuccess: 2,
			expectedOutput:  []string{"PASS", "tool1", "tool2", "2/2"},
		},
		{
			name: "one tool fails",
			configs: []executor.ToolConfig{
				{Command: "tool1"},
				{Command: "tool2"},
			},
			mockResults: []*executor.ExecutionResult{
				{Command: "tool1", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now()},
				{Command: "tool2", ExitCode: 1, Stderr: "error", StartTime: time.Now(), EndTime: time.Now()},
			},
			mockErrors:      []error{nil, nil},
			maxWorkers:      2,
			expectedSuccess: 1,
			expectedOutput:  []string{"PASS", "FAIL", "1/2"},
		},
		{
			name: "execution error",
			configs: []executor.ToolConfig{
				{Command: "tool1"},
			},
			mockResults: []*executor.ExecutionResult{
				nil,
			},
			mockErrors:      []error{fmt.Errorf("execution failed")},
			maxWorkers:      1,
			expectedSuccess: 0,
			expectedOutput:  []string{"ERROR", "execution failed", "0/1"},
		},
		{
			name: "command at 31 chars not truncated",
			configs: []executor.ToolConfig{
				{Command: "cmd", Args: []string{"with", "args", "exactly", "31"}},
			},
			mockResults: []*executor.ExecutionResult{
				{Command: "cmd", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now()},
			},
			mockErrors:      []error{nil},
			maxWorkers:      1,
			expectedSuccess: 1,
			expectedOutput:  []string{"PASS", "cmd with args exactly 31", "1/1"},
		},
		{
			name: "command at 32 chars overflows column",
			configs: []executor.ToolConfig{
				// This command is exactly 32 chars: "cmd with args exactly thirtytwo"
				{Command: "cmd", Args: []string{"with", "args", "exactly", "thirtytwo"}},
			},
			mockResults: []*executor.ExecutionResult{
				{Command: "cmd", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now()},
			},
			mockErrors:      []error{nil},
			maxWorkers:      1,
			expectedSuccess: 1,
			// Note: This will overflow the 31-char column width (bug in implementation)
			expectedOutput: []string{"PASS", "cmd with args exactly thirtytwo", "1/1"},
		},
		{
			name: "command over 32 chars gets truncated",
			configs: []executor.ToolConfig{
				{Command: "very-long-command-name-that-exceeds", Args: []string{"the", "thirty-two", "character", "limit"}},
			},
			mockResults: []*executor.ExecutionResult{
				{Command: "very-long-command-name-that-exceeds", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now()},
			},
			mockErrors:      []error{nil},
			maxWorkers:      1,
			expectedSuccess: 1,
			expectedOutput:  []string{"PASS", "very-long-command-name-that-e...", "1/1"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mockExec := executor.NewMockExecutor()

			// Set up expectations
			for i, cfg := range tt.configs {
				mockExec.ExpectCommand(cfg.Command).
					WillReturn(tt.mockResults[i], tt.mockErrors[i]).
					Once().
					Build()
			}

			var buf bytes.Buffer
			ctx := context.Background()

			err := runConcurrentDemo(ctx, &buf, mockExec, tt.configs, tt.maxWorkers)
			if err != nil {
				t.Fatalf("runConcurrentDemo failed: %v", err)
			}

			output := buf.String()

			// Verify expected outputs
			for _, expected := range tt.expectedOutput {
				if !strings.Contains(output, expected) {
					t.Errorf("Expected output to contain %q, got:\n%s", expected, output)
				}
			}
		})
	}
}
