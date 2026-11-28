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

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestVersion(t *testing.T) {
	for _, args := range [][]string{{"version"}, {"--version"}} {
		var stdout, stderr bytes.Buffer
		code := run(context.Background(), args, &stdout, &stderr, executor.NewMockExecutor())
		if code != 0 || stderr.Len() != 0 {
			t.Fatalf("%v: exit = %d, stderr = %q", args, code, stderr.String())
		}
		if !strings.HasPrefix(stdout.String(), "devcheck ") {
			t.Errorf("%v: stdout = %q, want 'devcheck ' prefix", args, stdout.String())
		}
	}

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"version", "--json"}, &stdout, &stderr, executor.NewMockExecutor())
	if code != 0 || stderr.Len() != 0 {
		t.Fatalf("version --json: exit = %d, stderr = %q", code, stderr.String())
	}
	var vo toolcli.VersionOutput
	if err := json.Unmarshal(stdout.Bytes(), &vo); err != nil {
		t.Fatalf("decode version json: %v\n%s", err, stdout.String())
	}
	if vo.Tool != "devcheck" || vo.Version == "" {
		t.Fatalf("unexpected VersionOutput: %+v", vo)
	}
}

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
	withBinsOnPath(t, "make")
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
	withBinsOnPath(t, "bazel", "make")
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
	withBinsOnPath(t, "gofumpt", "golangci-lint", "go")
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
	withBinsOnPath(t, "golangci-lint")
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
	withBinsOnPath(t, "golangci-lint")
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
	withBinsOnPath(t, "gofumpt")
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

func TestExtraArgsExitsTwo(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"dir1", "dir2"}, &stdout, &stderr, executor.NewMockExecutor())
	if code != 2 {
		t.Fatalf("extra args exit = %d, want 2", code)
	}
}

func TestMissingDirectoryExitsOne(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{filepath.Join(t.TempDir(), "does-not-exist")}, &stdout, &stderr, executor.NewMockExecutor())
	if code != 1 {
		t.Fatalf("missing dir exit = %d, want 1", code)
	}
}

func TestFixturesAcrossAllLayoutsAndFormats(t *testing.T) {
	tests := []struct {
		name         string
		bins         []string
		files        map[string]string
		args         []string
		wantBuild    string
		wantLangs    []string
		wantCommands []string
	}{
		{
			name: "go layout dry-run summary",
			bins: []string{"gofumpt", "golangci-lint", "go"},
			files: map[string]string{
				"go.mod":  "module example.com/goapp\n",
				"main.go": "package main\nfunc main() {}\n",
			},
			args:         []string{"--dry-run", "--format=summary"},
			wantCommands: []string{"gofumpt -w .", "golangci-lint run", "go test ./..."},
		},
		{
			name: "python uv layout dry-run json",
			bins: []string{"ruff", "pytest"},
			files: map[string]string{
				"pyproject.toml": "[project]\nname = \"pyapp\"\n",
				"uv.lock":        "version = 1\n",
				"app.py":         "x = 1\n",
			},
			args:         []string{"--dry-run", "--format=json"},
			wantBuild:    "none",
			wantLangs:    []string{"python"},
			wantCommands: []string{"ruff format", "ruff check --output-format json", "pytest"},
		},
		{
			name: "js ts biome pnpm layout dry-run prompt",
			bins: []string{"pnpm", "biome", "prettier", "eslint"},
			files: map[string]string{
				"tsconfig.json":  "{}\n",
				"package.json":   `{"name":"web","scripts":{"test":"vitest"}}`,
				"pnpm-lock.yaml": "lockfileVersion: '9.0'\n",
				"biome.json":     "{}\n",
				"src/index.ts":   "export const x = 1;\n",
			},
			args:         []string{"--dry-run", "--format=prompt"},
			wantBuild:    "none",
			wantLangs:    []string{"typescript"},
			wantCommands: []string{"biome format --write .", "biome lint --reporter=json .", "pnpm test"},
		},
		{
			name: "make layout dry-run with force-fallback",
			bins: []string{"make", "ruff", "pytest"},
			files: map[string]string{
				"Makefile":       "format:\n\techo fmt\nlint:\n\techo lint\ntest:\n\techo test\n",
				"pyproject.toml": "[project]\nname = \"mkapp\"\n",
				"app.py":         "x = 1\n",
			},
			args:         []string{"--dry-run", "--force-fallback", "--format=json"},
			wantBuild:    "make",
			wantLangs:    []string{"python"},
			wantCommands: []string{"ruff format", "ruff check --output-format json", "pytest"},
		},
		{
			name: "bazel bzlmod layout dry-run",
			bins: []string{"bazel", "go"},
			files: map[string]string{
				"MODULE.bazel":      "module(name = \"bzlapp\")\n",
				"tools/BUILD.bazel": "sh_binary(name = \"format\")\nsh_binary(name = \"lint\")\n",
				"go.mod":            "module example.com/bzlapp\n",
				"main.go":           "package main\nfunc main() {}\n",
			},
			args:         []string{"--dry-run", "--format=json"},
			wantBuild:    "bazel",
			wantLangs:    []string{"go"},
			wantCommands: []string{"bazel run //tools:format", "bazel run //tools:lint", "bazel test //..."},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			withBinsOnPath(t, tt.bins...)
			dir := t.TempDir()
			for rel, content := range tt.files {
				path := filepath.Join(dir, rel)
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
					t.Fatal(err)
				}
			}
			mock := executor.NewMockExecutor()
			for _, bin := range tt.bins {
				mock.SetAvailableCommand(bin, true)
			}

			args := append(append([]string(nil), tt.args...), dir)
			var stdout, stderr bytes.Buffer
			code := run(context.Background(), args, &stdout, &stderr, mock)
			if code != 0 {
				t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
			}
			out := stdout.String()
			if contains(tt.args, "--format=json") {
				var doc struct {
					BuildSystem string   `json:"buildSystem"`
					Languages   []string `json:"languages"`
					Tools       []struct {
						Command string   `json:"command"`
						Args    []string `json:"args"`
					} `json:"tools"`
				}
				if err := json.Unmarshal([]byte(out), &doc); err != nil {
					t.Fatalf("invalid JSON: %v\n%s", err, out)
				}
				var gotCmds []string
				for _, tool := range doc.Tools {
					if len(tool.Args) == 0 {
						gotCmds = append(gotCmds, tool.Command)
					} else {
						gotCmds = append(gotCmds, tool.Command+" "+strings.Join(tool.Args, " "))
					}
				}
				for _, cmd := range tt.wantCommands {
					if !contains(gotCmds, cmd) {
						t.Errorf("JSON tools = %v, want %q", gotCmds, cmd)
					}
				}
				if tt.wantBuild != "" && doc.BuildSystem != tt.wantBuild {
					t.Errorf("JSON buildSystem = %q, want %q", doc.BuildSystem, tt.wantBuild)
				}
				for _, lang := range tt.wantLangs {
					if !contains(doc.Languages, lang) {
						t.Errorf("JSON languages = %v, want %q", doc.Languages, lang)
					}
				}
				return
			}
			for _, cmd := range tt.wantCommands {
				if !strings.Contains(out, cmd) {
					t.Errorf("output missing command %q\n%s", cmd, out)
				}
			}
			if tt.wantBuild != "" && !strings.Contains(out, tt.wantBuild) {
				t.Errorf("output missing buildSystem %q\n%s", tt.wantBuild, out)
			}
			for _, lang := range tt.wantLangs {
				if !strings.Contains(out, lang) {
					t.Errorf("output missing language %q\n%s", lang, out)
				}
			}
		})
	}
}

func TestChangedOnlyEndToEndWithMockGit(t *testing.T) {
	withBinsOnPath(t, "git", "gofumpt", "ruff", "go")
	dir := fixtureDir(t, "go.mod", "main.go", "pkg/helper.go", "app.py", ".git/HEAD")
	mock := executor.NewMockExecutor()
	for _, bin := range []string{"git", "gofumpt", "ruff", "go"} {
		mock.SetAvailableCommand(bin, true)
	}
	mock.ExpectCommand("git").
		WillReturn(&executor.ExecutionResult{
			Command: "git", ExitCode: 0, Output: "pkg/helper.go\n",
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
	mock.ExpectCommand("go").
		WillReturn(&executor.ExecutionResult{
			Command: "go", ExitCode: 0, StartTime: time.Now(), EndTime: time.Now(),
		}, nil).Once().Build()

	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"--changed-only", "--format=summary", dir}, &stdout, &stderr, mock)
	if code != 0 {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	out := stdout.String()
	if !strings.Contains(out, "gofumpt -w pkg/helper.go") {
		t.Errorf("expected gofumpt on pkg/helper.go:\n%s", out)
	}
	if !strings.Contains(out, "go test ./pkg") {
		t.Errorf("expected go test on ./pkg:\n%s", out)
	}
}

func TestFilterRepeatableAndCommaSeparated(t *testing.T) {
	withBinsOnPath(t, "gofumpt", "golangci-lint")
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

func withBinsOnPath(t *testing.T, names ...string) {
	t.Helper()
	binDir := t.TempDir()
	for _, name := range names {
		path := filepath.Join(binDir, name)
		if err := os.WriteFile(path, []byte("#!/bin/sh\n"), 0o600); err != nil {
			t.Fatal(err)
		}
		if err := os.Chmod(path, 0o755); err != nil {
			t.Fatal(err)
		}
	}
	t.Setenv("PATH", binDir)
}

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
