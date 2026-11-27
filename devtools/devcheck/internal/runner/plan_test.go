package runner

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestPlan_MapsDetectedToolsToStructuredConfigs(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath:    "/repo",
		BuildSystem: config.BuildSystemBazel,
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("bazel", "run", "//tools:format"), tool("gofumpt")},
			config.ToolTypeLint:   {tool("bazel", "run", "//tools:lint"), tool("golangci-lint")},
			config.ToolTypeTest:   {tool("bazel", "test", "//..."), tool("go", "test")},
		},
	}
	exec := availableExec(t, "bazel")

	planned, err := Plan(project, Options{}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 3 {
		t.Fatalf("planned tools = %d, want 3", len(planned))
	}

	assertTool(t, planned[0], config.ToolTypeFormat, "bazel", []string{"run", "//tools:format"})
	assertTool(t, planned[1], config.ToolTypeLint, "bazel", []string{"run", "//tools:lint"})
	assertTool(t, planned[2], config.ToolTypeTest, "bazel", []string{"test", "//..."})

	for _, tool := range planned {
		if tool.Config.WorkingDir != "/repo" {
			t.Errorf("%s WorkingDir = %q, want /repo", tool.Type, tool.Config.WorkingDir)
		}
		if tool.Config.Timeout <= 0 {
			t.Errorf("%s Timeout = %v, want > 0", tool.Type, tool.Config.Timeout)
		}
		if tool.Config.CommandBuilder == nil {
			t.Errorf("%s CommandBuilder is nil, want shell builder for bazel", tool.Type)
		}
	}
}

func TestPlan_ForceFallbackSkipsBuildSystemTools(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath:    "/repo",
		BuildSystem: config.BuildSystemMake,
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("make", "format"), tool("gofumpt"), tool("gofmt")},
			config.ToolTypeLint:   {tool("make", "lint"), tool("golangci-lint")},
		},
	}
	exec := availableExec(t, "make", "gofumpt", "gofmt", "golangci-lint")

	planned, err := Plan(project, Options{ForceFallback: true}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 2 {
		t.Fatalf("planned tools = %d, want 2: %v", len(planned), displayAll(planned))
	}
	assertTool(t, planned[0], config.ToolTypeFormat, "gofumpt", []string{"-w", "."})
	assertTool(t, planned[1], config.ToolTypeLint, "golangci-lint", nil)
	if !containsAll(planned[1].Config.Args, "run") {
		t.Errorf("golangci-lint args = %v, want to include run", planned[1].Config.Args)
	}
}

func TestPlan_FilterRunsOnlySelectedTypes(t *testing.T) {
	project := bazelGoProject()
	exec := availableExec(t, "bazel")

	planned, err := Plan(project, Options{Filters: []config.ToolType{config.ToolTypeFormat}}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 1 {
		t.Fatalf("planned tools = %d, want 1", len(planned))
	}
	if planned[0].Type != config.ToolTypeFormat {
		t.Errorf("type = %s, want format", planned[0].Type)
	}
}

func TestPlan_FallsBackWhenPreferredBinaryMissing(t *testing.T) {
	project := bazelGoProject()
	exec := availableExec(t, "gofumpt", "golangci-lint", "go")

	planned, err := Plan(project, Options{}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	assertTool(t, planned[0], config.ToolTypeFormat, "gofumpt", []string{"-w", "."})
	if planned[1].Config.Command != "golangci-lint" {
		t.Errorf("lint command = %q, want golangci-lint", planned[1].Config.Command)
	}
	assertTool(t, planned[2], config.ToolTypeTest, "go", []string{"test", "./..."})
}

func TestPlan_MissingRequiredTool(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeLint: {tool("golangci-lint")},
		},
	}
	exec := availableExec(t)

	_, err := Plan(project, Options{Filters: []config.ToolType{config.ToolTypeLint}}, exec)
	if err == nil {
		t.Fatal("Plan() error = nil, want missing required tool")
	}
	if !strings.Contains(err.Error(), "golangci-lint") {
		t.Errorf("error %q, want to name golangci-lint", err)
	}
}

func TestPlan_EnhancesParserFriendlyArgs(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeLint: {tool("golangci-lint"), tool("ruff", "check")},
		},
	}
	exec := availableExec(t, "golangci-lint")

	planned, err := Plan(project, Options{}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 1 {
		t.Fatalf("planned = %d, want 1", len(planned))
	}
	args := strings.Join(planned[0].Config.Args, " ")
	if !strings.Contains(args, "run") {
		t.Errorf("args %q, want run", args)
	}
	if !strings.Contains(args, "--output.json.path") {
		t.Errorf("args %q, want golangci-lint JSON output flag", args)
	}
}

func TestPlan_EnhancesIfaceAssertJSON(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeLint: {tool("unnecessary-interface-assertion-linter")},
		},
	}
	exec := availableExec(t, "unnecessary-interface-assertion-linter")

	planned, err := Plan(project, Options{}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 1 {
		t.Fatalf("planned = %d, want 1", len(planned))
	}
	if planned[0].Config.Command != "unnecessary-interface-assertion-linter" {
		t.Errorf("command = %q", planned[0].Config.Command)
	}
	if !hasArg(planned[0].Config.Args, "--json") {
		t.Errorf("args = %v, want --json", planned[0].Config.Args)
	}
}

func TestPlan_EnhancesRuffCheckJSON(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeLint: {tool("ruff", "check")},
		},
	}
	exec := availableExec(t, "ruff")

	planned, err := Plan(project, Options{}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if !containsAll(planned[0].Config.Args, "check", "--output-format", "json") &&
		!containsAll(planned[0].Config.Args, "check", "--output-format=json") {
		t.Errorf("ruff args = %v, want check plus JSON output format", planned[0].Config.Args)
	}
}

func TestPlan_ChangedOnlySkipsToolsWithoutMatchingFiles(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("gofumpt")},
			config.ToolTypeLint:   {tool("ruff", "check")},
		},
	}
	exec := availableExec(t, "gofumpt", "ruff")

	planned, err := Plan(project, Options{
		ChangedOnly:  true,
		ChangedFiles: []string{"unused.py", "README.md"},
	}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 1 {
		t.Fatalf("planned = %v, want only ruff", displayAll(planned))
	}
	if planned[0].Config.Command != "ruff" {
		t.Errorf("command = %q, want ruff", planned[0].Config.Command)
	}
	if !containsAll(planned[0].Config.Args, "unused.py") {
		t.Errorf("args = %v, want unused.py", planned[0].Config.Args)
	}
}

func TestPlan_TimeoutPositive(t *testing.T) {
	project := bazelGoProject()
	exec := availableExec(t, "bazel")
	planned, err := Plan(project, Options{}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	for _, tool := range planned {
		if tool.Config.Timeout < time.Second {
			t.Errorf("%s timeout %v is too short", tool.Type, tool.Config.Timeout)
		}
	}
}

func TestPlan_GolangciAndEslintAndBiomeArgs(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("biome", "format", "--write")},
			config.ToolTypeLint:   {tool("eslint")},
		},
	}
	exec := availableExec(t, "biome", "eslint")

	planned, err := Plan(project, Options{}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	assertTool(t, planned[0], config.ToolTypeFormat, "biome", []string{"format", "--write", "."})
	assertTool(t, planned[1], config.ToolTypeLint, "eslint", []string{"--format", "json", "."})
}

func TestPlan_ChangedOnlyReplacesDotAndUsesGoPackages(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("prettier"), tool("gofumpt")},
			config.ToolTypeLint:   {tool("golangci-lint")},
			config.ToolTypeTest:   {tool("go", "test")},
		},
	}
	exec := availableExec(t, "prettier", "golangci-lint", "go")

	planned, err := Plan(project, Options{
		ChangedOnly:  true,
		ChangedFiles: []string{"pkg/a.go", "main.go", "web/app.ts"},
	}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 3 {
		t.Fatalf("planned = %v, want 3 tools", displayAll(planned))
	}
	// prettier must replace "." with changed JS/TS file, not keep "."
	assertTool(t, planned[0], config.ToolTypeFormat, "prettier", []string{"--write", "web/app.ts"})
	// golangci-lint and go test must receive deduplicated Go package dirs, not .go file paths
	if !containsAll(planned[1].Config.Args, ".", "./pkg") {
		t.Errorf("golangci-lint args = %v, want Go package dirs . and ./pkg", planned[1].Config.Args)
	}
	for _, arg := range planned[1].Config.Args {
		if strings.HasSuffix(arg, ".go") {
			t.Errorf("golangci-lint received file arg %q instead of package dir", arg)
		}
	}
	assertTool(t, planned[2], config.ToolTypeTest, "go", []string{"test", ".", "./pkg"})
}

func TestPlan_ChangedOnlySkipsNonMatchingTestAndPackageManagers(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("npm", "run", "format")},
			config.ToolTypeTest:   {tool("go", "test"), tool("pytest")},
		},
	}
	exec := availableExec(t, "npm", "go", "pytest")

	planned, err := Plan(project, Options{
		ChangedOnly:  true,
		ChangedFiles: []string{"README.md"},
	}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 0 {
		t.Fatalf("planned = %v, want 0 tools when only README.md changed", displayAll(planned))
	}
}

func TestPlan_ChangedOnlyPytestHandlesNonTestVsTestFiles(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeTest: {tool("pytest")},
		},
	}
	exec := availableExec(t, "pytest")

	// Non-test .py file changed: run full pytest suite instead of passing app.py (which causes exit 5).
	planned, err := Plan(project, Options{
		ChangedOnly:  true,
		ChangedFiles: []string{"app.py"},
	}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	assertTool(t, planned[0], config.ToolTypeTest, "pytest", []string{})

	// Only test files changed: pass the changed test files directly.
	planned, err = Plan(project, Options{
		ChangedOnly:  true,
		ChangedFiles: []string{"tests/test_app.py"},
	}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	assertTool(t, planned[0], config.ToolTypeTest, "pytest", []string{"tests/test_app.py"})
}

func TestPlan_NestedDirScopesWorkingDirAndChangedFiles(t *testing.T) {
	project := &config.ProjectConfig{
		RootPath: "/repo",
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeLint: {
				{Command: "pnpm", Args: []string{"run", "lint"}, Dir: "frontend"},
			},
		},
	}
	exec := availableExec(t, "pnpm")

	// Changed file outside frontend skips frontend pnpm tool.
	planned, err := Plan(project, Options{
		ChangedOnly:  true,
		ChangedFiles: []string{"backend/main.ts"},
	}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 0 {
		t.Fatalf("planned = %v, want 0 when change is outside Dir", displayAll(planned))
	}

	// Changed file inside frontend runs pnpm in /repo/frontend.
	planned, err = Plan(project, Options{
		ChangedOnly:  true,
		ChangedFiles: []string{"frontend/src/app.ts"},
	}, exec)
	if err != nil {
		t.Fatalf("Plan() error = %v", err)
	}
	if len(planned) != 1 || planned[0].Config.WorkingDir != "/repo/frontend" {
		t.Fatalf("planned = %+v, want WorkingDir /repo/frontend", planned)
	}
}

func bazelGoProject() *config.ProjectConfig {
	return &config.ProjectConfig{
		RootPath:    "/repo",
		BuildSystem: config.BuildSystemBazel,
		Tools: map[config.ToolType][]config.Tool{
			config.ToolTypeFormat: {tool("bazel", "run", "//tools:format"), tool("gofumpt"), tool("gofmt")},
			config.ToolTypeLint:   {tool("bazel", "run", "//tools:lint"), tool("golangci-lint")},
			config.ToolTypeTest:   {tool("bazel", "test", "//..."), tool("go", "test")},
		},
	}
}

func tool(command string, args ...string) config.Tool {
	return config.Tool{Command: command, Args: args}
}

func availableExec(t *testing.T, commands ...string) executor.Executor {
	t.Helper()
	mock := executor.NewMockExecutor()
	for _, cmd := range commands {
		mock.SetAvailableCommand(cmd, true)
	}
	return mock
}

func assertTool(t *testing.T, tool PlannedTool, toolType config.ToolType, command string, args []string) {
	t.Helper()
	if tool.Type != toolType {
		t.Errorf("type = %s, want %s", tool.Type, toolType)
	}
	if tool.Config.Command != command {
		t.Errorf("%s command = %q, want %q", toolType, tool.Config.Command, command)
	}
	if args != nil && !equalStrings(tool.Config.Args, args) {
		t.Errorf("%s args = %v, want %v", toolType, tool.Config.Args, args)
	}
}

func displayAll(tools []PlannedTool) []string {
	out := make([]string, len(tools))
	for i, tool := range tools {
		out[i] = tool.Display()
	}
	return out
}

func containsAll(got []string, want ...string) bool {
	set := make(map[string]struct{}, len(got))
	for _, g := range got {
		set[g] = struct{}{}
	}
	for _, w := range want {
		if _, ok := set[w]; !ok {
			return false
		}
	}
	return true
}

func equalStrings(got, want []string) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range got {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}
