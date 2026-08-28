// Package runner plans, executes, and reports DevCheck tool runs.
package runner

import (
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

var errNoMatchingFiles = errors.New("no matching changed files")

// Options controls which tools are selected and how they are run.
type Options struct {
	DryRun        bool
	Verbose       bool
	Filters       []config.ToolType
	OutputFormat  OutputFormat
	ChangedOnly   bool
	ForceFallback bool
	ChangedFiles  []string
}

// OutputFormat is the CLI report format.
type OutputFormat string

const (
	// FormatPrompt is the default LLM-oriented report.
	FormatPrompt OutputFormat = "prompt"
	// FormatSummary is a short human-readable report.
	FormatSummary OutputFormat = "summary"
	// FormatJSON is machine-readable JSON.
	FormatJSON OutputFormat = "json"
)

// PlannedTool is a detected tool mapped to a structured executor config.
type PlannedTool struct {
	Type   config.ToolType
	Config executor.ToolConfig
}

// Display returns the command line that will be executed.
func (t PlannedTool) Display() string {
	if len(t.Config.Args) == 0 {
		return t.Config.Command
	}
	return t.Config.Command + " " + strings.Join(t.Config.Args, " ")
}

// Plan selects one available tool per requested type and maps it to ToolConfig.
func Plan(project *config.ProjectConfig, opts Options, exec executor.Executor) ([]PlannedTool, error) {
	if project == nil {
		return nil, fmt.Errorf("project config is required")
	}
	types := selectedTypes(project, opts.Filters)
	planned := make([]PlannedTool, 0, len(types))
	for _, toolType := range types {
		tool, err := selectTool(project, toolType, opts, exec)
		if errors.Is(err, errNoMatchingFiles) {
			continue
		}
		if err != nil {
			return nil, err
		}
		planned = append(planned, tool)
	}
	return planned, nil
}

func selectedTypes(project *config.ProjectConfig, filters []config.ToolType) []config.ToolType {
	order := []config.ToolType{config.ToolTypeFormat, config.ToolTypeLint, config.ToolTypeTest}
	if len(filters) == 0 {
		var types []config.ToolType
		for _, toolType := range order {
			if len(project.Tools[toolType]) > 0 {
				types = append(types, toolType)
			}
		}
		return types
	}
	wanted := make(map[config.ToolType]struct{}, len(filters))
	for _, f := range filters {
		wanted[f] = struct{}{}
	}
	var types []config.ToolType
	for _, toolType := range order {
		if _, ok := wanted[toolType]; ok {
			types = append(types, toolType)
		}
	}
	return types
}

func selectTool(project *config.ProjectConfig, toolType config.ToolType, opts Options, exec executor.Executor) (PlannedTool, error) {
	candidates := project.Tools[toolType]
	var tried []string
	skippedForFiles := false
	for _, spec := range candidates {
		if opts.ForceFallback && isBuildSystemCommand(spec.Command) {
			continue
		}
		tried = append(tried, spec.String())
		if exec != nil && !exec.IsAvailable(spec.Command) {
			continue
		}
		if skipForChangedFiles(spec.Command, opts) {
			skippedForFiles = true
			continue
		}
		args := append([]string(nil), spec.Args...)
		cfg := buildConfig(spec.Command, args, toolType, project.RootPath, opts.ChangedFiles)
		return PlannedTool{Type: toolType, Config: cfg}, nil
	}
	if skippedForFiles {
		return PlannedTool{}, errNoMatchingFiles
	}
	if len(tried) == 0 {
		for _, spec := range candidates {
			tried = append(tried, spec.String())
		}
	}
	return PlannedTool{}, fmt.Errorf("missing required %s tool (tried: %s)", toolType, strings.Join(tried, ", "))
}

func skipForChangedFiles(command string, opts Options) bool {
	if !opts.ChangedOnly || !acceptsFileArgs(command) {
		return false
	}
	return len(filterFilesForCommand(command, opts.ChangedFiles)) == 0
}

func isBuildSystemCommand(command string) bool {
	return command == "bazel" || command == "make"
}

func buildConfig(command string, args []string, toolType config.ToolType, workDir string, changedFiles []string) executor.ToolConfig {
	args = enhanceArgs(command, args, toolType, changedFiles)
	cfg := executor.ToolConfig{
		Command:    command,
		Args:       args,
		WorkingDir: workDir,
		Timeout:    timeoutFor(command, toolType),
	}
	if command == "bazel" {
		cfg.CommandBuilder = &executor.ShellCommandBuilder{}
	}
	return cfg
}

func enhanceArgs(command string, args []string, toolType config.ToolType, changedFiles []string) []string {
	switch command {
	case "golangci-lint":
		args = ensureGolangciArgs(args)
	case "ruff":
		args = ensureRuffArgs(args)
	case "gofumpt", "gofmt":
		args = defaultArgs(args, "-w", ".")
	case "go":
		args = ensureGoTestArgs(args)
	case "black":
		args = defaultArgs(args, ".")
	case "prettier":
		args = defaultArgs(args, "--write", ".")
	case "eslint":
		args = defaultArgs(args, ".")
	}
	return appendChangedFiles(command, args, toolType, changedFiles)
}

func ensureGolangciArgs(args []string) []string {
	if !hasArg(args, "run") {
		args = append([]string{"run"}, args...)
	}
	if !hasArgPrefix(args, "--timeout") {
		args = append(args, "--timeout=30s")
	}
	if !hasArgPrefix(args, "--output.json.path") {
		args = append(args, "--output.json.path=stdout")
	}
	return args
}

func ensureRuffArgs(args []string) []string {
	if !hasArg(args, "check") {
		return args
	}
	if hasArgPrefix(args, "--output-format") {
		return args
	}
	return append(args, "--output-format", "json")
}

func ensureGoTestArgs(args []string) []string {
	if !hasArg(args, "test") {
		return args
	}
	if len(args) == 1 {
		return append(args, "./...")
	}
	return args
}

func defaultArgs(args []string, defaults ...string) []string {
	if len(args) > 0 {
		return args
	}
	return append([]string(nil), defaults...)
}

func appendChangedFiles(command string, args []string, _ config.ToolType, changedFiles []string) []string {
	if len(changedFiles) == 0 || !acceptsFileArgs(command) {
		return args
	}
	files := filterFilesForCommand(command, changedFiles)
	if len(files) == 0 {
		return args
	}
	if command == "gofumpt" || command == "gofmt" {
		args = replaceDotPath(args, files)
		return args
	}
	return append(args, files...)
}

func replaceDotPath(args, files []string) []string {
	out := make([]string, 0, len(args)+len(files))
	replaced := false
	for _, arg := range args {
		if arg == "." {
			out = append(out, files...)
			replaced = true
			continue
		}
		out = append(out, arg)
	}
	if !replaced {
		out = append(out, files...)
	}
	return out
}

func acceptsFileArgs(command string) bool {
	switch command {
	case "gofumpt", "gofmt", "golangci-lint", "ruff", "black", "flake8", "prettier", "eslint", "pytest":
		return true
	default:
		return false
	}
}

func filterFilesForCommand(command string, files []string) []string {
	var out []string
	for _, file := range files {
		if fileMatchesCommand(command, file) {
			out = append(out, file)
		}
	}
	return out
}

func fileMatchesCommand(command, file string) bool {
	switch command {
	case "gofumpt", "gofmt", "golangci-lint":
		return strings.HasSuffix(file, ".go")
	case "ruff", "black", "flake8", "pytest":
		return strings.HasSuffix(file, ".py")
	case "prettier", "eslint":
		return hasAnySuffix(file, ".js", ".jsx", ".ts", ".tsx")
	default:
		return true
	}
}

func hasAnySuffix(file string, suffixes ...string) bool {
	for _, suffix := range suffixes {
		if strings.HasSuffix(file, suffix) {
			return true
		}
	}
	return false
}

func hasArg(args []string, want string) bool {
	for _, arg := range args {
		if arg == want {
			return true
		}
	}
	return false
}

func hasArgPrefix(args []string, prefix string) bool {
	for _, arg := range args {
		if arg == prefix || strings.HasPrefix(arg, prefix+"=") {
			return true
		}
	}
	return false
}

func timeoutFor(command string, toolType config.ToolType) time.Duration {
	if command == "bazel" {
		if toolType == config.ToolTypeTest {
			return 10 * time.Minute
		}
		return 5 * time.Minute
	}
	if command == "make" && toolType == config.ToolTypeTest {
		return 5 * time.Minute
	}
	if command == "golangci-lint" {
		return 45 * time.Second
	}
	if toolType == config.ToolTypeTest {
		return 3 * time.Minute
	}
	return 30 * time.Second
}
