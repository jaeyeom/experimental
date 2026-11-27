// Package runner plans, executes, and reports DevCheck tool runs.
package runner

import (
	"errors"
	"fmt"
	"path/filepath"
	"sort"
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
		matchedFiles, ok := matchingChangedFiles(spec, opts)
		if !ok {
			skippedForFiles = true
			continue
		}
		args := append([]string(nil), spec.Args...)
		cfg := buildConfig(spec, args, toolType, project.RootPath, matchedFiles, opts.ChangedOnly)
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
	if len(tried) == 0 {
		return PlannedTool{}, fmt.Errorf("no %s tool detected", toolType)
	}
	return PlannedTool{}, fmt.Errorf("missing required %s tool (tried: %s)", toolType, strings.Join(tried, ", "))
}

func matchingChangedFiles(spec config.Tool, opts Options) ([]string, bool) {
	if !opts.ChangedOnly || isBuildSystemCommand(spec.Command) {
		return nil, true
	}
	files := scopeFilesToDir(opts.ChangedFiles, spec.Dir)
	matched := filterFilesForTool(spec, files)
	return matched, len(matched) > 0
}

func scopeFilesToDir(files []string, relDir string) []string {
	if relDir == "" || relDir == "." {
		return files
	}
	prefix := filepath.ToSlash(filepath.Clean(relDir)) + "/"
	var scoped []string
	for _, file := range files {
		slash := filepath.ToSlash(filepath.Clean(file))
		if strings.HasPrefix(slash, prefix) {
			scoped = append(scoped, strings.TrimPrefix(slash, prefix))
		}
	}
	return scoped
}

func isBuildSystemCommand(command string) bool {
	return command == "bazel" || command == "bazelisk" || command == "make"
}

func buildConfig(spec config.Tool, args []string, toolType config.ToolType, rootDir string, changedFiles []string, changedOnly bool) executor.ToolConfig {
	workDir := rootDir
	if spec.Dir != "" {
		workDir = filepath.Join(rootDir, filepath.FromSlash(spec.Dir))
	}
	args = enhanceArgs(spec.Command, args, toolType, changedFiles, changedOnly)
	cfg := executor.ToolConfig{
		Command:    spec.Command,
		Args:       args,
		WorkingDir: workDir,
		Timeout:    timeoutFor(spec.Command, toolType),
	}
	if spec.Command == "bazel" || spec.Command == "bazelisk" {
		cfg.CommandBuilder = &executor.ShellCommandBuilder{}
	}
	return cfg
}

func enhanceArgs(command string, args []string, _ config.ToolType, changedFiles []string, changedOnly bool) []string {
	switch command {
	case "golangci-lint":
		args = ensureGolangciArgs(args)
	case "unnecessary-interface-assertion-linter":
		args = ensureIfaceAssertArgs(args)
	case "ruff":
		args = ensureRuffArgs(args)
	case "uv":
		if hasArg(args, "ruff") && hasArg(args, "check") {
			args = ensureRuffArgs(args)
		}
	case "gofumpt", "gofmt":
		args = defaultArgs(args, "-w", ".")
	case "go":
		args = ensureGoArgs(args)
	case "black":
		args = defaultArgs(args, ".")
	case "prettier":
		args = defaultArgs(args, "--write", ".")
	case "eslint":
		args = ensureEslintArgs(args)
	case "biome":
		args = ensureBiomeArgs(args)
	}
	if !changedOnly || len(changedFiles) == 0 {
		return args
	}
	return appendChangedFiles(command, args, changedFiles)
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
	if !hasArgPrefix(args, "--show-stats") {
		args = append(args, "--show-stats=false")
	}
	return args
}

func ensureIfaceAssertArgs(args []string) []string {
	if !hasArg(args, "--json") {
		args = append(args, "--json")
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

func ensureGoArgs(args []string) []string {
	if !hasArg(args, "test") && !hasArg(args, "vet") {
		return args
	}
	if len(args) == 1 {
		return append(args, "./...")
	}
	return args
}

func ensureEslintArgs(args []string) []string {
	if !hasArgPrefix(args, "--format") && !hasArg(args, "-f") {
		args = append(args, "--format", "json")
	}
	if !hasPositionalArg(args, "--format", "-f") {
		args = append(args, ".")
	}
	return args
}

func ensureBiomeArgs(args []string) []string {
	if hasArg(args, "format") {
		if !hasArg(args, "--write") && !hasArg(args, "--fix") {
			args = append(args, "--write")
		}
		if !hasPositionalArgAfterSubcommand(args, "format") {
			args = append(args, ".")
		}
		return args
	}
	if hasArg(args, "lint") {
		if !hasArgPrefix(args, "--reporter") {
			args = append(args, "--reporter=json")
		}
		if !hasPositionalArgAfterSubcommand(args, "lint") {
			args = append(args, ".")
		}
	}
	return args
}

func hasPositionalArg(args []string, valueFlags ...string) bool {
	skipNext := false
	for _, arg := range args {
		if skipNext {
			skipNext = false
			continue
		}
		if strings.HasPrefix(arg, "-") {
			for _, f := range valueFlags {
				if arg == f {
					skipNext = true
					break
				}
			}
			continue
		}
		return true
	}
	return false
}

func hasPositionalArgAfterSubcommand(args []string, subcommand string) bool {
	seenSub := false
	for _, arg := range args {
		if !seenSub {
			if arg == subcommand {
				seenSub = true
			}
			continue
		}
		if strings.HasPrefix(arg, "-") {
			continue
		}
		return true
	}
	return false
}

func defaultArgs(args []string, defaults ...string) []string {
	if len(args) > 0 {
		return args
	}
	return append([]string(nil), defaults...)
}

func appendChangedFiles(command string, args, changedFiles []string) []string {
	switch command {
	case "gofumpt", "gofmt", "black", "prettier", "eslint", "biome":
		return replaceDotPath(args, changedFiles)
	case "ruff", "flake8":
		return append(args, changedFiles...)
	case "uv":
		if hasArg(args, "ruff") {
			return append(args, changedFiles...)
		}
		if hasArg(args, "pytest") {
			if testFiles := pythonTestFiles(changedFiles); len(testFiles) == len(changedFiles) {
				return append(args, testFiles...)
			}
		}
		return args
	case "pytest":
		if testFiles := pythonTestFiles(changedFiles); len(testFiles) == len(changedFiles) {
			return append(args, testFiles...)
		}
		return args
	case "golangci-lint", "unnecessary-interface-assertion-linter", "go":
		if hasAnyBase(changedFiles, "go.mod", "go.sum") {
			return args
		}
		pkgs := goPackageDirs(changedFiles)
		if len(pkgs) == 0 {
			return args
		}
		if command == "go" {
			return replaceTarget(args, "./...", pkgs)
		}
		return append(args, pkgs...)
	default:
		return args
	}
}

func replaceDotPath(args, files []string) []string {
	return replaceTarget(args, ".", files)
}

func replaceTarget(args []string, target string, replacement []string) []string {
	out := make([]string, 0, len(args)+len(replacement))
	replaced := false
	for _, arg := range args {
		if arg == target {
			out = append(out, replacement...)
			replaced = true
			continue
		}
		out = append(out, arg)
	}
	if !replaced {
		out = append(out, replacement...)
	}
	return out
}

func goPackageDirs(files []string) []string {
	seen := make(map[string]struct{})
	var dirs []string
	for _, file := range files {
		if !strings.HasSuffix(file, ".go") {
			continue
		}
		dir := filepath.ToSlash(filepath.Dir(file))
		if dir != "." && !strings.HasPrefix(dir, "./") {
			dir = "./" + dir
		}
		if _, ok := seen[dir]; ok {
			continue
		}
		seen[dir] = struct{}{}
		dirs = append(dirs, dir)
	}
	sort.Strings(dirs)
	return dirs
}

func pythonTestFiles(files []string) []string {
	var out []string
	for _, file := range files {
		base := filepath.Base(file)
		if strings.HasPrefix(base, "test_") || strings.HasSuffix(base, "_test.py") {
			out = append(out, file)
		}
	}
	return out
}

func hasAnyBase(files []string, names ...string) bool {
	for _, file := range files {
		base := filepath.Base(file)
		for _, name := range names {
			if base == name {
				return true
			}
		}
	}
	return false
}

func filterFilesForTool(spec config.Tool, files []string) []string {
	var out []string
	for _, file := range files {
		if fileMatchesTool(spec, file) {
			out = append(out, file)
		}
	}
	return out
}

func fileMatchesTool(spec config.Tool, file string) bool {
	switch spec.Command {
	case "gofumpt", "gofmt":
		return strings.HasSuffix(file, ".go")
	case "golangci-lint", "unnecessary-interface-assertion-linter", "go":
		return strings.HasSuffix(file, ".go") || hasAnyBase(
			[]string{file}, "go.mod", "go.sum",
		)
	case "ruff", "black", "flake8", "pytest", "python3", "python", "uv":
		return strings.HasSuffix(file, ".py")
	case "prettier", "eslint", "biome":
		return hasAnySuffix(file, ".js", ".jsx", ".mjs", ".cjs", ".ts", ".tsx", ".mts", ".cts")
	case "npm", "pnpm", "yarn", "bun", "jest", "mocha":
		return hasAnySuffix(file, ".js", ".jsx", ".mjs", ".cjs", ".ts", ".tsx", ".mts", ".cts") ||
			hasAnyBase([]string{file}, "package.json", "tsconfig.json")
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
	if command == "bazel" || command == "bazelisk" {
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
