package runner

import (
	"context"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

// Report is the result of planning and optionally executing tools.
type Report struct {
	Project *config.ProjectConfig
	Tools   []ToolResult
	Issues  []config.Issue
	DryRun  bool
}

// ToolResult is the outcome of one planned tool.
type ToolResult struct {
	Tool      PlannedTool
	Result    *executor.ExecutionResult
	Error     error
	Issues    []config.Issue
	RawOutput string
	DryRun    bool
}

// Failed reports whether any executed tool failed. Dry-run never fails.
func (r *Report) Failed() bool {
	if r == nil || r.DryRun {
		return false
	}
	for _, tool := range r.Tools {
		if toolFailed(tool) {
			return true
		}
	}
	return false
}

func toolFailed(tool ToolResult) bool {
	if tool.Error != nil {
		return true
	}
	return tool.Result != nil && tool.Result.ExitCode != 0
}

// Run plans tools and executes them unless DryRun is set.
func Run(ctx context.Context, project *config.ProjectConfig, opts Options, exec executor.Executor) (*Report, error) {
	if project == nil {
		return nil, fmt.Errorf("project config is required")
	}
	if opts.ChangedOnly {
		files, err := changedFiles(ctx, project, exec)
		if err != nil {
			return nil, err
		}
		opts.ChangedFiles = files
	}

	planned, err := Plan(project, opts, exec)
	if err != nil {
		return nil, err
	}

	report := &Report{Project: project, DryRun: opts.DryRun}
	if opts.DryRun {
		for _, tool := range planned {
			report.Tools = append(report.Tools, ToolResult{Tool: tool, DryRun: true})
		}
		return report, nil
	}

	results, err := executeAll(ctx, exec, planned)
	if err != nil {
		return nil, err
	}
	for i, tool := range planned {
		item := decorateResult(tool, results[i], opts.Verbose)
		report.Tools = append(report.Tools, item)
		report.Issues = append(report.Issues, item.Issues...)
	}
	return report, nil
}

func changedFiles(ctx context.Context, project *config.ProjectConfig, exec executor.Executor) ([]string, error) {
	if !project.HasGit {
		return nil, fmt.Errorf("--changed-only requires a git repository")
	}
	if exec != nil && !exec.IsAvailable("git") {
		return nil, fmt.Errorf("--changed-only requires git")
	}
	diff, err := gitOutput(ctx, exec, project.RootPath, "diff", "--name-only", "HEAD")
	if err != nil {
		return nil, fmt.Errorf("list changed files: %w", err)
	}
	untracked, err := gitOutput(ctx, exec, project.RootPath, "ls-files", "--others", "--exclude-standard")
	if err != nil {
		return nil, fmt.Errorf("list untracked files: %w", err)
	}
	return uniqueLines(diff, untracked), nil
}

func gitOutput(ctx context.Context, exec executor.Executor, workDir string, args ...string) (string, error) {
	result, err := exec.Execute(ctx, executor.ToolConfig{
		Command:    "git",
		Args:       args,
		WorkingDir: workDir,
	})
	if err != nil {
		return "", fmt.Errorf("git %s: %w", strings.Join(args, " "), err)
	}
	if result.ExitCode != 0 {
		return "", fmt.Errorf("git %s: exit %d: %s", strings.Join(args, " "), result.ExitCode, strings.TrimSpace(result.Stderr))
	}
	return result.Output, nil
}

func uniqueLines(chunks ...string) []string {
	seen := make(map[string]struct{})
	var files []string
	for _, chunk := range chunks {
		for _, line := range strings.Split(chunk, "\n") {
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}
			if _, ok := seen[line]; ok {
				continue
			}
			seen[line] = struct{}{}
			files = append(files, line)
		}
	}
	return files
}

func executeAll(ctx context.Context, exec executor.Executor, planned []PlannedTool) ([]executor.ConcurrentResult, error) {
	configs := make([]executor.ToolConfig, len(planned))
	for i, tool := range planned {
		configs[i] = tool.Config
	}
	concurrent := executor.NewConcurrentExecutor(exec)
	concurrent.SetMaxConcurrency(3)
	results, err := concurrent.ExecuteAll(ctx, configs)
	if err != nil {
		return nil, fmt.Errorf("execute tools: %w", err)
	}
	return results, nil
}

func decorateResult(tool PlannedTool, cr executor.ConcurrentResult, verbose bool) ToolResult {
	item := ToolResult{Tool: tool, Result: cr.Result, Error: cr.Error}
	if cr.Result == nil {
		return item
	}
	item.Issues = ParseIssues(tool.Config.Command, cr.Result.Output, cr.Result.Stderr)
	if len(item.Issues) == 0 && (verbose || cr.Result.ExitCode != 0) {
		item.RawOutput = combinedOutput(cr.Result)
	}
	return item
}

func combinedOutput(result *executor.ExecutionResult) string {
	out := strings.TrimSpace(result.Output)
	errOut := strings.TrimSpace(result.Stderr)
	switch {
	case out != "" && errOut != "":
		return out + "\n" + errOut
	case errOut != "":
		return errOut
	default:
		return out
	}
}
