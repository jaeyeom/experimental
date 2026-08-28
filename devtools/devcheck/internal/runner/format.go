package runner

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

type jsonDocument struct {
	RootPath    string             `json:"rootPath"`
	BuildSystem config.BuildSystem `json:"buildSystem"`
	Languages   []config.Language  `json:"languages"`
	Tools       []jsonTool         `json:"tools"`
	Issues      []config.Issue     `json:"issues"`
	Failed      bool               `json:"failed"`
	DryRun      bool               `json:"dryRun"`
}

type jsonTool struct {
	Type      config.ToolType `json:"type"`
	Command   string          `json:"command"`
	Args      []string        `json:"args"`
	Status    string          `json:"status"`
	ExitCode  *int            `json:"exitCode,omitempty"`
	Duration  string          `json:"duration,omitempty"`
	RawOutput string          `json:"rawOutput,omitempty"`
}

// Write renders a report in the requested format.
func Write(w io.Writer, format OutputFormat, report *Report) error {
	if report == nil {
		return fmt.Errorf("report is required")
	}
	switch format {
	case FormatPrompt, "":
		return writePrompt(w, report)
	case FormatSummary:
		return writeSummary(w, report)
	case FormatJSON:
		return writeJSON(w, report)
	default:
		return fmt.Errorf("unknown output format %q (want prompt, summary, or json)", format)
	}
}

func writePrompt(w io.Writer, report *Report) error {
	if err := writePromptHeader(w, report); err != nil {
		return err
	}
	if err := writePromptTools(w, report); err != nil {
		return err
	}
	if err := writePromptIssues(w, report); err != nil {
		return err
	}
	if err := writePromptNextSteps(w, report); err != nil {
		return err
	}
	return writePromptStatus(w, report)
}

func writePromptHeader(w io.Writer, report *Report) error {
	if _, err := fmt.Fprintln(w, "🔧 DevCheck Report"); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	if _, err := fmt.Fprintln(w); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	if _, err := fmt.Fprintln(w, "Repository Analysis:"); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	project := report.Project
	build := "none"
	if project != nil && project.BuildSystem != "" {
		build = string(project.BuildSystem)
	}
	if _, err := fmt.Fprintf(w, "- Build System: %s\n", build); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	if _, err := fmt.Fprintf(w, "- Languages: %s\n", languageList(project)); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	if _, err := fmt.Fprintf(w, "- Strategy: %s\n\n", strategyLine(project, report)); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	return nil
}

func writePromptTools(w io.Writer, report *Report) error {
	heading := "Tools Executed:"
	if report.DryRun {
		heading = "Tools that would run:"
	}
	if _, err := fmt.Fprintln(w, heading); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	if len(report.Tools) == 0 {
		if _, err := fmt.Fprintln(w, "none"); err != nil {
			return fmt.Errorf("write report: %w", err)
		}
		return nil
	}
	for _, tool := range report.Tools {
		if _, err := fmt.Fprintf(w, "%s %s - %s\n", toolMark(tool), tool.Tool.Display(), toolSummary(tool)); err != nil {
			return fmt.Errorf("write report: %w", err)
		}
		if tool.RawOutput != "" {
			if _, err := fmt.Fprintf(w, "   output: %s\n", truncate(tool.RawOutput, 400)); err != nil {
				return fmt.Errorf("write report: %w", err)
			}
		}
	}
	if _, err := fmt.Fprintln(w); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	return nil
}

func writePromptIssues(w io.Writer, report *Report) error {
	if _, err := fmt.Fprintln(w, "Issues Found:"); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	if len(report.Issues) == 0 {
		if _, err := fmt.Fprintln(w, "none"); err != nil {
			return fmt.Errorf("write report: %w", err)
		}
		return nil
	}
	for _, issue := range report.Issues {
		if _, err := fmt.Fprintf(w, "📍 %s\n", formatIssue(issue)); err != nil {
			return fmt.Errorf("write report: %w", err)
		}
	}
	if _, err := fmt.Fprintln(w); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	return nil
}

func writePromptNextSteps(w io.Writer, report *Report) error {
	if _, err := fmt.Fprintln(w, "Next Steps for AI Agent:"); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	steps := nextSteps(report)
	for i, step := range steps {
		if _, err := fmt.Fprintf(w, "%d. %s\n", i+1, step); err != nil {
			return fmt.Errorf("write report: %w", err)
		}
	}
	if _, err := fmt.Fprintln(w); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	return nil
}

func writePromptStatus(w io.Writer, report *Report) error {
	status := "Status: ✅ All checks passed"
	if report.DryRun {
		status = "Status: dry-run (commands were not executed)"
	} else if report.Failed() {
		status = fmt.Sprintf("Status: 🚨 Requires attention (%d issue", len(report.Issues))
		if len(report.Issues) != 1 {
			status += "s"
		}
		status += ")"
	}
	if _, err := fmt.Fprintln(w, status); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	return nil
}

func writeSummary(w io.Writer, report *Report) error {
	passed, failed := 0, 0
	for _, tool := range report.Tools {
		if toolFailed(tool) {
			failed++
			continue
		}
		passed++
	}
	if _, err := fmt.Fprintf(w, "DevCheck: %d passed, %d failed\n", passed, failed); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	for _, tool := range report.Tools {
		mark := "ok"
		if tool.DryRun {
			mark = "dry-run"
		} else if toolFailed(tool) {
			mark = "FAIL"
		}
		if _, err := fmt.Fprintf(w, "  %s %s\n", mark, tool.Tool.Display()); err != nil {
			return fmt.Errorf("write report: %w", err)
		}
	}
	return nil
}

func writeJSON(w io.Writer, report *Report) error {
	doc := jsonDocument{
		Issues: report.Issues,
		Failed: report.Failed(),
		DryRun: report.DryRun,
	}
	if report.Project != nil {
		doc.RootPath = report.Project.RootPath
		doc.BuildSystem = report.Project.BuildSystem
		doc.Languages = report.Project.Languages
	}
	if doc.Issues == nil {
		doc.Issues = []config.Issue{}
	}
	for _, tool := range report.Tools {
		doc.Tools = append(doc.Tools, toJSONTool(tool))
	}
	if doc.Tools == nil {
		doc.Tools = []jsonTool{}
	}
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	if err := enc.Encode(doc); err != nil {
		return fmt.Errorf("encode JSON report: %w", err)
	}
	return nil
}

func toJSONTool(tool ToolResult) jsonTool {
	item := jsonTool{
		Type:      tool.Tool.Type,
		Command:   tool.Tool.Config.Command,
		Args:      tool.Tool.Config.Args,
		Status:    jsonStatus(tool),
		RawOutput: tool.RawOutput,
	}
	if tool.Result != nil {
		code := tool.Result.ExitCode
		item.ExitCode = &code
		item.Duration = tool.Result.Duration().String()
	}
	return item
}

func jsonStatus(tool ToolResult) string {
	switch {
	case tool.DryRun:
		return "dry-run"
	case tool.Error != nil:
		return "error"
	case tool.Result != nil && tool.Result.ExitCode != 0:
		return "failed"
	default:
		return "passed"
	}
}

func languageList(project *config.ProjectConfig) string {
	if project == nil || len(project.Languages) == 0 {
		return "none detected"
	}
	names := make([]string, len(project.Languages))
	for i, lang := range project.Languages {
		names[i] = string(lang)
	}
	return strings.Join(names, ", ")
}

func strategyLine(project *config.ProjectConfig, report *Report) string {
	if report.DryRun {
		return "Dry-run; commands were not executed"
	}
	if project == nil {
		return "Language-specific tools"
	}
	switch project.BuildSystem {
	case config.BuildSystemBazel:
		return "Using bazel-based tools for consistency"
	case config.BuildSystemMake:
		return "Using make-based tools for consistency"
	default:
		return "Using language-specific tools"
	}
}

func toolMark(tool ToolResult) string {
	switch {
	case tool.DryRun:
		return "⏳"
	case toolFailed(tool):
		return "❌"
	default:
		return "✅"
	}
}

func toolSummary(tool ToolResult) string {
	switch {
	case tool.DryRun:
		return "would run"
	case tool.Error != nil:
		return tool.Error.Error()
	case len(tool.Issues) == 1:
		return "Found 1 issue requiring attention"
	case len(tool.Issues) > 1:
		return fmt.Sprintf("Found %d issues requiring attention", len(tool.Issues))
	case tool.Result != nil && tool.Result.ExitCode != 0:
		return fmt.Sprintf("exit code %d", tool.Result.ExitCode)
	default:
		return "succeeded"
	}
}

func formatIssue(issue config.Issue) string {
	loc := issue.FilePath
	if issue.Line > 0 {
		loc = fmt.Sprintf("%s:%d", loc, issue.Line)
		if issue.Column > 0 {
			loc = fmt.Sprintf("%s:%d", loc, issue.Column)
		}
	}
	code := issue.Code
	if code == "" {
		code = issue.ToolName
	}
	return fmt.Sprintf("%s - %s: %s", loc, code, issue.Message)
}

func nextSteps(report *Report) []string {
	if report.DryRun {
		return []string{"Re-run without --dry-run to execute the selected tools"}
	}
	if !report.Failed() {
		return []string{"No action required"}
	}
	var steps []string
	for _, issue := range report.Issues {
		steps = append(steps, fmt.Sprintf("Fix %s in %s", issue.Code, issue.String()))
	}
	if len(steps) == 0 {
		steps = append(steps, "Inspect failed tool output and fix the reported problems")
	}
	steps = append(steps, "Re-run devcheck after fixing to confirm the issues are resolved")
	return steps
}

func truncate(s string, n int) string {
	s = strings.TrimSpace(s)
	if len(s) <= n {
		return s
	}
	return s[:n] + "..."
}
