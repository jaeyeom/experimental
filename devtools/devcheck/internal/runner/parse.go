package runner

import (
	"encoding/json"
	"strings"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// External tool schemas keep the vendor field names.
type golangciReport struct {
	Issues []golangciIssue `json:"Issues"` //nolint:tagliatelle // golangci-lint schema
}

type golangciIssue struct {
	FromLinter string      `json:"FromLinter"` //nolint:tagliatelle // golangci-lint schema
	Text       string      `json:"Text"`       //nolint:tagliatelle // golangci-lint schema
	Severity   string      `json:"Severity"`   //nolint:tagliatelle // golangci-lint schema
	Pos        golangciPos `json:"Pos"`        //nolint:tagliatelle // golangci-lint schema
}

type golangciPos struct {
	Filename string `json:"Filename"` //nolint:tagliatelle // golangci-lint schema
	Line     int    `json:"Line"`     //nolint:tagliatelle // golangci-lint schema
	Column   int    `json:"Column"`   //nolint:tagliatelle // golangci-lint schema
}

type ruffIssue struct {
	Code        string    `json:"code"`
	Message     string    `json:"message"`
	Filename    string    `json:"filename"`
	Location    ruffPoint `json:"location"`
	EndLocation ruffPoint `json:"end_location"` //nolint:tagliatelle // ruff schema
}

type ruffPoint struct {
	Row    int `json:"row"`
	Column int `json:"column"`
}

// ParseIssues converts tool stdout/stderr into structured issues when possible.
func ParseIssues(command, stdout, stderr string) []config.Issue {
	payload := firstNonEmpty(stdout, stderr)
	switch command {
	case "golangci-lint":
		return parseGolangci(payload)
	case "ruff":
		return parseRuff(payload)
	case "unnecessary-interface-assertion-linter":
		return parseIssueJSON(payload)
	default:
		return nil
	}
}

func parseGolangci(payload string) []config.Issue {
	var report golangciReport
	if err := json.Unmarshal([]byte(payload), &report); err != nil {
		return nil
	}
	issues := make([]config.Issue, 0, len(report.Issues))
	for _, item := range report.Issues {
		issues = append(issues, config.Issue{
			FilePath: item.Pos.Filename,
			Line:     item.Pos.Line,
			Column:   item.Pos.Column,
			Severity: parseSeverity(item.Severity),
			Message:  item.Text,
			Code:     item.FromLinter,
			ToolName: "golangci-lint",
		})
	}
	return issues
}

func parseRuff(payload string) []config.Issue {
	var items []ruffIssue
	if err := json.Unmarshal([]byte(payload), &items); err != nil {
		return nil
	}
	issues := make([]config.Issue, 0, len(items))
	for _, item := range items {
		issues = append(issues, config.Issue{
			FilePath:  item.Filename,
			Line:      item.Location.Row,
			Column:    item.Location.Column,
			EndLine:   item.EndLocation.Row,
			EndColumn: item.EndLocation.Column,
			Severity:  config.SeverityError,
			Message:   item.Message,
			Code:      item.Code,
			ToolName:  "ruff",
		})
	}
	return issues
}

func parseIssueJSON(payload string) []config.Issue {
	var issues []config.Issue
	if err := json.Unmarshal([]byte(payload), &issues); err != nil {
		return nil
	}
	return issues
}

func parseSeverity(raw string) config.Severity {
	switch strings.ToLower(raw) {
	case string(config.SeverityWarning):
		return config.SeverityWarning
	case string(config.SeverityInfo):
		return config.SeverityInfo
	case string(config.SeverityHint):
		return config.SeverityHint
	default:
		return config.SeverityError
	}
}

func firstNonEmpty(values ...string) string {
	for _, v := range values {
		if strings.TrimSpace(v) != "" {
			return v
		}
	}
	return ""
}
