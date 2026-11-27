package runner

import (
	"encoding/json"
	"strings"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
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
	Severity    string    `json:"severity"`
	Location    ruffPoint `json:"location"`
	EndLocation ruffPoint `json:"end_location"` //nolint:tagliatelle // ruff schema
}

type ruffPoint struct {
	Row    int `json:"row"`
	Column int `json:"column"`
}

type eslintFileReport struct {
	FilePath string          `json:"filePath"`
	Messages []eslintMessage `json:"messages"`
}

type eslintMessage struct {
	RuleID    string `json:"ruleId"`
	Severity  int    `json:"severity"`
	Message   string `json:"message"`
	Line      int    `json:"line"`
	Column    int    `json:"column"`
	EndLine   int    `json:"endLine"`
	EndColumn int    `json:"endColumn"`
}

type biomeReport struct {
	Diagnostics []biomeDiagnostic `json:"diagnostics"`
}

type biomeDiagnostic struct {
	Severity string        `json:"severity"`
	Message  string        `json:"message"`
	Category string        `json:"category"`
	Location biomeLocation `json:"location"`
}

type biomeLocation struct {
	Path  string     `json:"path"`
	Start biomePoint `json:"start"`
	End   biomePoint `json:"end"`
}

type biomePoint struct {
	Line   int `json:"line"`
	Column int `json:"column"`
}

// ParseToolIssues converts a planned tool's stdout/stderr into structured issues when possible.
func ParseToolIssues(cfg executor.ToolConfig, stdout, stderr string) []config.Issue {
	if cfg.Command == "uv" && hasArg(cfg.Args, "ruff") && hasArg(cfg.Args, "check") {
		return ParseIssues("ruff", stdout, stderr)
	}
	return ParseIssues(cfg.Command, stdout, stderr)
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
	case "eslint":
		return parseEslint(payload)
	case "biome":
		return parseBiome(payload)
	default:
		return nil
	}
}

func parseGolangci(payload string) []config.Issue {
	var report golangciReport
	if !decodeJSONFrom(payload, '{', &report) {
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
	if !decodeJSONFrom(payload, '[', &items) {
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
			Severity:  parseSeverity(item.Severity),
			Message:   item.Message,
			Code:      item.Code,
			ToolName:  "ruff",
		})
	}
	return issues
}

func parseIssueJSON(payload string) []config.Issue {
	var issues []config.Issue
	if !decodeJSONFrom(payload, '[', &issues) {
		return nil
	}
	return issues
}

func parseEslint(payload string) []config.Issue {
	var reports []eslintFileReport
	if !decodeJSONFrom(payload, '[', &reports) {
		return nil
	}
	var issues []config.Issue
	for _, fileReport := range reports {
		for _, msg := range fileReport.Messages {
			if msg.Message == "" {
				continue
			}
			sev := config.SeverityError
			if msg.Severity == 1 {
				sev = config.SeverityWarning
			}
			issues = append(issues, config.Issue{
				FilePath:  fileReport.FilePath,
				Line:      msg.Line,
				Column:    msg.Column,
				EndLine:   msg.EndLine,
				EndColumn: msg.EndColumn,
				Severity:  sev,
				Message:   msg.Message,
				Code:      msg.RuleID,
				ToolName:  "eslint",
			})
		}
	}
	return issues
}

func parseBiome(payload string) []config.Issue {
	var report biomeReport
	if !decodeJSONFrom(payload, '{', &report) {
		return nil
	}
	issues := make([]config.Issue, 0, len(report.Diagnostics))
	for _, diag := range report.Diagnostics {
		if diag.Message == "" {
			continue
		}
		path := diag.Location.Path
		if path == "" {
			path = "."
		}
		issues = append(issues, config.Issue{
			FilePath:  path,
			Line:      diag.Location.Start.Line,
			Column:    diag.Location.Start.Column,
			EndLine:   diag.Location.End.Line,
			EndColumn: diag.Location.End.Column,
			Severity:  parseSeverity(diag.Severity),
			Message:   diag.Message,
			Code:      diag.Category,
			ToolName:  "biome",
		})
	}
	return issues
}

func decodeJSONFrom(payload string, startChar byte, dst any) bool {
	idx := strings.IndexByte(payload, startChar)
	if idx < 0 {
		return false
	}
	dec := json.NewDecoder(strings.NewReader(payload[idx:]))
	return dec.Decode(dst) == nil
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
