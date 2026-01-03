package browser

import (
	"bytes"
	"fmt"
	"log/slog"
	"text/template"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/gherkin"
)

// DefaultPromptTemplate is the standard prompt for Perplexity Comet Browser.
const DefaultPromptTemplate = `Execute the following Gherkin test scenarios as a manual QA tester using browser automation:

## Test Isolation

Before starting the test:
1. Check if you are currently logged in to the target site
2. Only log out if the test scenario requires starting from a logged-out state AND you are currently logged in
3. If the test needs a specific user account, verify you are logged in as that user (log out and re-login if needed)

{{.Content}}

After completing the test:
- Leave the browser state as-is (the next test will handle its own preconditions)

## Reporting Results

After completing the test:
1. Navigate to the GitHub issue: {{.IssueURL}}
2. Find the test entry for "{{.ID}}" ({{.Name}})
3. If ALL scenarios pass: Check the PASSED checkbox
4. If ANY scenario fails: Check the FAILED checkbox and leave a comment describing:
   - Which scenario failed
   - What was expected vs what happened
   - Any error messages or screenshots

Test ID: {{.ID}}
Feature: {{.Name}}
GitHub Issue: {{.IssueURL}}
`

// PromptData contains the data for template expansion.
type PromptData struct {
	ID       string
	Name     string
	Content  string
	IssueURL string
}

// PromptBuilder creates browser prompts for test execution.
type PromptBuilder struct {
	template *template.Template
}

// NewPromptBuilder creates a PromptBuilder with the default template.
func NewPromptBuilder() *PromptBuilder {
	tmpl := template.Must(template.New("prompt").Parse(DefaultPromptTemplate))
	return &PromptBuilder{template: tmpl}
}

// NewPromptBuilderWithTemplate creates a PromptBuilder with a custom template.
func NewPromptBuilderWithTemplate(templateStr string) (*PromptBuilder, error) {
	tmpl, err := template.New("prompt").Parse(templateStr)
	if err != nil {
		return nil, fmt.Errorf("parsing template: %w", err)
	}
	return &PromptBuilder{template: tmpl}, nil
}

// Build expands the prompt template with feature data.
// If template execution fails, it logs the error and returns an empty string.
func (b *PromptBuilder) Build(feature *gherkin.Feature, issueURL string) string {
	data := PromptData{
		ID:       feature.ID,
		Name:     feature.Name,
		Content:  feature.Content,
		IssueURL: issueURL,
	}

	var buf bytes.Buffer
	if err := b.template.Execute(&buf, data); err != nil {
		slog.Error("failed to execute prompt template", "error", err, "featureID", feature.ID)
		return ""
	}
	return buf.String()
}
