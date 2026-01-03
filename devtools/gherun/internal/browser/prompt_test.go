package browser

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/gherkin"
)

func TestBuildPerplexityURL(t *testing.T) {
	tests := []struct {
		name   string
		prompt string
		want   string
	}{
		{
			name:   "simple prompt",
			prompt: "hello world",
			want:   "https://www.perplexity.ai/search?q=hello+world",
		},
		{
			name:   "prompt with special chars",
			prompt: "test: feature #1 & #2",
			want:   "https://www.perplexity.ai/search?q=test%3A+feature+%231+%26+%232",
		},
		{
			name:   "prompt with newlines",
			prompt: "line1\nline2",
			want:   "https://www.perplexity.ai/search?q=line1%0Aline2",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := BuildPerplexityURL(tt.prompt)
			if got != tt.want {
				t.Errorf("BuildPerplexityURL() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestPromptBuilder_Build(t *testing.T) {
	feature := &gherkin.Feature{
		ID:   "login",
		Name: "User Login",
		Content: `Feature: User Login
  Scenario: Successful login
    Given I am on the login page
    When I enter valid credentials
    Then I should see my dashboard`,
	}

	builder := NewPromptBuilder()
	prompt := builder.Build(feature, "https://github.com/owner/repo/issues/123")

	// Check that prompt contains expected elements
	expectedParts := []string{
		"Feature: User Login",
		"Scenario: Successful login",
		"login",
		"https://github.com/owner/repo/issues/123",
		"PASSED",
		"FAILED",
		// Test isolation instructions
		"Test Isolation",
		"Check if you are currently logged in",
		"Leave the browser state as-is",
	}

	for _, expected := range expectedParts {
		if !strings.Contains(prompt, expected) {
			t.Errorf("Build() prompt missing %q", expected)
		}
	}
}

func TestPromptBuilder_CustomTemplate(t *testing.T) {
	customTemplate := "Test: {{.ID}} - {{.Name}}"
	builder, err := NewPromptBuilderWithTemplate(customTemplate)
	if err != nil {
		t.Fatalf("NewPromptBuilderWithTemplate() error = %v", err)
	}

	feature := &gherkin.Feature{
		ID:   "test",
		Name: "Test Feature",
	}

	prompt := builder.Build(feature, "https://example.com")
	expected := "Test: test - Test Feature"
	if prompt != expected {
		t.Errorf("Build() = %q, want %q", prompt, expected)
	}
}
