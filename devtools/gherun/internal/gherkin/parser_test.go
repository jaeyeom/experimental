package gherkin

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/vars"
)

func TestExtractFeatureID(t *testing.T) {
	tests := []struct {
		name     string
		filePath string
		want     string
	}{
		{
			name:     "simple filename",
			filePath: "login.feature",
			want:     "login",
		},
		{
			name:     "with path",
			filePath: "/path/to/checkout.feature",
			want:     "checkout",
		},
		{
			name:     "with dashes",
			filePath: "user-authentication.feature",
			want:     "user-authentication",
		},
		{
			name:     "with underscores",
			filePath: "shopping_cart.feature",
			want:     "shopping_cart",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ExtractFeatureID(tt.filePath)
			if got != tt.want {
				t.Errorf("ExtractFeatureID(%q) = %q, want %q", tt.filePath, got, tt.want)
			}
		})
	}
}

func TestDefaultParser_Parse(t *testing.T) {
	// Create a temporary feature file
	content := `Feature: User Login
  As a user
  I want to log in to the application
  So that I can access my account

  Scenario: Successful login
    Given I am on the login page
    When I enter valid credentials
    Then I should see my dashboard
`

	tmpDir := t.TempDir()
	featureFile := filepath.Join(tmpDir, "login.feature")
	if err := os.WriteFile(featureFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to write test file: %v", err)
	}

	parser := NewParser()
	feature, err := parser.Parse(featureFile)
	if err != nil {
		t.Fatalf("Parse() error = %v", err)
	}

	if feature.ID != "login" {
		t.Errorf("feature.ID = %q, want %q", feature.ID, "login")
	}
	if feature.Name != "User Login" {
		t.Errorf("feature.Name = %q, want %q", feature.Name, "User Login")
	}
	// Check key content is preserved (comment stripping may affect trailing whitespace)
	if !strings.Contains(feature.Content, "Feature: User Login") {
		t.Error("Feature line should be preserved")
	}
	if !strings.Contains(feature.Content, "Given I am on the login page") {
		t.Error("Steps should be preserved")
	}
}

func TestDefaultParser_ParseAll(t *testing.T) {
	tmpDir := t.TempDir()

	// Create multiple feature files
	files := map[string]string{
		"login.feature":    "Feature: User Login\n",
		"checkout.feature": "Feature: Shopping Checkout\n",
	}

	var paths []string
	for name, content := range files {
		path := filepath.Join(tmpDir, name)
		if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
			t.Fatalf("Failed to write %s: %v", name, err)
		}
		paths = append(paths, path)
	}

	parser := NewParser()
	features, err := parser.ParseAll(paths)
	if err != nil {
		t.Fatalf("ParseAll() error = %v", err)
	}

	if len(features) != 2 {
		t.Errorf("ParseAll() returned %d features, want 2", len(features))
	}
}

func TestExtractFeatureName(t *testing.T) {
	tests := []struct {
		name    string
		content string
		want    string
	}{
		{
			name:    "simple feature",
			content: "Feature: User Login\n  Scenario: ...",
			want:    "User Login",
		},
		{
			name:    "feature with leading spaces",
			content: "  Feature: Checkout Process\n",
			want:    "Checkout Process",
		},
		{
			name:    "no feature line",
			content: "Scenario: Something\n",
			want:    "",
		},
		{
			name:    "feature with comments before",
			content: "# Comment\nFeature: Test Feature\n",
			want:    "Test Feature",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := extractFeatureName(tt.content)
			if got != tt.want {
				t.Errorf("extractFeatureName() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestDefaultParser_Parse_FailsWithUnprovidedVariables(t *testing.T) {
	// Create a feature file with variables
	content := `Feature: User Login
  Scenario: Successful login
    Given I navigate to "{{BASE_URL}}/login"
    When I enter "{{USER}}" in the email field
    Then I should see the dashboard
`

	tmpDir := t.TempDir()
	featureFile := filepath.Join(tmpDir, "login.feature")
	if err := os.WriteFile(featureFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to write test file: %v", err)
	}

	// Test with no variables provided
	t.Run("no vars provided", func(t *testing.T) {
		parser := NewParser()
		_, err := parser.Parse(featureFile)
		if err == nil {
			t.Error("Parse() expected error for feature with variables but no vars provided")
		}
		if !strings.Contains(err.Error(), "BASE_URL") || !strings.Contains(err.Error(), "USER") {
			t.Errorf("Error should mention missing variables, got: %v", err)
		}
		if !strings.Contains(err.Error(), "--var") || !strings.Contains(err.Error(), "--env-file") {
			t.Errorf("Error should mention --var or --env-file, got: %v", err)
		}
	})

	// Test with only some variables provided
	t.Run("partial vars provided", func(t *testing.T) {
		parser := NewParserWithVars(vars.Vars{"BASE_URL": "https://example.com"})
		_, err := parser.Parse(featureFile)
		if err == nil {
			t.Error("Parse() expected error when not all variables are provided")
		}
		if !strings.Contains(err.Error(), "USER") {
			t.Errorf("Error should mention missing USER variable, got: %v", err)
		}
	})

	// Test with all variables provided
	t.Run("all vars provided", func(t *testing.T) {
		parser := NewParserWithVars(vars.Vars{
			"BASE_URL": "https://example.com",
			"USER":     "testuser@example.com",
		})
		feature, err := parser.Parse(featureFile)
		if err != nil {
			t.Fatalf("Parse() unexpected error: %v", err)
		}
		if !strings.Contains(feature.Content, "https://example.com/login") {
			t.Errorf("Content should have BASE_URL substituted, got: %s", feature.Content)
		}
		if !strings.Contains(feature.Content, "testuser@example.com") {
			t.Errorf("Content should have USER substituted, got: %s", feature.Content)
		}
		// Verify no placeholders remain
		if strings.Contains(feature.Content, "{{") {
			t.Errorf("Content should not contain any placeholders, got: %s", feature.Content)
		}
	})
}

func TestDefaultParser_Parse_NoVariablesInFile(t *testing.T) {
	// Feature file without variables should work with parser that has no vars
	content := `Feature: Simple Test
  Scenario: Basic scenario
    Given I do something
    Then I see the result
`

	tmpDir := t.TempDir()
	featureFile := filepath.Join(tmpDir, "simple.feature")
	if err := os.WriteFile(featureFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to write test file: %v", err)
	}

	parser := NewParser()
	feature, err := parser.Parse(featureFile)
	if err != nil {
		t.Fatalf("Parse() unexpected error for file without variables: %v", err)
	}
	// Check key content is preserved (comment stripping may affect whitespace)
	if !strings.Contains(feature.Content, "Feature: Simple Test") {
		t.Error("Feature line should be preserved")
	}
	if !strings.Contains(feature.Content, "Given I do something") {
		t.Error("Steps should be preserved")
	}
}

func TestStripComments(t *testing.T) {
	tests := []struct {
		name    string
		content string
		want    string
	}{
		{
			name: "removes comment lines",
			content: `Feature: Test
# This is a comment
  Scenario: Test scenario
    # Another comment
    Given I do something`,
			want: `Feature: Test
  Scenario: Test scenario
    Given I do something`,
		},
		{
			name: "preserves indented comment removal",
			content: `Feature: Test
  # Indented comment
  Scenario: Test`,
			want: `Feature: Test
  Scenario: Test`,
		},
		{
			name:    "no comments",
			content: "Feature: Test\n  Scenario: Test",
			want:    "Feature: Test\n  Scenario: Test",
		},
		{
			name:    "only comments",
			content: "# Comment 1\n# Comment 2",
			want:    "",
		},
		{
			name:    "empty content",
			content: "",
			want:    "",
		},
		{
			name: "hash in middle of line preserved",
			content: `Feature: Test
  Scenario: Navigate to URL
    Given I go to "https://example.com/#/path"`,
			want: `Feature: Test
  Scenario: Navigate to URL
    Given I go to "https://example.com/#/path"`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := stripComments(tt.content)
			if got != tt.want {
				t.Errorf("stripComments() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestDefaultParser_Parse_StripsComments(t *testing.T) {
	content := `# This is a header comment
Feature: User Login
  # Description comment
  Scenario: Successful login
    Given I am on the login page
    # Step comment
    When I enter valid credentials
    Then I should see my dashboard
`

	tmpDir := t.TempDir()
	featureFile := filepath.Join(tmpDir, "login.feature")
	if err := os.WriteFile(featureFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to write test file: %v", err)
	}

	parser := NewParser()
	feature, err := parser.Parse(featureFile)
	if err != nil {
		t.Fatalf("Parse() error = %v", err)
	}

	// Verify comments are stripped
	if strings.Contains(feature.Content, "# This is a header comment") {
		t.Error("Header comment should be stripped")
	}
	if strings.Contains(feature.Content, "# Description comment") {
		t.Error("Description comment should be stripped")
	}
	if strings.Contains(feature.Content, "# Step comment") {
		t.Error("Step comment should be stripped")
	}

	// Verify feature content is preserved
	if !strings.Contains(feature.Content, "Feature: User Login") {
		t.Error("Feature line should be preserved")
	}
	if !strings.Contains(feature.Content, "Given I am on the login page") {
		t.Error("Step should be preserved")
	}
}

func TestDefaultParser_Parse_CommentedVariableIgnored(t *testing.T) {
	// A variable in a comment line should not require a value
	content := `Feature: Test
  # This line has {{UNUSED_VAR}} but it's commented out
  Scenario: Test scenario
    Given I do something
`

	tmpDir := t.TempDir()
	featureFile := filepath.Join(tmpDir, "test.feature")
	if err := os.WriteFile(featureFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to write test file: %v", err)
	}

	parser := NewParser()
	feature, err := parser.Parse(featureFile)
	if err != nil {
		t.Fatalf("Parse() should not fail for variable in comment: %v", err)
	}

	// Verify the comment line is stripped
	if strings.Contains(feature.Content, "UNUSED_VAR") {
		t.Error("Comment line with variable should be stripped")
	}
}
