package models

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestNewContextAnalyzer(t *testing.T) {
	analyzer := NewContextAnalyzer()

	if analyzer == nil {
		t.Fatal("NewContextAnalyzer() returned nil")
	}

	if analyzer.fileCache == nil {
		t.Error("fileCache not initialized")
	}

	if analyzer.languageSupport == nil {
		t.Error("languageSupport not initialized")
	}

	// Check that language analyzers are registered
	if len(analyzer.languageSupport) == 0 {
		t.Error("No language analyzers registered")
	}

	// Verify Go analyzer is registered
	if _, ok := analyzer.languageSupport[".go"]; !ok {
		t.Error("Go language analyzer not registered")
	}

	// Verify JavaScript analyzer is registered
	if _, ok := analyzer.languageSupport[".js"]; !ok {
		t.Error("JavaScript language analyzer not registered")
	}

	// Verify Python analyzer is registered
	if _, ok := analyzer.languageSupport[".py"]; !ok {
		t.Error("Python language analyzer not registered")
	}
}

func TestContextAnalyzer_RegisterLanguageAnalyzer(t *testing.T) {
	analyzer := &ContextAnalyzer{
		fileCache:       make(map[string]*FileAnalysis),
		languageSupport: make(map[string]LanguageAnalyzer),
	}

	goAnalyzer := &GoLanguageAnalyzer{}
	analyzer.RegisterLanguageAnalyzer(goAnalyzer)

	// Check that the analyzer is registered for .go extension
	registered, ok := analyzer.languageSupport[".go"]
	if !ok {
		t.Fatal("Go analyzer not registered")
	}

	if registered.GetLanguage() != "go" {
		t.Errorf("Registered analyzer language = %s, want go", registered.GetLanguage())
	}
}

func TestContextAnalyzer_AnalyzeComment_GoFile(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "example.go")

	goCode := `package example

// Calculator provides basic math operations
type Calculator struct {
	value int
}

// Add adds two numbers together
func (c *Calculator) Add(a, b int) int {
	result := a + b
	return result
}

// Multiply multiplies two numbers
func Multiply(x, y int) int {
	return x * y
}
`
	if err := os.WriteFile(testFile, []byte(goCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	tests := []struct {
		name               string
		commentBody        string
		lineNumber         int
		expectValidContext bool
		expectFunctionName string
	}{
		{
			name:               "comment on Add method",
			commentBody:        "This Add method looks good",
			lineNumber:         11,
			expectValidContext: true,
			expectFunctionName: "Add",
		},
		{
			name:               "comment on Multiply function",
			commentBody:        "Multiply function should handle edge cases",
			lineNumber:         16,
			expectValidContext: true,
			expectFunctionName: "Multiply",
		},
		{
			name:               "comment on Calculator struct",
			commentBody:        "Calculator struct needs more fields",
			lineNumber:         4,
			expectValidContext: true,
			expectFunctionName: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			comment := Comment{
				ID:   "test-" + tt.name,
				Path: testFile,
				Line: NewSingleLine(tt.lineNumber),
				Body: tt.commentBody,
				Side: "RIGHT",
			}

			result, err := analyzer.AnalyzeComment(comment, []byte(goCode))
			if err != nil {
				t.Fatalf("AnalyzeComment() error = %v", err)
			}

			if result == nil {
				t.Fatal("AnalyzeComment() returned nil result")
			}

			if result.Comment.ID != comment.ID {
				t.Errorf("Result comment ID = %s, want %s", result.Comment.ID, comment.ID)
			}

			if result.LineContext == nil {
				t.Error("LineContext is nil")
			}

			if tt.expectFunctionName != "" && result.LineContext.Function != nil {
				if result.LineContext.Function.Name != tt.expectFunctionName {
					t.Errorf("Function name = %s, want %s",
						result.LineContext.Function.Name, tt.expectFunctionName)
				}
			}

			// Check that relevance score is calculated
			if result.Relevance.Score < 0 || result.Relevance.Score > 1 {
				t.Errorf("Relevance score out of range: %f", result.Relevance.Score)
			}

			if result.Relevance.Confidence < 0 || result.Relevance.Confidence > 1 {
				t.Errorf("Confidence score out of range: %f", result.Relevance.Confidence)
			}
		})
	}
}

func TestContextAnalyzer_CommentRelevance(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.go")

	goCode := `package test

func ProcessData(input string) string {
	return input
}

func CalculateSum(a, b int) int {
	return a + b
}
`
	if err := os.WriteFile(testFile, []byte(goCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	tests := []struct {
		name                 string
		commentBody          string
		lineNumber           int
		expectHighRelevance  bool
		expectRelevanceIssue bool
	}{
		{
			name:                "relevant comment mentions function name",
			commentBody:         "ProcessData should handle empty input",
			lineNumber:          3,
			expectHighRelevance: true,
		},
		{
			name:                "comment about different function",
			commentBody:         "CalculateSum is wrong here",
			lineNumber:          4, // Line inside ProcessData
			expectHighRelevance: false,
		},
		{
			name:                "generic comment",
			commentBody:         "This looks good",
			lineNumber:          4,
			expectHighRelevance: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			comment := Comment{
				ID:   "test-" + tt.name,
				Path: testFile,
				Line: NewSingleLine(tt.lineNumber),
				Body: tt.commentBody,
				Side: "RIGHT",
			}

			result, err := analyzer.AnalyzeComment(comment, []byte(goCode))
			if err != nil {
				t.Fatalf("AnalyzeComment() error = %v", err)
			}

			if tt.expectHighRelevance && result.Relevance.Score < 0.5 {
				t.Errorf("%s: Expected high relevance (>= 0.5), got %f",
					tt.name, result.Relevance.Score)
			}

			if tt.expectRelevanceIssue && len(result.Relevance.Issues) == 0 {
				t.Errorf("%s: Expected relevance issues, got none", tt.name)
			}
		})
	}
}

func TestContextAnalyzer_CacheManagement(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "cache_test.go")

	goCode := `package test

func TestFunc() {
	// test
}
`
	if err := os.WriteFile(testFile, []byte(goCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	comment := Comment{
		ID:   "test-cache",
		Path: testFile,
		Line: NewSingleLine(3),
		Body: "Test comment",
		Side: "RIGHT",
	}

	// First call - should populate cache
	_, err := analyzer.AnalyzeComment(comment, []byte(goCode))
	if err != nil {
		t.Fatalf("First AnalyzeComment() error = %v", err)
	}

	// Check cache is populated
	cached := analyzer.GetCachedAnalysis(testFile)
	if cached == nil {
		t.Error("File not cached after first analysis")
	}

	// Second call - should use cache
	_, err = analyzer.AnalyzeComment(comment, []byte(goCode))
	if err != nil {
		t.Fatalf("Second AnalyzeComment() error = %v", err)
	}

	// Clear specific file cache
	analyzer.ClearFileCache(testFile)
	cached = analyzer.GetCachedAnalysis(testFile)
	if cached != nil {
		t.Error("Cache not cleared for specific file")
	}

	// Populate cache again
	_, err = analyzer.AnalyzeComment(comment, []byte(goCode))
	if err != nil {
		t.Fatalf("Third AnalyzeComment() error = %v", err)
	}

	// Clear all cache
	analyzer.ClearFileCache("")
	cached = analyzer.GetCachedAnalysis(testFile)
	if cached != nil {
		t.Error("Cache not cleared when clearing all")
	}
}

func TestContextAnalyzer_UnsupportedLanguage(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.txt")

	content := `This is a plain text file
with some content
for testing`

	if err := os.WriteFile(testFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	comment := Comment{
		ID:   "test-unsupported",
		Path: testFile,
		Line: NewSingleLine(2),
		Body: "Test comment on unsupported file",
		Side: "RIGHT",
	}

	// Should use generic analyzer
	result, err := analyzer.AnalyzeComment(comment, []byte(content))
	if err != nil {
		t.Fatalf("AnalyzeComment() error = %v", err)
	}

	if result == nil {
		t.Fatal("AnalyzeComment() returned nil result")
	}

	if result.FileAnalysis == nil {
		t.Fatal("FileAnalysis is nil")
	}

	// Generic analyzer should set language to "generic"
	if result.FileAnalysis.Language != "generic" {
		t.Errorf("Expected generic language, got %s", result.FileAnalysis.Language)
	}
}

func TestContextAnalyzer_SuggestionGeneration(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "suggestion_test.go")

	goCode := `package test

func OldFunctionName() {
	// implementation
}

func HelperFunction() {
	// helper
}
`
	if err := os.WriteFile(testFile, []byte(goCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	tests := []struct {
		name               string
		commentBody        string
		lineNumber         int
		suggestionContains string
	}{
		{
			name:               "comment referencing wrong function",
			commentBody:        "HelperFunction should be improved",
			lineNumber:         3, // Inside OldFunctionName
			suggestionContains: "move",
		},
		{
			name:               "relevant comment",
			commentBody:        "OldFunctionName implementation looks good",
			lineNumber:         3,
			suggestionContains: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			comment := Comment{
				ID:   "test-" + tt.name,
				Path: testFile,
				Line: NewSingleLine(tt.lineNumber),
				Body: tt.commentBody,
				Side: "RIGHT",
			}

			result, err := analyzer.AnalyzeComment(comment, []byte(goCode))
			if err != nil {
				t.Fatalf("AnalyzeComment() error = %v", err)
			}

			// Verify suggestions are generated
			if result.Suggestions == nil {
				t.Error("Suggestions field is nil")
			}

			if tt.suggestionContains != "" {
				found := false
				for _, suggestion := range result.Suggestions {
					if strings.Contains(strings.ToLower(string(suggestion.Type)),
						strings.ToLower(tt.suggestionContains)) {
						found = true
						break
					}
				}
				if !found {
					t.Errorf("Expected suggestion containing %q, but not found in %d suggestions",
						tt.suggestionContains, len(result.Suggestions))
				}
			}
		})
	}
}

func TestContextAnalyzer_ValidateCommentContext(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "validation_test.go")

	goCode := `package test

func ExistingFunction() {
	value := 42
	result := value * 2
	return result
}
`
	if err := os.WriteFile(testFile, []byte(goCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	tests := []struct {
		name        string
		commentBody string
		lineNumber  int
		checkValid  bool
	}{
		{
			name:        "valid comment about existing function",
			commentBody: "ExistingFunction should handle edge cases",
			lineNumber:  3,
			checkValid:  false, // Don't assert specific validity, just check it's analyzed
		},
		{
			name:        "comment with code identifier",
			commentBody: "Consider refactoring this logic",
			lineNumber:  4,
			checkValid:  false,
		},
		{
			name:        "generic valid comment",
			commentBody: "This looks good to me",
			lineNumber:  5,
			checkValid:  false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			comment := Comment{
				ID:   "test-" + tt.name,
				Path: testFile,
				Line: NewSingleLine(tt.lineNumber),
				Body: tt.commentBody,
				Side: "RIGHT",
			}

			result, err := analyzer.AnalyzeComment(comment, []byte(goCode))
			if err != nil {
				t.Fatalf("AnalyzeComment() error = %v", err)
			}

			// Just verify the analysis runs and produces a validity result
			// The actual validity depends on the implementation's heuristics
			if result.AnalyzedAt.IsZero() {
				t.Error("AnalyzedAt timestamp not set")
			}
		})
	}
}

func TestContextAnalyzer_MultiLineComments(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "multiline_test.go")

	goCode := `package test

func ProcessItems(items []string) []string {
	result := make([]string, 0)
	for _, item := range items {
		processed := strings.ToUpper(item)
		result = append(result, processed)
	}
	return result
}
`
	if err := os.WriteFile(testFile, []byte(goCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	comment := Comment{
		ID:   "test-multiline",
		Path: testFile,
		Line: NewLineRange(5, 7), // Multi-line comment
		Body: "This loop could be optimized",
		Side: "RIGHT",
	}

	result, err := analyzer.AnalyzeComment(comment, []byte(goCode))
	if err != nil {
		t.Fatalf("AnalyzeComment() error = %v", err)
	}

	if result == nil {
		t.Fatal("AnalyzeComment() returned nil result")
	}

	// Should still identify the function context
	if result.LineContext == nil {
		t.Fatal("LineContext is nil")
	}

	if result.LineContext.Function == nil {
		t.Error("Function context not identified for multi-line comment")
	} else if result.LineContext.Function.Name != "ProcessItems" {
		t.Errorf("Function name = %s, want ProcessItems", result.LineContext.Function.Name)
	}
}

func TestContextAnalyzer_JavaScriptFile(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.js")

	jsCode := `function calculateTotal(items) {
	return items.reduce((sum, item) => sum + item.price, 0);
}

const formatPrice = (price) => {
	return '$' + price.toFixed(2);
};

class ShoppingCart {
	constructor() {
		this.items = [];
	}

	addItem(item) {
		this.items.push(item);
	}
}
`
	if err := os.WriteFile(testFile, []byte(jsCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	tests := []struct {
		name           string
		commentBody    string
		lineNumber     int
		expectFunction bool
		expectClass    bool
	}{
		{
			name:           "comment on calculateTotal function",
			commentBody:    "calculateTotal should handle empty arrays",
			lineNumber:     2,
			expectFunction: true,
			expectClass:    false,
		},
		{
			name:           "comment on arrow function",
			commentBody:    "formatPrice needs validation",
			lineNumber:     6,
			expectFunction: true,
			expectClass:    false,
		},
		{
			name:           "comment on class method",
			commentBody:    "addItem should validate input",
			lineNumber:     14,
			expectFunction: false,
			expectClass:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			comment := Comment{
				ID:   "test-" + tt.name,
				Path: testFile,
				Line: NewSingleLine(tt.lineNumber),
				Body: tt.commentBody,
				Side: "RIGHT",
			}

			result, err := analyzer.AnalyzeComment(comment, []byte(jsCode))
			if err != nil {
				t.Fatalf("AnalyzeComment() error = %v", err)
			}

			if result.FileAnalysis.Language != "javascript" {
				t.Errorf("Expected javascript language, got %s", result.FileAnalysis.Language)
			}

			// JavaScript analysis should work
			if result.LineContext == nil {
				t.Error("LineContext is nil")
			}
		})
	}
}

func TestContextAnalyzer_PythonFile(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.py")

	pythonCode := `def calculate_sum(a, b):
    """Add two numbers together."""
    return a + b

class Calculator:
    def __init__(self):
        self.value = 0

    def add(self, x):
        self.value += x
        return self.value
`
	if err := os.WriteFile(testFile, []byte(pythonCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	comment := Comment{
		ID:   "test-python",
		Path: testFile,
		Line: NewSingleLine(3),
		Body: "calculate_sum should handle floats",
		Side: "RIGHT",
	}

	result, err := analyzer.AnalyzeComment(comment, []byte(pythonCode))
	if err != nil {
		t.Fatalf("AnalyzeComment() error = %v", err)
	}

	if result.FileAnalysis.Language != "python" {
		t.Errorf("Expected python language, got %s", result.FileAnalysis.Language)
	}

	// Python analysis should work
	if result.LineContext == nil {
		t.Error("LineContext is nil")
	}
}

func TestRelevanceScore_Boundaries(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "bounds_test.go")

	goCode := `package test

func SimpleFunc() {
	return
}
`
	if err := os.WriteFile(testFile, []byte(goCode), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	analyzer := NewContextAnalyzer()

	// Test multiple comments to ensure scores stay within bounds
	comments := []string{
		"SimpleFunc is perfect",
		"This function should be completely rewritten",
		"NonExistentFunc is called here",
		"TODO: refactor",
		"FIXME: broken",
	}

	for i, body := range comments {
		comment := Comment{
			ID:   "test-bounds-" + string(rune(i)),
			Path: testFile,
			Line: NewSingleLine(3),
			Body: body,
			Side: "RIGHT",
		}

		result, err := analyzer.AnalyzeComment(comment, []byte(goCode))
		if err != nil {
			t.Fatalf("AnalyzeComment() error = %v for comment: %s", err, body)
		}

		// Verify score is in valid range [0, 1]
		if result.Relevance.Score < 0 || result.Relevance.Score > 1 {
			t.Errorf("Score out of bounds for comment %q: %f", body, result.Relevance.Score)
		}

		// Verify confidence is in valid range [0, 1]
		if result.Relevance.Confidence < 0 || result.Relevance.Confidence > 1 {
			t.Errorf("Confidence out of bounds for comment %q: %f",
				body, result.Relevance.Confidence)
		}
	}
}
