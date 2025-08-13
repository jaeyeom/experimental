package models

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"
	"time"
)

// ContextAnalyzer provides advanced context analysis for better comment relevance.
type ContextAnalyzer struct {
	fileCache       map[string]*FileAnalysis
	languageSupport map[string]LanguageAnalyzer
}

// FileAnalysis represents the analyzed structure of a file.
type FileAnalysis struct {
	FilePath     string              `json:"filePath"`
	Language     string              `json:"language"`
	Functions    []FunctionInfo      `json:"functions"`
	Classes      []ClassInfo         `json:"classes"`
	Imports      []ImportInfo        `json:"imports"`
	Variables    []VariableInfo      `json:"variables"`
	Comments     []CodeComment       `json:"comments"`
	Dependencies []string            `json:"dependencies"`
	LastModified time.Time           `json:"lastModified"`
	LineMapping  map[int]CodeContext `json:"lineMapping"`
}

// FunctionInfo represents information about a function.
type FunctionInfo struct {
	Name       string                 `json:"name"`
	StartLine  int                    `json:"startLine"`
	EndLine    int                    `json:"endLine"`
	Parameters []ParameterInfo        `json:"parameters"`
	ReturnType string                 `json:"returnType"`
	Visibility string                 `json:"visibility"` // public, private, protected
	IsMethod   bool                   `json:"isMethod"`
	ClassName  string                 `json:"className,omitempty"`
	Complexity int                    `json:"complexity"`
	CallSites  []int                  `json:"callSites"`
	Metadata   map[string]interface{} `json:"metadata,omitempty"`
}

// ClassInfo represents information about a class or struct.
type ClassInfo struct {
	Name        string                 `json:"name"`
	StartLine   int                    `json:"startLine"`
	EndLine     int                    `json:"endLine"`
	Methods     []FunctionInfo         `json:"methods"`
	Fields      []VariableInfo         `json:"fields"`
	Extends     string                 `json:"extends,omitempty"`
	Implements  []string               `json:"implements,omitempty"`
	Visibility  string                 `json:"visibility"`
	IsInterface bool                   `json:"isInterface"`
	Metadata    map[string]interface{} `json:"metadata,omitempty"`
}

// ImportInfo represents an import statement.
type ImportInfo struct {
	Package   string `json:"package"`
	Alias     string `json:"alias,omitempty"`
	StartLine int    `json:"startLine"`
	IsUsed    bool   `json:"isUsed"`
}

// VariableInfo represents a variable declaration.
type VariableInfo struct {
	Name       string `json:"name"`
	Type       string `json:"type"`
	StartLine  int    `json:"startLine"`
	Scope      string `json:"scope"` // global, function, class
	IsConstant bool   `json:"isConstant"`
}

// CodeComment represents a comment in the code.
type CodeComment struct {
	Text      string `json:"text"`
	StartLine int    `json:"startLine"`
	EndLine   int    `json:"endLine"`
	Type      string `json:"type"` // line, block, doc
}

// CodeContext represents the context at a specific line.
type CodeContext struct {
	Line             int                    `json:"line"`
	Function         *FunctionInfo          `json:"function,omitempty"`
	Class            *ClassInfo             `json:"class,omitempty"`
	Scope            string                 `json:"scope"`
	CodeType         string                 `json:"codeType"` // declaration, assignment, call, etc.
	SurroundingLines []string               `json:"surroundingLines"`
	Metadata         map[string]interface{} `json:"metadata,omitempty"`
}

// ParameterInfo represents function parameter information.
type ParameterInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// LanguageAnalyzer defines the interface for language-specific analysis.
type LanguageAnalyzer interface {
	AnalyzeFile(filePath string, content []byte) (*FileAnalysis, error)
	GetLanguage() string
	SupportedExtensions() []string
}

// NewContextAnalyzer creates a new context analyzer.
func NewContextAnalyzer() *ContextAnalyzer {
	analyzer := &ContextAnalyzer{
		fileCache:       make(map[string]*FileAnalysis),
		languageSupport: make(map[string]LanguageAnalyzer),
	}

	// Register language analyzers
	goAnalyzer := &GoLanguageAnalyzer{}
	analyzer.RegisterLanguageAnalyzer(goAnalyzer)

	jsAnalyzer := &JavaScriptAnalyzer{}
	analyzer.RegisterLanguageAnalyzer(jsAnalyzer)

	pythonAnalyzer := &PythonAnalyzer{}
	analyzer.RegisterLanguageAnalyzer(pythonAnalyzer)

	return analyzer
}

// RegisterLanguageAnalyzer registers a language-specific analyzer.
func (ca *ContextAnalyzer) RegisterLanguageAnalyzer(analyzer LanguageAnalyzer) {
	for _, ext := range analyzer.SupportedExtensions() {
		ca.languageSupport[ext] = analyzer
	}
}

// AnalyzeComment analyzes the context of a comment to determine its relevance.
func (ca *ContextAnalyzer) AnalyzeComment(comment Comment, fileContent []byte) (*CommentAnalysisResult, error) {
	// Get or create file analysis
	fileAnalysis, err := ca.getFileAnalysis(comment.Path, fileContent)
	if err != nil {
		return nil, fmt.Errorf("failed to analyze file: %w", err)
	}

	// Get line context
	lineContext := ca.getLineContext(fileAnalysis, comment.Line)

	// Analyze comment relevance
	relevance := ca.calculateCommentRelevance(comment, lineContext, fileAnalysis)

	// Detect if comment is still valid
	isValid := ca.validateCommentContext(comment, lineContext, fileAnalysis)

	// Generate suggestions
	suggestions := ca.generateContextSuggestions(comment, lineContext, fileAnalysis)

	result := &CommentAnalysisResult{
		Comment:      comment,
		LineContext:  lineContext,
		Relevance:    relevance,
		IsValid:      isValid,
		Suggestions:  suggestions,
		AnalyzedAt:   time.Now(),
		FileAnalysis: fileAnalysis,
	}

	return result, nil
}

// CommentAnalysisResult represents the result of comment context analysis.
type CommentAnalysisResult struct {
	Comment      Comment             `json:"comment"`
	LineContext  *CodeContext        `json:"lineContext"`
	Relevance    RelevanceScore      `json:"relevance"`
	IsValid      bool                `json:"isValid"`
	Suggestions  []ContextSuggestion `json:"suggestions"`
	AnalyzedAt   time.Time           `json:"analyzedAt"`
	FileAnalysis *FileAnalysis       `json:"fileAnalysis,omitempty"`
}

// RelevanceScore represents how relevant a comment is to its current location.
type RelevanceScore struct {
	Score      float64  `json:"score"`      // 0.0 to 1.0
	Confidence float64  `json:"confidence"` // 0.0 to 1.0
	Factors    []string `json:"factors"`    // Contributing factors
	Issues     []string `json:"issues"`     // Problems found
}

// ContextSuggestion represents a suggestion for improving comment placement or content.
type ContextSuggestion struct {
	Type        SuggestionType `json:"type"`
	Description string         `json:"description"`
	NewLine     *int           `json:"newLine,omitempty"`
	NewContent  *string        `json:"newContent,omitempty"`
	Confidence  float64        `json:"confidence"`
}

// SuggestionType represents the type of context suggestion.
type SuggestionType string

const (
	SuggestionMove    SuggestionType = "move"    // Move comment to different line
	SuggestionUpdate  SuggestionType = "update"  // Update comment content
	SuggestionRemove  SuggestionType = "remove"  // Remove comment (no longer relevant)
	SuggestionSplit   SuggestionType = "split"   // Split comment across multiple lines
	SuggestionMerge   SuggestionType = "merge"   // Merge with nearby comment
	SuggestionEnhance SuggestionType = "enhance" // Add more context to comment
)

// getFileAnalysis retrieves or creates file analysis.
func (ca *ContextAnalyzer) getFileAnalysis(filePath string, content []byte) (*FileAnalysis, error) {
	// Check cache first
	if analysis, exists := ca.fileCache[filePath]; exists {
		return analysis, nil
	}

	// Determine language
	ext := strings.ToLower(filepath.Ext(filePath))
	analyzer, exists := ca.languageSupport[ext]
	if !exists {
		// Use generic analyzer for unsupported languages
		analyzer = &GenericAnalyzer{}
	}

	// Analyze file
	analysis, err := analyzer.AnalyzeFile(filePath, content)
	if err != nil {
		return nil, fmt.Errorf("failed to analyze file %s: %w", filePath, err)
	}

	// Cache result
	ca.fileCache[filePath] = analysis

	return analysis, nil
}

// getLineContext gets the context for a specific line.
func (ca *ContextAnalyzer) getLineContext(fileAnalysis *FileAnalysis, lineNumber int) *CodeContext {
	if context, exists := fileAnalysis.LineMapping[lineNumber]; exists {
		return &context
	}

	// Create basic context if not found in mapping
	context := &CodeContext{
		Line:     lineNumber,
		Scope:    "unknown",
		CodeType: "unknown",
	}

	// Try to infer context from surrounding structures
	for _, function := range fileAnalysis.Functions {
		if lineNumber >= function.StartLine && lineNumber <= function.EndLine {
			context.Function = &function
			context.Scope = "function"
			break
		}
	}

	for _, class := range fileAnalysis.Classes {
		if lineNumber >= class.StartLine && lineNumber <= class.EndLine {
			context.Class = &class
			if context.Scope == "unknown" {
				context.Scope = "class"
			}
			break
		}
	}

	return context
}

// calculateCommentRelevance calculates how relevant a comment is to its current location.
func (ca *ContextAnalyzer) calculateCommentRelevance(comment Comment, lineContext *CodeContext, fileAnalysis *FileAnalysis) RelevanceScore {
	score := 0.5 // Start with neutral score
	confidence := 0.8
	var factors []string
	var issues []string

	// Check if comment is still in the same function
	if lineContext.Function != nil {
		if ca.commentReferencesFunction(comment, lineContext.Function) {
			score += 0.2
			factors = append(factors, "Comment references current function")
		}
	} else if ca.commentLooksLikeFunctionComment(comment) {
		score -= 0.3
		issues = append(issues, "Function comment not associated with any function")
	}

	// Check if comment is still in the same class
	if lineContext.Class != nil {
		if ca.commentReferencesClass(comment, lineContext.Class) {
			score += 0.15
			factors = append(factors, "Comment references current class")
		}
	}

	// Check for code movement indicators
	if ca.detectCodeMovement(comment, lineContext, fileAnalysis) {
		score -= 0.4
		issues = append(issues, "Code appears to have moved from original location")
	}

	// Check for obsolete references
	if ca.detectObsoleteReferences(comment, fileAnalysis) {
		score -= 0.3
		issues = append(issues, "Comment contains obsolete references")
	}

	// Normalize score
	if score > 1.0 {
		score = 1.0
	}
	if score < 0.0 {
		score = 0.0
	}

	return RelevanceScore{
		Score:      score,
		Confidence: confidence,
		Factors:    factors,
		Issues:     issues,
	}
}

// validateCommentContext validates if a comment is still contextually valid.
func (ca *ContextAnalyzer) validateCommentContext(comment Comment, lineContext *CodeContext, fileAnalysis *FileAnalysis) bool {
	// Check if comment references non-existent code elements
	if ca.referencesNonExistentElements(comment, fileAnalysis) {
		return false
	}

	// Check if comment is in completely wrong context
	if ca.isInWrongContext(comment, lineContext) {
		return false
	}

	return true
}

// generateContextSuggestions generates suggestions for improving comment context.
func (ca *ContextAnalyzer) generateContextSuggestions(comment Comment, lineContext *CodeContext, fileAnalysis *FileAnalysis) []ContextSuggestion {
	var suggestions []ContextSuggestion

	// Suggest moving comment if it belongs elsewhere
	if betterLine := ca.findBetterLocationForComment(comment, fileAnalysis); betterLine != nil {
		suggestions = append(suggestions, ContextSuggestion{
			Type:        SuggestionMove,
			Description: fmt.Sprintf("Consider moving comment to line %d where the referenced code is located", *betterLine),
			NewLine:     betterLine,
			Confidence:  0.8,
		})
	}

	// Suggest updating comment if it contains outdated information
	if outdatedInfo := ca.detectOutdatedInformation(comment, fileAnalysis); len(outdatedInfo) > 0 {
		suggestions = append(suggestions, ContextSuggestion{
			Type:        SuggestionUpdate,
			Description: fmt.Sprintf("Comment contains outdated information: %s", strings.Join(outdatedInfo, ", ")),
			Confidence:  0.7,
		})
	}

	// Suggest removing comment if it's no longer relevant
	if !ca.validateCommentContext(comment, lineContext, fileAnalysis) {
		suggestions = append(suggestions, ContextSuggestion{
			Type:        SuggestionRemove,
			Description: "Comment appears to be no longer relevant to the current code",
			Confidence:  0.6,
		})
	}

	return suggestions
}

// Helper methods for context analysis

func (ca *ContextAnalyzer) commentReferencesFunction(comment Comment, function *FunctionInfo) bool {
	commentText := strings.ToLower(comment.Body)
	functionName := strings.ToLower(function.Name)

	// Simple keyword matching - could be enhanced with NLP
	return strings.Contains(commentText, functionName) ||
		strings.Contains(commentText, "function") ||
		strings.Contains(commentText, "method")
}

func (ca *ContextAnalyzer) commentReferencesClass(comment Comment, class *ClassInfo) bool {
	commentText := strings.ToLower(comment.Body)
	className := strings.ToLower(class.Name)

	return strings.Contains(commentText, className) ||
		strings.Contains(commentText, "class") ||
		strings.Contains(commentText, "struct")
}

func (ca *ContextAnalyzer) commentLooksLikeFunctionComment(comment Comment) bool {
	commentText := strings.ToLower(comment.Body)

	functionKeywords := []string{"function", "method", "returns", "parameters", "args"}
	for _, keyword := range functionKeywords {
		if strings.Contains(commentText, keyword) {
			return true
		}
	}

	return false
}

func (ca *ContextAnalyzer) detectCodeMovement(comment Comment, lineContext *CodeContext, fileAnalysis *FileAnalysis) bool {
	// Simple heuristic: if comment mentions a function name that's not in current context
	commentText := strings.ToLower(comment.Body)

	for _, function := range fileAnalysis.Functions {
		functionName := strings.ToLower(function.Name)
		if strings.Contains(commentText, functionName) {
			// Check if we're not in this function
			if lineContext.Function == nil || strings.ToLower(lineContext.Function.Name) != functionName {
				return true
			}
		}
	}

	return false
}

func (ca *ContextAnalyzer) detectObsoleteReferences(comment Comment, fileAnalysis *FileAnalysis) bool {
	// Check for references to variables or functions that no longer exist
	commentText := strings.ToLower(comment.Body)

	// Extract potential identifiers from comment (simple regex)
	identifierRegex := regexp.MustCompile(`\b[a-zA-Z_][a-zA-Z0-9_]*\b`)
	identifiers := identifierRegex.FindAllString(commentText, -1)

	for _, identifier := range identifiers {
		if ca.isLikelyCodeIdentifier(identifier) && !ca.identifierExistsInFile(identifier, fileAnalysis) {
			return true
		}
	}

	return false
}

func (ca *ContextAnalyzer) isLikelyCodeIdentifier(identifier string) bool {
	// Skip common English words and very short identifiers
	commonWords := []string{"the", "and", "or", "but", "is", "are", "was", "were", "be", "been", "have", "has", "had", "do", "does", "did", "will", "would", "should", "could", "can", "may", "might", "must", "this", "that", "these", "those", "a", "an"}

	identifier = strings.ToLower(identifier)
	if len(identifier) < 3 {
		return false
	}

	for _, word := range commonWords {
		if identifier == word {
			return false
		}
	}

	// Look for camelCase or snake_case patterns
	return strings.Contains(identifier, "_") ||
		(len(identifier) > 3 && strings.ToLower(identifier) != identifier)
}

func (ca *ContextAnalyzer) identifierExistsInFile(identifier string, fileAnalysis *FileAnalysis) bool {
	identifier = strings.ToLower(identifier)

	// Check functions
	for _, function := range fileAnalysis.Functions {
		if strings.ToLower(function.Name) == identifier {
			return true
		}
	}

	// Check classes
	for _, class := range fileAnalysis.Classes {
		if strings.ToLower(class.Name) == identifier {
			return true
		}
	}

	// Check variables
	for _, variable := range fileAnalysis.Variables {
		if strings.ToLower(variable.Name) == identifier {
			return true
		}
	}

	return false
}

func (ca *ContextAnalyzer) referencesNonExistentElements(comment Comment, fileAnalysis *FileAnalysis) bool {
	return ca.detectObsoleteReferences(comment, fileAnalysis)
}

func (ca *ContextAnalyzer) isInWrongContext(comment Comment, lineContext *CodeContext) bool {
	// Very basic heuristic - could be improved
	if ca.commentLooksLikeFunctionComment(comment) && lineContext.Function == nil {
		return true
	}

	return false
}

func (ca *ContextAnalyzer) findBetterLocationForComment(comment Comment, fileAnalysis *FileAnalysis) *int {
	commentText := strings.ToLower(comment.Body)

	// Try to find a function that the comment might be referring to
	for _, function := range fileAnalysis.Functions {
		if strings.Contains(commentText, strings.ToLower(function.Name)) {
			// Suggest the line just before the function
			line := function.StartLine - 1
			if line < 1 {
				line = function.StartLine
			}
			return &line
		}
	}

	return nil
}

func (ca *ContextAnalyzer) detectOutdatedInformation(comment Comment, _ *FileAnalysis) []string {
	// This is a placeholder for more sophisticated analysis
	// Could check for outdated parameter names, return types, etc.
	var outdated []string

	// Simple example: check if comment mentions parameters that don't exist
	commentText := strings.ToLower(comment.Body)
	if strings.Contains(commentText, "parameter") || strings.Contains(commentText, "param") {
		// TODO: Implement sophisticated parameter extraction
		outdated = append(outdated, "Parameter information may be outdated")
	}

	return outdated
}

// ClearFileCache clears the analysis cache for a specific file or all files.
func (ca *ContextAnalyzer) ClearFileCache(filePath string) {
	if filePath == "" {
		// Clear all cache
		ca.fileCache = make(map[string]*FileAnalysis)
	} else {
		delete(ca.fileCache, filePath)
	}
}

// GetCachedAnalysis returns cached analysis for a file if it exists.
func (ca *ContextAnalyzer) GetCachedAnalysis(filePath string) *FileAnalysis {
	return ca.fileCache[filePath]
}
