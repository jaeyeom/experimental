package models

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"
	"time"
)

// AdvancedAnalyzer provides multi-file dependency analysis and cross-reference tracking.
type AdvancedAnalyzer struct {
	contextAnalyzer *ContextAnalyzer
	projectAnalysis *ProjectAnalysis
	dependencyGraph *DependencyGraph
}

// ProjectAnalysis represents the analysis of an entire project.
type ProjectAnalysis struct {
	RootPath        string                   `json:"rootPath"`
	Files           map[string]*FileAnalysis `json:"files"`
	Dependencies    *DependencyGraph         `json:"dependencies"`
	CrossReferences *CrossReferenceMap       `json:"crossReferences"`
	Metrics         *ProjectMetrics          `json:"metrics"`
	LastUpdated     time.Time                `json:"lastUpdated"`
	Language        string                   `json:"primaryLanguage"`
	Framework       string                   `json:"framework,omitempty"`
}

// DependencyGraph represents file dependencies within a project.
type DependencyGraph struct {
	Nodes map[string]*DependencyNode `json:"nodes"`
	Edges []DependencyEdge           `json:"edges"`
}

// DependencyNode represents a file in the dependency graph.
type DependencyNode struct {
	FilePath     string                 `json:"filePath"`
	Language     string                 `json:"language"`
	Dependencies []string               `json:"dependencies"`
	Dependents   []string               `json:"dependents"`
	Metadata     map[string]interface{} `json:"metadata,omitempty"`
}

// DependencyEdge represents a dependency relationship.
type DependencyEdge struct {
	From       string         `json:"from"`
	To         string         `json:"to"`
	Type       DependencyType `json:"type"`
	Confidence float64        `json:"confidence"`
}

// DependencyType represents the type of dependency relationship.
type DependencyType string

const (
	DepImport    DependencyType = "import"    // Direct import/require
	DepReference DependencyType = "reference" // Function/class reference
	DepCall      DependencyType = "call"      // Function call
	DepInherit   DependencyType = "inherit"   // Class inheritance
	DepInclude   DependencyType = "include"   // File include
)

// CrossReferenceMap tracks cross-references between code elements.
type CrossReferenceMap struct {
	References  map[string][]CrossReference `json:"references"`
	Definitions map[string]CodeDefinition   `json:"definitions"`
}

// CrossReference represents a reference to a code element.
type CrossReference struct {
	From       Location      `json:"from"`
	To         Location      `json:"to"`
	Type       ReferenceType `json:"type"`
	Confidence float64       `json:"confidence"`
	Context    string        `json:"context,omitempty"`
}

// Location represents a location in source code.
type Location struct {
	FilePath string `json:"filePath"`
	Line     int    `json:"line"`
	Column   int    `json:"column,omitempty"`
}

// CodeDefinition represents the definition of a code element.
type CodeDefinition struct {
	Name      string                 `json:"name"`
	Type      DefinitionType         `json:"type"`
	Location  Location               `json:"location"`
	Signature string                 `json:"signature,omitempty"`
	Metadata  map[string]interface{} `json:"metadata,omitempty"`
}

// ReferenceType represents the type of cross-reference.
type ReferenceType string

const (
	RefFunction ReferenceType = "function"
	RefVariable ReferenceType = "variable"
	RefClass    ReferenceType = "class"
	RefMethod   ReferenceType = "method"
	RefField    ReferenceType = "field"
	RefType     ReferenceType = "type"
)

// DefinitionType represents the type of code definition.
type DefinitionType string

const (
	DefFunction  DefinitionType = "function"
	DefVariable  DefinitionType = "variable"
	DefClass     DefinitionType = "class"
	DefInterface DefinitionType = "interface"
	DefType      DefinitionType = "type"
	DefConstant  DefinitionType = "constant"
)

// ProjectMetrics contains project-wide metrics.
type ProjectMetrics struct {
	TotalFiles           int     `json:"totalFiles"`
	TotalLines           int     `json:"totalLines"`
	TotalFunctions       int     `json:"totalFunctions"`
	TotalClasses         int     `json:"totalClasses"`
	CyclomaticComplexity int     `json:"cyclomaticComplexity"`
	CohesionScore        float64 `json:"cohesionScore"`
	CouplingScore        float64 `json:"couplingScore"`
	TestCoverage         float64 `json:"testCoverage,omitempty"`
}

// NewAdvancedAnalyzer creates a new advanced analyzer.
func NewAdvancedAnalyzer() *AdvancedAnalyzer {
	return &AdvancedAnalyzer{
		contextAnalyzer: NewContextAnalyzer(),
		projectAnalysis: &ProjectAnalysis{
			Files:        make(map[string]*FileAnalysis),
			Dependencies: &DependencyGraph{Nodes: make(map[string]*DependencyNode)},
			CrossReferences: &CrossReferenceMap{
				References:  make(map[string][]CrossReference),
				Definitions: make(map[string]CodeDefinition),
			},
			Metrics: &ProjectMetrics{},
		},
		dependencyGraph: &DependencyGraph{
			Nodes: make(map[string]*DependencyNode),
		},
	}
}

// AnalyzeProject performs comprehensive analysis of a project.
func (aa *AdvancedAnalyzer) AnalyzeProject(rootPath string, filePaths []string) (*ProjectAnalysis, error) {
	aa.projectAnalysis.RootPath = rootPath
	aa.projectAnalysis.LastUpdated = time.Now()

	// First pass: analyze individual files
	for _, filePath := range filePaths {
		if err := aa.analyzeFile(filePath); err != nil {
			fmt.Printf("Warning: Failed to analyze %s: %v\n", filePath, err)
			continue
		}
	}

	// Second pass: build dependency graph
	aa.buildDependencyGraph()

	// Third pass: find cross-references
	aa.findCrossReferences()

	// Fourth pass: calculate metrics
	aa.calculateProjectMetrics()

	// Detect primary language and framework
	aa.detectProjectCharacteristics()

	return aa.projectAnalysis, nil
}

// analyzeFile analyzes a single file and adds it to the project analysis.
func (aa *AdvancedAnalyzer) analyzeFile(filePath string) error {
	// TODO: Read file content
	// For now, use empty content - in real implementation would read from disk
	content := []byte{}

	analysis, err := aa.contextAnalyzer.getFileAnalysis(filePath, content)
	if err != nil {
		return err
	}

	aa.projectAnalysis.Files[filePath] = analysis
	return nil
}

// buildDependencyGraph constructs the project dependency graph.
func (aa *AdvancedAnalyzer) buildDependencyGraph() {
	for filePath, fileAnalysis := range aa.projectAnalysis.Files {
		// Create node if it doesn't exist
		if _, exists := aa.dependencyGraph.Nodes[filePath]; !exists {
			aa.dependencyGraph.Nodes[filePath] = &DependencyNode{
				FilePath:     filePath,
				Language:     fileAnalysis.Language,
				Dependencies: make([]string, 0),
				Dependents:   make([]string, 0),
			}
		}

		node := aa.dependencyGraph.Nodes[filePath]

		// Process imports/dependencies
		for _, importInfo := range fileAnalysis.Imports {
			depPath := aa.resolveImportPath(importInfo.Package, filePath)
			if depPath != "" {
				node.Dependencies = append(node.Dependencies, depPath)

				// Create edge
				edge := DependencyEdge{
					From:       filePath,
					To:         depPath,
					Type:       DepImport,
					Confidence: 1.0,
				}
				aa.dependencyGraph.Edges = append(aa.dependencyGraph.Edges, edge)

				// Update dependent's dependents list
				if depNode, exists := aa.dependencyGraph.Nodes[depPath]; exists {
					depNode.Dependents = append(depNode.Dependents, filePath)
				}
			}
		}
	}

	aa.projectAnalysis.Dependencies = aa.dependencyGraph
}

// resolveImportPath resolves an import path to an actual file path.
func (aa *AdvancedAnalyzer) resolveImportPath(importPath, fromFile string) string {
	// Simple resolution logic - could be enhanced
	if strings.HasPrefix(importPath, "./") || strings.HasPrefix(importPath, "../") {
		// Relative import
		baseDir := filepath.Dir(fromFile)
		resolved := filepath.Join(baseDir, importPath)
		return filepath.Clean(resolved)
	}

	// For absolute imports, check if file exists in project
	for filePath := range aa.projectAnalysis.Files {
		if strings.Contains(filePath, importPath) {
			return filePath
		}
	}

	return ""
}

// findCrossReferences identifies cross-references between code elements.
func (aa *AdvancedAnalyzer) findCrossReferences() {
	// Build definitions map first
	for filePath, fileAnalysis := range aa.projectAnalysis.Files {
		// Add function definitions
		for _, function := range fileAnalysis.Functions {
			key := fmt.Sprintf("%s:%s", filePath, function.Name)
			aa.projectAnalysis.CrossReferences.Definitions[key] = CodeDefinition{
				Name:      function.Name,
				Type:      DefFunction,
				Location:  Location{FilePath: filePath, Line: function.StartLine},
				Signature: aa.formatFunctionSignature(function),
			}
		}

		// Add class definitions
		for _, class := range fileAnalysis.Classes {
			key := fmt.Sprintf("%s:%s", filePath, class.Name)
			aa.projectAnalysis.CrossReferences.Definitions[key] = CodeDefinition{
				Name:     class.Name,
				Type:     DefClass,
				Location: Location{FilePath: filePath, Line: class.StartLine},
			}
		}

		// Add variable definitions
		for _, variable := range fileAnalysis.Variables {
			key := fmt.Sprintf("%s:%s", filePath, variable.Name)
			defType := DefVariable
			if variable.IsConstant {
				defType = DefConstant
			}
			aa.projectAnalysis.CrossReferences.Definitions[key] = CodeDefinition{
				Name:     variable.Name,
				Type:     defType,
				Location: Location{FilePath: filePath, Line: variable.StartLine},
			}
		}
	}

	// Find references by scanning code for identifiers
	for filePath, fileAnalysis := range aa.projectAnalysis.Files {
		aa.findReferencesInFile(filePath, fileAnalysis)
	}
}

// findReferencesInFile finds references within a specific file.
func (aa *AdvancedAnalyzer) findReferencesInFile(filePath string, fileAnalysis *FileAnalysis) {
	// Simple identifier extraction - could be enhanced with proper parsing
	identifierRegex := regexp.MustCompile(`\b[a-zA-Z_][a-zA-Z0-9_]*\b`)

	for line, context := range fileAnalysis.LineMapping {
		// Extract identifiers from surrounding lines
		for _, surroundingLine := range context.SurroundingLines {
			identifiers := identifierRegex.FindAllString(surroundingLine, -1)

			for _, identifier := range identifiers {
				// Check if this identifier is defined elsewhere
				for defKey, definition := range aa.projectAnalysis.CrossReferences.Definitions {
					if definition.Name == identifier && !strings.HasPrefix(defKey, filePath+":") {
						// Found a cross-file reference
						reference := CrossReference{
							From:       Location{FilePath: filePath, Line: line},
							To:         definition.Location,
							Type:       aa.mapDefinitionToReferenceType(definition.Type),
							Confidence: 0.7, // Could be improved with better analysis
						}

						loc := NewFileLocationSingleLine(filePath, line)
						refKey := loc.Key()
						aa.projectAnalysis.CrossReferences.References[refKey] = append(
							aa.projectAnalysis.CrossReferences.References[refKey], reference)
					}
				}
			}
		}
	}
}

// formatFunctionSignature formats a function signature string.
func (aa *AdvancedAnalyzer) formatFunctionSignature(function FunctionInfo) string {
	var params []string
	for _, param := range function.Parameters {
		if param.Type != "" {
			params = append(params, fmt.Sprintf("%s: %s", param.Name, param.Type))
		} else {
			params = append(params, param.Name)
		}
	}

	signature := fmt.Sprintf("%s(%s)", function.Name, strings.Join(params, ", "))
	if function.ReturnType != "" {
		signature += " -> " + function.ReturnType
	}

	return signature
}

// mapDefinitionToReferenceType maps definition type to reference type.
func (aa *AdvancedAnalyzer) mapDefinitionToReferenceType(defType DefinitionType) ReferenceType {
	switch defType {
	case DefFunction:
		return RefFunction
	case DefClass:
		return RefClass
	case DefVariable:
		return RefVariable
	case DefConstant:
		return RefVariable
	default:
		return RefFunction
	}
}

// calculateProjectMetrics calculates various project metrics.
func (aa *AdvancedAnalyzer) calculateProjectMetrics() {
	metrics := aa.projectAnalysis.Metrics

	metrics.TotalFiles = len(aa.projectAnalysis.Files)
	metrics.TotalLines = 0
	metrics.TotalFunctions = 0
	metrics.TotalClasses = 0
	metrics.CyclomaticComplexity = 0

	for _, fileAnalysis := range aa.projectAnalysis.Files {
		// Count lines (approximation)
		maxLine := 0
		for line := range fileAnalysis.LineMapping {
			if line > maxLine {
				maxLine = line
			}
		}
		metrics.TotalLines += maxLine

		// Count functions and classes
		metrics.TotalFunctions += len(fileAnalysis.Functions)
		metrics.TotalClasses += len(fileAnalysis.Classes)

		// Sum complexity
		for _, function := range fileAnalysis.Functions {
			metrics.CyclomaticComplexity += function.Complexity
		}
	}

	// Calculate cohesion and coupling scores
	metrics.CohesionScore = aa.calculateCohesionScore()
	metrics.CouplingScore = aa.calculateCouplingScore()
}

// calculateCohesionScore calculates a cohesion score for the project.
func (aa *AdvancedAnalyzer) calculateCohesionScore() float64 {
	if len(aa.projectAnalysis.Files) == 0 {
		return 0.0
	}

	// Simple cohesion metric based on internal vs external references
	totalReferences := 0
	internalReferences := 0

	for _, references := range aa.projectAnalysis.CrossReferences.References {
		for _, ref := range references {
			totalReferences++
			if aa.isInternalReference(ref) {
				internalReferences++
			}
		}
	}

	if totalReferences == 0 {
		return 1.0 // Perfect cohesion if no references
	}

	return float64(internalReferences) / float64(totalReferences)
}

// calculateCouplingScore calculates a coupling score for the project.
func (aa *AdvancedAnalyzer) calculateCouplingScore() float64 {
	if len(aa.dependencyGraph.Nodes) == 0 {
		return 0.0
	}

	totalEdges := len(aa.dependencyGraph.Edges)
	possibleEdges := len(aa.dependencyGraph.Nodes) * (len(aa.dependencyGraph.Nodes) - 1)

	if possibleEdges == 0 {
		return 0.0
	}

	return float64(totalEdges) / float64(possibleEdges)
}

// isInternalReference checks if a reference is within the same logical module.
func (aa *AdvancedAnalyzer) isInternalReference(ref CrossReference) bool {
	// Simple heuristic: same directory or subdirectory
	fromDir := filepath.Dir(ref.From.FilePath)
	toDir := filepath.Dir(ref.To.FilePath)

	return fromDir == toDir || strings.HasPrefix(toDir, fromDir) || strings.HasPrefix(fromDir, toDir)
}

// detectProjectCharacteristics detects the primary language and framework.
func (aa *AdvancedAnalyzer) detectProjectCharacteristics() {
	// Count languages
	languageCount := make(map[string]int)
	for _, fileAnalysis := range aa.projectAnalysis.Files {
		languageCount[fileAnalysis.Language]++
	}

	// Find most common language
	maxCount := 0
	primaryLanguage := "unknown"
	for language, count := range languageCount {
		if count > maxCount {
			maxCount = count
			primaryLanguage = language
		}
	}

	aa.projectAnalysis.Language = primaryLanguage

	// Detect framework (simple heuristics)
	aa.projectAnalysis.Framework = aa.detectFramework()
}

// detectFramework attempts to detect the primary framework being used.
func (aa *AdvancedAnalyzer) detectFramework() string {
	// Check for common framework indicators in imports
	for _, fileAnalysis := range aa.projectAnalysis.Files {
		for _, importInfo := range fileAnalysis.Imports {
			pkg := strings.ToLower(importInfo.Package)

			// React
			if strings.Contains(pkg, "react") {
				return "React"
			}

			// Angular
			if strings.Contains(pkg, "angular") || strings.Contains(pkg, "@angular") {
				return "Angular"
			}

			// Vue
			if strings.Contains(pkg, "vue") {
				return "Vue"
			}

			// Express
			if strings.Contains(pkg, "express") {
				return "Express"
			}

			// Django
			if strings.Contains(pkg, "django") {
				return "Django"
			}

			// Flask
			if strings.Contains(pkg, "flask") {
				return "Flask"
			}

			// Gin (Go)
			if strings.Contains(pkg, "gin-gonic/gin") {
				return "Gin"
			}

			// Spring (Java)
			if strings.Contains(pkg, "springframework") {
				return "Spring"
			}
		}
	}

	return ""
}

// AnalyzeCommentMovement analyzes whether comments should be moved based on code changes.
func (aa *AdvancedAnalyzer) AnalyzeCommentMovement(comment Comment, adjustments []LineAdjustment) *CommentMovementAnalysis {
	analysis := &CommentMovementAnalysis{
		Comment:          comment,
		OriginalLocation: Location{FilePath: comment.Path, Line: comment.Line.EndLine},
		ShouldMove:       false,
		Confidence:       0.5,
		Recommendations:  make([]MovementRecommendation, 0),
	}

	// Get file analysis
	fileAnalysis, exists := aa.projectAnalysis.Files[comment.Path]
	if !exists {
		analysis.Recommendations = append(analysis.Recommendations, MovementRecommendation{
			Type:        "warning",
			Description: "File not found in project analysis",
			Confidence:  1.0,
		})
		return analysis
	}

	// Check if comment references moved code
	if aa.commentReferencesMovedCode(comment, fileAnalysis, adjustments) {
		analysis.ShouldMove = true
		analysis.Confidence = 0.8

		// Find better location
		newLocation := aa.findBetterLocation(comment, fileAnalysis)
		if newLocation != nil {
			analysis.RecommendedLocation = newLocation
			analysis.Recommendations = append(analysis.Recommendations, MovementRecommendation{
				Type:        "move",
				Description: fmt.Sprintf("Move comment to line %d where referenced code is located", newLocation.Line),
				Confidence:  0.8,
			})
		}
	}

	// Check for cross-file references
	if aa.commentHasCrossFileReference(comment) {
		analysis.Recommendations = append(analysis.Recommendations, MovementRecommendation{
			Type:        "cross-reference",
			Description: "Comment references code in other files",
			Confidence:  0.6,
		})
	}

	return analysis
}

// CommentMovementAnalysis represents the analysis of whether a comment should be moved.
type CommentMovementAnalysis struct {
	Comment             Comment                  `json:"comment"`
	OriginalLocation    Location                 `json:"originalLocation"`
	ShouldMove          bool                     `json:"shouldMove"`
	RecommendedLocation *Location                `json:"recommendedLocation,omitempty"`
	Confidence          float64                  `json:"confidence"`
	Recommendations     []MovementRecommendation `json:"recommendations"`
}

// MovementRecommendation represents a recommendation for comment movement.
type MovementRecommendation struct {
	Type        string  `json:"type"`
	Description string  `json:"description"`
	Confidence  float64 `json:"confidence"`
}

// commentReferencesMovedCode checks if a comment references code that has moved.
func (aa *AdvancedAnalyzer) commentReferencesMovedCode(comment Comment, fileAnalysis *FileAnalysis, adjustments []LineAdjustment) bool {
	commentText := strings.ToLower(comment.Body)

	// Check if comment mentions function names
	for _, function := range fileAnalysis.Functions {
		if strings.Contains(commentText, strings.ToLower(function.Name)) {
			// Check if this function was affected by adjustments
			for _, adj := range adjustments {
				if function.StartLine >= adj.OldStart && function.EndLine <= adj.OldEnd {
					return true
				}
			}
		}
	}

	return false
}

// findBetterLocation finds a better location for a comment.
func (aa *AdvancedAnalyzer) findBetterLocation(comment Comment, fileAnalysis *FileAnalysis) *Location {
	commentText := strings.ToLower(comment.Body)

	// Look for function references
	for _, function := range fileAnalysis.Functions {
		if strings.Contains(commentText, strings.ToLower(function.Name)) {
			return &Location{
				FilePath: comment.Path,
				Line:     function.StartLine - 1, // Just before function
			}
		}
	}

	return nil
}

// commentHasCrossFileReference checks if a comment has cross-file references.
func (aa *AdvancedAnalyzer) commentHasCrossFileReference(comment Comment) bool {
	// Check cross-references at the comment location
	refKey := comment.GetLocationKey()
	references, exists := aa.projectAnalysis.CrossReferences.References[refKey]
	if !exists {
		return false
	}

	// Check if any references go to other files
	for _, ref := range references {
		if ref.To.FilePath != comment.Path {
			return true
		}
	}

	return false
}

// GetProjectSummary returns a summary of the project analysis.
func (aa *AdvancedAnalyzer) GetProjectSummary() *ProjectSummary {
	metrics := aa.projectAnalysis.Metrics

	return &ProjectSummary{
		Language:      aa.projectAnalysis.Language,
		Framework:     aa.projectAnalysis.Framework,
		FileCount:     metrics.TotalFiles,
		FunctionCount: metrics.TotalFunctions,
		ClassCount:    metrics.TotalClasses,
		Complexity:    metrics.CyclomaticComplexity,
		Cohesion:      metrics.CohesionScore,
		Coupling:      metrics.CouplingScore,
		Dependencies:  len(aa.dependencyGraph.Edges),
		LastUpdated:   aa.projectAnalysis.LastUpdated,
	}
}

// ProjectSummary provides a high-level summary of project analysis.
type ProjectSummary struct {
	Language      string    `json:"language"`
	Framework     string    `json:"framework,omitempty"`
	FileCount     int       `json:"fileCount"`
	FunctionCount int       `json:"functionCount"`
	ClassCount    int       `json:"classCount"`
	Complexity    int       `json:"complexity"`
	Cohesion      float64   `json:"cohesion"`
	Coupling      float64   `json:"coupling"`
	Dependencies  int       `json:"dependencies"`
	LastUpdated   time.Time `json:"lastUpdated"`
}
