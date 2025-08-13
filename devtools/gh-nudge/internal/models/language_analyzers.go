package models

import (
	"bufio"
	"go/ast"
	"go/parser"
	"go/token"
	"regexp"
	"strings"
	"time"
)

// GoLanguageAnalyzer analyzes Go source code.
type GoLanguageAnalyzer struct{}

// AnalyzeFile analyzes a Go source file.
func (g *GoLanguageAnalyzer) AnalyzeFile(filePath string, content []byte) (*FileAnalysis, error) {
	fileSet := token.NewFileSet()

	// Parse the Go source code
	node, err := parser.ParseFile(fileSet, filePath, content, parser.ParseComments)
	if err != nil {
		// If parsing fails, fall back to generic analysis
		generic := &GenericAnalyzer{}
		return generic.AnalyzeFile(filePath, content)
	}

	analysis := g.createFileAnalysis(filePath)
	g.extractImports(node, fileSet, analysis)
	g.extractASTNodes(node, fileSet, analysis)
	g.extractComments(node, fileSet, analysis)

	return analysis, nil
}

// createFileAnalysis creates a new FileAnalysis structure.
func (g *GoLanguageAnalyzer) createFileAnalysis(filePath string) *FileAnalysis {
	return &FileAnalysis{
		FilePath:     filePath,
		Language:     "go",
		Functions:    make([]FunctionInfo, 0),
		Classes:      make([]ClassInfo, 0),
		Imports:      make([]ImportInfo, 0),
		Variables:    make([]VariableInfo, 0),
		Comments:     make([]CodeComment, 0),
		Dependencies: make([]string, 0),
		LastModified: time.Now(),
		LineMapping:  make(map[int]CodeContext),
	}
}

// extractImports extracts import information from the AST.
func (g *GoLanguageAnalyzer) extractImports(node *ast.File, fileSet *token.FileSet, analysis *FileAnalysis) {
	for _, importSpec := range node.Imports {
		importInfo := ImportInfo{
			Package:   strings.Trim(importSpec.Path.Value, "\""),
			StartLine: fileSet.Position(importSpec.Pos()).Line,
			IsUsed:    true, // TODO: Could analyze usage
		}
		if importSpec.Name != nil {
			importInfo.Alias = importSpec.Name.Name
		}
		analysis.Imports = append(analysis.Imports, importInfo)
		analysis.Dependencies = append(analysis.Dependencies, importInfo.Package)
	}
}

// extractASTNodes walks the AST to extract functions, structs, and variables.
func (g *GoLanguageAnalyzer) extractASTNodes(node *ast.File, fileSet *token.FileSet, analysis *FileAnalysis) {
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			g.processFuncDecl(x, fileSet, analysis)
		case *ast.TypeSpec:
			g.processTypeSpec(x, fileSet, analysis)
		case *ast.GenDecl:
			g.processGenDecl(x, fileSet, analysis)
		}
		return true
	})
}

// processFuncDecl processes a function declaration.
func (g *GoLanguageAnalyzer) processFuncDecl(funcDecl *ast.FuncDecl, fileSet *token.FileSet, analysis *FileAnalysis) {
	function := g.extractFunctionInfo(funcDecl, fileSet)
	analysis.Functions = append(analysis.Functions, function)

	// Update line mapping for function
	for line := function.StartLine; line <= function.EndLine; line++ {
		analysis.LineMapping[line] = CodeContext{
			Line:     line,
			Function: &function,
			Scope:    "function",
			CodeType: "function",
		}
	}
}

// processTypeSpec processes a type specification (structs, interfaces, etc.).
func (g *GoLanguageAnalyzer) processTypeSpec(typeSpec *ast.TypeSpec, fileSet *token.FileSet, analysis *FileAnalysis) {
	if structType, ok := typeSpec.Type.(*ast.StructType); ok {
		class := g.extractStructInfo(typeSpec, structType, fileSet)
		analysis.Classes = append(analysis.Classes, class)

		// Update line mapping for struct
		for line := class.StartLine; line <= class.EndLine; line++ {
			existing := analysis.LineMapping[line]
			existing.Class = &class
			if existing.Scope == "" {
				existing.Scope = "struct"
			}
			analysis.LineMapping[line] = existing
		}
	}
}

// processGenDecl processes general declarations (variables, constants, etc.).
func (g *GoLanguageAnalyzer) processGenDecl(genDecl *ast.GenDecl, fileSet *token.FileSet, analysis *FileAnalysis) {
	for _, spec := range genDecl.Specs {
		if valueSpec, ok := spec.(*ast.ValueSpec); ok {
			for _, name := range valueSpec.Names {
				variable := VariableInfo{
					Name:      name.Name,
					StartLine: fileSet.Position(name.Pos()).Line,
					Scope:     "global",
				}
				if valueSpec.Type != nil {
					variable.Type = g.typeToString(valueSpec.Type)
				}
				variable.IsConstant = genDecl.Tok == token.CONST
				analysis.Variables = append(analysis.Variables, variable)
			}
		}
	}
}

// extractComments extracts comments from the AST.
func (g *GoLanguageAnalyzer) extractComments(node *ast.File, fileSet *token.FileSet, analysis *FileAnalysis) {
	for _, commentGroup := range node.Comments {
		for _, comment := range commentGroup.List {
			pos := fileSet.Position(comment.Pos())
			endPos := fileSet.Position(comment.End())

			codeComment := CodeComment{
				Text:      comment.Text,
				StartLine: pos.Line,
				EndLine:   endPos.Line,
				Type:      "line",
			}

			if strings.HasPrefix(comment.Text, "/*") {
				codeComment.Type = "block"
			}

			analysis.Comments = append(analysis.Comments, codeComment)
		}
	}
}

// extractFunctionInfo extracts information about a function.
func (g *GoLanguageAnalyzer) extractFunctionInfo(funcDecl *ast.FuncDecl, fileSet *token.FileSet) FunctionInfo {
	startPos := fileSet.Position(funcDecl.Pos())
	endPos := fileSet.Position(funcDecl.End())

	function := FunctionInfo{
		Name:       funcDecl.Name.Name,
		StartLine:  startPos.Line,
		EndLine:    endPos.Line,
		Parameters: make([]ParameterInfo, 0),
		Visibility: "public", // Go uses capitalization for visibility
		IsMethod:   funcDecl.Recv != nil,
		Complexity: 1, // TODO: Calculate cyclomatic complexity
		CallSites:  make([]int, 0),
	}

	// Check visibility
	if len(function.Name) > 0 && function.Name[0] >= 'a' && function.Name[0] <= 'z' {
		function.Visibility = "private"
	}

	// Extract parameters
	if funcDecl.Type.Params != nil {
		for _, field := range funcDecl.Type.Params.List {
			paramType := g.typeToString(field.Type)
			if len(field.Names) == 0 {
				// Anonymous parameter
				function.Parameters = append(function.Parameters, ParameterInfo{
					Name: "",
					Type: paramType,
				})
			} else {
				for _, name := range field.Names {
					function.Parameters = append(function.Parameters, ParameterInfo{
						Name: name.Name,
						Type: paramType,
					})
				}
			}
		}
	}

	// Extract return type
	if funcDecl.Type.Results != nil {
		function.ReturnType = g.resultsToString(funcDecl.Type.Results)
	}

	// Extract method receiver information
	if funcDecl.Recv != nil && len(funcDecl.Recv.List) > 0 {
		receiverType := g.typeToString(funcDecl.Recv.List[0].Type)
		function.ClassName = receiverType
	}

	return function
}

// extractStructInfo extracts information about a struct.
func (g *GoLanguageAnalyzer) extractStructInfo(typeSpec *ast.TypeSpec, structType *ast.StructType, fileSet *token.FileSet) ClassInfo {
	startPos := fileSet.Position(typeSpec.Pos())
	endPos := fileSet.Position(structType.End())

	class := ClassInfo{
		Name:        typeSpec.Name.Name,
		StartLine:   startPos.Line,
		EndLine:     endPos.Line,
		Methods:     make([]FunctionInfo, 0), // Will be populated later
		Fields:      make([]VariableInfo, 0),
		Visibility:  "public",
		IsInterface: false,
	}

	// Check visibility
	if len(class.Name) > 0 && class.Name[0] >= 'a' && class.Name[0] <= 'z' {
		class.Visibility = "private"
	}

	// Extract fields
	for _, field := range structType.Fields.List {
		fieldType := g.typeToString(field.Type)
		if len(field.Names) == 0 {
			// Anonymous field
			class.Fields = append(class.Fields, VariableInfo{
				Name:      "",
				Type:      fieldType,
				StartLine: fileSet.Position(field.Pos()).Line,
				Scope:     "struct",
			})
		} else {
			for _, name := range field.Names {
				visibility := "public"
				if len(name.Name) > 0 && name.Name[0] >= 'a' && name.Name[0] <= 'z' {
					visibility = "private"
				}

				class.Fields = append(class.Fields, VariableInfo{
					Name:      name.Name,
					Type:      fieldType,
					StartLine: fileSet.Position(name.Pos()).Line,
					Scope:     "struct",
				})
				_ = visibility // TODO: Store visibility in VariableInfo
			}
		}
	}

	return class
}

// typeToString converts an ast.Expr type to string representation.
func (g *GoLanguageAnalyzer) typeToString(expr ast.Expr) string {
	switch t := expr.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.SelectorExpr:
		return g.typeToString(t.X) + "." + t.Sel.Name
	case *ast.StarExpr:
		return "*" + g.typeToString(t.X)
	case *ast.ArrayType:
		return "[]" + g.typeToString(t.Elt)
	case *ast.MapType:
		return "map[" + g.typeToString(t.Key) + "]" + g.typeToString(t.Value)
	case *ast.ChanType:
		return "chan " + g.typeToString(t.Value)
	case *ast.InterfaceType:
		return "interface{}"
	case *ast.StructType:
		return "struct{}"
	case *ast.FuncType:
		return "func"
	default:
		return "unknown"
	}
}

// resultsToString converts function results to string representation.
func (g *GoLanguageAnalyzer) resultsToString(results *ast.FieldList) string {
	if results == nil || len(results.List) == 0 {
		return ""
	}

	var types []string
	for _, field := range results.List {
		fieldType := g.typeToString(field.Type)
		if len(field.Names) == 0 {
			types = append(types, fieldType)
		} else {
			for range field.Names {
				types = append(types, fieldType)
			}
		}
	}

	if len(types) == 1 {
		return types[0]
	}
	return "(" + strings.Join(types, ", ") + ")"
}

// GetLanguage returns the language name.
func (g *GoLanguageAnalyzer) GetLanguage() string {
	return "go"
}

// SupportedExtensions returns the supported file extensions.
func (g *GoLanguageAnalyzer) SupportedExtensions() []string {
	return []string{".go"}
}

// JavaScriptAnalyzer analyzes JavaScript/TypeScript source code.
type JavaScriptAnalyzer struct{}

// AnalyzeFile analyzes a JavaScript/TypeScript source file.
func (j *JavaScriptAnalyzer) AnalyzeFile(filePath string, content []byte) (*FileAnalysis, error) {
	analysis := &FileAnalysis{
		FilePath:     filePath,
		Language:     "javascript",
		Functions:    make([]FunctionInfo, 0),
		Classes:      make([]ClassInfo, 0),
		Imports:      make([]ImportInfo, 0),
		Variables:    make([]VariableInfo, 0),
		Comments:     make([]CodeComment, 0),
		Dependencies: make([]string, 0),
		LastModified: time.Now(),
		LineMapping:  make(map[int]CodeContext),
	}

	// Use regex-based parsing for basic analysis
	lines := strings.Split(string(content), "\n")

	// Extract functions
	functionRegex := regexp.MustCompile(`^\s*(export\s+)?(async\s+)?function\s+([a-zA-Z_$][a-zA-Z0-9_$]*)\s*\(([^)]*)\)`)
	arrowFunctionRegex := regexp.MustCompile(`^\s*(const|let|var)\s+([a-zA-Z_$][a-zA-Z0-9_$]*)\s*=\s*(\([^)]*\)|[a-zA-Z_$][a-zA-Z0-9_$]*)\s*=>`)
	// methodRegex for future use
	// methodRegex := regexp.MustCompile(`^\s*([a-zA-Z_$][a-zA-Z0-9_$]*)\s*\([^)]*\)\s*\{`)

	for lineNum, line := range lines {
		lineNumber := lineNum + 1

		// Check for function declarations
		if matches := functionRegex.FindStringSubmatch(line); matches != nil {
			function := FunctionInfo{
				Name:       matches[3],
				StartLine:  lineNumber,
				EndLine:    j.findEndOfBlock(lines, lineNum),
				Parameters: j.parseJSParameters(matches[4]),
				Visibility: "public",
				IsMethod:   false,
				Complexity: 1,
				CallSites:  make([]int, 0),
			}
			analysis.Functions = append(analysis.Functions, function)
		}

		// Check for arrow functions
		if matches := arrowFunctionRegex.FindStringSubmatch(line); matches != nil {
			function := FunctionInfo{
				Name:       matches[2],
				StartLine:  lineNumber,
				EndLine:    j.findEndOfExpression(lines, lineNum),
				Parameters: j.parseJSParameters(matches[3]),
				Visibility: "public",
				IsMethod:   false,
				Complexity: 1,
				CallSites:  make([]int, 0),
			}
			analysis.Functions = append(analysis.Functions, function)
		}
	}

	// Extract classes
	classRegex := regexp.MustCompile(`^\s*(export\s+)?class\s+([a-zA-Z_$][a-zA-Z0-9_$]*)\s*(\\extends\s+([a-zA-Z_$][a-zA-Z0-9_$]*))?\s*\{`)

	for lineNum, line := range lines {
		lineNumber := lineNum + 1

		if matches := classRegex.FindStringSubmatch(line); matches != nil {
			class := ClassInfo{
				Name:        matches[2],
				StartLine:   lineNumber,
				EndLine:     j.findEndOfBlock(lines, lineNum),
				Methods:     make([]FunctionInfo, 0),
				Fields:      make([]VariableInfo, 0),
				Visibility:  "public",
				IsInterface: false,
			}
			if matches[4] != "" {
				class.Extends = matches[4]
			}
			analysis.Classes = append(analysis.Classes, class)
		}
	}

	// Extract imports
	importRegex := regexp.MustCompile(`^\s*import\s+(.+)\s+from\s+['"]([^'"]+)['"]`)
	requireRegex := regexp.MustCompile(`^\s*(const|let|var)\s+(.+)\s*=\s*require\s*\(\s*['"]([^'"]+)['"]\s*\)`)

	for lineNum, line := range lines {
		lineNumber := lineNum + 1

		if matches := importRegex.FindStringSubmatch(line); matches != nil {
			importInfo := ImportInfo{
				Package:   matches[2],
				StartLine: lineNumber,
				IsUsed:    true,
			}
			analysis.Imports = append(analysis.Imports, importInfo)
			analysis.Dependencies = append(analysis.Dependencies, importInfo.Package)
		}

		if matches := requireRegex.FindStringSubmatch(line); matches != nil {
			importInfo := ImportInfo{
				Package:   matches[3],
				Alias:     matches[2],
				StartLine: lineNumber,
				IsUsed:    true,
			}
			analysis.Imports = append(analysis.Imports, importInfo)
			analysis.Dependencies = append(analysis.Dependencies, importInfo.Package)
		}
	}

	return analysis, nil
}

// findEndOfBlock finds the end of a code block starting from the given line.
func (j *JavaScriptAnalyzer) findEndOfBlock(lines []string, startLine int) int {
	braceCount := 0
	inString := false
	stringChar := byte(0)

	for i := startLine; i < len(lines); i++ {
		line := lines[i]
		for j := 0; j < len(line); j++ {
			char := line[j]

			// Handle strings
			if !inString && (char == '"' || char == '\'' || char == '`') {
				inString = true
				stringChar = char
			} else if inString && char == stringChar {
				inString = false
			}

			if !inString {
				switch char {
				case '{':
					braceCount++
				case '}':
					braceCount--
					if braceCount == 0 {
						return i + 1
					}
				}
			}
		}
	}

	return len(lines)
}

// findEndOfExpression finds the end of an expression (for arrow functions).
func (j *JavaScriptAnalyzer) findEndOfExpression(lines []string, startLine int) int {
	// Simple heuristic: if the line doesn't end with {, assume single expression
	line := strings.TrimSpace(lines[startLine])
	if strings.HasSuffix(line, "{") {
		return j.findEndOfBlock(lines, startLine)
	}
	return startLine + 1
}

// parseJSParameters parses JavaScript function parameters.
func (j *JavaScriptAnalyzer) parseJSParameters(paramStr string) []ParameterInfo {
	var params []ParameterInfo

	if strings.TrimSpace(paramStr) == "" {
		return params
	}

	// Simple parameter parsing - could be enhanced
	parts := strings.Split(paramStr, ",")
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part != "" {
			// Handle default parameters and destructuring (basic)
			if idx := strings.Index(part, "="); idx != -1 {
				part = strings.TrimSpace(part[:idx])
			}

			params = append(params, ParameterInfo{
				Name: part,
				Type: "any", // JavaScript is dynamically typed
			})
		}
	}

	return params
}

// GetLanguage returns the language name.
func (j *JavaScriptAnalyzer) GetLanguage() string {
	return "javascript"
}

// SupportedExtensions returns the supported file extensions.
func (j *JavaScriptAnalyzer) SupportedExtensions() []string {
	return []string{".js", ".jsx", ".ts", ".tsx"}
}

// PythonAnalyzer analyzes Python source code.
type PythonAnalyzer struct{}

// AnalyzeFile analyzes a Python source file.
func (p *PythonAnalyzer) AnalyzeFile(filePath string, content []byte) (*FileAnalysis, error) {
	analysis := &FileAnalysis{
		FilePath:     filePath,
		Language:     "python",
		Functions:    make([]FunctionInfo, 0),
		Classes:      make([]ClassInfo, 0),
		Imports:      make([]ImportInfo, 0),
		Variables:    make([]VariableInfo, 0),
		Comments:     make([]CodeComment, 0),
		Dependencies: make([]string, 0),
		LastModified: time.Now(),
		LineMapping:  make(map[int]CodeContext),
	}

	lines := strings.Split(string(content), "\n")

	// Extract functions
	functionRegex := regexp.MustCompile(`^\s*def\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)\s*:`)

	for lineNum, line := range lines {
		lineNumber := lineNum + 1

		if matches := functionRegex.FindStringSubmatch(line); matches != nil {
			function := FunctionInfo{
				Name:       matches[1],
				StartLine:  lineNumber,
				EndLine:    p.findEndOfPythonBlock(lines, lineNum),
				Parameters: p.parsePythonParameters(matches[2]),
				Visibility: p.getPythonVisibility(matches[1]),
				IsMethod:   p.isInClass(lines, lineNum),
				Complexity: 1,
				CallSites:  make([]int, 0),
			}
			analysis.Functions = append(analysis.Functions, function)
		}
	}

	// Extract classes
	classRegex := regexp.MustCompile(`^\s*class\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*(\([^)]*\))?\s*:`)

	for lineNum, line := range lines {
		lineNumber := lineNum + 1

		if matches := classRegex.FindStringSubmatch(line); matches != nil {
			class := ClassInfo{
				Name:        matches[1],
				StartLine:   lineNumber,
				EndLine:     p.findEndOfPythonBlock(lines, lineNum),
				Methods:     make([]FunctionInfo, 0),
				Fields:      make([]VariableInfo, 0),
				Visibility:  p.getPythonVisibility(matches[1]),
				IsInterface: false,
			}
			if matches[2] != "" {
				// Extract parent class from parentheses
				parentStr := strings.Trim(matches[2], "()")
				if parentStr != "" {
					class.Extends = parentStr
				}
			}
			analysis.Classes = append(analysis.Classes, class)
		}
	}

	// Extract imports
	importRegex := regexp.MustCompile(`^\s*import\s+([a-zA-Z_][a-zA-Z0-9_.]*(?:\s*,\s*[a-zA-Z_][a-zA-Z0-9_.]*)*)`)
	fromImportRegex := regexp.MustCompile(`^\s*from\s+([a-zA-Z_][a-zA-Z0-9_.]*)\s+import\s+(.+)`)

	for lineNum, line := range lines {
		lineNumber := lineNum + 1

		if matches := importRegex.FindStringSubmatch(line); matches != nil {
			packages := strings.Split(matches[1], ",")
			for _, pkg := range packages {
				pkg = strings.TrimSpace(pkg)
				importInfo := ImportInfo{
					Package:   pkg,
					StartLine: lineNumber,
					IsUsed:    true,
				}
				analysis.Imports = append(analysis.Imports, importInfo)
				analysis.Dependencies = append(analysis.Dependencies, pkg)
			}
		}

		if matches := fromImportRegex.FindStringSubmatch(line); matches != nil {
			importInfo := ImportInfo{
				Package:   matches[1],
				Alias:     matches[2],
				StartLine: lineNumber,
				IsUsed:    true,
			}
			analysis.Imports = append(analysis.Imports, importInfo)
			analysis.Dependencies = append(analysis.Dependencies, matches[1])
		}
	}

	return analysis, nil
}

// findEndOfPythonBlock finds the end of a Python code block based on indentation.
func (p *PythonAnalyzer) findEndOfPythonBlock(lines []string, startLine int) int {
	if startLine >= len(lines) {
		return startLine
	}

	// Get the indentation level of the line after the definition
	baseIndentation := p.getIndentationLevel(lines[startLine])

	for i := startLine + 1; i < len(lines); i++ {
		line := strings.TrimRight(lines[i], " \t\n\r")
		if line == "" {
			continue // Skip empty lines
		}

		currentIndentation := p.getIndentationLevel(line)
		if currentIndentation <= baseIndentation {
			return i
		}
	}

	return len(lines)
}

// getIndentationLevel calculates the indentation level of a line.
func (p *PythonAnalyzer) getIndentationLevel(line string) int {
	count := 0
	for _, char := range line {
		switch char {
		case ' ':
			count++
		case '\t':
			count += 4 // Assume tab = 4 spaces
		default:
			break
		}
	}
	return count
}

// parsePythonParameters parses Python function parameters.
func (p *PythonAnalyzer) parsePythonParameters(paramStr string) []ParameterInfo {
	var params []ParameterInfo

	if strings.TrimSpace(paramStr) == "" {
		return params
	}

	parts := strings.Split(paramStr, ",")
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part != "" {
			// Handle default parameters and type hints (basic)
			name := part
			paramType := "any"

			// Remove default values
			if idx := strings.Index(part, "="); idx != -1 {
				name = strings.TrimSpace(part[:idx])
			}

			// Extract type hints
			if idx := strings.Index(name, ":"); idx != -1 {
				paramType = strings.TrimSpace(name[idx+1:])
				name = strings.TrimSpace(name[:idx])
			}

			params = append(params, ParameterInfo{
				Name: name,
				Type: paramType,
			})
		}
	}

	return params
}

// getPythonVisibility determines visibility based on Python naming conventions.
func (p *PythonAnalyzer) getPythonVisibility(name string) string {
	if strings.HasPrefix(name, "__") {
		return "private"
	} else if strings.HasPrefix(name, "_") {
		return "protected"
	}
	return "public"
}

// isInClass checks if a function is defined inside a class.
func (p *PythonAnalyzer) isInClass(lines []string, functionLine int) bool {
	functionIndent := p.getIndentationLevel(lines[functionLine])

	// Look backwards for a class definition
	for i := functionLine - 1; i >= 0; i-- {
		line := strings.TrimSpace(lines[i])
		if line == "" {
			continue
		}

		lineIndent := p.getIndentationLevel(lines[i])
		if lineIndent < functionIndent {
			return strings.HasPrefix(line, "class ")
		}
	}

	return false
}

// GetLanguage returns the language name.
func (p *PythonAnalyzer) GetLanguage() string {
	return "python"
}

// SupportedExtensions returns the supported file extensions.
func (p *PythonAnalyzer) SupportedExtensions() []string {
	return []string{".py"}
}

// GenericAnalyzer provides basic analysis for unsupported languages.
type GenericAnalyzer struct{}

// AnalyzeFile performs generic analysis using simple heuristics.
func (g *GenericAnalyzer) AnalyzeFile(filePath string, content []byte) (*FileAnalysis, error) {
	analysis := &FileAnalysis{
		FilePath:     filePath,
		Language:     "generic",
		Functions:    make([]FunctionInfo, 0),
		Classes:      make([]ClassInfo, 0),
		Imports:      make([]ImportInfo, 0),
		Variables:    make([]VariableInfo, 0),
		Comments:     make([]CodeComment, 0),
		Dependencies: make([]string, 0),
		LastModified: time.Now(),
		LineMapping:  make(map[int]CodeContext),
	}

	scanner := bufio.NewScanner(strings.NewReader(string(content)))
	lineNum := 0

	// Basic comment extraction
	for scanner.Scan() {
		lineNum++
		line := scanner.Text()

		// Detect common comment patterns
		if strings.Contains(line, "//") || strings.Contains(line, "#") || strings.Contains(line, "/*") {
			comment := CodeComment{
				Text:      line,
				StartLine: lineNum,
				EndLine:   lineNum,
				Type:      "line",
			}
			analysis.Comments = append(analysis.Comments, comment)
		}

		// Basic line mapping
		context := CodeContext{
			Line:     lineNum,
			Scope:    "file",
			CodeType: "unknown",
		}
		analysis.LineMapping[lineNum] = context
	}

	return analysis, nil
}

// GetLanguage returns the language name.
func (g *GenericAnalyzer) GetLanguage() string {
	return "generic"
}

// SupportedExtensions returns empty slice since this is a fallback analyzer.
func (g *GenericAnalyzer) SupportedExtensions() []string {
	return []string{}
}
