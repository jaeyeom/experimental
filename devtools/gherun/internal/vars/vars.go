// Package vars provides template variable substitution for Gherkin feature files.
package vars

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strings"
)

// Vars holds template variables for substitution.
type Vars map[string]string

// varPattern matches {{VAR_NAME}} template variables.
// Supports alphanumeric characters and underscores.
var varPattern = regexp.MustCompile(`\{\{([A-Za-z_][A-Za-z0-9_]*)\}\}`)

// ParseFlag parses a single --var flag value in KEY=VALUE format.
// Returns an error if the format is invalid.
func ParseFlag(s string) (key, value string, err error) {
	key, value, _ = strings.Cut(s, "=")
	if key == "" {
		return "", "", fmt.Errorf("invalid var format %q: empty key", s)
	}
	return key, value, nil
}

// ParseFlags parses multiple --var flag values and returns a Vars map.
func ParseFlags(flags []string) (Vars, error) {
	vars := make(Vars)
	for _, f := range flags {
		key, value, err := ParseFlag(f)
		if err != nil {
			return nil, err
		}
		vars[key] = value
	}
	return vars, nil
}

// LoadEnvFile loads variables from a file in KEY=VALUE format.
// Lines starting with # are treated as comments and ignored.
// Empty lines are also ignored.
func LoadEnvFile(path string) (Vars, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("opening env file: %w", err)
	}
	defer file.Close()

	vars := make(Vars)
	scanner := bufio.NewScanner(file)
	lineNum := 0
	for scanner.Scan() {
		lineNum++
		line := strings.TrimSpace(scanner.Text())

		// Skip empty lines and comments
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		key, value, err := ParseFlag(line)
		if err != nil {
			return nil, fmt.Errorf("line %d: %w", lineNum, err)
		}
		vars[key] = value
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("reading env file: %w", err)
	}

	return vars, nil
}

// Substitute replaces all {{VAR_NAME}} placeholders in content with their values.
// Returns an error if any referenced variable is not defined.
func (v Vars) Substitute(content string) (string, error) {
	var missingVars []string

	result := varPattern.ReplaceAllStringFunc(content, func(match string) string {
		// Extract variable name from {{VAR_NAME}}
		name := match[2 : len(match)-2]
		if value, ok := v[name]; ok {
			return value
		}
		missingVars = append(missingVars, name)
		return match // Keep original if not found
	})

	if len(missingVars) > 0 {
		return "", fmt.Errorf("undefined variables: %s", strings.Join(missingVars, ", "))
	}

	return result, nil
}

// SubstituteWithDefaults replaces placeholders, using empty string for undefined vars.
// This is useful when you want to allow optional variables.
func (v Vars) SubstituteWithDefaults(content string) string {
	return varPattern.ReplaceAllStringFunc(content, func(match string) string {
		name := match[2 : len(match)-2]
		if value, ok := v[name]; ok {
			return value
		}
		return "" // Return empty for undefined
	})
}

// FindVariables returns all variable names referenced in the content.
func FindVariables(content string) []string {
	matches := varPattern.FindAllStringSubmatch(content, -1)
	seen := make(map[string]bool)
	var vars []string
	for _, m := range matches {
		name := m[1]
		if !seen[name] {
			seen[name] = true
			vars = append(vars, name)
		}
	}
	return vars
}

// Merge combines multiple Vars maps, with later maps taking precedence.
func Merge(varsList ...Vars) Vars {
	result := make(Vars)
	for _, vars := range varsList {
		for k, v := range vars {
			result[k] = v
		}
	}
	return result
}
