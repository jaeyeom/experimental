// Package mapping loads and lints docsync.yml files.
package mapping

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/bmatcuk/doublestar/v4"
	"gopkg.in/yaml.v3"
)

// CurrentVersion is the only schema version this binary accepts.
const CurrentVersion = 1

// ErrNotFound is returned by Find when no docsync.yml exists in startDir or any parent.
var ErrNotFound = errors.New("docsync.yml not found")

// Mapping is a loaded docsync.yml plus the directory that contains it.
type Mapping struct {
	Version int
	Rules   []Rule
	Root    string
	Path    string
}

// Rule maps one or more globs to the docs they shadow.
type Rule struct {
	Match []string `yaml:"match"`
	Docs  []Doc    `yaml:"docs"`
}

// Doc is a documentation path, with optional section and reason.
type Doc struct {
	Path    string `yaml:"path"`
	Section string `yaml:"section,omitempty"`
	Why     string `yaml:"why,omitempty"`
}

// Severity is the lint/schema issue level.
type Severity string

const (
	// SeverityError is a hard mapping problem.
	SeverityError Severity = "error"
	// SeverityWarn is a non-fatal mapping smell.
	SeverityWarn Severity = "warn"
)

// Issue is one schema or lint finding.
type Issue struct {
	Severity Severity
	Message  string
}

type file struct {
	Version int    `yaml:"version"`
	Rules   []Rule `yaml:"rules"`
}

// ParseFile decodes path with KnownFields(true) and records Root/Path.
// It does not run SchemaIssues; version must still be CurrentVersion.
func ParseFile(path string) (Mapping, error) {
	abs, err := filepath.Abs(path)
	if err != nil {
		return Mapping{}, fmt.Errorf("resolve %s: %w", path, err)
	}
	f, err := os.Open(abs) //nolint:gosec // operator-supplied mapping path
	if err != nil {
		return Mapping{}, fmt.Errorf("open %s: %w", abs, err)
	}
	defer f.Close() //nolint:errcheck
	var decoded file
	dec := yaml.NewDecoder(f)
	dec.KnownFields(true)
	if err := dec.Decode(&decoded); err != nil {
		if errors.Is(err, io.EOF) {
			return Mapping{}, fmt.Errorf("decode %s: empty file", abs)
		}
		return Mapping{}, fmt.Errorf("decode %s: %w", abs, err)
	}
	if decoded.Version != CurrentVersion {
		return Mapping{}, fmt.Errorf("unsupported version %d (want %d)", decoded.Version, CurrentVersion)
	}
	for i := range decoded.Rules {
		for j, g := range decoded.Rules[i].Match {
			decoded.Rules[i].Match[j] = strings.TrimPrefix(g, "/")
		}
	}
	return Mapping{
		Version: decoded.Version,
		Rules:   decoded.Rules,
		Root:    filepath.Dir(abs),
		Path:    abs,
	}, nil
}

// Load parses path and fails on the first schema issue.
func Load(path string) (Mapping, error) {
	m, err := ParseFile(path)
	if err != nil {
		return Mapping{}, err
	}
	if issues := m.SchemaIssues(); len(issues) > 0 {
		return Mapping{}, errors.New(issues[0].Message)
	}
	return m, nil
}

// SchemaIssues reports required-field and glob problems. 0-based rule indices.
func (m Mapping) SchemaIssues() []Issue {
	var issues []Issue
	for i, rule := range m.Rules {
		if len(rule.Match) == 0 {
			issues = append(issues, Issue{Severity: SeverityError, Message: fmt.Sprintf("rule[%d].match must be non-empty", i)})
		}
		for _, g := range rule.Match {
			if msg := globError(g); msg != "" {
				issues = append(issues, Issue{Severity: SeverityError, Message: fmt.Sprintf("rule[%d].match %q %s", i, g, msg)})
			}
		}
		if len(rule.Docs) == 0 {
			issues = append(issues, Issue{Severity: SeverityError, Message: fmt.Sprintf("rule[%d].docs must be non-empty", i)})
		}
		for j, doc := range rule.Docs {
			if strings.TrimSpace(doc.Path) == "" {
				issues = append(issues, Issue{Severity: SeverityError, Message: fmt.Sprintf("rule[%d].docs[%d].path must be non-empty", i, j)})
			}
		}
	}
	return issues
}

func globError(pattern string) string {
	if hasBareDoublestar(pattern) {
		return "is not a valid glob (bare ** must be a path segment)"
	}
	if !doublestar.ValidatePattern(pattern) {
		return "is not a valid glob"
	}
	if _, err := doublestar.Match(pattern, "x"); err != nil {
		return "is not a valid glob"
	}
	return ""
}

func hasBareDoublestar(pattern string) bool {
	for i := 0; i < len(pattern); i++ {
		if i+1 < len(pattern) && pattern[i] == '*' && pattern[i+1] == '*' {
			prevOK := i == 0 || pattern[i-1] == '/'
			nextOK := i+2 == len(pattern) || pattern[i+2] == '/'
			if !prevOK || !nextOK {
				return true
			}
			i++
		}
	}
	return false
}

// Find walks up from startDir looking for docsync.yml.
func Find(startDir string) (string, error) {
	dir, err := filepath.Abs(startDir)
	if err != nil {
		return "", fmt.Errorf("resolve start dir: %w", err)
	}
	for {
		candidate := filepath.Join(dir, "docsync.yml")
		info, err := os.Stat(candidate)
		if err == nil && !info.IsDir() {
			return candidate, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", ErrNotFound
		}
		dir = parent
	}
}

// Relativize returns the slash-separated path of absPath relative to root.
// ok is false when absPath is outside root.
func Relativize(root, absPath string) (string, bool) {
	rel, err := filepath.Rel(root, absPath)
	if err != nil {
		return "", false
	}
	if rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return "", false
	}
	return filepath.ToSlash(rel), true
}

// Lint reports missing doc paths (error) and rules whose globs match no files (warn).
func (m Mapping) Lint() []Issue {
	files, err := listFiles(m.Root)
	if err != nil {
		return []Issue{{Severity: SeverityError, Message: err.Error()}}
	}
	var issues []Issue
	for i, rule := range m.Rules {
		for j, doc := range rule.Docs {
			p := filepath.Join(m.Root, filepath.FromSlash(doc.Path))
			if _, err := os.Stat(p); err != nil {
				issues = append(issues, Issue{
					Severity: SeverityError,
					Message:  fmt.Sprintf("rule[%d].docs[%d].path %q does not exist", i, j, doc.Path),
				})
			}
		}
		if !ruleMatchesAny(rule.Match, files) {
			encoded, encErr := json.Marshal(rule.Match)
			if encErr != nil {
				encoded = []byte("[]")
			}
			issues = append(issues, Issue{
				Severity: SeverityWarn,
				Message:  fmt.Sprintf("rule[%d].match %s matches no files currently tracked", i, encoded),
			})
		}
	}
	return issues
}

func listFiles(root string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == ".git" {
				return filepath.SkipDir
			}
			return nil
		}
		rel, ok := Relativize(root, path)
		if !ok {
			return nil
		}
		files = append(files, rel)
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("walk %s: %w", root, err)
	}
	return files, nil
}

func ruleMatchesAny(globs, files []string) bool {
	for _, file := range files {
		for _, g := range globs {
			ok, err := doublestar.Match(g, file)
			if err == nil && ok {
				return true
			}
		}
	}
	return false
}
