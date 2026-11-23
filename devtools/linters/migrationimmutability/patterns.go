package migrationimmutability

import (
	"fmt"
	"regexp"
	"strings"
)

// Pattern names a regular expression matched against repo-relative,
// forward-slash paths to recognize one framework's migration files.
type Pattern struct {
	Name string
	Re   *regexp.Regexp
}

// DefaultPatterns returns the built-in migration layouts. Runner code and
// config beside the migrations (env.py, __init__.py, migration_lock.toml,
// build files) does not match.
func DefaultPatterns() []Pattern {
	return []Pattern{
		{"django", regexp.MustCompile(`(^|/)migrations/\d{4}_[^/]+\.py$`)},
		{"alembic", regexp.MustCompile(`(^|/)migrations/versions/[^/]+\.py$`)},
		{"sql", regexp.MustCompile(`(^|/)migrations/[^/]+\.sql$`)},
		{"prisma", regexp.MustCompile(`(^|/)prisma/migrations/[^/]+/migration\.sql$`)},
	}
}

// ParsePattern parses a NAME=REGEXP flag value.
func ParsePattern(s string) (Pattern, error) {
	name, expr, ok := strings.Cut(s, "=")
	if !ok || name == "" || expr == "" {
		return Pattern{}, fmt.Errorf("pattern %q: want NAME=REGEXP", s)
	}
	re, err := regexp.Compile(expr)
	if err != nil {
		return Pattern{}, fmt.Errorf("pattern %q: %w", name, err)
	}
	return Pattern{Name: name, Re: re}, nil
}

// SelectPatterns returns the default patterns minus those named in disable,
// followed by extra. Unknown disable names and duplicate names are errors.
func SelectPatterns(extra []Pattern, disable []string) ([]Pattern, error) {
	defaults := DefaultPatterns()
	off := map[string]bool{}
	for _, name := range disable {
		found := false
		for _, p := range defaults {
			found = found || p.Name == name
		}
		if !found {
			return nil, fmt.Errorf("--disable-pattern %q: no such built-in pattern", name)
		}
		off[name] = true
	}
	var out []Pattern
	seen := map[string]bool{}
	for _, p := range append(defaults, extra...) {
		if off[p.Name] {
			continue
		}
		if seen[p.Name] {
			return nil, fmt.Errorf("duplicate pattern name %q", p.Name)
		}
		seen[p.Name] = true
		out = append(out, p)
	}
	return out, nil
}

// matchPattern returns the name of the first pattern matching path, or "".
func matchPattern(patterns []Pattern, path string) string {
	for _, p := range patterns {
		if p.Re.MatchString(path) {
			return p.Name
		}
	}
	return ""
}
