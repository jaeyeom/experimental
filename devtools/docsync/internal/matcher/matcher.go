// Package matcher computes implicated docs from a mapping and changed files.
package matcher

import (
	"strings"

	"github.com/bmatcuk/doublestar/v4"
	"github.com/jaeyeom/experimental/devtools/docsync/internal/mapping"
)

// Affected is one implicated doc plus the files that triggered it.
type Affected struct {
	Path        string   `json:"path"`
	Section     string   `json:"section,omitempty"`
	Why         string   `json:"why,omitempty"`
	TriggeredBy []string `json:"triggered_by"` //nolint:tagliatelle // spec JSON shape
}

// Result is the matcher output, also the check --json document.
type Result struct {
	Affected       []Affected `json:"affected"`
	ChangedFiles   []string   `json:"changed_files"`   //nolint:tagliatelle // spec JSON shape
	UnmatchedFiles []string   `json:"unmatched_files"` //nolint:tagliatelle // spec JSON shape
}

type pair struct {
	path    string
	section string
}

// Match reports the union of docs implicated by changed, with no I/O.
func Match(m mapping.Mapping, changed []string) Result {
	order := make([]pair, 0)
	seen := make(map[pair]*Affected)
	hit := make([]bool, len(changed))

	for _, rule := range m.Rules {
		for i, file := range changed {
			if !matchesAny(rule.Match, file) {
				continue
			}
			hit[i] = true
			for _, doc := range rule.Docs {
				k := pair{path: doc.Path, section: doc.Section}
				if a, ok := seen[k]; ok {
					if !contains(a.TriggeredBy, file) {
						a.TriggeredBy = append(a.TriggeredBy, file)
					}
					continue
				}
				a := &Affected{
					Path:        doc.Path,
					Section:     doc.Section,
					Why:         doc.Why,
					TriggeredBy: []string{file},
				}
				seen[k] = a
				order = append(order, k)
			}
		}
	}

	affected := make([]Affected, 0, len(order))
	for _, k := range order {
		affected = append(affected, *seen[k])
	}
	unmatched := make([]string, 0)
	for i, file := range changed {
		if !hit[i] {
			unmatched = append(unmatched, file)
		}
	}
	copied := make([]string, len(changed))
	copy(copied, changed)
	return Result{Affected: affected, ChangedFiles: copied, UnmatchedFiles: unmatched}
}

func matchesAny(globs []string, file string) bool {
	file = strings.TrimPrefix(file, "/")
	for _, g := range globs {
		g = strings.TrimPrefix(g, "/")
		ok, err := doublestar.Match(g, file)
		if err == nil && ok {
			return true
		}
	}
	return false
}

func contains(ss []string, v string) bool {
	for _, s := range ss {
		if s == v {
			return true
		}
	}
	return false
}
