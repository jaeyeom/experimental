package orglint

import (
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/devtools/linters/orglintparity"
)

// Finding is a normalized lint finding, printed as "file:line: checker: message".
type Finding = orglintparity.Finding

// Checker inspects a parsed document and reports findings. Checker.Name is
// used as the checker column of each finding.
type Checker struct {
	Name  string
	Check func(d *Document) []Finding
}

// Checkers is the list of enabled checkers.
func Checkers() []Checker {
	return append(append([]Checker{}, linkCheckers...), structureCheckers...)
}

// LintFile parses the named file and runs the checkers on it. Findings carry
// the name as given.
func LintFile(name string, checkers []Checker) ([]Finding, error) {
	f, err := os.Open(name)
	if err != nil {
		return nil, fmt.Errorf("open %s: %w", name, err)
	}
	defer f.Close()
	d, err := Parse(f)
	if err != nil {
		return nil, fmt.Errorf("parse %s: %w", name, err)
	}
	d.Dir = filepath.Dir(name)
	var out []Finding
	for _, c := range checkers {
		for _, fd := range c.Check(d) {
			fd.File, fd.Checker = name, c.Name
			out = append(out, fd)
		}
	}
	return out, nil
}

// Run lints each file and writes findings to w in normalized format. It
// reports whether any finding was written. Unreadable files are returned as
// an error after all files were tried.
func Run(w io.Writer, files []string, checkers []Checker) (found bool, err error) {
	var all []Finding
	var firstErr error
	for _, name := range files {
		fs, err := LintFile(name, checkers)
		if err != nil && firstErr == nil {
			firstErr = err
		}
		all = append(all, fs...)
	}
	if len(all) > 0 {
		if _, err := io.WriteString(w, orglintparity.Format(all)); err != nil {
			return true, fmt.Errorf("write findings: %w", err)
		}
	}
	return len(all) > 0, firstErr
}
