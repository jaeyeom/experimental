// Package migrationimmutability fails when a change edits, deletes, or
// renames a database migration file that the base branch already has.
//
// Once a migration merges, some environment may already have applied it,
// and migration runners record what they applied by name. Editing the file
// does not re-run it there, so a database that applied the old text
// silently drifts from a fresh one that applies the new text. A schema
// change belongs in a new migration.
//
// Reviewed exceptions are pinned in a devtools/pkg/baseline file as
// transitions: the exact blob before and after the change.
package migrationimmutability

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"

	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
)

// CheckerName is the canonical checker identifier for baseline files.
const CheckerName = "migration-immutability"

// Config configures Check.
type Config struct {
	// Root is any directory inside the repository.
	Root string
	// Base overrides base ref resolution when non-empty.
	Base string
	// Patterns recognize migration files.
	Patterns []Pattern
	// Excludes are baseline-syntax globs of paths never treated as
	// migrations.
	Excludes []string
	// Baseline holds the allowed transitions. Nil means none.
	Baseline *baseline.File
	// Getenv reads the environment; nil means os.Getenv.
	Getenv func(string) string
}

// Finding is a change to a migration path that the base already has, or a
// symlink at a migration path.
type Finding struct {
	Path    string
	Pattern string
	// Status is "added", "modified", "deleted", "type changed", or
	// "changed (git status X)".
	Status  string
	Symlink bool
	// Before and After are the blob hashes of the change (After may be
	// baseline.TransitionDeleted). Empty for symlinks.
	Before, After string
	// Allowed is the baseline transition that allows the change, if any.
	Allowed *baseline.Transition
}

// IsBlocked reports whether the finding fails the check.
func (f Finding) IsBlocked() bool {
	return f.Allowed == nil
}

// Report is the result of Check.
type Report struct {
	// TopLevel is the repository's top-level directory.
	TopLevel string
	// BaseRef is the ref the diff was taken against.
	BaseRef string
	// MergeBase is the commit diffed against.
	MergeBase string
	// Findings are sorted by path.
	Findings []Finding
}

// Blocked returns the findings that fail the check.
func (r *Report) Blocked() []Finding {
	var out []Finding
	for _, f := range r.Findings {
		if f.IsBlocked() {
			out = append(out, f)
		}
	}
	return out
}

// RecordTransitions adds a transition to bf for every blocked finding that
// a baseline can allow (everything but symlinks).
func (r *Report) RecordTransitions(bf *baseline.File, reason, issue string) error {
	if reason == "" {
		return errors.New("a reason is required to record exceptions")
	}
	for _, f := range r.Blocked() {
		if f.Symlink {
			continue
		}
		t := baseline.Transition{Before: f.Before, After: f.After, Reason: reason, Issue: issue}
		if err := bf.AddTransition(f.Path, t); err != nil {
			return fmt.Errorf("record %s: %w", f.Path, err)
		}
	}
	return nil
}

// TopLevel returns the top-level directory of the repository containing dir.
func TopLevel(dir string) (string, error) {
	top, err := git(dir, "rev-parse", "--show-toplevel")
	if err != nil {
		return "", fmt.Errorf("%s is not inside a git repository: %w", dir, err)
	}
	return top, nil
}

// Check diffs the working tree against the merge-base with the base branch
// and returns a finding for every change to an existing migration and every
// symlink at a migration path. It errors rather than passing vacuously when
// no base resolves or no tracked file is a migration.
func Check(cfg Config) (*Report, error) {
	if cfg.Getenv == nil {
		cfg.Getenv = os.Getenv
	}
	for _, g := range cfg.Excludes {
		if _, err := baseline.MatchGlob(g, "x"); err != nil {
			return nil, fmt.Errorf("--exclude %q: %w", g, err)
		}
	}
	top, err := TopLevel(cfg.Root)
	if err != nil {
		return nil, err
	}
	m := matcher{patterns: cfg.Patterns, excludes: cfg.Excludes}
	if err := requireMigrations(top, m); err != nil {
		return nil, err
	}
	ref, mergeBase, err := resolveBase(top, cfg.Base, cfg.Getenv)
	if err != nil {
		return nil, err
	}
	changes, err := changesSince(top, mergeBase)
	if err != nil {
		return nil, err
	}
	rep := &Report{TopLevel: top, BaseRef: ref, MergeBase: mergeBase}
	for _, c := range changes {
		f, ok, err := evaluate(top, mergeBase, m, cfg.Baseline, c)
		if err != nil {
			return nil, err
		}
		if ok {
			rep.Findings = append(rep.Findings, f)
		}
	}
	sort.Slice(rep.Findings, func(i, j int) bool { return rep.Findings[i].Path < rep.Findings[j].Path })
	return rep, nil
}

type matcher struct {
	patterns []Pattern
	excludes []string
}

// match returns the pattern name for path, or "" if it is not a guarded
// migration.
func (m matcher) match(path string) string {
	for _, g := range m.excludes {
		if ok, _ := baseline.MatchGlob(g, path); ok {
			return ""
		}
	}
	return matchPattern(m.patterns, path)
}

func requireMigrations(top string, m matcher) error {
	out, err := gitRaw(top, "ls-files", "-z")
	if err != nil {
		return err
	}
	for _, p := range splitZ(out) {
		if m.match(p) != "" {
			return nil
		}
	}
	return fmt.Errorf("no tracked file in %s matches the migration patterns (after excludes); "+
		"the check would pass vacuously. Add --pattern for this repository's layout", top)
}

// changesSince returns the changes between mergeBase and the working tree,
// including untracked (not ignored) files as additions. Renames are split
// into a deletion and an addition.
func changesSince(top, mergeBase string) ([]change, error) {
	out, err := gitRaw(top, "diff", "--name-status", "--no-renames", "-z", mergeBase, "--")
	if err != nil {
		return nil, err
	}
	changes, err := parseNameStatus(out)
	if err != nil {
		return nil, err
	}
	untracked, err := gitRaw(top, "ls-files", "--others", "--exclude-standard", "-z")
	if err != nil {
		return nil, err
	}
	for _, p := range splitZ(untracked) {
		changes = append(changes, change{Status: "A", Path: p})
	}
	return changes, nil
}

func describeStatus(s string) string {
	switch s {
	case "A":
		return "added"
	case "M":
		return "modified"
	case "D":
		return "deleted"
	case "T":
		return "type changed"
	default:
		return fmt.Sprintf("changed (git status %s)", s)
	}
}

func evaluate(top, mergeBase string, m matcher, bf *baseline.File, c change) (Finding, bool, error) {
	pattern := m.match(c.Path)
	if pattern == "" {
		return Finding{}, false, nil
	}
	f := Finding{Path: c.Path, Pattern: pattern, Status: describeStatus(c.Status)}
	if c.Status != "D" {
		info, err := os.Lstat(filepath.Join(top, filepath.FromSlash(c.Path)))
		if err != nil && !errors.Is(err, os.ErrNotExist) {
			return Finding{}, false, fmt.Errorf("stat %s: %w", c.Path, err)
		}
		if err == nil && info.Mode()&os.ModeSymlink != 0 {
			// A symlink's target is unguarded and could change what the
			// migration runs, so it is never allowed, not even when new.
			f.Symlink = true
			return f, true, nil
		}
	}
	if c.Status == "A" {
		return Finding{}, false, nil
	}
	before, err := git(top, "rev-parse", mergeBase+":"+c.Path)
	if err != nil {
		return Finding{}, false, err
	}
	f.Before = before
	if c.Status == "D" {
		f.After = baseline.TransitionDeleted
	} else {
		after, err := git(top, "hash-object", "--", c.Path)
		if err != nil {
			return Finding{}, false, err
		}
		f.After = after
	}
	if t, ok := bf.AllowsTransition(c.Path, f.Before, f.After); ok {
		f.Allowed = &t
	}
	return f, true, nil
}
