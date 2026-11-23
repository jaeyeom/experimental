// Package agentsmdsize checks per-file and cascading ancestor byte size
// limits for AGENTS.md files using devtools/pkg/baseline.
package agentsmdsize

import (
	"fmt"
	"io/fs"
	"path"
	"path/filepath"
	"sort"

	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
)

const (
	// CheckerName is the canonical checker identifier for baseline files.
	CheckerName = "agents-md-size"
	// DefaultMaxBytes is the default single-file size limit (4 KiB).
	DefaultMaxBytes int64 = 4096
	// DefaultMaxCascadingBytes is the default cascading ancestor size limit (16 KiB).
	DefaultMaxCascadingBytes int64 = 16384
	// MetricMaxBytes is the baseline metric name for single-file byte size.
	MetricMaxBytes = "max_bytes"
	// MetricMaxCascadingBytes is the baseline metric name for cascading byte size.
	MetricMaxCascadingBytes = "max_cascading_bytes"

	agentsFileName = "AGENTS.md"
)

// DefaultSkipDirs returns the default directory name patterns skipped during
// traversal.
func DefaultSkipDirs() []string {
	return []string{".git", ".tmp", "node_modules", "vendor", "bazel-*"}
}

// Config configures AGENTS.md size checking.
type Config struct {
	MaxBytes          int64
	MaxCascadingBytes int64
	Baseline          *baseline.File
	StrictBaseline    bool
	SkipDirs          []string
}

// Violation describes a single metric limit violation for an AGENTS.md file.
type Violation struct {
	Path       string
	Metric     string
	Actual     int64
	Limit      int64
	Default    int64
	MatchedKey string
}

// Report contains the results of checking AGENTS.md files under a directory.
type Report struct {
	FilesChecked    int
	Violations      []Violation
	StaleFindings   []baseline.StaleFinding
	StrictBaseline  bool
	eval            *baseline.Evaluator
	existingTargets map[string]bool
}

// Passed reports whether the check succeeded with no violations and, when
// StrictBaseline is enabled, no stale baseline findings.
func (r *Report) Passed() bool {
	if len(r.Violations) > 0 {
		return false
	}
	if r.StrictBaseline && len(r.StaleFindings) > 0 {
		return false
	}
	return true
}

// UpdatedBaseline returns a new baseline File updated according to mode using
// the observations collected during CheckDir.
func (r *Report) UpdatedBaseline(mode baseline.UpdateMode) *baseline.File {
	if r.eval == nil {
		return &baseline.File{
			Version: baseline.CurrentVersion,
			Checker: CheckerName,
			Entries: map[string]baseline.Entry{},
		}
	}
	out := r.eval.UpdatedFile(r.existingTargets, mode)
	if out.Checker == "" {
		out.Checker = CheckerName
	}
	return out
}

// CheckDir discovers all AGENTS.md files under root, evaluates per-file and
// cascading ancestor byte size limits against cfg, and returns a Report.
func CheckDir(root string, cfg Config) (*Report, error) {
	maxBytes := cfg.MaxBytes
	if maxBytes <= 0 {
		maxBytes = DefaultMaxBytes
	}
	maxCascading := cfg.MaxCascadingBytes
	if maxCascading <= 0 {
		maxCascading = DefaultMaxCascadingBytes
	}
	skipDirs := cfg.SkipDirs
	if skipDirs == nil {
		skipDirs = DefaultSkipDirs()
	}

	sizes, paths, err := discoverFiles(root, skipDirs)
	if err != nil {
		return nil, err
	}

	eval := baseline.NewEvaluator(cfg.Baseline)
	existing := make(map[string]bool, len(paths))
	var violations []Violation

	for _, rel := range paths {
		existing[rel] = true
		size := sizes[rel]
		cascading := cascadingSize(rel, sizes)

		if v, ok := checkMetric(eval, rel, MetricMaxBytes, size, maxBytes); ok {
			violations = append(violations, v)
		}
		if v, ok := checkMetric(eval, rel, MetricMaxCascadingBytes, cascading, maxCascading); ok {
			violations = append(violations, v)
		}
	}

	return &Report{
		FilesChecked:    len(paths),
		Violations:      violations,
		StaleFindings:   eval.StaleFindings(existing),
		StrictBaseline:  cfg.StrictBaseline,
		eval:            eval,
		existingTargets: existing,
	}, nil
}

func checkMetric(eval *baseline.Evaluator, rel, metric string, actual, defaultLimit int64) (Violation, bool) {
	res := eval.CheckMax(rel, metric, actual, defaultLimit)
	if res.Passed() {
		return Violation{}, false
	}
	return Violation{
		Path:       rel,
		Metric:     metric,
		Actual:     res.Actual,
		Limit:      res.Effective,
		Default:    res.Default,
		MatchedKey: res.MatchedKey,
	}, true
}

func discoverFiles(root string, skipDirs []string) (map[string]int64, []string, error) {
	sizes := map[string]int64{}
	var paths []string

	err := filepath.WalkDir(root, func(p string, d fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return fmt.Errorf("walk %s: %w", p, walkErr)
		}
		rel, err := filepath.Rel(root, p)
		if err != nil {
			return fmt.Errorf("relative path for %s: %w", p, err)
		}
		if d.IsDir() {
			if rel != "." && shouldSkipDir(d.Name(), skipDirs) {
				return filepath.SkipDir
			}
			return nil
		}
		if d.Name() != agentsFileName {
			return nil
		}
		info, err := d.Info()
		if err != nil {
			return fmt.Errorf("stat %s: %w", p, err)
		}
		relSlash := filepath.ToSlash(rel)
		sizes[relSlash] = info.Size()
		paths = append(paths, relSlash)
		return nil
	})
	if err != nil {
		return nil, nil, fmt.Errorf("discover %s files under %s: %w", agentsFileName, root, err)
	}

	sort.Strings(paths)
	return sizes, paths, nil
}

func shouldSkipDir(name string, patterns []string) bool {
	for _, pat := range patterns {
		if matched, err := path.Match(pat, name); err == nil && matched {
			return true
		}
	}
	return false
}

func cascadingSize(rel string, sizes map[string]int64) int64 {
	var total int64
	dir := path.Dir(rel)
	for {
		var candidate string
		if dir == "." {
			candidate = agentsFileName
		} else {
			candidate = path.Join(dir, agentsFileName)
		}
		if sz, ok := sizes[candidate]; ok {
			total += sz
		}
		if dir == "." {
			break
		}
		dir = path.Dir(dir)
	}
	return total
}
