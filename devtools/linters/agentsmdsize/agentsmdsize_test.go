package agentsmdsize_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/agentsmdsize"
	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
)

func writeSizedFile(t *testing.T, root, rel string, size int) {
	t.Helper()
	full := filepath.Join(root, filepath.FromSlash(rel))
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", filepath.Dir(full), err)
	}
	data := bytes.Repeat([]byte("a"), size)
	if err := os.WriteFile(full, data, 0o644); err != nil {
		t.Fatalf("write %s: %v", full, err)
	}
}

func TestCheckDirSingleFileLimits(t *testing.T) {
	tests := []struct {
		name           string
		size           int
		wantViolations int
	}{
		{
			name:           "under limit",
			size:           4095,
			wantViolations: 0,
		},
		{
			name:           "at limit",
			size:           4096,
			wantViolations: 0,
		},
		{
			name:           "over limit",
			size:           4097,
			wantViolations: 1,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			root := t.TempDir()
			writeSizedFile(t, root, "AGENTS.md", tc.size)

			report, err := agentsmdsize.CheckDir(root, agentsmdsize.Config{
				StrictBaseline: true,
			})
			if err != nil {
				t.Fatalf("CheckDir: %v", err)
			}
			if report.FilesChecked != 1 {
				t.Fatalf("FilesChecked = %d, want 1", report.FilesChecked)
			}
			if len(report.Violations) != tc.wantViolations {
				t.Fatalf("len(Violations) = %d, want %d: %+v", len(report.Violations), tc.wantViolations, report.Violations)
			}
			if tc.wantViolations == 1 {
				v := report.Violations[0]
				if v.Path != "AGENTS.md" || v.Metric != agentsmdsize.MetricMaxBytes {
					t.Errorf("unexpected violation: %+v", v)
				}
				if v.Actual != int64(tc.size) || v.Limit != agentsmdsize.DefaultMaxBytes {
					t.Errorf("got actual=%d limit=%d, want actual=%d limit=%d", v.Actual, v.Limit, tc.size, agentsmdsize.DefaultMaxBytes)
				}
			}
		})
	}
}

func TestCheckDirCascadingLimits(t *testing.T) {
	tests := []struct {
		name           string
		files          map[string]int
		wantFiles      int
		wantViolations []string
	}{
		{
			name: "nested files within 4KB each and cascading within 16KB",
			files: map[string]int{
				"AGENTS.md":       4000,
				"a/AGENTS.md":     4000,
				"a/b/AGENTS.md":   4000,
				"a/b/c/AGENTS.md": 4000,
			},
			wantFiles: 4,
		},
		{
			name: "nested files within 4KB each but cascading exceeds 16KB",
			files: map[string]int{
				"AGENTS.md":         4000,
				"a/AGENTS.md":       4000,
				"a/b/AGENTS.md":     4000,
				"a/b/c/AGENTS.md":   4000,
				"a/b/c/d/AGENTS.md": 385,
			},
			wantFiles:      5,
			wantViolations: []string{"a/b/c/d/AGENTS.md"},
		},
		{
			name: "missing intermediate ancestor AGENTS.md is not counted",
			files: map[string]int{
				"AGENTS.md":           4000,
				"a/b/c/d/AGENTS.md":   4000,
				"a/b/c/d/e/AGENTS.md": 4000,
			},
			wantFiles: 3,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			root := t.TempDir()
			for rel, size := range tc.files {
				writeSizedFile(t, root, rel, size)
			}

			report, err := agentsmdsize.CheckDir(root, agentsmdsize.Config{
				StrictBaseline: true,
			})
			if err != nil {
				t.Fatalf("CheckDir: %v", err)
			}
			if report.FilesChecked != tc.wantFiles {
				t.Fatalf("FilesChecked = %d, want %d", report.FilesChecked, tc.wantFiles)
			}
			if len(report.Violations) != len(tc.wantViolations) {
				t.Fatalf("Violations = %+v, want paths %v", report.Violations, tc.wantViolations)
			}
			for i, wantPath := range tc.wantViolations {
				v := report.Violations[i]
				if v.Path != wantPath || v.Metric != agentsmdsize.MetricMaxCascadingBytes {
					t.Errorf("violation[%d] = %+v, want path=%s metric=%s", i, v, wantPath, agentsmdsize.MetricMaxCascadingBytes)
				}
			}
		})
	}
}

func TestCheckDirBaselineOverridesAndExemptions(t *testing.T) {
	root := t.TempDir()
	writeSizedFile(t, root, "AGENTS.md", 4096)
	writeSizedFile(t, root, "legacy/AGENTS.md", 5000)
	writeSizedFile(t, root, "legacy/sub/AGENTS.md", 4096)
	writeSizedFile(t, root, "legacy/sub/deep/AGENTS.md", 3000)
	writeSizedFile(t, root, "legacy/sub/deep/deeper/AGENTS.md", 4000)
	writeSizedFile(t, root, "exempt/AGENTS.md", 10000)
	writeSizedFile(t, root, "pkg/testdata/AGENTS.md", 9000)

	bl := &baseline.File{
		Version: baseline.CurrentVersion,
		Checker: agentsmdsize.CheckerName,
		Entries: map[string]baseline.Entry{
			"legacy/AGENTS.md": {
				Limits: map[string]int64{
					agentsmdsize.MetricMaxBytes: 5000,
				},
				Reason: "pending split",
				Issue:  "#1069",
			},
			"legacy/sub/deep/deeper/AGENTS.md": {
				Limits: map[string]int64{
					agentsmdsize.MetricMaxCascadingBytes: 20192,
				},
			},
			"exempt/AGENTS.md": {},
			"**/testdata/**": {
				Reason: "test fixtures",
			},
		},
	}

	report, err := agentsmdsize.CheckDir(root, agentsmdsize.Config{
		Baseline:       bl,
		StrictBaseline: true,
	})
	if err != nil {
		t.Fatalf("CheckDir: %v", err)
	}
	if !report.Passed() {
		t.Fatalf("expected pass, got violations=%+v stale=%+v", report.Violations, report.StaleFindings)
	}
}

func TestCheckDirStaleAndRatchet(t *testing.T) {
	root := t.TempDir()
	writeSizedFile(t, root, "AGENTS.md", 3000)
	writeSizedFile(t, root, "tighten/AGENTS.md", 4500)
	writeSizedFile(t, root, "violate/AGENTS.md", 5000)

	bl := &baseline.File{
		Version: baseline.CurrentVersion,
		Checker: agentsmdsize.CheckerName,
		Entries: map[string]baseline.Entry{
			"AGENTS.md": {
				Limits: map[string]int64{
					agentsmdsize.MetricMaxBytes: 5000,
				},
			},
			"tighten/AGENTS.md": {
				Limits: map[string]int64{
					agentsmdsize.MetricMaxBytes: 6000,
				},
				Reason: "keep metadata",
				Issue:  "#42",
			},
			"deleted/AGENTS.md": {
				Limits: map[string]int64{
					agentsmdsize.MetricMaxBytes: 5000,
				},
			},
			"nomatch/**": {},
		},
	}

	report, err := agentsmdsize.CheckDir(root, agentsmdsize.Config{
		Baseline:       bl,
		StrictBaseline: true,
	})
	if err != nil {
		t.Fatalf("CheckDir: %v", err)
	}
	if report.Passed() {
		t.Fatal("expected failure due to violation and stale findings")
	}
	if len(report.Violations) != 1 || report.Violations[0].Path != "violate/AGENTS.md" {
		t.Fatalf("unexpected violations: %+v", report.Violations)
	}
	if len(report.StaleFindings) != 4 {
		t.Fatalf("expected 4 stale findings, got %d: %+v", len(report.StaleFindings), report.StaleFindings)
	}

	tightened := report.UpdatedBaseline(baseline.ModeTighten)
	if _, ok := tightened.Entries["AGENTS.md"]; ok {
		t.Error("expected AGENTS.md to be removed in tighten mode")
	}
	if _, ok := tightened.Entries["deleted/AGENTS.md"]; ok {
		t.Error("expected deleted/AGENTS.md to be removed in tighten mode")
	}
	if _, ok := tightened.Entries["violate/AGENTS.md"]; ok {
		t.Error("expected violate/AGENTS.md NOT to be added in tighten mode")
	}
	if got := tightened.Entries["tighten/AGENTS.md"]; got.Limits[agentsmdsize.MetricMaxBytes] != 4500 || got.Reason != "keep metadata" || got.Issue != "#42" {
		t.Errorf("unexpected tightened entry: %+v", got)
	}
	if _, ok := tightened.Entries["nomatch/**"]; !ok {
		t.Error("expected manual glob nomatch/** to be preserved")
	}

	updated := report.UpdatedBaseline(baseline.ModeUpdate)
	if got := updated.Entries["violate/AGENTS.md"]; got.Limits[agentsmdsize.MetricMaxBytes] != 5000 {
		t.Errorf("expected violate/AGENTS.md to be added with 5000 in update mode, got %+v", got)
	}
}

func TestCheckDirSkipsDirectories(t *testing.T) {
	root := t.TempDir()
	writeSizedFile(t, root, "AGENTS.md", 100)
	writeSizedFile(t, root, ".git/AGENTS.md", 9000)
	writeSizedFile(t, root, ".tmp/AGENTS.md", 9000)
	writeSizedFile(t, root, "node_modules/pkg/AGENTS.md", 9000)
	writeSizedFile(t, root, "vendor/pkg/AGENTS.md", 9000)
	writeSizedFile(t, root, "bazel-out/AGENTS.md", 9000)
	writeSizedFile(t, root, "bazel-bin/AGENTS.md", 9000)

	report, err := agentsmdsize.CheckDir(root, agentsmdsize.Config{
		StrictBaseline: true,
	})
	if err != nil {
		t.Fatalf("CheckDir: %v", err)
	}
	if report.FilesChecked != 1 {
		t.Fatalf("FilesChecked = %d, want 1", report.FilesChecked)
	}
	if !report.Passed() {
		t.Fatalf("expected pass, got violations=%+v", report.Violations)
	}
}
