package baseline

import (
	"reflect"
	"testing"
)

func TestEvaluatorCheckExempt(t *testing.T) {
	f := &File{
		Version: CurrentVersion,
		Checker: "test",
		Entries: map[string]Entry{
			"third_party/a.md": {},
			"**/testdata/**":   {Reason: "fixtures"},
			"docs/b.md":        {Limits: map[string]int64{"max_bytes": 5000}},
		},
	}
	eval := NewEvaluator(f)

	tests := []struct {
		name   string
		target string
		want   bool
	}{
		{name: "exact pure exemption", target: "third_party/a.md", want: true},
		{name: "unnormalized pure exemption", target: "./third_party/a.md", want: true},
		{name: "glob pure exemption", target: "pkg/testdata/x.md", want: true},
		{name: "numeric override not exempt", target: "docs/b.md", want: false},
		{name: "unmatched target not exempt", target: "src/main.go", want: false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := eval.CheckExempt(tc.target); got != tc.want {
				t.Errorf("CheckExempt(%q) = %v, want %v", tc.target, got, tc.want)
			}
		})
	}
}

func TestEvaluatorCheckMax(t *testing.T) {
	f := &File{
		Version: CurrentVersion,
		Checker: "agents-md-size",
		Entries: map[string]Entry{
			"exempt/AGENTS.md": {},
			"exact/AGENTS.md": {
				Limits: map[string]int64{"max_bytes": 6000},
			},
			"glob/**": {
				Limits: map[string]int64{"max_bytes": 7000},
			},
		},
	}

	tests := []struct {
		name       string
		target     string
		actual     int64
		defaultMax int64
		wantStatus Status
		wantExempt bool
		wantStale  bool
		wantRatch  bool
		wantRatchV int64
	}{
		{
			name:       "default pass",
			target:     "normal/AGENTS.md",
			actual:     3000,
			defaultMax: 4096,
			wantStatus: StatusPass,
		},
		{
			name:       "default fail",
			target:     "normal/AGENTS.md",
			actual:     4500,
			defaultMax: 4096,
			wantStatus: StatusFail,
		},
		{
			name:       "pure exemption passes even above default",
			target:     "exempt/AGENTS.md",
			actual:     99999,
			defaultMax: 4096,
			wantStatus: StatusPass,
			wantExempt: true,
		},
		{
			name:       "exact override at limit passes",
			target:     "exact/AGENTS.md",
			actual:     6000,
			defaultMax: 4096,
			wantStatus: StatusPass,
		},
		{
			name:       "exact override exceeded fails",
			target:     "exact/AGENTS.md",
			actual:     6001,
			defaultMax: 4096,
			wantStatus: StatusFail,
		},
		{
			name:       "exact override ratchet opportunity",
			target:     "exact/AGENTS.md",
			actual:     5000,
			defaultMax: 4096,
			wantStatus: StatusPass,
			wantRatch:  true,
			wantRatchV: 5000,
		},
		{
			name:       "exact override stale when satisfies default",
			target:     "exact/AGENTS.md",
			actual:     4096,
			defaultMax: 4096,
			wantStatus: StatusPass,
			wantStale:  true,
		},
		{
			name:       "glob override passes within glob limit",
			target:     "glob/sub/AGENTS.md",
			actual:     6500,
			defaultMax: 4096,
			wantStatus: StatusPass,
		},
		{
			name:       "glob override fails above glob limit",
			target:     "glob/sub/AGENTS.md",
			actual:     7001,
			defaultMax: 4096,
			wantStatus: StatusFail,
		},
		{
			name:       "glob override does not mark stale or ratchet",
			target:     "glob/sub/AGENTS.md",
			actual:     1000,
			defaultMax: 4096,
			wantStatus: StatusPass,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			eval := NewEvaluator(f)
			got := eval.CheckMax(tc.target, "max_bytes", tc.actual, tc.defaultMax)
			if got.Status != tc.wantStatus {
				t.Errorf("Status = %v, want %v", got.Status, tc.wantStatus)
			}
			if got.Exempt != tc.wantExempt {
				t.Errorf("Exempt = %v, want %v", got.Exempt, tc.wantExempt)
			}
			if got.Stale != tc.wantStale {
				t.Errorf("Stale = %v, want %v", got.Stale, tc.wantStale)
			}
			if got.CanRatchet != tc.wantRatch || got.RatchetTo != tc.wantRatchV {
				t.Errorf("Ratchet = (%v, %d), want (%v, %d)", got.CanRatchet, got.RatchetTo, tc.wantRatch, tc.wantRatchV)
			}
		})
	}
}

func TestEvaluatorCheckMin(t *testing.T) {
	f := &File{
		Version: CurrentVersion,
		Checker: "doc-coverage",
		Entries: map[string]Entry{
			"exempt/file.go": {},
			"exact/file.go": {
				Limits: map[string]int64{"min_doc_bytes": 50},
			},
			"glob/**": {
				Limits: map[string]int64{"min_doc_bytes": 20},
			},
		},
	}

	tests := []struct {
		name       string
		target     string
		actual     int64
		defaultMin int64
		wantStatus Status
		wantExempt bool
		wantStale  bool
		wantRatch  bool
		wantRatchV int64
	}{
		{
			name:       "default pass",
			target:     "normal/file.go",
			actual:     100,
			defaultMin: 100,
			wantStatus: StatusPass,
		},
		{
			name:       "default fail",
			target:     "normal/file.go",
			actual:     99,
			defaultMin: 100,
			wantStatus: StatusFail,
		},
		{
			name:       "pure exemption passes even below default",
			target:     "exempt/file.go",
			actual:     0,
			defaultMin: 100,
			wantStatus: StatusPass,
			wantExempt: true,
		},
		{
			name:       "exact override at limit passes",
			target:     "exact/file.go",
			actual:     50,
			defaultMin: 100,
			wantStatus: StatusPass,
		},
		{
			name:       "exact override below limit fails",
			target:     "exact/file.go",
			actual:     49,
			defaultMin: 100,
			wantStatus: StatusFail,
		},
		{
			name:       "exact override ratchet upward",
			target:     "exact/file.go",
			actual:     75,
			defaultMin: 100,
			wantStatus: StatusPass,
			wantRatch:  true,
			wantRatchV: 75,
		},
		{
			name:       "exact override stale when satisfies default",
			target:     "exact/file.go",
			actual:     100,
			defaultMin: 100,
			wantStatus: StatusPass,
			wantStale:  true,
		},
		{
			name:       "glob override passes",
			target:     "glob/a.go",
			actual:     25,
			defaultMin: 100,
			wantStatus: StatusPass,
		},
		{
			name:       "glob override fails",
			target:     "glob/a.go",
			actual:     19,
			defaultMin: 100,
			wantStatus: StatusFail,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			eval := NewEvaluator(f)
			got := eval.CheckMin(tc.target, "min_doc_bytes", tc.actual, tc.defaultMin)
			if got.Status != tc.wantStatus {
				t.Errorf("Status = %v, want %v", got.Status, tc.wantStatus)
			}
			if got.Exempt != tc.wantExempt {
				t.Errorf("Exempt = %v, want %v", got.Exempt, tc.wantExempt)
			}
			if got.Stale != tc.wantStale {
				t.Errorf("Stale = %v, want %v", got.Stale, tc.wantStale)
			}
			if got.CanRatchet != tc.wantRatch || got.RatchetTo != tc.wantRatchV {
				t.Errorf("Ratchet = (%v, %d), want (%v, %d)", got.CanRatchet, got.RatchetTo, tc.wantRatch, tc.wantRatchV)
			}
		})
	}
}

func TestEvaluatorStaleFindings(t *testing.T) {
	f := &File{
		Version: CurrentVersion,
		Checker: "mixed",
		Entries: map[string]Entry{
			"deleted.md":             {Limits: map[string]int64{"max_bytes": 5000}},
			"exempt-deleted.md":      {},
			"exempt-existing.md":     {},
			"now-passing.md":         {Limits: map[string]int64{"max_bytes": 5000}},
			"ratchet-down.md":        {Limits: map[string]int64{"max_bytes": 6000}},
			"ratchet-up.md":          {Limits: map[string]int64{"min_doc_bytes": 40}},
			"unchanged.md":           {Limits: map[string]int64{"max_bytes": 6000}},
			"matched-glob/**":        {Reason: "matched"},
			"unmatched-glob/**/*.go": {Reason: "unmatched"},
		},
	}
	eval := NewEvaluator(f)

	existing := map[string]bool{
		"exempt-existing.md": true,
		"now-passing.md":     true,
		"ratchet-down.md":    true,
		"ratchet-up.md":      true,
		"unchanged.md":       true,
		"matched-glob/a.md":  true,
	}

	eval.CheckExempt("exempt-existing.md")
	eval.CheckMax("now-passing.md", "max_bytes", 3000, 4096)
	eval.CheckMax("ratchet-down.md", "max_bytes", 5000, 4096)
	eval.CheckMin("ratchet-up.md", "min_doc_bytes", 70, 100)
	eval.CheckMax("unchanged.md", "max_bytes", 6000, 4096)
	eval.CheckExempt("matched-glob/a.md")

	got := eval.StaleFindings(existing)
	want := []StaleFinding{
		{Kind: StaleDeletedTarget, Key: "deleted.md"},
		{Kind: StaleDeletedTarget, Key: "exempt-deleted.md"},
		{Kind: StaleSatisfiesDefault, Key: "now-passing.md", Metric: "max_bytes", Current: 5000, Suggested: 4096},
		{Kind: StaleRatchet, Key: "ratchet-down.md", Metric: "max_bytes", Current: 6000, Suggested: 5000},
		{Kind: StaleRatchet, Key: "ratchet-up.md", Metric: "min_doc_bytes", Current: 40, Suggested: 70},
		{Kind: StaleUnmatchedGlob, Key: "unmatched-glob/**/*.go"},
	}

	if !reflect.DeepEqual(got, want) {
		t.Errorf("StaleFindings() mismatch:\ngot:  %+v\nwant: %+v", got, want)
	}
}

func TestNewEvaluatorNilFile(t *testing.T) {
	eval := NewEvaluator(nil)
	if eval.CheckExempt("a.md") {
		t.Error("CheckExempt on nil file = true, want false")
	}
	res := eval.CheckMax("a.md", "max_bytes", 5000, 4096)
	if res.Status != StatusFail {
		t.Errorf("CheckMax on nil file = %v, want StatusFail", res.Status)
	}
	if findings := eval.StaleFindings(map[string]bool{"a.md": true}); len(findings) != 0 {
		t.Errorf("StaleFindings on nil file = %+v, want empty", findings)
	}
}
