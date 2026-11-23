package baseline

import (
	"reflect"
	"testing"
)

func TestUpdatedFileModeUpdate(t *testing.T) {
	orig := &File{
		Version: CurrentVersion,
		Checker: "agents-md-size",
		Entries: map[string]Entry{
			"deleted.md": {
				Limits: map[string]int64{"max_bytes": 5000},
				Reason: "gone",
			},
			"exempt.md": {
				Reason: "generated",
				Issue:  "#100",
			},
			"now-passing.md": {
				Limits: map[string]int64{"max_bytes": 5000},
				Reason: "fixed",
			},
			"ratchet-down.md": {
				Limits: map[string]int64{"max_bytes": 6000},
				Reason: "improving",
				Issue:  "#101",
			},
			"ratchet-up.md": {
				Limits: map[string]int64{"min_doc_bytes": 30},
				Reason: "adding docs",
				Issue:  "#102",
			},
			"worsened.md": {
				Limits: map[string]int64{"max_bytes": 5000},
				Reason: "legacy",
				Issue:  "#103",
			},
			"partial-stale.md": {
				Limits: map[string]int64{
					"max_bytes":           5000,
					"max_cascading_bytes": 20000,
				},
				Reason: "split pending",
				Issue:  "#104",
			},
			"**/testdata/**": {
				Reason: "test fixtures",
				Issue:  "#105",
			},
		},
	}

	eval := NewEvaluator(orig)
	existing := map[string]bool{
		"exempt.md":        true,
		"now-passing.md":   true,
		"ratchet-down.md":  true,
		"ratchet-up.md":    true,
		"worsened.md":      true,
		"partial-stale.md": true,
		"new-failing.md":   true,
	}

	eval.CheckExempt("exempt.md")
	eval.CheckMax("now-passing.md", "max_bytes", 3500, 4096)
	eval.CheckMax("ratchet-down.md", "max_bytes", 5200, 4096)
	eval.CheckMin("ratchet-up.md", "min_doc_bytes", 60, 100)
	eval.CheckMax("worsened.md", "max_bytes", 6500, 4096)
	eval.CheckMax("partial-stale.md", "max_bytes", 3000, 4096)
	eval.CheckMax("partial-stale.md", "max_cascading_bytes", 19000, 16384)
	eval.CheckMax("new-failing.md", "max_bytes", 4800, 4096)

	got := eval.UpdatedFile(existing, ModeUpdate)
	want := &File{
		Version: CurrentVersion,
		Checker: "agents-md-size",
		Entries: map[string]Entry{
			"exempt.md": {
				Reason: "generated",
				Issue:  "#100",
			},
			"ratchet-down.md": {
				Limits: map[string]int64{"max_bytes": 5200},
				Reason: "improving",
				Issue:  "#101",
			},
			"ratchet-up.md": {
				Limits: map[string]int64{"min_doc_bytes": 60},
				Reason: "adding docs",
				Issue:  "#102",
			},
			"worsened.md": {
				Limits: map[string]int64{"max_bytes": 6500},
				Reason: "legacy",
				Issue:  "#103",
			},
			"partial-stale.md": {
				Limits: map[string]int64{"max_cascading_bytes": 19000},
				Reason: "split pending",
				Issue:  "#104",
			},
			"new-failing.md": {
				Limits: map[string]int64{"max_bytes": 4800},
			},
			"**/testdata/**": {
				Reason: "test fixtures",
				Issue:  "#105",
			},
		},
	}

	if !reflect.DeepEqual(got, want) {
		t.Errorf("UpdatedFile(ModeUpdate) mismatch:\ngot:  %+v\nwant: %+v", got, want)
	}
}

func TestUpdatedFileModeTighten(t *testing.T) {
	orig := &File{
		Version: CurrentVersion,
		Checker: "agents-md-size",
		Entries: map[string]Entry{
			"deleted.md": {
				Limits: map[string]int64{"max_bytes": 5000},
			},
			"now-passing.md": {
				Limits: map[string]int64{"max_bytes": 5000},
			},
			"ratchet-down.md": {
				Limits: map[string]int64{"max_bytes": 6000},
				Reason: "improving",
				Issue:  "#101",
			},
			"worsened-max.md": {
				Limits: map[string]int64{"max_bytes": 5000},
				Reason: "must not loosen",
			},
			"worsened-min.md": {
				Limits: map[string]int64{"min_doc_bytes": 50},
				Reason: "must not loosen min",
			},
			"unmatched-glob/**": {
				Reason: "keep manual glob",
			},
		},
	}

	eval := NewEvaluator(orig)
	existing := map[string]bool{
		"now-passing.md":  true,
		"ratchet-down.md": true,
		"worsened-max.md": true,
		"worsened-min.md": true,
		"new-failing.md":  true,
	}

	eval.CheckMax("now-passing.md", "max_bytes", 3000, 4096)
	eval.CheckMax("ratchet-down.md", "max_bytes", 5000, 4096)
	eval.CheckMax("worsened-max.md", "max_bytes", 7000, 4096)
	eval.CheckMin("worsened-min.md", "min_doc_bytes", 20, 100)
	eval.CheckMax("new-failing.md", "max_bytes", 8000, 4096)

	got := eval.UpdatedFile(existing, ModeTighten)
	want := &File{
		Version: CurrentVersion,
		Checker: "agents-md-size",
		Entries: map[string]Entry{
			"ratchet-down.md": {
				Limits: map[string]int64{"max_bytes": 5000},
				Reason: "improving",
				Issue:  "#101",
			},
			"worsened-max.md": {
				Limits: map[string]int64{"max_bytes": 5000},
				Reason: "must not loosen",
			},
			"worsened-min.md": {
				Limits: map[string]int64{"min_doc_bytes": 50},
				Reason: "must not loosen min",
			},
			"unmatched-glob/**": {
				Reason: "keep manual glob",
			},
		},
	}

	if !reflect.DeepEqual(got, want) {
		t.Errorf("UpdatedFile(ModeTighten) mismatch:\ngot:  %+v\nwant: %+v", got, want)
	}
}
