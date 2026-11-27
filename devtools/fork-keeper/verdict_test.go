package forkkeeper

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func sampleSHA(ch byte) string {
	return strings.Repeat(string(ch), 40)
}

func sampleRange() RangeInfo {
	base := sampleSHA('a')
	return RangeInfo{
		UpstreamOld:            sampleSHA('a'),
		UpstreamNew:            sampleSHA('b'),
		FirstRun:               false,
		UpToDate:               false,
		Rewritten:              false,
		ReviewBase:             &base,
		UserCommits:            []string{sampleSHA('c')},
		ForkHasMerges:          false,
		ForkBehindRecordedHead: false,
		MovedTags:              []MovedTag{},
		DeletedTags:            []string{},
	}
}

func sampleCleanVerdict() Verdict {
	return Verdict{
		Schema:        1,
		Verdict:       "clean",
		CandidateHead: sampleSHA('d'),
		Range:         sampleRange(),
		Conflicts:     []VerdictConflict{},
		Findings:      []VerdictFinding{},
		Summary:       "all clear",
	}
}

func mustJSON(t *testing.T, v any) []byte {
	t.Helper()
	b, err := json.Marshal(v)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func TestParseAndValidateVerdictValid(t *testing.T) {
	wantRange := sampleRange()
	wantHead := sampleSHA('d')

	v := sampleCleanVerdict()
	parsed, errs := ParseAndValidateVerdict(mustJSON(t, v), wantHead, &wantRange)
	if len(errs) != 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if parsed == nil || parsed.Verdict != VerdictClean || parsed.CandidateHead != wantHead {
		t.Fatalf("unexpected parsed verdict: %+v", parsed)
	}

	// Clean with confident conflict is allowed.
	v.Conflicts = []VerdictConflict{{File: "a.txt", Resolution: "kept ours", Confident: true}}
	if _, errs := ParseAndValidateVerdict(mustJSON(t, v), wantHead, &wantRange); len(errs) != 0 {
		t.Fatalf("expected clean with confident conflict to pass, got %v", errs)
	}

	// needs-review with first_run, findings, and unconfident conflict is valid.
	nrRange := sampleRange()
	nrRange.FirstRun = true
	nr := Verdict{
		Schema:        1,
		Verdict:       VerdictNeedsReview,
		CandidateHead: wantHead,
		Range:         nrRange,
		Conflicts:     []VerdictConflict{{File: "a.txt", Resolution: "unsure", Confident: false}},
		Findings:      []VerdictFinding{{Category: "supply-chain", Detail: "new URL"}},
		Summary:       "needs review",
	}
	if _, errs := ParseAndValidateVerdict(mustJSON(t, nr), wantHead, &nrRange); len(errs) != 0 {
		t.Fatalf("expected valid needs-review verdict, got %v", errs)
	}
}

func TestParseAndValidateVerdictRejections(t *testing.T) {
	wantRange := sampleRange()
	wantHead := sampleSHA('d')

	cases := []struct {
		name      string
		payload   string
		wantHead  string
		wantRange *RangeInfo
		wantSub   string
	}{
		{
			name:    "empty",
			payload: "   ",
			wantSub: "not exactly one JSON object",
		},
		{
			name:    "non-object",
			payload: "[]",
			wantSub: "not exactly one JSON object",
		},
		{
			name:    "trailing-json",
			payload: `{"schema":1}{"extra":true}`,
			wantSub: "not exactly one JSON object",
		},
		{
			name:     "bad-want-head",
			payload:  string(mustJSON(t, sampleCleanVerdict())),
			wantHead: "not-a-sha",
			wantSub:  "--candidate-head must be a 40-hex sha",
		},
		{
			name:     "wrong-schema",
			payload:  strings.Replace(string(mustJSON(t, sampleCleanVerdict())), `"schema":1`, `"schema":2`, 1),
			wantHead: wantHead,
			wantSub:  "schema must be 1",
		},
		{
			name:     "bad-verdict-value",
			payload:  strings.Replace(string(mustJSON(t, sampleCleanVerdict())), `"verdict":"clean"`, `"verdict":"approve"`, 1),
			wantHead: wantHead,
			wantSub:  `verdict must be "clean" or "needs-review"`,
		},
		{
			name:     "candidate-head-mismatch",
			payload:  string(mustJSON(t, sampleCleanVerdict())),
			wantHead: sampleSHA('e'),
			wantSub:  "!= expected",
		},
		{
			name:    "null-user-commits",
			payload: strings.Replace(string(mustJSON(t, sampleCleanVerdict())), `"user_commits":["`+sampleSHA('c')+`"]`, `"user_commits":null`, 1),
			wantSub: "range.user_commits must be an array",
		},
		{
			name: "unknown-finding-category",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Verdict = VerdictNeedsReview
				v.Findings = []VerdictFinding{{Category: "made-up"}}
				return v
			}())),
			wantSub: "unknown finding category: made-up",
		},
		{
			name: "clean-with-findings",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Findings = []VerdictFinding{{Category: "secret-leak"}}
				return v
			}())),
			wantSub: "clean verdict has findings",
		},
		{
			name: "clean-with-unconfident-conflict",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Conflicts = []VerdictConflict{{File: "x", Confident: false}}
				return v
			}())),
			wantSub: "unconfident conflict",
		},
		{
			name: "clean-with-missing-confident",
			payload: strings.Replace(string(mustJSON(t, sampleCleanVerdict())),
				`"conflicts":[]`, `"conflicts":[{"file":"x"}]`, 1),
			wantSub: "unconfident conflict",
		},
		{
			name: "clean-on-first-run",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Range.FirstRun = true
				return v
			}())),
			wantSub: "first review",
		},
		{
			name: "clean-on-rewritten",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Range.Rewritten = true
				return v
			}())),
			wantSub: "rewritten",
		},
		{
			name: "clean-on-fork-merges",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Range.ForkHasMerges = true
				return v
			}())),
			wantSub: "merge commits",
		},
		{
			name: "clean-on-fork-behind",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Range.ForkBehindRecordedHead = true
				return v
			}())),
			wantSub: "recorded upstream head",
		},
		{
			name: "clean-on-moved-tags",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Range.MovedTags = []MovedTag{{Name: "v1", Was: sampleSHA('a'), Now: sampleSHA('b')}}
				return v
			}())),
			wantSub: "tags moved",
		},
		{
			name: "clean-on-deleted-tags",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Range.DeletedTags = []string{"v0"}
				return v
			}())),
			wantSub: "tags were deleted",
		},
		{
			name: "range-mismatch",
			payload: string(mustJSON(t, func() Verdict {
				v := sampleCleanVerdict()
				v.Range.UpstreamNew = sampleSHA('f')
				return v
			}())),
			wantRange: &wantRange,
			wantSub:   "range does not match",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, errs := ParseAndValidateVerdict([]byte(tc.payload), tc.wantHead, tc.wantRange)
			if len(errs) == 0 {
				t.Fatalf("expected error containing %q, got none", tc.wantSub)
			}
			joined := strings.Join(errs, "\n")
			if !strings.Contains(joined, tc.wantSub) {
				t.Fatalf("errors %q do not contain %q", joined, tc.wantSub)
			}
		})
	}
}

func TestValidateVerdictFile(t *testing.T) {
	dir := t.TempDir()
	missing := filepath.Join(dir, "missing.json")
	if _, ok, msg := ValidateVerdictFile(missing, "", nil); ok || !strings.Contains(msg, "no such file") {
		t.Fatalf("expected missing file failure, got ok=%v msg=%q", ok, msg)
	}

	validPath := filepath.Join(dir, "verdict.json")
	r := sampleRange()
	v := sampleCleanVerdict()
	if err := os.WriteFile(validPath, mustJSON(t, v), 0o600); err != nil {
		t.Fatal(err)
	}
	got, ok, msg := ValidateVerdictFile(validPath, v.CandidateHead, &r)
	if !ok || msg != "" || got == nil {
		t.Fatalf("ValidateVerdictFile failed: ok=%v msg=%q got=%+v", ok, msg, got)
	}
}
