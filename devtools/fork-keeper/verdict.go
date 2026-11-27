package forkkeeper

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"slices"
	"strings"
)

const (
	// VerdictSchemaVersion is the supported verdict.json schema version.
	VerdictSchemaVersion = 1
	// VerdictClean indicates no threats or uncertain conflicts were found.
	VerdictClean = "clean"
	// VerdictNeedsReview indicates human review is required before promoting.
	VerdictNeedsReview = "needs-review"
)

// VerdictOutputSchema is the JSON Schema for the verdict.json contract.
const VerdictOutputSchema = `{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "required": ["schema", "verdict", "candidate_head", "range", "conflicts", "findings"],
  "properties": {
    "schema": { "type": "integer", "const": 1 },
    "verdict": { "type": "string", "enum": ["clean", "needs-review"] },
    "candidate_head": { "type": "string", "pattern": "^[0-9a-f]{40}$" },
    "range": {
      "type": "object",
      "required": [
        "upstream_old",
        "upstream_new",
        "first_run",
        "up_to_date",
        "rewritten",
        "review_base",
        "user_commits",
        "fork_has_merges",
        "fork_behind_recorded_head",
        "moved_tags",
        "deleted_tags"
      ],
      "properties": {
        "upstream_old": { "type": "string", "pattern": "^[0-9a-f]{40}$" },
        "upstream_new": { "type": "string", "pattern": "^[0-9a-f]{40}$" },
        "first_run": { "type": "boolean" },
        "up_to_date": { "type": "boolean" },
        "rewritten": { "type": "boolean" },
        "review_base": { "type": ["string", "null"] },
        "user_commits": { "type": "array", "items": { "type": "string", "pattern": "^[0-9a-f]{40}$" } },
        "fork_has_merges": { "type": "boolean" },
        "fork_behind_recorded_head": { "type": "boolean" },
        "moved_tags": { "type": "array" },
        "deleted_tags": { "type": "array", "items": { "type": "string" } }
      }
    },
    "conflicts": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["confident"],
        "properties": {
          "file": { "type": "string" },
          "resolution": { "type": "string" },
          "confident": { "type": "boolean" }
        }
      }
    },
    "findings": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["category"],
        "properties": {
          "category": {
            "type": "string",
            "enum": [
              "secret-leak",
              "destructive",
              "concealment",
              "supply-chain",
              "repo-signal",
              "agent-manipulation",
              "uncertain"
            ]
          },
          "severity": { "type": "string" },
          "file": { "type": "string" },
          "detail": { "type": "string" },
          "summary": { "type": "string" }
        }
      }
    },
    "summary": { "type": "string" }
  }
}`

// VerdictConflict describes a rebase conflict resolved by an agent.
type VerdictConflict struct {
	File       string `json:"file,omitempty"`
	Resolution string `json:"resolution,omitempty"`
	Confident  bool   `json:"confident"`
}

// VerdictFinding describes a threat or risk identified during review.
type VerdictFinding struct {
	Category string `json:"category"`
	Severity string `json:"severity,omitempty"`
	File     string `json:"file,omitempty"`
	Detail   string `json:"detail,omitempty"`
	Summary  string `json:"summary,omitempty"`
}

// Verdict represents the validated JSON verdict for a fork-keeper candidate.
type Verdict struct {
	Schema        int               `json:"schema"`
	Verdict       string            `json:"verdict"`
	CandidateHead string            `json:"candidate_head"` //nolint:tagliatelle // External verdict.json snake_case schema.
	Range         RangeInfo         `json:"range"`
	Conflicts     []VerdictConflict `json:"conflicts"`
	Findings      []VerdictFinding  `json:"findings"`
	Summary       string            `json:"summary,omitempty"`
}

type rawRangeInfo struct {
	UpstreamOld            *string     `json:"upstream_old"`              //nolint:tagliatelle // External verdict.json snake_case schema.
	UpstreamNew            *string     `json:"upstream_new"`              //nolint:tagliatelle // External verdict.json snake_case schema.
	FirstRun               *bool       `json:"first_run"`                 //nolint:tagliatelle // External verdict.json snake_case schema.
	UpToDate               *bool       `json:"up_to_date"`                //nolint:tagliatelle // External verdict.json snake_case schema.
	Rewritten              *bool       `json:"rewritten"`                 //nolint:tagliatelle // External verdict.json snake_case schema.
	ReviewBase             *string     `json:"review_base"`               //nolint:tagliatelle // External verdict.json snake_case schema.
	UserCommits            *[]string   `json:"user_commits"`              //nolint:tagliatelle // External verdict.json snake_case schema.
	ForkHasMerges          *bool       `json:"fork_has_merges"`           //nolint:tagliatelle // External verdict.json snake_case schema.
	ForkBehindRecordedHead *bool       `json:"fork_behind_recorded_head"` //nolint:tagliatelle // External verdict.json snake_case schema.
	MovedTags              *[]MovedTag `json:"moved_tags"`                //nolint:tagliatelle // External verdict.json snake_case schema.
	DeletedTags            *[]string   `json:"deleted_tags"`              //nolint:tagliatelle // External verdict.json snake_case schema.
}

type rawVerdictConflict struct {
	File       string `json:"file"`
	Resolution string `json:"resolution"`
	Confident  *bool  `json:"confident"`
}

type rawVerdictFinding struct {
	Category *string `json:"category"`
	Severity string  `json:"severity"`
	File     string  `json:"file"`
	Detail   string  `json:"detail"`
	Summary  string  `json:"summary"`
}

type rawVerdict struct {
	Schema        *int                  `json:"schema"`
	Verdict       *string               `json:"verdict"`
	CandidateHead *string               `json:"candidate_head"` //nolint:tagliatelle // External verdict.json snake_case schema.
	Range         *rawRangeInfo         `json:"range"`
	Conflicts     *[]rawVerdictConflict `json:"conflicts"`
	Findings      *[]rawVerdictFinding  `json:"findings"`
	Summary       string                `json:"summary"`
}

// IsValidSHA40 reports whether s is a lowercase 40-character hexadecimal git SHA.
func IsValidSHA40(s string) bool {
	if len(s) != 40 {
		return false
	}
	for i := 0; i < len(s); i++ {
		c := s[i]
		if (c < '0' || c > '9') && (c < 'a' || c > 'f') {
			return false
		}
	}
	return true
}

// IsValidFindingCategory reports whether cat is one of the accepted threat categories.
func IsValidFindingCategory(cat string) bool {
	switch cat {
	case "secret-leak", "destructive", "concealment", "supply-chain",
		"repo-signal", "agent-manipulation", "uncertain":
		return true
	default:
		return false
	}
}

// ValidateVerdictFile reads path and validates it against wantHead and wantRange.
// Any missing file, parse error, or contract violation returns (nilOrPartial, false, reasons).
func ValidateVerdictFile(path, wantHead string, wantRange *RangeInfo) (*Verdict, bool, string) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, false, fmt.Sprintf("no such file: %s", path)
	}
	v, errs := ParseAndValidateVerdict(data, wantHead, wantRange)
	if len(errs) > 0 {
		return v, false, strings.Join(errs, "\n")
	}
	return v, true, ""
}

// ParseAndValidateVerdict decodes data as a single JSON object and checks every
// contract invariant against optional wantHead and wantRange expectations.
func ParseAndValidateVerdict(data []byte, wantHead string, wantRange *RangeInfo) (*Verdict, []string) {
	if wantHead != "" && !IsValidSHA40(wantHead) {
		return nil, []string{fmt.Sprintf("--candidate-head must be a 40-hex sha, got %q", wantHead)}
	}

	trimmed := bytes.TrimSpace(data)
	if len(trimmed) == 0 || trimmed[0] != '{' {
		return nil, []string{"not exactly one JSON object"}
	}

	dec := json.NewDecoder(bytes.NewReader(trimmed))
	var raw rawVerdict
	if err := dec.Decode(&raw); err != nil {
		return nil, []string{fmt.Sprintf("could not evaluate verdict JSON: %v", err)}
	}
	var trailing any
	if err := dec.Decode(&trailing); !errors.Is(err, io.EOF) {
		return nil, []string{"not exactly one JSON object"}
	}

	errs := collectVerdictErrors(&raw, wantHead, wantRange)
	v := buildVerdict(&raw)
	if len(errs) > 0 {
		return v, errs
	}
	return v, nil
}

func collectVerdictErrors(raw *rawVerdict, wantHead string, wantRange *RangeInfo) []string {
	var errs []string
	if raw.Schema == nil || *raw.Schema != VerdictSchemaVersion {
		errs = append(errs, "schema must be 1")
	}
	verdictVal := derefString(raw.Verdict)
	if verdictVal != VerdictClean && verdictVal != VerdictNeedsReview {
		errs = append(errs, `verdict must be "clean" or "needs-review"`)
	}
	candHead := derefString(raw.CandidateHead)
	if !IsValidSHA40(candHead) {
		errs = append(errs, "candidate_head must be a 40-hex sha")
	}
	errs = append(errs, collectRangeErrors(raw.Range, wantRange)...)
	errs = append(errs, collectFindingErrors(raw.Findings)...)
	if raw.Conflicts == nil {
		errs = append(errs, "conflicts must be an array")
	}
	if wantHead != "" && candHead != wantHead {
		errs = append(errs, fmt.Sprintf("candidate_head %s != expected %s", candHead, wantHead))
	}
	if verdictVal == VerdictClean {
		errs = append(errs, collectCleanErrors(raw)...)
	}
	return errs
}

func collectFindingErrors(findings *[]rawVerdictFinding) []string {
	if findings == nil {
		return []string{"findings must be an array"}
	}
	var errs []string
	for _, f := range *findings {
		cat := derefString(f.Category)
		if !IsValidFindingCategory(cat) {
			errs = append(errs, fmt.Sprintf("unknown finding category: %s", cat))
		}
	}
	return errs
}

func collectRangeErrors(r *rawRangeInfo, wantRange *RangeInfo) []string {
	if r == nil {
		return []string{"range must be an object"}
	}
	var errs []string
	if !IsValidSHA40(derefString(r.UpstreamOld)) {
		errs = append(errs, "range.upstream_old must be a 40-hex sha")
	}
	if !IsValidSHA40(derefString(r.UpstreamNew)) {
		errs = append(errs, "range.upstream_new must be a 40-hex sha")
	}
	if r.UserCommits == nil || !allValidSHAs(*r.UserCommits) {
		errs = append(errs, "range.user_commits must be an array of 40-hex shas")
	}
	if r.MovedTags == nil {
		errs = append(errs, "range.moved_tags must be an array")
	}
	if r.DeletedTags == nil {
		errs = append(errs, "range.deleted_tags must be an array")
	}
	if wantRange != nil && !rawRangeEquals(r, *wantRange) {
		errs = append(errs, "range does not match the independently computed range")
	}
	return errs
}

func collectCleanErrors(raw *rawVerdict) []string {
	var errs []string
	if raw.Findings != nil && len(*raw.Findings) > 0 {
		errs = append(errs, "clean verdict has findings")
	}
	if raw.Conflicts != nil {
		for _, c := range *raw.Conflicts {
			if !derefBool(c.Confident) {
				errs = append(errs, "clean verdict has an unconfident conflict")
				break
			}
		}
	}
	return append(errs, collectCleanRangeErrors(raw.Range)...)
}

func collectCleanRangeErrors(r *rawRangeInfo) []string {
	if r == nil {
		return nil
	}
	var errs []string
	if r.Rewritten == nil || *r.Rewritten {
		errs = append(errs, "clean verdict but upstream history was rewritten")
	}
	if r.ForkHasMerges == nil || *r.ForkHasMerges {
		errs = append(errs, "clean verdict but fork has merge commits")
	}
	if r.FirstRun == nil || *r.FirstRun {
		errs = append(errs, "clean verdict on a first review")
	}
	if r.ForkBehindRecordedHead == nil || *r.ForkBehindRecordedHead {
		errs = append(errs, "clean verdict but fork does not contain the recorded upstream head")
	}
	if r.MovedTags == nil || len(*r.MovedTags) > 0 {
		errs = append(errs, "clean verdict but upstream tags moved")
	}
	if r.DeletedTags == nil || len(*r.DeletedTags) > 0 {
		errs = append(errs, "clean verdict but upstream tags were deleted")
	}
	return errs
}

func allValidSHAs(shas []string) bool {
	for _, s := range shas {
		if !IsValidSHA40(s) {
			return false
		}
	}
	return true
}

func rawRangeEquals(got *rawRangeInfo, want RangeInfo) bool {
	if !rawRangeHasRequiredFields(got) {
		return false
	}
	converted := buildRangeInfo(got)
	return converted.UpstreamOld == want.UpstreamOld &&
		converted.UpstreamNew == want.UpstreamNew &&
		converted.FirstRun == want.FirstRun &&
		converted.UpToDate == want.UpToDate &&
		converted.Rewritten == want.Rewritten &&
		converted.ForkHasMerges == want.ForkHasMerges &&
		converted.ForkBehindRecordedHead == want.ForkBehindRecordedHead &&
		stringPtrEqual(converted.ReviewBase, want.ReviewBase) &&
		slices.Equal(converted.UserCommits, want.UserCommits) &&
		slices.Equal(converted.MovedTags, want.MovedTags) &&
		slices.Equal(converted.DeletedTags, want.DeletedTags)
}

func rawRangeHasRequiredFields(r *rawRangeInfo) bool {
	return r != nil &&
		r.UpstreamOld != nil &&
		r.UpstreamNew != nil &&
		r.FirstRun != nil &&
		r.UpToDate != nil &&
		r.Rewritten != nil &&
		r.ForkHasMerges != nil &&
		r.ForkBehindRecordedHead != nil &&
		r.UserCommits != nil &&
		r.MovedTags != nil &&
		r.DeletedTags != nil
}

func stringPtrEqual(a, b *string) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	return *a == *b
}

func derefString(p *string) string {
	if p == nil {
		return ""
	}
	return *p
}

func derefBool(p *bool) bool {
	return p != nil && *p
}

func buildVerdict(raw *rawVerdict) *Verdict {
	v := &Verdict{
		Summary:       raw.Summary,
		CandidateHead: derefString(raw.CandidateHead),
		Verdict:       derefString(raw.Verdict),
		Range:         buildRangeInfo(raw.Range),
	}
	if raw.Schema != nil {
		v.Schema = *raw.Schema
	}
	if raw.Conflicts != nil {
		v.Conflicts = make([]VerdictConflict, 0, len(*raw.Conflicts))
		for _, c := range *raw.Conflicts {
			v.Conflicts = append(v.Conflicts, VerdictConflict{
				File:       c.File,
				Resolution: c.Resolution,
				Confident:  derefBool(c.Confident),
			})
		}
	}
	if raw.Findings != nil {
		v.Findings = make([]VerdictFinding, 0, len(*raw.Findings))
		for _, f := range *raw.Findings {
			v.Findings = append(v.Findings, VerdictFinding{
				Category: derefString(f.Category),
				Severity: f.Severity,
				File:     f.File,
				Detail:   f.Detail,
				Summary:  f.Summary,
			})
		}
	}
	return v
}

func buildRangeInfo(r *rawRangeInfo) RangeInfo {
	if r == nil {
		return RangeInfo{}
	}
	out := RangeInfo{
		UpstreamOld:            derefString(r.UpstreamOld),
		UpstreamNew:            derefString(r.UpstreamNew),
		FirstRun:               derefBool(r.FirstRun),
		UpToDate:               derefBool(r.UpToDate),
		Rewritten:              derefBool(r.Rewritten),
		ReviewBase:             r.ReviewBase,
		ForkHasMerges:          derefBool(r.ForkHasMerges),
		ForkBehindRecordedHead: derefBool(r.ForkBehindRecordedHead),
	}
	if r.UserCommits != nil {
		out.UserCommits = append([]string(nil), *r.UserCommits...)
	}
	if r.MovedTags != nil {
		out.MovedTags = append([]MovedTag(nil), *r.MovedTags...)
	}
	if r.DeletedTags != nil {
		out.DeletedTags = append([]string(nil), *r.DeletedTags...)
	}
	return out
}
