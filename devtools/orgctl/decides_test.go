package orgctl

import (
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func roleSpec(decides ...string) Spec {
	return Spec{Roles: map[string]Role{"coord/lead": {Decides: decides}}}
}

func TestDecidesRejectsApprovalVerbs(t *testing.T) {
	_, err := roleSpec("merge PRs").CheckDecides(nil)
	if err == nil || err.Error() != `role coord/lead: decides "merge PRs" contains approval word "merge"; approvals belong to the user, not a role` {
		t.Fatalf("merge: %v", err)
	}
	_, err = roleSpec("Approves releases").CheckDecides(nil)
	if err == nil || !strings.Contains(err.Error(), `approval word "approve"`) {
		t.Fatalf("Approves: %v", err)
	}
}

func TestDecidesAcceptsOrdinaryWork(t *testing.T) {
	warnings, err := roleSpec("triage incoming issues").CheckDecides(nil)
	if err != nil {
		t.Fatalf("triage: %v", err)
	}
	if len(warnings) != 0 {
		t.Fatalf("warnings = %q, want none", warnings)
	}
}

func TestDecidesCustomWordList(t *testing.T) {
	spec := roleSpec("ship the build")
	if _, err := spec.CheckDecides(nil); err != nil {
		t.Fatalf("without a custom word: %v", err)
	}
	spec.ApprovalWords = []string{"ship"}
	_, err := spec.CheckDecides(nil)
	if err == nil || !strings.Contains(err.Error(), `approval word "ship"`) {
		t.Fatalf("spec word: %v", err)
	}
	spec.ApprovalWords = nil
	_, err = spec.CheckDecides([]string{"ship"})
	if err == nil || !strings.Contains(err.Error(), `approval word "ship"`) {
		t.Fatalf("extra word: %v", err)
	}
}

func TestApprovalWordsFromPkl(t *testing.T) {
	stdout, stderr, code := evalPkl(t, filepath.Join(orgctlRoot(t), "pkl/testdata/approval-words.pkl"))
	if code != 0 {
		t.Fatalf("pkl exit %d\n%s", code, stderr)
	}
	spec, err := Decode(stdout)
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(spec.ApprovalWords, []string{"ship"}) || !slices.Equal(spec.AllowApprovalWords, []string{"merge"}) {
		t.Fatalf("words = %#v allow = %#v", spec.ApprovalWords, spec.AllowApprovalWords)
	}
	warnings, err := spec.CheckDecides(nil)
	if err != nil || len(warnings) != 1 || warnings[0] != `warning: allowApprovalWords "merge"` {
		t.Fatalf("warnings %q err %v", warnings, err)
	}
}

func TestAllowApprovalWordsWarns(t *testing.T) {
	spec := roleSpec("merge PRs")
	spec.AllowApprovalWords = []string{"merge"}
	warnings, err := spec.CheckDecides(nil)
	if err != nil {
		t.Fatalf("allowed merge: %v", err)
	}
	if len(warnings) != 1 || warnings[0] != `warning: allowApprovalWords "merge"` {
		t.Fatalf("warnings = %q", warnings)
	}
}
