package approve

import (
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

const noneMineCodeowners = `/dash/** @me
/shared/** @acme/core
/solo/** @acme/solo
/other/** @acme/everyone
/bob/** @bob
/ext/** dev@example.com
`

func noneMineRule(mutate func(*config.ApproveRule)) config.ApproveRule {
	rule := config.ApproveRule{
		Name:               "not-mine",
		CodeownersNoneMine: true,
		NotDraft:           true,
	}
	if mutate != nil {
		mutate(&rule)
	}
	return rule
}

func noneMinePR(files ...models.File) github.ApprovalPullRequest {
	pr := prNumber(1)
	pr.BaseRef = "main"
	pr.Files = files
	pr.ChangedFiles = len(files)
	return pr
}

func noneMineFake(prs ...github.ApprovalPullRequest) *fakeClient {
	fake := newFake(prs...)
	fake.codeowners = noneMineCodeowners
	fake.teams = map[string][]string{
		"acme/everyone": {"bob", "carol"},
		"acme/core":     {"me", "bob"},
		"acme/solo":     {"me"},
	}
	return fake
}

func TestCodeownersNoneMinePasses(t *testing.T) {
	tests := map[string]struct {
		files []string
	}{
		"owned only by another team": {files: []string{"other/x.go"}},
		"owned only by another user": {files: []string{"bob/x.go"}},
		"no owners":                  {files: []string{"unowned/x.go"}},
		"mixed other team and none":  {files: []string{"other/x.go", "unowned/x.go"}},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			var files []models.File
			for _, path := range tt.files {
				files = append(files, models.File{Path: path})
			}
			pr := noneMinePR(files...)
			res := run(t, noneMineFake(pr), []config.ApproveRule{noneMineRule(nil)}, Options{Apply: true, Explain: true})
			if res.err != nil {
				t.Fatal(res.err)
			}
			cond := conditionResult(t, res.log[0].Rules[0], "codeowners_none_mine")
			if !cond.Pass {
				t.Fatalf("condition = %+v", cond)
			}
			if res.log[0].Action != ActionApproved || len(res.fake.approvals) != 1 {
				t.Fatalf("action = %q, approvals = %+v", res.log[0].Action, res.fake.approvals)
			}
		})
	}
}

func TestCodeownersNoneMineFailures(t *testing.T) {
	tests := map[string]struct {
		file   string
		teams  []string
		mutate func(*config.ApproveRule)
		edit   func(*fakeClient)
		want   []string
	}{
		"owned by you by name": {
			file: "dash/x.go",
			want: []string{"dash/x.go", "@me"},
		},
		"no paths_any_match escape": {
			file:   "dash/x.go",
			mutate: func(r *config.ApproveRule) { r.PathsAnyMatch = []string{"dash/**"} },
			want:   []string{"dash/x.go", "@me"},
		},
		"owned by your team even when requested with other members": {
			file:  "shared/x.go",
			teams: []string{"core"},
			want:  []string{"shared/x.go", "@acme/core"},
		},
		"owned by solo team": {
			file:  "solo/x.go",
			teams: []string{"solo"},
			want:  []string{"solo/x.go", "@acme/solo"},
		},
		"email owner fails closed": {
			file: "ext/x.go",
			want: []string{"ext/x.go", "dev@example.com"},
		},
		"unreadable team membership fails closed": {
			file:  "shared/x.go",
			teams: []string{"core"},
			edit:  func(f *fakeClient) { f.teamErr = map[string]error{"acme/core": errors.New("needs read:org")} },
			want:  []string{"shared/x.go", "@acme/core", "needs read:org"},
		},
		"unreadable CODEOWNERS fails closed": {
			file: "other/x.go",
			edit: func(f *fakeClient) { f.codeownersErr = errors.New("no CODEOWNERS file") },
			want: []string{"no CODEOWNERS file"},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			pr := noneMinePR(models.File{Path: tt.file})
			pr.RequestedTeams = tt.teams
			fake := noneMineFake(pr)
			if tt.edit != nil {
				tt.edit(fake)
			}
			res := run(t, fake, []config.ApproveRule{noneMineRule(tt.mutate)}, Options{Apply: true, Explain: true})
			if res.err != nil {
				t.Fatal(res.err)
			}
			cond := conditionResult(t, res.log[0].Rules[0], "codeowners_none_mine")
			if cond.Pass {
				t.Fatalf("condition passed unexpectedly: %+v", cond)
			}
			if res.log[0].Action != ActionManual || len(fake.approvals) != 0 {
				t.Fatalf("action = %q, approvals = %+v", res.log[0].Action, fake.approvals)
			}
			for _, want := range tt.want {
				if !strings.Contains(res.out, want) {
					t.Fatalf("explain output missing %q:\n%s", want, res.out)
				}
			}
		})
	}
}

func TestCodeownersNoneMineDefaultFalseLeavesBehaviorUnchanged(t *testing.T) {
	pr := noneMinePR(models.File{Path: "shared/x.go"})
	pr.RequestedTeams = []string{"core"}
	rule := config.ApproveRule{
		Name:              "covered-only",
		CodeownersCovered: true,
		NotDraft:          true,
	}
	res := run(t, noneMineFake(pr), []config.ApproveRule{rule}, Options{Apply: true, Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionApproved || len(res.fake.approvals) != 1 {
		t.Fatalf("codeowners_covered should pass when codeowners_none_mine is false: action=%q reason=%q", res.log[0].Action, res.log[0].Reason)
	}
	for _, cond := range res.log[0].Rules[0].Conditions {
		if cond.Condition == "codeowners_none_mine" {
			t.Fatalf("unexpected codeowners_none_mine condition when false: %+v", cond)
		}
	}

	plain := config.ApproveRule{Name: "plain", NotDraft: true}
	plainRes := run(t, noneMineFake(pr), []config.ApproveRule{plain}, Options{Apply: true})
	if plainRes.err != nil {
		t.Fatal(plainRes.err)
	}
	if plainRes.fake.codeownersReads != 0 {
		t.Fatalf("CODEOWNERS read %d times without codeowners condition", plainRes.fake.codeownersReads)
	}
}

func TestCodeownersNoneMineCombinedWithCodeownersCovered(t *testing.T) {
	both := noneMineRule(func(r *config.ApproveRule) {
		r.CodeownersCovered = true
		r.PathsAnyMatch = []string{"dash/**"}
	})

	t.Run("requested team with others fails via stricter check", func(t *testing.T) {
		pr := noneMinePR(models.File{Path: "dash/a.go"}, models.File{Path: "shared/x.go"})
		pr.RequestedTeams = []string{"core"}
		res := run(t, noneMineFake(pr), []config.ApproveRule{both}, Options{Apply: true, Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if res.log[0].Action != ActionManual || len(res.fake.approvals) != 0 {
			t.Fatalf("action = %q, approvals = %+v", res.log[0].Action, res.fake.approvals)
		}
		covered := conditionResult(t, res.log[0].Rules[0], "codeowners_covered")
		noneMine := conditionResult(t, res.log[0].Rules[0], "codeowners_none_mine")
		if !covered.Pass || noneMine.Pass {
			t.Fatalf("covered=%+v noneMine=%+v", covered, noneMine)
		}
		if !strings.Contains(res.out, "dash/a.go") || !strings.Contains(res.out, "@me") {
			t.Fatalf("explain missing blocking file/owner:\n%s", res.out)
		}
	})

	t.Run("other team matching paths_any_match passes both", func(t *testing.T) {
		bothOther := noneMineRule(func(r *config.ApproveRule) {
			r.CodeownersCovered = true
			r.PathsAnyMatch = []string{"other/**"}
		})
		pr := noneMinePR(models.File{Path: "other/x.go"})
		res := run(t, noneMineFake(pr), []config.ApproveRule{bothOther}, Options{Apply: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if res.log[0].Action != ActionApproved || len(res.fake.approvals) != 1 {
			t.Fatalf("action = %q, approvals = %+v", res.log[0].Action, res.fake.approvals)
		}
	})
}

func TestCodeownersNoneMineCombinesWithFileChecksAndOtherConditions(t *testing.T) {
	rule := noneMineRule(func(r *config.ApproveRule) {
		r.LabelsRequired = []string{"safe"}
		r.FileChecks = map[string]config.FileCheck{
			"dash/old.txt": {Builtin: config.FileCheckOnlyDeletions},
		}
	})

	t.Run("covered mine file passes file_checks and uncovered other file passes codeowners_none_mine", func(t *testing.T) {
		pr := noneMinePR(fcFile("dash/old.txt", 0, 3), fcFile("other/x.go", 1, 1))
		pr.Labels = []string{"safe"}
		res := run(t, noneMineFake(pr), []config.ApproveRule{rule}, Options{Apply: true, Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if res.log[0].Action != ActionApproved || len(res.fake.approvals) != 1 {
			t.Fatalf("action = %q, approvals = %+v, out:\n%s", res.log[0].Action, res.fake.approvals, res.out)
		}
		if !strings.Contains(res.out, "left to codeowners_none_mine, which passed") {
			t.Fatalf("explain missing uncovered note:\n%s", res.out)
		}
	})

	t.Run("uncovered team file fails codeowners_none_mine", func(t *testing.T) {
		pr := noneMinePR(fcFile("dash/old.txt", 0, 3), fcFile("shared/x.go", 1, 1))
		pr.Labels = []string{"safe"}
		pr.RequestedTeams = []string{"core"}
		res := run(t, noneMineFake(pr), []config.ApproveRule{rule}, Options{Apply: true, Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if res.log[0].Action != ActionManual || len(res.fake.approvals) != 0 {
			t.Fatalf("action = %q, approvals = %+v", res.log[0].Action, res.fake.approvals)
		}
		if !strings.Contains(res.out, "shared/x.go") || !strings.Contains(res.out, "@acme/core") {
			t.Fatalf("explain missing blocking file/owner:\n%s", res.out)
		}
	})

	t.Run("ANDed cheap condition failure skips CODEOWNERS load", func(t *testing.T) {
		pr := noneMinePR(fcFile("dash/old.txt", 0, 3), fcFile("other/x.go", 1, 1))
		// Missing required label "safe".
		fake := noneMineFake(pr)
		res := run(t, fake, []config.ApproveRule{rule}, Options{Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if fake.codeownersReads != 0 {
			t.Fatalf("CODEOWNERS read %d times despite cheap condition failure", fake.codeownersReads)
		}
		cond := conditionResult(t, res.log[0].Rules[0], "codeowners_none_mine")
		if !cond.NotEvaluated {
			t.Fatalf("expected NotEvaluated=true, got %+v", cond)
		}
	})
}
