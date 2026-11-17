package approve

import (
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

const mineCodeowners = `** @acme/everyone
/deploy/** @acme/core
/deploy/app.go @acme/core @bob
/infra/** @acme/infra
`

const mineBody = "Comment-only change in {my_files} ({my_teams}); approving for those files only. Rule {rule}."

var mineRule = config.ApproveRule{Name: "my-comments", MineCommentsOnly: true, NotDraft: true, Body: mineBody}

const (
	yamlCommentPatch = "@@ -1,2 +1,2 @@\n-# old note\n+# new note\n key: v\n"
	yamlCodePatch    = "@@ -1,2 +1,2 @@\n # note\n-replicas: 1\n+replicas: 2\n"
	goCodePatch      = "@@ -1 +1 @@\n-x := 1\n+x := 2\n"
)

func patchFile(path, patch string) github.PullFile {
	adds, dels := 0, 0
	for _, line := range strings.Split(patch, "\n") {
		switch {
		case strings.HasPrefix(line, "+"):
			adds++
		case strings.HasPrefix(line, "-"):
			dels++
		}
	}
	return github.PullFile{Path: path, Status: "modified", Patch: patch, PatchSet: true, Additions: adds, Deletions: dels}
}

// minePR returns a pull request changing files with the given patches, and
// a fake that serves mineCodeowners.
func minePR(files ...github.PullFile) (github.ApprovalPullRequest, *fakeClient) {
	pr := prNumber(1)
	pr.BaseRef = "main"
	pr.Files = nil
	for _, file := range files {
		pr.Files = append(pr.Files, models.File{Path: file.Path})
	}
	pr.ChangedFiles = len(files)
	fake := newFake(pr)
	fake.codeowners = mineCodeowners
	fake.teams = map[string][]string{
		"acme/everyone": {"bob", "carol"},
		"acme/core":     {"me", "bob"},
		"acme/infra":    {"me", "dave"},
	}
	fake.patches = map[int][]github.PullFile{1: files}
	return pr, fake
}

func mineCondition(t *testing.T, res runResult) ConditionResult {
	t.Helper()
	if len(res.log) != 1 {
		t.Fatalf("log = %+v", res.log)
	}
	return conditionResult(t, res.log[0].Rules[0], "mine_comments_only")
}

func TestMineCommentsOnlyYAMLCommentPassesAndBodyListsFile(t *testing.T) {
	_, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch), patchFile("src/main.go", goCodePatch))
	res := run(t, fake, []config.ApproveRule{mineRule}, Options{Apply: true, Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if cond := mineCondition(t, res); !cond.Pass {
		t.Fatalf("condition = %+v", cond)
	}
	want := "Comment-only change in deploy/values.yaml (@acme/core); approving for those files only. Rule my-comments."
	if len(fake.approvals) != 1 || fake.approvals[0].body != want {
		t.Fatalf("approvals = %+v", fake.approvals)
	}
	if !strings.Contains(res.out, "pass deploy/values.yaml (/deploy/** @acme/core): comment or blank lines only") {
		t.Fatalf("explain does not list my file:\n%s", res.out)
	}
}

func TestMineCommentsOnlyFailures(t *testing.T) {
	tests := map[string]struct {
		files []github.PullFile
		want  string
	}{
		"owned file changes code": {
			files: []github.PullFile{patchFile("deploy/values.yaml", yamlCodePatch)},
			want:  "deploy/values.yaml:+2 is code",
		},
		"owned file deletes code": {
			files: []github.PullFile{patchFile("deploy/values.yaml", "@@ -1,2 +1 @@\n # note\n-replicas: 1\n")},
			want:  "deploy/values.yaml:-2 is code",
		},
		"another team of the user changes code": {
			files: []github.PullFile{
				patchFile("deploy/values.yaml", yamlCommentPatch),
				patchFile("infra/run.sh", "@@ -1 +1 @@\n-echo a\n+echo b\n"),
			},
			want: "infra/run.sh:+1 is code",
		},
		"jointly owned file changes code": {
			files: []github.PullFile{patchFile("deploy/values.yaml", yamlCommentPatch), patchFile("deploy/app.go", goCodePatch)},
			want:  "deploy/app.go:+1 is code",
		},
		"no owned file": {
			files: []github.PullFile{patchFile("src/main.go", goCodePatch)},
			want:  "no changed file is owned by you or your teams",
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			_, fake := minePR(tt.files...)
			res := run(t, fake, []config.ApproveRule{mineRule}, Options{Apply: true})
			cond := mineCondition(t, res)
			if cond.Pass || !strings.Contains(cond.Saw, tt.want) {
				t.Fatalf("condition = %+v, want failure mentioning %q", cond, tt.want)
			}
			if len(fake.approvals) != 0 {
				t.Fatalf("approved %+v", fake.approvals)
			}
		})
	}
}

func TestMineCommentsOnlyDeletedCommentPasses(t *testing.T) {
	_, fake := minePR(patchFile("deploy/values.yaml", "@@ -1,2 +1 @@\n-# stale note\n key: v\n"))
	res := run(t, fake, []config.ApproveRule{mineRule}, Options{})
	if cond := mineCondition(t, res); !cond.Pass {
		t.Fatalf("condition = %+v", cond)
	}
}

func TestMineCommentsOnlyUnreadableDataFailsWithReason(t *testing.T) {
	t.Run("codeowners", func(t *testing.T) {
		_, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
		fake.codeownersErr = errors.New("no CODEOWNERS file on main")
		res := run(t, fake, []config.ApproveRule{mineRule}, Options{Explain: true})
		cond := mineCondition(t, res)
		if cond.Pass || !strings.Contains(res.out, "FAIL mine_comments_only: unreadable: no CODEOWNERS file on main") {
			t.Fatalf("condition = %+v, out:\n%s", cond, res.out)
		}
	})
	t.Run("team", func(t *testing.T) {
		_, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
		fake.teamErr = map[string]error{"acme/core": errors.New("needs read:org")}
		res := run(t, fake, []config.ApproveRule{mineRule}, Options{Explain: true})
		cond := mineCondition(t, res)
		if cond.Pass || !strings.Contains(res.out, "FAIL mine_comments_only: unreadable: team data: @acme/core: needs read:org") {
			t.Fatalf("condition = %+v, out:\n%s", cond, res.out)
		}
	})
}

func teamOnlyPR() (github.ApprovalPullRequest, *fakeClient) {
	pr, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
	pr.RequestedUsers = nil
	pr.RequestedTeams = []string{"core"}
	fake.prs[1] = pr
	return pr, fake
}

func TestTeamOnlyRequestSkippedWithoutOptIn(t *testing.T) {
	_, fake := teamOnlyPR()
	rule := mineRule
	res := run(t, fake, []config.ApproveRule{rule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.log) != 0 || len(fake.approvals) != 0 || fake.teamSearched != 0 {
		t.Fatalf("log = %+v, approvals = %+v, team searches = %d", res.log, fake.approvals, fake.teamSearched)
	}
}

func TestTeamOnlyRequestEvaluatedWithOptIn(t *testing.T) {
	_, fake := teamOnlyPR()
	team := mineRule
	team.Name = "team-comments"
	team.RequestedViaTeam = true
	// other would match too, but does not opt in, so it still skips the
	// team-only request.
	other := config.ApproveRule{Name: "other", NotDraft: true}
	res := run(t, fake, []config.ApproveRule{other, team}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.teamSearched != 1 || fake.searched != 0 {
		t.Fatalf("searches: team %d, individual %d", fake.teamSearched, fake.searched)
	}
	if len(res.log) != 1 || res.log[0].Rule == nil || *res.log[0].Rule != "team-comments" || res.log[0].Action != ActionApproved {
		t.Fatalf("log = %+v", res.log)
	}
	for _, result := range res.log[0].Rules {
		if result.Rule == "other" {
			t.Fatalf("rule other was evaluated: %+v", result)
		}
	}
	if len(fake.approvals) != 1 {
		t.Fatalf("approvals = %+v", fake.approvals)
	}
}

func TestTeamOnlyRequestNotMemberIsSkipped(t *testing.T) {
	pr, fake := teamOnlyPR()
	pr.RequestedTeams = []string{"everyone"}
	fake.prs[1] = pr
	team := mineRule
	team.RequestedViaTeam = true
	res := run(t, fake, []config.ApproveRule{team}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.log) != 0 || len(fake.approvals) != 0 {
		t.Fatalf("log = %+v, approvals = %+v", res.log, fake.approvals)
	}
}

func TestTeamOnlyRequestNoMatchIsSkippedNotManual(t *testing.T) {
	pr, fake := teamOnlyPR()
	fake.patches[1] = []github.PullFile{patchFile("deploy/values.yaml", yamlCodePatch)}
	fake.prs[1] = pr
	team := mineRule
	team.RequestedViaTeam = true
	res := run(t, fake, []config.ApproveRule{team}, Options{PR: "acme/widgets#1"})
	if len(res.log) != 1 || res.log[0].Action != ActionSkipped ||
		!strings.Contains(res.log[0].Reason, "requested only through @acme/core") {
		t.Fatalf("log = %+v", res.log)
	}
}

func TestRuleBodyTemplate(t *testing.T) {
	plain := config.ApproveRule{Name: "plain", NotDraft: true, Body: "Fine by me"}
	res := run(t, newFake(prNumber(1)), []config.ApproveRule{plain}, Options{Apply: true})
	if got := res.fake.approvals[0].body; got != "Fine by me\n\n<!-- gh-approve: plain -->" {
		t.Fatalf("plain rule body = %q", got)
	}
	global := config.ApproveRule{Name: "global", NotDraft: true}
	res = run(t, newFake(prNumber(1)), []config.ApproveRule{global}, Options{Apply: true})
	if got := res.fake.approvals[0].body; got != "LGTM\n\n<!-- gh-approve: global -->" {
		t.Fatalf("global body = %q", got)
	}
	named := config.ApproveRule{Name: "named", NotDraft: true, Body: "Approved by {rule}"}
	res = run(t, newFake(prNumber(1)), []config.ApproveRule{named}, Options{Apply: true})
	if got := res.fake.approvals[0].body; got != "Approved by named" {
		t.Fatalf("templated body = %q", got)
	}
}
