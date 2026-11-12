package approve

import (
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// handoffFake is the hand-off part of fakeClient. writes records every write
// call in order.
type handoffFake struct {
	codeowners      string
	codeownersErr   error
	codeownersReads int
	teams           map[string][]string
	teamErr         map[string]error
	timeline        map[int][]github.ReviewRequestEvent
	timelineErr     error
	requestErr      error
	removeErr       error
	writes          []string
}

func (f *handoffFake) CodeownersFile(_ github.PullRef, _ string) (string, string, error) {
	f.codeownersReads++
	if f.codeownersErr != nil {
		return "", "", f.codeownersErr
	}
	return "CODEOWNERS", f.codeowners, nil
}

func (f *handoffFake) TeamMembers(org, slug string) ([]string, error) {
	if err := f.teamErr[org+"/"+slug]; err != nil {
		return nil, err
	}
	members, ok := f.teams[org+"/"+slug]
	if !ok {
		return nil, errors.New("unexpected team " + org + "/" + slug)
	}
	return members, nil
}

func (f *handoffFake) ReviewRequestTimeline(ref github.PullRef) ([]github.ReviewRequestEvent, error) {
	if f.timelineErr != nil {
		return nil, f.timelineErr
	}
	return f.timeline[ref.Number], nil
}

func (f *handoffFake) RequestReviewers(_ github.PullRef, logins []string) error {
	f.writes = append(f.writes, "request "+strings.Join(logins, ","))
	return f.requestErr
}

func (f *handoffFake) RemoveRequestedReviewer(_ github.PullRef, login string) error {
	f.writes = append(f.writes, "remove "+login)
	return f.removeErr
}

const testCodeowners = `** @acme/everyone
/dash/** @me
/tools/dashboard/** @me @colleague
/shared/** @acme/core
/solo/** @acme/solo
/other/** @bob
/ext/** dev@example.com
`

var handoffRule = config.ApproveRule{
	Name:              "dash-handoff",
	Action:            config.ActionHandoff,
	Reviewers:         []string{"colleague"},
	PathsAnyMatch:     []string{"tools/dashboard/**"},
	CodeownersCovered: true,
	NotDraft:          true,
}

func handoffPR(paths ...string) github.ApprovalPullRequest {
	pr := prNumber(1)
	pr.BaseRef = "main"
	if len(paths) == 0 {
		paths = []string{"tools/dashboard/a.go"}
	}
	pr.Files = nil
	for _, path := range paths {
		pr.Files = append(pr.Files, models.File{Path: path})
	}
	pr.ChangedFiles = len(paths)
	return pr
}

func handoffFakeFor(prs ...github.ApprovalPullRequest) *fakeClient {
	fake := newFake(prs...)
	fake.codeowners = testCodeowners
	fake.teams = map[string][]string{
		"acme/everyone": {"bob", "carol"},
		"acme/core":     {"me", "bob"},
		"acme/solo":     {"me"},
	}
	fake.timeline = map[int][]github.ReviewRequestEvent{
		1: {{Event: github.ReviewRequested, Reviewer: "me", Actor: "alice", Time: "2026-11-01T00:00:00Z"}},
	}
	return fake
}

func coverage(t *testing.T, res runResult) ConditionResult {
	t.Helper()
	if len(res.log) != 1 {
		t.Fatalf("log = %+v", res.log)
	}
	for _, rule := range res.log[0].Rules {
		for _, cond := range rule.Conditions {
			if cond.Condition == "codeowners_covered" {
				return cond
			}
		}
	}
	t.Fatalf("no codeowners_covered condition: %+v", res.log[0].Rules)
	return ConditionResult{}
}

func TestHandoffCoverageBranchesPassOnTheirOwn(t *testing.T) {
	tests := map[string]struct {
		paths []string
		teams []string
		want  string
	}{
		"in paths_any_match":     {paths: []string{"tools/dashboard/a.go"}, want: "paths_any_match"},
		"not owned by you":       {paths: []string{"tools/dashboard/a.go", "other/x.go"}, want: "not owned by you"},
		"your team is requested": {paths: []string{"tools/dashboard/a.go", "shared/x.go"}, teams: []string{"core"}, want: "requested"},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			pr := handoffPR(tt.paths...)
			pr.RequestedTeams = tt.teams
			res := run(t, handoffFakeFor(pr), []config.ApproveRule{handoffRule}, Options{Explain: true})
			if res.err != nil {
				t.Fatal(res.err)
			}
			if cond := coverage(t, res); !cond.Pass {
				t.Fatalf("coverage = %+v", cond)
			}
			if res.log[0].Action != ActionWouldHandoff {
				t.Fatalf("action = %q reason %q", res.log[0].Action, res.log[0].Reason)
			}
			last := tt.paths[len(tt.paths)-1]
			if !strings.Contains(res.out, last) || !strings.Contains(res.out, tt.want) {
				t.Fatalf("explain output missing %q / %q:\n%s", last, tt.want, res.out)
			}
		})
	}
}

func TestHandoffCoverageFailures(t *testing.T) {
	tests := map[string]struct {
		file  string
		teams []string
		edit  func(*fakeClient)
		want  []string
	}{
		"owned by you alone":            {file: "dash/x.go", want: []string{"dash/x.go", "@me"}},
		"your team is not requested":    {file: "shared/x.go", want: []string{"shared/x.go", "@acme/core", "not a requested reviewer"}},
		"your team has no other member": {file: "solo/x.go", teams: []string{"solo"}, want: []string{"solo/x.go", "@acme/solo", "only member"}},
		"email owner":                   {file: "ext/x.go", want: []string{"ext/x.go", "dev@example.com"}},
		"team membership unreadable":    {file: "shared/x.go", teams: []string{"core"}, edit: func(f *fakeClient) { f.teamErr = map[string]error{"acme/core": errors.New("needs read:org")} }, want: []string{"shared/x.go", "read:org"}},
		"codeowners unreadable":         {file: "other/x.go", edit: func(f *fakeClient) { f.codeownersErr = errors.New("no CODEOWNERS file") }, want: []string{"no CODEOWNERS file"}},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			pr := handoffPR("tools/dashboard/a.go", tt.file)
			pr.RequestedTeams = tt.teams
			fake := handoffFakeFor(pr)
			if tt.edit != nil {
				tt.edit(fake)
			}
			res := run(t, fake, []config.ApproveRule{handoffRule}, Options{Explain: true, Apply: true})
			if res.err != nil {
				t.Fatal(res.err)
			}
			if cond := coverage(t, res); cond.Pass {
				t.Fatalf("coverage passed: %+v", cond)
			}
			if res.log[0].Action != ActionManual || len(fake.writes) != 0 {
				t.Fatalf("action = %q writes = %v", res.log[0].Action, fake.writes)
			}
			for _, want := range tt.want {
				if !strings.Contains(res.out, want) {
					t.Fatalf("explain output missing %q:\n%s", want, res.out)
				}
			}
		})
	}
}

func TestHandoffPathsAnyMatchNeedsOneFile(t *testing.T) {
	rule := handoffRule
	rule.CodeownersCovered = false
	res := run(t, handoffFakeFor(handoffPR("other/x.go")), []config.ApproveRule{rule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionManual || !strings.Contains(res.out, "paths_any_match") {
		t.Fatalf("action = %q out:\n%s", res.log[0].Action, res.out)
	}
	if res.fake.codeownersReads != 0 {
		t.Fatalf("CODEOWNERS read %d times without codeowners_covered", res.fake.codeownersReads)
	}
}

func TestHandoffApplyRequestsBeforeRemoving(t *testing.T) {
	pr := handoffPR()
	pr.RequestedTeams = []string{"core"}
	res := run(t, handoffFakeFor(pr), []config.ApproveRule{handoffRule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if got := strings.Join(res.fake.writes, "; "); got != "request colleague; remove me" {
		t.Fatalf("writes = %q", got)
	}
	if len(res.fake.approvals) != 0 {
		t.Fatalf("hand-off approved: %+v", res.fake.approvals)
	}
	entry := res.log[0]
	if entry.Action != ActionHandoff || entry.Rule == nil || *entry.Rule != "dash-handoff" ||
		strings.Join(entry.Reviewers, ",") != "colleague" || entry.HeadSHA != "h1" || len(entry.Timeline) != 1 {
		t.Fatalf("entry = %+v", entry)
	}
	if res.fake.headReads != 1 {
		t.Fatalf("head reads = %d", res.fake.headReads)
	}
}

func TestHandoffAddFailureDoesNotRemove(t *testing.T) {
	fake := handoffFakeFor(handoffPR())
	fake.requestErr = errors.New("HTTP 422")
	res := run(t, fake, []config.ApproveRule{handoffRule}, Options{Apply: true})
	if res.err == nil {
		t.Fatal("Run() error = nil")
	}
	if got := strings.Join(fake.writes, "; "); got != "request colleague" {
		t.Fatalf("writes = %q", got)
	}
	if entry := res.log[0]; entry.Action != ActionError || !strings.Contains(entry.Reason, "HTTP 422") {
		t.Fatalf("entry = %+v", entry)
	}
}

func TestHandoffRemoveFailureIsLoggedWithBothSteps(t *testing.T) {
	fake := handoffFakeFor(handoffPR())
	fake.removeErr = errors.New("HTTP 500")
	res := run(t, fake, []config.ApproveRule{handoffRule}, Options{Apply: true})
	if res.err == nil {
		t.Fatal("Run() error = nil")
	}
	if got := strings.Join(fake.writes, "; "); got != "request colleague; remove me" {
		t.Fatalf("writes = %q", got)
	}
	entry := res.log[0]
	if entry.Action != ActionError || !strings.Contains(entry.Reason, "HTTP 500") || !strings.Contains(entry.Reason, "colleague") {
		t.Fatalf("entry = %+v", entry)
	}
	steps := strings.Join(entry.Steps, "\n")
	if len(entry.Steps) != 2 || !strings.Contains(steps, "requested colleague") || !strings.Contains(steps, "HTTP 500") {
		t.Fatalf("steps = %q", entry.Steps)
	}
}

func TestHandoffSkips(t *testing.T) {
	tests := map[string]struct {
		edit func(*github.ApprovalPullRequest)
		want string
	}{
		"reviewer already requested": {func(pr *github.ApprovalPullRequest) { pr.RequestedUsers = append(pr.RequestedUsers, "Colleague") }, "colleague is already requested"},
		"reviewer already reviewed": {func(pr *github.ApprovalPullRequest) {
			pr.Reviews = append(pr.Reviews, github.ApprovalReview{Author: "colleague", State: "COMMENTED", CommitSHA: "h0"})
		}, "colleague already reviewed"},
		"you already reviewed": {func(pr *github.ApprovalPullRequest) {
			pr.Reviews = append(pr.Reviews, github.ApprovalReview{Author: "me", State: "COMMENTED", CommitSHA: "h1"})
		}, "you already reviewed"},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			pr := handoffPR()
			tt.edit(&pr)
			res := run(t, handoffFakeFor(pr), []config.ApproveRule{handoffRule}, Options{Apply: true})
			if res.err != nil {
				t.Fatal(res.err)
			}
			if len(res.fake.writes) != 0 {
				t.Fatalf("writes = %v", res.fake.writes)
			}
			if entry := res.log[0]; entry.Action != ActionSkipped || !strings.Contains(entry.Reason, tt.want) {
				t.Fatalf("entry = %+v", entry)
			}
		})
	}
}

func TestHandoffLoopGuardKeepsReRequestedPR(t *testing.T) {
	fake := handoffFakeFor(handoffPR())
	fake.timeline[1] = []github.ReviewRequestEvent{
		{Event: github.ReviewRequested, Reviewer: "me", Actor: "alice", Time: "2026-11-01T00:00:00Z"},
		{Event: github.ReviewRequested, Reviewer: "colleague", Actor: "me", Time: "2026-11-02T00:00:00Z"},
		{Event: github.ReviewRequestRemoved, Reviewer: "me", Actor: "me", Time: "2026-11-02T00:00:01Z"},
		{Event: github.ReviewRequested, Reviewer: "me", Actor: "alice", Time: "2026-11-03T00:00:00Z"},
	}
	res := run(t, fake, []config.ApproveRule{handoffRule}, Options{Apply: true, Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(fake.writes) != 0 || fake.headReads != 0 {
		t.Fatalf("writes = %v head reads = %d", fake.writes, fake.headReads)
	}
	entry := res.log[0]
	if entry.Action != ActionManual || entry.Reason != "re-requested after hand-off" {
		t.Fatalf("entry = %+v", entry)
	}
	if len(entry.Timeline) != 3 || entry.LoopGuard == "" {
		t.Fatalf("timeline evidence = %+v loop guard %q", entry.Timeline, entry.LoopGuard)
	}
	if !strings.Contains(res.out, "loop guard") || !strings.Contains(res.out, "re-requested after hand-off") {
		t.Fatalf("out:\n%s", res.out)
	}
}

func TestHandoffLoopGuardIgnoresRemovalByOthers(t *testing.T) {
	fake := handoffFakeFor(handoffPR())
	fake.timeline[1] = []github.ReviewRequestEvent{
		{Event: github.ReviewRequested, Reviewer: "me", Actor: "alice", Time: "2026-11-01T00:00:00Z"},
		{Event: github.ReviewRequestRemoved, Reviewer: "me", Actor: "alice", Time: "2026-11-02T00:00:00Z"},
		{Event: github.ReviewRequested, Reviewer: "me", Actor: "alice", Time: "2026-11-03T00:00:00Z"},
	}
	res := run(t, fake, []config.ApproveRule{handoffRule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionWouldHandoff {
		t.Fatalf("entry = %+v", res.log[0])
	}
}

func TestHandoffTimelineErrorFailsLoudly(t *testing.T) {
	fake := handoffFakeFor(handoffPR())
	fake.timelineErr = errors.New("gh: HTTP 502")
	res := run(t, fake, []config.ApproveRule{handoffRule}, Options{Apply: true})
	if res.err == nil {
		t.Fatal("Run() error = nil")
	}
	if len(fake.writes) != 0 || res.log[0].Action != ActionError || !strings.Contains(res.log[0].Reason, "HTTP 502") {
		t.Fatalf("writes = %v entry = %+v", fake.writes, res.log[0])
	}
}

func TestHandoffDryRunMakesNoWrites(t *testing.T) {
	res := run(t, handoffFakeFor(handoffPR()), []config.ApproveRule{handoffRule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.writes) != 0 || res.fake.headReads != 0 || len(res.fake.approvals) != 0 {
		t.Fatalf("writes = %v head reads = %d approvals = %v", res.fake.writes, res.fake.headReads, res.fake.approvals)
	}
	entry := res.log[0]
	if entry.Action != ActionWouldHandoff || strings.Join(entry.Reviewers, ",") != "colleague" || entry.HeadSHA != "h1" {
		t.Fatalf("entry = %+v", entry)
	}
	if !strings.Contains(res.out, "would_handoff") || !strings.Contains(res.out, "dash-handoff") || !strings.Contains(res.out, "colleague") {
		t.Fatalf("out:\n%s", res.out)
	}
}

func TestHandoffSkipsWhenHeadMoved(t *testing.T) {
	fake := handoffFakeFor(handoffPR())
	fake.heads = map[int]string{1: "h2"}
	res := run(t, fake, []config.ApproveRule{handoffRule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(fake.writes) != 0 {
		t.Fatalf("writes = %v", fake.writes)
	}
	if entry := res.log[0]; entry.Action != ActionSkipped || !strings.Contains(entry.Reason, "head moved") {
		t.Fatalf("entry = %+v", entry)
	}
}

func TestHandoffNeverActsOnOwnPR(t *testing.T) {
	pr := handoffPR()
	pr.Author = "me"
	res := run(t, handoffFakeFor(pr), []config.ApproveRule{handoffRule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.writes) != 0 || res.log[0].Action != ActionSkipped {
		t.Fatalf("writes = %v entry = %+v", res.fake.writes, res.log[0])
	}
}
