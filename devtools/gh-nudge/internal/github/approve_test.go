package github

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestViewerLogin(t *testing.T) {
	exec := &routeExec{route: map[string]string{"viewer": `{"data":{"viewer":{"login":"me"}}}`}}
	login, err := NewClientWithExecutor(exec).ViewerLogin()
	if err != nil || login != "me" {
		t.Fatalf("ViewerLogin() = %q, %v", login, err)
	}
}

func TestSearchUserReviewRequestedUsesIndividualQualifier(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"user-review-requested:@me -f after=CUR": `{"data":{"search":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[
			{"number": 3, "url": "https://github.com/acme/gadgets/pull/3", "repository": {"nameWithOwner": "acme/gadgets"}}
		]}}}`,
		"user-review-requested:@me": `{"data":{"search":{"pageInfo":{"hasNextPage":true,"endCursor":"CUR"},"nodes":[
			{"number": 12, "url": "https://github.com/acme/widgets/pull/12", "repository": {"nameWithOwner": "acme/widgets"}},
			{}
		]}}}`,
	}}
	refs, err := NewClientWithExecutor(exec).SearchUserReviewRequested()
	if err != nil {
		t.Fatal(err)
	}
	if len(refs) != 2 || refs[0].Owner != "acme" || refs[0].Repo != "widgets" || refs[0].Number != 12 ||
		refs[1].Repo != "gadgets" || refs[1].Number != 3 {
		t.Fatalf("refs = %+v", refs)
	}
	if !strings.Contains(exec.calls[0], "q=is:pr is:open archived:false user-review-requested:@me") {
		t.Fatalf("search call = %s", exec.calls[0])
	}
}

func TestSearchUserReviewRequestedReportsGraphQLError(t *testing.T) {
	exec := &routeExec{route: map[string]string{"search": `{"errors":[{"message":"rate limited"}]}`}}
	if _, err := NewClientWithExecutor(exec).SearchUserReviewRequested(); err == nil || !strings.Contains(err.Error(), "rate limited") {
		t.Fatalf("error = %v", err)
	}
}

const approvalPRFixture = `{"data":{"repository":{"nameWithOwner":"acme/widgets","pullRequest":{
	"number": 12,
	"url": "https://github.com/acme/widgets/pull/12",
	"title": "Tidy dashboard",
	"state": "OPEN",
	"isDraft": false,
	"headRefOid": "head1",
	"author": {"login": "alice"},
	"additions": 5,
	"deletions": 2,
	"changedFiles": 2,
	"files": {"pageInfo": {"hasNextPage": false}, "nodes": [
		{"path": "dash/a.go", "additions": 4, "deletions": 1},
		{"path": "dash/b.go", "additions": 1, "deletions": 1}
	]},
	"labels": {"pageInfo": {"hasNextPage": false}, "nodes": [{"name": "internal-only"}]},
	"reviewRequests": {"nodes": [
		{"requestedReviewer": {"__typename": "User", "login": "me"}},
		{"requestedReviewer": {"__typename": "Team", "slug": "core"}}
	]},
	"reviews": {"nodes": [
		{"author": {"login": "review-bot"}, "state": "APPROVED", "commit": {"oid": "head1"}}
	]},
	"reviewThreads": {"totalCount": 2, "nodes": [{"isResolved": false}, {"isResolved": true}]},
	"commits": {"nodes": [{"commit": {"oid": "head1", "statusCheckRollup": {"contexts": {"pageInfo": {"hasNextPage": false}, "nodes": [
		{"__typename": "CheckRun", "name": "ci", "status": "COMPLETED", "conclusion": "SUCCESS", "isRequired": true},
		{"__typename": "CheckRun", "name": "slow", "status": "IN_PROGRESS", "conclusion": "", "isRequired": false},
		{"__typename": "StatusContext", "context": "lint", "state": "FAILURE", "isRequired": false}
	]}}}}]}
}}}}`

func TestLoadApprovalPullRequest(t *testing.T) {
	exec := &routeExec{route: map[string]string{"reviewThreads": approvalPRFixture}}
	pr, err := NewClientWithExecutor(exec).LoadApprovalPullRequest(PullRef{Owner: "acme", Repo: "widgets", Number: 12})
	if err != nil {
		t.Fatal(err)
	}
	if pr.Ref.Owner != "acme" || pr.Ref.Repo != "widgets" || pr.Ref.Number != 12 || pr.Title != "Tidy dashboard" ||
		pr.Author != "alice" || pr.HeadSHA != "head1" || pr.State != "OPEN" || pr.Draft {
		t.Fatalf("pr = %+v", pr)
	}
	if pr.Additions != 5 || pr.Deletions != 2 || pr.ChangedFiles != 2 || len(pr.Files) != 2 || pr.Files[0].Path != "dash/a.go" || pr.FilesErr != "" {
		t.Fatalf("files = %+v", pr)
	}
	if strings.Join(pr.Labels, ",") != "internal-only" || pr.LabelsErr != "" {
		t.Fatalf("labels = %+v", pr.Labels)
	}
	if strings.Join(pr.RequestedUsers, ",") != "me" || strings.Join(pr.RequestedTeams, ",") != "core" {
		t.Fatalf("requests = %v %v", pr.RequestedUsers, pr.RequestedTeams)
	}
	if len(pr.Reviews) != 1 || pr.Reviews[0] != (ApprovalReview{Author: "review-bot", State: "APPROVED", CommitSHA: "head1"}) {
		t.Fatalf("reviews = %+v", pr.Reviews)
	}
	if pr.UnresolvedThreads != 1 || pr.ThreadsErr != "" {
		t.Fatalf("threads = %d %q", pr.UnresolvedThreads, pr.ThreadsErr)
	}
	want := []ApprovalCheck{
		{Name: "ci", Required: true, Outcome: models.ChecksPass},
		{Name: "slow", Outcome: models.ChecksPending},
		{Name: "lint", Outcome: models.ChecksFail},
	}
	if pr.ChecksErr != "" || len(pr.Checks) != len(want) {
		t.Fatalf("checks = %+v %q", pr.Checks, pr.ChecksErr)
	}
	for i := range want {
		if pr.Checks[i] != want[i] {
			t.Fatalf("check %d = %+v, want %+v", i, pr.Checks[i], want[i])
		}
	}
	if !strings.Contains(exec.calls[0], "number=12") || !strings.Contains(exec.calls[0], "owner=acme") {
		t.Fatalf("call = %s", exec.calls[0])
	}
}

func TestLoadApprovalPullRequestMarksUnreadableData(t *testing.T) {
	var raw map[string]any
	if err := json.Unmarshal([]byte(approvalPRFixture), &raw); err != nil {
		t.Fatal(err)
	}
	pull := raw["data"].(map[string]any)["repository"].(map[string]any)["pullRequest"].(map[string]any)
	tests := map[string]struct {
		edit  func(pr map[string]any)
		check func(pr ApprovalPullRequest) string
	}{
		"threads missing": {
			edit:  func(pr map[string]any) { pr["reviewThreads"] = nil },
			check: func(pr ApprovalPullRequest) string { return pr.ThreadsErr },
		},
		"threads truncated": {
			edit:  func(pr map[string]any) { pr["reviewThreads"] = map[string]any{"totalCount": 150, "nodes": []any{}} },
			check: func(pr ApprovalPullRequest) string { return pr.ThreadsErr },
		},
		"rollup missing": {
			edit: func(pr map[string]any) {
				pr["commits"] = map[string]any{"nodes": []any{map[string]any{"commit": map[string]any{"oid": "head1", "statusCheckRollup": nil}}}}
			},
			check: func(pr ApprovalPullRequest) string { return pr.ChecksErr },
		},
		"commit missing": {
			edit:  func(pr map[string]any) { pr["commits"] = map[string]any{"nodes": []any{}} },
			check: func(pr ApprovalPullRequest) string { return pr.ChecksErr },
		},
		"rollup on another commit": {
			edit: func(pr map[string]any) {
				pr["commits"] = map[string]any{"nodes": []any{map[string]any{"commit": map[string]any{"oid": "other", "statusCheckRollup": map[string]any{"contexts": map[string]any{"nodes": []any{}}}}}}}
			},
			check: func(pr ApprovalPullRequest) string { return pr.ChecksErr },
		},
		"files truncated": {
			edit: func(pr map[string]any) {
				pr["files"] = map[string]any{"pageInfo": map[string]any{"hasNextPage": true}, "nodes": []any{}}
			},
			check: func(pr ApprovalPullRequest) string { return pr.FilesErr },
		},
		"files missing": {
			edit:  func(pr map[string]any) { pr["files"] = nil },
			check: func(pr ApprovalPullRequest) string { return pr.FilesErr },
		},
		"labels truncated": {
			edit: func(pr map[string]any) {
				pr["labels"] = map[string]any{"pageInfo": map[string]any{"hasNextPage": true}, "nodes": []any{}}
			},
			check: func(pr ApprovalPullRequest) string { return pr.LabelsErr },
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			edited := map[string]any{}
			for k, v := range pull {
				edited[k] = v
			}
			tt.edit(edited)
			body, err := json.Marshal(map[string]any{"data": map[string]any{"repository": map[string]any{"nameWithOwner": "acme/widgets", "pullRequest": edited}}})
			if err != nil {
				t.Fatal(err)
			}
			exec := &routeExec{route: map[string]string{"reviewThreads": string(body)}}
			pr, err := NewClientWithExecutor(exec).LoadApprovalPullRequest(PullRef{Owner: "acme", Repo: "widgets", Number: 12})
			if err != nil {
				t.Fatal(err)
			}
			if tt.check(pr) == "" {
				t.Fatalf("no unreadable reason in %+v", pr)
			}
		})
	}
}

func TestLoadApprovalPullRequestNotFound(t *testing.T) {
	exec := &routeExec{route: map[string]string{"reviewThreads": `{"data":{"repository":{"pullRequest":null}}}`}}
	if _, err := NewClientWithExecutor(exec).LoadApprovalPullRequest(PullRef{Owner: "acme", Repo: "widgets", Number: 12}); err == nil {
		t.Fatal("error = nil")
	}
}

func TestPullRequestHeadSHA(t *testing.T) {
	exec := &routeExec{route: map[string]string{"repos/acme/widgets/pulls/12": "head2\n"}}
	sha, err := NewClientWithExecutor(exec).PullRequestHeadSHA(PullRef{Owner: "acme", Repo: "widgets", Number: 12})
	if err != nil || sha != "head2" {
		t.Fatalf("PullRequestHeadSHA() = %q, %v", sha, err)
	}
	exec = &routeExec{route: map[string]string{"repos/acme/widgets/pulls/12": "\n"}}
	if _, err := NewClientWithExecutor(exec).PullRequestHeadSHA(PullRef{Owner: "acme", Repo: "widgets", Number: 12}); err == nil {
		t.Fatal("empty head sha error = nil")
	}
}

func TestPullRequestDiff(t *testing.T) {
	const diff = "diff --git a/dash/a.go b/dash/a.go\n-old\n+new\n"
	exec := &routeExec{route: map[string]string{"repos/acme/widgets/pulls/12": diff}}
	got, err := NewClientWithExecutor(exec).PullRequestDiff(PullRef{Owner: "acme", Repo: "widgets", Number: 12})
	if err != nil || got != diff {
		t.Fatalf("PullRequestDiff() = %q, %v", got, err)
	}
	if len(exec.calls) != 1 || !strings.Contains(exec.calls[0], "Accept: application/vnd.github.diff") {
		t.Fatalf("calls = %v", exec.calls)
	}

	exec = &routeExec{fail: map[string]error{"pulls/12": errors.New("HTTP 406")}}
	if _, err := NewClientWithExecutor(exec).PullRequestDiff(PullRef{Owner: "acme", Repo: "widgets", Number: 12}); err == nil ||
		!strings.Contains(err.Error(), "HTTP 406") || !strings.Contains(err.Error(), "acme/widgets#12") {
		t.Fatalf("error = %v", err)
	}
}

func TestApprovePullRequestPinsCommit(t *testing.T) {
	exec := &routeExec{route: map[string]string{"repos/acme/widgets/pulls/12/reviews": `{"id": 1}`}}
	err := NewClientWithExecutor(exec).ApprovePullRequest(PullRef{Owner: "acme", Repo: "widgets", Number: 12}, "head1", "LGTM")
	if err != nil {
		t.Fatal(err)
	}
	if len(exec.calls) != 1 || !strings.Contains(exec.calls[0], "--method POST") {
		t.Fatalf("calls = %v", exec.calls)
	}
	var body map[string]string
	if err := json.Unmarshal([]byte(exec.stdin[0]), &body); err != nil {
		t.Fatal(err)
	}
	if body["commit_id"] != "head1" || body["event"] != "APPROVE" || body["body"] != "LGTM" {
		t.Fatalf("body = %v", body)
	}
}

func TestApprovePullRequestReportsGhError(t *testing.T) {
	exec := &routeExec{fail: map[string]error{"reviews": errors.New("exit 1")}}
	err := NewClientWithExecutor(exec).ApprovePullRequest(PullRef{Owner: "acme", Repo: "widgets", Number: 12}, "head1", "LGTM")
	if err == nil || !strings.Contains(err.Error(), "acme/widgets#12") {
		t.Fatalf("error = %v", err)
	}
}
