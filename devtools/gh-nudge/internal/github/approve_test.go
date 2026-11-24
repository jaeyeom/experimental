package github

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestPullRequestFilePatches(t *testing.T) {
	body := `[
		{"filename":"a.go","status":"modified","additions":1,"deletions":1,"patch":"@@ -1 +1 @@\n+// a\n"},
		{"filename":"b.bin","status":"modified","additions":0,"deletions":0},
		{"filename":"c.go","status":"renamed","previous_filename":"d.go","additions":0,"deletions":0}
	]`
	exec := &scriptedExecutor{outputs: []string{body}}
	files, err := NewClientWithExecutor(exec).PullRequestFilePatches(PullRef{Owner: "acme", Repo: "widgets", Number: 7})
	if err != nil {
		t.Fatal(err)
	}
	if len(files) != 3 || !files[0].PatchSet || files[0].Patch == "" || files[1].PatchSet || files[1].Path != "b.bin" {
		t.Fatalf("files = %+v", files)
	}
	if files[2].Status != "renamed" || files[2].Previous != "d.go" || files[2].PatchSet {
		t.Fatalf("rename = %+v", files[2])
	}
	joined := strings.Join(exec.calls[0].args, " ")
	if !strings.Contains(joined, "api") || !strings.Contains(joined, "--paginate") ||
		!strings.Contains(joined, "repos/acme/widgets/pulls/7/files?per_page=100") {
		t.Fatalf("args = %q", joined)
	}
}

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

func TestSearchReviewRequestedIncludesTeamRequests(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"archived:false review-requested:@me": `{"data":{"search":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[
			{"number": 7, "url": "https://github.com/acme/widgets/pull/7", "repository": {"nameWithOwner": "acme/widgets"}}
		]}}}`,
	}}
	refs, err := NewClientWithExecutor(exec).SearchReviewRequested()
	if err != nil {
		t.Fatal(err)
	}
	if len(refs) != 1 || refs[0].Number != 7 {
		t.Fatalf("refs = %+v", refs)
	}
	if !strings.Contains(exec.calls[0], "q=is:pr is:open archived:false review-requested:@me") {
		t.Fatalf("search call = %s", exec.calls[0])
	}
}

func TestSearchTeamApprovalCandidatesFiltersAndDeduplicatesLabelSets(t *testing.T) {
	response := `{"data":{"search":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[
		{"number":7,"url":"https://github.com/acme/widgets/pull/7","title":"Tidy","isDraft":false,"author":{"login":"alice"},"changedFiles":2,"additions":3,"deletions":1,"labels":{"nodes":[{"name":"ready"},{"name":"docs"}]},"repository":{"nameWithOwner":"acme/widgets"}}
	]}}}`
	exec := &routeExec{route: map[string]string{
		`review-requested:@me label:"ready"`: response,
		`review-requested:@me label:"docs"`:  response,
	}}
	candidates, err := NewClientWithExecutor(exec).SearchTeamApprovalCandidates([][]string{{"ready"}, {"docs"}})
	if err != nil {
		t.Fatal(err)
	}
	if len(candidates) != 1 || candidates[0].ChangedFiles != 2 || candidates[0].Additions != 3 || candidates[0].Author != "alice" || len(candidates[0].Labels) != 2 {
		t.Fatalf("candidates = %+v", candidates)
	}
	if len(exec.calls) != 2 {
		t.Fatalf("calls = %d, want one search per label set", len(exec.calls))
	}
	for _, call := range exec.calls {
		if !strings.Contains(call, `q=is:pr is:open archived:false review-requested:@me label:`) {
			t.Errorf("search lacks request scope and label qualifier: %s", call)
		}
	}
}

func TestSearchTeamApprovalCandidatesWithoutLabelSetsKeepsUnfilteredScope(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		`review-requested:@me`: `{"data":{"search":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}}}`,
	}}
	if _, err := NewClientWithExecutor(exec).SearchTeamApprovalCandidates([][]string{{}}); err != nil {
		t.Fatal(err)
	}
	if len(exec.calls) != 1 || !strings.Contains(exec.calls[0], `q=is:pr is:open archived:false review-requested:@me`) || strings.Contains(exec.calls[0], `label:`) {
		t.Fatalf("unfiltered search = %q", exec.calls)
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
	exec := &routeExec{route: map[string]string{"repos/acme/widgets/pulls/12": "b1e40324aa2f3c1d5e6f708192a3b4c5d6e7f809\n"}}
	sha, err := NewClientWithExecutor(exec).PullRequestHeadSHA(PullRef{Owner: "acme", Repo: "widgets", Number: 12})
	if err != nil || sha != "b1e40324aa2f3c1d5e6f708192a3b4c5d6e7f809" {
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

func TestLoadApprovalPullRequestReadsViewedState(t *testing.T) {
	fixture := strings.Replace(approvalPRFixture, `"headRefOid": "head1",`, `"id": "PR_node", "headRefOid": "head1",`, 1)
	fixture = strings.Replace(fixture, `"path": "dash/a.go", "additions": 4, "deletions": 1`, `"path": "dash/a.go", "additions": 4, "deletions": 1, "viewerViewedState": "UNVIEWED"`, 1)
	exec := &routeExec{route: map[string]string{"reviewThreads": fixture}}
	pr, err := NewClientWithExecutor(exec).LoadApprovalPullRequest(PullRef{Owner: "acme", Repo: "widgets", Number: 12})
	if err != nil {
		t.Fatal(err)
	}
	if pr.NodeID != "PR_node" || pr.Files[0].ViewerViewedState != models.ViewerUnviewed || !strings.Contains(exec.calls[0], "viewerViewedState") {
		t.Fatalf("node %q file %+v call %s", pr.NodeID, pr.Files[0], exec.calls[0])
	}
}

func TestMarkFileAsViewed(t *testing.T) {
	ref := PullRef{Owner: "acme", Repo: "widgets", Number: 12}
	exec := &routeExec{route: map[string]string{"markFileAsViewed": `{"data":{"markFileAsViewed":{"clientMutationId":null}}}`}}
	if err := NewClientWithExecutor(exec).MarkFileAsViewed(ref, "PR_node", "go.sum"); err != nil {
		t.Fatal(err)
	}
	if len(exec.calls) != 1 || !strings.Contains(exec.calls[0], "id=PR_node") || !strings.Contains(exec.calls[0], "path=go.sum") || !strings.Contains(exec.calls[0], "mutation") {
		t.Fatalf("calls = %v", exec.calls)
	}
	if err := NewClientWithExecutor(exec).MarkFileAsViewed(ref, "", "go.sum"); err == nil || len(exec.calls) != 1 {
		t.Fatalf("empty id error = %v calls %d", err, len(exec.calls))
	}
	exec = &routeExec{route: map[string]string{"markFileAsViewed": `{"errors":[{"message":"forbidden"}]}`}}
	if err := NewClientWithExecutor(exec).MarkFileAsViewed(ref, "PR_node", "go.sum"); err == nil || !strings.Contains(err.Error(), "forbidden") {
		t.Fatalf("error = %v", err)
	}
}

// ghHint is the kind of advice newer gh releases print on stderr after a
// successful pull request read.
const ghHint = "hint: if you need to wait for this pull request, use this instead of polling:\n" +
	"    gh event listen --resource https://github.com/acme/widgets/pull/12\n" +
	"  - It prints the first event as a JSON line on stdout and exits.\n"

// hintExec answers like the real executor: stdout and stderr apart from
// ExecuteSeparated, and combined from Execute.
type hintExec struct {
	route map[string]string
	calls []string
}

func (h *hintExec) stdout(args []string) string {
	joined := strings.Join(args, " ")
	h.calls = append(h.calls, joined)
	if key := longestRouteKey(joined, h.route); key != "" {
		return h.route[key]
	}
	return ""
}

func (h *hintExec) Execute(_ string, args ...string) (string, error) {
	return h.stdout(args) + ghHint, nil
}

func (h *hintExec) ExecuteWithStdin(_, _ string, args ...string) (string, error) {
	return h.stdout(args) + ghHint, nil
}

func (h *hintExec) ExecuteSeparated(_ string, args ...string) (string, string, error) {
	return h.stdout(args), ghHint, nil
}

func TestPullRequestHeadSHAIgnoresStderrHint(t *testing.T) {
	const sha = "b1e40324aa2f3c1d5e6f708192a3b4c5d6e7f809"
	exec := &hintExec{route: map[string]string{"repos/acme/widgets/pulls/12": sha + "\n"}}
	got, err := NewClientWithExecutor(exec).PullRequestHeadSHA(PullRef{Owner: "acme", Repo: "widgets", Number: 12})
	if err != nil || got != sha {
		t.Fatalf("PullRequestHeadSHA() = %q, %v; want %q", got, err, sha)
	}
}

func TestPullRequestHeadSHARejectsNonSHA(t *testing.T) {
	exec := &hintExec{route: map[string]string{"repos/acme/widgets/pulls/12": "not a sha\n"}}
	_, err := NewClientWithExecutor(exec).PullRequestHeadSHA(PullRef{Owner: "acme", Repo: "widgets", Number: 12})
	if err == nil {
		t.Fatal("error = nil, want a non-SHA error")
	}
	if !strings.Contains(err.Error(), "not a sha") || !strings.Contains(err.Error(), "hint:") {
		t.Fatalf("error = %v, want the stdout and the first stderr line", err)
	}
}

func TestFailingStepLogIgnoresStderrHint(t *testing.T) {
	exec := &hintExec{route: map[string]string{
		"actions/jobs/8/logs": "##[group]Run tests\nFAIL\n",
		"actions/jobs/8":      `{"steps":[{"name":"Run tests","conclusion":"failure"}]}`,
	}}
	_, log, err := NewClientWithExecutor(exec).failingStepLog("acme", "widgets", 8)
	if err != nil {
		t.Fatalf("failingStepLog() error = %v", err)
	}
	if strings.Contains(log, "hint:") || strings.Contains(log, "gh event listen") {
		t.Fatalf("log = %q, want no stderr text", log)
	}
	if !strings.Contains(log, "FAIL") {
		t.Fatalf("log = %q, want the step output", log)
	}
}

type funcExec struct {
	fn func(cmd string, args ...string) (string, error)
}

func (f *funcExec) Execute(cmd string, args ...string) (string, error) {
	return f.fn(cmd, args...)
}

func (f *funcExec) ExecuteWithStdin(_, cmd string, args ...string) (string, error) {
	return f.fn(cmd, args...)
}

func (f *funcExec) ExecuteSeparated(cmd string, args ...string) (string, string, error) {
	out, err := f.fn(cmd, args...)
	return out, "", err
}

func TestLoadApprovalPullRequestsOneBatch(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"p0:repository": `{"data":{` +
			`"p0":{"pullRequest":{"id":"PR_1","number":1,"url":"https://github.com/acme/widgets/pull/1","title":"One","state":"OPEN","isDraft":false,"headRefOid":"h1","baseRefName":"main","baseRefOid":"b1","author":{"login":"alice"},"additions":2,"deletions":1,"changedFiles":1,"files":{"pageInfo":{"hasNextPage":false},"nodes":[{"path":"dash/a.go","viewerViewedState":"UNVIEWED"}]},"labels":{"pageInfo":{"hasNextPage":false},"nodes":[{"name":"ready"}]},"reviewRequests":{"nodes":[{"requestedReviewer":{"__typename":"User","login":"me"}}]},"reviews":{"nodes":[]},"reviewThreads":{"totalCount":0,"nodes":[]},"commits":{"nodes":[{"commit":{"oid":"h1","statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[{"__typename":"CheckRun","name":"ci","status":"COMPLETED","conclusion":"SUCCESS","isRequired":true}]}}}}]}}},` +
			`"p1":{"pullRequest":{"id":"PR_2","number":2,"url":"https://github.com/acme/widgets/pull/2","title":"Two","state":"OPEN","isDraft":true,"headRefOid":"h2","baseRefName":"main","baseRefOid":"b2","author":{"login":"bob"},"additions":5,"deletions":0,"changedFiles":2,"files":{"pageInfo":{"hasNextPage":false},"nodes":[]},"labels":{"pageInfo":{"hasNextPage":false},"nodes":[]},"reviewRequests":{"nodes":[{"requestedReviewer":{"__typename":"Team","slug":"core"}}]},"reviews":{"nodes":[]},"reviewThreads":{"totalCount":1,"nodes":[{"isResolved":false}]},"commits":{"nodes":[{"commit":{"oid":"h2","statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[]}}}}]}}}` +
			`}}`,
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.LoadApprovalPullRequests([]PullRef{
		{Owner: "acme", Repo: "widgets", Number: 1},
		{Owner: "acme", Repo: "widgets", Number: 2},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(exec.calls) != 1 || client.GraphQLCalls() != 1 {
		t.Fatalf("calls = %d, GraphQLCalls = %d, want 1", len(exec.calls), client.GraphQLCalls())
	}
	r1 := got["acme/widgets#1"]
	if r1.Err != nil || r1.PR.HeadSHA != "h1" || r1.PR.ChangedFiles != 1 || len(r1.PR.Labels) != 1 || r1.PR.Labels[0] != "ready" {
		t.Fatalf("r1 = %+v", r1)
	}
	if r1.PR.Files != nil || r1.PR.FilesErr != "" {
		t.Fatalf("r1.PR.Files = %v, FilesErr = %q, want nil/empty for light batch", r1.PR.Files, r1.PR.FilesErr)
	}
	if r1.PR.ViewedStates["dash/a.go"] != models.ViewerUnviewed {
		t.Fatalf("r1.PR.ViewedStates = %+v, want dash/a.go=%s", r1.PR.ViewedStates, models.ViewerUnviewed)
	}
	r2 := got["acme/widgets#2"]
	if r2.Err != nil || !r2.PR.Draft || r2.PR.UnresolvedThreads != 1 || len(r2.PR.RequestedTeams) != 1 || r2.PR.RequestedTeams[0] != "core" {
		t.Fatalf("r2 = %+v", r2)
	}
}

func TestLoadApprovalPullRequestsSplitsAfterBatchSize(t *testing.T) {
	total := ApprovalPullRequestBatchSize + 1
	refs := make([]PullRef, total)
	for i := range refs {
		refs[i] = PullRef{Owner: "acme", Repo: "widgets", Number: i + 1}
	}
	var calls int
	exec := &funcExec{fn: func(_ string, args ...string) (string, error) {
		calls++
		// Return empty pullRequests for the aliases in this call; count nodes.
		query := strings.Join(args, " ")
		data := make(map[string]any)
		for i := range ApprovalPullRequestBatchSize {
			alias := fmt.Sprintf("p%d", i)
			if strings.Contains(query, alias+":repository") {
				data[alias] = map[string]any{
					"pullRequest": map[string]any{
						"number": i + 1, "state": "OPEN", "headRefOid": "h",
						"labels":        map[string]any{"pageInfo": map[string]any{"hasNextPage": false}, "nodes": []any{}},
						"reviewThreads": map[string]any{"totalCount": 0, "nodes": []any{}},
						"commits":       map[string]any{"nodes": []any{map[string]any{"commit": map[string]any{"oid": "h", "statusCheckRollup": map[string]any{"contexts": map[string]any{"pageInfo": map[string]any{"hasNextPage": false}, "nodes": []any{}}}}}}},
					},
				}
			}
		}
		b, _ := json.Marshal(map[string]any{"data": data})
		return string(b), nil
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.LoadApprovalPullRequests(refs)
	if err != nil {
		t.Fatal(err)
	}
	if calls != 2 || client.GraphQLCalls() != 2 {
		t.Fatalf("calls = %d, GraphQLCalls = %d, want 2", calls, client.GraphQLCalls())
	}
	if len(got) != total {
		t.Fatalf("len(got) = %d, want %d", len(got), total)
	}
}

func TestLoadApprovalPullRequestsPartialFailure(t *testing.T) {
	exec := &funcExec{fn: func(_ string, _ ...string) (string, error) {
		return `{"data":{` +
			`"p0":{"pullRequest":{"id":"PR_1","number":1,"state":"OPEN","headRefOid":"h1","labels":{"pageInfo":{"hasNextPage":false},"nodes":[]},"reviewThreads":{"totalCount":0,"nodes":[]},"commits":{"nodes":[{"commit":{"oid":"h1","statusCheckRollup":{"contexts":{"pageInfo":{"hasNextPage":false},"nodes":[]}}}}]}}},` +
			`"p1":{"pullRequest":null}` +
			`},"errors":[{"type":"NOT_FOUND","message":"Could not resolve to a PullRequest with the number of 99.","path":["p1","pullRequest"]}]}`, errors.New("exit 1")
	}}
	got, err := NewClientWithExecutor(exec).LoadApprovalPullRequests([]PullRef{
		{Owner: "acme", Repo: "widgets", Number: 1},
		{Owner: "acme", Repo: "widgets", Number: 99},
	})
	if err != nil {
		t.Fatalf("unexpected batch error: %v", err)
	}
	if got["acme/widgets#1"].Err != nil || got["acme/widgets#1"].PR.HeadSHA != "h1" {
		t.Fatalf("pr 1 = %+v", got["acme/widgets#1"])
	}
	if got["acme/widgets#99"].Err == nil || !strings.Contains(got["acme/widgets#99"].Err.Error(), "Could not resolve") {
		t.Fatalf("pr 99 = %+v, want error", got["acme/widgets#99"])
	}
}

func TestLoadApprovalPullRequestsEmptyMakesNoCall(t *testing.T) {
	exec := &routeExec{}
	client := NewClientWithExecutor(exec)
	got, err := client.LoadApprovalPullRequests(nil)
	if err != nil || len(got) != 0 || len(exec.calls) != 0 || client.GraphQLCalls() != 0 {
		t.Fatalf("got=%v err=%v calls=%d", got, err, len(exec.calls))
	}
}

func TestLoadApprovalPullRequestsTruncatedJSONReturnsSyntaxError(t *testing.T) {
	exec := &funcExec{fn: func(_ string, _ ...string) (string, error) {
		return `{"data":{"p0":{"pullRequest":`, nil
	}}
	_, err := NewClientWithExecutor(exec).LoadApprovalPullRequests([]PullRef{
		{Owner: "acme", Repo: "widgets", Number: 1},
	})
	if err == nil {
		t.Fatal("expected error for truncated JSON stdout, got nil")
	}
	var syntaxErr *json.SyntaxError
	if !errors.As(err, &syntaxErr) {
		t.Fatalf("err = %v, want *json.SyntaxError", err)
	}
}

func TestLoadApprovalPullRequestsEmptyStdoutReturnsUnexpectedEOF(t *testing.T) {
	exec := &funcExec{fn: func(_ string, _ ...string) (string, error) {
		return "", nil
	}}
	client := NewClientWithExecutor(exec)
	client.sleep = func(time.Duration) {}
	client.warn = func(string, ...any) {}
	_, err := client.LoadApprovalPullRequests([]PullRef{
		{Owner: "acme", Repo: "widgets", Number: 1},
	})
	if err == nil {
		t.Fatal("expected error for empty stdout, got nil")
	}
	if !errors.Is(err, io.ErrUnexpectedEOF) {
		t.Fatalf("err = %v, want io.ErrUnexpectedEOF", err)
	}
}
