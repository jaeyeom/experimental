package github

import (
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestParsePullRef(t *testing.T) {
	tests := []struct {
		raw     string
		owner   string
		repo    string
		number  int
		wantErr bool
	}{
		{raw: "acme/widgets#12", owner: "acme", repo: "widgets", number: 12},
		{raw: " https://github.com/acme/widgets/pull/12 ", owner: "acme", repo: "widgets", number: 12},
		{raw: "acme/widgets", wantErr: true},
		{raw: "https://example.com/acme/widgets/pull/12", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.raw, func(t *testing.T) {
			got, err := ParsePullRef(tc.raw)
			if tc.wantErr {
				if err == nil {
					t.Fatal("ParsePullRef() error = nil")
				}
				return
			}
			if err != nil {
				t.Fatalf("ParsePullRef() error = %v", err)
			}
			if got.Owner != tc.owner || got.Repo != tc.repo || got.Number != tc.number {
				t.Errorf("ParsePullRef() = %+v", got)
			}
		})
	}
}

type routeExec struct {
	calls []string
	stdin []string
	route map[string]string
	fail  map[string]error
}

func (r *routeExec) Execute(cmd string, args ...string) (string, error) {
	return r.dispatch("", cmd, args...)
}

func (r *routeExec) ExecuteWithStdin(stdin, cmd string, args ...string) (string, error) {
	return r.dispatch(stdin, cmd, args...)
}

func (r *routeExec) dispatch(stdin, cmd string, args ...string) (string, error) {
	joined := cmd + " " + strings.Join(args, " ")
	r.calls = append(r.calls, joined)
	r.stdin = append(r.stdin, stdin)
	if key := longestRouteKey(joined, r.fail); key != "" {
		return "", r.fail[key]
	}
	if key := longestRouteKey(joined, r.route); key != "" {
		return r.route[key], nil
	}
	return "", errors.New("unexpected call: " + joined)
}

func longestRouteKey[T any](joined string, routes map[string]T) string {
	best := ""
	for key := range routes {
		if strings.Contains(joined, key) && len(key) > len(best) {
			best = key
		}
	}
	return best
}

func TestLoadPullRequestParsesQueueState(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"REMOVED_FROM_MERGE_QUEUE_EVENT": `{
			"data": {"repository": {"pullRequest": {
				"id": "PR_NODE",
				"isDraft": false,
				"reviewDecision": "APPROVED",
				"state": "OPEN",
				"merged": false,
				"isInMergeQueue": false,
				"headRefOid": "abc123",
				"labels": {"nodes": [{"name": "ready"}]},
				"reviewThreads": {"nodes": [{"isResolved": false}, {"isResolved": true}]},
				"commits": {"nodes": [{"commit": {"statusCheckRollup": {"contexts": {"nodes": [
					{"name": "ci", "conclusion": "SUCCESS", "status": "COMPLETED"},
					{"context": "lint", "state": "FAILURE"}
				]}}}}]},
				"comments": {"nodes": [{"body": "RCA EVT class=flake"}]},
				"timelineItems": {"nodes": [
					{"__typename": "AddedToMergeQueueEvent", "id": "A1", "createdAt": "2026-01-01T00:00:00Z"},
					{"__typename": "RemovedFromMergeQueueEvent", "id": "EVT", "createdAt": "2026-01-01T01:00:00Z", "reason": "failed_checks", "beforeCommit": {"oid": "deadbeef"}}
				]}
			}}}
		}`,
	}}
	client := NewClientWithExecutor(exec)
	pr, err := client.LoadPullRequest("acme/widgets#12")
	if err != nil {
		t.Fatalf("LoadPullRequest() error = %v", err)
	}
	if pr.NodeID != "PR_NODE" || pr.Ref.Owner != "acme" || pr.Ref.Number != 12 {
		t.Fatalf("pr = %+v", pr)
	}
	if pr.InMergeQueue {
		t.Error("InMergeQueue = true, want false")
	}
	if pr.UnresolvedThreads != 1 {
		t.Errorf("unresolved = %d, want 1", pr.UnresolvedThreads)
	}
	if len(pr.Checks) != 2 || !pr.Checks[0].Green || pr.Checks[1].Green || pr.Checks[1].Name != "lint" {
		t.Errorf("checks = %+v", pr.Checks)
	}
	if len(pr.Events) != 2 || pr.Events[1].Kind != QueueEventRemoved || pr.Events[1].CommitSHA != "deadbeef" {
		t.Errorf("events = %+v", pr.Events)
	}
	if !strings.Contains(exec.calls[0], "enqueuePullRequest") && !strings.Contains(exec.calls[0], "REMOVED_FROM_MERGE_QUEUE_EVENT") {
		t.Errorf("call = %s", exec.calls[0])
	}
}

func TestEnqueuePullRequestUsesMutation(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"enqueuePullRequest": `{"data":{"enqueuePullRequest":{"mergeQueueEntry":{"id":"MQ"}}}}`,
	}}
	client := NewClientWithExecutor(exec)
	if err := client.EnqueuePullRequest("PR_NODE"); err != nil {
		t.Fatalf("EnqueuePullRequest() error = %v", err)
	}
	if !strings.Contains(exec.calls[0], "enqueuePullRequest") || !strings.Contains(exec.calls[0], "id=PR_NODE") {
		t.Errorf("call = %s", exec.calls[0])
	}
}

func TestEnqueuePullRequestReturnsGraphQLError(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"enqueuePullRequest": `{"errors":[{"message":"This pull request is part of a stack and must be merged sequentially using the stack merge API."}]}`,
	}}
	client := NewClientWithExecutor(exec)
	err := client.EnqueuePullRequest("PR_NODE")
	if err == nil || !strings.Contains(err.Error(), "part of a stack") {
		t.Fatalf("error = %v", err)
	}
}

func TestOpenDownstackListsOpenPullsBelow(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"stacks?pull_request=12": `[
			{"pull_requests": [
				{"number": 9, "state": "closed"},
				{"number": 10, "state": "open"},
				{"number": 11, "state": "open"},
				{"number": 12, "state": "open"}
			]}
		]`,
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.OpenDownstack("acme", "widgets", 12)
	if err != nil {
		t.Fatalf("OpenDownstack() error = %v", err)
	}
	if len(got) != 2 || got[0].Number != 10 || got[1].Number != 11 {
		t.Errorf("downstack = %+v", got)
	}
}

func TestFailureEvidenceReadsFailedJobLog(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"check-runs": `{"check_runs":[
			{"conclusion":"success","html_url":"https://github.com/acme/widgets/runs/1","details_url":"https://github.com/acme/widgets/actions/runs/9/job/1"},
			{"name":"unit","conclusion":"failure","html_url":"https://github.com/acme/widgets/runs/2","details_url":"https://github.com/acme/widgets/actions/runs/9/job/8"}
		]}`,
		"commits/deadbeef/status": `{"statuses":[]}`,
		"actions/jobs/8/logs":     "ok\n##[group]Run tests\nconnection reset by peer\n##[group]Run later\nignored",
		"actions/jobs/8":          `{"steps":[{"name":"Run tests","conclusion":"failure"}]}`,
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.FailureEvidence("acme", "widgets", "deadbeef")
	if err != nil {
		t.Fatalf("FailureEvidence() error = %v", err)
	}
	if got.Name != "unit" || got.TargetURL != "https://github.com/acme/widgets/actions/runs/9/job/8" {
		t.Errorf("chosen = %s %s", got.Name, got.TargetURL)
	}
	if got.CheckRunURL != "https://github.com/acme/widgets/runs/2" {
		t.Errorf("check run = %s", got.CheckRunURL)
	}
	if got.JobURL != "https://github.com/acme/widgets/actions/runs/9/job/8" {
		t.Errorf("job = %s", got.JobURL)
	}
	if !strings.Contains(got.Log, "connection reset by peer") || strings.Contains(got.Log, "ignored") {
		t.Errorf("log = %q", got.Log)
	}
}

func TestFailureEvidenceRanksChecksAndStatuses(t *testing.T) {
	const sha = "abc123"
	cancelled := `{"name":"actions","conclusion":"cancelled","html_url":"https://github.com/acme/widgets/runs/9","details_url":"https://github.com/acme/widgets/actions/runs/1/job/3"}`
	tests := []struct {
		name       string
		checks     string
		statuses   string
		wantName   string
		wantTarget string
		wantCheck  string
	}{
		{
			name:       "failure status beats cancelled check",
			checks:     "[" + cancelled + `]`,
			statuses:   `[{"context":"ci/pending","state":"pending","target_url":"https://example.com/pending"},{"context":"ci/circleci","state":"failure","target_url":"https://app.circleci.com/pipelines/github/acme/widgets/42"}]`,
			wantName:   "ci/circleci",
			wantTarget: "https://app.circleci.com/pipelines/github/acme/widgets/42",
		},
		{
			name:       "error status beats cancelled check",
			checks:     "[" + cancelled + `]`,
			statuses:   `[{"context":"ci/circleci","state":"error","target_url":"https://app.circleci.com/pipelines/github/acme/widgets/7"}]`,
			wantName:   "ci/circleci",
			wantTarget: "https://app.circleci.com/pipelines/github/acme/widgets/7",
		},
		{
			name:       "failure check beats error status",
			checks:     `[{"name":"unit","conclusion":"failure","html_url":"https://github.com/acme/widgets/runs/2","details_url":"https://example.com/unit"}]`,
			statuses:   `[{"context":"ci/circleci","state":"error","target_url":"https://app.circleci.com/pipelines/github/acme/widgets/7"}]`,
			wantName:   "unit",
			wantTarget: "https://example.com/unit",
			wantCheck:  "https://github.com/acme/widgets/runs/2",
		},
		{
			name:       "timed out check beats error status",
			checks:     `[{"name":"actions","conclusion":"cancelled","html_url":"https://github.com/acme/widgets/runs/9","details_url":"https://github.com/acme/widgets/actions/runs/1/job/3"},{"name":"unit","conclusion":"timed_out","html_url":"https://github.com/acme/widgets/runs/4","details_url":"https://example.com/timeout"}]`,
			statuses:   `[{"context":"ci/circleci","state":"error","target_url":"https://app.circleci.com/pipelines/github/acme/widgets/7"}]`,
			wantName:   "unit",
			wantTarget: "https://example.com/timeout",
			wantCheck:  "https://github.com/acme/widgets/runs/4",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			exec := &routeExec{route: map[string]string{
				"check-runs":                 fmt.Sprintf(`{"check_runs":%s}`, tc.checks),
				"commits/" + sha + "/status": fmt.Sprintf(`{"statuses":%s}`, tc.statuses),
			}}
			client := NewClientWithExecutor(exec)
			got, err := client.FailureEvidence("acme", "widgets", sha)
			if err != nil {
				t.Fatalf("FailureEvidence() error = %v", err)
			}
			if got.Name != tc.wantName || got.TargetURL != tc.wantTarget || got.CheckRunURL != tc.wantCheck {
				t.Fatalf("got name=%q target=%q check=%q", got.Name, got.TargetURL, got.CheckRunURL)
			}
			if got.Log != "" {
				t.Fatalf("log = %q, want none", got.Log)
			}
		})
	}
}

func TestLandedFirstPicksEarliestMerged(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"commits/deadbeef/pulls": `[
			{"number": 12, "merged_at": "2026-01-03T00:00:00Z"},
			{"number": 4, "merged_at": "2026-01-02T00:00:00Z", "html_url": "https://github.com/acme/widgets/pull/4"},
			{"number": 7, "merged_at": "2026-01-01T00:00:00Z", "html_url": "https://github.com/acme/widgets/pull/7"}
		]`,
	}}
	client := NewClientWithExecutor(exec)
	got, ok, err := client.LandedFirst("acme", "widgets", "deadbeef", 12)
	if err != nil || !ok {
		t.Fatalf("LandedFirst() ok=%v err=%v", ok, err)
	}
	if got.Number != 7 {
		t.Errorf("number = %d, want 7", got.Number)
	}
	want := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	if !got.MergedAt.Equal(want) {
		t.Errorf("merged at = %s", got.MergedAt)
	}
}

func TestMergeAsyncSendsQueueActionAndSHA(t *testing.T) {
	exec := &routeExec{route: map[string]string{"merge-async": `{"status":"enqueued"}`}}
	client := NewClientWithExecutor(exec)
	if err := client.MergeAsync("acme", "widgets", 12, "abc123"); err != nil {
		t.Fatalf("MergeAsync() error = %v", err)
	}
	if len(exec.stdin) != 1 || exec.stdin[0] != `{"merge_action":"merge_queue","sha":"abc123"}` {
		t.Errorf("stdin = %q", exec.stdin)
	}
	if !strings.Contains(exec.calls[0], "repos/acme/widgets/pulls/12/merge-async") || !strings.Contains(exec.calls[0], "--method PUT") {
		t.Errorf("call = %s", exec.calls[0])
	}
}
