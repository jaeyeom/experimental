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
				"mergeable": "CONFLICTING",
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
	if pr.Mergeable != "CONFLICTING" {
		t.Errorf("Mergeable = %q, want CONFLICTING", pr.Mergeable)
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

func TestLoadPullRequestParsesNullBeforeCommitSignals(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"forcePushes": `{
			"data": {"repository": {"pullRequest": {
				"id": "PR_NODE",
				"headRefOid": "abc123",
				"commits": {"nodes": [{"commit": {
					"committedDate": "2026-01-02T03:05:05Z",
					"pushedDate": "2026-01-02T03:06:05Z"
				}}]},
				"timelineItems": {"nodes": [
					{"__typename": "RemovedFromMergeQueueEvent", "id": "EVT", "createdAt": "2026-01-02T03:04:05Z", "reason": "merge_conflict", "beforeCommit": null}
				]},
				"forcePushes": {"nodes": [
					{"createdAt": "2026-01-02T03:07:05Z"}
				]}
			}}}
		}`,
	}}
	client := NewClientWithExecutor(exec)
	pr, err := client.LoadPullRequest("acme/widgets#12")
	if err != nil {
		t.Fatalf("LoadPullRequest() error = %v", err)
	}
	if len(pr.Events) != 1 || pr.Events[0].CommitSHA != "" || pr.Events[0].Reason != "merge_conflict" {
		t.Fatalf("events = %+v", pr.Events)
	}
	if !pr.HeadCommittedAt.Equal(time.Date(2026, 1, 2, 3, 5, 5, 0, time.UTC)) {
		t.Fatalf("committed = %s", pr.HeadCommittedAt)
	}
	if !pr.HeadPushedAt.Equal(time.Date(2026, 1, 2, 3, 6, 5, 0, time.UTC)) {
		t.Fatalf("pushed = %s", pr.HeadPushedAt)
	}
	if len(pr.ForcePushedAt) != 1 || !pr.ForcePushedAt[0].Equal(time.Date(2026, 1, 2, 3, 7, 5, 0, time.UTC)) {
		t.Fatalf("force pushes = %v", pr.ForcePushedAt)
	}
}

func TestDequeuePullRequestUsesMutation(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"dequeuePullRequest": `{"data":{"dequeuePullRequest":{"mergeQueueEntry":{"id":"MQ"}}}}`,
	}}
	client := NewClientWithExecutor(exec)
	if err := client.DequeuePullRequest("PR_NODE"); err != nil {
		t.Fatalf("DequeuePullRequest() error = %v", err)
	}
	if len(exec.calls) != 1 || !strings.Contains(exec.calls[0], "dequeuePullRequest") || !strings.Contains(exec.calls[0], "id=PR_NODE") {
		t.Fatalf("call = %s", exec.calls)
	}
	if strings.Contains(exec.calls[0], "pullRequestId") {
		t.Fatalf("call = %s", exec.calls[0])
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

func TestOpenAboveListsOpenPullsAboveNearestFirst(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"stacks?pull_request=12": `[
			{"pull_requests": [
				{"number": 10, "state": "open"},
				{"number": 12, "state": "open"},
				{"number": 13, "state": "closed"},
				{"number": 14, "state": "open"},
				{"number": 15, "state": "open"}
			]}
		]`,
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.OpenAbove("acme", "widgets", 12)
	if err != nil {
		t.Fatalf("OpenAbove() error = %v", err)
	}
	if len(got) != 2 || got[0].Number != 14 || got[1].Number != 15 {
		t.Errorf("above = %+v", got)
	}
}

func TestInNativeStackEmptyIsNotAnError(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"stacks?pull_request=11": `[]`,
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.InNativeStack("acme", "widgets", 11)
	if err != nil {
		t.Fatalf("InNativeStack() error = %v", err)
	}
	if got {
		t.Fatal("InNativeStack() = true, want false")
	}
}

func TestInNativeStackContainsPullRequest(t *testing.T) {
	body := `[{"pull_requests":[{"number":10,"state":"open"},{"number":11,"state":"open"}]}]`
	exec := &routeExec{route: map[string]string{
		"stacks?pull_request=11": body,
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.InNativeStack("acme", "widgets", 11)
	if err != nil {
		t.Fatalf("InNativeStack() error = %v", err)
	}
	if !got {
		t.Fatal("InNativeStack() = false, want true")
	}
	above, err := client.OpenAbove("acme", "widgets", 11)
	if err != nil {
		t.Fatalf("OpenAbove() error = %v", err)
	}
	if above != nil {
		t.Fatalf("OpenAbove() = %+v, want none at the top of the stack", above)
	}
}

func TestInNativeStackKeepsListError(t *testing.T) {
	exec := &routeExec{fail: map[string]error{
		"stacks?pull_request=11": errors.New("down"),
	}}
	client := NewClientWithExecutor(exec)
	_, err := client.InNativeStack("acme", "widgets", 11)
	if err == nil || !strings.Contains(err.Error(), "list stacks") {
		t.Fatalf("error = %v", err)
	}
}

func TestOpenByBaseAndHeadQueryPulls(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"base=topic-a": `[{"number":11,"state":"open","head":{"ref":"topic-b"},"base":{"ref":"topic-a"}}]`,
		"head=":        `[{"number":10,"state":"open","head":{"ref":"topic-a"},"base":{"ref":"other"}}]`,
	}}
	client := NewClientWithExecutor(exec)
	byBase, err := client.OpenByBase("acme", "widgets", "topic-a")
	if err != nil {
		t.Fatalf("OpenByBase() error = %v", err)
	}
	if len(byBase) != 1 || byBase[0].Number != 11 || byBase[0].HeadRefName != "topic-b" || byBase[0].BaseRefName != "topic-a" {
		t.Fatalf("by base = %+v", byBase)
	}
	if !strings.Contains(exec.calls[0], "state=open") || !strings.Contains(exec.calls[0], "base=topic-a") {
		t.Fatalf("call = %s", exec.calls[0])
	}
	byHead, err := client.OpenByHead("acme", "widgets", "topic-a")
	if err != nil {
		t.Fatalf("OpenByHead() error = %v", err)
	}
	if len(byHead) != 1 || byHead[0].Number != 10 || byHead[0].HeadRefName != "topic-a" {
		t.Fatalf("by head = %+v", byHead)
	}
	if !strings.Contains(exec.calls[1], "state=open") || !strings.Contains(exec.calls[1], "head=acme%3Atopic-a") {
		t.Fatalf("call = %s", exec.calls[1])
	}
}

func TestLoadPullRequestReadsBaseHeadAndDefaultBranch(t *testing.T) {
	defaultBranch := "default-ref"
	exec := &routeExec{route: map[string]string{
		"defaultBranchRef": fmt.Sprintf(`{
			"data": {"repository": {
				"defaultBranchRef": {"name": %q},
				"pullRequest": {
					"id": "PR_NODE",
					"baseRefName": "topic-a",
					"headRefName": "topic-b"
				}
			}}
		}`, defaultBranch),
	}}
	client := NewClientWithExecutor(exec)
	pr, err := client.LoadPullRequest("acme/widgets#11")
	if err != nil {
		t.Fatalf("LoadPullRequest() error = %v", err)
	}
	if pr.BaseRefName != "topic-a" || pr.HeadRefName != "topic-b" || pr.DefaultBranch != defaultBranch {
		t.Fatalf("pr = %+v", pr)
	}
	call := exec.calls[0]
	for _, field := range []string{"baseRefName", "headRefName", "defaultBranchRef"} {
		if !strings.Contains(call, field) {
			t.Errorf("query missing %s: %s", field, call)
		}
	}
}

func TestLoadPullRequestAllowsEmptyDefaultBranch(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"defaultBranchRef": `{
			"data": {"repository": {"defaultBranchRef": null, "pullRequest": {"id": "PR_NODE"}}}
		}`,
	}}
	client := NewClientWithExecutor(exec)
	pr, err := client.LoadPullRequest("acme/widgets#11")
	if err != nil {
		t.Fatalf("LoadPullRequest() error = %v", err)
	}
	if pr.DefaultBranch != "" || pr.BaseRefName != "" {
		t.Fatalf("pr = %+v", pr)
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

func TestFailureEvidenceStatusTargetSkipsCheckRunLink(t *testing.T) {
	const sha = "abc123"
	const id = "11111111-1111-1111-1111-111111111111"
	tests := []struct {
		name     string
		workflow string
	}{
		{name: "plural", workflow: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflows/" + id},
		{name: "singular", workflow: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflow/" + id},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			exec := &routeExec{route: map[string]string{
				"check-runs": `{
					"check_runs":[{
						"name":"build",
						"conclusion":"success",
						"html_url":"https://github.com/acme/widgets/runs/3",
						"details_url":"` + tc.workflow + `"
					}]
				}`,
				"commits/" + sha + "/status": `{
					"statuses":[{
						"context":"ci/circleci",
						"state":"failure",
						"target_url":"https://circleci.com/gh/acme/widgets/4242"
					}]
				}`,
			}}
			client := NewClientWithExecutor(exec)
			got, err := client.FailureEvidence("acme", "widgets", sha)
			if err != nil {
				t.Fatalf("FailureEvidence() error = %v", err)
			}
			if got.Name != "ci/circleci" || got.TargetURL != "https://circleci.com/gh/acme/widgets/4242" {
				t.Fatalf("chosen = %s %s", got.Name, got.TargetURL)
			}
			if got.CheckRunURL != "" {
				t.Fatalf("check run = %s", got.CheckRunURL)
			}
		})
	}
}

func TestFailureEvidenceEmptyStatusTargetUsesCheckRun(t *testing.T) {
	const sha = "abc123"
	const workflow = "https://app.circleci.com/pipelines/github/acme/widgets/42/workflows/11111111-1111-1111-1111-111111111111"
	exec := &routeExec{route: map[string]string{
		"check-runs": `{
			"check_runs":[{
				"name":"build",
				"conclusion":"cancelled",
				"html_url":"https://github.com/acme/widgets/runs/9",
				"details_url":"` + workflow + `"
			}]
		}`,
		"commits/" + sha + "/status": `{
			"statuses":[{
				"context":"ci/circleci: unit",
				"state":"failure",
				"target_url":""
			}]
		}`,
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.FailureEvidence("acme", "widgets", sha)
	if err != nil {
		t.Fatalf("FailureEvidence() error = %v", err)
	}
	if got.Name != "ci/circleci: unit" || got.TargetURL != "" {
		t.Fatalf("chosen = %s %s", got.Name, got.TargetURL)
	}
	if got.CheckRunURL != workflow {
		t.Fatalf("check run = %s", got.CheckRunURL)
	}
}

func TestFailureEvidenceKeepsFailingStatusDescriptions(t *testing.T) {
	const sha = "abc123"
	const workflow = "https://app.circleci.com/pipelines/github/acme/widgets/42/workflow/11111111-1111-1111-1111-111111111111"
	exec := &routeExec{route: map[string]string{
		"check-runs": `{
			"check_runs":[{
				"name":"build",
				"conclusion":"success",
				"html_url":"https://github.com/acme/widgets/runs/3",
				"details_url":"` + workflow + `"
			}]
		}`,
		"commits/" + sha + "/status": `{
			"statuses":[
				{"context":"ci/pending","state":"pending","description":"pending-only-line","target_url":"https://example.com/pending"},
				{"context":"ci/ok","state":"success","description":"success-only-line","target_url":"https://example.com/ok"},
				{"context":"webhook/build","state":"error","description":"apt mirror HTTP 500","target_url":"https://circleci.com/gh/acme/widgets/4242"},
				{"context":"ci/tests","state":"failure","description":"unit tests failed","target_url":"https://example.com/tests"},
				{"context":"ci/empty","state":"error","description":"","target_url":"https://example.com/empty"}
			]
		}`,
	}}
	client := NewClientWithExecutor(exec)
	got, err := client.FailureEvidence("acme", "widgets", sha)
	if err != nil {
		t.Fatalf("FailureEvidence() error = %v", err)
	}
	if got.Name != "ci/tests" || got.TargetURL != "https://example.com/tests" {
		t.Fatalf("chosen = %s %s", got.Name, got.TargetURL)
	}
	if got.CheckRunURL != "" {
		t.Fatalf("check run = %s", got.CheckRunURL)
	}
	want := []CommitStatus{
		{Context: "webhook/build", State: "error", Description: "apt mirror HTTP 500"},
		{Context: "ci/tests", State: "failure", Description: "unit tests failed"},
		{Context: "ci/empty", State: "error"},
	}
	if len(got.Statuses) != len(want) {
		t.Fatalf("statuses = %+v", got.Statuses)
	}
	for i := range want {
		if got.Statuses[i] != want[i] {
			t.Errorf("statuses[%d] = %+v, want %+v", i, got.Statuses[i], want[i])
		}
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

func TestCommentOnPullRequestPostsIssueComment(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"issues/12/comments": `{"id":1}`,
	}}
	client := NewClientWithExecutor(exec)
	body := "Investigated the reset.\nRCA EVT class=flake"
	if err := client.CommentOnPullRequest("acme", "widgets", 12, body); err != nil {
		t.Fatalf("CommentOnPullRequest() error = %v", err)
	}
	if len(exec.calls) != 1 || !strings.Contains(exec.calls[0], "--method POST") || !strings.Contains(exec.calls[0], "repos/acme/widgets/issues/12/comments") {
		t.Fatalf("call = %s", exec.calls)
	}
	if len(exec.stdin) != 1 || !strings.Contains(exec.stdin[0], "Investigated the reset.") || !strings.Contains(exec.stdin[0], "RCA EVT class=flake") {
		t.Fatalf("stdin = %s", exec.stdin)
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
