package github

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"
)

var handoffRef = PullRef{Owner: "acme", Repo: "widgets", Number: 12}

func TestLoadApprovalPullRequestReadsBaseRef(t *testing.T) {
	fixture := strings.Replace(approvalPRFixture, `"headRefOid": "head1",`, `"headRefOid": "head1", "baseRefName": "main",`, 1)
	exec := &routeExec{route: map[string]string{"reviewThreads": fixture}}
	pr, err := NewClientWithExecutor(exec).LoadApprovalPullRequest(handoffRef)
	if err != nil {
		t.Fatal(err)
	}
	if pr.BaseRef != "main" {
		t.Fatalf("BaseRef = %q", pr.BaseRef)
	}
}

func TestCodeownersFileReadsFirstLocation(t *testing.T) {
	exec := &routeExec{route: map[string]string{"CODEOWNERS": `{"data":{"repository":{
		"github": null,
		"root": {"text": "* @acme/core\n", "isTruncated": false},
		"docs": {"text": "* @someone\n", "isTruncated": false}
	}}}`}}
	path, text, err := NewClientWithExecutor(exec).CodeownersFile(handoffRef, "main")
	if err != nil {
		t.Fatal(err)
	}
	if path != "CODEOWNERS" || text != "* @acme/core\n" {
		t.Fatalf("CodeownersFile() = %q %q", path, text)
	}
	if !strings.Contains(exec.calls[0], "github=main:.github/CODEOWNERS") || !strings.Contains(exec.calls[0], "docs=main:docs/CODEOWNERS") {
		t.Fatalf("call = %s", exec.calls[0])
	}
}

func TestCodeownersFileFailsWhenMissingOrUnreadable(t *testing.T) {
	tests := map[string]*routeExec{
		"missing":   {route: map[string]string{"CODEOWNERS": `{"data":{"repository":{"github":null,"root":null,"docs":null}}}`}},
		"truncated": {route: map[string]string{"CODEOWNERS": `{"data":{"repository":{"github":{"text":"* @a","isTruncated":true},"root":null,"docs":null}}}`}},
		"binary":    {route: map[string]string{"CODEOWNERS": `{"data":{"repository":{"github":{"text":null,"isTruncated":false},"root":null,"docs":null}}}`}},
		"gh error":  {fail: map[string]error{"CODEOWNERS": errors.New("exit 1")}},
	}
	for name, exec := range tests {
		t.Run(name, func(t *testing.T) {
			if _, _, err := NewClientWithExecutor(exec).CodeownersFile(handoffRef, "main"); err == nil {
				t.Fatal("error = nil")
			}
		})
	}
	if _, _, err := NewClientWithExecutor(&routeExec{}).CodeownersFile(handoffRef, ""); err == nil {
		t.Fatal("empty base ref error = nil")
	}
}

func TestTeamMembersPaginates(t *testing.T) {
	exec := &routeExec{route: map[string]string{
		"slug=core":             `{"data":{"organization":{"team":{"members":{"pageInfo":{"hasNextPage":true,"endCursor":"C1"},"nodes":[{"login":"me"}]}}}}}`,
		"slug=core -f after=C1": `{"data":{"organization":{"team":{"members":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"login":"bob"}]}}}}}`,
	}}
	members, err := NewClientWithExecutor(exec).TeamMembers("acme", "core")
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(members, ",") != "me,bob" {
		t.Fatalf("members = %v", members)
	}
}

func TestTeamMembersErrorMentionsReadOrg(t *testing.T) {
	tests := map[string]*routeExec{
		"team not visible": {route: map[string]string{"slug=core": `{"data":{"organization":{"team":null}}}`}},
		"graphql error":    {route: map[string]string{"slug=core": `{"errors":[{"message":"forbidden"}]}`}},
	}
	for name, exec := range tests {
		t.Run(name, func(t *testing.T) {
			_, err := NewClientWithExecutor(exec).TeamMembers("acme", "core")
			if err == nil || !strings.Contains(err.Error(), "read:org") || !strings.Contains(err.Error(), "@acme/core") {
				t.Fatalf("error = %v", err)
			}
		})
	}
}

func TestReviewRequestTimeline(t *testing.T) {
	exec := &routeExec{route: map[string]string{"REVIEW_REQUESTED_EVENT": `{"data":{"repository":{"pullRequest":{"timelineItems":{
		"pageInfo": {"hasNextPage": false, "endCursor": ""},
		"nodes": [
			{"__typename": "ReviewRequestedEvent", "createdAt": "2026-11-01T00:00:00Z", "actor": {"login": "alice"}, "requestedReviewer": {"__typename": "User", "login": "me"}},
			{"__typename": "ReviewRequestedEvent", "createdAt": "2026-11-01T00:00:01Z", "actor": {"login": "alice"}, "requestedReviewer": {"__typename": "Team", "slug": "core"}},
			{"__typename": "ReviewRequestRemovedEvent", "createdAt": "2026-11-02T00:00:00Z", "actor": {"login": "me"}, "requestedReviewer": {"__typename": "User", "login": "me"}},
			{"__typename": "ReviewRequestedEvent", "createdAt": "2026-11-03T00:00:00Z", "actor": null, "requestedReviewer": null}
		]
	}}}}}`}}
	events, err := NewClientWithExecutor(exec).ReviewRequestTimeline(handoffRef)
	if err != nil {
		t.Fatal(err)
	}
	want := []ReviewRequestEvent{
		{Event: ReviewRequested, Reviewer: "me", Actor: "alice", Time: "2026-11-01T00:00:00Z"},
		{Event: ReviewRequestRemoved, Reviewer: "me", Actor: "me", Time: "2026-11-02T00:00:00Z"},
	}
	if len(events) != len(want) {
		t.Fatalf("events = %+v", events)
	}
	for i := range want {
		if events[i] != want[i] {
			t.Fatalf("event %d = %+v, want %+v", i, events[i], want[i])
		}
	}
}

func TestReviewRequestTimelineReportsError(t *testing.T) {
	exec := &routeExec{route: map[string]string{"REVIEW_REQUESTED_EVENT": `{"data":{"repository":{"pullRequest":null}}}`}}
	if _, err := NewClientWithExecutor(exec).ReviewRequestTimeline(handoffRef); err == nil {
		t.Fatal("error = nil")
	}
}

func TestRequestReviewersPostsOnce(t *testing.T) {
	exec := &routeExec{route: map[string]string{"requested_reviewers": `{}`}}
	if err := NewClientWithExecutor(exec).RequestReviewers(handoffRef, []string{"colleague", "dana"}); err != nil {
		t.Fatal(err)
	}
	if len(exec.calls) != 1 || !strings.Contains(exec.calls[0], "--method POST repos/acme/widgets/pulls/12/requested_reviewers") {
		t.Fatalf("calls = %v", exec.calls)
	}
	var body map[string][]string
	if err := json.Unmarshal([]byte(exec.stdin[0]), &body); err != nil {
		t.Fatal(err)
	}
	if strings.Join(body["reviewers"], ",") != "colleague,dana" || len(body) != 1 {
		t.Fatalf("body = %v", body)
	}

	exec = &routeExec{fail: map[string]error{"requested_reviewers": errors.New("exit 1")}}
	err := NewClientWithExecutor(exec).RequestReviewers(handoffRef, []string{"colleague"})
	if err == nil || !strings.Contains(err.Error(), "acme/widgets#12") || len(exec.calls) != 1 {
		t.Fatalf("error = %v, calls = %v", err, exec.calls)
	}
}

func TestRemoveRequestedReviewerRemovesOnlyTheUser(t *testing.T) {
	exec := &routeExec{route: map[string]string{"requested_reviewers": `{}`}}
	if err := NewClientWithExecutor(exec).RemoveRequestedReviewer(handoffRef, "me"); err != nil {
		t.Fatal(err)
	}
	if len(exec.calls) != 1 || !strings.Contains(exec.calls[0], "--method DELETE repos/acme/widgets/pulls/12/requested_reviewers") {
		t.Fatalf("calls = %v", exec.calls)
	}
	var body map[string][]string
	if err := json.Unmarshal([]byte(exec.stdin[0]), &body); err != nil {
		t.Fatal(err)
	}
	if strings.Join(body["reviewers"], ",") != "me" || len(body) != 1 {
		t.Fatalf("body = %v; a team request must never be removed", body)
	}

	exec = &routeExec{fail: map[string]error{"requested_reviewers": errors.New("exit 1")}}
	if err := NewClientWithExecutor(exec).RemoveRequestedReviewer(handoffRef, "me"); err == nil || len(exec.calls) != 1 {
		t.Fatalf("error = %v, calls = %v", err, exec.calls)
	}
}
