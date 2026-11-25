package github

import (
	"errors"
	"testing"
	"time"
)

func TestLoadRerequestPRState(t *testing.T) {
	fixture := `{"data":{"repository":{"pullRequest":{
		"number": 123,
		"state": "OPEN",
		"isDraft": false,
		"author": {"login": "author1"},
		"reviewRequests": {
			"pageInfo": {"hasNextPage": false, "endCursor": ""},
			"nodes": [
				{"requestedReviewer": {"__typename": "User", "login": "pending-user"}},
				{"requestedReviewer": {"__typename": "Team", "slug": "core-team"}}
			]
		},
		"reviews": {
			"pageInfo": {"hasNextPage": false, "endCursor": ""},
			"nodes": [
				{"author": {"login": "alice"}, "state": "APPROVED", "submittedAt": "2026-11-24T10:00:00Z"},
				{"author": {"login": "bob"}, "state": "CHANGES_REQUESTED", "submittedAt": "2026-11-24T11:00:00Z"},
				{"author": {"login": "draft-rev"}, "state": "PENDING", "submittedAt": ""}
			]
		},
		"reviewThreads": {
			"pageInfo": {"hasNextPage": false, "endCursor": ""},
			"nodes": [
				{
					"id": "T1",
					"isResolved": false,
					"comments": {
						"pageInfo": {"hasNextPage": false, "endCursor": ""},
						"nodes": [
							{"author": {"login": "bob"}, "createdAt": "2026-11-24T11:00:00Z"},
							{"author": {"login": "author1"}, "createdAt": "2026-11-24T12:00:00Z"}
						]
					}
				}
			]
		},
		"commits": {
			"pageInfo": {"hasNextPage": false, "endCursor": ""},
			"nodes": [
				{
					"commit": {
						"committedDate": "2026-11-24T12:30:00Z",
						"pushedDate": "2026-11-24T12:35:00Z",
						"author": {"user": {"login": "author1"}},
						"committer": {"user": {"login": "author1"}}
					}
				},
				{
					"commit": {
						"committedDate": "2026-11-24T12:40:00Z",
						"pushedDate": "",
						"author": {"user": {"login": "other-user"}},
						"committer": {"user": {"login": "other-user"}}
					}
				}
			]
		},
		"timelineItems": {
			"pageInfo": {"hasNextPage": false, "endCursor": ""},
			"nodes": [
				{"createdAt": "2026-11-24T13:00:00Z", "actor": {"login": "author1"}},
				{"createdAt": "2026-11-24T13:05:00Z", "actor": {"login": "other-user"}}
			]
		}
	}}}}`

	exec := &routeExec{route: map[string]string{"HEAD_REF_FORCE_PUSHED_EVENT": fixture}}
	ref := PullRef{Owner: "acme", Repo: "widgets", Number: 123}
	got, err := NewClientWithExecutor(exec).LoadRerequestPRState(ref)
	if err != nil {
		t.Fatalf("LoadRerequestPRState() error = %v", err)
	}

	if got.Number != 123 || got.State != "OPEN" || got.IsDraft || got.Author != "author1" {
		t.Fatalf("metadata = %+v", got)
	}
	if len(got.RequestedUsers) != 1 || got.RequestedUsers[0] != "pending-user" {
		t.Fatalf("RequestedUsers = %v", got.RequestedUsers)
	}
	if len(got.RequestedTeams) != 1 || got.RequestedTeams[0] != "core-team" {
		t.Fatalf("RequestedTeams = %v", got.RequestedTeams)
	}
	if len(got.Reviews) != 2 || got.Reviews[0].Author != "alice" || got.Reviews[1].Author != "bob" {
		t.Fatalf("Reviews = %+v", got.Reviews)
	}
	if len(got.Threads) != 1 || len(got.Threads[0].Comments) != 2 {
		t.Fatalf("Threads = %+v", got.Threads)
	}
	wantPush1 := time.Date(2026, 11, 24, 12, 35, 0, 0, time.UTC)
	wantPush2 := time.Date(2026, 11, 24, 13, 0, 0, 0, time.UTC)
	if len(got.AuthorPushTimes) != 2 || !got.AuthorPushTimes[0].Equal(wantPush1) || !got.AuthorPushTimes[1].Equal(wantPush2) {
		t.Fatalf("AuthorPushTimes = %v, want [%v %v]", got.AuthorPushTimes, wantPush1, wantPush2)
	}
}

func TestLoadRerequestPRStatePaginatesThreadsAndComments(t *testing.T) {
	page1 := `{"data":{"repository":{"pullRequest":{
		"number": 7,
		"state": "OPEN",
		"isDraft": false,
		"author": {"login": "author1"},
		"reviewRequests": {"pageInfo": {"hasNextPage": false, "endCursor": ""}, "nodes": []},
		"reviews": {"pageInfo": {"hasNextPage": false, "endCursor": ""}, "nodes": []},
		"reviewThreads": {
			"pageInfo": {"hasNextPage": true, "endCursor": "TH1"},
			"nodes": [{
				"id": "THREAD_1",
				"isResolved": false,
				"comments": {
					"pageInfo": {"hasNextPage": true, "endCursor": "CM1"},
					"nodes": [{"author": {"login": "bob"}, "createdAt": "2026-11-24T10:00:00Z"}]
				}
			}]
		},
		"commits": {"pageInfo": {"hasNextPage": false, "endCursor": ""}, "nodes": []},
		"timelineItems": {"pageInfo": {"hasNextPage": false, "endCursor": ""}, "nodes": []}
	}}}}`
	commentsPage2 := `{"data":{"node":{"comments":{
		"pageInfo": {"hasNextPage": false, "endCursor": ""},
		"nodes": [{"author": {"login": "author1"}, "createdAt": "2026-11-24T11:00:00Z"}]
	}}}}`
	threadsPage2 := `{"data":{"repository":{"pullRequest":{"reviewThreads":{
		"pageInfo": {"hasNextPage": false, "endCursor": ""},
		"nodes": [{
			"id": "THREAD_2",
			"isResolved": true,
			"comments": {
				"pageInfo": {"hasNextPage": false, "endCursor": ""},
				"nodes": [{"author": {"login": "carol"}, "createdAt": "2026-11-24T12:00:00Z"}]
			}
		}]
	}}}}}`

	exec := &routeExec{route: map[string]string{
		"-f id=THREAD_1 -f after=CM1": commentsPage2,
		"-f after=TH1":                threadsPage2,
		"HEAD_REF_FORCE_PUSHED_EVENT": page1,
	}}
	got, err := NewClientWithExecutor(exec).LoadRerequestPRState(PullRef{Owner: "acme", Repo: "widgets", Number: 7})
	if err != nil {
		t.Fatalf("LoadRerequestPRState() error = %v", err)
	}
	if len(got.Threads) != 2 {
		t.Fatalf("len(Threads) = %d, want 2", len(got.Threads))
	}
	if len(got.Threads[0].Comments) != 2 || got.Threads[0].Comments[1].Author != "author1" {
		t.Fatalf("Threads[0] = %+v", got.Threads[0])
	}
	if !got.Threads[1].IsResolved || got.Threads[1].Comments[0].Author != "carol" {
		t.Fatalf("Threads[1] = %+v", got.Threads[1])
	}
}

func TestLoadRerequestPRStateErrors(t *testing.T) {
	tests := []struct {
		name string
		exec *routeExec
	}{
		{
			name: "not found",
			exec: &routeExec{route: map[string]string{"HEAD_REF_FORCE_PUSHED_EVENT": `{"data":{"repository":{"pullRequest":null}}}`}},
		},
		{
			name: "gh error",
			exec: &routeExec{fail: map[string]error{"HEAD_REF_FORCE_PUSHED_EVENT": errors.New("auth failed")}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if _, err := NewClientWithExecutor(tc.exec).LoadRerequestPRState(handoffRef); err == nil {
				t.Fatal("expected error, got nil")
			}
		})
	}
}
