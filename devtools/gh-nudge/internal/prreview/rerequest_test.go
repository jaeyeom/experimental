package prreview

import (
	"bytes"
	"encoding/json"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

type fakeRerequestClient struct {
	state      github.RerequestPRState
	loadErr    error
	requestErr error
	requested  [][]string
}

func (f *fakeRerequestClient) LoadRerequestPRState(_ github.PullRef) (github.RerequestPRState, error) {
	if f.loadErr != nil {
		return github.RerequestPRState{}, f.loadErr
	}
	return f.state, nil
}

func (f *fakeRerequestClient) RequestReviewers(_ github.PullRef, logins []string) error {
	cp := append([]string(nil), logins...)
	f.requested = append(f.requested, cp)
	return f.requestErr
}

func TestEvaluateRerequestRules(t *testing.T) {
	t0 := time.Date(2026, 11, 24, 10, 0, 0, 0, time.UTC)
	t1 := t0.Add(1 * time.Hour)
	t2 := t0.Add(2 * time.Hour)
	t3 := t0.Add(3 * time.Hour)

	tests := []struct {
		name               string
		pr                 github.RerequestPRState
		wantAction         string
		wantReasonContains string
		wantReviewer       string
		wantThreadsTotal   int
		wantThreadsAns     int
	}{
		{
			name: "approved reviewer is always skipped",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "OPEN",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "alice", State: "APPROVED", SubmittedAt: t1},
				},
				AuthorPushTimes: []time.Time{t2},
			},
			wantReviewer:       "alice",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "approved",
		},
		{
			name: "approved reviewer who later commented is still skipped",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "OPEN",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "alice", State: "APPROVED", SubmittedAt: t1},
					{Author: "alice", State: "COMMENTED", SubmittedAt: t2},
				},
				AuthorPushTimes: []time.Time{t3},
			},
			wantReviewer:       "alice",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "approved",
		},
		{
			name: "thread without author reply is skipped",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "OPEN",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "bob", State: "CHANGES_REQUESTED", SubmittedAt: t1},
				},
				Threads: []github.RerequestThread{
					{
						IsResolved: false,
						Comments: []github.RerequestThreadComment{
							{Author: "bob", CreatedAt: t1},
							{Author: "author", CreatedAt: t2},
						},
					},
					{
						IsResolved: false,
						Comments: []github.RerequestThreadComment{
							{Author: "bob", CreatedAt: t1},
						},
					},
					{
						IsResolved: false,
						Comments: []github.RerequestThreadComment{
							{Author: "bob", CreatedAt: t1},
						},
					},
				},
				AuthorPushTimes: []time.Time{t3},
			},
			wantReviewer:       "bob",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "2 of 3 threads have no author reply",
			wantThreadsTotal:   3,
			wantThreadsAns:     1,
		},
		{
			name: "resolved thread without author reply still counts as unanswered",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "OPEN",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "bob", State: "CHANGES_REQUESTED", SubmittedAt: t1},
				},
				Threads: []github.RerequestThread{
					{
						IsResolved: true,
						Comments: []github.RerequestThreadComment{
							{Author: "bob", CreatedAt: t1},
						},
					},
				},
				AuthorPushTimes: []time.Time{t2},
			},
			wantReviewer:       "bob",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "1 of 1 threads have no author reply",
			wantThreadsTotal:   1,
			wantThreadsAns:     0,
		},
		{
			name: "no author activity since last review is skipped",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "OPEN",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "carol", State: "CHANGES_REQUESTED", SubmittedAt: t2},
				},
				Threads: []github.RerequestThread{
					{
						IsResolved: false,
						Comments: []github.RerequestThreadComment{
							{Author: "carol", CreatedAt: t0},
							{Author: "author", CreatedAt: t1},
						},
					},
				},
				AuthorPushTimes: []time.Time{t1},
			},
			wantReviewer:       "carol",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "no author activity since last review",
			wantThreadsTotal:   1,
			wantThreadsAns:     1,
		},
		{
			name: "reviewer with pending request is skipped",
			pr: github.RerequestPRState{
				Number:         123,
				State:          "OPEN",
				Author:         "author",
				RequestedUsers: []string{"dave"},
				Reviews: []github.RerequestReview{
					{Author: "dave", State: "CHANGES_REQUESTED", SubmittedAt: t1},
				},
				AuthorPushTimes: []time.Time{t2},
			},
			wantReviewer:       "dave",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "review request already pending",
		},
		{
			name: "draft PR skips whole PR",
			pr: github.RerequestPRState{
				Number:  123,
				State:   "OPEN",
				IsDraft: true,
				Author:  "author",
				Reviews: []github.RerequestReview{
					{Author: "carol", State: "CHANGES_REQUESTED", SubmittedAt: t1},
				},
				AuthorPushTimes: []time.Time{t2},
			},
			wantReviewer:       "",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "PR is draft",
		},
		{
			name: "closed PR skips whole PR",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "CLOSED",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "carol", State: "CHANGES_REQUESTED", SubmittedAt: t1},
				},
				AuthorPushTimes: []time.Time{t2},
			},
			wantReviewer:       "",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "PR is closed",
		},
		{
			name: "merged PR skips whole PR",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "MERGED",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "carol", State: "CHANGES_REQUESTED", SubmittedAt: t1},
				},
				AuthorPushTimes: []time.Time{t2},
			},
			wantReviewer:       "",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "PR is merged",
		},
		{
			name: "author as reviewer is skipped",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "OPEN",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "author", State: "COMMENTED", SubmittedAt: t1},
				},
				AuthorPushTimes: []time.Time{t2},
			},
			wantReviewer:       "author",
			wantAction:         RerequestActionSkip,
			wantReasonContains: "reviewer is PR author",
		},
		{
			name: "team reviewers are untouched and do not block user rerequest",
			pr: github.RerequestPRState{
				Number:         123,
				State:          "OPEN",
				Author:         "author",
				RequestedTeams: []string{"core-team"},
				Reviews: []github.RerequestReview{
					{Author: "carol", State: "COMMENTED", SubmittedAt: t1},
				},
				AuthorPushTimes: []time.Time{t2},
			},
			wantReviewer:       "carol",
			wantAction:         RerequestActionWouldRerequest,
			wantReasonContains: "author pushed after review",
		},
		{
			name: "author reply in reviewer thread after review qualifies",
			pr: github.RerequestPRState{
				Number: 123,
				State:  "OPEN",
				Author: "author",
				Reviews: []github.RerequestReview{
					{Author: "carol", State: "COMMENTED", SubmittedAt: t1},
				},
				Threads: []github.RerequestThread{
					{
						IsResolved: false,
						Comments: []github.RerequestThreadComment{
							{Author: "carol", CreatedAt: t1},
							{Author: "author", CreatedAt: t2},
						},
					},
				},
			},
			wantReviewer:       "carol",
			wantAction:         RerequestActionWouldRerequest,
			wantReasonContains: "author replied after review, 1 thread answered",
			wantThreadsTotal:   1,
			wantThreadsAns:     1,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := EvaluateRerequest(tc.pr)
			if len(got) != 1 {
				t.Fatalf("len(decisions) = %d, want 1 (%+v)", len(got), got)
			}
			d := got[0]
			if d.Reviewer != tc.wantReviewer {
				t.Errorf("Reviewer = %q, want %q", d.Reviewer, tc.wantReviewer)
			}
			if d.Action != tc.wantAction {
				t.Errorf("Action = %q, want %q", d.Action, tc.wantAction)
			}
			if !strings.Contains(d.Reason, tc.wantReasonContains) {
				t.Errorf("Reason = %q, want substring %q", d.Reason, tc.wantReasonContains)
			}
			if d.ThreadsTotal != tc.wantThreadsTotal || d.ThreadsAnswered != tc.wantThreadsAns {
				t.Errorf("threads = %d/%d, want %d/%d", d.ThreadsAnswered, d.ThreadsTotal, tc.wantThreadsAns, tc.wantThreadsTotal)
			}
		})
	}
}

func TestRunRerequestFixtureDryRunAndApply(t *testing.T) {
	t0 := time.Date(2026, 11, 24, 10, 0, 0, 0, time.UTC)
	t1 := t0.Add(1 * time.Hour)
	t2 := t0.Add(2 * time.Hour)

	// Fixture PR with one reviewer each in APPROVED (alice), COMMENTED (bob),
	// and CHANGES_REQUESTED (carol), plus a team reviewer that must stay untouched.
	fixture := github.RerequestPRState{
		Number:         123,
		State:          "OPEN",
		IsDraft:        false,
		Author:         "author",
		RequestedTeams: []string{"platform"},
		Reviews: []github.RerequestReview{
			{Author: "alice", State: "APPROVED", SubmittedAt: t1},
			{Author: "bob", State: "COMMENTED", SubmittedAt: t1},
			{Author: "carol", State: "CHANGES_REQUESTED", SubmittedAt: t1},
		},
		Threads: []github.RerequestThread{
			// bob has 1 answered and 2 unanswered threads -> skip.
			{Comments: []github.RerequestThreadComment{{Author: "bob", CreatedAt: t1}, {Author: "author", CreatedAt: t2}}},
			{Comments: []github.RerequestThreadComment{{Author: "bob", CreatedAt: t1}}},
			{IsResolved: true, Comments: []github.RerequestThreadComment{{Author: "bob", CreatedAt: t1}}},
			// carol has 2 threads, both answered -> rerequest.
			{Comments: []github.RerequestThreadComment{{Author: "carol", CreatedAt: t1}, {Author: "author", CreatedAt: t2}}},
			{Comments: []github.RerequestThreadComment{{Author: "carol", CreatedAt: t1}, {Author: "author", CreatedAt: t2}}},
		},
		AuthorPushTimes: []time.Time{t2},
	}

	ref := github.PullRef{Owner: "acme", Repo: "widgets", Number: 123}

	t.Run("dry run", func(t *testing.T) {
		client := &fakeRerequestClient{state: fixture}
		var buf bytes.Buffer
		if err := RunRerequest(client, ref, RerequestOptions{Apply: false}, &buf); err != nil {
			t.Fatalf("RunRerequest() error = %v", err)
		}
		if len(client.requested) != 0 {
			t.Fatalf("dry run called RequestReviewers: %v", client.requested)
		}
		wantLines := []string{
			"#123 alice skip: approved",
			"#123 bob skip: 2 of 3 threads have no author reply",
			"#123 carol would rerequest: author pushed and replied after review, all 2 threads answered",
		}
		gotLines := strings.Split(strings.TrimSuffix(buf.String(), "\n"), "\n")
		if !reflect.DeepEqual(gotLines, wantLines) {
			t.Fatalf("output =\n%s\nwant:\n%s", buf.String(), strings.Join(wantLines, "\n"))
		}
	})

	t.Run("apply calls RequestReviewers with exact expected logins", func(t *testing.T) {
		client := &fakeRerequestClient{state: fixture}
		var buf bytes.Buffer
		if err := RunRerequest(client, ref, RerequestOptions{Apply: true}, &buf); err != nil {
			t.Fatalf("RunRerequest() error = %v", err)
		}
		wantRequested := [][]string{{"carol"}}
		if !reflect.DeepEqual(client.requested, wantRequested) {
			t.Fatalf("RequestReviewers calls = %v, want %v", client.requested, wantRequested)
		}
		if !strings.Contains(buf.String(), "#123 carol rerequest: ") {
			t.Fatalf("output missing applied rerequest line:\n%s", buf.String())
		}
	})

	t.Run("json output shape", func(t *testing.T) {
		client := &fakeRerequestClient{state: fixture}
		var buf bytes.Buffer
		if err := RunRerequest(client, ref, RerequestOptions{JSON: true}, &buf); err != nil {
			t.Fatalf("RunRerequest() error = %v", err)
		}

		var raw []map[string]any
		if err := json.Unmarshal(buf.Bytes(), &raw); err != nil {
			t.Fatalf("unmarshal JSON output: %v\n%s", err, buf.String())
		}
		if len(raw) != 3 {
			t.Fatalf("len(raw) = %d, want 3", len(raw))
		}
		wantKeys := []string{
			"pr",
			"reviewer",
			"action",
			"reason",
			"threads_total",
			"threads_answered",
			"last_review_state",
			"last_review_at",
			"author_activity_at",
		}
		for i, item := range raw {
			for _, k := range wantKeys {
				if _, ok := item[k]; !ok {
					t.Errorf("item[%d] missing key %q: %v", i, k, item)
				}
			}
			if len(item) != len(wantKeys) {
				t.Errorf("item[%d] has %d keys, want %d: %v", i, len(item), len(wantKeys), item)
			}
		}
		if raw[2]["reviewer"] != "carol" || raw[2]["action"] != RerequestActionWouldRerequest ||
			raw[2]["last_review_state"] != "CHANGES_REQUESTED" {
			t.Errorf("unexpected carol decision: %v", raw[2])
		}
	})

	t.Run("apply failure is reported per reviewer and returns error", func(t *testing.T) {
		client := &fakeRerequestClient{
			state:      fixture,
			requestErr: errors.New("422 unprocessable entity"),
		}
		var buf bytes.Buffer
		err := RunRerequest(client, ref, RerequestOptions{Apply: true}, &buf)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(buf.String(), "#123 carol error: rerequest failed: 422 unprocessable entity") {
			t.Fatalf("output missing per-reviewer error:\n%s", buf.String())
		}
	})

	t.Run("load error returns error", func(t *testing.T) {
		client := &fakeRerequestClient{loadErr: errors.New("auth error")}
		var buf bytes.Buffer
		if err := RunRerequest(client, ref, RerequestOptions{}, &buf); err == nil {
			t.Fatal("expected error, got nil")
		}
	})
}
