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

func TestEvaluateRerequestBotAndExcludeFiltering(t *testing.T) {
	t0 := time.Date(2026, 11, 24, 10, 0, 0, 0, time.UTC)
	t1 := t0.Add(1 * time.Hour)
	t2 := t0.Add(2 * time.Hour)

	makePR := func(reviews []github.RerequestReview, threads []github.RerequestThread) github.RerequestPRState {
		return github.RerequestPRState{
			Number:          456,
			State:           "OPEN",
			Author:          "author",
			Reviews:         reviews,
			Threads:         threads,
			AuthorPushTimes: []time.Time{t2},
		}
	}

	t.Run("Bot reviewer with COMMENTED and answered threads is skipped", func(t *testing.T) {
		pr := makePR(
			[]github.RerequestReview{
				{Author: "copilot[bot]", AuthorType: "Bot", State: "COMMENTED", SubmittedAt: t1},
			},
			[]github.RerequestThread{
				{Comments: []github.RerequestThreadComment{
					{Author: "copilot[bot]", CreatedAt: t1},
					{Author: "author", CreatedAt: t2},
				}},
			},
		)

		got := EvaluateRerequestWithOptions(pr, RerequestOptions{})
		if len(got) != 1 {
			t.Fatalf("len(decisions) = %d, want 1", len(got))
		}
		if got[0].Action != RerequestActionSkip {
			t.Errorf("Action = %q, want %q", got[0].Action, RerequestActionSkip)
		}
		wantReason := "skipped: bot reviewer copilot[bot]"
		if got[0].Reason != wantReason {
			t.Errorf("Reason = %q, want %q", got[0].Reason, wantReason)
		}
	})

	t.Run("User reviewer in same state is still re-requested", func(t *testing.T) {
		pr := makePR(
			[]github.RerequestReview{
				{Author: "alice", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
			},
			[]github.RerequestThread{
				{Comments: []github.RerequestThreadComment{
					{Author: "alice", CreatedAt: t1},
					{Author: "author", CreatedAt: t2},
				}},
			},
		)

		got := EvaluateRerequestWithOptions(pr, RerequestOptions{})
		if len(got) != 1 {
			t.Fatalf("len(decisions) = %d, want 1", len(got))
		}
		if got[0].Action != RerequestActionWouldRerequest {
			t.Errorf("Action = %q, want %q (%s)", got[0].Action, RerequestActionWouldRerequest, got[0].Reason)
		}
	})

	t.Run("User login matching exclude pattern is skipped", func(t *testing.T) {
		pr := makePR(
			[]github.RerequestReview{
				{Author: "alice", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
				{Author: "agent-reviewer", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
				{Author: "lint-bot", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
				{Author: "ExactUser", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
			},
			nil,
		)
		opts := RerequestOptions{
			ExcludeReviewers: []string{"*-bot", "agent-*", "exactuser"},
		}

		got := EvaluateRerequestWithOptions(pr, opts)
		if len(got) != 4 {
			t.Fatalf("len(decisions) = %d, want 4", len(got))
		}
		byReviewer := make(map[string]RerequestDecision, len(got))
		for _, d := range got {
			byReviewer[d.Reviewer] = d
		}

		if d := byReviewer["alice"]; d.Action != RerequestActionWouldRerequest {
			t.Errorf("alice Action = %q, want %q", d.Action, RerequestActionWouldRerequest)
		}
		for _, login := range []string{"agent-reviewer", "lint-bot", "ExactUser"} {
			d := byReviewer[login]
			if d.Action != RerequestActionSkip {
				t.Errorf("%s Action = %q, want %q", login, d.Action, RerequestActionSkip)
			}
			wantReason := "skipped: bot reviewer " + login
			if d.Reason != wantReason {
				t.Errorf("%s Reason = %q, want %q", login, d.Reason, wantReason)
			}
		}
	})

	t.Run("--include-bots disables both Bot type check and exclude list", func(t *testing.T) {
		pr := makePR(
			[]github.RerequestReview{
				{Author: "copilot[bot]", AuthorType: "Bot", State: "COMMENTED", SubmittedAt: t1},
				{Author: "agent-reviewer", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
			},
			[]github.RerequestThread{
				{Comments: []github.RerequestThreadComment{
					{Author: "copilot[bot]", CreatedAt: t1},
					{Author: "author", CreatedAt: t2},
				}},
				{Comments: []github.RerequestThreadComment{
					{Author: "agent-reviewer", CreatedAt: t1},
					{Author: "author", CreatedAt: t2},
				}},
			},
		)
		opts := RerequestOptions{
			IncludeBots:      true,
			ExcludeReviewers: []string{"agent-*"},
		}

		got := EvaluateRerequestWithOptions(pr, opts)
		if len(got) != 2 {
			t.Fatalf("len(decisions) = %d, want 2", len(got))
		}
		for _, d := range got {
			if d.Action != RerequestActionWouldRerequest {
				t.Errorf("%s Action = %q (reason %q), want %q", d.Reviewer, d.Action, d.Reason, RerequestActionWouldRerequest)
			}
		}
	})

	t.Run("bot reason appears in both text and json output and is not applied", func(t *testing.T) {
		pr := makePR(
			[]github.RerequestReview{
				{Author: "alice", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
				{Author: "ci-bot", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
				{Author: "dependabot[bot]", AuthorType: "Bot", State: "COMMENTED", SubmittedAt: t1},
			},
			nil,
		)
		ref := github.PullRef{Owner: "acme", Repo: "widgets", Number: 456}
		opts := RerequestOptions{
			Apply:            true,
			ExcludeReviewers: []string{"*-bot"},
		}

		client := &fakeRerequestClient{state: pr}
		var textBuf bytes.Buffer
		if err := RunRerequest(client, ref, opts, &textBuf); err != nil {
			t.Fatalf("RunRerequest(text) error = %v", err)
		}
		if want := [][]string{{"alice"}}; !reflect.DeepEqual(client.requested, want) {
			t.Fatalf("RequestReviewers = %v, want %v", client.requested, want)
		}
		textOut := textBuf.String()
		for _, wantSub := range []string{
			"skipped: bot reviewer ci-bot",
			"skipped: bot reviewer dependabot[bot]",
		} {
			if !strings.Contains(textOut, wantSub) {
				t.Errorf("text output missing %q:\n%s", wantSub, textOut)
			}
		}

		jsonOpts := opts
		jsonOpts.Apply = false
		jsonOpts.JSON = true
		var jsonBuf bytes.Buffer
		if err := RunRerequest(&fakeRerequestClient{state: pr}, ref, jsonOpts, &jsonBuf); err != nil {
			t.Fatalf("RunRerequest(json) error = %v", err)
		}
		var parsed []RerequestDecision
		if err := json.Unmarshal(jsonBuf.Bytes(), &parsed); err != nil {
			t.Fatalf("unmarshal json: %v\n%s", err, jsonBuf.String())
		}
		if len(parsed) != 3 {
			t.Fatalf("len(parsed) = %d, want 3", len(parsed))
		}
		if parsed[1].Reviewer != "ci-bot" || parsed[1].Action != RerequestActionSkip || parsed[1].Reason != "skipped: bot reviewer ci-bot" {
			t.Errorf("parsed[1] = %+v", parsed[1])
		}
		if parsed[2].Reviewer != "dependabot[bot]" || parsed[2].Action != RerequestActionSkip || parsed[2].Reason != "skipped: bot reviewer dependabot[bot]" {
			t.Errorf("parsed[2] = %+v", parsed[2])
		}
	})
}
