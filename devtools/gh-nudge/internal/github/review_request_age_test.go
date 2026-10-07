package github

import (
	"errors"
	"strings"
	"testing"
	"time"
)

func TestEarliestReviewRequestedAt(t *testing.T) {
	const prURL = "https://github.com/org/repo/pull/123"
	events := `[
		{"event":"review_requested","created_at":"2026-10-05T00:00:00Z","requested_reviewer":{"login":"alice"}},
		{"event":"review_request_removed","created_at":"2026-10-06T00:00:00Z","requested_reviewer":{"login":"alice"}},
		{"event":"review_requested","created_at":"2026-10-07T00:00:00Z","requested_reviewer":{"login":"alice"}},
		{"event":"review_requested","created_at":"2026-10-04T00:00:00Z","requested_reviewer":{"login":"bob"}},
		{"event":"review_requested","created_at":"2026-10-03T00:00:00Z","requested_reviewer":{"login":"Alice"}},
		{"event":"review_requested","created_at":"2026-10-02T00:00:00Z","requested_team":{"slug":"devs"}},
		{"event":"review_requested","created_at":"2026-10-01T00:00:00Z","requested_reviewer":{"login":""}},
		{"event":"review_requested","requested_reviewer":{"login":"cara"}},
		{"event":"labeled","created_at":"2026-10-08T00:00:00Z","label":{"name":"X"}}
	]`

	t.Run("returns the earliest review request for each user", func(t *testing.T) {
		exec := &scriptedExecutor{outputs: []string{events}}
		client := NewClientWithExecutor(exec)
		got, err := client.EarliestReviewRequestedAt(prURL)
		if err != nil {
			t.Fatalf("EarliestReviewRequestedAt() error = %v", err)
		}
		wantAlice := time.Date(2026, 10, 5, 0, 0, 0, 0, time.UTC)
		wantBob := time.Date(2026, 10, 4, 0, 0, 0, 0, time.UTC)
		wantAliceCase := time.Date(2026, 10, 3, 0, 0, 0, 0, time.UTC)
		if !got["alice"].Equal(wantAlice) {
			t.Errorf("alice requested at %v, want %v", got["alice"], wantAlice)
		}
		if !got["bob"].Equal(wantBob) {
			t.Errorf("bob requested at %v, want %v", got["bob"], wantBob)
		}
		if !got["Alice"].Equal(wantAliceCase) {
			t.Errorf("Alice requested at %v, want %v", got["Alice"], wantAliceCase)
		}
		if _, ok := got["devs"]; ok {
			t.Errorf("team request should be absent, got %v", got["devs"])
		}
		if _, ok := got[""]; ok {
			t.Error("empty login should be absent")
		}
		if _, ok := got["cara"]; ok {
			t.Errorf("request with no timestamp should be absent, got %v", got["cara"])
		}
		if len(exec.calls) != 1 {
			t.Fatalf("gh calls = %d, want 1", len(exec.calls))
		}
		joined := strings.Join(exec.calls[0].args, " ")
		if exec.calls[0].cmd != "gh" || !strings.Contains(joined, "api") || !strings.Contains(joined, "--paginate") {
			t.Errorf("call = gh %s, want gh api --paginate", joined)
		}
		if !strings.Contains(joined, "repos/org/repo/issues/123/events?per_page=100") {
			t.Errorf("args = %q, want issue events path", joined)
		}
	})

	t.Run("keeps the earliest timestamp when events are newest first", func(t *testing.T) {
		reversed := `[
			{"event":"review_requested","created_at":"2026-10-07T00:00:00Z","requested_reviewer":{"login":"alice"}},
			{"event":"review_requested","created_at":"2026-10-01T00:00:00Z","requested_reviewer":{"login":"alice"}}
		]`
		client := NewClientWithExecutor(&scriptedExecutor{outputs: []string{reversed}})
		got, err := client.EarliestReviewRequestedAt(prURL)
		if err != nil {
			t.Fatalf("EarliestReviewRequestedAt() error = %v", err)
		}
		want := time.Date(2026, 10, 1, 0, 0, 0, 0, time.UTC)
		if !got["alice"].Equal(want) {
			t.Errorf("alice requested at %v, want %v", got["alice"], want)
		}
	})

	t.Run("returns an error when gh fails", func(t *testing.T) {
		exec := &scriptedExecutor{outputs: []string{"nope"}, errs: []error{errors.New("command failed")}}
		client := NewClientWithExecutor(exec)
		got, err := client.EarliestReviewRequestedAt(prURL)
		if err == nil {
			t.Fatal("expected error when gh fails")
		}
		if got != nil {
			t.Errorf("got %v, want nil map on error", got)
		}
	})

	t.Run("returns an error when the payload is not a JSON array", func(t *testing.T) {
		client := NewClientWithExecutor(&scriptedExecutor{outputs: []string{`{"message":"Not Found"}`}})
		got, err := client.EarliestReviewRequestedAt(prURL)
		if err == nil {
			t.Fatal("expected error for non-array payload")
		}
		if got != nil {
			t.Errorf("got %v, want nil map on error", got)
		}
	})

	t.Run("returns an error for a URL that is not a pull request", func(t *testing.T) {
		exec := &scriptedExecutor{}
		client := NewClientWithExecutor(exec)
		_, err := client.EarliestReviewRequestedAt("https://example.com/not-a-pr")
		if err == nil {
			t.Fatal("expected error for unparseable pull request URL")
		}
		if len(exec.calls) != 0 {
			t.Errorf("gh calls = %d, want 0", len(exec.calls))
		}
	})
}
