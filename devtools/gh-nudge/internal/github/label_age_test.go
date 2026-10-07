package github

import (
	"errors"
	"strings"
	"testing"
	"time"
)

func TestLatestLabelAddedAt(t *testing.T) {
	const prURL = "https://github.com/org/repo/pull/123"
	events := `[
		{"event":"labeled","created_at":"2026-10-01T00:00:00Z","label":{"name":"X"}},
		{"event":"unlabeled","created_at":"2026-10-03T00:00:00Z","label":{"name":"X"}},
		{"event":"labeled","created_at":"2026-10-05T00:00:00Z","label":{"name":"X"}},
		{"event":"labeled","created_at":"2026-10-04T00:00:00Z","label":{"name":"Y"}},
		{"event":"review_requested","created_at":"2026-10-06T00:00:00Z"},
		{"event":"labeled","created_at":"2026-10-02T00:00:00Z","label":null},
		{"event":"labeled","label":{"name":"Z"}}
	]`

	t.Run("returns the newest labeled time for each requested label", func(t *testing.T) {
		exec := &scriptedExecutor{outputs: []string{events}}
		client := NewClientWithExecutor(exec)
		got, err := client.LatestLabelAddedAt(prURL, []string{"X", "Y", "missing"})
		if err != nil {
			t.Fatalf("LatestLabelAddedAt() error = %v", err)
		}
		wantX := time.Date(2026, 10, 5, 0, 0, 0, 0, time.UTC)
		wantY := time.Date(2026, 10, 4, 0, 0, 0, 0, time.UTC)
		if !got["X"].Equal(wantX) {
			t.Errorf("X added at %v, want %v", got["X"], wantX)
		}
		if !got["Y"].Equal(wantY) {
			t.Errorf("Y added at %v, want %v", got["Y"], wantY)
		}
		if _, ok := got["missing"]; ok {
			t.Errorf("missing label should be absent, got %v", got["missing"])
		}
		if _, ok := got["Z"]; ok {
			t.Errorf("unrequested label Z should be absent, got %v", got["Z"])
		}
		if len(exec.calls) != 1 {
			t.Fatalf("gh calls = %d, want 1", len(exec.calls))
		}
		call := exec.calls[0]
		if call.cmd != "gh" {
			t.Errorf("cmd = %q, want gh", call.cmd)
		}
		joined := strings.Join(call.args, " ")
		if !strings.Contains(joined, "api") || !strings.Contains(joined, "--paginate") {
			t.Errorf("args = %q, want gh api --paginate", joined)
		}
		if !strings.Contains(joined, "repos/org/repo/issues/123/events?per_page=100") {
			t.Errorf("args = %q, want issue events path", joined)
		}
	})

	t.Run("uses the latest timestamp when events are newest first", func(t *testing.T) {
		reversed := `[
			{"event":"labeled","created_at":"2026-10-05T00:00:00Z","label":{"name":"X"}},
			{"event":"labeled","created_at":"2026-10-01T00:00:00Z","label":{"name":"X"}}
		]`
		client := NewClientWithExecutor(&scriptedExecutor{outputs: []string{reversed}})
		got, err := client.LatestLabelAddedAt(prURL, []string{"X"})
		if err != nil {
			t.Fatalf("LatestLabelAddedAt() error = %v", err)
		}
		want := time.Date(2026, 10, 5, 0, 0, 0, 0, time.UTC)
		if !got["X"].Equal(want) {
			t.Errorf("X added at %v, want %v", got["X"], want)
		}
	})

	t.Run("does not call gh when no labels are requested", func(t *testing.T) {
		exec := &scriptedExecutor{}
		client := NewClientWithExecutor(exec)
		got, err := client.LatestLabelAddedAt(prURL, nil)
		if err != nil {
			t.Fatalf("LatestLabelAddedAt() error = %v", err)
		}
		if len(got) != 0 {
			t.Errorf("got %v, want empty map", got)
		}
		if len(exec.calls) != 0 {
			t.Errorf("gh calls = %d, want 0", len(exec.calls))
		}
	})

	t.Run("returns an error when gh fails", func(t *testing.T) {
		exec := &scriptedExecutor{outputs: []string{"nope"}, errs: []error{errors.New("command failed")}}
		client := NewClientWithExecutor(exec)
		got, err := client.LatestLabelAddedAt(prURL, []string{"X"})
		if err == nil {
			t.Fatal("expected error when gh fails")
		}
		if got != nil {
			t.Errorf("got %v, want nil map on error", got)
		}
	})

	t.Run("returns an error when the payload is not a JSON array", func(t *testing.T) {
		client := NewClientWithExecutor(&scriptedExecutor{outputs: []string{`{"message":"Not Found"}`}})
		got, err := client.LatestLabelAddedAt(prURL, []string{"X"})
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
		_, err := client.LatestLabelAddedAt("https://example.com/not-a-pr", []string{"X"})
		if err == nil {
			t.Fatal("expected error for unparseable pull request URL")
		}
		if len(exec.calls) != 0 {
			t.Errorf("gh calls = %d, want 0", len(exec.calls))
		}
	})
}

type scriptedCall struct {
	cmd  string
	args []string
}

type scriptedExecutor struct {
	outputs []string
	errs    []error
	calls   []scriptedCall
}

func (s *scriptedExecutor) Execute(cmd string, args ...string) (string, error) {
	s.calls = append(s.calls, scriptedCall{cmd: cmd, args: append([]string(nil), args...)})
	i := len(s.calls) - 1
	var output string
	if i < len(s.outputs) {
		output = s.outputs[i]
	}
	var err error
	if i < len(s.errs) {
		err = s.errs[i]
	}
	if err == nil && i >= len(s.outputs) {
		err = errors.New("unexpected gh call")
	}
	return output, err
}

func (s *scriptedExecutor) ExecuteWithStdin(_ string, cmd string, args ...string) (string, error) {
	return s.Execute(cmd, args...)
}
