package source_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestGitHubIDRoundTrip(t *testing.T) {
	tests := []struct {
		id   string
		repo string
		num  int
	}{
		{id: "owner/repo#12", repo: "owner/repo", num: 12},
		{id: "jaeyeom/agentic-factory#1", repo: "jaeyeom/agentic-factory", num: 1},
		{id: "a/b#99999", repo: "a/b", num: 99999},
	}
	for _, tc := range tests {
		t.Run(tc.id, func(t *testing.T) {
			repo, num, err := source.ParseGitHubID(tc.id)
			if err != nil {
				t.Fatalf("ParseGitHubID(%q) error = %v", tc.id, err)
			}
			if repo != tc.repo || num != tc.num {
				t.Fatalf("ParseGitHubID(%q) = (%q, %d), want (%q, %d)", tc.id, repo, num, tc.repo, tc.num)
			}
			if got := source.GitHubID(repo, num); got != tc.id {
				t.Fatalf("GitHubID(%q, %d) = %q, want %q", repo, num, got, tc.id)
			}
		})
	}
}

func TestParseGitHubIDRejects(t *testing.T) {
	tests := []struct {
		name string
		id   string
	}{
		{name: "repo without owner and bad number", id: "repo#x"},
		{name: "missing repo", id: "#1"},
		{name: "zero", id: "owner/repo#0"},
		{name: "leading zero", id: "owner/repo#01"},
		{name: "no hash", id: "owner/repo"},
		{name: "empty", id: ""},
		{name: "extra slash", id: "owner/repo/name#1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, _, err := source.ParseGitHubID(tc.id)
			if err == nil {
				t.Fatal("error = nil, want an error")
			}
			msg := err.Error()
			if !strings.Contains(msg, "expected") || !strings.Contains(msg, fmt.Sprintf("%q", tc.id)) {
				t.Fatalf("error %q does not say what was expected and what was found", msg)
			}
		})
	}
}

func TestIsJiraKey(t *testing.T) {
	tests := []struct {
		in   string
		want bool
	}{
		{in: "AB-1", want: true},
		{in: "PRJ-123", want: true},
		{in: "A1-2", want: true},
		{in: "ABC_X-10", want: true},
		{in: "A-1", want: false},
		{in: "ab-1", want: false},
		{in: "AB-0", want: false},
		{in: "AB-01", want: false},
		{in: "AB-", want: false},
		{in: "AB-1X", want: false},
		{in: "", want: false},
	}
	for _, tc := range tests {
		name := tc.in
		if name == "" {
			name = "empty"
		}
		t.Run(name, func(t *testing.T) {
			if got := source.IsJiraKey(tc.in); got != tc.want {
				t.Fatalf("IsJiraKey(%q) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}
