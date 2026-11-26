package source_test

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

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

func TestContentHash(t *testing.T) {
	h1 := source.ContentHash("ab", "c")
	h2 := source.ContentHash("a", "bc")
	if h1 == h2 {
		t.Fatalf("ContentHash boundary collision: %s == %s", h1, h2)
	}
	if !strings.HasPrefix(h1, "sha256:") || len(h1) != len("sha256:")+64 {
		t.Fatalf("ContentHash format = %q", h1)
	}
	if got := source.ContentHash("ab", "c"); got != h1 {
		t.Fatalf("ContentHash not deterministic: %q != %q", got, h1)
	}
	if source.ContentHash("title", "body1") == source.ContentHash("title", "body2") {
		t.Fatal("ContentHash did not change when body changed")
	}
	if source.ContentHash("title1", "body") == source.ContentHash("title2", "body") {
		t.Fatal("ContentHash did not change when title changed")
	}
}

type stubSource struct{}

func (stubSource) Name() string                                           { return "stub" }
func (stubSource) Me(context.Context) (string, error)                     { return "me", nil }
func (stubSource) Sync(context.Context, time.Time) ([]source.Item, error) { return nil, nil }
func (stubSource) IDs(context.Context) ([]string, error)                  { return nil, nil }
func (stubSource) Get(_ context.Context, id string) (source.Item, error) {
	return source.Item{ID: id, Title: "T"}, nil
}
func (stubSource) Claim(context.Context, string) error { return nil }
func (stubSource) ClaimWithOptions(context.Context, string, source.ClaimOptions) error {
	return nil
}
func (stubSource) PostClaimComment(context.Context, string, string) error { return nil }
func (stubSource) LatestClaimComment(context.Context, string) (*source.ClaimComment, error) {
	return nil, nil
}
func (stubSource) Release(context.Context, string) error        { return nil }
func (stubSource) Finish(context.Context, string) error         { return nil }
func (stubSource) Close(context.Context, string, string) error  { return nil }
func (stubSource) Link(context.Context, string, string) error   { return nil }
func (stubSource) Unlink(context.Context, string, string) error { return nil }
func (stubSource) Create(context.Context, source.CreateRequest) (string, error) {
	return "", nil
}
func (stubSource) SetPriority(context.Context, string, int) error { return nil }

type fullStubSource struct {
	stubSource
	status string
}

func (fullStubSource) GetContent(_ context.Context, id string) (source.Item, string, error) {
	return source.Item{ID: id, Title: "T"}, "Body", nil
}

func (f *fullStubSource) SetStatus(_ context.Context, _ string, status string) error {
	f.status = status
	return nil
}

func TestReadContentAndSetStatusHelpers(t *testing.T) {
	ctx := context.Background()
	it, body, err := source.ReadContent(ctx, stubSource{}, "X-1")
	if err != nil || it.ID != "X-1" || body != "" {
		t.Fatalf("ReadContent fallback = (%+v, %q, %v)", it, body, err)
	}
	if err := source.SetStatus(ctx, stubSource{}, "X-1", "Todo"); !errors.Is(err, source.ErrNotSupported) {
		t.Fatalf("SetStatus unsupported = %v, want ErrNotSupported", err)
	}

	full := &fullStubSource{}
	it, body, err = source.ReadContent(ctx, full, "X-2")
	if err != nil || it.ID != "X-2" || body != "Body" {
		t.Fatalf("ReadContent ContentReader = (%+v, %q, %v)", it, body, err)
	}
	if err := source.SetStatus(ctx, full, "X-2", "Todo"); err != nil || full.status != "Todo" {
		t.Fatalf("SetStatus = %v, status = %q", err, full.status)
	}
}
