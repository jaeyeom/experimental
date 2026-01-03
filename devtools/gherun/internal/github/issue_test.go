package github

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/gherkin"
)

func TestBuildSuiteIssueBody(t *testing.T) {
	features := []*gherkin.Feature{
		{ID: "login", Name: "User Login"},
		{ID: "checkout", Name: "Shopping Checkout"},
	}

	body := BuildSuiteIssueBody(features, "Test Suite")

	// Check that body contains expected elements
	expectedParts := []string{
		"## Test Suite: Test Suite",
		"- **login** - User Login",
		"- **checkout** - Shopping Checkout",
		"- [ ] PASSED",
		"- [ ] FAILED",
	}

	for _, expected := range expectedParts {
		if !strings.Contains(body, expected) {
			t.Errorf("BuildSuiteIssueBody() body missing %q", expected)
		}
	}
}

func TestParseProgress(t *testing.T) {
	tests := []struct {
		name          string
		body          string
		wantTotal     int
		wantCompleted int
		wantPassed    int
		wantFailed    int
	}{
		{
			name: "all pending",
			body: `## Test Suite: Test

- **login** - User Login
    - [ ] PASSED
    - [ ] FAILED
- **checkout** - Checkout
    - [ ] PASSED
    - [ ] FAILED
`,
			wantTotal:     2,
			wantCompleted: 0,
			wantPassed:    0,
			wantFailed:    0,
		},
		{
			name: "one passed one failed",
			body: `## Test Suite: Test

- **login** - User Login
    - [x] PASSED
    - [ ] FAILED
- **checkout** - Checkout
    - [ ] PASSED
    - [x] FAILED
`,
			wantTotal:     2,
			wantCompleted: 2,
			wantPassed:    1,
			wantFailed:    1,
		},
		{
			name: "all passed",
			body: `## Test Suite: Test

- **login** - User Login
    - [x] PASSED
    - [ ] FAILED
- **checkout** - Checkout
    - [X] PASSED
    - [ ] FAILED
`,
			wantTotal:     2,
			wantCompleted: 2,
			wantPassed:    2,
			wantFailed:    0,
		},
		{
			name: "partial completion",
			body: `## Test Suite: Test

- **login** - User Login
    - [x] PASSED
    - [ ] FAILED
- **checkout** - Checkout
    - [ ] PASSED
    - [ ] FAILED
- **profile** - Profile
    - [ ] PASSED
    - [x] FAILED
`,
			wantTotal:     3,
			wantCompleted: 2,
			wantPassed:    1,
			wantFailed:    1,
		},
		{
			name: "both checkboxes checked counts as failed",
			body: `## Test Suite: Test

- **login** - User Login
    - [x] PASSED
    - [x] FAILED
`,
			wantTotal:     1,
			wantCompleted: 1,
			wantPassed:    0,
			wantFailed:    1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			progress := ParseProgress(tt.body)

			if progress.Total != tt.wantTotal {
				t.Errorf("Total = %d, want %d", progress.Total, tt.wantTotal)
			}
			if progress.Completed != tt.wantCompleted {
				t.Errorf("Completed = %d, want %d", progress.Completed, tt.wantCompleted)
			}
			if progress.Passed != tt.wantPassed {
				t.Errorf("Passed = %d, want %d", progress.Passed, tt.wantPassed)
			}
			if progress.Failed != tt.wantFailed {
				t.Errorf("Failed = %d, want %d", progress.Failed, tt.wantFailed)
			}
		})
	}
}

func TestExtractIssueNumber(t *testing.T) {
	tests := []struct {
		name    string
		url     string
		want    int
		wantErr bool
	}{
		{
			name: "valid URL",
			url:  "https://github.com/owner/repo/issues/123",
			want: 123,
		},
		{
			name: "URL with trailing newline",
			url:  "https://github.com/owner/repo/issues/456\n",
			want: 456,
		},
		{
			name:    "not a URL",
			url:     "not-a-url",
			wantErr: true,
		},
		{
			name:    "non-GitHub URL",
			url:     "https://gitlab.com/owner/repo/issues/123",
			wantErr: true,
		},
		{
			name:    "GitHub URL without issues path",
			url:     "https://github.com/owner/repo/pulls/123",
			wantErr: true,
		},
		{
			name:    "GitHub URL with non-numeric issue",
			url:     "https://github.com/owner/repo/issues/abc",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := extractIssueNumber(tt.url)
			if (err != nil) != tt.wantErr {
				t.Errorf("extractIssueNumber() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && got != tt.want {
				t.Errorf("extractIssueNumber() = %d, want %d", got, tt.want)
			}
		})
	}
}
