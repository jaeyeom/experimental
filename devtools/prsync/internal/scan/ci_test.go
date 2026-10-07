package scan

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

func TestCIState(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		checks []gh.StatusCheck
		want   string
	}{
		{
			name:   "no checks configured",
			checks: nil,
			want:   "none",
		},
		{
			name:   "empty rollup",
			checks: []gh.StatusCheck{},
			want:   "none",
		},
		{
			name: "all checks passing",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Name: "test", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Name: "lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			},
			want: "green",
		},
		{
			name: "one check failed",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Name: "test", Status: "COMPLETED", Conclusion: "FAILURE"},
			},
			want: "failing",
		},
		{
			name: "one check cancelled",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Name: "deploy", Status: "COMPLETED", Conclusion: "CANCELLED"},
			},
			want: "failing",
		},
		{
			name: "action required treated as failure",
			checks: []gh.StatusCheck{
				{Name: "review", Status: "COMPLETED", Conclusion: "ACTION_REQUIRED"},
			},
			want: "failing",
		},
		{
			name: "timed out treated as failure",
			checks: []gh.StatusCheck{
				{Name: "e2e", Status: "COMPLETED", Conclusion: "TIMED_OUT"},
			},
			want: "failing",
		},
		{
			name: "one check pending",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Name: "test", Status: "IN_PROGRESS", Conclusion: ""},
			},
			want: "pending",
		},
		{
			name: "queued check treated as pending",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "QUEUED", Conclusion: ""},
			},
			want: "pending",
		},
		{
			name: "failure takes priority over pending",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "FAILURE"},
				{Name: "test", Status: "IN_PROGRESS", Conclusion: ""},
			},
			want: "failing",
		},
		{
			name: "neutral treated as passing",
			checks: []gh.StatusCheck{
				{Name: "info", Status: "COMPLETED", Conclusion: "NEUTRAL"},
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
			},
			want: "green",
		},
		{
			name: "skipped treated as passing",
			checks: []gh.StatusCheck{
				{Name: "optional", Status: "COMPLETED", Conclusion: "SKIPPED"},
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
			},
			want: "green",
		},
		{
			name: "status context failure",
			checks: []gh.StatusCheck{
				{Context: "ci/circleci: test", State: "FAILURE"},
			},
			want: "failing",
		},
		{
			name: "status context error",
			checks: []gh.StatusCheck{
				{Context: "ci/circleci: lint", State: "ERROR"},
			},
			want: "failing",
		},
		{
			name: "status context pending",
			checks: []gh.StatusCheck{
				{Context: "ci/circleci: test", State: "PENDING"},
			},
			want: "pending",
		},
		{
			name: "status context expected",
			checks: []gh.StatusCheck{
				{Context: "ci/circleci: test", State: "EXPECTED"},
			},
			want: "pending",
		},
		{
			name: "status context success is green",
			checks: []gh.StatusCheck{
				{Context: "ci/circleci: test", State: "SUCCESS"},
			},
			want: "green",
		},
		{
			name: "status context failure beats green check run",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Context: "ci/circleci: test", State: "FAILURE"},
			},
			want: "failing",
		},
		{
			name: "status context error beats pending status context",
			checks: []gh.StatusCheck{
				{Context: "ci/circleci: test", State: "PENDING"},
				{Context: "ci/circleci: lint", State: "ERROR"},
			},
			want: "failing",
		},
		{
			name: "status context pending with green check run",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Context: "ci/circleci: test", State: "PENDING"},
			},
			want: "pending",
		},
		{
			name: "status context success does not mask in-progress check run",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "IN_PROGRESS"},
				{Context: "ci/circleci: test", State: "SUCCESS"},
			},
			want: "pending",
		},
		{
			name: "green check run and success status context",
			checks: []gh.StatusCheck{
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Context: "ci/circleci: test", State: "SUCCESS"},
			},
			want: "green",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := CIState(tc.checks)
			if got != tc.want {
				t.Fatalf("CIState() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestStatusCheckDisplayName(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		check gh.StatusCheck
		want  string
	}{
		{
			name:  "check run name",
			check: gh.StatusCheck{Name: "build"},
			want:  "build",
		},
		{
			name:  "commit status context when name is empty",
			check: gh.StatusCheck{Context: "ci/circleci: test"},
			want:  "ci/circleci: test",
		},
		{
			name:  "name wins when both are set",
			check: gh.StatusCheck{Name: "build", Context: "ci/circleci: test"},
			want:  "build",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := tc.check.DisplayName(); got != tc.want {
				t.Fatalf("DisplayName() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestMixedStatusCheckRollupFixture(t *testing.T) {
	t.Parallel()

	raw, err := os.ReadFile(filepath.Join("testdata", "fixtures", "mixed-status-check-rollup.json")) //nolint:gosec // testdata path
	if err != nil {
		t.Fatalf("read fixture: %v", err)
	}
	var checks []gh.StatusCheck
	if err := json.Unmarshal(raw, &checks); err != nil {
		t.Fatalf("unmarshal fixture: %v", err)
	}
	if len(checks) != 2 {
		t.Fatalf("len(checks) = %d, want 2", len(checks))
	}
	if got := CIState(checks); got != "failing" {
		t.Fatalf("CIState() = %q, want failing", got)
	}
	if got, want := checks[0].DisplayName(), "build"; got != want {
		t.Fatalf("check run display name = %q, want %q", got, want)
	}
	if checks[0].Conclusion != "SUCCESS" || checks[0].Status != "COMPLETED" {
		t.Fatalf("check run = %+v", checks[0])
	}
	if got, want := checks[1].DisplayName(), "ci/circleci: test"; got != want {
		t.Fatalf("status context display name = %q, want %q", got, want)
	}
	if checks[1].Name != "" || checks[1].State != "FAILURE" {
		t.Fatalf("status context = %+v", checks[1])
	}
}
