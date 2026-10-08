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

// mixedReviewGateRollup is a CheckRun plus StatusContext rollup: green CI,
// a pending real CI status, and red review-gate entries that are not CI.
func mixedReviewGateRollup(build gh.StatusCheck) []gh.StatusCheck {
	return []gh.StatusCheck{
		build,
		{Name: "test-unit", Status: "COMPLETED", Conclusion: "SUCCESS"},
		{Context: "ci/circleci: test", State: "SUCCESS"},
		{Context: "code owners approved", State: "FAILURE"},
		{Context: "review threads answered", State: "PENDING"},
		{Name: "lint-advisory/style", Status: "COMPLETED", Conclusion: "FAILURE"},
		{Name: "ready for human review", Status: "COMPLETED", Conclusion: "ACTION_REQUIRED"},
	}
}

func TestCIStateFilterMixedRollup(t *testing.T) {
	t.Parallel()

	greenBuild := gh.StatusCheck{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"}
	pendingBuild := gh.StatusCheck{Name: "build", Status: "IN_PROGRESS"}
	redBuild := gh.StatusCheck{Name: "build", Status: "COMPLETED", Conclusion: "FAILURE"}
	ignore := []string{"*approved*", "*review*", "lint-advisory/*"}
	only := []string{"ci/*", "build", "test-*"}

	tests := []struct {
		name   string
		checks []gh.StatusCheck
		ignore []string
		only   []string
		want   string
	}{
		{
			name:   "default counts review-gate failures",
			checks: mixedReviewGateRollup(greenBuild),
			want:   "failing",
		},
		{
			name:   "ignored review gates leave green ci",
			checks: mixedReviewGateRollup(greenBuild),
			ignore: ignore,
			want:   "green",
		},
		{
			name:   "ignored review gates leave pending ci",
			checks: mixedReviewGateRollup(pendingBuild),
			ignore: ignore,
			want:   "pending",
		},
		{
			name: "ignored reds with pending status context",
			checks: []gh.StatusCheck{
				greenBuild,
				{Context: "ci/circleci: test", State: "PENDING"},
				{Context: "code owners approved", State: "FAILURE"},
				{Name: "ready for human review", Status: "COMPLETED", Conclusion: "FAILURE"},
			},
			ignore: ignore,
			want:   "pending",
		},
		{
			name:   "red build still failing when review gates are ignored",
			checks: mixedReviewGateRollup(redBuild),
			ignore: ignore,
			want:   "failing",
		},
		{
			name:   "allowlist drops review gates",
			checks: mixedReviewGateRollup(greenBuild),
			only:   only,
			want:   "green",
		},
		{
			name: "allowlist keeps a red ci status context",
			checks: []gh.StatusCheck{
				greenBuild,
				{Context: "ci/circleci: test", State: "FAILURE"},
				{Context: "code owners approved", State: "FAILURE"},
				{Name: "lint-advisory/style", Status: "COMPLETED", Conclusion: "FAILURE"},
			},
			only: only,
			want: "failing",
		},
		{
			name: "allowlist wins over ignore",
			checks: []gh.StatusCheck{
				redBuild,
				{Context: "code owners approved", State: "FAILURE"},
			},
			ignore: []string{"build", "*approved*"},
			only:   []string{"build"},
			want:   "failing",
		},
		{
			name: "allowlist excludes a red check outside the list",
			checks: []gh.StatusCheck{
				greenBuild,
				{Name: "lint", Status: "COMPLETED", Conclusion: "FAILURE"},
				{Context: "code owners approved", State: "FAILURE"},
			},
			only: []string{"build"},
			want: "green",
		},
		{
			name: "ignore matches context when name does not",
			checks: []gh.StatusCheck{
				{Name: "build", Context: "code owners approved", Status: "COMPLETED", Conclusion: "FAILURE"},
				{Name: "test", Status: "COMPLETED", Conclusion: "SUCCESS"},
			},
			ignore: ignore,
			want:   "green",
		},
		{
			name: "ignore matches name when context does not",
			checks: []gh.StatusCheck{
				{Name: "ready for human review", Context: "ci/circleci: test", Status: "COMPLETED", Conclusion: "FAILURE"},
				{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
			},
			ignore: ignore,
			want:   "green",
		},
		{
			name: "every check ignored is none",
			checks: []gh.StatusCheck{
				{Context: "code owners approved", State: "FAILURE"},
				{Name: "ready for human review", Status: "COMPLETED", Conclusion: "FAILURE"},
			},
			ignore: ignore,
			want:   "none",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := CIState(selectCIChecks(tc.checks, tc.ignore, tc.only))
			if got != tc.want {
				t.Fatalf("CIState() = %q, want %q", got, tc.want)
			}
		})
	}
}
