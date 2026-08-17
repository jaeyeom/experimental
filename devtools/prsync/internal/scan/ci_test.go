package scan

import (
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
