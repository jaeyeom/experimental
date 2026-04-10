package models

import "testing"

func TestChecksStatus(t *testing.T) {
	tests := []struct {
		name string
		pr   PullRequest
		want string
	}{
		{
			name: "no checks configured",
			pr:   PullRequest{},
			want: ChecksNone,
		},
		{
			name: "all checks passing",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
					{Name: "test", Status: "COMPLETED", Conclusion: "SUCCESS"},
					{Name: "lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
				},
			},
			want: ChecksPass,
		},
		{
			name: "one check failed",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
					{Name: "test", Status: "COMPLETED", Conclusion: "FAILURE"},
				},
			},
			want: ChecksFail,
		},
		{
			name: "one check cancelled",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
					{Name: "deploy", Status: "COMPLETED", Conclusion: "CANCELLED"},
				},
			},
			want: ChecksFail,
		},
		{
			name: "action required treated as failure",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "review", Status: "COMPLETED", Conclusion: "ACTION_REQUIRED"},
				},
			},
			want: ChecksFail,
		},
		{
			name: "one check pending",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
					{Name: "test", Status: "IN_PROGRESS", Conclusion: ""},
				},
			},
			want: ChecksPending,
		},
		{
			name: "queued check treated as pending",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "build", Status: "QUEUED", Conclusion: ""},
				},
			},
			want: ChecksPending,
		},
		{
			name: "failure takes priority over pending",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "build", Status: "COMPLETED", Conclusion: "FAILURE"},
					{Name: "test", Status: "IN_PROGRESS", Conclusion: ""},
				},
			},
			want: ChecksFail,
		},
		{
			name: "neutral treated as passing",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "info", Status: "COMPLETED", Conclusion: "NEUTRAL"},
					{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				},
			},
			want: ChecksPass,
		},
		{
			name: "skipped treated as passing",
			pr: PullRequest{
				StatusCheckRollup: []StatusCheck{
					{Name: "optional", Status: "COMPLETED", Conclusion: "SKIPPED"},
					{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
				},
			},
			want: ChecksPass,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.pr.ChecksStatus()
			if got != tt.want {
				t.Errorf("ChecksStatus() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestFailedChecks(t *testing.T) {
	pr := PullRequest{
		StatusCheckRollup: []StatusCheck{
			{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Name: "test", Status: "COMPLETED", Conclusion: "FAILURE"},
			{Name: "lint", Status: "COMPLETED", Conclusion: "CANCELLED"},
			{Name: "deploy", Status: "COMPLETED", Conclusion: "ACTION_REQUIRED"},
		},
	}

	failed := pr.FailedChecks()
	if len(failed) != 3 {
		t.Fatalf("expected 3 failed checks, got %d", len(failed))
	}

	names := map[string]bool{}
	for _, c := range failed {
		names[c.Name] = true
	}
	for _, want := range []string{"test", "lint", "deploy"} {
		if !names[want] {
			t.Errorf("expected %q in failed checks", want)
		}
	}
}

func TestFailedChecks_Empty(t *testing.T) {
	pr := PullRequest{
		StatusCheckRollup: []StatusCheck{
			{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
		},
	}

	failed := pr.FailedChecks()
	if len(failed) != 0 {
		t.Errorf("expected 0 failed checks, got %d", len(failed))
	}
}

func TestPendingChecks(t *testing.T) {
	pr := PullRequest{
		StatusCheckRollup: []StatusCheck{
			{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
			{Name: "test", Status: "IN_PROGRESS", Conclusion: ""},
			{Name: "lint", Status: "QUEUED", Conclusion: ""},
		},
	}

	pending := pr.PendingChecks()
	if len(pending) != 2 {
		t.Fatalf("expected 2 pending checks, got %d", len(pending))
	}

	names := map[string]bool{}
	for _, c := range pending {
		names[c.Name] = true
	}
	for _, want := range []string{"test", "lint"} {
		if !names[want] {
			t.Errorf("expected %q in pending checks", want)
		}
	}
}

func TestPendingChecks_Empty(t *testing.T) {
	pr := PullRequest{
		StatusCheckRollup: []StatusCheck{
			{Name: "build", Status: "COMPLETED", Conclusion: "SUCCESS"},
		},
	}

	pending := pr.PendingChecks()
	if len(pending) != 0 {
		t.Errorf("expected 0 pending checks, got %d", len(pending))
	}
}
