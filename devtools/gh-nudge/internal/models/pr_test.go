package models

import (
	"testing"
	"time"
)

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

func prWithLabels(names ...string) PullRequest {
	pr := PullRequest{}
	for _, name := range names {
		pr.Labels = append(pr.Labels, Label{Name: name})
	}
	return pr
}

func TestPullRequestAllowsNudge(t *testing.T) {
	tests := []struct {
		name            string
		pr              PullRequest
		requireLabels   []string
		skipLabels      []string
		wantAllowsNudge bool
	}{
		{
			name:            "empty filters allow unlabeled PR",
			pr:              prWithLabels(),
			wantAllowsNudge: true,
		},
		{
			name:            "empty filters allow labeled PR",
			pr:              prWithLabels("ready-for-review", "backend"),
			wantAllowsNudge: true,
		},
		{
			name:            "require labels skip when any required label is missing",
			pr:              prWithLabels("backend"),
			requireLabels:   []string{"ready-for-review"},
			wantAllowsNudge: false,
		},
		{
			name:            "require labels skip when unlabeled",
			pr:              prWithLabels(),
			requireLabels:   []string{"ready-for-review"},
			wantAllowsNudge: false,
		},
		{
			name:            "require labels allow when all required labels are present",
			pr:              prWithLabels("ready-for-review", "backend"),
			requireLabels:   []string{"ready-for-review"},
			wantAllowsNudge: true,
		},
		{
			name:            "require labels skip unless every required label is present",
			pr:              prWithLabels("ready-for-review"),
			requireLabels:   []string{"ready-for-review", "needs-review"},
			wantAllowsNudge: false,
		},
		{
			name:            "require labels allow when every required label is present",
			pr:              prWithLabels("ready-for-review", "needs-review"),
			requireLabels:   []string{"ready-for-review", "needs-review"},
			wantAllowsNudge: true,
		},
		{
			name:            "skip labels skip when any skip label is present",
			pr:              prWithLabels("wip"),
			skipLabels:      []string{"wip", "do-not-nudge"},
			wantAllowsNudge: false,
		},
		{
			name:            "skip labels allow when none of the skip labels are present",
			pr:              prWithLabels("ready-for-review"),
			skipLabels:      []string{"wip", "do-not-nudge"},
			wantAllowsNudge: true,
		},
		{
			name:            "skip labels allow unlabeled PR",
			pr:              prWithLabels(),
			skipLabels:      []string{"wip"},
			wantAllowsNudge: true,
		},
		{
			name:            "label names match exactly including case",
			pr:              prWithLabels("WIP"),
			skipLabels:      []string{"wip"},
			wantAllowsNudge: true,
		},
		{
			name:            "both filters skip when a skip label is present even if required labels match",
			pr:              prWithLabels("ready-for-review", "wip"),
			requireLabels:   []string{"ready-for-review"},
			skipLabels:      []string{"wip"},
			wantAllowsNudge: false,
		},
		{
			name:            "both filters skip when a required label is missing",
			pr:              prWithLabels("backend"),
			requireLabels:   []string{"ready-for-review"},
			skipLabels:      []string{"wip"},
			wantAllowsNudge: false,
		},
		{
			name:            "both filters allow when required labels are present and skip labels are not",
			pr:              prWithLabels("ready-for-review", "backend"),
			requireLabels:   []string{"ready-for-review"},
			skipLabels:      []string{"wip"},
			wantAllowsNudge: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.pr.AllowsNudge(tc.requireLabels, tc.skipLabels)
			if got != tc.wantAllowsNudge {
				t.Errorf("AllowsNudge() = %v, want %v", got, tc.wantAllowsNudge)
			}
		})
	}
}

func TestPullRequestMeetsLabelMinAges(t *testing.T) {
	now := time.Date(2026, 10, 7, 12, 0, 0, 0, time.UTC)
	day := 24 * time.Hour
	tests := []struct {
		name    string
		pr      PullRequest
		rules   []LabelMinAge
		addedAt map[string]time.Time
		want    bool
	}{
		{
			name: "empty rules allow a PR with no label ages",
			pr:   prWithLabels(),
			want: true,
		},
		{
			name:  "missing label does not meet the age",
			pr:    prWithLabels("backend"),
			rules: []LabelMinAge{{Label: "X", MinHours: 24}},
			addedAt: map[string]time.Time{
				"X": now.Add(-2 * day),
			},
			want: false,
		},
		{
			name:  "present label with no add time does not meet the age",
			pr:    prWithLabels("X"),
			rules: []LabelMinAge{{Label: "X", MinHours: 24}},
			want:  false,
		},
		{
			name:  "zero add time does not meet the age",
			pr:    prWithLabels("X"),
			rules: []LabelMinAge{{Label: "X", MinHours: 24}},
			addedAt: map[string]time.Time{
				"X": {},
			},
			want: false,
		},
		{
			name:  "label younger than the minimum does not meet the age",
			pr:    prWithLabels("X"),
			rules: []LabelMinAge{{Label: "X", MinHours: 24}},
			addedAt: map[string]time.Time{
				"X": now.Add(-24*time.Hour + time.Second),
			},
			want: false,
		},
		{
			name:  "label aged exactly the minimum meets the age",
			pr:    prWithLabels("X"),
			rules: []LabelMinAge{{Label: "X", MinHours: 24}},
			addedAt: map[string]time.Time{
				"X": now.Add(-day),
			},
			want: true,
		},
		{
			name:  "label older than the minimum meets the age",
			pr:    prWithLabels("X", "backend"),
			rules: []LabelMinAge{{Label: "X", MinHours: 24}},
			addedAt: map[string]time.Time{
				"X": now.Add(-2 * day),
			},
			want: true,
		},
		{
			name:  "label name match is case sensitive",
			pr:    prWithLabels("X"),
			rules: []LabelMinAge{{Label: "x", MinHours: 24}},
			addedAt: map[string]time.Time{
				"x": now.Add(-2 * day),
			},
			want: false,
		},
		{
			name: "every rule must be met",
			pr:   prWithLabels("X", "Y"),
			rules: []LabelMinAge{
				{Label: "X", MinHours: 24},
				{Label: "Y", MinHours: 48},
			},
			addedAt: map[string]time.Time{
				"X": now.Add(-2 * day),
				"Y": now.Add(-day),
			},
			want: false,
		},
		{
			name: "all rules met when each label is old enough",
			pr:   prWithLabels("X", "Y"),
			rules: []LabelMinAge{
				{Label: "X", MinHours: 24},
				{Label: "Y", MinHours: 48},
			},
			addedAt: map[string]time.Time{
				"X": now.Add(-2 * day),
				"Y": now.Add(-3 * day),
			},
			want: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.pr.MeetsLabelMinAges(tc.rules, tc.addedAt, now)
			if got != tc.want {
				t.Errorf("MeetsLabelMinAges() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestMeetsFirstReviewRequestAge(t *testing.T) {
	now := time.Date(2026, 10, 7, 12, 0, 0, 0, time.UTC)
	day := 24 * time.Hour
	tests := []struct {
		name        string
		requestedAt time.Time
		minHours    int
		want        bool
	}{
		{
			name:     "non-positive minimum has no requirement",
			minHours: 0,
			want:     true,
		},
		{
			name:     "negative minimum has no requirement",
			minHours: -1,
			want:     true,
		},
		{
			name:     "missing request time does not meet a positive minimum",
			minHours: 24,
			want:     false,
		},
		{
			name:        "request younger than the minimum does not meet it",
			requestedAt: now.Add(-24*time.Hour + time.Second),
			minHours:    24,
			want:        false,
		},
		{
			name:        "request aged exactly the minimum meets it",
			requestedAt: now.Add(-day),
			minHours:    24,
			want:        true,
		},
		{
			name:        "older first request meets the minimum",
			requestedAt: now.Add(-2 * day),
			minHours:    24,
			want:        true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := MeetsFirstReviewRequestAge(tc.requestedAt, tc.minHours, now)
			if got != tc.want {
				t.Errorf("MeetsFirstReviewRequestAge() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestLatestReviewSubmittedAt(t *testing.T) {
	aliceEarlier := time.Date(2026, 3, 1, 10, 0, 0, 0, time.UTC)
	aliceLater := time.Date(2026, 3, 1, 15, 0, 0, 0, time.UTC)
	bobTime := time.Date(2026, 3, 1, 16, 0, 0, 0, time.UTC)

	tests := []struct {
		name  string
		pr    PullRequest
		login string
		want  time.Time
	}{
		{
			name:  "no reviews",
			pr:    PullRequest{},
			login: "alice",
		},
		{
			name: "review by requested reviewer",
			pr: PullRequest{
				LatestReviews: []Review{
					{
						Author:      ReviewAuthor{Login: "alice"},
						SubmittedAt: aliceEarlier,
						State:       "COMMENTED",
					},
				},
			},
			login: "alice",
			want:  aliceEarlier,
		},
		{
			name: "ignores reviews by other reviewers",
			pr: PullRequest{
				LatestReviews: []Review{
					{
						Author:      ReviewAuthor{Login: "bob"},
						SubmittedAt: bobTime,
						State:       "APPROVED",
					},
				},
			},
			login: "alice",
		},
		{
			name: "picks the latest review for the reviewer",
			pr: PullRequest{
				LatestReviews: []Review{
					{
						Author:      ReviewAuthor{Login: "alice"},
						SubmittedAt: aliceEarlier,
						State:       "COMMENTED",
					},
					{
						Author:      ReviewAuthor{Login: "bob"},
						SubmittedAt: bobTime,
						State:       "APPROVED",
					},
					{
						Author:      ReviewAuthor{Login: "alice"},
						SubmittedAt: aliceLater,
						State:       "CHANGES_REQUESTED",
					},
				},
			},
			login: "alice",
			want:  aliceLater,
		},
		{
			name: "ignores reviews with empty author login",
			pr: PullRequest{
				LatestReviews: []Review{
					{
						Author:      ReviewAuthor{},
						SubmittedAt: aliceLater,
						State:       "COMMENTED",
					},
				},
			},
			login: "alice",
		},
		{
			name: "ignores zero submitted time",
			pr: PullRequest{
				LatestReviews: []Review{
					{
						Author: ReviewAuthor{Login: "alice"},
						State:  "COMMENTED",
					},
				},
			},
			login: "alice",
		},
		{
			name: "login match is case-sensitive",
			pr: PullRequest{
				LatestReviews: []Review{
					{
						Author:      ReviewAuthor{Login: "Alice"},
						SubmittedAt: aliceLater,
						State:       "APPROVED",
					},
				},
			},
			login: "alice",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.pr.LatestReviewSubmittedAt(tc.login)
			if !got.Equal(tc.want) {
				t.Errorf("LatestReviewSubmittedAt(%q) = %v, want %v", tc.login, got, tc.want)
			}
		})
	}
}
