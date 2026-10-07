package main

import (
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/notification"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/slack"
	slackapi "github.com/slack-go/slack"
)

type recordingPoster struct {
	calls int
}

func (r *recordingPoster) PostMessage(channel string, _ ...slackapi.MsgOption) (string, string, error) {
	r.calls++
	return channel, "123.456", nil
}

func TestProcessReviewerSkipUsers(t *testing.T) {
	pr := models.PullRequest{
		Title: "Test PR",
		URL:   "https://github.com/org/repo/pull/1",
	}

	tests := []struct {
		name          string
		reviewer      string
		skipUsers     []string
		mapped        bool
		wantErrSubstr string
		wantPosted    bool
		wantRecorded  bool
	}{
		{
			name:         "skips unmapped user in skip_users without error",
			reviewer:     "bot-account",
			skipUsers:    []string{"bot-account"},
			mapped:       false,
			wantPosted:   false,
			wantRecorded: false,
		},
		{
			name:         "skips mapped user in skip_users without nudging",
			reviewer:     "opted-out",
			skipUsers:    []string{"opted-out"},
			mapped:       true,
			wantPosted:   false,
			wantRecorded: false,
		},
		{
			name:          "unmapped user not in skip_users still errors",
			reviewer:      "unknown-user",
			skipUsers:     []string{"bot-account"},
			mapped:        false,
			wantErrSubstr: "no Slack user ID mapping for GitHub user: unknown-user",
			wantPosted:    false,
			wantRecorded:  false,
		},
		{
			name:         "empty skip_users nudges mapped user",
			reviewer:     "github-user",
			skipUsers:    nil,
			mapped:       true,
			wantPosted:   true,
			wantRecorded: true,
		},
		{
			name:          "empty skip_users errors for unmapped user",
			reviewer:      "unknown-user",
			skipUsers:     nil,
			mapped:        false,
			wantErrSubstr: "no Slack user ID mapping for GitHub user: unknown-user",
			wantPosted:    false,
			wantRecorded:  false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			poster := &recordingPoster{}
			mapping := slack.UserIDMapping{}
			dmMapping := slack.DMChannelIDMapping{}
			if tc.mapped {
				mapping[slack.GitHubUsername(tc.reviewer)] = slack.UserID("U12345")
				dmMapping[slack.GitHubUsername(tc.reviewer)] = slack.ChannelID("C12345")
			}
			client := slack.NewClient(slack.ClientConfig{
				Token:              "test-token",
				UserIDMapping:      mapping,
				DMChannelIDMapping: dmMapping,
				MessagePoster:      poster,
			})
			client.SetDefaultChannel("#reviews")

			tracker := notification.NewTracker()
			cfg := &config.Config{
				Settings: config.SettingsConfig{
					ReminderThresholdHours: 24,
					MessageTemplate:        "review {title}",
					DMByDefault:            true,
					SkipUsers:              tc.skipUsers,
				},
			}
			reviewer := models.ReviewRequest{Type: "User", Login: tc.reviewer}

			err := processReviewer(pr, reviewer, client, tracker, cfg)

			switch {
			case tc.wantErrSubstr == "" && err != nil:
				t.Fatalf("processReviewer() unexpected error: %v", err)
			case tc.wantErrSubstr != "" && err == nil:
				t.Fatalf("processReviewer() error = nil, want substring %q", tc.wantErrSubstr)
			case err != nil && !strings.Contains(err.Error(), tc.wantErrSubstr):
				t.Errorf("processReviewer() error = %v, want substring %q", err, tc.wantErrSubstr)
			}

			if (poster.calls > 0) != tc.wantPosted {
				t.Errorf("posted = %d calls, wantPosted %v", poster.calls, tc.wantPosted)
			}

			recorded := !tracker.ShouldNotify(pr.URL, tc.reviewer, 24)
			if recorded != tc.wantRecorded {
				t.Errorf("recorded = %v, want %v", recorded, tc.wantRecorded)
			}
		})
	}
}

func TestProcessReviewerRequestCycle(t *testing.T) {
	reviewer := "alice"
	other := "bob"
	prURL := "https://github.com/org/repo/pull/1"
	now := time.Now()
	seedPing := now.Add(-3 * time.Hour)
	reviewAfterPing := now.Add(-1 * time.Hour)
	reviewBeforePing := now.Add(-4 * time.Hour)

	tests := []struct {
		name         string
		seedPing     bool
		reviews      []models.Review
		dryRun       bool
		skipUsers    []string
		wantPosted   bool
		wantRecorded bool
		wantNewCycle bool
	}{
		{
			name:         "first request with no history is pinged",
			wantPosted:   true,
			wantRecorded: true,
		},
		{
			name:         "waiting reviewer within threshold is not pinged",
			seedPing:     true,
			wantPosted:   false,
			wantRecorded: true,
		},
		{
			name:     "re-request after review within threshold is pinged",
			seedPing: true,
			reviews: []models.Review{
				{
					Author:      models.ReviewAuthor{Login: reviewer},
					SubmittedAt: reviewAfterPing,
					State:       "CHANGES_REQUESTED",
				},
			},
			wantPosted:   true,
			wantRecorded: true,
		},
		{
			name:     "review from before the last ping does not reset cooldown",
			seedPing: true,
			reviews: []models.Review{
				{
					Author:      models.ReviewAuthor{Login: reviewer},
					SubmittedAt: reviewBeforePing,
					State:       "COMMENTED",
				},
			},
			wantPosted:   false,
			wantRecorded: true,
		},
		{
			name:     "another reviewer's later review does not reset this reviewer's cooldown",
			seedPing: true,
			reviews: []models.Review{
				{
					Author:      models.ReviewAuthor{Login: other},
					SubmittedAt: reviewAfterPing,
					State:       "APPROVED",
				},
			},
			wantPosted:   false,
			wantRecorded: true,
		},
		{
			name:     "dry-run on a new request cycle does not record a ping",
			seedPing: true,
			dryRun:   true,
			reviews: []models.Review{
				{
					Author:      models.ReviewAuthor{Login: reviewer},
					SubmittedAt: reviewAfterPing,
					State:       "APPROVED",
				},
			},
			wantPosted:   false,
			wantRecorded: true,
			wantNewCycle: true,
		},
		{
			name:      "skip_users still skips a re-requested reviewer",
			seedPing:  true,
			skipUsers: []string{reviewer},
			reviews: []models.Review{
				{
					Author:      models.ReviewAuthor{Login: reviewer},
					SubmittedAt: reviewAfterPing,
					State:       "COMMENTED",
				},
			},
			wantPosted:   false,
			wantRecorded: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			poster := &recordingPoster{}
			var messagePoster slack.MessagePoster = poster
			if tc.dryRun {
				messagePoster = slack.NewDryRunMessagePoster()
			}
			client := slack.NewClient(slack.ClientConfig{
				Token: "test-token",
				UserIDMapping: slack.UserIDMapping{
					slack.GitHubUsername(reviewer): slack.UserID("U12345"),
				},
				DMChannelIDMapping: slack.DMChannelIDMapping{
					slack.GitHubUsername(reviewer): slack.ChannelID("C12345"),
				},
				MessagePoster: messagePoster,
			})
			client.SetDefaultChannel("#reviews")

			tracker := notification.NewTracker()
			if tc.seedPing {
				if err := tracker.RecordNotificationAt(prURL, reviewer, seedPing); err != nil {
					t.Fatalf("RecordNotificationAt() error = %v", err)
				}
			}

			cfg := &config.Config{
				Settings: config.SettingsConfig{
					ReminderThresholdHours: 24,
					MessageTemplate:        "review {title}",
					DMByDefault:            true,
					SkipUsers:              tc.skipUsers,
				},
			}
			pr := models.PullRequest{
				Title:         "Test PR",
				URL:           prURL,
				LatestReviews: tc.reviews,
			}
			request := models.ReviewRequest{Type: "User", Login: reviewer}

			err := processReviewer(pr, request, client, tracker, cfg)
			if err != nil {
				t.Fatalf("processReviewer() unexpected error: %v", err)
			}

			if (poster.calls > 0) != tc.wantPosted {
				t.Errorf("posted = %d calls, wantPosted %v", poster.calls, tc.wantPosted)
			}

			recorded := !tracker.ShouldNotify(pr.URL, reviewer, 24)
			if recorded != tc.wantRecorded {
				t.Errorf("recorded = %v, want %v", recorded, tc.wantRecorded)
			}

			if tc.wantNewCycle && !tracker.ShouldNotifyReviewer(pr.URL, reviewer, 24, reviewAfterPing) {
				t.Error("dry-run overwrote the previous ping timestamp")
			}
		})
	}
}

func TestProcessReviewerNewCyclePingThenRespectsThreshold(t *testing.T) {
	reviewer := "alice"
	prURL := "https://github.com/org/repo/pull/1"
	seedPing := time.Now().Add(-3 * time.Hour)
	reviewAt := time.Now().Add(-1 * time.Hour)

	poster := &recordingPoster{}
	client := slack.NewClient(slack.ClientConfig{
		Token: "test-token",
		UserIDMapping: slack.UserIDMapping{
			slack.GitHubUsername(reviewer): slack.UserID("U12345"),
		},
		DMChannelIDMapping: slack.DMChannelIDMapping{
			slack.GitHubUsername(reviewer): slack.ChannelID("C12345"),
		},
		MessagePoster: poster,
	})
	client.SetDefaultChannel("#reviews")

	tracker := notification.NewTracker()
	if err := tracker.RecordNotificationAt(prURL, reviewer, seedPing); err != nil {
		t.Fatalf("RecordNotificationAt() error = %v", err)
	}

	cfg := &config.Config{
		Settings: config.SettingsConfig{
			ReminderThresholdHours: 24,
			MessageTemplate:        "review {title}",
			DMByDefault:            true,
		},
	}
	pr := models.PullRequest{
		Title: "Test PR",
		URL:   prURL,
		LatestReviews: []models.Review{
			{
				Author:      models.ReviewAuthor{Login: reviewer},
				SubmittedAt: reviewAt,
				State:       "CHANGES_REQUESTED",
			},
		},
	}
	request := models.ReviewRequest{Type: "User", Login: reviewer}

	if err := processReviewer(pr, request, client, tracker, cfg); err != nil {
		t.Fatalf("first processReviewer() error = %v", err)
	}
	if poster.calls != 1 {
		t.Fatalf("first run posted = %d, want 1", poster.calls)
	}

	if err := processReviewer(pr, request, client, tracker, cfg); err != nil {
		t.Fatalf("second processReviewer() error = %v", err)
	}
	if poster.calls != 1 {
		t.Errorf("second run posted = %d, want 1 (cooldown after new-cycle ping)", poster.calls)
	}
}

type fakeLabelAges struct {
	added  map[string]time.Time
	err    error
	calls  int
	labels []string

	requested    map[string]time.Time
	requestErr   error
	requestCalls int
}

func (f *fakeLabelAges) LatestLabelAddedAt(_ string, labels []string) (map[string]time.Time, error) {
	f.calls++
	f.labels = append([]string(nil), labels...)
	if f.err != nil {
		return nil, f.err
	}
	return f.added, nil
}

func (f *fakeLabelAges) EarliestReviewRequestedAt(string) (map[string]time.Time, error) {
	f.requestCalls++
	if f.requestErr != nil {
		return nil, f.requestErr
	}
	return f.requested, nil
}

func TestProcessPullRequestRequireLabelAges(t *testing.T) {
	reviewer := "alice"
	prURL := "https://github.com/org/repo/pull/1"
	now := time.Now()

	tests := []struct {
		name          string
		labels        []string
		rules         []config.LabelAgeConfig
		requireLabels []string
		added         map[string]time.Time
		lookupErr     error
		wantCalls     int
		wantLabels    []string
		wantPosted    bool
		wantRecorded  bool
	}{
		{
			name:         "no age rules nudges without a lookup",
			wantPosted:   true,
			wantRecorded: true,
		},
		{
			name:   "missing required age label skips without a lookup",
			labels: []string{"backend"},
			rules:  []config.LabelAgeConfig{{Label: "X", MinHours: 24}},
		},
		{
			name:   "skips lookup when a later required label is missing",
			labels: []string{"X"},
			rules: []config.LabelAgeConfig{
				{Label: "X", MinHours: 24},
				{Label: "Y", MinHours: 24},
			},
		},
		{
			name:       "label younger than the minimum is not nudged",
			labels:     []string{"X"},
			rules:      []config.LabelAgeConfig{{Label: "X", MinHours: 24}},
			added:      map[string]time.Time{"X": now.Add(-23 * time.Hour)},
			wantCalls:  1,
			wantLabels: []string{"X"},
		},
		{
			name:         "label at least as old as the minimum is nudged",
			labels:       []string{"X"},
			rules:        []config.LabelAgeConfig{{Label: "X", MinHours: 24}},
			added:        map[string]time.Time{"X": now.Add(-24 * time.Hour)},
			wantCalls:    1,
			wantLabels:   []string{"X"},
			wantPosted:   true,
			wantRecorded: true,
		},
		{
			name:       "lookup error skips the pull request",
			labels:     []string{"X"},
			rules:      []config.LabelAgeConfig{{Label: "X", MinHours: 24}},
			lookupErr:  errors.New("timeline unavailable"),
			wantCalls:  1,
			wantLabels: []string{"X"},
		},
		{
			name:          "label filter skips before the age lookup",
			labels:        []string{"wip"},
			requireLabels: []string{"ready-for-review"},
			rules:         []config.LabelAgeConfig{{Label: "wip", MinHours: 1}},
		},
		{
			name:   "every age rule must pass",
			labels: []string{"X", "Y"},
			rules: []config.LabelAgeConfig{
				{Label: "X", MinHours: 24},
				{Label: "Y", MinHours: 48},
			},
			added: map[string]time.Time{
				"X": now.Add(-25 * time.Hour),
				"Y": now.Add(-30 * time.Hour),
			},
			wantCalls:  1,
			wantLabels: []string{"X", "Y"},
		},
		{
			name:   "all aged labels are nudged",
			labels: []string{"X", "Y"},
			rules: []config.LabelAgeConfig{
				{Label: "X", MinHours: 24},
				{Label: "Y", MinHours: 48},
			},
			added: map[string]time.Time{
				"X": now.Add(-25 * time.Hour),
				"Y": now.Add(-49 * time.Hour),
			},
			wantCalls:    1,
			wantLabels:   []string{"X", "Y"},
			wantPosted:   true,
			wantRecorded: true,
		},
		{
			name:   "stricter duplicate rule blocks a younger label",
			labels: []string{"X"},
			rules: []config.LabelAgeConfig{
				{Label: "X", MinHours: 24},
				{Label: "X", MinHours: 48},
			},
			added:      map[string]time.Time{"X": now.Add(-30 * time.Hour)},
			wantCalls:  1,
			wantLabels: []string{"X"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			poster := &recordingPoster{}
			client := slack.NewClient(slack.ClientConfig{
				Token: "test-token",
				UserIDMapping: slack.UserIDMapping{
					slack.GitHubUsername(reviewer): slack.UserID("U12345"),
				},
				DMChannelIDMapping: slack.DMChannelIDMapping{
					slack.GitHubUsername(reviewer): slack.ChannelID("C12345"),
				},
				MessagePoster: poster,
			})
			client.SetDefaultChannel("#reviews")

			tracker := notification.NewTracker()
			lookup := &fakeLabelAges{added: tc.added, err: tc.lookupErr}
			cfg := &config.Config{
				Settings: config.SettingsConfig{
					ReminderThresholdHours: 24,
					MessageTemplate:        "review {title}",
					DMByDefault:            true,
					RequireLabels:          tc.requireLabels,
					RequireLabelAges:       tc.rules,
				},
			}
			pr := models.PullRequest{
				Title: "Test PR",
				URL:   prURL,
				ReviewRequests: []models.ReviewRequest{
					{Type: "User", Login: reviewer},
				},
			}
			for _, name := range tc.labels {
				pr.Labels = append(pr.Labels, models.Label{Name: name})
			}

			processPullRequest(pr, lookup, client, tracker, cfg)

			if lookup.calls != tc.wantCalls {
				t.Errorf("lookup calls = %d, want %d", lookup.calls, tc.wantCalls)
			}
			if strings.Join(lookup.labels, ",") != strings.Join(tc.wantLabels, ",") {
				t.Errorf("lookup labels = %v, want %v", lookup.labels, tc.wantLabels)
			}
			if (poster.calls > 0) != tc.wantPosted {
				t.Errorf("posted = %d calls, wantPosted %v", poster.calls, tc.wantPosted)
			}
			recorded := !tracker.ShouldNotify(pr.URL, reviewer, 24)
			if recorded != tc.wantRecorded {
				t.Errorf("recorded = %v, want %v", recorded, tc.wantRecorded)
			}
		})
	}
}

func TestProcessPullRequestMinFirstReviewRequestHours(t *testing.T) {
	alice := "alice"
	bob := "bob"
	prURL := "https://github.com/org/repo/pull/1"
	now := time.Now()
	oldEnough := now.Add(-24 * time.Hour)
	tooYoung := now.Add(-23 * time.Hour)

	tests := []struct {
		name          string
		reviewers     []models.ReviewRequest
		minHours      int
		labels        []string
		requireLabels []string
		labelRules    []config.LabelAgeConfig
		skipUsers     []string
		requested     map[string]time.Time
		requestErr    error
		wantCalls     int
		wantPosted    int
		recorded      []string
		unrecorded    []string
	}{
		{
			name:       "no minimum nudges without a lookup",
			wantPosted: 1,
			recorded:   []string{alice},
		},
		{
			name:       "first request younger than the minimum is not nudged",
			minHours:   24,
			requested:  map[string]time.Time{alice: tooYoung},
			wantCalls:  1,
			unrecorded: []string{alice},
		},
		{
			name:       "first request at least as old as the minimum is nudged",
			minHours:   24,
			requested:  map[string]time.Time{alice: oldEnough},
			wantCalls:  1,
			wantPosted: 1,
			recorded:   []string{alice},
		},
		{
			name:       "missing first request skips that reviewer",
			minHours:   24,
			wantCalls:  1,
			unrecorded: []string{alice},
		},
		{
			name:       "lookup error skips the pull request",
			minHours:   24,
			requestErr: errors.New("timeline unavailable"),
			wantCalls:  1,
			unrecorded: []string{alice},
		},
		{
			name:          "label filter skips before the review-request lookup",
			minHours:      24,
			labels:        []string{"wip"},
			requireLabels: []string{"ready-for-review"},
			unrecorded:    []string{alice},
		},
		{
			name:       "unmet label age skips before the review-request lookup",
			minHours:   24,
			labelRules: []config.LabelAgeConfig{{Label: "X", MinHours: 24}},
			unrecorded: []string{alice},
		},
		{
			name: "only a reviewer whose first request is old enough is nudged",
			reviewers: []models.ReviewRequest{
				{Type: "User", Login: alice},
				{Type: "User", Login: bob},
				{Type: "Team", Name: "devs"},
			},
			minHours: 24,
			requested: map[string]time.Time{
				alice: oldEnough,
				bob:   tooYoung,
			},
			wantCalls:  1,
			wantPosted: 1,
			recorded:   []string{alice},
			unrecorded: []string{bob},
		},
		{
			name:       "skip_users is not nudged",
			minHours:   24,
			skipUsers:  []string{alice},
			requested:  map[string]time.Time{alice: oldEnough},
			wantCalls:  1,
			unrecorded: []string{alice},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			poster := &recordingPoster{}
			client := slack.NewClient(slack.ClientConfig{
				Token: "test-token",
				UserIDMapping: slack.UserIDMapping{
					slack.GitHubUsername(alice): slack.UserID("U12345"),
					slack.GitHubUsername(bob):   slack.UserID("U67890"),
				},
				DMChannelIDMapping: slack.DMChannelIDMapping{
					slack.GitHubUsername(alice): slack.ChannelID("C12345"),
					slack.GitHubUsername(bob):   slack.ChannelID("C67890"),
				},
				MessagePoster: poster,
			})
			client.SetDefaultChannel("#reviews")

			tracker := notification.NewTracker()
			lookup := &fakeLabelAges{requested: tc.requested, requestErr: tc.requestErr}
			cfg := &config.Config{
				Settings: config.SettingsConfig{
					ReminderThresholdHours:     24,
					MessageTemplate:            "review {title}",
					DMByDefault:                true,
					RequireLabels:              tc.requireLabels,
					RequireLabelAges:           tc.labelRules,
					SkipUsers:                  tc.skipUsers,
					MinFirstReviewRequestHours: tc.minHours,
				},
			}
			reviewers := tc.reviewers
			if reviewers == nil {
				reviewers = []models.ReviewRequest{{Type: "User", Login: alice}}
			}
			pr := models.PullRequest{
				Title:          "Test PR",
				URL:            prURL,
				ReviewRequests: reviewers,
			}
			for _, name := range tc.labels {
				pr.Labels = append(pr.Labels, models.Label{Name: name})
			}

			processPullRequest(pr, lookup, client, tracker, cfg)

			if lookup.requestCalls != tc.wantCalls {
				t.Errorf("review request lookup calls = %d, want %d", lookup.requestCalls, tc.wantCalls)
			}
			if poster.calls != tc.wantPosted {
				t.Errorf("posted = %d, want %d", poster.calls, tc.wantPosted)
			}
			for _, login := range tc.recorded {
				if tracker.ShouldNotify(pr.URL, login, 24) {
					t.Errorf("expected %s to be recorded", login)
				}
			}
			for _, login := range tc.unrecorded {
				if !tracker.ShouldNotify(pr.URL, login, 24) {
					t.Errorf("expected %s not to be recorded", login)
				}
			}
		})
	}
}
