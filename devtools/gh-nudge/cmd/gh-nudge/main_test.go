package main

import (
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
