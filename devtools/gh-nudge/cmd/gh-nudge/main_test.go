package main

import (
	"strings"
	"testing"

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
