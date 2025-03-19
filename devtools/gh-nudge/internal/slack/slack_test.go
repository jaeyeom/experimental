package slack

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestFormatMessage(t *testing.T) {
	client := NewClient("test-token", map[string]string{
		"github-user": "slack-user-id",
	})

	pr := models.PullRequest{
		Title: "Test PR",
		URL:   "https://github.com/org/repo/pull/1",
		ReviewRequests: []models.ReviewRequest{
			{
				Type:  "User",
				Login: "github-user",
			},
		},
	}

	template := "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours."
	message := client.FormatMessage(template, pr, "github-user", 24)

	expected := "Hey <@slack-user-id>, the PR 'Test PR' has been waiting for your review for 24 hours."
	if message != expected {
		t.Errorf("Expected message '%s', got '%s'", expected, message)
	}
}

func TestGetSlackIDForGitHubUser(t *testing.T) {
	userMapping := map[string]string{
		"github-user1": "slack-user-id1",
		"github-user2": "slack-user-id2",
	}

	client := NewClient("test-token", userMapping)

	t.Run("should return correct Slack ID for GitHub user", func(t *testing.T) {
		slackID, ok := client.GetSlackIDForGitHubUser("github-user1")
		if !ok {
			t.Error("Expected to find Slack ID for github-user1")
		}
		if slackID != "slack-user-id1" {
			t.Errorf("Expected Slack ID 'slack-user-id1', got '%s'", slackID)
		}
	})

	t.Run("should handle unknown GitHub user", func(t *testing.T) {
		_, ok := client.GetSlackIDForGitHubUser("unknown-user")
		if ok {
			t.Error("Expected not to find Slack ID for unknown-user")
		}
	})
}

func TestGetChannelForPR(t *testing.T) {
	channelRouting := []ChannelRouting{
		{Pattern: "frontend/.*\\.js$", Channel: "#frontend"},
		{Pattern: "backend/.*\\.go$", Channel: "#backend"},
	}

	client := NewClient("test-token", nil)
	client.SetChannelRouting(channelRouting)
	client.SetDefaultChannel("#default")

	t.Run("should match frontend pattern", func(t *testing.T) {
		pr := models.PullRequest{
			Files: []models.File{
				{Path: "frontend/main.js"},
			},
		}

		channel := client.GetChannelForPR(pr)
		if channel != "#frontend" {
			t.Errorf("Expected channel '#frontend', got '%s'", channel)
		}
	})

	t.Run("should match backend pattern", func(t *testing.T) {
		pr := models.PullRequest{
			Files: []models.File{
				{Path: "backend/server.go"},
			},
		}

		channel := client.GetChannelForPR(pr)
		if channel != "#backend" {
			t.Errorf("Expected channel '#backend', got '%s'", channel)
		}
	})

	t.Run("should use default channel when no pattern matches", func(t *testing.T) {
		pr := models.PullRequest{
			Files: []models.File{
				{Path: "docs/README.md"},
			},
		}

		channel := client.GetChannelForPR(pr)
		if channel != "#default" {
			t.Errorf("Expected channel '#default', got '%s'", channel)
		}
	})
}
