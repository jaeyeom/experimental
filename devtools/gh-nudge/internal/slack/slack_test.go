package slack

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestFormatMessage(t *testing.T) {
	userIDMapping := UserIDMapping{
		"github-user": "U12345",
	}
	dmChannelIDMapping := DMChannelIDMapping{
		"github-user": "C12345",
	}

	client := NewClient(ClientConfig{
		Token:              "test-token",
		UserIDMapping:      userIDMapping,
		DMChannelIDMapping: dmChannelIDMapping,
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

	t.Run("should replace all placeholders in template", func(t *testing.T) {
		template := "Hey <@{slack_id}>, the PR '{title}' by {githubUsername} has been waiting for your review for {hours} hours. {url}"
		message := client.FormatMessage(template, pr, GitHubUsername("github-user"), 24)

		expected := "Hey <@U12345>, the PR 'Test PR' by github-user has been waiting for your review for 24 hours. https://github.com/org/repo/pull/1"
		if message != expected {
			t.Errorf("Expected message %q, got %q", expected, message)
		}
	})

	t.Run("should handle template without githubUsername placeholder", func(t *testing.T) {
		template := "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours. {url}"
		message := client.FormatMessage(template, pr, GitHubUsername("github-user"), 24)

		expected := "Hey <@U12345>, the PR 'Test PR' has been waiting for your review for 24 hours. https://github.com/org/repo/pull/1"
		if message != expected {
			t.Errorf("Expected message %q, got %q", expected, message)
		}
	})
}

func TestGetSlackUserIDForGitHubUser(t *testing.T) {
	userIDMapping := UserIDMapping{
		"github-user1": "U12345",
		"github-user2": "U67890",
	}
	dmChannelIDMapping := DMChannelIDMapping{
		"github-user1": "C12345",
		"github-user2": "C67890",
	}

	client := NewClient(ClientConfig{
		Token:              "test-token",
		UserIDMapping:      userIDMapping,
		DMChannelIDMapping: dmChannelIDMapping,
	})

	t.Run("should return correct Slack user ID for GitHub user", func(t *testing.T) {
		slackID, ok := client.GetSlackUserIDForGitHubUser(GitHubUsername("github-user1"))
		if !ok {
			t.Error("Expected to find Slack user ID for github-user1")
		}
		if string(slackID) != "U12345" {
			t.Errorf("Expected Slack user ID 'U12345', got %q", slackID)
		}
	})

	t.Run("should handle unknown GitHub user", func(t *testing.T) {
		_, ok := client.GetSlackUserIDForGitHubUser(GitHubUsername("unknown-user"))
		if ok {
			t.Error("Expected not to find Slack user ID for unknown-user")
		}
	})
}

func TestGetDMChannelIDForGitHubUser(t *testing.T) {
	userIDMapping := UserIDMapping{
		"github-user1": "U12345",
		"github-user2": "U67890",
	}
	dmChannelIDMapping := DMChannelIDMapping{
		"github-user1": "C12345",
		"github-user2": "C67890",
	}

	client := NewClient(ClientConfig{
		Token:              "test-token",
		UserIDMapping:      userIDMapping,
		DMChannelIDMapping: dmChannelIDMapping,
	})

	t.Run("should return correct DM channel ID for GitHub user", func(t *testing.T) {
		channelID, ok := client.GetDMChannelIDForGitHubUser(GitHubUsername("github-user1"))
		if !ok {
			t.Error("Expected to find DM channel ID for github-user1")
		}
		if string(channelID) != "C12345" {
			t.Errorf("Expected DM channel ID 'C12345', got %q", channelID)
		}
	})

	t.Run("should handle unknown GitHub user", func(t *testing.T) {
		_, ok := client.GetDMChannelIDForGitHubUser(GitHubUsername("unknown-user"))
		if ok {
			t.Error("Expected not to find DM channel ID for unknown-user")
		}
	})
}

func TestGetChannelForPR(t *testing.T) {
	channelRouting := []ChannelRouting{
		{Pattern: "frontend/.*\\.js$", Channel: "#frontend"},
		{Pattern: "backend/.*\\.go$", Channel: "#backend"},
	}

	client := NewClient(ClientConfig{
		Token:              "test-token",
		UserIDMapping:      nil,
		DMChannelIDMapping: nil,
	})
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
			t.Errorf("Expected channel '#frontend', got %q", channel)
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
			t.Errorf("Expected channel '#backend', got %q", channel)
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
			t.Errorf("Expected channel '#default', got %q", channel)
		}
	})
}

func TestSendDirectMessage(t *testing.T) {
	userIDMapping := UserIDMapping{
		"github-user": "U12345",
	}
	dmChannelIDMapping := DMChannelIDMapping{
		"github-user": "C12345",
	}

	client := NewClient(ClientConfig{
		Token:              "test-token",
		UserIDMapping:      userIDMapping,
		DMChannelIDMapping: dmChannelIDMapping,
	})

	// This is just a basic test structure since we can't actually send messages in tests
	t.Run("should prefer DM channel ID when available", func(t *testing.T) {
		// In a real test, we would mock the Slack API and verify the correct channel ID is used
		// For now, we're just ensuring the method doesn't panic
		err := client.SendDirectMessage(GitHubUsername("github-user"), "Test message")
		// We expect an error in tests since we're not actually connecting to Slack
		// Just checking that the function handles the logic correctly
		if err == nil {
			t.Error("Expected an error when trying to send a message without a real Slack connection")
		}
	})
}

func TestNudgeReviewer(t *testing.T) {
	userIDMapping := UserIDMapping{
		"github-user": "U12345",
	}
	dmChannelIDMapping := DMChannelIDMapping{
		"github-user": "C12345",
	}

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

	template := "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours. {url}"

	t.Run("should return destination and message in dry run mode with DM", func(t *testing.T) {
		client := NewClient(ClientConfig{
			Token:              "test-token",
			UserIDMapping:      userIDMapping,
			DMChannelIDMapping: dmChannelIDMapping,
			MessagePoster:      NewDryRunMessagePoster(),
		})
		client.SetDefaultChannel("#default")

		destination, message, err := client.NudgeReviewer(pr, GitHubUsername("github-user"), 24, template, true)
		if err != nil {
			t.Errorf("Expected no error in dry run mode, got: %v", err)
		}

		// Should use DM channel ID when available
		if destination != "C12345" {
			t.Errorf("Expected destination 'C12345', got %q", destination)
		}

		expectedMessage := "Hey <@U12345>, the PR 'Test PR' has been waiting for your review for 24 hours. https://github.com/org/repo/pull/1"
		if message != expectedMessage {
			t.Errorf("Expected message %q, got %q", expectedMessage, message)
		}
	})

	t.Run("should return destination and message in dry run mode with channel", func(t *testing.T) {
		client := NewClient(ClientConfig{
			Token:              "test-token",
			UserIDMapping:      userIDMapping,
			DMChannelIDMapping: dmChannelIDMapping,
			MessagePoster:      NewDryRunMessagePoster(),
		})
		client.SetDefaultChannel("#default")

		destination, message, err := client.NudgeReviewer(pr, GitHubUsername("github-user"), 24, template, false)
		if err != nil {
			t.Errorf("Expected no error in dry run mode, got: %v", err)
		}

		// Should use default channel when DMByDefault is false
		if destination != "#default" {
			t.Errorf("Expected destination '#default', got %q", destination)
		}

		expectedMessage := "Hey <@U12345>, the PR 'Test PR' has been waiting for your review for 24 hours. https://github.com/org/repo/pull/1"
		if message != expectedMessage {
			t.Errorf("Expected message %q, got %q", expectedMessage, message)
		}
	})

	t.Run("should handle unknown GitHub user", func(t *testing.T) {
		client := NewClient(ClientConfig{
			Token:              "test-token",
			UserIDMapping:      userIDMapping,
			DMChannelIDMapping: dmChannelIDMapping,
			MessagePoster:      NewDryRunMessagePoster(),
		})
		client.SetDefaultChannel("#default")

		_, _, err := client.NudgeReviewer(pr, GitHubUsername("unknown-user"), 24, template, true)

		if err == nil {
			t.Error("Expected an error for unknown GitHub user")
		}
	})

	t.Run("should attempt to send message with real client", func(t *testing.T) {
		// Test with nil MessagePoster (should default to real Slack client)
		client := NewClient(ClientConfig{
			Token:              "test-token",
			UserIDMapping:      userIDMapping,
			DMChannelIDMapping: dmChannelIDMapping,
			MessagePoster:      nil, // Will default to real Slack client
		})
		client.SetDefaultChannel("#default")

		_, _, err := client.NudgeReviewer(pr, GitHubUsername("github-user"), 24, template, true)

		// We expect an error in tests since we're not actually connecting to Slack
		if err == nil {
			t.Error("Expected an error when trying to send a message without a real Slack connection")
		}
	})
}
