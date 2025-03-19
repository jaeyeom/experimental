package config

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadConfig(t *testing.T) {
	t.Run("should load valid config file", func(t *testing.T) {
		// Create a temporary config file
		tempDir := t.TempDir()
		configPath := filepath.Join(tempDir, "config.yaml")

		configContent := `
github:
  owner: "test-org"
  repos:
    - "repo1"
    - "repo2"

slack:
  token: "xoxb-test-token"
  default_channel: "#code-reviews"
  user_id_mapping:
    "github-user1": "U12345"
    "github-user2": "U67890"
  dm_channel_id_mapping:
    "github-user1": "C12345"
    "github-user2": "C67890"
  channel_routing:
    - pattern: "frontend/.*\\.js$"
      channel: "#frontend"
    - pattern: "backend/.*\\.go$"
      channel: "#backend"

settings:
  reminder_threshold_hours: 24
  working_hours_only: true
  message_template: "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours."
  dm_by_default: true
`
		err := os.WriteFile(configPath, []byte(configContent), 0o644)
		if err != nil {
			t.Fatalf("Failed to write test config file: %v", err)
		}

		// Load the config
		cfg, err := LoadConfig(configPath)
		if err != nil {
			t.Fatalf("Failed to load config: %v", err)
		}

		// Verify the config values
		if cfg.GitHub.Owner != "test-org" {
			t.Errorf("Expected GitHub owner 'test-org', got '%s'", cfg.GitHub.Owner)
		}

		if len(cfg.GitHub.Repos) != 2 {
			t.Errorf("Expected 2 repos, got %d", len(cfg.GitHub.Repos))
		}

		if cfg.Slack.Token != "xoxb-test-token" {
			t.Errorf("Expected Slack token 'xoxb-test-token', got '%s'", cfg.Slack.Token)
		}

		if cfg.Slack.DefaultChannel != "#code-reviews" {
			t.Errorf("Expected default channel '#code-reviews', got '%s'", cfg.Slack.DefaultChannel)
		}

		if len(cfg.Slack.UserIDMapping) != 2 {
			t.Errorf("Expected 2 user ID mappings, got %d", len(cfg.Slack.UserIDMapping))
		}

		if len(cfg.Slack.DMChannelIDMapping) != 2 {
			t.Errorf("Expected 2 DM channel ID mappings, got %d", len(cfg.Slack.DMChannelIDMapping))
		}

		if len(cfg.Slack.ChannelRouting) != 2 {
			t.Errorf("Expected 2 channel routings, got %d", len(cfg.Slack.ChannelRouting))
		}

		if cfg.Settings.ReminderThresholdHours != 24 {
			t.Errorf("Expected reminder threshold 24, got %d", cfg.Settings.ReminderThresholdHours)
		}

		if !cfg.Settings.WorkingHoursOnly {
			t.Error("Expected working hours only to be true")
		}

		if !cfg.Settings.DMByDefault {
			t.Error("Expected DM by default to be true")
		}
	})

	t.Run("should handle missing config file", func(t *testing.T) {
		_, err := LoadConfig("/nonexistent/path/to/config.yaml")
		if err == nil {
			t.Error("Expected error for nonexistent config file, got nil")
		}
	})

	t.Run("should handle invalid YAML", func(t *testing.T) {
		// Create a temporary config file with invalid YAML
		tempDir := t.TempDir()
		configPath := filepath.Join(tempDir, "config.yaml")

		configContent := `
github:
  owner: "test-org"
  repos:
    - "repo1"
    - "repo2"
slack: invalid-yaml-here
`
		err := os.WriteFile(configPath, []byte(configContent), 0o644)
		if err != nil {
			t.Fatalf("Failed to write test config file: %v", err)
		}

		// Load the config
		_, err = LoadConfig(configPath)
		if err == nil {
			t.Error("Expected error for invalid YAML, got nil")
		}
	})
}
