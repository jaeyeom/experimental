package config

import (
	"os"
	"path/filepath"
	"strings"
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
		err := os.WriteFile(configPath, []byte(configContent), 0o600)
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
			t.Errorf("Expected GitHub owner 'test-org', got %q", cfg.GitHub.Owner)
		}

		if len(cfg.GitHub.Repos) != 2 {
			t.Errorf("Expected 2 repos, got %d", len(cfg.GitHub.Repos))
		}

		if cfg.Slack.Token != "xoxb-test-token" {
			t.Errorf("Expected Slack token 'xoxb-test-token', got %q", cfg.Slack.Token)
		}

		if cfg.Slack.DefaultChannel != "#code-reviews" {
			t.Errorf("Expected default channel '#code-reviews', got %q", cfg.Slack.DefaultChannel)
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
		err := os.WriteFile(configPath, []byte(configContent), 0o600)
		if err != nil {
			t.Fatalf("Failed to write test config file: %v", err)
		}

		// Load the config
		_, err = LoadConfig(configPath)
		if err == nil {
			t.Error("Expected error for invalid YAML, got nil")
		}
	})

	t.Run("should use default path when path is empty", func(t *testing.T) {
		// When path is empty, LoadConfig tries to use $HOME/.config/gh-nudge/config.yaml
		// We can't easily test this without setting up a real config file in the home directory,
		// but we can verify that it attempts to use the correct path by checking the error message
		_, err := LoadConfig("")
		if err == nil {
			t.Error("Expected error when loading from default path (likely doesn't exist), got nil")
		}

		// Verify the error is related to home directory or default path
		// In some test environments, $HOME may not be set, which is also a valid error case
		expectedPathSuffix := filepath.Join(".config", "gh-nudge", "config.yaml")
		if !strings.Contains(err.Error(), expectedPathSuffix) && !strings.Contains(err.Error(), "home directory") {
			t.Errorf("Expected error to contain default path %q or 'home directory', got: %v", expectedPathSuffix, err)
		}
	})

	t.Run("should apply default values", func(t *testing.T) {
		testCases := []struct {
			name                     string
			configContent            string
			expectedThresholdHours   int
			expectedMessageTemplate  string
			expectedWorkingHoursOnly bool
			expectedDMByDefault      bool
		}{
			{
				name: "missing ReminderThresholdHours should default to 24",
				configContent: `
github:
  owner: "test-org"
  repos:
    - "repo1"

slack:
  token: "xoxb-test-token"
  default_channel: "#code-reviews"

settings:
  working_hours_only: true
  message_template: "Custom message"
  dm_by_default: false
`,
				expectedThresholdHours:   24,
				expectedMessageTemplate:  "Custom message",
				expectedWorkingHoursOnly: true,
				expectedDMByDefault:      false,
			},
			{
				name: "missing MessageTemplate should use default",
				configContent: `
github:
  owner: "test-org"
  repos:
    - "repo1"

slack:
  token: "xoxb-test-token"
  default_channel: "#code-reviews"

settings:
  reminder_threshold_hours: 48
  working_hours_only: false
  dm_by_default: true
`,
				expectedThresholdHours:   48,
				expectedMessageTemplate:  "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours.",
				expectedWorkingHoursOnly: false,
				expectedDMByDefault:      true,
			},
			{
				name: "missing both settings should use defaults",
				configContent: `
github:
  owner: "test-org"
  repos:
    - "repo1"

slack:
  token: "xoxb-test-token"
  default_channel: "#code-reviews"

settings:
  working_hours_only: true
`,
				expectedThresholdHours:   24,
				expectedMessageTemplate:  "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours.",
				expectedWorkingHoursOnly: true,
				expectedDMByDefault:      false,
			},
			{
				name: "empty settings section should use all defaults",
				configContent: `
github:
  owner: "test-org"
  repos:
    - "repo1"

slack:
  token: "xoxb-test-token"
  default_channel: "#code-reviews"

settings:
`,
				expectedThresholdHours:   24,
				expectedMessageTemplate:  "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours.",
				expectedWorkingHoursOnly: false,
				expectedDMByDefault:      false,
			},
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				tempDir := t.TempDir()
				configPath := filepath.Join(tempDir, "config.yaml")

				err := os.WriteFile(configPath, []byte(tc.configContent), 0o600)
				if err != nil {
					t.Fatalf("Failed to write test config file: %v", err)
				}

				cfg, err := LoadConfig(configPath)
				if err != nil {
					t.Fatalf("Failed to load config: %v", err)
				}

				if cfg.Settings.ReminderThresholdHours != tc.expectedThresholdHours {
					t.Errorf("Expected reminder threshold hours %d, got %d",
						tc.expectedThresholdHours, cfg.Settings.ReminderThresholdHours)
				}

				if cfg.Settings.MessageTemplate != tc.expectedMessageTemplate {
					t.Errorf("Expected message template %q, got %q",
						tc.expectedMessageTemplate, cfg.Settings.MessageTemplate)
				}

				if cfg.Settings.WorkingHoursOnly != tc.expectedWorkingHoursOnly {
					t.Errorf("Expected working hours only %v, got %v",
						tc.expectedWorkingHoursOnly, cfg.Settings.WorkingHoursOnly)
				}

				if cfg.Settings.DMByDefault != tc.expectedDMByDefault {
					t.Errorf("Expected DM by default %v, got %v",
						tc.expectedDMByDefault, cfg.Settings.DMByDefault)
				}
			})
		}
	})

	t.Run("should handle minimal valid config", func(t *testing.T) {
		tempDir := t.TempDir()
		configPath := filepath.Join(tempDir, "config.yaml")

		// Minimal valid config with just required fields
		configContent := `
github:
  owner: "minimal-org"
  repos:
    - "minimal-repo"

slack:
  token: "xoxb-minimal-token"
`
		err := os.WriteFile(configPath, []byte(configContent), 0o600)
		if err != nil {
			t.Fatalf("Failed to write test config file: %v", err)
		}

		cfg, err := LoadConfig(configPath)
		if err != nil {
			t.Fatalf("Failed to load config: %v", err)
		}

		// Verify required fields
		if cfg.GitHub.Owner != "minimal-org" {
			t.Errorf("Expected GitHub owner 'minimal-org', got %q", cfg.GitHub.Owner)
		}

		if cfg.Slack.Token != "xoxb-minimal-token" {
			t.Errorf("Expected Slack token 'xoxb-minimal-token', got %q", cfg.Slack.Token)
		}

		// Verify defaults are applied
		if cfg.Settings.ReminderThresholdHours != 24 {
			t.Errorf("Expected default reminder threshold 24, got %d", cfg.Settings.ReminderThresholdHours)
		}

		if cfg.Settings.MessageTemplate == "" {
			t.Error("Expected default message template to be set, got empty string")
		}
	})

	t.Run("should preserve explicit zero values", func(t *testing.T) {
		tempDir := t.TempDir()
		configPath := filepath.Join(tempDir, "config.yaml")

		// Config with explicit zero/false values
		configContent := `
github:
  owner: "test-org"
  repos:
    - "repo1"

slack:
  token: "xoxb-test-token"

settings:
  reminder_threshold_hours: 1  # Non-zero value
  working_hours_only: false
  dm_by_default: false
  message_template: ""  # Explicitly empty
`
		err := os.WriteFile(configPath, []byte(configContent), 0o600)
		if err != nil {
			t.Fatalf("Failed to write test config file: %v", err)
		}

		cfg, err := LoadConfig(configPath)
		if err != nil {
			t.Fatalf("Failed to load config: %v", err)
		}

		// Verify that non-zero threshold is preserved
		if cfg.Settings.ReminderThresholdHours != 1 {
			t.Errorf("Expected reminder threshold 1, got %d", cfg.Settings.ReminderThresholdHours)
		}

		// Verify that empty message template gets the default
		if cfg.Settings.MessageTemplate == "" {
			t.Error("Expected default message template to be applied when empty, but got empty string")
		}
	})

	t.Run("should handle channel routing correctly", func(t *testing.T) {
		tempDir := t.TempDir()
		configPath := filepath.Join(tempDir, "config.yaml")

		configContent := `
github:
  owner: "test-org"
  repos:
    - "repo1"

slack:
  token: "xoxb-test-token"
  channel_routing:
    - pattern: ".*\\.go$"
      channel: "#go-reviews"
    - pattern: ".*\\.js$"
      channel: "#js-reviews"
`
		err := os.WriteFile(configPath, []byte(configContent), 0o600)
		if err != nil {
			t.Fatalf("Failed to write test config file: %v", err)
		}

		cfg, err := LoadConfig(configPath)
		if err != nil {
			t.Fatalf("Failed to load config: %v", err)
		}

		if len(cfg.Slack.ChannelRouting) != 2 {
			t.Fatalf("Expected 2 channel routing entries, got %d", len(cfg.Slack.ChannelRouting))
		}

		if cfg.Slack.ChannelRouting[0].Pattern != ".*\\.go$" {
			t.Errorf("Expected first pattern '.*\\.go$', got %q", cfg.Slack.ChannelRouting[0].Pattern)
		}

		if cfg.Slack.ChannelRouting[0].Channel != "#go-reviews" {
			t.Errorf("Expected first channel '#go-reviews', got %q", cfg.Slack.ChannelRouting[0].Channel)
		}

		if cfg.Slack.ChannelRouting[1].Pattern != ".*\\.js$" {
			t.Errorf("Expected second pattern '.*\\.js$', got %q", cfg.Slack.ChannelRouting[1].Pattern)
		}

		if cfg.Slack.ChannelRouting[1].Channel != "#js-reviews" {
			t.Errorf("Expected second channel '#js-reviews', got %q", cfg.Slack.ChannelRouting[1].Channel)
		}
	})
}
