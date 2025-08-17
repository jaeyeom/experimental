// Package config provides functionality to load and parse configuration.
package config

import (
	"fmt"
	"os"
	"path/filepath"

	"gopkg.in/yaml.v3"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/slack"
)

// Config represents the application configuration.
type Config struct {
	GitHub   GitHubConfig   `yaml:"github"`
	Slack    SlackConfig    `yaml:"slack"`
	Settings SettingsConfig `yaml:"settings"`
}

// GitHubConfig contains GitHub-related configuration.
type GitHubConfig struct {
	Owner string   `yaml:"owner"`
	Repos []string `yaml:"repos"`
}

// SlackConfig contains Slack-related configuration.
type SlackConfig struct {
	Token              string                   `yaml:"token"`
	DefaultChannel     string                   `yaml:"default_channel"`
	UserIDMapping      slack.UserIDMapping      `yaml:"user_id_mapping"`
	DMChannelIDMapping slack.DMChannelIDMapping `yaml:"dm_channel_id_mapping"`
	ChannelRouting     []ChannelRoutingConfig   `yaml:"channel_routing"`
}

// ChannelRoutingConfig defines file pattern to Slack channel mapping.
type ChannelRoutingConfig struct {
	Pattern string `yaml:"pattern"`
	Channel string `yaml:"channel"`
}

// SettingsConfig contains general application settings.
type SettingsConfig struct {
	ReminderThresholdHours int    `yaml:"reminder_threshold_hours"`
	WorkingHoursOnly       bool   `yaml:"working_hours_only"`
	MessageTemplate        string `yaml:"message_template"`
	DMByDefault            bool   `yaml:"dm_by_default"`
}

// LoadConfig loads the configuration from the specified file path.
// If path is empty, it attempts to load from the default location.
func LoadConfig(path string) (*Config, error) {
	if path == "" {
		// Default config path is $HOME/.config/gh-nudge/config.yaml
		home, err := os.UserHomeDir()
		if err != nil {
			return nil, fmt.Errorf("failed to get user home directory: %w", err)
		}
		path = filepath.Join(home, ".config", "gh-nudge", "config.yaml")
	}

	// Read the config file
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	// Parse the YAML
	var cfg Config
	if err := yaml.Unmarshal(data, &cfg); err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}

	// Set default values if not specified
	if cfg.Settings.ReminderThresholdHours == 0 {
		cfg.Settings.ReminderThresholdHours = 24 // Default to 24 hours
	}

	if cfg.Settings.MessageTemplate == "" {
		cfg.Settings.MessageTemplate = "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours."
	}

	return &cfg, nil
}
