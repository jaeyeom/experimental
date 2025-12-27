// Package config provides functionality to load and parse configuration.
package config

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"strings"

	"github.com/apple/pkl-go/pkl"
	"gopkg.in/yaml.v3"

	pklconfig "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl"
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
// Supports both .pkl and .yaml files. If path is empty, it attempts
// to load from the default location (preferring .pkl over .yaml).
func LoadConfig(path string) (*Config, error) {
	if path == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return nil, fmt.Errorf("failed to get user home directory: %w", err)
		}
		basePath := filepath.Join(home, ".config", "gh-nudge", "config")

		// Prefer .pkl over .yaml
		if fileExists(basePath + ".pkl") {
			path = basePath + ".pkl"
		} else {
			path = basePath + ".yaml"
		}
	}

	if strings.HasSuffix(path, ".pkl") {
		return loadPklConfig(path)
	}
	return loadYamlConfig(path)
}

func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func loadPklConfig(path string) (*Config, error) {
	evaluator, err := pkl.NewEvaluator(context.Background(), pkl.PreconfiguredOptions)
	if err != nil {
		return nil, fmt.Errorf("failed to create pkl evaluator: %w", err)
	}
	defer evaluator.Close()

	var pklCfg pklconfig.Config
	if err := evaluator.EvaluateModule(context.Background(), pkl.FileSource(path), &pklCfg); err != nil {
		return nil, fmt.Errorf("failed to evaluate pkl config: %w", err)
	}

	return convertPklConfig(&pklCfg), nil
}

func convertPklConfig(pklCfg *pklconfig.Config) *Config {
	// Convert map[string]string to slack.UserIDMapping
	userIDMapping := make(slack.UserIDMapping)
	for k, v := range pklCfg.Slack.UserIdMapping {
		userIDMapping[slack.GitHubUsername(k)] = slack.UserID(v)
	}

	// Convert map[string]string to slack.DMChannelIDMapping
	dmChannelIDMapping := make(slack.DMChannelIDMapping)
	for k, v := range pklCfg.Slack.DmChannelIdMapping {
		dmChannelIDMapping[slack.GitHubUsername(k)] = slack.ChannelID(v)
	}

	cfg := &Config{
		GitHub: GitHubConfig{
			Repos: pklCfg.Github.Repos,
		},
		Slack: SlackConfig{
			Token:              pklCfg.Slack.Token,
			DefaultChannel:     pklCfg.Slack.DefaultChannel,
			UserIDMapping:      userIDMapping,
			DMChannelIDMapping: dmChannelIDMapping,
		},
		Settings: SettingsConfig{
			ReminderThresholdHours: pklCfg.Settings.ReminderThresholdHours,
			WorkingHoursOnly:       pklCfg.Settings.WorkingHoursOnly,
			MessageTemplate:        pklCfg.Settings.MessageTemplate,
			DMByDefault:            pklCfg.Settings.DmByDefault,
		},
	}

	if pklCfg.Github.Owner != nil {
		cfg.GitHub.Owner = *pklCfg.Github.Owner
	}

	for _, route := range pklCfg.Slack.ChannelRouting {
		cfg.Slack.ChannelRouting = append(cfg.Slack.ChannelRouting, ChannelRoutingConfig{
			Pattern: route.Pattern,
			Channel: route.Channel,
		})
	}

	return cfg
}

func loadYamlConfig(path string) (*Config, error) {
	slog.Warn("YAML configuration is deprecated and will be removed in a future release; please migrate to Pkl format",
		"config_path", path,
		"migration_guide", "https://github.com/jaeyeom/experimental/blob/main/devtools/gh-nudge/PKL_MIGRATION_PLAN.md")

	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var cfg Config
	if err := yaml.Unmarshal(data, &cfg); err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}

	// Set default values if not specified
	if cfg.Settings.ReminderThresholdHours == 0 {
		cfg.Settings.ReminderThresholdHours = 24
	}
	if cfg.Settings.MessageTemplate == "" {
		cfg.Settings.MessageTemplate = "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours."
	}

	return &cfg, nil
}
