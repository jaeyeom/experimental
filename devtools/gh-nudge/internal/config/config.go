// Package config provides functionality to load and parse configuration.
package config

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/apple/pkl-go/pkl"
	"gopkg.in/yaml.v3"

	pklconfig "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/slack"
)

// SettingsFromGlobal is the settings source when no repo_settings entry applied.
const SettingsFromGlobal = "global"

// Config represents the application configuration.
type Config struct {
	GitHub   GitHubConfig   `yaml:"github"`
	Slack    SlackConfig    `yaml:"slack"`
	Settings SettingsConfig `yaml:"settings"`
	// RepoSettings overrides Settings for one <owner>/<repo> key.
	// A nil field inherits the global field. A non-nil empty list replaces it.
	RepoSettings map[string]RepoSettings `yaml:"repo_settings"`
	// Approve holds gh-approve settings. Only Pkl config supports it.
	Approve ApproveConfig `yaml:"-"`
	// Viewed holds gh-approve "mark viewed" settings. Only Pkl config supports it.
	Viewed ViewedConfig `yaml:"-"`
	// Path is the file gh-merge selected. It is not read from the file.
	Path string `yaml:"-"`
	// SettingsFrom is the repo_settings key that applied, or SettingsFromGlobal.
	// It is not read from the file.
	SettingsFrom string `yaml:"-"`
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

// LabelAgeConfig requires a label to have been on the pull request for MinHours
// since it was most recently added.
type LabelAgeConfig struct {
	Label    string `yaml:"label"`
	MinHours int    `yaml:"min_hours"`
}

// SettingsConfig contains general application settings.
type SettingsConfig struct {
	ReminderThresholdHours int              `yaml:"reminder_threshold_hours"`
	WorkingHoursOnly       bool             `yaml:"working_hours_only"`
	MessageTemplate        string           `yaml:"message_template"`
	DMByDefault            bool             `yaml:"dm_by_default"`
	RequireLabels          []string         `yaml:"require_labels"`
	SkipLabels             []string         `yaml:"skip_labels"`
	SkipUsers              []string         `yaml:"skip_users"`
	RequireLabelAges       []LabelAgeConfig `yaml:"require_label_ages"`
	// MinFirstReviewRequestHours is the minimum age of a reviewer's first
	// review request. Zero disables the gate. A later request does not restart it.
	MinFirstReviewRequestHours int `yaml:"min_first_review_request_hours"`
	// BlockMergeLabels stops gh-merge from merging a pull request that has any
	// of these labels. gh-nudge ignores this list. Empty means no label blocks
	// a merge. Names match exactly, including case.
	BlockMergeLabels []string `yaml:"block_merge_labels"`
	// RequiredChecks are check names that must be green before gh-merge queue
	// enqueues. Empty makes queue refuse unless --allow-no-required-checks is
	// set. Names match exactly.
	RequiredChecks []string `yaml:"required_checks"`
	// FlakeSignatures are regular expressions matched against a failing log.
	// A match is a hint, not a verdict.
	FlakeSignatures []string `yaml:"flake_signatures"`
	// RCAMarker is a comment template. Placeholders are {event_id} and {class}.
	// Class is our-code, flake, or infra. Empty matches nothing.
	RCAMarker string `yaml:"rca_marker"`
	// MaxRequeues is how many earlier flake or infra requeues are allowed.
	// Nil means the default, 2. Zero means none are allowed.
	MaxRequeues *int `yaml:"max_requeues"`
	// FailureLogPattern is the regular expression for CircleCI failure lines.
	// Empty means DefaultFailureLogPattern. An invalid pattern fails config load.
	// The default matches a line that starts with ERROR: and a timestamp prefix
	// such as (12:34:56) ERROR:.
	FailureLogPattern string `yaml:"failure_log_pattern"`
	// CircleCILog chooses how gh-merge reads CircleCI logs: auto, api, or cli.
	// auto uses the HTTP API when CIRCLE_TOKEN is set, and the circleci CLI otherwise.
	// api keeps the HTTP path. cli uses the CLI even when a token is set and does
	// not call the HTTP API. The CLI may read its token from the system keyring
	// and then needs an unsandboxed environment.
	CircleCILog string `yaml:"circleci_log"`
}

// RepoSettings is the subset of settings gh-merge reads for one repository.
// A nil field inherits the global setting. A non-nil empty list does not.
type RepoSettings struct {
	BlockMergeLabels  *[]string `yaml:"block_merge_labels"`
	RequiredChecks    *[]string `yaml:"required_checks"`
	FlakeSignatures   *[]string `yaml:"flake_signatures"`
	RCAMarker         *string   `yaml:"rca_marker"`
	MaxRequeues       *int      `yaml:"max_requeues"`
	FailureLogPattern *string   `yaml:"failure_log_pattern"`
	CircleCILog       *string   `yaml:"circleci_log"`
}

// DefaultMaxRequeues is the requeue cap when config omits max_requeues.
const DefaultMaxRequeues = 2

// DefaultFailureLogPattern matches a CircleCI line that starts with ERROR:
// and a line with a timestamp prefix such as (12:34:56) ERROR:.
const DefaultFailureLogPattern = `^(?:[(][0-9]{2}:[0-9]{2}:[0-9]{2}[)] )?ERROR:`

// CircleCI log modes.
const (
	// CircleCILogAuto uses the HTTP API when CIRCLE_TOKEN is set, otherwise the CLI.
	CircleCILogAuto = "auto"
	// CircleCILogAPI keeps the HTTP API path.
	CircleCILogAPI = "api"
	// CircleCILogCLI uses the circleci CLI and does not call the HTTP API.
	CircleCILogCLI = "cli"
)

// RequeueLimit returns the number of earlier requeues that are still allowed.
// A nil MaxRequeues uses DefaultMaxRequeues.
func (s SettingsConfig) RequeueLimit() int {
	if s.MaxRequeues == nil {
		return DefaultMaxRequeues
	}
	return *s.MaxRequeues
}

// EffectiveSettings returns the settings for owner/repo and the source name.
// A missing entry returns the global settings and SettingsFromGlobal.
// A present entry replaces each non-nil field. An explicit empty list replaces
// the global list and does not inherit it.
func (c *Config) EffectiveSettings(owner, repo string) (SettingsConfig, string) {
	key := owner + "/" + repo
	entry, ok := c.RepoSettings[key]
	if owner == "" || repo == "" || !ok {
		return cloneSettings(c.Settings), SettingsFromGlobal
	}
	settings := cloneSettings(c.Settings)
	if entry.BlockMergeLabels != nil {
		settings.BlockMergeLabels = cloneStrings(*entry.BlockMergeLabels)
	}
	if entry.RequiredChecks != nil {
		settings.RequiredChecks = cloneStrings(*entry.RequiredChecks)
	}
	if entry.FlakeSignatures != nil {
		settings.FlakeSignatures = cloneStrings(*entry.FlakeSignatures)
	}
	if entry.RCAMarker != nil {
		settings.RCAMarker = *entry.RCAMarker
	}
	if entry.MaxRequeues != nil {
		settings.MaxRequeues = intPtr(*entry.MaxRequeues)
	}
	if entry.FailureLogPattern != nil {
		settings.FailureLogPattern = *entry.FailureLogPattern
		if settings.FailureLogPattern == "" {
			settings.FailureLogPattern = DefaultFailureLogPattern
		}
	}
	if entry.CircleCILog != nil {
		settings.CircleCILog = *entry.CircleCILog
		if settings.CircleCILog == "" {
			settings.CircleCILog = CircleCILogAuto
		}
	}
	return settings, key
}

// ForRepository returns a copy whose Settings are effective for owner/repo.
func (c *Config) ForRepository(owner, repo string) *Config {
	settings, from := c.EffectiveSettings(owner, repo)
	next := *c
	next.Settings = settings
	next.SettingsFrom = from
	return &next
}

func cloneSettings(settings SettingsConfig) SettingsConfig {
	settings.BlockMergeLabels = cloneStrings(settings.BlockMergeLabels)
	settings.RequiredChecks = cloneStrings(settings.RequiredChecks)
	settings.FlakeSignatures = cloneStrings(settings.FlakeSignatures)
	settings.RequireLabels = cloneStrings(settings.RequireLabels)
	settings.SkipLabels = cloneStrings(settings.SkipLabels)
	settings.SkipUsers = cloneStrings(settings.SkipUsers)
	if settings.RequireLabelAges != nil {
		settings.RequireLabelAges = append([]LabelAgeConfig(nil), settings.RequireLabelAges...)
	}
	settings.MaxRequeues = copyIntPtr(settings.MaxRequeues)
	return settings
}

func cloneStrings(items []string) []string {
	if items == nil {
		return nil
	}
	copied := make([]string, len(items))
	copy(copied, items)
	return copied
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

	cfg := convertPklConfig(&pklCfg)
	if err := validateConfig(cfg); err != nil {
		return nil, err
	}
	return cfg, nil
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
			ReminderThresholdHours:     pklCfg.Settings.ReminderThresholdHours,
			WorkingHoursOnly:           pklCfg.Settings.WorkingHoursOnly,
			MessageTemplate:            pklCfg.Settings.MessageTemplate,
			DMByDefault:                pklCfg.Settings.DmByDefault,
			RequireLabels:              pklCfg.Settings.RequireLabels,
			SkipLabels:                 pklCfg.Settings.SkipLabels,
			SkipUsers:                  pklCfg.Settings.SkipUsers,
			MinFirstReviewRequestHours: pklCfg.Settings.MinFirstReviewRequestHours,
			BlockMergeLabels:           pklCfg.Settings.BlockMergeLabels,
			RequiredChecks:             pklCfg.Settings.RequiredChecks,
			FlakeSignatures:            pklCfg.Settings.FlakeSignatures,
			RCAMarker:                  pklCfg.Settings.RcaMarker,
			MaxRequeues:                intPtr(pklCfg.Settings.MaxRequeues),
			FailureLogPattern:          pklCfg.Settings.FailureLogPattern,
			CircleCILog:                pklCfg.Settings.CircleciLog,
		},
		RepoSettings: convertRepoSettings(pklCfg.RepoSettings),
		Approve:      convertApprove(pklCfg.Approve),
		Viewed:       convertViewed(pklCfg.Viewed),
	}
	normalizeSettings(&cfg.Settings)

	for _, age := range pklCfg.Settings.RequireLabelAges {
		cfg.Settings.RequireLabelAges = append(cfg.Settings.RequireLabelAges, LabelAgeConfig{
			Label:    age.Label,
			MinHours: age.MinHours,
		})
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

func convertRepoSettings(entries map[string]pklconfig.RepoSettings) map[string]RepoSettings {
	if len(entries) == 0 {
		return nil
	}
	out := make(map[string]RepoSettings, len(entries))
	for key, entry := range entries {
		out[key] = RepoSettings{
			BlockMergeLabels:  stringListPtr(entry.BlockMergeLabels),
			RequiredChecks:    stringListPtr(entry.RequiredChecks),
			FlakeSignatures:   stringListPtr(entry.FlakeSignatures),
			RCAMarker:         stringPtr(entry.RcaMarker),
			MaxRequeues:       copyIntPtr(entry.MaxRequeues),
			FailureLogPattern: stringPtr(entry.FailureLogPattern),
			CircleCILog:       stringPtr(entry.CircleciLog),
		}
	}
	return out
}

func stringListPtr(items *[]string) *[]string {
	if items == nil {
		return nil
	}
	copied := cloneStrings(*items)
	return &copied
}

func stringPtr(value *string) *string {
	if value == nil {
		return nil
	}
	copied := *value
	return &copied
}

func copyIntPtr(value *int) *int {
	if value == nil {
		return nil
	}
	return intPtr(*value)
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
	var keys map[string]any
	if err := yaml.Unmarshal(data, &keys); err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}
	if _, ok := keys["approve"]; ok {
		return nil, fmt.Errorf("approve is supported only in Pkl config, not in %s", path)
	}

	// Set default values if not specified
	if cfg.Settings.ReminderThresholdHours == 0 {
		cfg.Settings.ReminderThresholdHours = 24
	}
	if cfg.Settings.MessageTemplate == "" {
		cfg.Settings.MessageTemplate = "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours."
	}
	normalizeSettings(&cfg.Settings)

	if err := validateConfig(&cfg); err != nil {
		return nil, err
	}

	return &cfg, nil
}

func validateConfig(cfg *Config) error {
	if err := validateSettings(cfg.Settings); err != nil {
		return err
	}
	if err := validateApprove(cfg.Approve); err != nil {
		return err
	}
	if err := validateViewed(cfg.Viewed); err != nil {
		return err
	}
	return validateRepoSettings(cfg.RepoSettings)
}

func validateSettings(settings SettingsConfig) error {
	for _, rule := range settings.RequireLabelAges {
		if rule.Label == "" {
			return fmt.Errorf("require_label_ages label must not be empty")
		}
		if rule.MinHours <= 0 {
			return fmt.Errorf("require_label_ages min_hours must be > 0 for label %q", rule.Label)
		}
	}
	if settings.MinFirstReviewRequestHours < 0 {
		return fmt.Errorf("min_first_review_request_hours must be >= 0")
	}
	for _, signature := range settings.FlakeSignatures {
		if _, err := regexp.Compile(signature); err != nil {
			return fmt.Errorf("flake_signatures %q: %w", signature, err)
		}
	}
	if settings.FailureLogPattern != "" {
		if _, err := regexp.Compile(settings.FailureLogPattern); err != nil {
			return fmt.Errorf("failure_log_pattern %q: %w", settings.FailureLogPattern, err)
		}
	}
	switch settings.CircleCILog {
	case "", CircleCILogAuto, CircleCILogAPI, CircleCILogCLI:
	default:
		return fmt.Errorf("circleci_log %q is not auto, api, or cli", settings.CircleCILog)
	}
	if settings.MaxRequeues != nil && *settings.MaxRequeues < 0 {
		return fmt.Errorf("max_requeues must be >= 0")
	}
	return nil
}

func validateRepoSettings(entries map[string]RepoSettings) error {
	for key, entry := range entries {
		if err := validateRepoEntry(key, entry); err != nil {
			return err
		}
	}
	return nil
}

func validateRepoEntry(key string, entry RepoSettings) error {
	probe := SettingsConfig{MaxRequeues: copyIntPtr(entry.MaxRequeues)}
	if entry.FlakeSignatures != nil {
		probe.FlakeSignatures = *entry.FlakeSignatures
	}
	if entry.FailureLogPattern != nil {
		probe.FailureLogPattern = *entry.FailureLogPattern
	}
	if entry.CircleCILog != nil {
		probe.CircleCILog = *entry.CircleCILog
	}
	if err := validateSettings(probe); err != nil {
		return fmt.Errorf("repo_settings %s: %w", key, err)
	}
	return nil
}

func normalizeSettings(settings *SettingsConfig) {
	if settings.FailureLogPattern == "" {
		settings.FailureLogPattern = DefaultFailureLogPattern
	}
	if settings.CircleCILog == "" {
		settings.CircleCILog = CircleCILogAuto
	}
}

func intPtr(n int) *int {
	return &n
}
