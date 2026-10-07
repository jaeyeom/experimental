// Package main provides the entry point for the gh-nudge application.
package main

import (
	"context"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/notification"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/slack"
	executor "github.com/jaeyeom/go-cmdexec"
)

var (
	configPath string
	dryRun     bool
	verbose    bool
)

func init() {
	flag.StringVar(&configPath, "config", "", "Path to configuration file")
	flag.BoolVar(&dryRun, "dry-run", false, "Run in dry-run mode (no notifications sent)")
	flag.BoolVar(&verbose, "verbose", false, "Show verbose output")
}

// setupLogger configures the application logger based on verbosity level.
func setupLogger() *slog.Logger {
	logLevel := slog.LevelInfo
	if verbose {
		logLevel = slog.LevelDebug
	}
	logger := slog.New(slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{
		Level: logLevel,
	}))
	slog.SetDefault(logger)
	return logger
}

// getNotificationPath determines the path for storing notification data.
func getNotificationPath() (string, error) {
	if configPath != "" {
		// If config path is specified, use the same directory
		return filepath.Join(filepath.Dir(configPath), "notifications.json"), nil
	}

	// Otherwise use the default location in user's home directory
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("failed to get user home directory: %w", err)
	}
	return filepath.Join(home, ".config", "gh-nudge", "notifications.json"), nil
}

// initializeClients sets up the GitHub and Slack clients.
func initializeClients(cfg *config.Config) (*github.Client, *slack.Client) {
	// Initialize GitHub client
	ctx := context.Background()
	exec := executor.NewBasicExecutor()
	githubClient := github.NewClient(ctx, exec)

	// Initialize Slack client with appropriate MessagePoster
	var messagePoster slack.MessagePoster
	if dryRun {
		messagePoster = slack.NewDryRunMessagePoster()
	} else {
		// Use nil to let NewClient create the real Slack client
		messagePoster = nil
	}

	slackClient := slack.NewClient(slack.ClientConfig{
		Token:              cfg.Slack.Token,
		UserIDMapping:      cfg.Slack.UserIDMapping,
		DMChannelIDMapping: cfg.Slack.DMChannelIDMapping,
		MessagePoster:      messagePoster,
	})
	slackClient.SetChannelRouting(convertChannelRouting(cfg.Slack.ChannelRouting))
	slackClient.SetDefaultChannel(cfg.Slack.DefaultChannel)

	return githubClient, slackClient
}

// initializeNotificationTracker sets up the notification tracker.
func initializeNotificationTracker(notificationPath string) (*notification.Tracker, error) {
	notificationTracker, err := notification.NewPersistentTracker(notificationPath)
	if err != nil {
		return nil, fmt.Errorf("failed to initialize notification tracker: %w", err)
	}
	slog.Info("Using notification history file", "path", notificationPath)
	return notificationTracker, nil
}

// processReviewer handles the notification logic for a single reviewer.
func processReviewer(
	pr models.PullRequest,
	reviewer models.ReviewRequest,
	slackClient *slack.Client,
	notificationTracker *notification.Tracker,
	cfg *config.Config,
) error {
	// Skip team reviews for now
	if reviewer.Type != "User" {
		return nil
	}

	slog.Debug("Processing reviewer", "login", reviewer.Login)

	if slices.Contains(cfg.Settings.SkipUsers, reviewer.Login) {
		slog.Info("Skipping notification for user in skip_users",
			"pr", pr.Title,
			"reviewer", reviewer.Login)
		return nil
	}

	// Check if we have a Slack user ID for this GitHub user
	_, ok := slackClient.GetSlackUserIDForGitHubUser(slack.GitHubUsername(reviewer.Login))
	if !ok {
		return fmt.Errorf("no Slack user ID mapping for GitHub user: %s", reviewer.Login)
	}

	// Check if we should notify based on threshold hours and request cycle.
	shouldNotify := notificationTracker.ShouldNotifyReviewer(
		pr.URL,
		reviewer.Login,
		cfg.Settings.ReminderThresholdHours,
		pr.LatestReviewSubmittedAt(reviewer.Login),
	)
	if !shouldNotify {
		slog.Info("Skipping notification within threshold period",
			"pr", pr.Title,
			"reviewer", reviewer.Login,
			"threshold_hours", cfg.Settings.ReminderThresholdHours)
		return nil
	}

	// Use the NudgeReviewer method to send or simulate sending a notification
	destination, message, err := slackClient.NudgeReviewer(
		pr,
		slack.GitHubUsername(reviewer.Login),
		cfg.Settings.ReminderThresholdHours,
		cfg.Settings.MessageTemplate,
		cfg.Settings.DMByDefault,
	)

	if err == slack.ErrDryRun {
		slog.Info("Dry run mode, not sending notification",
			"pr", pr.Title,
			"reviewer", reviewer.Login,
			"destination", destination,
			"message", message)
		return nil
	} else if err != nil {
		return fmt.Errorf("failed to send notification: %w", err)
	}

	// Record that we sent a notification
	if err := notificationTracker.RecordNotification(pr.URL, reviewer.Login); err != nil {
		return fmt.Errorf("failed to record notification: %w", err)
	}

	slog.Info("Recorded notification", "pr", pr.Title, "reviewer", reviewer.Login)
	return nil
}

// pullRequestLookup fetches label and review-request times for a pull request.
type pullRequestLookup interface {
	LatestLabelAddedAt(prURL string, labels []string) (map[string]time.Time, error)
	EarliestReviewRequestedAt(prURL string) (map[string]time.Time, error)
}

// processPullRequest handles the notification logic for a single pull request.
func processPullRequest(
	pr models.PullRequest,
	lookup pullRequestLookup,
	slackClient *slack.Client,
	notificationTracker *notification.Tracker,
	cfg *config.Config,
) {
	slog.Debug("Processing pull request", "title", pr.Title, "url", pr.URL)

	if !pr.AllowsNudge(cfg.Settings.RequireLabels, cfg.Settings.SkipLabels) {
		slog.Info("Skipping pull request due to label filter",
			"pr", pr.Title,
			"url", pr.URL,
			"labels", pr.Labels,
			"require_labels", cfg.Settings.RequireLabels,
			"skip_labels", cfg.Settings.SkipLabels)
		return
	}

	if !labelAgesAllowNudge(pr, lookup, cfg.Settings.RequireLabelAges, time.Now()) {
		return
	}

	requestedAt, ok := firstReviewRequestedAt(pr, lookup, cfg.Settings.MinFirstReviewRequestHours)
	if !ok {
		return
	}

	// Process each reviewer
	for _, reviewer := range pr.ReviewRequests {
		if !firstReviewRequestAllowsNudge(
			pr,
			reviewer,
			requestedAt,
			cfg.Settings.MinFirstReviewRequestHours,
			time.Now(),
		) {
			continue
		}

		err := processReviewer(pr, reviewer, slackClient, notificationTracker, cfg)
		if err != nil {
			slog.Error("Error processing reviewer", "reviewer", reviewer.Login, "error", err)
		}

		// Sleep briefly to avoid rate limiting
		time.Sleep(100 * time.Millisecond)
	}
}

// firstReviewRequestedAt loads the earliest review-request time for each user.
// A non-positive minimum skips the lookup and allows the pull request.
// A lookup failure skips the pull request.
func firstReviewRequestedAt(pr models.PullRequest, lookup pullRequestLookup, minHours int) (map[string]time.Time, bool) {
	if minHours <= 0 {
		return nil, true
	}
	requested, err := lookup.EarliestReviewRequestedAt(pr.URL)
	if err != nil {
		slog.Error("Skipping pull request because first review request lookup failed",
			"pr", pr.Title,
			"url", pr.URL,
			"error", err)
		return nil, false
	}
	return requested, true
}

// firstReviewRequestAllowsNudge reports whether this reviewer has been
// requested for at least minHours, measured from the first request.
// A non-positive minimum allows the reviewer. Teams are left to processReviewer.
func firstReviewRequestAllowsNudge(pr models.PullRequest, reviewer models.ReviewRequest, requestedAt map[string]time.Time, minHours int, now time.Time) bool {
	if minHours <= 0 || reviewer.Type != "User" {
		return true
	}
	requested := requestedAt[reviewer.Login]
	if models.MeetsFirstReviewRequestAge(requested, minHours, now) {
		return true
	}
	slog.Info("Skipping reviewer until the first review request is old enough",
		"pr", pr.Title,
		"url", pr.URL,
		"reviewer", reviewer.Login,
		"min_first_review_request_hours", minHours,
		"first_requested_at", requested)
	return false
}

func main() {
	flag.Parse()

	// Set up logging
	setupLogger()

	// Load configuration
	cfg, err := config.LoadConfig(configPath)
	if err != nil {
		slog.Error("Failed to load configuration", "error", err)
		os.Exit(1)
	}

	// Initialize clients
	githubClient, slackClient := initializeClients(cfg)

	// Initialize notification tracker with persistence
	notificationPath, err := getNotificationPath()
	if err != nil {
		slog.Error("Failed to determine notification path", "error", err)
		os.Exit(1)
	}

	notificationTracker, err := initializeNotificationTracker(notificationPath)
	if err != nil {
		slog.Error("Failed to initialize notification tracker", "error", err)
		os.Exit(1)
	}

	// Get pending pull requests
	slog.Info("Fetching pending pull requests...")
	prs, err := githubClient.GetPendingPullRequests()
	if err != nil {
		slog.Error("Failed to fetch pull requests", "error", err)
		os.Exit(1)
	}

	slog.Info("Found pull requests", "count", len(prs))

	// Process each pull request
	for _, pr := range prs {
		processPullRequest(pr, githubClient, slackClient, notificationTracker, cfg)
	}

	slog.Info("Finished processing pull requests")
}

// labelAgesAllowNudge reports whether every configured label-age rule is met.
// An empty rule list allows the pull request without a lookup.
// A pull request that is missing one of the labels is skipped without a lookup.
// A lookup failure or an unmet age skips the pull request.
func labelAgesAllowNudge(pr models.PullRequest, lookup pullRequestLookup, rules []config.LabelAgeConfig, now time.Time) bool {
	if len(rules) == 0 {
		return true
	}

	modelRules := make([]models.LabelMinAge, len(rules))
	var names []string
	seen := make(map[string]struct{}, len(rules))
	for i, rule := range rules {
		modelRules[i] = models.LabelMinAge{Label: rule.Label, MinHours: rule.MinHours}
		if !pr.HasLabel(rule.Label) {
			slog.Info("Skipping pull request until required label ages are met",
				"pr", pr.Title,
				"url", pr.URL,
				"labels", pr.Labels,
				"require_label_ages", rules)
			return false
		}
		if _, ok := seen[rule.Label]; ok {
			continue
		}
		seen[rule.Label] = struct{}{}
		names = append(names, rule.Label)
	}

	added, err := lookup.LatestLabelAddedAt(pr.URL, names)
	if err != nil {
		slog.Error("Skipping pull request because label age lookup failed",
			"pr", pr.Title,
			"url", pr.URL,
			"error", err)
		return false
	}
	if !pr.MeetsLabelMinAges(modelRules, added, now) {
		slog.Info("Skipping pull request until required label ages are met",
			"pr", pr.Title,
			"url", pr.URL,
			"labels", pr.Labels,
			"require_label_ages", rules,
			"added_at", added)
		return false
	}
	return true
}

// convertChannelRouting converts the channel routing configuration from the config
// package format to the slack package format.
func convertChannelRouting(routingConfig []config.ChannelRoutingConfig) []slack.ChannelRouting {
	var routing []slack.ChannelRouting
	for _, r := range routingConfig {
		routing = append(routing, slack.ChannelRouting{
			Pattern: r.Pattern,
			Channel: r.Channel,
		})
	}
	return routing
}
