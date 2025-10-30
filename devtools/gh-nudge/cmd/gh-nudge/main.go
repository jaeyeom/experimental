// Package main provides the entry point for the gh-nudge application.
package main

import (
	"context"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/notification"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/slack"
	"github.com/jaeyeom/experimental/devtools/internal/executor"
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

	// Check if we have a Slack user ID for this GitHub user
	_, ok := slackClient.GetSlackUserIDForGitHubUser(slack.GitHubUsername(reviewer.Login))
	if !ok {
		return fmt.Errorf("no Slack user ID mapping for GitHub user: %s", reviewer.Login)
	}

	// Check if we should notify based on threshold hours
	shouldNotify := notificationTracker.ShouldNotify(pr.URL, reviewer.Login, cfg.Settings.ReminderThresholdHours)
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

// processPullRequest handles the notification logic for a single pull request.
func processPullRequest(
	pr models.PullRequest,
	slackClient *slack.Client,
	notificationTracker *notification.Tracker,
	cfg *config.Config,
) {
	slog.Debug("Processing pull request", "title", pr.Title, "url", pr.URL)

	// Process each reviewer
	for _, reviewer := range pr.ReviewRequests {
		err := processReviewer(pr, reviewer, slackClient, notificationTracker, cfg)
		if err != nil {
			slog.Error("Error processing reviewer", "reviewer", reviewer.Login, "error", err)
		}

		// Sleep briefly to avoid rate limiting
		time.Sleep(100 * time.Millisecond)
	}
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
		processPullRequest(pr, slackClient, notificationTracker, cfg)
	}

	slog.Info("Finished processing pull requests")
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
