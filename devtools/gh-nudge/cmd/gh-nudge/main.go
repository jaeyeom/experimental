// Package main provides the entry point for the gh-nudge application.
package main

import (
	"flag"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/notification"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/slack"
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

func main() {
	flag.Parse()

	// Set up logging
	logLevel := slog.LevelInfo
	if verbose {
		logLevel = slog.LevelDebug
	}
	logger := slog.New(slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{
		Level: logLevel,
	}))
	slog.SetDefault(logger)

	// Load configuration
	cfg, err := config.LoadConfig(configPath)
	if err != nil {
		slog.Error("Failed to load configuration", "error", err)
		os.Exit(1)
	}

	// Initialize GitHub client
	githubClient := github.NewClient(nil)

	// Initialize Slack client
	slackClient := slack.NewClient(cfg.Slack.Token, cfg.Slack.UserIDMapping, cfg.Slack.DMChannelIDMapping)
	slackClient.SetChannelRouting(convertChannelRouting(cfg.Slack.ChannelRouting))
	slackClient.SetDefaultChannel(cfg.Slack.DefaultChannel)

	// Initialize notification tracker with persistence
	// Store notifications in the same directory as the config file
	var notificationTracker *notification.Tracker
	var notificationPath string

	// Determine the path for notification storage
	if configPath != "" {
		// If config path is specified, use the same directory
		notificationPath = filepath.Join(filepath.Dir(configPath), "notifications.json")
	} else {
		// Otherwise use the default location in user's home directory
		home, err := os.UserHomeDir()
		if err != nil {
			slog.Error("Failed to get user home directory", "error", err)
			os.Exit(1)
		}
		notificationPath = filepath.Join(home, ".config", "gh-nudge", "notifications.json")
	}

	// Create the persistent tracker
	notificationTracker, err = notification.NewPersistentTracker(notificationPath)
	if err != nil {
		slog.Error("Failed to initialize notification tracker", "error", err)
		os.Exit(1)
	}

	slog.Info("Using notification history file", "path", notificationPath)

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
		slog.Debug("Processing pull request", "title", pr.Title, "url", pr.URL)

		// For each reviewer, send a notification
		for _, reviewer := range pr.ReviewRequests {
			// Skip team reviews for now
			if reviewer.Type != "User" {
				continue
			}

			slog.Debug("Processing reviewer", "login", reviewer.Login)

			// Check if we have a Slack user ID for this GitHub user
			_, ok := slackClient.GetSlackUserIDForGitHubUser(reviewer.Login)
			if !ok {
				slog.Error("No Slack user ID mapping for GitHub user", "github_user", reviewer.Login)
				continue
			}

			// Check if we should notify based on threshold hours
			shouldNotify := notificationTracker.ShouldNotify(pr.URL, reviewer.Login, cfg.Settings.ReminderThresholdHours)
			if !shouldNotify {
				slog.Info("Skipping notification within threshold period",
					"pr", pr.Title,
					"reviewer", reviewer.Login,
					"threshold_hours", cfg.Settings.ReminderThresholdHours)
				continue
			}

			// Use the NudgeReviewer method to send or simulate sending a notification
			destination, message, err := slackClient.NudgeReviewer(
				pr,
				reviewer.Login,
				cfg.Settings.ReminderThresholdHours,
				cfg.Settings.MessageTemplate,
				cfg.Settings.DMByDefault,
				dryRun,
			)

			// Handle the results
			if dryRun {
				if err != nil {
					slog.Error("Would fail to send notification", "error", err)
				} else {
					fmt.Printf("Would send to %s: %s\n", destination, message)
				}

				// In dry-run mode, don't actually record the notification
				continue
			}

			// Handle the actual notification result
			if err != nil {
				slog.Error("Failed to send notification", "error", err)
			} else {
				// Record that we sent a notification
				if err := notificationTracker.RecordNotification(pr.URL, reviewer.Login); err != nil {
					slog.Error("Failed to record notification", "error", err)
				} else {
					slog.Info("Recorded notification", "pr", pr.Title, "reviewer", reviewer.Login)
				}
			}

			// Sleep briefly to avoid rate limiting
			time.Sleep(100 * time.Millisecond)
		}
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
