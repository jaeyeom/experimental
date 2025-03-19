// Package main provides the entry point for the gh-nudge application.
package main

import (
	"flag"
	"fmt"
	"log/slog"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
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
	slackClient := slack.NewClient(cfg.Slack.Token, cfg.Slack.UserMapping)
	slackClient.SetChannelRouting(convertChannelRouting(cfg.Slack.ChannelRouting))
	slackClient.SetDefaultChannel(cfg.Slack.DefaultChannel)

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

			// Check if we have a Slack ID for this GitHub user
			slackID, ok := slackClient.GetSlackIDForGitHubUser(reviewer.Login)
			if !ok {
				slog.Error("No Slack ID mapping for GitHub user", "github_user", reviewer.Login)
				continue
			}

			// Format the message
			message := slackClient.FormatMessage(
				cfg.Settings.MessageTemplate,
				pr,
				reviewer.Login,
				cfg.Settings.ReminderThresholdHours,
			)

			// In dry-run mode, just print the message
			if dryRun {
				fmt.Printf("Would send to %s: %s\n", slackID, message)
				continue
			}

			// Send the notification
			var notifyErr error
			if cfg.Settings.DMByDefault {
				slog.Info("Sending DM", "github_user", reviewer.Login, "slack_id", slackID)
				notifyErr = slackClient.SendDirectMessage(slackID, message)
			} else {
				channel := slackClient.GetChannelForPR(pr)
				slog.Info("Sending channel message", "channel", channel)
				notifyErr = slackClient.SendChannelMessage(channel, message)
			}

			if notifyErr != nil {
				slog.Error("Failed to send notification", "error", notifyErr)
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
