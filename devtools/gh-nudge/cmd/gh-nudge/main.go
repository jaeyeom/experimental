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
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/codeowners"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
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
	githubClient := github.NewClient(nil)

	// Initialize Slack client
	slackClient := slack.NewClient(
		cfg.Slack.Token,
		cfg.Slack.UserIDMapping,
		cfg.Slack.DMChannelIDMapping,
		cfg.Slack.TeamChannelMapping,
	)
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
	ghClient *github.Client, // Added ghClient for CODEOWNERS
	pr models.PullRequest,
	reviewer models.ReviewRequest,
	slackClient *slack.Client,
	notificationTracker *notification.Tracker,
	cfg *config.Config,
) error {
	if reviewer.Type == "User" {
		slog.Debug("Processing user reviewer", "login", reviewer.Login)
		// Check if we have a Slack user ID for this GitHub user
		_, ok := slackClient.GetSlackUserIDForGitHubUser(reviewer.Login)
		if !ok {
			return fmt.Errorf("no Slack user ID mapping for GitHub user: %s", reviewer.Login)
		}

		// Check if we should notify based on threshold hours
		shouldNotify := notificationTracker.ShouldNotify(pr.URL, reviewer.Login, cfg.Settings.ReminderThresholdHours)
		if !shouldNotify {
			slog.Info("Skipping user notification within threshold period",
				"pr", pr.Title,
				"reviewer", reviewer.Login,
				"threshold_hours", cfg.Settings.ReminderThresholdHours)
			return nil
		}

		// Use the NudgeReviewer method to send or simulate sending a notification
		destination, message, err := slackClient.NudgeReviewer(
			pr,
			reviewer.Login,
			cfg.Settings.ReminderThresholdHours,
			cfg.Settings.MessageTemplate,
			cfg.Settings.DMByDefault, // DM for users or channel based on PR files
			dryRun,
		)

		// Handle the results
		if dryRun {
			if err != nil {
				slog.Error("Would fail to send user notification", "error", err)
			} else {
				fmt.Printf("Would send to user %s (channel/DM: %s): %s\n", reviewer.Login, destination, message)
			}
			return nil
		}

		// Handle the actual notification result
		if err != nil {
			return fmt.Errorf("failed to send user notification: %w", err)
		}

		// Record that we sent a notification
		if err := notificationTracker.RecordNotification(pr.URL, reviewer.Login); err != nil {
			return fmt.Errorf("failed to record user notification: %w", err)
		}
		slog.Info("Recorded user notification", "pr", pr.Title, "reviewer", reviewer.Login, "destination", destination)
		return nil
	} else if reviewer.Type == "Team" {
		slog.Debug("Processing team reviewer", "team_slug", reviewer.Login) // For teams, Login is the team slug

		// --- Fetch and Parse CODEOWNERS ---
		var parsedCodeowners *codeowners.Codeowners
		var codeownersContent string
		var err error

		// Extract owner, repo from PR URL
		urlParts := strings.Split(pr.URL, "/")
		if len(urlParts) < 5 {
			slog.Error("Cannot parse owner/repo from PR URL", "url", pr.URL)
			return fmt.Errorf("invalid PR URL format: %s", pr.URL)
		}
		owner := urlParts[3]
		repo := urlParts[4]

		baseRef := pr.BaseRefName
		if baseRef == "" {
			slog.Warn("pr.BaseRefName is empty, defaulting to 'main' for CODEOWNERS fetching.", "pr_url", pr.URL, "title", pr.Title)
			baseRef = "main"
		}
		slog.Debug("Using base ref for CODEOWNERS", "ref", baseRef, "pr_url", pr.URL)

		codeownersPaths := []string{".github/CODEOWNERS", "docs/CODEOWNERS", "CODEOWNERS"}
		foundCodeowners := false
		for _, codownersPath := range codeownersPaths {
			slog.Debug("Attempting to fetch CODEOWNERS file", "path", codownersPath, "owner", owner, "repo", repo, "ref", baseRef)
			codeownersContent, err = ghClient.GetFileContent(owner, repo, codownersPath, baseRef)
			if err == nil {
				slog.Info("Successfully fetched CODEOWNERS", "path", codownersPath, "pr_url", pr.URL)
				parsedCodeowners = codeowners.ParseSections(strings.NewReader(codeownersContent))
				foundCodeowners = true
				break // Found and parsed
			}
			// Check if it's a "file not found" type of error to continue, otherwise log and potentially break/return
			if strings.Contains(err.Error(), "file not found") { // Fragile check, depends on github.GetFileContent error format
				slog.Debug("CODEOWNERS file not found at path", "path", codownersPath, "pr_url", pr.URL)
			} else {
				slog.Error("Error fetching CODEOWNERS file", "path", codownersPath, "error", err, "pr_url", pr.URL)
				// Depending on policy, might return err or just log and try notifying assigned team
			}
		}

		if !foundCodeowners {
			slog.Warn("CODEOWNERS file not found in common locations or failed to parse.", "pr_url", pr.URL)
			// Proceed without CODEOWNERS-based team identification, will rely on reviewer.Login if it's a team.
		}

		// --- Determine Relevant Teams ---
		relevantTeams := make(map[string]struct{})
		if parsedCodeowners != nil && len(pr.Files) > 0 {
			slog.Debug("Identifying teams from CODEOWNERS", "pr_url", pr.URL, "num_files", len(pr.Files))
			for _, file := range pr.Files {
				fileOwners := parsedCodeowners.OwnersFor(file.Path)
				for _, ownerEntry := range fileOwners {
					if strings.Contains(ownerEntry, "/") { // Team format: org/team-slug or @org/team-slug
						parts := strings.Split(strings.TrimPrefix(ownerEntry, "@"), "/")
						if len(parts) == 2 {
							teamSlug := parts[1]
							relevantTeams[teamSlug] = struct{}{}
							slog.Debug("Found team in CODEOWNERS", "team_slug", teamSlug, "file", file.Path)
						}
					}
				}
			}
		}

		// Fallback: If no teams from CODEOWNERS, or CODEOWNERS not found/parsed,
		// and a team was explicitly assigned as reviewer, use that team.
		if len(relevantTeams) == 0 && reviewer.Login != "" {
			slog.Info("No teams identified via CODEOWNERS or PR has no files. Using explicitly assigned team.", "team_slug", reviewer.Login, "pr_url", pr.URL)
			relevantTeams[reviewer.Login] = struct{}{}
		}

		if len(relevantTeams) == 0 {
			slog.Info("No relevant teams found to notify for PR", "pr_url", pr.URL, "requested_team", reviewer.Login)
			return nil // No teams to notify
		}

		// --- Notify Teams ---
		for teamSlug := range relevantTeams {
			slog.Debug("Processing notification for identified team", "team_slug", teamSlug, "pr_url", pr.URL)
			// Check if we should notify this team (rate limiting)
			shouldNotify := notificationTracker.ShouldNotify(pr.URL, teamSlug, cfg.Settings.ReminderThresholdHours)
			if !shouldNotify {
				slog.Info("Skipping team notification within threshold period",
					"pr", pr.Title,
					"team", teamSlug,
					"threshold_hours", cfg.Settings.ReminderThresholdHours)
				continue // Next team
			}

			teamChannel, ok := slackClient.GetChannelForTeam(teamSlug)
			if !ok {
				slog.Warn("No Slack channel mapping for team", "team_slug", teamSlug, "pr", pr.Title)
				continue // No channel configured for this team
			}

			// Format message for team.
			// Replace {slack_id} with team name or a generic mention if not possible.
			// The original template might be like: "Hey <@{slack_id}>, the PR '{title}' needs review."
			// For a team, it might be: "Team @{team_name}, the PR '{title}' needs review."
			// Or just post to the channel with a generic message.
			teamMessage := strings.ReplaceAll(cfg.Settings.MessageTemplate, "<@{slack_id}>", fmt.Sprintf("@%s team", teamSlug)) // Or simply "@channel" or a specific team mention format
			teamMessage = strings.ReplaceAll(teamMessage, "{githubUsername}", teamSlug) // Use team slug for {githubUsername} contextually
			teamMessage = strings.ReplaceAll(teamMessage, "{title}", pr.Title)
			teamMessage = strings.ReplaceAll(teamMessage, "{url}", pr.URL)
			teamMessage = strings.ReplaceAll(teamMessage, "{hours}", fmt.Sprintf("%d", cfg.Settings.ReminderThresholdHours))


			if dryRun {
				fmt.Printf("Would send to team channel %s (for team %s): %s\n", teamChannel, teamSlug, teamMessage)
			} else {
				err := slackClient.SendChannelMessage(teamChannel, teamMessage)
				if err != nil {
					slog.Error("Failed to send team notification to channel", "team_slug", teamSlug, "channel", teamChannel, "error", err)
					// Potentially continue to next team rather than returning error for the whole reviewer processing
					continue
				}
				slog.Info("Sent team notification", "pr", pr.Title, "team", teamSlug, "channel", teamChannel)
				if err := notificationTracker.RecordNotification(pr.URL, teamSlug); err != nil {
					slog.Error("Failed to record team notification", "team_slug", teamSlug, "error", err)
					// Potentially continue
				}
			}
		}
		return nil
	}
	// Handle other reviewer types or log if necessary
	slog.Warn("Unknown or unhandled reviewer type", "type", reviewer.Type, "login", reviewer.Login)
	return nil
}

// processPullRequest handles the notification logic for a single pull request.
func processPullRequest(
	ghClient *github.Client, // Added ghClient
	pr models.PullRequest,
	slackClient *slack.Client,
	notificationTracker *notification.Tracker,
	cfg *config.Config,
) {
	slog.Debug("Processing pull request", "title", pr.Title, "url", pr.URL)

	// Process each reviewer
	for _, reviewer := range pr.ReviewRequests {
		err := processReviewer(ghClient, pr, reviewer, slackClient, notificationTracker, cfg)
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
		// Pass ghClient to processPullRequest
		processPullRequest(githubClient, pr, slackClient, notificationTracker, cfg)
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
