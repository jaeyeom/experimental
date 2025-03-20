// Package main provides the gh-slack command-line utility.
//
// gh-slack is a tool for sending Slack messages to users based on their GitHub usernames.
// It leverages the same configuration as gh-nudge to map GitHub usernames to Slack user IDs
// or DM channel IDs.
//
// Usage:
//
//	# Send a simple message
//	gh-slack --to githubuser --message "Hello, this is a test message"
//
//	# Use a template with variables
//	gh-slack --to githubuser --template "Hey {slack_id}, just wanted to let you know about {topic}" --var "topic=the upcoming meeting"
//
//	# Dry run to preview the message without sending
//	gh-slack --to githubuser --message "Test message" --dry-run
//
//	# Send to multiple users
//	gh-slack --to user1,user2,user3 --message "Group announcement"
//
//	# Send with a custom config file
//	gh-slack --to githubuser --message "Test" --config /path/to/config.yaml
//
// Configuration:
//
// gh-slack uses the same configuration file as gh-nudge, typically located at
// $HOME/.config/gh-nudge/config.yaml. The relevant sections for Slack messaging are:
//
//	slack:
//	  token: "xoxb-your-slack-token"
//	  user_id_mapping:
//	    githubuser1: "U12345678"
//	    githubuser2: "U87654321"
//	  dm_channel_id_mapping:
//	    githubuser1: "D12345678"
//	    githubuser2: "D87654321"
package main

import (
	"flag"
	"fmt"
	"log/slog"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/slack"
)

var (
	configPath string
	dryRun     bool
	verbose    bool
	recipients string
	message    string
	template   string
	vars       string
)

func init() {
	flag.StringVar(&configPath, "config", "", "Path to configuration file")
	flag.BoolVar(&dryRun, "dry-run", false, "Run in dry-run mode (no messages sent)")
	flag.BoolVar(&verbose, "verbose", false, "Show verbose output")
	flag.StringVar(&recipients, "to", "", "GitHub usernames to send messages to (comma-separated)")
	flag.StringVar(&message, "message", "", "Message to send")
	flag.StringVar(&template, "template", "", "Message template to use")
	flag.StringVar(&vars, "var", "", "Variables for template in format key=value,key2=value2")
}

// setupLogger configures the application logger based on verbosity level
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

// parseVariables parses the variables string into a map
func parseVariables(varsStr string) map[string]string {
	result := make(map[string]string)
	if varsStr == "" {
		return result
	}

	pairs := strings.Split(varsStr, ",")
	for _, pair := range pairs {
		kv := strings.SplitN(pair, "=", 2)
		if len(kv) == 2 {
			result[kv[0]] = kv[1]
		}
	}
	return result
}

// formatMessage formats a message using the provided template and variables
func formatMessage(msgTemplate string, variables map[string]string) string {
	result := msgTemplate
	for key, value := range variables {
		result = strings.ReplaceAll(result, "{"+key+"}", value)
	}
	return result
}

// processMessage handles sending a message to a GitHub user
func processMessage(githubUsername, msgContent string, slackClient *slack.Client, dryRun bool) error {
	destination, err := slackClient.SendDirectMessageWithDryRun(githubUsername, msgContent, dryRun)

	if dryRun {
		if err != nil {
			return fmt.Errorf("would fail to send message: %w", err)
		}
		fmt.Printf("Would send to %s: %s\n", destination, msgContent)
		return nil
	}

	if err != nil {
		return fmt.Errorf("failed to send message: %w", err)
	}

	slog.Info("Message sent", "recipient", githubUsername, "destination", destination)
	return nil
}

func main() {
	flag.Parse()

	// Set up logging
	setupLogger()

	// Validate required parameters
	if recipients == "" {
		slog.Error("No recipients specified. Use --to flag to specify GitHub usernames")
		os.Exit(1)
	}

	if message == "" && template == "" {
		slog.Error("No message content. Use --message or --template flag")
		os.Exit(1)
	}

	// Load configuration
	cfg, err := config.LoadConfig(configPath)
	if err != nil {
		slog.Error("Failed to load configuration", "error", err)
		os.Exit(1)
	}

	// Initialize Slack client
	slackClient := slack.NewClient(cfg.Slack.Token, cfg.Slack.UserIDMapping, cfg.Slack.DMChannelIDMapping)

	// Parse variables for template
	variables := parseVariables(vars)

	// Determine message content
	var msgContent string
	if template != "" {
		msgContent = formatMessage(template, variables)
	} else {
		msgContent = message
	}

	// Process each recipient
	recipientList := strings.Split(recipients, ",")
	for _, recipient := range recipientList {
		recipient = strings.TrimSpace(recipient)
		if recipient == "" {
			continue
		}

		slog.Debug("Processing recipient", "github_username", recipient)
		err := processMessage(recipient, msgContent, slackClient, dryRun)
		if err != nil {
			slog.Error("Failed to send message", "recipient", recipient, "error", err)
		}
	}

	if dryRun {
		slog.Info("Dry run completed. No messages were actually sent")
	} else {
		slog.Info("All messages processed")
	}
}
