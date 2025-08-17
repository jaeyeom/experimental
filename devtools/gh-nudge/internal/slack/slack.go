// Package slack provides functionality to send notifications to Slack.
package slack

import (
	"errors"
	"fmt"
	"log/slog"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/slack-go/slack"
)

// ErrDryRun is returned by DryRunMessagePoster to indicate a dry run operation.
var ErrDryRun = errors.New("dry run - message not actually sent")

// GitHubUsername represents a GitHub username.
type GitHubUsername string

// UserID represents a Slack user ID (e.g., "U12345").
type UserID string

// ChannelID represents a Slack channel ID (e.g., "C12345").
type ChannelID string

// UserIDMapping maps GitHub usernames to Slack user IDs.
type UserIDMapping map[GitHubUsername]UserID

// DMChannelIDMapping maps GitHub usernames to Slack DM channel IDs.
type DMChannelIDMapping map[GitHubUsername]ChannelID

// ChannelRouting defines a mapping from file pattern to Slack channel.
type ChannelRouting struct {
	Pattern string
	Channel string
}

// MessagePoster interface abstracts the message posting functionality.
type MessagePoster interface {
	// PostMessage sends a message to the specified channel with the given options.
	// It mirrors the signature of slack.Client.PostMessage for compatibility.
	//
	// Parameters:
	//   - channel: The channel ID or name to send the message to (e.g., "C1234567890", "#general", "@username")
	//   - options: Message options such as text content, attachments, blocks, etc.
	//
	// Returns:
	//   - string: The channel ID where the message was sent (useful when channel parameter was a name)
	//   - string: The message timestamp/ID that can be used to reference this message later
	//   - error: Any error that occurred during message posting
	//
	// For real implementations, this calls the actual Slack API.
	// For dry-run implementations, this simulates the posting and returns fake values.
	PostMessage(channel string, options ...slack.MsgOption) (string, string, error)
}

// DryRunMessagePoster simulates sending messages without actually sending them.
// It always returns ErrDryRun to naturally stop notification recording in dry-run mode.
type DryRunMessagePoster struct{}

// NewDryRunMessagePoster creates a new DryRunMessagePoster.
func NewDryRunMessagePoster() *DryRunMessagePoster {
	return &DryRunMessagePoster{}
}

// PostMessage simulates posting a message and always returns ErrDryRun.
func (d *DryRunMessagePoster) PostMessage(channel string, _ ...slack.MsgOption) (string, string, error) {
	slog.Info("DRY RUN: Would send message", "channel", channel)
	return "", "", ErrDryRun
}

// ClientConfig holds configuration options for creating a Slack client.
type ClientConfig struct {
	Token              string
	UserIDMapping      UserIDMapping
	DMChannelIDMapping DMChannelIDMapping
	MessagePoster      MessagePoster
}

// Client provides methods to interact with Slack.
type Client struct {
	messagePoster      MessagePoster
	userIDMapping      UserIDMapping
	dmChannelIDMapping DMChannelIDMapping
	channelRouting     []ChannelRouting
	defaultChannel     string
}

// NewClient creates a new Slack client with the given configuration.
func NewClient(config ClientConfig) *Client {
	var messagePoster MessagePoster
	if config.MessagePoster != nil {
		messagePoster = config.MessagePoster
	} else {
		// Default to real Slack client if none provided
		messagePoster = slack.New(config.Token)
	}

	return &Client{
		messagePoster:      messagePoster,
		userIDMapping:      config.UserIDMapping,
		dmChannelIDMapping: config.DMChannelIDMapping,
	}
}

// SetChannelRouting sets the channel routing configuration.
func (c *Client) SetChannelRouting(routing []ChannelRouting) {
	c.channelRouting = routing
}

// SetDefaultChannel sets the default channel for notifications.
func (c *Client) SetDefaultChannel(channel string) {
	c.defaultChannel = channel
}

// GetSlackUserIDForGitHubUser returns the Slack user ID for a GitHub username.
func (c *Client) GetSlackUserIDForGitHubUser(githubUsername GitHubUsername) (UserID, bool) {
	slackID, ok := c.userIDMapping[githubUsername]
	return slackID, ok
}

// GetDMChannelIDForGitHubUser returns the Slack DM channel ID for a GitHub username.
func (c *Client) GetDMChannelIDForGitHubUser(githubUsername GitHubUsername) (ChannelID, bool) {
	channelID, ok := c.dmChannelIDMapping[githubUsername]
	return channelID, ok
}

// GetChannelForPR determines the appropriate Slack channel for a pull request
// based on the files changed and channel routing configuration.
func (c *Client) GetChannelForPR(pr models.PullRequest) string {
	// Check each file against the channel routing patterns
	for _, file := range pr.Files {
		for _, route := range c.channelRouting {
			matched, err := regexp.MatchString(route.Pattern, file.Path)
			if err == nil && matched {
				return route.Channel
			}
		}
	}

	// If no pattern matches, return the default channel
	return c.defaultChannel
}

// FormatMessage formats a notification message using the provided template.
//
// Available template placeholders:
//   - {githubUsername}: GitHub username of the PR author
//   - {title}: Title of the pull request
//   - {url}: URL of the pull request
//   - {hours}: Number of hours since the PR was created
//   - {slack_id}: Slack user ID (when used as <@{slack_id}>, falls back to githubUsername if no mapping exists)
func (c *Client) FormatMessage(template string, pr models.PullRequest, githubUsername GitHubUsername, hours int) string {
	message := template

	// Replace {githubUsername} with the Github Username
	message = strings.ReplaceAll(message, "{githubUsername}", string(githubUsername))

	// Replace {title} with the PR title
	message = strings.ReplaceAll(message, "{title}", pr.Title)

	// Replace {url} with the PR URL
	message = strings.ReplaceAll(message, "{url}", pr.URL)

	// Replace {hours} with the number of hours
	message = strings.ReplaceAll(message, "{hours}", fmt.Sprintf("%d", hours))

	// Replace {slack_id} with the Slack user ID
	if slackID, ok := c.GetSlackUserIDForGitHubUser(githubUsername); ok {
		message = strings.ReplaceAll(message, "{slack_id}", string(slackID))
	} else {
		// If no mapping exists, just use the GitHub username
		message = strings.ReplaceAll(message, "<@{slack_id}>", string(githubUsername))
	}

	return message
}

// SendDirectMessage sends a direct message to a Slack user.
func (c *Client) SendDirectMessage(githubUsername GitHubUsername, message string) error {
	// First try to get the DM channel ID
	if channelID, ok := c.GetDMChannelIDForGitHubUser(githubUsername); ok {
		_, _, err := c.messagePoster.PostMessage(string(channelID), slack.MsgOptionText(message, false))
		if err != nil {
			return fmt.Errorf("failed to send message to DM channel: %w", err)
		}
		slog.Info("Sent direct message to Slack user via DM channel", "github_username", githubUsername, "channel_id", channelID)
		return nil
	}

	// Fall back to user ID if DM channel ID is not available
	if slackUserID, ok := c.GetSlackUserIDForGitHubUser(githubUsername); ok {
		_, _, err := c.messagePoster.PostMessage(string(slackUserID), slack.MsgOptionText(message, false))
		if err != nil {
			return fmt.Errorf("failed to send direct message: %w", err)
		}
		slog.Info("Sent direct message to Slack user via user ID", "github_username", githubUsername, "user_id", slackUserID)
		return nil
	}

	return fmt.Errorf("no Slack ID or DM channel mapping for GitHub user: %s", githubUsername)
}

// SendChannelMessage sends a message to a Slack channel.
func (c *Client) SendChannelMessage(channel, message string) error {
	_, _, err := c.messagePoster.PostMessage(channel, slack.MsgOptionText(message, false))
	if err != nil {
		return fmt.Errorf("failed to send channel message: %w", err)
	}
	slog.Info("Sent message to Slack channel", "channel", channel)
	return nil
}

// SendDirectMessageWithDestination sends a direct message to a GitHub user and returns the destination.
//
// Parameters:
//   - githubUsername: The GitHub username of the user to message
//   - message: The message content to send
//
// Returns:
//   - destination: Where the message was sent (DM channel ID or user ID)
//   - error: Any error that occurred during the process
func (c *Client) SendDirectMessageWithDestination(githubUsername GitHubUsername, message string) (string, error) {
	var destination string

	// Check if we have a mapping for this GitHub user
	// Try to get DM channel ID first
	if dmChannelID, ok := c.GetDMChannelIDForGitHubUser(githubUsername); ok {
		destination = string(dmChannelID)
	} else if slackUserID, ok := c.GetSlackUserIDForGitHubUser(githubUsername); ok {
		destination = string(slackUserID)
	} else {
		return "", fmt.Errorf("no Slack mapping for GitHub user: %s", githubUsername)
	}

	// Send the message
	err := c.SendDirectMessage(githubUsername, message)
	return destination, err
}

// NudgeReviewer sends a notification to a reviewer about a pending PR.
// It formats a message using the provided template and sends it to the appropriate
// destination (DM or channel) based on the dmByDefault parameter.
//
// Parameters:
//   - pr: The pull request to notify about
//   - githubUsername: The GitHub username of the reviewer to notify
//   - hours: The number of hours the PR has been waiting for review (used in the message template)
//   - template: The message template to use for formatting the notification
//   - dmByDefault: Whether to send as a direct message (true) or to a channel (false)
//
// Returns:
//   - destination: Where the message was sent (channel name or user ID)
//   - message: The formatted message content
//   - error: Any error that occurred during the process
func (c *Client) NudgeReviewer(pr models.PullRequest, githubUsername GitHubUsername, hours int, template string, dmByDefault bool) (string, string, error) {
	message := c.FormatMessage(template, pr, githubUsername, hours)
	var destination string
	var err error

	// Determine the destination and send the message
	if dmByDefault {
		destination, err = c.SendDirectMessageWithDestination(githubUsername, message)
	} else {
		destination = c.GetChannelForPR(pr)
		err = c.SendChannelMessage(destination, message)
	}

	// ErrDryRun is expected and indicates successful dry-run operation
	if errors.Is(err, ErrDryRun) {
		return destination, message, nil
	}

	return destination, message, err
}
