// Package slack provides functionality to send notifications to Slack.
package slack

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/slack-go/slack"
)

// ChannelRouting defines a mapping from file pattern to Slack channel.
type ChannelRouting struct {
	Pattern string
	Channel string
}

// Client provides methods to interact with Slack.
type Client struct {
	api                *slack.Client
	userIDMapping      map[string]string
	dmChannelIDMapping map[string]string
	channelRouting     []ChannelRouting
	defaultChannel     string
}

// NewClient creates a new Slack client with the given token and mappings.
func NewClient(token string, userIDMapping, dmChannelIDMapping map[string]string) *Client {
	return &Client{
		api:                slack.New(token),
		userIDMapping:      userIDMapping,
		dmChannelIDMapping: dmChannelIDMapping,
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
func (c *Client) GetSlackUserIDForGitHubUser(githubUsername string) (string, bool) {
	slackID, ok := c.userIDMapping[githubUsername]
	return slackID, ok
}

// GetDMChannelIDForGitHubUser returns the Slack DM channel ID for a GitHub username.
func (c *Client) GetDMChannelIDForGitHubUser(githubUsername string) (string, bool) {
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
func (c *Client) FormatMessage(template string, pr models.PullRequest, githubUsername string, hours int) string {
	message := template

	// Replace {githubUsername} with the Github Username
	message = strings.ReplaceAll(message, "{githubUsername}", githubUsername)

	// Replace {title} with the PR title
	message = strings.ReplaceAll(message, "{title}", pr.Title)

	// Replace {url} with the PR URL
	message = strings.ReplaceAll(message, "{url}", pr.URL)

	// Replace {hours} with the number of hours
	message = strings.ReplaceAll(message, "{hours}", fmt.Sprintf("%d", hours))

	// Replace {slack_id} with the Slack user ID
	if slackID, ok := c.GetSlackUserIDForGitHubUser(githubUsername); ok {
		message = strings.ReplaceAll(message, "{slack_id}", slackID)
	} else {
		// If no mapping exists, just use the GitHub username
		message = strings.ReplaceAll(message, "<@{slack_id}>", githubUsername)
	}

	return message
}

// SendDirectMessage sends a direct message to a Slack user.
func (c *Client) SendDirectMessage(githubUsername, message string) error {
	// First try to get the DM channel ID
	if channelID, ok := c.GetDMChannelIDForGitHubUser(githubUsername); ok {
		_, _, err := c.api.PostMessage(channelID, slack.MsgOptionText(message, false))
		if err != nil {
			return fmt.Errorf("failed to send message to DM channel: %w", err)
		}
		return nil
	}

	// Fall back to user ID if DM channel ID is not available
	if slackUserID, ok := c.GetSlackUserIDForGitHubUser(githubUsername); ok {
		_, _, err := c.api.PostMessage(slackUserID, slack.MsgOptionText(message, false))
		if err != nil {
			return fmt.Errorf("failed to send direct message: %w", err)
		}
		return nil
	}

	return fmt.Errorf("no Slack ID or DM channel mapping for GitHub user: %s", githubUsername)
}

// SendChannelMessage sends a message to a Slack channel.
func (c *Client) SendChannelMessage(channel, message string) error {
	_, _, err := c.api.PostMessage(channel, slack.MsgOptionText(message, false))
	if err != nil {
		return fmt.Errorf("failed to send channel message: %w", err)
	}
	return nil
}

// SendDirectMessageWithDryRun sends a direct message to a GitHub user with
// dry-run support. If dryRun is true, it will return what would be sent without
// actually sending.
//
// Parameters:
//   - githubUsername: The GitHub username of the user to message
//   - message: The message content to send
//   - dryRun: If true, will not actually send the message
//
// Returns:
//   - destination: Where the message would be/was sent (DM channel ID or user ID)
//   - error: Any error that occurred during the process
func (c *Client) SendDirectMessageWithDryRun(githubUsername, message string, dryRun bool) (string, error) {
	var destination string

	// Check if we have a mapping for this GitHub user
	// Try to get DM channel ID first
	if dmChannelID, ok := c.GetDMChannelIDForGitHubUser(githubUsername); ok {
		destination = dmChannelID
	} else if slackUserID, ok := c.GetSlackUserIDForGitHubUser(githubUsername); ok {
		destination = slackUserID
	} else {
		return "", fmt.Errorf("no Slack mapping for GitHub user: %s", githubUsername)
	}

	// If dry run, just return what would be sent
	if dryRun {
		return destination, nil
	}

	// Send the actual message
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
//   - dryRun: If true, will return what would be sent without actually sending
//
// Returns:
//   - destination: Where the message would be/was sent (channel name or user ID)
//   - message: The formatted message content
//   - error: Any error that occurred during the process
func (c *Client) NudgeReviewer(pr models.PullRequest, githubUsername string, hours int, template string, dmByDefault bool, dryRun bool) (string, string, error) {
	message := c.FormatMessage(template, pr, githubUsername, hours)
	var destination string
	var err error

	// Determine the destination and send the message
	if dmByDefault {
		destination, err = c.SendDirectMessageWithDryRun(githubUsername, message, dryRun)
	} else {
		destination = c.GetChannelForPR(pr)

		// If dry run, just return what would be sent
		if dryRun {
			return destination, message, nil
		}

		err = c.SendChannelMessage(destination, message)
	}

	return destination, message, err
}
