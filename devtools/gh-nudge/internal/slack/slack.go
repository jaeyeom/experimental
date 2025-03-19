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

// NudgeReviewer sends a notification to a reviewer about a pending PR.
func (c *Client) NudgeReviewer(pr models.PullRequest, githubUsername string, hours int, template string, dmByDefault bool) error {
	message := c.FormatMessage(template, pr, githubUsername, hours)

	// Send as DM if configured to do so
	if dmByDefault {
		return c.SendDirectMessage(githubUsername, message)
	}

	// Otherwise, send to the appropriate channel
	channel := c.GetChannelForPR(pr)
	return c.SendChannelMessage(channel, message)
}
