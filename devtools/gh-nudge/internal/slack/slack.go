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
	api            *slack.Client
	userMapping    map[string]string
	channelRouting []ChannelRouting
	defaultChannel string
}

// NewClient creates a new Slack client with the given token and user mapping.
func NewClient(token string, userMapping map[string]string) *Client {
	return &Client{
		api:         slack.New(token),
		userMapping: userMapping,
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

// GetSlackIDForGitHubUser returns the Slack user ID for a GitHub username.
func (c *Client) GetSlackIDForGitHubUser(githubUsername string) (string, bool) {
	slackID, ok := c.userMapping[githubUsername]
	return slackID, ok
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

	// Replace {title} with the PR title
	message = strings.ReplaceAll(message, "{title}", pr.Title)

	// Replace {url} with the PR URL
	message = strings.ReplaceAll(message, "{url}", pr.URL)

	// Replace {hours} with the number of hours
	message = strings.ReplaceAll(message, "{hours}", fmt.Sprintf("%d", hours))

	// Replace {slack_id} with the Slack user ID
	if slackID, ok := c.GetSlackIDForGitHubUser(githubUsername); ok {
		message = strings.ReplaceAll(message, "{slack_id}", slackID)
	} else {
		// If no mapping exists, just use the GitHub username
		message = strings.ReplaceAll(message, "<@{slack_id}>", githubUsername)
	}

	return message
}

// SendDirectMessage sends a direct message to a Slack user.
func (c *Client) SendDirectMessage(slackUserID, message string) error {
	_, _, err := c.api.PostMessage(slackUserID, slack.MsgOptionText(message, false))
	if err != nil {
		return fmt.Errorf("failed to send direct message: %w", err)
	}
	return nil
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

	// Get the Slack user ID for the GitHub username
	slackID, ok := c.GetSlackIDForGitHubUser(githubUsername)
	if !ok {
		return fmt.Errorf("no Slack ID mapping for GitHub user: %s", githubUsername)
	}

	// Send as DM if configured to do so
	if dmByDefault {
		return c.SendDirectMessage(slackID, message)
	}

	// Otherwise, send to the appropriate channel
	channel := c.GetChannelForPR(pr)
	return c.SendChannelMessage(channel, message)
}
