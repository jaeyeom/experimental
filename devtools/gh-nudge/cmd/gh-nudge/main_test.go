package main

import (
	"fmt"
	"log/slog"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/codeowners"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// --- Mock Implementations ---

// mockGithubClient mocks internal/github.Client
type mockGithubClient struct {
	GetFileContentFunc func(owner, repo, path, ref string) (string, error)
}

func (m *mockGithubClient) GetFileContent(owner, repo, path, ref string) (string, error) {
	if m.GetFileContentFunc != nil {
		return m.GetFileContentFunc(owner, repo, path, ref)
	}
	return "", fmt.Errorf("GetFileContentFunc not set")
}
// Add other methods if processReviewer starts using them, e.g., GetPendingPullRequests (though not directly used by processReviewer itself)
func (m *mockGithubClient) GetPendingPullRequests() ([]models.PullRequest, error) { return nil, nil }
func (m *mockGithubClient) GetMergeablePullRequests() ([]models.PullRequest, error) { return nil, nil }
func (m *mockGithubClient) FilterPullRequestsByAge(prs []models.PullRequest, hours int) []models.PullRequest { return nil }
func (m *mockGithubClient) GetPullRequestDetails(pr models.PullRequest) (models.PullRequest, error) { return pr, nil }
func (m *mockGithubClient) MergePullRequest(prURL string, deleteBranch bool) error { return nil }


// mockSlackClient mocks internal/slack.Client
type mockSlackClient struct {
	GetChannelForTeamFunc  func(teamName string) (string, bool)
	SendChannelMessageFunc func(channel, message string) error
	NudgeReviewerFunc      func(pr models.PullRequest, githubUsername string, hours int, template string, dmByDefault bool, dryRun bool) (string, string, error)
	GetSlackUserIDForGitHubUserFunc func(githubUsername string) (string, bool)
}

func (m *mockSlackClient) GetChannelForTeam(teamName string) (string, bool) {
	if m.GetChannelForTeamFunc != nil {
		return m.GetChannelForTeamFunc(teamName)
	}
	return "", false
}

func (m *mockSlackClient) SendChannelMessage(channel, message string) error {
	if m.SendChannelMessageFunc != nil {
		return m.SendChannelMessageFunc(channel, message)
	}
	return fmt.Errorf("SendChannelMessageFunc not set")
}
// Add other methods if processReviewer starts using them
func (m *mockSlackClient) SetChannelRouting(routing []config.ChannelRoutingConfig) {} // Convert and set
func (m *mockSlackClient) SetDefaultChannel(channel string) {}
func (m *mockSlackClient) GetSlackUserIDForGitHubUser(githubUsername string) (string, bool) {
	if m.GetSlackUserIDForGitHubUserFunc != nil {
		return m.GetSlackUserIDForGitHubUserFunc(githubUsername)
	}
	return "", false
}
func (m *mockSlackClient) GetDMChannelIDForGitHubUser(githubUsername string) (string, bool) { return "", false }
func (m *mockSlackClient) GetChannelForPR(pr models.PullRequest) string { return "" }
func (m *mockSlackClient) FormatMessage(template string, pr models.PullRequest, githubUsername string, hours int) string { return "" }
func (m *mockSlackClient) SendDirectMessage(githubUsername, message string) error { return nil }
func (m *mockSlackClient) SendDirectMessageWithDryRun(githubUsername, message string, dryRun bool) (string, error) { return "", nil }
func (m *mockSlackClient) NudgeReviewer(pr models.PullRequest, githubUsername string, hours int, template string, dmByDefault bool, dryRun bool) (string, string, error) {
	if m.NudgeReviewerFunc != nil {
		return m.NudgeReviewerFunc(pr, githubUsername, hours, template, dmByDefault, dryRun)
	}
	return "", "", fmt.Errorf("NudgeReviewerFunc not set")
}


// mockNotificationTracker mocks internal/notification.Tracker
type mockNotificationTracker struct {
	ShouldNotifyFunc        func(prURL, recipient string, thresholdHours int) bool
	RecordNotificationFunc  func(prURL, recipient string) error
}

func (m *mockNotificationTracker) ShouldNotify(prURL, recipient string, thresholdHours int) bool {
	if m.ShouldNotifyFunc != nil {
		return m.ShouldNotifyFunc(prURL, recipient, thresholdHours)
	}
	return true // Default to should notify
}

func (m *mockNotificationTracker) RecordNotification(prURL, recipient string) error {
	if m.RecordNotificationFunc != nil {
		return m.RecordNotificationFunc(prURL, recipient)
	}
	return nil // Default to success
}
func (m *mockNotificationTracker) Save() error { return nil }


// --- Test Setup ---
func setupProcessReviewerTest(t *testing.T) (*config.Config, *models.PullRequest, models.ReviewRequest) {
	// Setup logger to avoid nil panics if slog is used directly in tested functions
	slog.SetDefault(slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: slog.LevelDebug})))

	cfg := &config.Config{
		Settings: config.SettingsConfig{
			ReminderThresholdHours: 24,
			MessageTemplate:        "Hey {recipient}, PR '{title}' from {repo} needs review after {hours}h. URL: {url}",
		},
		Slack: config.SlackConfig{}, // Add mappings if needed per test
	}

	pr := models.PullRequest{
		URL:         "https://github.com/test-owner/test-repo/pull/1",
		Title:       "Test PR for Teams",
		BaseRefName: "main",
		Files: []models.File{
			{Path: "src/component.js"},
			{Path: "docs/README.md"},
		},
	}

	teamReviewer := models.ReviewRequest{
		Type:  "Team",
		Login: "test-team", // This is team-slug
	}

	// Reset dryRun for each test, can be overridden in specific tests
	dryRun = false

	return cfg, &pr, teamReviewer
}

// --- Tests for processReviewer (Team Reviewers) ---

func TestProcessReviewer_Team_CodeownersFound_ChannelMapped(t *testing.T) {
	cfg, pr, reviewer := setupProcessReviewerTest(t)

	mockGH := &mockGithubClient{
		GetFileContentFunc: func(owner, repo, path, ref string) (string, error) {
			if path == ".github/CODEOWNERS" && ref == "main" {
				return "src/component.js @test-org/test-team\ndocs/README.md @test-org/docs-team", nil
			}
			return "", fmt.Errorf("file not found")
		},
	}
	var sentMessageToChannel string
	var sentToChannel string
	mockSlack := &mockSlackClient{
		GetChannelForTeamFunc: func(teamName string) (string, bool) {
			if teamName == "test-team" {
				return "#test-team-channel", true
			}
			return "", false
		},
		SendChannelMessageFunc: func(channel, message string) error {
			sentToChannel = channel
			sentMessageToChannel = message
			return nil
		},
	}
	mockTracker := &mockNotificationTracker{
		ShouldNotifyFunc: func(prURL, recipient string, thresholdHours int) bool { return true },
		RecordNotificationFunc: func(prURL, recipient string) error { return nil },
	}

	err := processReviewer(mockGH, *pr, reviewer, mockSlack, mockTracker, cfg)
	if err != nil {
		t.Fatalf("processReviewer failed: %v", err)
	}

	if sentToChannel != "#test-team-channel" {
		t.Errorf("Expected message to be sent to '#test-team-channel', got '%s'", sentToChannel)
	}
	if !strings.Contains(sentMessageToChannel, "@test-team team") {
		t.Errorf("Expected message to mention '@test-team team', got '%s'", sentMessageToChannel)
	}
	if !strings.Contains(sentMessageToChannel, "Test PR for Teams") {
		t.Errorf("Expected message to contain PR title, got '%s'", sentMessageToChannel)
	}
}

func TestProcessReviewer_Team_CodeownersFound_TeamNotInFiles_FallbackToAssigned(t *testing.T) {
	cfg, pr, reviewer := setupProcessReviewerTest(t)
	reviewer.Login = "assigned-team" // This is the team explicitly assigned for review

	mockGH := &mockGithubClient{
		GetFileContentFunc: func(owner, repo, path, ref string) (string, error) {
			if path == ".github/CODEOWNERS" && ref == "main" {
				// "assigned-team" is NOT in this CODEOWNERS file for these paths
				return "src/component.js @test-org/other-team\ndocs/README.md @test-org/another-team", nil
			}
			return "", fmt.Errorf("file not found")
		},
	}
	var sentToChannel string
	mockSlack := &mockSlackClient{
		GetChannelForTeamFunc: func(teamName string) (string, bool) {
			if teamName == "assigned-team" { // Check for the explicitly assigned team
				return "#assigned-team-channel", true
			}
			return "", false
		},
		SendChannelMessageFunc: func(channel, message string) error {
			sentToChannel = channel
			return nil
		},
	}
	mockTracker := &mockNotificationTracker{
		ShouldNotifyFunc: func(prURL, recipient string, thresholdHours int) bool { return true },
		RecordNotificationFunc: func(prURL, recipient string) error { return nil },
	}

	err := processReviewer(mockGH, *pr, reviewer, mockSlack, mockTracker, cfg)
	if err != nil {
		t.Fatalf("processReviewer failed: %v", err)
	}

	if sentToChannel != "#assigned-team-channel" {
		t.Errorf("Expected message to be sent to '#assigned-team-channel' (fallback), got '%s'", sentToChannel)
	}
}

func TestProcessReviewer_Team_CodeownersFound_TeamInFiles_NoChannelMap(t *testing.T) {
	cfg, pr, reviewer := setupProcessReviewerTest(t) // reviewer.Login is "test-team"

	mockGH := &mockGithubClient{
		GetFileContentFunc: func(owner, repo, path, ref string) (string, error) {
			if path == ".github/CODEOWNERS" && ref == "main" {
				return "src/component.js @test-org/test-team", nil // test-team owns a file
			}
			return "", fmt.Errorf("file not found")
		},
	}
	var messageSent bool
	mockSlack := &mockSlackClient{
		GetChannelForTeamFunc: func(teamName string) (string, bool) {
			// "test-team" has files, but no channel mapping for it
			return "", false
		},
		SendChannelMessageFunc: func(channel, message string) error {
			messageSent = true
			return nil
		},
	}
	mockTracker := &mockNotificationTracker{ ShouldNotifyFunc: func(prURL, recipient string, thresholdHours int) bool { return true } }

	err := processReviewer(mockGH, *pr, reviewer, mockSlack, mockTracker, cfg)
	if err != nil {
		t.Fatalf("processReviewer failed: %v", err)
	}

	if messageSent {
		t.Error("Expected no message to be sent as team channel is not mapped")
	}
}


func TestProcessReviewer_Team_CodeownersNotFound_FallbackToAssigned(t *testing.T) {
	cfg, pr, reviewer := setupProcessReviewerTest(t)
	reviewer.Login = "assigned-team-no-codeowners"

	mockGH := &mockGithubClient{
		GetFileContentFunc: func(owner, repo, path, ref string) (string, error) {
			return "", fmt.Errorf("file not found") // Simulate CODEOWNERS not found
		},
	}
	var sentToChannel string
	mockSlack := &mockSlackClient{
		GetChannelForTeamFunc: func(teamName string) (string, bool) {
			if teamName == "assigned-team-no-codeowners" {
				return "#assigned-fallback-channel", true
			}
			return "", false
		},
		SendChannelMessageFunc: func(channel, message string) error {
			sentToChannel = channel
			return nil
		},
	}
	mockTracker := &mockNotificationTracker{ ShouldNotifyFunc: func(prURL, recipient string, thresholdHours int) bool { return true } }

	err := processReviewer(mockGH, *pr, reviewer, mockSlack, mockTracker, cfg)
	if err != nil {
		t.Fatalf("processReviewer failed: %v", err)
	}

	if sentToChannel != "#assigned-fallback-channel" {
		t.Errorf("Expected message to be sent to '#assigned-fallback-channel', got '%s'", sentToChannel)
	}
}

func TestProcessReviewer_Team_EmptyBaseRef_UsesMain(t *testing.T) {
	cfg, pr, reviewer := setupProcessReviewerTest(t)
	pr.BaseRefName = "" // Simulate empty BaseRefName

	var usedRefForCodeowners string
	mockGH := &mockGithubClient{
		GetFileContentFunc: func(owner, repo, path, ref string) (string, error) {
			if path == ".github/CODEOWNERS" {
				usedRefForCodeowners = ref // Capture the ref used
				return "src/component.js @test-org/test-team", nil
			}
			return "", fmt.Errorf("file not found")
		},
	}
	mockSlack := &mockSlackClient{ // Basic setup, not the focus of this test
		GetChannelForTeamFunc:  func(teamName string) (string, bool) { return "#some-channel", true },
		SendChannelMessageFunc: func(channel, message string) error { return nil },
	}
	mockTracker := &mockNotificationTracker{ ShouldNotifyFunc: func(prURL, recipient string, thresholdHours int) bool { return true } }

	// Mute slog warnings for this specific test run to avoid clutter from the "defaulting to main" warning
	originalHandler := slog.Default().Handler()
	slog.SetDefault(slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: slog.LevelError}))) // Temporarily raise level
	defer slog.SetDefault(slog.New(originalHandler)) // Restore original handler


	err := processReviewer(mockGH, *pr, reviewer, mockSlack, mockTracker, cfg)
	if err != nil {
		t.Fatalf("processReviewer failed: %v", err)
	}

	if usedRefForCodeowners != "main" {
		t.Errorf("Expected CODEOWNERS to be fetched with ref 'main', but got '%s'", usedRefForCodeowners)
	}
}

func TestProcessReviewer_Team_DryRun(t *testing.T) {
	cfg, pr, reviewer := setupProcessReviewerTest(t)
	dryRun = true // Enable dryRun mode

	mockGH := &mockGithubClient{
		GetFileContentFunc: func(owner, repo, path, ref string) (string, error) {
			return "src/component.js @test-org/test-team", nil
		},
	}
	var messageSent bool
	mockSlack := &mockSlackClient{
		GetChannelForTeamFunc: func(teamName string) (string, bool) {
			if teamName == "test-team" { return "#test-team-dryrun-channel", true }
			return "", false
		},
		SendChannelMessageFunc: func(channel, message string) error {
			messageSent = true // Should not be called in dry run
			return nil
		},
	}
	mockTracker := &mockNotificationTracker{ ShouldNotifyFunc: func(prURL, recipient string, thresholdHours int) bool { return true } }

	// Capture stdout to check dry-run output (if your dry-run prints)
	// For this test, we'll rely on SendChannelMessageFunc not being called.
	// If dry-run prints, you'd use a pipe to capture os.Stdout.

	err := processReviewer(mockGH, *pr, reviewer, mockSlack, mockTracker, cfg)
	if err != nil {
		t.Fatalf("processReviewer failed in dryRun mode: %v", err)
	}

	if messageSent {
		t.Error("SendChannelMessage should not be called in dryRun mode")
	}
	// Add assertion here if your dryRun mode prints to stdout and you capture it.
	// For example, check for "Would send to team channel #test-team-dryrun-channel..."
}

func TestProcessReviewer_Team_ShouldNotNotify(t *testing.T) {
	cfg, pr, reviewer := setupProcessReviewerTest(t)

	mockGH := &mockGithubClient{
		GetFileContentFunc: func(owner, repo, path, ref string) (string, error) {
			return "src/component.js @test-org/test-team", nil
		},
	}
	var messageSent bool
	mockSlack := &mockSlackClient{
		GetChannelForTeamFunc: func(teamName string) (string, bool) { return "#channel", true },
		SendChannelMessageFunc: func(channel, message string) error {
			messageSent = true
			return nil
		},
	}
	mockTracker := &mockNotificationTracker{
		ShouldNotifyFunc: func(prURL, recipient string, thresholdHours int) bool {
			// Specifically for "test-team" (which is reviewer.Login and also in CODEOWNERS)
			if recipient == "test-team" { return false }
			return true
		},
	}

	err := processReviewer(mockGH, *pr, reviewer, mockSlack, mockTracker, cfg)
	if err != nil {
		t.Fatalf("processReviewer failed: %v", err)
	}

	if messageSent {
		t.Error("Expected no message to be sent when ShouldNotify returns false for the team")
	}
}


// Example test for user reviewer, ensuring it still works
func TestProcessReviewer_User_DMByDefault(t *testing.T) {
	cfg, pr, _ := setupProcessReviewerTest(t) // reviewer is discarded, we make a new one
	cfg.Settings.DMByDefault = true

	userReviewer := models.ReviewRequest{Type: "User", Login: "test-user"}
	pr.ReviewRequests = []models.ReviewRequest{userReviewer}


	mockGH := &mockGithubClient{} // Not used for user notifications directly in processReviewer

	var nudgeReviewerCalled bool
	var nudgeDryRun bool
	var nudgeDmByDefault bool

	mockSlack := &mockSlackClient{
		GetSlackUserIDForGitHubUserFunc: func(githubUsername string) (string, bool) {
			if githubUsername == "test-user" { return "U123", true}
			return "", false
		},
		NudgeReviewerFunc: func(p models.PullRequest, githubUsername string, hours int, template string, dmByDefault bool, dryRun bool) (string, string, error) {
			nudgeReviewerCalled = true
			nudgeDryRun = dryRun
			nudgeDmByDefault = dmByDefault
			if githubUsername == "test-user" {
				return "DM_DESTINATION_USER", "formatted_message_user", nil
			}
			return "", "", fmt.Errorf("unexpected user")
		},
	}

	recordCalledForUser := ""
	mockTracker := &mockNotificationTracker{
		ShouldNotifyFunc: func(prURL, recipient string, thresholdHours int) bool { return true } ,
		RecordNotificationFunc: func(prURL, recipient string) error {
			recordCalledForUser = recipient
			return nil
		},
	}

	err := processReviewer(mockGH, *pr, userReviewer, mockSlack, mockTracker, cfg)
	if err != nil {
		t.Fatalf("processReviewer for User failed: %v", err)
	}

	if !nudgeReviewerCalled {
		t.Error("Expected NudgeReviewer to be called for user reviewer")
	}
	if !nudgeDmByDefault {
		t.Error("Expected DMByDefault to be true when passed to NudgeReviewer")
	}
	if nudgeDryRun { // dryRun is global, ensure it's false for this test by default
		t.Error("Expected dryRun to be false for this specific test")
	}
	if recordCalledForUser != "test-user" {
		t.Errorf("Expected RecordNotification to be called for 'test-user', got '%s'", recordCalledForUser)
	}
}

// Minimal main func to satisfy the compiler for 'package main'.
func main() {
	// This is not run during 'go test'.
	// It's here because files in 'package main' typically have a 'main' function.
	// The actual application entry point is in the original main.go.
	fmt.Println("Test main func - not executed during tests")
	os.Exit(1)
}
