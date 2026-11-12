package approve

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// Decisions an agent command can print.
const (
	AgentApprove = "approve"
	AgentDefer   = "defer"
)

// agentCondition is the condition name of a rule's agent command.
const agentCondition = "agent_command"

// maxStderrInReason bounds how much of a failed command's stderr is kept.
const maxStderrInReason = 300

// AgentResult is what a rule's agent command decided. A failure is a defer
// with the error as the reason.
type AgentResult struct {
	Decision   string `json:"decision"`
	Reason     string `json:"reason"`
	DurationMs int64  `json:"durationMs"`
}

func (a AgentResult) String() string {
	duration := (time.Duration(a.DurationMs) * time.Millisecond).String()
	if a.Reason == "" {
		return fmt.Sprintf("%s (%s)", a.Decision, duration)
	}
	return fmt.Sprintf("%s: %s (%s)", a.Decision, a.Reason, duration)
}

// AgentPR is the pull request metadata in AgentInput.
type AgentPR struct {
	Repo         string   `json:"repo"`
	Number       int      `json:"number"`
	URL          string   `json:"url"`
	Title        string   `json:"title"`
	Author       string   `json:"author"`
	HeadSHA      string   `json:"headSha"`
	Draft        bool     `json:"draft"`
	Additions    int      `json:"additions"`
	Deletions    int      `json:"deletions"`
	ChangedFiles int      `json:"changedFiles"`
	Labels       []string `json:"labels"`
}

// AgentInput is the JSON object written to an agent command's stdin.
type AgentInput struct {
	// Rule is the name of the rule whose agent command runs.
	Rule  string        `json:"rule"`
	PR    AgentPR       `json:"pr"`
	Files []models.File `json:"files"`
	// FilesError is set when the file list could not be read completely.
	FilesError string `json:"filesError,omitempty"`
	Diff       string `json:"diff"`
}

func newAgentInput(pr github.ApprovalPullRequest, diff string) AgentInput {
	return AgentInput{
		PR: AgentPR{
			Repo:         pr.Ref.Owner + "/" + pr.Ref.Repo,
			Number:       pr.Ref.Number,
			URL:          pr.URL,
			Title:        pr.Title,
			Author:       pr.Author,
			HeadSHA:      pr.HeadSHA,
			Draft:        pr.Draft,
			Additions:    pr.Additions,
			Deletions:    pr.Deletions,
			ChangedFiles: pr.ChangedFiles,
			Labels:       pr.Labels,
		},
		Files:      pr.Files,
		FilesError: pr.FilesErr,
		Diff:       diff,
	}
}

// CommandRunner runs argv with stdin. When ctx ends it kills the command and
// sets Result.TimedOut. A non-nil error means the command did not start.
type CommandRunner func(ctx context.Context, stdin []byte, argv []string) (remotecall.Result, error)

// ExecCommand is the CommandRunner that runs a real process, without a
// shell, in its own process group. A timeout kills the whole group.
func ExecCommand(ctx context.Context, stdin []byte, argv []string) (remotecall.Result, error) {
	var stdout, stderr bytes.Buffer
	res, err := remotecall.RunIO(ctx, bytes.NewReader(stdin), &stdout, &stderr, argv[0], argv[1:]...)
	res.Stdout, res.Stderr = stdout.String(), stderr.String()
	if err != nil {
		return res, fmt.Errorf("start %s: %w", argv[0], err)
	}
	return res, nil
}

// runAgent runs rule's agent command with input on stdin. Every failure is a
// defer whose reason is the error.
func runAgent(run CommandRunner, rule config.ApproveRule, input AgentInput) AgentResult {
	input.Rule = rule.Name
	start := time.Now()
	decision, reason := agentDecision(run, rule, input)
	return AgentResult{Decision: decision, Reason: reason, DurationMs: time.Since(start).Milliseconds()}
}

func agentDecision(run CommandRunner, rule config.ApproveRule, input AgentInput) (string, string) {
	stdin, err := json.Marshal(input)
	if err != nil {
		return AgentDefer, "encode agent input: " + err.Error()
	}
	timeout := rule.AgentTimeout
	if timeout <= 0 {
		timeout = config.DefaultAgentTimeout
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	res, err := run(ctx, stdin, rule.AgentCommand)
	switch {
	case err != nil:
		return AgentDefer, "agent command: " + err.Error()
	case res.TimedOut:
		return AgentDefer, fmt.Sprintf("agent command timed out after %s", timeout)
	case res.Code != 0:
		return AgentDefer, fmt.Sprintf("agent command exited %d%s", res.Code, stderrSuffix(res.Stderr))
	}
	var out struct {
		Decision string `json:"decision"`
		Reason   string `json:"reason"`
	}
	if err := json.Unmarshal([]byte(strings.TrimSpace(res.Stdout)), &out); err != nil {
		return AgentDefer, "agent command output is not one JSON object: " + err.Error()
	}
	switch out.Decision {
	case AgentApprove, AgentDefer:
		return out.Decision, out.Reason
	default:
		return AgentDefer, fmt.Sprintf("agent command decision %q is not approve or defer", out.Decision)
	}
}

func stderrSuffix(stderr string) string {
	stderr = strings.TrimSpace(stderr)
	if stderr == "" {
		return ""
	}
	if len(stderr) > maxStderrInReason {
		stderr = "..." + stderr[len(stderr)-maxStderrInReason:]
	}
	return ": " + stderr
}
