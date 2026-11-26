package prreview

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

const outboxCommandTimeout = 2 * time.Minute

// OutboxLineComment is a single line comment passed to gh-outbox submit.
type OutboxLineComment struct {
	Path string
	Line int
	Text string
}

// OutboxSubmitRequest describes a draft to submit to gh-outbox.
type OutboxSubmitRequest struct {
	Repo  string
	PR    int
	Owner string
	Kind  string
	Body  string
	Lines []OutboxLineComment
	Ref   string
}

// OutboxDraftStatus describes the state of a draft returned by gh-outbox show --json.
type OutboxDraftStatus struct {
	ID    int64  `json:"id"`
	State string `json:"state"`
}

// OutboxClient submits drafts to gh-outbox and queries draft status.
type OutboxClient interface {
	Submit(ctx context.Context, req OutboxSubmitRequest) (draftID string, err error)
	Show(ctx context.Context, draftID string) (OutboxDraftStatus, error)
}

// OutboxError wraps a failure from invoking gh-outbox, preserving its stderr
// output (such as a lint refusal) verbatim.
type OutboxError struct {
	Stderr string
	Err    error
}

// Error returns the trimmed stderr output if present, or the underlying error.
func (e *OutboxError) Error() string {
	if s := strings.TrimSpace(e.Stderr); s != "" {
		return s
	}
	return e.Err.Error()
}

// Unwrap returns the underlying execution error.
func (e *OutboxError) Unwrap() error { return e.Err }

// CommandRunner executes argv and returns stdout, stderr, and any error.
type CommandRunner func(ctx context.Context, argv []string) (stdout, stderr []byte, err error)

// CLIOutboxClient invokes gh-outbox via a configured argv prefix.
type CLIOutboxClient struct {
	Command []string
	Run     CommandRunner
}

// NewCLIOutboxClient returns a CLIOutboxClient running command (default ["gh-outbox"]).
func NewCLIOutboxClient(command []string) *CLIOutboxClient {
	return &CLIOutboxClient{
		Command: slices.Clone(command),
		Run:     execCommandRunner,
	}
}

func (c *CLIOutboxClient) effectiveCommand() []string {
	if len(c.Command) == 0 {
		return []string{"gh-outbox"}
	}
	return slices.Clone(c.Command)
}

func (c *CLIOutboxClient) runner() CommandRunner {
	if c.Run != nil {
		return c.Run
	}
	return execCommandRunner
}

func execCommandRunner(ctx context.Context, argv []string) ([]byte, []byte, error) {
	if len(argv) == 0 || argv[0] == "" {
		return nil, nil, errors.New("outbox command must not be empty")
	}
	ctx, cancel := context.WithTimeout(ctx, outboxCommandTimeout)
	defer cancel()
	// argv comes from the user's own config.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: user config.
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	cmd.WaitDelay = time.Second
	err := cmd.Run()
	return stdout.Bytes(), stderr.Bytes(), err
}

// Submit writes body/line text files into a temporary directory, invokes
// `<command> submit ...`, and returns the printed draft ID.
func (c *CLIOutboxClient) Submit(ctx context.Context, req OutboxSubmitRequest) (string, error) {
	tmpDir, err := os.MkdirTemp("", "gh-pr-review-outbox-*")
	if err != nil {
		return "", fmt.Errorf("create temp dir for outbox draft: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	argv := append(c.effectiveCommand(),
		"submit",
		"--repo", req.Repo,
		"--pr", strconv.Itoa(req.PR),
		"--owner", req.Owner,
		"--kind", req.Kind,
	)

	if req.Body != "" {
		bodyFile := filepath.Join(tmpDir, "body.txt")
		if err := os.WriteFile(bodyFile, []byte(req.Body), 0o600); err != nil {
			return "", fmt.Errorf("write outbox body file: %w", err)
		}
		argv = append(argv, "--body-file", bodyFile)
	}

	for i, line := range req.Lines {
		lineFile := filepath.Join(tmpDir, fmt.Sprintf("line-%d.txt", i))
		if err := os.WriteFile(lineFile, []byte(line.Text), 0o600); err != nil {
			return "", fmt.Errorf("write outbox line file: %w", err)
		}
		argv = append(argv, "--line", fmt.Sprintf("%s:%d=%s", line.Path, line.Line, lineFile))
	}

	if req.Ref != "" {
		argv = append(argv, "--ref", req.Ref)
	}

	stdout, stderr, err := c.runner()(ctx, argv)
	if err != nil {
		return "", &OutboxError{Stderr: string(stderr), Err: err}
	}

	draftID := strings.TrimSpace(string(stdout))
	if draftID == "" {
		return "", errors.New("gh-outbox submit printed no draft id")
	}
	return draftID, nil
}

// Show queries `<command> show --json <draftID>` and decodes its status.
func (c *CLIOutboxClient) Show(ctx context.Context, draftID string) (OutboxDraftStatus, error) {
	argv := append(c.effectiveCommand(), "show", "--json", draftID)
	stdout, stderr, err := c.runner()(ctx, argv)
	if err != nil {
		return OutboxDraftStatus{}, &OutboxError{Stderr: string(stderr), Err: err}
	}
	var status OutboxDraftStatus
	if err := json.Unmarshal(stdout, &status); err != nil {
		return OutboxDraftStatus{}, fmt.Errorf("parse gh-outbox show output: %w", err)
	}
	return status, nil
}

// OutboxKindForEvent maps a gh-pr-review --event value to a gh-outbox --kind value.
func OutboxKindForEvent(event string) (string, error) {
	switch event {
	case "":
		return "thread", nil
	case "COMMENT":
		return "comment", nil
	case "REQUEST_CHANGES":
		return "request-changes", nil
	case "APPROVE":
		return "approve", nil
	default:
		return "", fmt.Errorf("invalid event %q", event)
	}
}

// ReconcileOutbox checks any local comments marked with an outbox draft ID
// against gh-outbox and applies their deferred --after action once posted.
func (ch *CommandHandler) ReconcileOutbox(ctx context.Context, repository models.Repository, target models.ReviewTarget) error {
	if ch.outboxClient == nil {
		return nil
	}

	reviewComments, err := ch.storage.GetComments(repository, target)
	if err != nil {
		return fmt.Errorf("failed to get comments: %w", err)
	}

	draftIDs := uniqueOutboxDraftIDs(reviewComments.Comments)
	if len(draftIDs) == 0 {
		return nil
	}

	states := make(map[string]string, len(draftIDs))
	for _, id := range draftIDs {
		status, err := ch.outboxClient.Show(ctx, id)
		if err != nil {
			continue
		}
		states[id] = status.State
	}
	if len(states) == 0 {
		return nil
	}

	return ch.applyOutboxStates(repository, target, reviewComments, states)
}

func uniqueOutboxDraftIDs(comments []models.Comment) []string {
	seen := make(map[string]bool)
	var ids []string
	for _, c := range comments {
		if c.OutboxDraftID != "" && !seen[c.OutboxDraftID] {
			seen[c.OutboxDraftID] = true
			ids = append(ids, c.OutboxDraftID)
		}
	}
	return ids
}

func (ch *CommandHandler) applyOutboxStates(
	repository models.Repository,
	target models.ReviewTarget,
	reviewComments *models.ReviewComments,
	states map[string]string,
) error {
	remaining, toArchive, clearedCount, changed := partitionReconciledComments(reviewComments.Comments, states)
	if !changed {
		return nil
	}
	return ch.persistReconciledComments(repository, target, reviewComments, remaining, toArchive, clearedCount)
}

func partitionReconciledComments(
	comments []models.Comment,
	states map[string]string,
) (remaining []models.Comment, toArchive []models.Comment, clearedCount int, changed bool) {
	for _, c := range comments {
		state, ok := states[c.OutboxDraftID]
		if c.OutboxDraftID == "" || !ok {
			remaining = append(remaining, c)
			continue
		}
		switch state {
		case "posted":
			changed = true
			switch strings.ToLower(c.OutboxAfter) {
			case "keep":
				c.OutboxDraftID = ""
				c.OutboxAfter = ""
				remaining = append(remaining, c)
			case "archive":
				toArchive = append(toArchive, c)
			default:
				clearedCount++
			}
		case "cancelled", "needs-redraft":
			changed = true
			c.OutboxDraftID = ""
			c.OutboxAfter = ""
			remaining = append(remaining, c)
		default:
			remaining = append(remaining, c)
		}
	}
	return remaining, toArchive, clearedCount, changed
}

func (ch *CommandHandler) persistReconciledComments(
	repository models.Repository,
	target models.ReviewTarget,
	reviewComments *models.ReviewComments,
	remaining []models.Comment,
	toArchive []models.Comment,
	clearedCount int,
) error {
	if len(remaining) == 0 && len(toArchive) > 0 && clearedCount == 0 {
		if _, err := ch.storage.ArchiveComments(repository, target, "", "COMMENT"); err != nil {
			return fmt.Errorf("failed to archive posted outbox comments: %w", err)
		}
		return nil
	}
	if len(remaining) == 0 && len(toArchive) == 0 {
		if err := ch.storage.ClearComments(repository, target, nil); err != nil {
			return fmt.Errorf("failed to clear posted outbox comments: %w", err)
		}
		return nil
	}
	for _, c := range toArchive {
		c.Archive("posted via gh-outbox draft " + c.OutboxDraftID)
		c.OutboxDraftID = ""
		c.OutboxAfter = ""
		remaining = append(remaining, c)
	}
	if remaining == nil {
		remaining = []models.Comment{}
	}
	reviewComments.Comments = remaining
	if err := ch.storage.UpdateComments(repository, target, reviewComments); err != nil {
		return fmt.Errorf("failed to update comments after outbox reconcile: %w", err)
	}
	return nil
}
