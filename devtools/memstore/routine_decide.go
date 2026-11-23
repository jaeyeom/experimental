package memstore

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// DecideRequest describes an approval request to submit to devtools/decide.
type DecideRequest struct {
	Owner    string `json:"owner"`
	Parent   string `json:"parent,omitempty"`
	Question string `json:"question"`
	Verbatim string `json:"verbatim"`
}

// DecideSubmitter submits approval requests and returns a decide item ID.
type DecideSubmitter interface {
	Submit(ctx context.Context, req DecideRequest) (decideID string, err error)
}

// ExecDecideSubmitter invokes the decide CLI on PATH when available.
type ExecDecideSubmitter struct {
	Command    string
	NotifyPane string
	Timeout    time.Duration
}

// Submit files a `decide ask --class approval` item if the decide binary is
// present on PATH. When decide is not found on PATH, Submit returns "", nil.
func (s *ExecDecideSubmitter) Submit(ctx context.Context, req DecideRequest) (string, error) {
	if strings.TrimSpace(req.Question) == "" {
		return "", fmt.Errorf("decide question is required")
	}
	cmdName := s.Command
	if cmdName == "" {
		cmdName = "decide"
	}
	bin, err := exec.LookPath(cmdName)
	if err != nil {
		if errors.Is(err, exec.ErrNotFound) {
			return "", nil
		}
		return "", fmt.Errorf("lookup %s: %w", cmdName, err)
	}

	args := buildDecideAskArgs(req, s.NotifyPane)
	timeout := s.Timeout
	if timeout <= 0 {
		timeout = 30 * time.Second
	}
	runCtx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	var stdout, stderr bytes.Buffer
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command -- bin is resolved via LookPath for the configured decide CLI
	cmd := exec.CommandContext(runCtx, bin, args...)
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg != "" {
			return "", fmt.Errorf("decide ask: %w: %s", err, msg)
		}
		return "", fmt.Errorf("decide ask: %w", err)
	}

	var out struct {
		ID string `json:"id"`
	}
	if err := json.Unmarshal(stdout.Bytes(), &out); err != nil {
		return "", fmt.Errorf("parse decide ask output: %w", err)
	}
	if out.ID == "" {
		return "", fmt.Errorf("decide ask returned empty id")
	}
	return out.ID, nil
}

func buildDecideAskArgs(req DecideRequest, notifyPane string) []string {
	owner := req.Owner
	if owner == "" {
		owner = "memstore"
	}
	verbatim := req.Verbatim
	if verbatim == "" {
		verbatim = req.Question
	}
	args := []string{
		"ask",
		"--class", "approval",
		"--owner", owner,
		"--kind", "relay",
		"--verbatim", verbatim,
	}
	if req.Parent != "" {
		args = append(args, "--parent", req.Parent)
	}
	if notifyPane != "" {
		args = append(args, "--notify-pane", notifyPane)
	}
	return append(args, "--json", req.Question)
}
