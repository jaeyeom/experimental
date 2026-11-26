package herdr

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"time"
)

// Error codes from herdr's JSON error envelope that start reacts to.
const (
	CodePaneBusy      = "agent_pane_busy"
	CodePromptStalled = "agent_prompt_stalled"
	CodeTimeout       = "timeout"
)

// ErrorCode returns the error.code herdr printed for a failed call, or ""
// when err is not a herdr exit with a JSON error.
func ErrorCode(err error) string {
	var proc *ProcError
	if !errors.As(err, &proc) {
		return ""
	}
	return commandErrorCode(proc.Stdout, proc.Stderr)
}

// WaitAgent runs `herdr agent wait TARGET --until S... --timeout MS` and
// returns the agent_status it matched.
func (c *Client) WaitAgent(ctx context.Context, target string, until []string, timeout time.Duration) (string, error) {
	if target == "" {
		return "", errors.New("herdr agent wait: expected a target, found empty")
	}
	if len(until) == 0 {
		return "", errors.New("herdr agent wait: expected at least one --until state, found none")
	}
	args := []string{"agent", "wait", target}
	for _, status := range until {
		args = append(args, "--until", status)
	}
	args = append(args, "--timeout", formatMS(timeout))
	raw, err := c.output(ctx, timeout+promptTimeoutGrace, args...)
	if err != nil {
		return "", err
	}
	result, err := decodeEnvelope(raw, "agent_info")
	if err != nil {
		return "", err
	}
	if err := requireAgent(result); err != nil {
		return "", err
	}
	var agent struct {
		AgentStatus string `json:"agent_status"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	}
	if err := json.Unmarshal(result["agent"], &agent); err != nil {
		return "", fmt.Errorf("%w: agent: %w", ErrDegraded, err)
	}
	return agent.AgentStatus, nil
}

// ReadAgent runs `herdr agent read TARGET --source visible --lines N
// --format text` and returns the visible screen as plain text.
func (c *Client) ReadAgent(ctx context.Context, target string, lines int) (string, error) {
	if target == "" {
		return "", errors.New("herdr agent read: expected a target, found empty")
	}
	if lines < 1 {
		return "", fmt.Errorf("herdr agent read: expected lines >= 1, found %d", lines)
	}
	raw, err := c.output(ctx, defaultCallTimeout, "agent", "read", target, "--source", "visible", "--lines", strconv.Itoa(lines), "--format", "text")
	if err != nil {
		return "", err
	}
	return string(raw), nil
}

// SendKeys runs `herdr agent send-keys TARGET KEY...`. A zero exit is success.
func (c *Client) SendKeys(ctx context.Context, target string, keys ...string) error {
	if target == "" || len(keys) == 0 {
		return fmt.Errorf("herdr agent send-keys: expected a target and keys, found target %q keys %q", target, keys)
	}
	_, err := c.output(ctx, defaultCallTimeout, append([]string{"agent", "send-keys", target}, keys...)...)
	return err
}
