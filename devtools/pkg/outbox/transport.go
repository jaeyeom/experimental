package outbox

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

// Notice describes an outcome reported back to the submitting owner.
type Notice struct {
	RecordID int64
	State    string
	Kind     string
	Target   string
	Owner    string
	Ref      string
	Outcome  string
	Notes    string
	Receipts []Receipt
	Message  string
	Metadata map[string]string
}

// Transport delivers outcome notices to the submitting owner.
type Transport interface {
	Notify(ctx context.Context, n Notice) error
}

// TransportFunc adapts a function into a Transport.
type TransportFunc func(ctx context.Context, n Notice) error

// Notify invokes f(ctx, n).
func (f TransportFunc) Notify(ctx context.Context, n Notice) error {
	return f(ctx, n)
}

// PullTransport discards push notifications when callers pull status instead.
type PullTransport struct{}

// Notify is a no-op returning nil.
func (PullTransport) Notify(context.Context, Notice) error {
	return nil
}

// CommandTransport runs a configured argv directly (without a shell) to
// deliver an outcome notice. The formatted notice goes on stdin and structured
// fields are passed in OUTBOX_* environment variables.
type CommandTransport struct {
	Command []string
	Timeout time.Duration
	BaseEnv []string
}

// Notify executes t.Command with OUTBOX_* environment variables and n.Message on stdin.
func (t CommandTransport) Notify(ctx context.Context, n Notice) error {
	if len(t.Command) == 0 || t.Command[0] == "" {
		return errors.New("command transport: empty command")
	}
	timeout := t.Timeout
	if timeout <= 0 {
		timeout = commandTimeout
	}
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, t.Command[0], t.Command[1:]...) //nolint:gosec // G204: operator config.
	cmd.Stdin = strings.NewReader(n.Message)
	cmd.Env = noticeEnv(t.BaseEnv, n)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	cmd.WaitDelay = time.Second
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%s: %w: %s", t.Command[0], err, strings.TrimSpace(stderr.String()))
	}
	return nil
}

func noticeEnv(baseEnv []string, n Notice) []string {
	env := make([]string, 0, len(baseEnv)+7)
	env = append(env, baseEnv...)
	return append(env,
		"OUTBOX_RECORD_ID="+strconv.FormatInt(n.RecordID, 10),
		"OUTBOX_STATE="+n.State,
		"OUTBOX_KIND="+n.Kind,
		"OUTBOX_TARGET="+n.Target,
		"OUTBOX_OWNER="+n.Owner,
		"OUTBOX_REF="+n.Ref,
		"OUTBOX_OUTCOME="+n.Outcome,
	)
}
