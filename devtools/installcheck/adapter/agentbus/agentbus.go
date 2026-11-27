// Package agentbus adapts installcheck notifications to the agentbus CLI.
package agentbus

import (
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strings"

	"github.com/jaeyeom/experimental/devtools/installcheck"
)

// Notifier delivers installcheck notifications over agentbus.
// Exit code 3 from `agentbus send` means the message is stored and held by the
// daemon, which counts as successfully sent.
type Notifier struct {
	Recipient string
	Run       installcheck.Runner
}

// Notify runs `agentbus send <recipient> <body>`.
func (n Notifier) Notify(ctx context.Context, msg installcheck.Notification) error {
	if strings.TrimSpace(n.Recipient) == "" {
		return errors.New("agentbus notifier: recipient is empty")
	}
	run := n.Run
	if run == nil {
		run = installcheck.Exec
	}
	_, err := run(ctx, "agentbus", "send", n.Recipient, msg.Body)
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) && exitErr.ExitCode() == 3 {
		return nil
	}
	if err != nil {
		return fmt.Errorf("send to %s: %w", n.Recipient, err)
	}
	return nil
}
