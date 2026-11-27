package installcheck

import (
	"context"
	"errors"
	"fmt"
	"io"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/pkg/notify"
)

// NotificationKind identifies why a notification is being delivered.
type NotificationKind string

const (
	// NotifyLanded indicates one or more watched issues closed on main.
	NotifyLanded NotificationKind = "landed"
	// NotifyFailure indicates one or more repository checks failed.
	NotifyFailure NotificationKind = "failure"
)

// Notification is one message emitted by Check.
type Notification struct {
	Kind NotificationKind
	Body string
}

// Notifier delivers a notification produced by Check.
type Notifier interface {
	Notify(ctx context.Context, n Notification) error
}

// NotifierFunc adapts a function to Notifier.
type NotifierFunc func(ctx context.Context, n Notification) error

// Notify invokes f(ctx, n).
func (f NotifierFunc) Notify(ctx context.Context, n Notification) error {
	return f(ctx, n)
}

// StdoutNotifier writes notifications to an io.Writer.
type StdoutNotifier struct {
	Out io.Writer
}

// Notify writes n.Body to s.Out.
func (s StdoutNotifier) Notify(_ context.Context, n Notification) error {
	if s.Out == nil {
		return errors.New("stdout notifier: writer is nil")
	}
	body := n.Body
	if body != "" && !strings.HasSuffix(body, "\n") {
		body += "\n"
	}
	if _, err := io.WriteString(s.Out, body); err != nil {
		return fmt.Errorf("write notification: %w", err)
	}
	return nil
}

// CommandNotifier runs a direct-argv command template without a shell.
// It supports {message}, {kind}, and {recipient} placeholders (and exports
// INSTALL_CHECK_MESSAGE, INSTALL_CHECK_KIND, and INSTALL_CHECK_RECIPIENT).
// When no placeholder is present in Template, the message body is appended
// as a trailing positional argument.
type CommandNotifier struct {
	Template  []string
	Recipient string
	BaseEnv   []string
	Run       notify.Runner
}

// Notify builds the argv and environment for n and runs the configured command.
func (c CommandNotifier) Notify(ctx context.Context, n Notification) error {
	if len(c.Template) == 0 {
		return errors.New("command notifier: template is empty")
	}
	tmpl := slices.Clone(c.Template)
	bin, err := expandHomePath(tmpl[0], c.BaseEnv)
	if err != nil {
		return fmt.Errorf("command notifier: %w", err)
	}
	tmpl[0] = bin

	vars := []notify.Var{
		{Placeholder: "{kind}", EnvKey: "INSTALL_CHECK_KIND", Positional: false, Value: string(n.Kind)},
		{Placeholder: "{recipient}", EnvKey: "INSTALL_CHECK_RECIPIENT", Positional: false, Value: c.Recipient},
		{Placeholder: "{message}", EnvKey: "INSTALL_CHECK_MESSAGE", Positional: true, Value: n.Body},
	}
	argv, env := notify.BuildCommand(tmpl, c.BaseEnv, vars)
	runner := c.Run
	if runner == nil {
		runner = notify.Exec
	}
	if err := runner(ctx, argv, env); err != nil {
		return fmt.Errorf("notify command %s: %w", tmpl[0], err)
	}
	return nil
}
