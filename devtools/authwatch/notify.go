package authwatch

import (
	"context"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/notify"
)

// NotifyInfo describes a level transition for the notifier command.
type NotifyInfo struct {
	Profile  string
	Level    Level
	TimeLeft string
	Since    time.Time
}

// NotifyRunner executes a configured notifier command without a shell.
type NotifyRunner = notify.Runner

// ExecNotify runs argv directly (no shell) with env.
func ExecNotify(ctx context.Context, argv, env []string) error {
	return notify.Exec(ctx, argv, env) //nolint:wrapcheck // thin wrapper around devtools/pkg/notify
}

// BuildNotifyCommand prepares the argv and environment for a notifier invocation.
// If template contains any placeholder ({profile}, {level}, {time_left}, {since}),
// placeholders are substituted. Otherwise, profile, level, time_left, and since
// are appended as positional arguments.
func BuildNotifyCommand(template, baseEnv []string, info NotifyInfo) ([]string, []string) {
	sinceStr := ""
	if !info.Since.IsZero() {
		sinceStr = info.Since.UTC().Format(time.RFC3339)
	}
	vars := []notify.Var{
		{Placeholder: "{profile}", EnvKey: "AUTHWATCH_PROFILE", Positional: true, Value: info.Profile},
		{Placeholder: "{level}", EnvKey: "AUTHWATCH_LEVEL", Positional: true, Value: string(info.Level)},
		{Placeholder: "{time_left}", EnvKey: "AUTHWATCH_TIME_LEFT", Positional: true, Value: info.TimeLeft},
		{Placeholder: "{since}", EnvKey: "AUTHWATCH_SINCE", Positional: true, Value: sinceStr},
	}
	return notify.BuildCommand(template, baseEnv, vars)
}
