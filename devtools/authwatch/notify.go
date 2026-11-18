package authwatch

import (
	"context"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// NotifyInfo describes a level transition for the notifier command.
type NotifyInfo struct {
	Profile  string
	Level    Level
	TimeLeft string
	Since    time.Time
}

// NotifyRunner executes a configured notifier command without a shell.
type NotifyRunner func(ctx context.Context, argv []string, env []string) error

// ExecNotify runs argv directly (no shell) with env.
func ExecNotify(ctx context.Context, argv, env []string) error {
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: user-configured argv without shell
	if len(env) > 0 {
		cmd.Env = env
	}
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("run notifier: %w", err)
	}
	return nil
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
	replacer := strings.NewReplacer(
		"{profile}", info.Profile,
		"{level}", string(info.Level),
		"{time_left}", info.TimeLeft,
		"{since}", sinceStr,
	)
	hasPlaceholder := false
	for _, arg := range template {
		if strings.Contains(arg, "{profile}") ||
			strings.Contains(arg, "{level}") ||
			strings.Contains(arg, "{time_left}") ||
			strings.Contains(arg, "{since}") {
			hasPlaceholder = true
			break
		}
	}
	argv := make([]string, 0, len(template)+4)
	if hasPlaceholder {
		for _, arg := range template {
			argv = append(argv, replacer.Replace(arg))
		}
	} else {
		argv = append(argv, template...)
		argv = append(argv, info.Profile, string(info.Level), info.TimeLeft, sinceStr)
	}

	env := make([]string, 0, len(baseEnv)+4)
	env = append(env, baseEnv...)
	env = append(env,
		"AUTHWATCH_PROFILE="+info.Profile,
		"AUTHWATCH_LEVEL="+string(info.Level),
		"AUTHWATCH_TIME_LEFT="+info.TimeLeft,
		"AUTHWATCH_SINCE="+sinceStr,
	)
	return argv, env
}
