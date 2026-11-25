// Package notify prepares and executes direct-argv notification commands without a shell.
package notify

import (
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strings"
)

// Runner executes a configured notifier command without a shell.
type Runner func(ctx context.Context, argv []string, env []string) error

// Var binds a template placeholder and environment variable to a value.
type Var struct {
	Placeholder string
	EnvKey      string
	Positional  bool
	Value       string
}

// Exec runs argv directly (no shell) with env.
func Exec(ctx context.Context, argv, env []string) error {
	if len(argv) == 0 {
		return errors.New("run notifier: empty command")
	}
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

// BuildCommand prepares argv and env for a notifier invocation.
// If any element of template contains a Var.Placeholder, placeholders are
// substituted in template. Otherwise, positional Var values are appended
// after template as trailing arguments. All vars with a non-empty EnvKey
// are appended to a copy of baseEnv.
func BuildCommand(template, baseEnv []string, vars []Var) ([]string, []string) {
	replPairs := make([]string, 0, len(vars)*2)
	hasPlaceholder := false
	for _, v := range vars {
		if v.Placeholder != "" {
			replPairs = append(replPairs, v.Placeholder, v.Value)
			if !hasPlaceholder && templateContains(template, v.Placeholder) {
				hasPlaceholder = true
			}
		}
	}

	argv := make([]string, 0, len(template)+len(vars))
	if hasPlaceholder {
		replacer := strings.NewReplacer(replPairs...)
		for _, arg := range template {
			argv = append(argv, replacer.Replace(arg))
		}
	} else {
		argv = append(argv, template...)
		for _, v := range vars {
			if v.Positional {
				argv = append(argv, v.Value)
			}
		}
	}

	env := make([]string, 0, len(baseEnv)+len(vars))
	env = append(env, baseEnv...)
	for _, v := range vars {
		if v.EnvKey != "" {
			env = append(env, v.EnvKey+"="+v.Value)
		}
	}
	return argv, env
}

func templateContains(template []string, placeholder string) bool {
	for _, arg := range template {
		if strings.Contains(arg, placeholder) {
			return true
		}
	}
	return false
}
