package authwatch

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
)

// CLIStatusChecker checks the agent CLI's own auth status command as an
// optional secondary signal.
type CLIStatusChecker func(ctx context.Context, cliCmd string, configDir string, env []string) (loggedIn bool, err error)

// DefaultCLIStatusChecker runs `<cliCmd> auth status` with CLAUDE_CONFIG_DIR
// set to configDir when non-empty. Exit 0 means logged in, exit 1 means logged out.
func DefaultCLIStatusChecker(runCmd CmdOutputRunner) CLIStatusChecker {
	if runCmd == nil {
		runCmd = ExecOutput
	}
	return func(ctx context.Context, cliCmd, configDir string, env []string) (bool, error) {
		if cliCmd == "" {
			cliCmd = DefaultCLICommand
		}
		cmdEnv := withEnvVar(env, "CLAUDE_CONFIG_DIR", configDir)
		out, code, err := runCmd(ctx, cliCmd, []string{"auth", "status"}, cmdEnv)
		if code == 0 && err == nil {
			var parsed struct {
				LoggedIn *bool `json:"loggedIn"`
			}
			if len(out) > 0 && json.Unmarshal(out, &parsed) == nil && parsed.LoggedIn != nil {
				return *parsed.LoggedIn, nil
			}
			return true, nil
		}
		if code == 1 {
			return false, nil
		}
		return false, fmt.Errorf("%s auth status failed (exit %d): %w", cliCmd, code, err)
	}
}

func withEnvVar(env []string, key, value string) []string {
	if value == "" {
		return env
	}
	prefix := key + "="
	out := make([]string, 0, len(env)+1)
	for _, item := range env {
		if !strings.HasPrefix(item, prefix) {
			out = append(out, item)
		}
	}
	return append(out, prefix+value)
}
