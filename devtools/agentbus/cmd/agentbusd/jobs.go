package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
)

const (
	// holdResource is the decide status resource that holds the jobs
	// queue while it is busy.
	holdResource = "jobs"
	// reconcileTimeout bounds one reconcile hook.
	reconcileTimeout = 30 * time.Second
)

// jobsConfig reads jobs.json beside config.toml. A missing file is the
// default config.
func jobsConfig(env []string) (jobs.Config, error) {
	path, err := config.Path(env)
	if err != nil {
		return jobs.Config{}, fmt.Errorf("locate the jobs config: %w", err)
	}
	cfg, err := jobs.LoadConfig(filepath.Join(filepath.Dir(path), "jobs.json"))
	if err != nil {
		return jobs.Config{}, fmt.Errorf("%w", err)
	}
	return cfg, nil
}

// decideHold holds the jobs queue while the decide status resource
// "jobs" is busy, or nil when decide is not installed. A status that
// cannot be read holds too: the hold is a safety stop.
func decideHold(lookPath func(string) (string, error), output func(ctx context.Context, argv []string) ([]byte, error)) func() (bool, string) {
	decide, err := lookPath("decide")
	if err != nil {
		return nil
	}
	return func() (bool, string) {
		ctx, cancel := context.WithTimeout(context.Background(), decideTimeout)
		defer cancel()
		out, err := output(ctx, []string{decide, "status", "list", "--json"})
		if err != nil {
			return true, "decide status cannot be read: " + err.Error()
		}
		var board struct {
			Resources []struct {
				Name         string `json:"name"`
				Availability string `json:"availability"`
				Note         string `json:"note"`
			} `json:"resources"`
		}
		if err := json.Unmarshal(out, &board); err != nil {
			return true, "decide status cannot be read: " + err.Error()
		}
		for _, r := range board.Resources {
			if r.Name == holdResource && r.Availability == "busy" {
				why := "decide status resource jobs is busy"
				if r.Note != "" {
					why += ": " + r.Note
				}
				return true, why
			}
		}
		return false, ""
	}
}

// runOutput runs argv and returns its standard output.
func runOutput(ctx context.Context, argv []string) ([]byte, error) {
	// argv[0] is decide from PATH with fixed arguments.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	out, err := exec.CommandContext(ctx, argv[0], argv[1:]...).Output() //nolint:gosec // G204: see above
	if err != nil {
		return nil, fmt.Errorf("%s: %w", argv[0], err)
	}
	return out, nil
}
