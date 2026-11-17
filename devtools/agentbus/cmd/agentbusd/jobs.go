package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/decide"
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

// jobsPolicy reads the user's gate policy: jobs_policy in config.toml,
// or jobs-policy.json beside it. A missing, refused, or unreadable file
// asks for every kind. It says which on stderr; a refused or unreadable
// file also goes to the user through notify, when there is one.
func jobsPolicy(env []string, cfg config.Config, stderr io.Writer, notify func(string)) jobs.Policy {
	path := cfg.JobsPolicy
	if path == "" {
		cfgPath, err := config.Path(env)
		if err != nil {
			fmt.Fprintf(stderr, "agentbusd: WARNING: locate the jobs policy: %s; every job kind asks in decide first\n", err)
			return jobs.Policy{}
		}
		path = filepath.Join(filepath.Dir(cfgPath), "jobs-policy.json")
	}
	p, err := jobs.LoadPolicy(path)
	switch {
	case errors.Is(err, jobs.ErrNoPolicy):
		fmt.Fprintf(stderr, "agentbusd: jobs policy %s is missing; every job kind asks in decide first\n", path)
	case err != nil:
		msg := fmt.Sprintf("agentbusd jobs policy refused: %s. Every job kind asks in decide first until it is fixed and agentbusd restarts.", err)
		fmt.Fprintf(stderr, "agentbusd: WARNING: %s\n", msg)
		if notify != nil {
			notify(msg)
		}
	default:
		fmt.Fprintf(stderr, "agentbusd: jobs policy %s\n", path)
	}
	return p
}

// decideGate is the ask gate's Decider over the decide command.
type decideGate struct {
	decide string
	output func(ctx context.Context, argv []string) ([]byte, error)
}

// gateDecider is the ask gate's Decider, or nil when decide is not
// installed.
func gateDecider(lookPath func(string) (string, error), output func(ctx context.Context, argv []string) ([]byte, error)) jobs.Decider {
	path, err := lookPath("decide")
	if err != nil {
		return nil
	}
	return decideGate{decide: path, output: output}
}

func (d decideGate) run(ctx context.Context, it *decide.Item, args ...string) error {
	ctx, cancel := context.WithTimeout(ctx, decideTimeout)
	defer cancel()
	out, err := d.output(ctx, append([]string{d.decide}, args...))
	if err != nil {
		return err
	}
	if err := json.Unmarshal(out, it); err != nil {
		return fmt.Errorf("decide %s: parse output: %w", args[0], err)
	}
	return nil
}

// Ask files a relay item owned by agentbus with approve and reject
// options and free text for a note. It names no pane, so nothing is
// typed into an agent.
func (d decideGate) Ask(ctx context.Context, question string) (string, error) {
	var it decide.Item
	if err := d.run(ctx, &it, "ask", "--owner", "agentbus", "--kind", "relay",
		"--option", jobs.OptionApprove+":Run it", "--option", jobs.OptionReject+":Reject",
		"--free-text", "--json", question); err != nil {
		return "", err
	}
	if it.ID == "" {
		return "", errors.New("decide ask printed no id")
	}
	return it.ID, nil
}

// Show reads an item.
func (d decideGate) Show(ctx context.Context, id string) (decide.Item, error) {
	var it decide.Item
	err := d.run(ctx, &it, "show", "--json", id)
	return it, err
}

// Done closes an item with outcome.
func (d decideGate) Done(ctx context.Context, id, outcome string) error {
	var it decide.Item
	return d.run(ctx, &it, "done", "--json", id, outcome)
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

// decideStatus publishes the jobs status view as decide status agent
// rows.
type decideStatus struct {
	decide string
	output func(ctx context.Context, argv []string) ([]byte, error)
}

// decideStatusSink is the jobs status view's sink over the decide
// command, or nil when decide is not installed.
func decideStatusSink(lookPath func(string) (string, error), output func(ctx context.Context, argv []string) ([]byte, error)) jobs.StatusSink {
	path, err := lookPath("decide")
	if err != nil {
		return nil
	}
	return decideStatus{decide: path, output: output}
}

func (d decideStatus) run(ctx context.Context, args ...string) error {
	ctx, cancel := context.WithTimeout(ctx, decideTimeout)
	defer cancel()
	if _, err := d.output(ctx, append([]string{d.decide, "status"}, args...)); err != nil {
		return fmt.Errorf("decide status %s: %w", args[0], err)
	}
	return nil
}

// Set writes the agent row name.
func (d decideStatus) Set(ctx context.Context, name, state, text string) error {
	return d.run(ctx, "set", "--agent", name, "--state", state, "--", text)
}

// Clear removes the agent row name.
func (d decideStatus) Clear(ctx context.Context, name string) error {
	return d.run(ctx, "clear", "--agent", name)
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
