package outbox

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

// Step is one external write performed after approval.
type Step struct {
	Where string
	Run   func(ctx context.Context) (string, error)
}

// UncertainError is returned when execution stopped with an in-flight step
// and cannot safely retry without risking a duplicate external write.
type UncertainError struct {
	Where string
}

// Error returns a clear explanation naming the uncertain step.
func (e *UncertainError) Error() string {
	return "execution stopped while executing " + e.Where + "; it may or may not have completed and is not retried"
}

// ExecuteSteps runs pending steps while persisting crash-safe InFlight markers.
// Already-recorded receipts are skipped. Any pre-existing InFlight index refuses
// retry and returns *UncertainError so an external write is never issued twice.
func ExecuteSteps(ctx context.Context, state *ExecutionState, steps []Step, save func() error) error {
	if state.InFlight != nil {
		where := "?"
		if *state.InFlight >= 0 && *state.InFlight < len(steps) {
			where = steps[*state.InFlight].Where
		}
		state.InFlight = nil
		return &UncertainError{Where: where}
	}
	for i := len(state.Receipts); i < len(steps); i++ {
		idx := i
		state.InFlight = &idx
		if err := save(); err != nil {
			return err
		}
		url, err := steps[i].Run(ctx)
		state.InFlight = nil
		if err != nil {
			return errors.Join(fmt.Errorf("execute %s: %w", steps[i].Where, err), save())
		}
		state.Receipts = append(state.Receipts, Receipt{Where: steps[i].Where, URL: url})
		if err := save(); err != nil {
			return err
		}
	}
	return nil
}

// Executor plans external execution steps for an approved record.
type Executor interface {
	Steps(ctx context.Context, r Record) ([]Step, error)
}

// ExecutorFunc adapts a single-step execution function into an Executor.
type ExecutorFunc func(ctx context.Context, r Record) (where, url string, err error)

// Steps returns a single Step calling f.
func (f ExecutorFunc) Steps(_ context.Context, r Record) ([]Step, error) {
	where := r.Target
	if where == "" {
		where = r.Kind
	}
	return []Step{{
		Where: where,
		Run: func(ctx context.Context) (string, error) {
			w, url, err := f(ctx, r)
			if w != "" && url == "" {
				url = w
			}
			return url, err
		},
	}}, nil
}

// CommandExecutor runs a configured argv directly (without a shell) for an
// approved Record, passing the JSON Record on stdin and OUTBOX_* env vars.
// Stdout may be either a single line URL or JSON {"receipts":[{"where","url"}]}
// / {"where","url"}.
type CommandExecutor struct {
	Command []string
	Timeout time.Duration
	BaseEnv []string
}

// Steps plans one execution Step that invokes e.Command.
func (e CommandExecutor) Steps(_ context.Context, r Record) ([]Step, error) {
	if len(e.Command) == 0 || e.Command[0] == "" {
		return nil, errors.New("command executor: empty command")
	}
	where := r.Target
	if where == "" {
		where = "command"
	}
	return []Step{{
		Where: where,
		Run: func(ctx context.Context) (string, error) {
			return e.runOne(ctx, r)
		},
	}}, nil
}

func (e CommandExecutor) runOne(ctx context.Context, r Record) (string, error) {
	payload, err := json.Marshal(r)
	if err != nil {
		return "", fmt.Errorf("encode record %d: %w", r.ID, err)
	}
	timeout := e.Timeout
	if timeout <= 0 {
		timeout = commandTimeout
	}
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, e.Command[0], e.Command[1:]...) //nolint:gosec // G204: operator config.
	cmd.Stdin = bytes.NewReader(payload)
	cmd.Env = executorEnv(e.BaseEnv, r)
	var stdout, stderr bytes.Buffer
	cmd.Stdout, cmd.Stderr = &stdout, &stderr
	cmd.WaitDelay = time.Second
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("%s: %w: %s", e.Command[0], err, strings.TrimSpace(stderr.String()))
	}
	return parseExecutorOutput(stdout.Bytes()), nil
}

func executorEnv(baseEnv []string, r Record) []string {
	env := make([]string, 0, len(baseEnv)+6)
	env = append(env, baseEnv...)
	return append(env,
		"OUTBOX_RECORD_ID="+strconv.FormatInt(r.ID, 10),
		"OUTBOX_KIND="+r.Kind,
		"OUTBOX_TARGET="+r.Target,
		"OUTBOX_OWNER="+r.Owner,
		"OUTBOX_REF="+r.Ref,
		"OUTBOX_DECIDE_ID="+r.DecideID,
	)
}

func parseExecutorOutput(raw []byte) string {
	trimmed := strings.TrimSpace(string(raw))
	if trimmed == "" {
		return ""
	}
	var res struct {
		URL      string    `json:"url"`
		Receipts []Receipt `json:"receipts"`
	}
	if strings.HasPrefix(trimmed, "{") && json.Unmarshal([]byte(trimmed), &res) == nil {
		if res.URL != "" {
			return res.URL
		}
		if len(res.Receipts) > 0 {
			return res.Receipts[0].URL
		}
	}
	return trimmed
}
