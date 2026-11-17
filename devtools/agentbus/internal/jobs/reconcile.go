package jobs

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// Outcome is what a reconcile hook found.
type Outcome string

// Hook outcomes. Unclear, and any hook error, raises a decide item.
const (
	// OutcomeDone: the work is there (head moved, PR merged, comment
	// present). The job is settled as done.
	OutcomeDone Outcome = "done"
	// OutcomeFailed: the work is not there. The job is settled as
	// failed and is not resent.
	OutcomeFailed Outcome = "failed"
	// OutcomeUnclear: the hook cannot tell.
	OutcomeUnclear Outcome = "unclear"
)

// Hook runs a kind's reconcile argv for one job.
type Hook func(ctx context.Context, argv []string, job Job) (Outcome, string, error)

// Reconcile checks every lost outward job that nobody settled. A kind
// with a reconcile hook runs it: done or failed settles the job. An
// unclear answer, a hook error, or a kind without a hook raises one
// decide item through ask, and the job waits for the user. A job
// settled meanwhile is left alone. Hooks run with the store unlocked.
func Reconcile(ctx context.Context, s *Store, hook Hook, ask func(text string)) {
	for _, j := range s.Unsettled() {
		if ctx.Err() != nil {
			return
		}
		k := s.Config().Kind(j.Kind)
		why := "kind " + j.Kind + " has no reconcile hook"
		if len(k.Reconcile) > 0 && hook != nil {
			out, note, err := hook(ctx, k.Reconcile, j)
			switch {
			case err != nil:
				why = "its reconcile hook failed: " + err.Error()
			case out == OutcomeDone || out == OutcomeFailed:
				// An error here is a job settled meanwhile, or a save
				// that the next pass tries again.
				_ = s.Settle(j.ID, out, note)
				continue
			default:
				why = "its reconcile hook could not tell"
				if note != "" {
					why += ": " + note
				}
			}
		}
		if s.MarkAsked(j.ID) != nil {
			continue
		}
		if ask != nil {
			ask(askText(j, why))
		}
	}
}

func askText(j Job, why string) string {
	what := j.Kind + " " + j.Subject
	if j.Head != "" {
		what += " at " + j.Head
	}
	return fmt.Sprintf("agentbus job %s (%s) is %s: runner %s stopped sending heartbeats, and it may have acted. "+
		"It will not be run again. %s. Check it, then settle it with: agentbus jobs done %s, or agentbus jobs fail %s.",
		j.ID, what, j.State, j.Holder, capitalize(why), j.ID, j.ID)
}

func capitalize(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}

// ExecHook runs a reconcile argv with the job in JOB_ID, JOB_KIND,
// JOB_SUBJECT, and JOB_HEAD, on top of env (nil is this process's
// environment), within timeout. The first word of its first output
// line is the outcome, done or failed; the rest of the line is a note.
// Anything else is unclear. A non-zero exit is an error.
func ExecHook(timeout time.Duration, env []string) Hook {
	return func(ctx context.Context, argv []string, j Job) (Outcome, string, error) {
		ctx, cancel := context.WithTimeout(ctx, timeout)
		defer cancel()
		// argv comes from the user's jobs config, run as its own argv,
		// not a shell string.
		// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
		cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: see above
		base := env
		if base == nil {
			base = os.Environ()
		}
		cmd.Env = append(append([]string{}, base...),
			"JOB_ID="+j.ID, "JOB_KIND="+j.Kind, "JOB_SUBJECT="+j.Subject, "JOB_HEAD="+j.Head)
		out, err := cmd.Output()
		if err != nil {
			return "", "", fmt.Errorf("%s: %w", argv[0], err)
		}
		line, _, _ := strings.Cut(string(out), "\n")
		word, note, _ := strings.Cut(strings.TrimSpace(line), " ")
		switch Outcome(word) {
		case OutcomeDone, OutcomeFailed:
			return Outcome(word), strings.TrimSpace(note), nil
		default:
			return OutcomeUnclear, strings.TrimSpace(line), nil
		}
	}
}
