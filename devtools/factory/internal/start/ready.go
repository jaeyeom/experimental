package start

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	defaultReadyTimeout = 60 * time.Second
	defaultBusyTimeout  = 20 * time.Second
	// statusProbe bounds a wait that matches every state, which returns
	// the agent's status at once.
	statusProbe = 5 * time.Second
	// recoverWait bounds the wait for working after the recovery Enter.
	recoverWait = 15 * time.Second
	// screenLines is how much of the agent's screen a stall reads.
	screenLines = 60
	// excerptLines is how much of it a stall error quotes.
	excerptLines = 12
	// Recovery steps recorded in the run detail.
	recoveryEnter  = "enter"
	recoveryResent = "resent"
	recoveryNone   = "none"
)

// launchLog is what the run detail records about the launch.
type launchLog struct {
	woke      bool          // A hibernated agent was woken before the prompt.
	readyWait time.Duration // Time spent waiting for the new agent to be idle.
	stalled   bool          // The first prompt stalled or timed out.
	recovery  string        // enter, resent, or none, when stalled.
}

var allStatuses = []string{
	herdr.AgentStatusIdle, herdr.AgentStatusWorking, herdr.AgentStatusBlocked,
	herdr.AgentStatusDone, herdr.AgentStatusUnknown,
}

// startAgent runs herdr agent start, retrying with backoff while the new
// pane's shell is still busy (agent_pane_busy), for example loading a slow
// .envrc, until AgentBusyTimeout. Any other error is returned at once.
func (s *Starter) startAgent(ctx context.Context, agent, pane string, agentArgs []string) error {
	limit := s.Defaults.AgentBusyTimeout
	if limit <= 0 {
		limit = defaultBusyTimeout
	}
	backoff := time.Second
	var waited time.Duration
	for attempt := 1; ; attempt++ {
		err := s.Herdr.StartAgent(ctx, agent, s.effectiveAgent(), pane, agentTimeout, agentArgs)
		if err == nil || herdr.ErrorCode(err) != herdr.CodePaneBusy {
			return err //nolint:wrapcheck // propagate context cancellation
		}
		if waited >= limit {
			return fmt.Errorf("pane %s still busy after %d attempts over %s: %w", pane, attempt, waited, err)
		}
		pause := min(backoff, limit-waited)
		slog.Info("agent pane busy, retrying", "pane", pane, "attempt", attempt, "after", pause)
		if err := s.sleep(ctx, pause); err != nil {
			return err
		}
		waited += pause
		backoff *= 2
	}
}

// waitReady waits for the new agent to be idle, then pauses
// AgentReadySettle so its input box accepts the prompt. It returns how
// long the idle wait took.
func (s *Starter) waitReady(ctx context.Context, pane string) (time.Duration, error) {
	limit := s.Defaults.AgentReadyTimeout
	if limit <= 0 {
		limit = defaultReadyTimeout
	}
	began := s.clock()
	if _, err := s.Herdr.WaitAgent(ctx, pane, []string{herdr.AgentStatusIdle}, limit); err != nil {
		return 0, fmt.Errorf("wait for agent ready: %w", err)
	}
	waited := s.clock().Sub(began)
	if s.Defaults.AgentReadySettle > 0 {
		if err := s.sleep(ctx, s.Defaults.AgentReadySettle); err != nil {
			return waited, err
		}
	}
	return waited, nil
}

// stalled reports whether the prompt failed in a way that may have left
// the text in the agent's input box: herdr saw no working state in time.
func stalled(err error) bool {
	code := herdr.ErrorCode(err)
	return code == herdr.CodePromptStalled || code == herdr.CodeTimeout
}

// recoverStall makes at most one recovery attempt after a stalled prompt.
// A stall does not prove that no input was sent, so it reads the agent
// first:
//   - working or blocked: the prompt went through; nothing to do.
//   - idle with the prompt's first and last lines on screen: the text sits
//     unsubmitted in the input box, so it sends Enter and waits for working.
//   - idle without the prompt on screen: nothing was typed, so it sends the
//     prompt once more.
//   - anything else: no retry; the error quotes the screen.
//
// The prompt is never submitted a third time.
func (s *Starter) recoverStall(ctx context.Context, pane, prompt string, cause error) (string, error) {
	slog.Warn("start prompt stalled; reading the agent", "pane", pane, "err", cause)
	status, err := s.Herdr.WaitAgent(ctx, pane, allStatuses, statusProbe)
	if err != nil {
		return recoveryNone, fmt.Errorf("prompt stalled (%w); read agent status: %w", cause, err)
	}
	if status == herdr.AgentStatusWorking || status == herdr.AgentStatusBlocked {
		slog.Info("stalled prompt went through", "pane", pane, "status", status)
		return recoveryNone, nil
	}
	screen, err := s.Herdr.ReadAgent(ctx, pane, screenLines)
	if err != nil {
		return recoveryNone, fmt.Errorf("prompt stalled (%w); read agent screen: %w", cause, err)
	}
	inBox := promptOnScreen(prompt, screen)
	switch {
	case status == herdr.AgentStatusIdle && inBox:
		slog.Info("stalled prompt is in the input box; sending Enter", "pane", pane)
		if err := s.Herdr.SendKeys(ctx, pane, "enter"); err != nil {
			return recoveryEnter, fmt.Errorf("prompt stalled (%w); send Enter: %w", cause, err)
		}
		if _, err := s.Herdr.WaitAgent(ctx, pane, []string{herdr.AgentStatusWorking, herdr.AgentStatusBlocked}, recoverWait); err != nil {
			return recoveryEnter, fmt.Errorf("prompt stalled (%w); sent Enter, then: %w; screen:\n%s", cause, err, excerpt(screen))
		}
		return recoveryEnter, nil
	case status == herdr.AgentStatusIdle:
		slog.Info("stalled prompt is not on screen; sending it once more", "pane", pane)
		if err := s.Herdr.Prompt(ctx, pane, prompt, []string{herdr.AgentStatusWorking}, promptTimeout); err != nil {
			return recoveryResent, fmt.Errorf("prompt stalled (%w); resent once, then: %w", cause, err)
		}
		return recoveryResent, nil
	default:
		return recoveryNone, fmt.Errorf("prompt stalled (%w); agent is %s, not retried; screen:\n%s", cause, status, excerpt(screen))
	}
}

// promptOnScreen reports whether the prompt's first and last non-blank
// lines both appear on screen. Whitespace runs compare equal, because the
// agent's input box rewraps and reindents.
func promptOnScreen(prompt, screen string) bool {
	var lines []string
	for _, line := range strings.Split(prompt, "\n") {
		if text := squash(line); text != "" {
			lines = append(lines, text)
		}
	}
	if len(lines) == 0 {
		return false
	}
	flat := squash(screen)
	return strings.Contains(flat, lines[0]) && strings.Contains(flat, lines[len(lines)-1])
}

func squash(text string) string {
	return strings.Join(strings.Fields(text), " ")
}

func excerpt(screen string) string {
	lines := strings.Split(strings.TrimRight(screen, "\n"), "\n")
	if len(lines) > excerptLines {
		lines = lines[len(lines)-excerptLines:]
	}
	return strings.Join(lines, "\n")
}

// abandon undoes what this start did when the agent never ran: it closes the
// tab this run opened and, when changedTracker is true, releases the tracker
// claim and marks the local row released. When changedTracker is false (the
// item was already claimed before this start), it leaves the tracker item
// untouched and marks its own local row failed.
func (s *Starter) abandon(ctx context.Context, row store.Claim, opened placed, step int, cause error, changedTracker bool) (Outcome, error) {
	var problems []error
	if _, err := s.Herdr.CloseTab(ctx, opened.tabID); err != nil && !errors.Is(err, herdr.ErrTabNotFound) {
		problems = append(problems, fmt.Errorf("close tab %s: %w", opened.tabID, err))
	}
	if !changedTracker {
		var ferr error
		if len(problems) > 0 {
			ferr = s.fail(ctx, row, step, fmt.Errorf("%w; cleanup: %w", cause, errors.Join(problems...)))
		} else {
			ferr = s.fail(ctx, row, step, fmt.Errorf("%w; closed tab %s without releasing tracker item", cause, opened.tabID))
		}
		return claimKeptOutcome(row.ID, ferr), ferr
	}
	if err := s.Src.Release(ctx, row.ID); err != nil {
		problems = append(problems, fmt.Errorf("release %s: %w", row.ID, err))
	}
	if len(problems) > 0 {
		ferr := s.fail(ctx, row, step, fmt.Errorf("%w; cleanup: %w", cause, errors.Join(problems...)))
		return claimKeptOutcome(row.ID, ferr), ferr
	}
	detail := fmt.Sprintf("step %d: %s; closed tab %s and released", step, cause.Error(), opened.tabID)
	if err := s.Store.UpdateClaim(ctx, row.Source, row.ID, row.ClaimedAt, store.Released, detail, "", ""); err != nil {
		ferr := fmt.Errorf("step %d: %w; closed tab %s and released %s, but marking the claim released failed: %w", step, cause, opened.tabID, row.ID, err)
		return claimKeptOutcome(row.ID, ferr), ferr
	}
	rerr := fmt.Errorf("step %d: %w; closed tab %s and released %s; re-run: factory start %s --apply", step, cause, opened.tabID, row.ID, row.ID)
	return Outcome{
		ID:       row.ID,
		Status:   OutcomeFailedReleased,
		Reason:   cause.Error(),
		NextStep: fmt.Sprintf("re-run: factory start %s --apply", row.ID),
		ExitCode: ExitReleased,
	}, rerr
}

func (s *Starter) clock() time.Time {
	if s.Now != nil {
		return s.Now()
	}
	return time.Now()
}

// sleep pauses d, or returns early when ctx ends. Sleep replaces it in tests.
func (s *Starter) sleep(ctx context.Context, d time.Duration) error {
	if s.Sleep != nil {
		return s.Sleep(ctx, d)
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return ctx.Err() //nolint:wrapcheck // propagate context cancellation
	case <-timer.C:
		return nil
	}
}
