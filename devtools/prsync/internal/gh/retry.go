package gh

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	executor "github.com/jaeyeom/go-cmdexec"
)

// ghRetryAttempts is how many times one gh call is tried.
// Issue 368 asks for 2–3 attempts with a backoff such as 2s / 8s / 20s.
// Three attempts wait 2s, then 8s.
const ghRetryAttempts = 3

// ghRetryBackoffs are the waits before the second and third attempts.
var ghRetryBackoffs = []time.Duration{2 * time.Second, 8 * time.Second}

// ghListHTTPRetryAttempts is the open-PR list budget for HTTP 502, 503, and 504.
// Five attempts wait 2s, 8s, 32s, then about 60s.
const ghListHTTPRetryAttempts = 5

// ghListHTTPRetryBackoffs are the waits before attempts 2 through 5 of that list query.
var ghListHTTPRetryBackoffs = []time.Duration{2 * time.Second, 8 * time.Second, 32 * time.Second, 60 * time.Second}

var transientHTTPRE = regexp.MustCompile(`(?i)\bhttp\s+50[234]\b`)

type outputCheck func(*executor.ExecutionResult) error

type transientKind int

const (
	transientNone transientKind = iota
	transientHTTP
	transientTruncated
	transientTimeout
	transientSecondary
)

func (c *Client) execute(ctx context.Context, timeout time.Duration, args ...string) (*executor.ExecutionResult, error) {
	return c.executeChecked(ctx, timeout, nil, args...)
}

// ghRetryPolicy lengthens the HTTP 502/503/504 budget for one call.
// A zero value keeps the three-attempt budget for every transient kind.
// nextHTTPArgs, when set, returns the args for the attempt after an
// HTTP 502/503/504, so a call can shrink its request before the retry.
type ghRetryPolicy struct {
	httpAttempts int
	httpBackoffs []time.Duration
	nextHTTPArgs func(ctx context.Context, failErr error) []string
}

func (c *Client) executeChecked(ctx context.Context, timeout time.Duration, check outputCheck, args ...string) (*executor.ExecutionResult, error) {
	return c.executeRetry(ctx, timeout, check, ghRetryPolicy{}, args...)
}

func (c *Client) executeRetry(ctx context.Context, timeout time.Duration, check outputCheck, policy ghRetryPolicy, args ...string) (*executor.ExecutionResult, error) {
	var lastErr error
	for attempt := 1; ; attempt++ {
		if err := ctx.Err(); err != nil {
			return nil, runErr(c.bin, err)
		}
		result, execErr := c.exec.Execute(ctx, executor.ToolConfig{
			Command: c.bin,
			Args:    args,
			Timeout: timeout,
		})
		kind, done, failErr := classifyForRetry(c.bin, result, execErr, check)
		if done {
			return result, failErr
		}
		lastErr = failErr
		limit, backoffs := policy.budget(kind)
		if attempt >= limit {
			break
		}
		if kind == transientHTTP && policy.nextHTTPArgs != nil {
			args = policy.nextHTTPArgs(ctx, failErr)
		}
		wait := c.retryWait(backoffs, attempt, kind, attemptText(result, execErr))
		c.logRetry(ctx, attempt+1, limit, wait, failErr)
		if err := c.sleepForRetry(ctx, wait); err != nil {
			return nil, runErr(c.bin, err)
		}
	}
	return nil, fmt.Errorf("transient upstream error: %w", lastErr)
}

func (p ghRetryPolicy) budget(kind transientKind) (int, []time.Duration) {
	if kind == transientHTTP && p.httpAttempts > ghRetryAttempts && len(p.httpBackoffs) > 0 {
		return p.httpAttempts, p.httpBackoffs
	}
	return ghRetryAttempts, ghRetryBackoffs
}

func (c *Client) retryWait(backoffs []time.Duration, failedAttempt int, kind transientKind, text string) time.Duration {
	if kind == transientSecondary {
		if d, ok := c.parseRetryAfter(text); ok {
			return d
		}
	}
	idx := failedAttempt - 1
	if idx < 0 {
		idx = 0
	}
	if idx >= len(backoffs) {
		return backoffs[len(backoffs)-1]
	}
	return backoffs[idx]
}

func (c *Client) sleepForRetry(ctx context.Context, d time.Duration) error {
	sleep := c.sleep
	if sleep == nil {
		sleep = sleepWithContext
	}
	return sleep(ctx, d)
}

func (c *Client) logRetry(ctx context.Context, nextAttempt, maxAttempts int, wait time.Duration, err error) {
	if c.errOut != nil {
		fmt.Fprintf(c.errOut, "prsync: retrying gh after transient error (attempt %d/%d, waiting %s): %v\n",
			nextAttempt, maxAttempts, wait, err)
	}
	runlog.FromContext(ctx).Warn("gh retry",
		"attempt", nextAttempt,
		"max_attempts", maxAttempts,
		"wait", wait.String(),
		"err", err.Error(),
	)
}

func sleepWithContext(ctx context.Context, d time.Duration) error {
	if d <= 0 {
		return nil
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return fmt.Errorf("retry wait: %w", ctx.Err())
	case <-timer.C:
		return nil
	}
}

func runErr(bin string, err error) error {
	return fmt.Errorf("run %s: %w", bin, err)
}

func classifyForRetry(bin string, result *executor.ExecutionResult, execErr error, check outputCheck) (transientKind, bool, error) {
	if execErr != nil {
		var timeoutErr *executor.TimeoutError
		if !errors.As(execErr, &timeoutErr) {
			return transientNone, true, runErr(bin, execErr)
		}
		return transientTimeout, false, runErr(bin, execErr)
	}
	kind, failErr := classifyResult(result, check)
	if kind == transientNone {
		return transientNone, true, nil
	}
	return kind, false, failErr
}

func classifyResult(result *executor.ExecutionResult, check outputCheck) (transientKind, error) {
	if result.ExitCode != 0 {
		return classifyFailedResult(result)
	}
	if check == nil {
		return transientNone, nil
	}
	if err := check(result); err != nil {
		return transientTruncated, err
	}
	return transientNone, nil
}

func classifyFailedResult(result *executor.ExecutionResult) (transientKind, error) {
	text := result.Stderr + "\n" + result.Output
	if kind, ok := classifyText(text); ok {
		return kind, &ProcError{ExitCode: result.ExitCode, Stdout: result.Output, Stderr: result.Stderr}
	}
	if strings.TrimSpace(result.Stderr) == "" && strings.TrimSpace(result.Output) == "" {
		return transientTruncated, errors.New("truncated or empty output")
	}
	return transientNone, nil
}

func classifyText(text string) (transientKind, bool) {
	lower := strings.ToLower(text)
	switch {
	case strings.Contains(lower, "secondary rate limit"), strings.Contains(lower, "secondary rate-limit"):
		return transientSecondary, true
	case transientHTTPRE.MatchString(lower):
		return transientHTTP, true
	case strings.Contains(lower, "unexpected end of json input"),
		strings.Contains(lower, "unexpected eof"),
		strings.Contains(lower, "unexpected end of file"):
		return transientTruncated, true
	default:
		return transientNone, false
	}
}

func validJSONOutput(result *executor.ExecutionResult) error {
	if result.ExitCode != 0 {
		return nil
	}
	if json.Valid([]byte(result.Output)) {
		return nil
	}
	return errors.New("truncated or empty output")
}

func nonEmptyOutput(result *executor.ExecutionResult) error {
	if result.ExitCode != 0 {
		return nil
	}
	if strings.TrimSpace(result.Output) != "" {
		return nil
	}
	return errors.New("truncated or empty output")
}

func attemptText(result *executor.ExecutionResult, execErr error) string {
	if result != nil {
		return result.Stderr + "\n" + result.Output
	}
	if execErr != nil {
		return execErr.Error()
	}
	return ""
}

func (c *Client) parseRetryAfter(text string) (time.Duration, bool) {
	if d, ok := parseRetryAfterKey(text, "retry-after"); ok {
		return c.delayUntil(d)
	}
	if d, ok := parseRetryAfterKey(text, "retry after"); ok {
		return c.delayUntil(d)
	}
	return 0, false
}

func (c *Client) delayUntil(v retryAfterValue) (time.Duration, bool) {
	if !v.isDate {
		return v.delay, true
	}
	now := time.Now
	if c.now != nil {
		now = c.now
	}
	d := v.when.Sub(now())
	if d < 0 {
		return 0, true
	}
	return d, true
}

type retryAfterValue struct {
	delay  time.Duration
	when   time.Time
	isDate bool
}

func parseRetryAfterKey(text, key string) (retryAfterValue, bool) {
	lower := strings.ToLower(text)
	i := strings.Index(lower, key)
	if i < 0 {
		return retryAfterValue{}, false
	}
	rest := strings.TrimSpace(text[i+len(key):])
	rest = strings.TrimLeft(rest, ":= \t")
	if rest == "" {
		return retryAfterValue{}, false
	}
	if d, ok := delaySeconds(rest); ok {
		return retryAfterValue{delay: d}, true
	}
	when, err := httpTime(rest)
	if err != nil {
		return retryAfterValue{}, false
	}
	return retryAfterValue{when: when, isDate: true}, true
}

func delaySeconds(rest string) (time.Duration, bool) {
	field := rest
	if i := strings.IndexAny(rest, " \t\r\n"); i >= 0 {
		field = rest[:i]
	}
	n, err := strconv.Atoi(field)
	if err != nil || n < 0 {
		return 0, false
	}
	return time.Duration(n) * time.Second, true
}

func httpTime(rest string) (time.Time, error) {
	line := rest
	if i := strings.IndexByte(rest, '\n'); i >= 0 {
		line = rest[:i]
	}
	t, err := http.ParseTime(strings.TrimSpace(line))
	if err != nil {
		return time.Time{}, fmt.Errorf("parse retry-after date: %w", err)
	}
	return t, nil
}
