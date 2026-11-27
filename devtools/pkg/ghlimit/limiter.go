package ghlimit

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// Result is what one gh run produced.
type Result struct {
	ExitCode int
	// Stderr is gh's stderr. StderrShown reports whether it was also
	// written out as gh ran; otherwise the caller writes it.
	Stderr      []byte
	StderrShown bool
	// StdoutLen is how many bytes gh wrote to stdout, and StdoutHead the
	// first of them. StdoutUncounted reports that stdout went to gh as
	// is, so StdoutLen is unknown.
	StdoutLen       int64
	StdoutHead      []byte
	StdoutUncounted bool
	// Pages is the number of pages counted for a --paginate call, when
	// counted during execution.
	Pages int
	// Stdout is all of stdout when StdoutKept, as asked for by
	// ExecRunner.KeepStdout.
	Stdout     []byte
	StdoutKept bool
}

// Runner runs gh once. It returns an error only when gh could not be
// run; a gh failure is a Result with a non-zero ExitCode.
type Runner func(ctx context.Context) (Result, error)

// Limiter coordinates gh calls through lock files in Dir.
type Limiter struct {
	Dir    string
	Config Config
	// Now and Sleep default to the real clock.
	Now   func() time.Time
	Sleep func(time.Duration)
	// Log, when set, receives one Entry for each call that waited longer
	// than LogThreshold.
	Log func(Entry) error
	// LogCall, when set, receives one CallEntry for each gh run and each
	// read the cache served.
	LogCall func(CallEntry) error
}

// LogThreshold is the shortest total wait that is logged.
const LogThreshold = 200 * time.Millisecond

// pollInterval is how often a lock wait retries.
const pollInterval = 20 * time.Millisecond

// Wait reasons in log entries.
const (
	ReasonSlot        = "slot"
	ReasonSlotTimeout = "slot_timeout"
	ReasonMutationGap = "mutation_gap"
	ReasonBackoff     = "backoff"
)

type wait struct {
	reason string
	d      time.Duration
}

func (l *Limiter) now() time.Time {
	if l.Now != nil {
		return l.Now()
	}
	return time.Now()
}

func (l *Limiter) sleep(d time.Duration) {
	if l.Sleep != nil {
		l.Sleep(d)
		return
	}
	time.Sleep(d)
}

func (l *Limiter) sleepCtx(ctx context.Context, d time.Duration) error {
	if d <= 0 {
		return nil
	}
	if err := ctx.Err(); err != nil {
		return fmt.Errorf("wait for gh: %w", err)
	}
	if l.Sleep != nil {
		l.Sleep(d)
		return nil
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-timer.C:
		return nil
	case <-ctx.Done():
		return fmt.Errorf("wait for gh: %w", ctx.Err())
	}
}

func (l *Limiter) ensureDir() error {
	if err := os.MkdirAll(l.Dir, 0o700); err != nil {
		return fmt.Errorf("create %s: %w", l.Dir, err)
	}
	if err := os.Chmod(l.Dir, 0o700); err != nil {
		return fmt.Errorf("make %s private: %w", l.Dir, err)
	}
	return nil
}

// Run runs gh args through run under the limits: it waits on any
// account-wide cooldown before taking a slot, takes a slot, keeps the
// mutation gap for a mutation, caps paginated calls, and when gh fails
// with a rate limit before writing to stdout, releases its slot before
// backing off and retrying once. Preflight calls fail fast during a
// cooldown instead of waiting or retrying.
func (l *Limiter) Run(ctx context.Context, args []string, run Runner) (Result, error) {
	if err := l.ensureDir(); err != nil {
		return Result{}, err
	}
	local := isLocalCommand(args)
	preflight := IsPreflight(args)
	info := ClassifyCall(args)

	var waits []wait
	wasInCooldown := false
	if !local {
		if preflight {
			if res, blocked, err := l.preflightCooldownCheck(args, info); err != nil || blocked {
				return res, err
			}
		} else {
			cw, waited, err := l.waitCooldown(ctx)
			if cw.d > 0 {
				waits = append(waits, cw)
			}
			if err != nil {
				l.log(args, waits)
				return Result{}, err
			}
			wasInCooldown = waited
		}
	}

	res, attemptWaits, hit, retriable, err := l.runAttempt(ctx, args, info, run, local, preflight, wasInCooldown)
	waits = append(waits, attemptWaits...)
	if err != nil || !retriable || hit == nil {
		l.log(args, waits)
		return res, err
	}

	// No slot or mutation lock is held during the backoff sleep.
	if err := l.sleepCtx(ctx, hit.wait); err != nil {
		l.log(args, waits)
		return Result{}, err
	}
	if hit.wait > 0 {
		waits = append(waits, wait{ReasonBackoff, hit.wait})
	}
	extraWait, _, err := l.waitCooldown(ctx)
	if extraWait.d > 0 {
		waits = append(waits, extraWait)
	}
	if err != nil {
		l.log(args, waits)
		return Result{}, err
	}
	res, retryWaits, _, _, err := l.runAttempt(ctx, args, info, run, local, preflight, true)
	waits = append(waits, retryWaits...)
	l.log(args, waits)
	return res, err
}

func (l *Limiter) runAttempt(ctx context.Context, args []string, info CallInfo, run Runner, local, preflight, wasInCooldown bool) (Result, []wait, *cooldownHit, bool, error) {
	var waits []wait
	begin := l.now()
	for {
		release, slotWait, err := l.acquireSlot(ctx)
		if err != nil {
			return Result{}, waits, nil, false, err
		}
		waits = append(waits, slotWait)

		forceBudget := slotWait.reason == ReasonSlotTimeout || l.now().Sub(begin) >= l.Config.MaxWait
		id, budgetWait, err := l.tryReserve(ctx, info, forceBudget)
		if err != nil {
			release()
			return Result{}, waits, nil, false, err
		}
		if budgetWait.d == 0 {
			return l.runWithSlot(ctx, args, info, run, id, release, waits, local, preflight, wasInCooldown)
		}
		release()
		if err := l.sleepCtx(ctx, budgetWait.d); err != nil {
			return Result{}, waits, nil, false, err
		}
		coalesceWait(&waits, budgetWait)
		if !local && !preflight {
			cw, waited, err := l.waitCooldown(ctx)
			if cw.d > 0 {
				coalesceWait(&waits, cw)
			}
			if err != nil {
				return Result{}, waits, nil, false, err
			}
			if waited {
				wasInCooldown = true
			}
		}
	}
}

func (l *Limiter) runWithSlot(ctx context.Context, args []string, info CallInfo, run Runner, id string, release func(), waits []wait, local, preflight, wasInCooldown bool) (Result, []wait, *cooldownHit, bool, error) {
	defer release()
	isMut := IsMutation(args)
	if isMut {
		unlock, mutWaits, err := l.acquireMutation(ctx)
		if err != nil {
			return Result{}, waits, nil, false, err
		}
		defer unlock()
		waits = append(waits, mutWaits...)
	}

	paginated := IsPaginated(args)
	callCtx, cancel := withPaginateTimeout(ctx, paginated, l.Config.PaginateTimeout)
	defer cancel()

	start := l.now()
	res, err := run(callCtx)
	took := l.now().Sub(start)
	timedOut := isPaginateTimedOut(ctx, callCtx, paginated, took, l.Config.PaginateTimeout, l.Now != nil)
	if timedOut && err == nil {
		err = fmt.Errorf("paginated call exceeded timeout %s: %w", l.Config.PaginateTimeout, context.DeadlineExceeded)
	}

	pages, points := l.resolveCost(args, info, paginated, res)
	if err != nil && !timedOut {
		return res, waits, nil, false, err
	}
	l.finish(ctx, id, info, took, points, pages)

	var hit *cooldownHit
	retriable := false
	if !timedOut {
		hit, retriable = l.handleCooldownOutcome(ctx, args, &res, local, preflight, wasInCooldown)
	}
	if isMut && !retriable {
		l.recordMutation()
	}
	l.logRunResult(args, info, start, took, res, pages, points, hit)
	return res, waits, hit, retriable, err
}

func (l *Limiter) acquireMutation(ctx context.Context) (func(), []wait, error) {
	unlock, lockWait, err := l.acquire(ctx, []string{"mutation.lock"})
	if err != nil {
		return nil, nil, err
	}
	waits := []wait{lockWait}
	if d := l.gapLeft(); d > 0 {
		l.sleep(d)
		waits = append(waits, wait{ReasonMutationGap, d})
	}
	return unlock, waits, nil
}

func (l *Limiter) handleCooldownOutcome(ctx context.Context, args []string, res *Result, local, preflight, wasInCooldown bool) (*cooldownHit, bool) {
	if res.ExitCode != 0 && RateLimited(args, string(res.Stderr), string(res.StdoutHead)) {
		waitDur, cause, _ := l.recordCooldown(ctx, args, *res, wasInCooldown)
		hit := &cooldownHit{wait: waitDur, duration: waitDur, cause: cause}
		if preflight {
			ensurePreflightMarker(res)
			return hit, false
		}
		return hit, !local && res.StdoutLen == 0
	}
	if !local {
		l.clearCooldownIfExpired(ctx)
	}
	return nil, false
}

func (l *Limiter) logRunResult(args []string, info CallInfo, start time.Time, took time.Duration, res Result, pages, points int, hit *cooldownHit) {
	var cooldownMS int64
	var cooldownCause string
	if hit != nil {
		cooldownMS = hit.duration.Milliseconds()
		cooldownCause = hit.cause
	}
	l.logCall(args, info, start, took, res, pages, points, cooldownMS, cooldownCause)
}

func (l *Limiter) resolveCost(args []string, info CallInfo, paginated bool, res Result) (pages, points int) {
	if !paginated {
		return 0, info.Points
	}
	pages = countPaginatedPages(args, res)
	return pages, info.Points * pages
}

func withPaginateTimeout(ctx context.Context, paginated bool, timeout time.Duration) (context.Context, context.CancelFunc) {
	if !paginated || timeout <= 0 {
		return ctx, func() {}
	}
	return context.WithTimeout(ctx, timeout)
}

func isPaginateTimedOut(parent, callCtx context.Context, paginated bool, took, timeout time.Duration, injectedClock bool) bool {
	if !paginated || timeout <= 0 || parent.Err() != nil {
		return false
	}
	if errors.Is(callCtx.Err(), context.DeadlineExceeded) {
		return true
	}
	return injectedClock && took > timeout
}

func coalesceWait(waits *[]wait, w wait) {
	if n := len(*waits); n > 0 && (*waits)[n-1].reason == w.reason {
		(*waits)[n-1].d += w.d
		return
	}
	*waits = append(*waits, w)
}

// logCall sends the CallEntry of one gh run.
func (l *Limiter) logCall(args []string, info CallInfo, start time.Time, took time.Duration, res Result, pages, points int, cooldownMS int64, cooldownCause string) {
	if l.LogCall == nil {
		return
	}
	e := newCallEntry(args, info, start)
	e.DurationMS = took.Milliseconds()
	e.ExitCode = res.ExitCode
	if points > 0 || res.ExitCode == ExitRateLimited {
		e.Points = points
	}
	if pages > 0 {
		e.Pages = pages
	}
	e.RateLimited = res.ExitCode != 0 && (res.ExitCode == ExitRateLimited || RateLimited(args, string(res.Stderr), string(res.StdoutHead)))
	e.CooldownMS = cooldownMS
	e.CooldownCause = cooldownCause
	e.StderrHead = stderrHead(res.Stderr)
	if !res.StdoutUncounted {
		e.StdoutBytes = &res.StdoutLen
		if res.ExitCode == 0 && res.StdoutLen == 0 && AsksJSON(args) {
			e.Anomaly = AnomalyEmptyJSON
		}
	}
	if res.StdoutKept {
		if cost, remaining, ok := ParseRateLimit(res.Stdout); ok {
			e.RateLimitCost, e.RateLimitRemaining = &cost, &remaining
		}
	}
	_ = l.LogCall(e)
}

// logCacheHit sends the CallEntry of a read served from the cache,
// which costs no points.
func (l *Limiter) logCacheHit(args []string) {
	if l.LogCall == nil {
		return
	}
	e := newCallEntry(args, ClassifyCall(args), l.now())
	e.Points, e.CacheHit = 0, true
	_ = l.LogCall(e)
}

// acquireSlot takes one of the MaxConcurrent slot locks.
func (l *Limiter) acquireSlot(ctx context.Context) (func(), wait, error) {
	names := []string{"slots.lock"}
	if n := l.Config.MaxConcurrent; n > 1 {
		names = make([]string, n)
		for i := range names {
			names[i] = "slot-" + strconv.Itoa(i) + ".lock"
		}
	}
	return l.acquire(ctx, names)
}

// acquire takes a lock on any one of the named files in Dir, polling
// until one is free. Past MaxWait it gives up and returns a no-op
// release, so the call runs unlocked.
func (l *Limiter) acquire(ctx context.Context, names []string) (func(), wait, error) {
	files := make([]*os.File, 0, len(names))
	closeAll := func(keep *os.File) {
		for _, f := range files {
			if f != keep {
				_ = f.Close()
			}
		}
	}
	for _, name := range names {
		f, err := os.OpenFile(filepath.Join(l.Dir, name), os.O_CREATE|os.O_RDWR, 0o600)
		if err != nil {
			closeAll(nil)
			return nil, wait{}, fmt.Errorf("open lock: %w", err)
		}
		files = append(files, f)
	}
	start := l.now()
	for polled := false; ; polled = true {
		for _, f := range files {
			ok, err := tryLock(f)
			if err != nil {
				closeAll(nil)
				return nil, wait{}, err
			}
			if ok {
				closeAll(f)
				w := wait{reason: ReasonSlot}
				if polled {
					w.d = l.now().Sub(start)
				}
				return func() { _ = f.Close() }, w, nil
			}
		}
		waited := l.now().Sub(start)
		if waited >= l.Config.MaxWait {
			closeAll(nil)
			return func() {}, wait{ReasonSlotTimeout, waited}, nil
		}
		if err := ctx.Err(); err != nil {
			closeAll(nil)
			return nil, wait{}, fmt.Errorf("wait for gh slot: %w", err)
		}
		l.sleep(pollInterval)
	}
}

func (l *Limiter) lastMutationPath() string { return filepath.Join(l.Dir, "last-mutation") }

// gapLeft is how long until MutationGap has passed since the last
// mutation by any process.
func (l *Limiter) gapLeft() time.Duration {
	raw, err := os.ReadFile(l.lastMutationPath())
	if err != nil {
		return 0
	}
	nanos, err := strconv.ParseInt(strings.TrimSpace(string(raw)), 10, 64)
	if err != nil {
		return 0
	}
	return time.Unix(0, nanos).Add(l.Config.MutationGap).Sub(l.now())
}

// recordMutation stores now as the last mutation time. A failed write
// only loosens the gap for the next call.
func (l *Limiter) recordMutation() {
	_ = os.WriteFile(l.lastMutationPath(), []byte(strconv.FormatInt(l.now().UnixNano(), 10)+"\n"), 0o600)
}

// backoff is the wait gh output asks for, else Backoff, capped at
// MaxBackoff.
func (l *Limiter) backoff(res Result) time.Duration {
	d, ok := RetryAfter(string(res.Stderr)+"\n"+string(res.StdoutHead), l.now())
	if !ok {
		d = l.Config.Backoff
	}
	return max(min(d, l.Config.MaxBackoff), 0)
}

// log sends one Entry when the waits add up to more than LogThreshold.
// A slot timeout is always logged.
func (l *Limiter) log(args []string, waits []wait) {
	if l.Log == nil {
		return
	}
	var (
		total   time.Duration
		reasons []string
		timeout bool
	)
	for _, w := range waits {
		if w.reason == ReasonSlotTimeout {
			timeout = true
		}
		if w.d <= 0 {
			continue
		}
		total += w.d
		reasons = append(reasons, w.reason)
	}
	if total <= LogThreshold && !timeout {
		return
	}
	_ = l.Log(Entry{
		Time:   l.now(),
		Args:   strings.Join(Redact(args), " "),
		Reason: strings.Join(reasons, "+"),
		WaitMS: total.Milliseconds(),
	})
}
