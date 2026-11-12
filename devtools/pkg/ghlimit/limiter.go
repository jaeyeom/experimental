package ghlimit

import (
	"context"
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
	// first of them.
	StdoutLen  int64
	StdoutHead []byte
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

// Run runs gh args through run under the limits: it takes a slot, keeps
// the mutation gap for a mutation, and when gh fails with a rate limit
// before writing to stdout, waits and runs it once more. It returns the
// last run's Result.
func (l *Limiter) Run(ctx context.Context, args []string, run Runner) (Result, error) {
	if err := os.MkdirAll(l.Dir, 0o700); err != nil {
		return Result{}, fmt.Errorf("create %s: %w", l.Dir, err)
	}
	if err := os.Chmod(l.Dir, 0o700); err != nil {
		return Result{}, fmt.Errorf("make %s private: %w", l.Dir, err)
	}
	release, slotWait, err := l.acquireSlot(ctx)
	if err != nil {
		return Result{}, err
	}
	defer release()
	waits := []wait{slotWait}

	if IsMutation(args) {
		unlock, lockWait, err := l.acquire(ctx, []string{"mutation.lock"})
		if err != nil {
			return Result{}, err
		}
		defer unlock()
		waits = append(waits, lockWait)
		if d := l.gapLeft(); d > 0 {
			l.sleep(d)
			waits = append(waits, wait{ReasonMutationGap, d})
		}
		defer l.recordMutation()
	}

	info := ClassifyCall(args)
	res, err := l.runCall(ctx, args, info, run, &waits)
	if err == nil && res.ExitCode != 0 && res.StdoutLen == 0 &&
		RateLimited(args, string(res.Stderr), string(res.StdoutHead)) {
		d := l.backoff(res)
		l.sleep(d)
		waits = append(waits, wait{ReasonBackoff, d})
		res, err = l.runCall(ctx, args, info, run, &waits)
	}
	l.log(args, waits)
	return res, err
}

// runCall runs gh once within the per-minute budgets, adding any budget
// wait to waits, and logs the call.
func (l *Limiter) runCall(ctx context.Context, args []string, info CallInfo, run Runner, waits *[]wait) (Result, error) {
	id, budgetWaits, err := l.reserve(ctx, info)
	*waits = append(*waits, budgetWaits...)
	if err != nil {
		return Result{}, err
	}
	start := l.now()
	res, err := run(ctx)
	took := l.now().Sub(start)
	l.finish(ctx, id, info, took)
	if err == nil {
		l.logCall(args, info, start, took, res)
	}
	return res, err
}

// logCall sends the CallEntry of one gh run.
func (l *Limiter) logCall(args []string, info CallInfo, start time.Time, took time.Duration, res Result) {
	if l.LogCall == nil {
		return
	}
	e := newCallEntry(args, info, start)
	e.DurationMS = took.Milliseconds()
	e.ExitCode = res.ExitCode
	e.RateLimited = res.ExitCode != 0 && RateLimited(args, string(res.Stderr), string(res.StdoutHead))
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
