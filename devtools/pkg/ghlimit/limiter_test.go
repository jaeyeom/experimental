package ghlimit

import (
	"bytes"
	"context"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

// fakeClock advances only when slept on.
type fakeClock struct {
	mu    sync.Mutex
	now   time.Time
	slept []time.Duration
}

func newFakeClock() *fakeClock { return &fakeClock{now: time.Unix(1_700_000_000, 0)} }

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *fakeClock) Sleep(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.slept = append(c.slept, d)
	c.now = c.now.Add(d)
}

// fakeGH writes an executable shell script standing in for gh and returns
// its path.
func fakeGH(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "gh")
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o700); err != nil { //nolint:gosec // G306: the fake gh must be executable
		t.Fatal(err)
	}
	return path
}

type logSink struct {
	mu      sync.Mutex
	entries []Entry
}

func (s *logSink) Log(e Entry) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.entries = append(s.entries, e)
	return nil
}

func (s *logSink) Entries() []Entry {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]Entry(nil), s.entries...)
}

func newLimiter(t *testing.T, clock *fakeClock) (*Limiter, *logSink) {
	t.Helper()
	sink := &logSink{}
	l := &Limiter{Dir: t.TempDir(), Config: DefaultConfig(), Log: sink.Log}
	if clock != nil {
		l.Now, l.Sleep = clock.Now, clock.Sleep
	}
	return l, sink
}

type call struct {
	stdout, stderr bytes.Buffer
	res            Result
	err            error
}

func runGH(t *testing.T, l *Limiter, gh string, args []string, stdin string) *call {
	t.Helper()
	c := &call{}
	r, err := NewExecRunner(gh, args, strings.NewReader(stdin), &c.stdout, &c.stderr)
	if err != nil {
		t.Fatal(err)
	}
	c.res, c.err = l.Run(context.Background(), args, r.Run)
	if c.err == nil && !c.res.StderrShown {
		c.stderr.Write(c.res.Stderr)
	}
	return c
}

// runSilently runs gh through l, discarding its output. It is safe to
// call from another goroutine.
func runSilently(l *Limiter, gh string, args []string) error {
	r, err := NewExecRunner(gh, args, strings.NewReader(""), io.Discard, io.Discard)
	if err != nil {
		return err
	}
	_, err = l.Run(context.Background(), args, r.Run)
	return err
}

func readLines(t *testing.T, path string) []string {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return strings.Fields(string(raw))
}

func TestConcurrentCallsDoNotOverlap(t *testing.T) {
	trace := filepath.Join(t.TempDir(), "trace")
	gh := fakeGH(t, "echo start >> "+trace+"\nsleep 0.3\necho end >> "+trace+"\n")
	l, _ := newLimiter(t, nil)
	var wg sync.WaitGroup
	for range 2 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if err := runSilently(l, gh, []string{"pr", "view"}); err != nil {
				t.Error(err)
			}
		}()
	}
	wg.Wait()
	if got := strings.Join(readLines(t, trace), " "); got != "start end start end" {
		t.Fatalf("trace = %q, want the calls one after the other", got)
	}
}

func TestMoreSlotsLetCallsOverlap(t *testing.T) {
	trace := filepath.Join(t.TempDir(), "trace")
	gh := fakeGH(t, "echo start >> "+trace+"\nsleep 0.3\necho end >> "+trace+"\n")
	l, _ := newLimiter(t, nil)
	l.Config.MaxConcurrent = 2
	var wg sync.WaitGroup
	for range 2 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if err := runSilently(l, gh, []string{"pr", "view"}); err != nil {
				t.Error(err)
			}
		}()
	}
	wg.Wait()
	if got := strings.Join(readLines(t, trace), " "); got != "start start end end" {
		t.Fatalf("trace = %q, want the calls to overlap", got)
	}
}

func TestMutationGap(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	var starts []time.Time
	run := func(context.Context) (Result, error) {
		starts = append(starts, clock.Now())
		clock.Sleep(100 * time.Millisecond)
		return Result{}, nil
	}
	merge := []string{"pr", "merge", "1"}
	for _, args := range [][]string{merge, {"pr", "view", "1"}, merge} {
		if _, err := l.Run(context.Background(), args, run); err != nil {
			t.Fatal(err)
		}
	}
	if gap := starts[1].Sub(starts[0]); gap != 100*time.Millisecond {
		t.Errorf("read waited: started %v after the mutation, want 100ms", gap)
	}
	if gap := starts[2].Sub(starts[0]); gap < time.Second {
		t.Errorf("mutations %v apart, want >= 1s", gap)
	}
}

func TestMutationGapAcrossLimiters(t *testing.T) {
	clock := newFakeClock()
	a, _ := newLimiter(t, clock)
	b := &Limiter{Dir: a.Dir, Config: a.Config, Now: clock.Now, Sleep: clock.Sleep}
	var starts []time.Time
	run := func(context.Context) (Result, error) {
		starts = append(starts, clock.Now())
		return Result{}, nil
	}
	for _, l := range []*Limiter{a, b} {
		if _, err := l.Run(context.Background(), []string{"issue", "comment", "1"}, run); err != nil {
			t.Fatal(err)
		}
	}
	if gap := starts[1].Sub(starts[0]); gap < time.Second {
		t.Fatalf("mutations from two limiters %v apart, want >= 1s", gap)
	}
}

const flakyBody = `count=$(cat "$STATE" 2>/dev/null || echo 0)
count=$((count + 1))
echo $count > "$STATE"
`

func TestBackoffThenSuccess(t *testing.T) {
	state := filepath.Join(t.TempDir(), "count")
	gh := fakeGH(t, "STATE="+state+"\n"+flakyBody+`if [ $count -eq 1 ]; then echo "You have exceeded a secondary rate limit" >&2; exit 1; fi
echo ok
`)
	clock := newFakeClock()
	l, sink := newLimiter(t, clock)
	c := runGH(t, l, gh, []string{"pr", "view", "1"}, "")
	if c.err != nil || c.res.ExitCode != 0 {
		t.Fatalf("Run = %+v, %v; want success", c.res, c.err)
	}
	if c.stdout.String() != "ok\n" || c.stderr.String() != "" {
		t.Fatalf("stdout %q stderr %q; want only the retry's output", c.stdout.String(), c.stderr.String())
	}
	if len(clock.slept) != 1 || clock.slept[0] != time.Minute {
		t.Fatalf("slept %v, want one 1m backoff", clock.slept)
	}
	entries := sink.Entries()
	if len(entries) != 1 || entries[0].Reason != "backoff" || entries[0].WaitMS != 60000 || entries[0].Args != "pr view _" {
		t.Fatalf("log = %+v, want one backoff line", entries)
	}
}

func TestBackoffUsesRetryAfterCappedAtMax(t *testing.T) {
	state := filepath.Join(t.TempDir(), "count")
	gh := fakeGH(t, "STATE="+state+"\n"+flakyBody+`if [ $count -eq 1 ]; then printf 'gh: rate limit\nRetry-After: 9000\n' >&2; exit 1; fi
`)
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	runGH(t, l, gh, []string{"pr", "view", "1"}, "")
	if len(clock.slept) != 1 || clock.slept[0] != 5*time.Minute {
		t.Fatalf("slept %v, want the 5m cap", clock.slept)
	}
}

func TestFailsTwiceReturnsGHResult(t *testing.T) {
	state := filepath.Join(t.TempDir(), "count")
	gh := fakeGH(t, "STATE="+state+"\n"+flakyBody+`echo "secondary rate limit, try $count" >&2
exit 4
`)
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	c := runGH(t, l, gh, []string{"pr", "merge", "1"}, "")
	if c.err != nil || c.res.ExitCode != 4 {
		t.Fatalf("Run = %+v, %v; want exit 4", c.res, c.err)
	}
	if got := c.stderr.String(); got != "secondary rate limit, try 2\n" {
		t.Fatalf("stderr = %q, want gh's own", got)
	}
	if got := readLines(t, state); got[0] != "2" {
		t.Fatalf("gh ran %s times, want 2", got[0])
	}
}

func TestPartialStdoutIsNotRetried(t *testing.T) {
	state := filepath.Join(t.TempDir(), "count")
	gh := fakeGH(t, "STATE="+state+"\n"+flakyBody+`echo partial
echo "secondary rate limit" >&2
exit 1
`)
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	c := runGH(t, l, gh, []string{"pr", "list"}, "")
	if c.res.ExitCode != 1 || c.stdout.String() != "partial\n" || c.stderr.String() != "secondary rate limit\n" {
		t.Fatalf("Run = %+v stdout %q stderr %q", c.res, c.stdout.String(), c.stderr.String())
	}
	if got := readLines(t, state); got[0] != "1" || len(clock.slept) != 0 {
		t.Fatalf("gh ran %s times and slept %v; want one run, no wait", got[0], clock.slept)
	}
}

func TestOtherFailureIsNotRetried(t *testing.T) {
	gh := fakeGH(t, "echo 'no pull requests found' >&2\nexit 1\n")
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	c := runGH(t, l, gh, []string{"pr", "view"}, "")
	if c.res.ExitCode != 1 || len(clock.slept) != 0 {
		t.Fatalf("Run = %+v, slept %v", c.res, clock.slept)
	}
}

func TestStdinPassesThroughAndIsReplayedOnRetry(t *testing.T) {
	dir := t.TempDir()
	state, bodies := filepath.Join(dir, "count"), filepath.Join(dir, "bodies")
	gh := fakeGH(t, "STATE="+state+"\n"+flakyBody+`cat >> `+bodies+`
if [ $count -eq 1 ]; then echo "secondary rate limit" >&2; exit 1; fi
`)
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	c := runGH(t, l, gh, []string{"pr", "comment", "1", "--body-file", "-"}, "hello\n")
	if c.res.ExitCode != 0 {
		t.Fatalf("Run = %+v", c.res)
	}
	if got := strings.Join(readLines(t, bodies), " "); got != "hello hello" {
		t.Fatalf("gh read %q from stdin, want the body on both runs", got)
	}
}

func TestSlotWaitPastMaxWaitRunsAnyway(t *testing.T) {
	l, sink := newLimiter(t, nil)
	l.Config.MaxWait = 300 * time.Millisecond
	release, _, err := l.acquireSlot(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	defer release()
	ran := false
	if _, err := l.Run(context.Background(), []string{"pr", "view"}, func(context.Context) (Result, error) {
		ran = true
		return Result{}, nil
	}); err != nil {
		t.Fatal(err)
	}
	entries := sink.Entries()
	if !ran || len(entries) != 1 || entries[0].Reason != "slot_timeout" {
		t.Fatalf("ran %v, log %+v; want a run and one slot_timeout line", ran, entries)
	}
}

func TestShortWaitIsNotLogged(t *testing.T) {
	clock := newFakeClock()
	l, sink := newLimiter(t, clock)
	l.Config.MutationGap = 150 * time.Millisecond
	run := func(context.Context) (Result, error) { return Result{}, nil }
	for range 2 {
		if _, err := l.Run(context.Background(), []string{"pr", "close", "1"}, run); err != nil {
			t.Fatal(err)
		}
	}
	if len(clock.slept) != 1 || len(sink.Entries()) != 0 {
		t.Fatalf("slept %v, log %+v; want one short unlogged wait", clock.slept, sink.Entries())
	}
}

func TestStateDirIsPrivate(t *testing.T) {
	l, _ := newLimiter(t, newFakeClock())
	l.Dir = filepath.Join(l.Dir, "gh-throttle")
	if _, err := l.Run(context.Background(), []string{"pr", "view"}, func(context.Context) (Result, error) { return Result{}, nil }); err != nil {
		t.Fatal(err)
	}
	info, err := os.Stat(l.Dir)
	if err != nil {
		t.Fatal(err)
	}
	if perm := info.Mode().Perm(); perm != 0o700 {
		t.Fatalf("state dir mode %v, want 0700", perm)
	}
}

func TestSharedCooldownAcrossProcessesHonorsRetryAfterAndDoubles(t *testing.T) {
	clock := newFakeClock()
	a, sinkA := newLimiter(t, clock)
	a.Config.Backoff = 10 * time.Second
	a.Config.MaxBackoff = 35 * time.Second
	b := &Limiter{Dir: a.Dir, Config: a.Config, Now: clock.Now, Sleep: clock.Sleep, Log: sinkA.Log}

	// Process A gets rate-limited with Retry-After: 12 on attempt 1, then succeeds on retry.
	runsA := 0
	res, err := a.Run(context.Background(), []string{"api", "repos/o/r"}, func(context.Context) (Result, error) {
		runsA++
		if runsA == 1 {
			return Result{
				ExitCode:   1,
				StdoutLen:  10,
				StdoutHead: []byte(`{"msg":"x"}`),
				Stderr:     []byte("HTTP/2.0 429 Too Many Requests\nRetry-After: 12\n"),
			}, nil
		}
		return Result{}, nil
	})
	if err != nil || res.ExitCode != 0 || runsA != 1 {
		// StdoutLen > 0 is not retried, so it should return immediately while setting shared cooldown.
		if err != nil || res.ExitCode != 1 || runsA != 1 {
			t.Fatalf("A Run = %+v, %v (runs %d); want 1 non-retried run", res, err, runsA)
		}
	}

	// Process B starts at t0+2s; it must wait the remaining 10s on cooldown before calling gh.
	clock.Sleep(2 * time.Second)
	var bStart time.Time
	beforeB := clock.Now()
	_, err = b.Run(context.Background(), []string{"pr", "view", "1"}, func(context.Context) (Result, error) {
		bStart = clock.Now()
		return Result{}, nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if waited := bStart.Sub(beforeB); waited != 10*time.Second {
		t.Fatalf("second process waited %v on cooldown, want 10s", waited)
	}
	entries := sinkA.Entries()
	if len(entries) != 1 || entries[0].Reason != ReasonCooldown || entries[0].WaitMS != 10000 {
		t.Fatalf("wait log = %+v, want one cooldown wait of 10000ms", entries)
	}

	// Repeat hits during cooldown double the cooldown up to MaxBackoff.
	// Hit 1: Retry-After 12s -> 12s.
	c1 := &Limiter{Dir: t.TempDir(), Config: a.Config, Now: clock.Now, Sleep: clock.Sleep}
	c2 := &Limiter{Dir: c1.Dir, Config: a.Config, Now: clock.Now, Sleep: clock.Sleep}
	_, err = c1.Run(context.Background(), []string{"api", "repos/o/r"}, func(context.Context) (Result, error) {
		return Result{ExitCode: 1, StdoutLen: 1, Stderr: []byte("HTTP/2.0 429\nRetry-After: 12\n")}, nil
	})
	if err != nil {
		t.Fatal(err)
	}
	// Hit 2 during cooldown (before 12s elapsed): doubles 12s -> 24s.
	if _, _, err := c2.recordCooldown(context.Background(), []string{"pr", "view", "1"}, Result{ExitCode: 1, Stderr: []byte("secondary rate limit\n")}, false); err != nil {
		t.Fatal(err)
	}
	// Hit 3 during cooldown: doubles 24s -> capped at MaxBackoff (35s).
	if _, _, err := c2.recordCooldown(context.Background(), []string{"pr", "view", "1"}, Result{ExitCode: 1, Stderr: []byte("secondary rate limit\n")}, false); err != nil {
		t.Fatal(err)
	}
	beforeC := clock.Now()
	var cStart time.Time
	_, err = c1.Run(context.Background(), []string{"pr", "view", "2"}, func(context.Context) (Result, error) {
		cStart = clock.Now()
		return Result{}, nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if waited := cStart.Sub(beforeC); waited != 35*time.Second {
		t.Fatalf("waited %v after doubled cooldown, want MaxBackoff 35s", waited)
	}
}

func TestNoSlotHeldDuringBackoffSleep(t *testing.T) {
	dir := t.TempDir()
	inBackoff := make(chan struct{})
	resumeBackoff := make(chan struct{})
	var closeOnce sync.Once

	l1 := &Limiter{
		Dir:    dir,
		Config: DefaultConfig(),
		Sleep: func(d time.Duration) {
			if d >= 50*time.Millisecond {
				closeOnce.Do(func() { close(inBackoff) })
				<-resumeBackoff
			}
		},
	}
	l1.Config.Backoff = 200 * time.Millisecond
	l1.Config.MaxBackoff = time.Second

	l2 := &Limiter{
		Dir:    dir,
		Config: l1.Config,
	}
	l2.Config.MaxWait = 2 * time.Second

	var (
		wg       sync.WaitGroup
		l1Err    error
		l1Res    Result
		attempts int
	)
	wg.Add(1)
	go func() {
		defer wg.Done()
		l1Res, l1Err = l1.Run(context.Background(), []string{"pr", "view", "1"}, func(context.Context) (Result, error) {
			attempts++
			if attempts == 1 {
				return Result{ExitCode: 1, Stderr: []byte("secondary rate limit\n")}, nil
			}
			return Result{ExitCode: 0}, nil
		})
	}()

	select {
	case <-inBackoff:
	case <-time.After(3 * time.Second):
		t.Fatal("timed out waiting for backoff sleep")
	}

	// While l1 is sleeping in backoff, no slot lock should be held:
	// a concurrent non-GitHub-blocked call must complete immediately.
	doneNonBlocked := make(chan error, 1)
	go func() {
		_, err := l2.Run(context.Background(), []string{"--version"}, func(context.Context) (Result, error) {
			return Result{ExitCode: 0}, nil
		})
		doneNonBlocked <- err
	}()

	select {
	case err := <-doneNonBlocked:
		if err != nil {
			t.Fatalf("concurrent non-GitHub-blocked call failed: %v", err)
		}
	case <-time.After(2 * time.Second):
		close(resumeBackoff)
		t.Fatal("concurrent non-GitHub-blocked call blocked while l1 slept in backoff")
	}

	close(resumeBackoff)
	wg.Wait()
	if l1Err != nil || l1Res.ExitCode != 0 || attempts != 2 {
		t.Fatalf("l1 = %+v, %v (attempts=%d); want success after retry", l1Res, l1Err, attempts)
	}
}

func TestPreflightAuthStatusDuringCooldownReturnsFastWithMarker(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	l.Config.Backoff = time.Minute
	var calls []CallEntry
	l.LogCall = func(e CallEntry) error {
		calls = append(calls, e)
		return nil
	}

	// Trigger an active cooldown.
	_, err := l.Run(context.Background(), []string{"api", "repos/o/r"}, func(context.Context) (Result, error) {
		return Result{ExitCode: 1, Stderr: []byte("secondary rate limit\n"), StdoutLen: 1}, nil
	})
	if err != nil {
		t.Fatal(err)
	}

	// Auth status during cooldown must NOT call gh, must NOT sleep, and must return ExitRateLimited + marker.
	ghCalled := false
	before := clock.Now()
	sleptBefore := len(clock.slept)
	res, err := l.Run(context.Background(), []string{"auth", "status"}, func(context.Context) (Result, error) {
		ghCalled = true
		return Result{}, nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if ghCalled {
		t.Fatal("auth status ran gh during active cooldown")
	}
	if len(clock.slept) != sleptBefore || clock.Now() != before {
		t.Fatalf("auth status slept during cooldown: %v", clock.slept[sleptBefore:])
	}
	if res.ExitCode != ExitRateLimited {
		t.Fatalf("ExitCode = %d, want %d (ExitRateLimited)", res.ExitCode, ExitRateLimited)
	}
	stderr := string(res.Stderr)
	if !strings.Contains(stderr, RateLimitMarker) || !strings.Contains(strings.ToLower(stderr), "rate limit") {
		t.Fatalf("stderr = %q, want %q and rate limit marker", stderr, RateLimitMarker)
	}

	// Live auth status hitting a rate limit must NOT retry in a loop.
	l2, _ := newLimiter(t, clock)
	liveRuns := 0
	res2, err := l2.Run(context.Background(), []string{"auth", "status"}, func(context.Context) (Result, error) {
		liveRuns++
		return Result{ExitCode: 1, Stderr: []byte("API rate limit exceeded\n")}, nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if liveRuns != 1 || res2.ExitCode != ExitRateLimited || !strings.Contains(string(res2.Stderr), RateLimitMarker) {
		t.Fatalf("live rate-limited auth status: runs=%d res=%+v stderr=%q", liveRuns, res2, string(res2.Stderr))
	}

	// Genuine not-logged-in auth status passes through unchanged (exit 1, no marker).
	l3, _ := newLimiter(t, clock)
	res3, err := l3.Run(context.Background(), []string{"auth", "status"}, func(context.Context) (Result, error) {
		return Result{ExitCode: 1, Stderr: []byte("You are not logged into any GitHub hosts.\n")}, nil
	})
	if err != nil || res3.ExitCode != 1 || strings.Contains(string(res3.Stderr), RateLimitMarker) {
		t.Fatalf("unauthenticated auth status = %+v, %v", res3, err)
	}
	_ = calls
}

func TestPaginateChargesPagesAndStopsRunawayTimeout(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	l.Config.PointsPerMin = 5
	var calls []CallEntry
	l.LogCall = func(e CallEntry) error {
		calls = append(calls, e)
		return nil
	}

	// A 3-page --paginate fixture (via ExecRunner) charges 3 points and logs pages=3.
	gh := fakeGH(t, `printf '{"data":{"page":1}}\n{"data":{"page":2}}\n{"data":{"page":3}}\n'`+"\n")
	c := runGH(t, l, gh, []string{"api", "graphql", "--paginate", "-f", "query={ viewer { login } }"}, "")
	if c.err != nil || c.res.ExitCode != 0 {
		t.Fatalf("paginate run = %+v, %v", c.res, c.err)
	}
	if len(calls) != 1 || calls[0].Pages != 3 || calls[0].Points != 3 {
		t.Fatalf("call entry = %+v; want pages=3 points=3", calls)
	}

	// Two more 1-point calls fit (3 + 2 = 5), then the 6th point waits for the window.
	before := clock.Now()
	for range 2 {
		if _, err := l.Run(context.Background(), []string{"api", "repos/o/r"}, func(context.Context) (Result, error) {
			return Result{}, nil
		}); err != nil {
			t.Fatal(err)
		}
	}
	if clock.Now().Sub(before) != 0 {
		t.Fatalf("calls 4 and 5 waited unexpectedly: %v", clock.Now().Sub(before))
	}
	if _, err := l.Run(context.Background(), []string{"api", "repos/o/r"}, func(context.Context) (Result, error) {
		return Result{}, nil
	}); err != nil {
		t.Fatal(err)
	}
	if waited := clock.Now().Sub(before); waited < 59*time.Second {
		t.Fatalf("6th point waited %v after 3-page paginate, want ~1m", waited)
	}

	// Runaway --paginate call is stopped by PaginateTimeout.
	l2, _ := newLimiter(t, nil)
	l2.Config.PaginateTimeout = 50 * time.Millisecond
	slowGH := fakeGH(t, "sleep 5\n")
	r, err := NewExecRunner(slowGH, []string{"api", "--paginate", "repos/o/r/pulls"}, strings.NewReader(""), io.Discard, io.Discard)
	if err != nil {
		t.Fatal(err)
	}
	start := time.Now()
	_, err = l2.Run(context.Background(), []string{"api", "--paginate", "repos/o/r/pulls"}, r.Run)
	elapsed := time.Since(start)
	if err == nil || elapsed > 2*time.Second {
		t.Fatalf("runaway paginate err=%v elapsed=%v; want quick timeout error", err, elapsed)
	}
}

func TestRetryDoublesCooldownAndUsesResetHeader(t *testing.T) {
	clock := newFakeClock()
	l1, _ := newLimiter(t, clock)
	l1.Config.Backoff = 10 * time.Second
	l1.Config.MaxBackoff = 50 * time.Second
	l2 := &Limiter{Dir: l1.Dir, Config: l1.Config, Now: clock.Now, Sleep: clock.Sleep}

	// Hit 1 has x-ratelimit-reset = now + 15s; retry also hits rate limit -> doubles 15s -> 30s.
	resetEpoch := strconv.FormatInt(clock.Now().Add(15*time.Second).Unix(), 10)
	attempts := 0
	res, err := l1.Run(context.Background(), []string{"pr", "view", "1"}, func(context.Context) (Result, error) {
		attempts++
		return Result{ExitCode: 1, Stderr: []byte("API rate limit exceeded\nx-ratelimit-reset: " + resetEpoch + "\n")}, nil
	})
	if err != nil || res.ExitCode != 1 || attempts != 2 {
		t.Fatalf("Run = %+v, %v (attempts=%d); want 2 attempts", res, err, attempts)
	}
	if len(clock.slept) != 1 || clock.slept[0] != 15*time.Second {
		t.Fatalf("slept %v, want 15s from x-ratelimit-reset", clock.slept)
	}

	// Caller l2 arrives immediately and must wait the doubled 30s cooldown.
	before := clock.Now()
	var l2Start time.Time
	_, err = l2.Run(context.Background(), []string{"pr", "view", "2"}, func(context.Context) (Result, error) {
		l2Start = clock.Now()
		return Result{}, nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if waited := l2Start.Sub(before); waited != 30*time.Second {
		t.Fatalf("second caller waited %v after doubled retry, want 30s", waited)
	}
}
