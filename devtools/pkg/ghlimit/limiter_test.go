package ghlimit

import (
	"bytes"
	"context"
	"io"
	"os"
	"path/filepath"
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
