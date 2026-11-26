package main

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"slices"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

type lockedRefreshClient struct {
	mu        sync.Mutex
	prs       []models.PullRequest
	next      map[string][]string
	checks    map[string][]models.StatusCheck
	refreshed []string
	checked   []string
	sleeps    []time.Duration
	onRefresh func(url string) (string, error)
	onChecks  func(url string) ([]models.StatusCheck, error)
}

func (c *lockedRefreshClient) GetAuthoredPullRequests() ([]models.PullRequest, error) {
	return c.prs, nil
}

func (c *lockedRefreshClient) GetMergeablePullRequests() ([]models.PullRequest, error) {
	return nil, errors.New("unexpected GetMergeablePullRequests")
}

func (c *lockedRefreshClient) MergePullRequest(string, bool) error { return nil }

func (c *lockedRefreshClient) MergeableState(url string) (string, error) {
	if c.onRefresh != nil {
		c.mu.Lock()
		c.refreshed = append(c.refreshed, url)
		c.mu.Unlock()
		return c.onRefresh(url)
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.refreshed = append(c.refreshed, url)
	seq := c.next[url]
	if len(seq) == 0 {
		return "UNKNOWN", nil
	}
	c.next[url] = seq[1:]
	return seq[0], nil
}

func (c *lockedRefreshClient) StatusChecks(url string) ([]models.StatusCheck, error) {
	c.mu.Lock()
	c.checked = append(c.checked, url)
	fn := c.onChecks
	checks := c.checks[url]
	c.mu.Unlock()
	if fn != nil {
		return fn(url)
	}
	return checks, nil
}

func (c *lockedRefreshClient) sleep(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.sleeps = append(c.sleeps, d)
}

func TestList30UnknownPRsOnlyRefreshesCheapFilterPassers(t *testing.T) {
	withVerbose(t, true)
	prs := make([]models.PullRequest, 30)
	next := make(map[string][]string)
	wantRefreshed := make(map[string]bool)
	for i := range 30 {
		n := i + 1
		url := fmt.Sprintf("https://github.com/acme/widgets/pull/%d", n)
		pr := mergeFixture(fmt.Sprintf("PR %d", n), url)
		pr.Mergeable = "UNKNOWN"
		switch {
		case n <= 10:
			pr.IsDraft = true
		case n <= 20:
			pr.ReviewRequests = []models.ReviewRequest{{Type: "User", Login: "alice"}}
		case n <= 25:
			pr.Labels = []models.Label{{Name: "do-not-merge"}}
		default:
			wantRefreshed[url] = true
			if n == 30 {
				next[url] = []string{"UNKNOWN", "UNKNOWN"}
			} else {
				next[url] = []string{"MERGEABLE"}
			}
		}
		prs[i] = pr
	}

	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}}}
	client := &lockedRefreshClient{prs: prs, next: next}
	var stdout, stderr bytes.Buffer
	err := listPullRequests(context.Background(), client, cfg, listOptions{
		out:     &stdout,
		errOut:  &stderr,
		sleep:   client.sleep,
		maxConc: 2,
	})
	if err != nil {
		t.Fatalf("listPullRequests() error = %v", err)
	}
	for _, url := range client.refreshed {
		if !wantRefreshed[url] {
			t.Fatalf("refreshed cheap-filtered PR %s (refreshed=%v)", url, client.refreshed)
		}
	}
	for url := range wantRefreshed {
		if !slices.Contains(client.refreshed, url) {
			t.Fatalf("did not refresh eligible UNKNOWN PR %s (refreshed=%v)", url, client.refreshed)
		}
	}
	out := stdout.String()
	for n := 26; n <= 29; n++ {
		if !strings.Contains(out, fmt.Sprintf("URL: https://github.com/acme/widgets/pull/%d", n)) {
			t.Fatalf("missing ready PR %d in output:\n%s", n, out)
		}
	}
	if !strings.Contains(out, "skipped: https://github.com/acme/widgets/pull/30 unknown") {
		t.Fatalf("missing skipped unknown PR 30:\n%s", out)
	}
	if !strings.Contains(out, "skipped: https://github.com/acme/widgets/pull/21 blocking label do-not-merge, unknown") {
		t.Fatalf("missing skipped blocked unknown PR 21:\n%s", out)
	}
}

func TestList30UnknownPRsBatchedQueryExcludesCheapFiltered(t *testing.T) {
	withVerbose(t, true)
	prs := make([]models.PullRequest, 30)
	for i := range 30 {
		n := i + 1
		url := fmt.Sprintf("https://github.com/acme/widgets/pull/%d", n)
		pr := mergeFixture(fmt.Sprintf("PR %d", n), url)
		pr.Mergeable = "UNKNOWN"
		switch {
		case n <= 10:
			pr.IsDraft = true
		case n <= 20:
			pr.ReviewRequests = []models.ReviewRequest{{Type: "User", Login: "alice"}}
		case n <= 26:
			pr.Labels = []models.Label{{Name: "do-not-merge"}}
		}
		prs[i] = pr
	}

	unknownList := slices.Clone(prs)
	for i := 26; i < 30; i++ {
		prs[i].Mergeable = "MERGEABLE"
	}

	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}}}
	// Wrap sweepExec so pr list sees UNKNOWN while graphql sees MERGEABLE for 27..30.
	wrapped := &splitSweepExec{
		listExec: &sweepExec{t: t, list: unknownList, checks: map[string][]models.StatusCheck{}},
		gqlExec:  &sweepExec{t: t, list: prs, checks: map[string][]models.StatusCheck{}},
	}
	client := github.NewClientWithExecutor(wrapped)
	var stdout, stderr bytes.Buffer
	err := listPullRequests(context.Background(), client, cfg, listOptions{
		out:    &stdout,
		errOut: &stderr,
		sleep:  func(time.Duration) {},
	})
	if err != nil {
		t.Fatalf("listPullRequests() error = %v", err)
	}
	for n := 27; n <= 30; n++ {
		if !strings.Contains(stdout.String(), fmt.Sprintf("URL: https://github.com/acme/widgets/pull/%d", n)) {
			t.Fatalf("missing ready PR %d:\n%s", n, stdout.String())
		}
	}
	if len(wrapped.gqlExec.calls) != 1 {
		t.Fatalf("graphql calls = %d, want 1", len(wrapped.gqlExec.calls))
	}
	query := wrapped.gqlExec.calls[0][len(wrapped.gqlExec.calls[0])-1]
	for n := 1; n <= 26; n++ {
		if strings.Contains(query, fmt.Sprintf("pullRequest(number:%d)", n)) {
			t.Fatalf("batched refresh included cheap-filtered PR %d:\n%s", n, query)
		}
	}
	for n := 27; n <= 30; n++ {
		if !strings.Contains(query, fmt.Sprintf("pullRequest(number:%d)", n)) {
			t.Fatalf("batched refresh omitted eligible PR %d:\n%s", n, query)
		}
	}
}

type splitSweepExec struct {
	listExec *sweepExec
	gqlExec  *sweepExec
}

func (s *splitSweepExec) Execute(cmd string, args ...string) (string, error) {
	out, _, err := s.ExecuteSeparated(cmd, args...)
	return out, err
}

func (s *splitSweepExec) ExecuteWithStdin(_ string, cmd string, args ...string) (string, error) {
	return s.Execute(cmd, args...)
}

func (s *splitSweepExec) ExecuteSeparated(cmd string, args ...string) (string, string, error) {
	if len(args) >= 2 && args[0] == "pr" && args[1] == "list" {
		return s.listExec.ExecuteSeparated(cmd, args...)
	}
	return s.gqlExec.ExecuteSeparated(cmd, args...)
}

func TestListStreamsFirstPRBeforeLastChecked(t *testing.T) {
	withVerbose(t, true)
	draft := mergeFixture("Draft PR", "https://github.com/acme/widgets/pull/1")
	draft.IsDraft = true
	pr2 := mergeFixture("Fast Ready PR", "https://github.com/acme/widgets/pull/2")
	pr3 := mergeFixture("Slow Ready PR", "https://github.com/acme/widgets/pull/3")

	pr2Written := make(chan struct{})
	var pr2Once sync.Once
	outR, outW := io.Pipe()
	var errOut bytes.Buffer

	var collected strings.Builder
	readDone := make(chan error, 1)
	go func() {
		scanner := bufio.NewScanner(outR)
		for scanner.Scan() {
			line := scanner.Text()
			collected.WriteString(line)
			collected.WriteByte('\n')
			if strings.Contains(line, "URL: "+pr2.URL) {
				pr2Once.Do(func() { close(pr2Written) })
			}
		}
		readDone <- scanner.Err()
	}()

	var pr3Started atomic.Bool
	client := &lockedRefreshClient{
		prs: []models.PullRequest{draft, pr2, pr3},
		onChecks: func(url string) ([]models.StatusCheck, error) {
			if url == pr3.URL {
				pr3Started.Store(true)
				select {
				case <-pr2Written:
				case <-time.After(5 * time.Second):
					return nil, errors.New("timed out waiting for PR 2 to stream before PR 3 finished")
				}
			}
			return []models.StatusCheck{{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"}}, nil
		},
	}

	err := listPullRequests(context.Background(), client, &config.Config{}, listOptions{
		out:     outW,
		errOut:  &errOut,
		sleep:   client.sleep,
		maxConc: 2,
	})
	_ = outW.Close()
	if readErr := <-readDone; readErr != nil {
		t.Fatalf("read pipe: %v", readErr)
	}
	if err != nil {
		t.Fatalf("listPullRequests() error = %v", err)
	}
	if !strings.Contains(errOut.String(), "checking 3 PRs") {
		t.Fatalf("stderr = %q, want checking 3 PRs", errOut.String())
	}
	out := collected.String()
	if !strings.Contains(out, "skipped: "+draft.URL+" draft") {
		t.Fatalf("missing skipped draft line:\n%s", out)
	}
	if !strings.Contains(out, "URL: "+pr2.URL) || !strings.Contains(out, "URL: "+pr3.URL) {
		t.Fatalf("missing ready PRs:\n%s", out)
	}
	_ = pr3Started.Load()
}

func TestListPRFilterLimitsCalls(t *testing.T) {
	prs := sweepFixture()
	exec := newSweepExec(t)
	client := github.NewClientWithExecutor(exec)

	var stdout, stderr bytes.Buffer
	err := listPullRequests(context.Background(), client, &config.Config{Settings: config.SettingsConfig{RequiredChecks: []string{"ci"}}}, listOptions{
		prRefs:     []string{"acme/widgets#1", "https://github.com/acme/widgets/pull/3"},
		out:        &stdout,
		errOut:     &stderr,
		resolveRef: func(s string) (string, error) { return s, nil },
	})
	if err != nil {
		t.Fatalf("listPullRequests() error = %v", err)
	}
	if lists := exec.callsWith("list", "--json"); len(lists) != 0 {
		t.Fatalf("--pr should not run pr list, got: %v", exec.calls)
	}
	if len(exec.calls) != 1 {
		t.Fatalf("gh calls = %d, want 1 batched graphql call: %v", len(exec.calls), exec.calls)
	}
	query := exec.calls[0][len(exec.calls[0])-1]
	for _, n := range []int{1, 3} {
		if !strings.Contains(query, fmt.Sprintf("pullRequest(number:%d)", n)) {
			t.Fatalf("query missing PR %d:\n%s", n, query)
		}
	}
	for _, n := range []int{2, 4, 5, 6, 7, 8, 9, 10} {
		if strings.Contains(query, fmt.Sprintf("pullRequest(number:%d)", n)) {
			t.Fatalf("query unexpectedly included PR %d:\n%s", n, query)
		}
	}
	if !strings.Contains(stdout.String(), "URL: "+prs[0].URL) || strings.Contains(stdout.String(), "URL: "+prs[1].URL) {
		t.Fatalf("output mismatch:\n%s", stdout.String())
	}

	// Non-batched client also limits MergeableState / StatusChecks calls to --pr refs.
	unknown := make([]models.PullRequest, 5)
	for i := range unknown {
		n := i + 1
		unknown[i] = mergeFixture(fmt.Sprintf("PR %d", n), fmt.Sprintf("https://github.com/acme/widgets/pull/%d", n))
		unknown[i].Mergeable = "UNKNOWN"
	}
	fake := &lockedRefreshClient{
		prs:  unknown,
		next: map[string][]string{unknown[1].URL: {"MERGEABLE"}},
	}
	stdout.Reset()
	stderr.Reset()
	err = listPullRequests(context.Background(), fake, &config.Config{}, listOptions{
		prRefs:     []string{"acme/widgets#2"},
		out:        &stdout,
		errOut:     &stderr,
		sleep:      fake.sleep,
		resolveRef: func(s string) (string, error) { return s, nil },
	})
	if err != nil {
		t.Fatalf("fallback listPullRequests() error = %v", err)
	}
	if !slices.Equal(fake.refreshed, []string{unknown[1].URL}) {
		t.Fatalf("refreshed = %v, want only %s", fake.refreshed, unknown[1].URL)
	}
	if !slices.Equal(fake.checked, []string{unknown[1].URL}) {
		t.Fatalf("checked = %v, want only %s", fake.checked, unknown[1].URL)
	}
}

func TestListTimeoutPrintsPartialResultsAndNotChecked(t *testing.T) {
	withVerbose(t, true)
	draft := mergeFixture("Draft PR", "https://github.com/acme/widgets/pull/1")
	draft.IsDraft = true
	pr2 := mergeFixture("Fast PR", "https://github.com/acme/widgets/pull/2")
	pr3 := mergeFixture("Blocked PR 3", "https://github.com/acme/widgets/pull/3")
	pr4 := mergeFixture("Blocked PR 4", "https://github.com/acme/widgets/pull/4")

	pr2Done := make(chan struct{})
	release := make(chan struct{})
	t.Cleanup(func() { close(release) })

	client := &lockedRefreshClient{
		prs: []models.PullRequest{draft, pr2, pr3, pr4},
		onChecks: func(url string) ([]models.StatusCheck, error) {
			if url == pr2.URL {
				defer close(pr2Done)
				return []models.StatusCheck{{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"}}, nil
			}
			<-pr2Done
			<-release
			return nil, nil
		},
	}

	var stdout, stderr bytes.Buffer
	ctx, cancel := context.WithCancel(context.Background())
	go func() {
		<-pr2Done
		time.Sleep(20 * time.Millisecond)
		cancel()
	}()

	err := listPullRequests(ctx, client, &config.Config{}, listOptions{
		timeout: 50 * time.Millisecond,
		out:     &stdout,
		errOut:  &stderr,
		sleep:   client.sleep,
		maxConc: 1,
	})
	if err == nil {
		t.Fatal("expected non-nil error on timeout")
	}
	out := stdout.String()
	if !strings.Contains(out, "skipped: "+draft.URL+" draft") {
		t.Fatalf("missing decided skipped PR 1:\n%s", out)
	}
	if !strings.Contains(out, "URL: "+pr2.URL) {
		t.Fatalf("missing decided ready PR 2:\n%s", out)
	}
	if !strings.Contains(out, "not checked: 2") {
		t.Fatalf("missing not checked count:\n%s", out)
	}
	if strings.Contains(out, pr3.URL) || strings.Contains(out, pr4.URL) || strings.Contains(out, "No PRs with no review requests found.") {
		t.Fatalf("unexpected PRs or empty banner on timeout:\n%s", out)
	}
}

func TestListTimeoutDuringBatchedUnknownPoll(t *testing.T) {
	withVerbose(t, true)
	pr1 := mergeFixture("Ready PR", "https://github.com/acme/widgets/pull/1")
	pr2 := mergeFixture("Unknown PR", "https://github.com/acme/widgets/pull/2")
	pr2.Mergeable = "UNKNOWN"

	exec := &splitSweepExec{
		listExec: &sweepExec{t: t, list: []models.PullRequest{pr1, pr2}, checks: map[string][]models.StatusCheck{}},
		gqlExec:  &sweepExec{t: t, list: []models.PullRequest{pr1, pr2}, checks: map[string][]models.StatusCheck{}},
	}
	client := github.NewClientWithExecutor(exec)

	start := time.Now()
	var stdout, stderr bytes.Buffer
	err := listPullRequests(context.Background(), client, &config.Config{}, listOptions{
		timeout: 30 * time.Millisecond,
		out:     &stdout,
		errOut:  &stderr,
	})
	elapsed := time.Since(start)
	if err == nil {
		t.Fatal("expected timeout error")
	}
	if elapsed > 500*time.Millisecond {
		t.Fatalf("timeout took %s, expected prompt exit near 30ms", elapsed)
	}
	out := stdout.String()
	if !strings.Contains(out, "URL: "+pr1.URL) {
		t.Fatalf("missing ready PR 1 before timeout:\n%s", out)
	}
	if !strings.Contains(out, "not checked: 1") {
		t.Fatalf("missing not checked: 1:\n%s", out)
	}
}

func TestListBoundedConcurrencyRespectsLimit(t *testing.T) {
	prs := make([]models.PullRequest, 6)
	for i := range prs {
		n := i + 1
		prs[i] = mergeFixture(fmt.Sprintf("PR %d", n), fmt.Sprintf("https://github.com/acme/widgets/pull/%d", n))
	}

	for _, limit := range []int{1, 2} {
		t.Run(fmt.Sprintf("limit=%d", limit), func(t *testing.T) {
			var active, maxActive atomic.Int32
			client := &lockedRefreshClient{
				prs: prs,
				onChecks: func(string) ([]models.StatusCheck, error) {
					cur := active.Add(1)
					for {
						prev := maxActive.Load()
						if cur <= prev || maxActive.CompareAndSwap(prev, cur) {
							break
						}
					}
					time.Sleep(10 * time.Millisecond)
					active.Add(-1)
					return nil, nil
				},
			}
			var stdout, stderr bytes.Buffer
			err := listPullRequests(context.Background(), client, &config.Config{}, listOptions{
				out:     &stdout,
				errOut:  &stderr,
				maxConc: limit,
			})
			if err != nil {
				t.Fatalf("listPullRequests() error = %v", err)
			}
			if got := int(maxActive.Load()); got > limit {
				t.Fatalf("maxActive = %d, exceeded limit %d", got, limit)
			}
			if limit == 2 && int(maxActive.Load()) < 2 {
				t.Fatalf("maxActive = %d, expected concurrent execution up to 2", maxActive.Load())
			}
		})
	}
}

func TestListDebugLogsPhaseAndCallTimingsToStderr(t *testing.T) {
	var logBuf bytes.Buffer
	prevLog := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&logBuf, &slog.HandlerOptions{Level: slog.LevelDebug})))
	t.Cleanup(func() { slog.SetDefault(prevLog) })

	exec := newSweepExec(t)
	client := github.NewClientWithExecutor(exec)
	var stdout, stderr bytes.Buffer
	err := listPullRequests(context.Background(), client, &config.Config{}, listOptions{
		out:    &stdout,
		errOut: &stderr,
	})
	if err != nil {
		t.Fatalf("listPullRequests() error = %v", err)
	}
	logs := logBuf.String()
	for _, want := range []string{"phase=fetch_prs", "phase=cheap_filter", "phase=check_candidates", "phase=total", "gh call", "duration="} {
		if !strings.Contains(logs, want) {
			t.Fatalf("debug logs missing %q:\n%s", want, logs)
		}
	}
	if strings.Contains(stdout.String(), "level=DEBUG") || strings.Contains(stdout.String(), "level=INFO") {
		t.Fatalf("logs leaked to stdout:\n%s", stdout.String())
	}
}

func TestSetupLoggingWritesToStderrAndParsesLevel(t *testing.T) {
	prevLevel := logLevelFlag
	prevVerbose := verbose
	prevLogger := slog.Default()
	t.Cleanup(func() {
		logLevelFlag = prevLevel
		verbose = prevVerbose
		slog.SetDefault(prevLogger)
	})

	logLevelFlag = "debug"
	verbose = false
	stdout, stderr := captureOutputs(t, func() {
		logger, err := setupLogging()
		if err != nil {
			t.Fatalf("setupLogging() error = %v", err)
		}
		logger.Debug("timing check", "duration", "10ms")
	})
	if stdout != "" {
		t.Fatalf("setupLogging wrote to stdout: %q", stdout)
	}
	if !strings.Contains(stderr, "timing check") {
		t.Fatalf("stderr missing debug log: %q", stderr)
	}

	logLevelFlag = "loud"
	if _, err := setupLogging(); err == nil {
		t.Fatal("expected error for invalid --log-level")
	}
}

func TestParseInvocationAndListArgs(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		flagPRs     []string
		flagTimeout time.Duration
		timeoutSet  bool
		wantCmd     string
		wantPRs     []string
		wantTimeout time.Duration
		wantErr     bool
	}{
		{
			name:    "default empty runs list",
			wantCmd: "list",
		},
		{
			name:    "positional numbers invoke list",
			args:    []string{"12", "34"},
			wantCmd: "list",
			wantPRs: []string{"12", "34"},
		},
		{
			name:        "list subcommand with trailing flags and PRs",
			args:        []string{"list", "--pr", "12,34", "--timeout", "15s", "acme/widgets#56"},
			wantCmd:     "list",
			wantPRs:     []string{"12", "34", "acme/widgets#56"},
			wantTimeout: 15 * time.Second,
		},
		{
			name:        "top-level timeout propagates to list",
			flagPRs:     []string{"10"},
			flagTimeout: 20 * time.Second,
			timeoutSet:  true,
			wantCmd:     "list",
			wantPRs:     []string{"10"},
			wantTimeout: 20 * time.Second,
		},
		{
			name:    "pr flag rejected with merge",
			args:    []string{"merge"},
			flagPRs: []string{"10"},
			wantErr: true,
		},
		{
			name:    "negative timeout rejected",
			args:    []string{"list", "--timeout", "-1s"},
			wantErr: true,
		},
		{
			name:    "unknown command rejected",
			args:    []string{"bogus"},
			wantErr: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := listOptions{prRefs: slices.Clone(tc.flagPRs)}
			if tc.timeoutSet {
				opts.timeout = tc.flagTimeout
			}
			cmd, _, gotOpts, err := resolveInvocation(tc.args, opts)
			if (err != nil) != tc.wantErr {
				t.Fatalf("resolveInvocation() err = %v, wantErr %v", err, tc.wantErr)
			}
			if tc.wantErr {
				return
			}
			if cmd != tc.wantCmd {
				t.Fatalf("cmd = %q, want %q", cmd, tc.wantCmd)
			}
			if !slices.Equal(gotOpts.prRefs, tc.wantPRs) {
				t.Fatalf("prRefs = %v, want %v", gotOpts.prRefs, tc.wantPRs)
			}
			if gotOpts.timeout != tc.wantTimeout {
				t.Fatalf("timeout = %s, want %s", gotOpts.timeout, tc.wantTimeout)
			}
		})
	}
}
