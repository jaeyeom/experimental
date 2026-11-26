package main

import (
	"bufio"
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"log/slog"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

const candidateBatchSize = 50

var (
	errUnknownCommand = errors.New("unknown command")
	errListUsage      = errors.New("invalid list arguments")
)

type prRefFlag []string

func (f *prRefFlag) String() string {
	return strings.Join(*f, ",")
}

func (f *prRefFlag) Set(value string) error {
	for _, part := range strings.Split(value, ",") {
		part = strings.TrimSpace(part)
		if part != "" {
			*f = append(*f, part)
		}
	}
	return nil
}

type listOptions struct {
	prRefs     []string
	timeout    time.Duration
	out        io.Writer
	errOut     io.Writer
	sleep      func(time.Duration)
	resolveRef func(string) (string, error)
	maxConc    int
}

type authoredPullRequestsContext interface {
	GetAuthoredPullRequestsContext(ctx context.Context) ([]models.PullRequest, error)
}

type pullRequestsByRefReader interface {
	PullRequestsByRefContext(ctx context.Context, refs []github.PullRef) ([]models.PullRequest, error)
}

type candidateStatesReader interface {
	CandidateStatesContext(ctx context.Context, prURLs []string) (map[string]github.CandidateState, error)
}

type mergeableContextReader interface {
	MergeableStateContext(ctx context.Context, prURL string) (string, error)
}

type statusChecksContextReader interface {
	StatusChecksContext(ctx context.Context, prURL string) ([]models.StatusCheck, error)
}

func initialListOptions() listOptions {
	opts := listOptions{
		prRefs: append([]string(nil), prListFlag...),
	}
	flag.Visit(func(f *flag.Flag) {
		if f.Name == "timeout" {
			opts.timeout = watchTimeout
		}
	})
	return opts
}

func resolveInvocation(args []string, initial listOptions) (command string, cmdArgs []string, opts listOptions, err error) {
	opts = initial
	if len(args) == 0 {
		return "list", nil, opts, validateListTimeout(opts.timeout)
	}
	switch args[0] {
	case "list":
		if err := parseListArgs(args[1:], &opts); err != nil {
			return "", nil, opts, err
		}
		return "list", args[1:], opts, validateListTimeout(opts.timeout)
	case "merge", "queue", "watch", "rca", "evidence", "dequeue", "handle-ejection":
		if len(opts.prRefs) > 0 {
			return "", nil, opts, fmt.Errorf("%w: --pr is only supported for list", errListUsage)
		}
		return args[0], args, opts, nil
	default:
		if looksLikePullRef(args[0]) {
			if err := parseListArgs(args, &opts); err != nil {
				return "", nil, opts, err
			}
			return "list", args, opts, validateListTimeout(opts.timeout)
		}
		return args[0], args, opts, fmt.Errorf("%w: %s", errUnknownCommand, args[0])
	}
}

func validateListTimeout(d time.Duration) error {
	if d < 0 {
		return fmt.Errorf("%w: timeout must be non-negative (got %s)", errListUsage, d)
	}
	return nil
}

func looksLikePullRef(raw string) bool {
	raw = strings.TrimSpace(raw)
	if _, ok := barePullNumber(raw); ok {
		return true
	}
	_, err := github.ParsePullRef(raw)
	return err == nil
}

func parseListArgs(args []string, opts *listOptions) error {
	for i := 0; i < len(args); i++ {
		arg := args[i]
		if !strings.HasPrefix(arg, "-") {
			if !looksLikePullRef(arg) {
				return fmt.Errorf("%w: invalid pull request %q", errListUsage, arg)
			}
			opts.prRefs = append(opts.prRefs, strings.TrimSpace(arg))
			continue
		}
		name, value, hasValue := strings.Cut(arg, "=")
		name = strings.TrimPrefix(strings.TrimPrefix(name, "-"), "-")
		switch name {
		case "v":
			if hasValue {
				v, err := strconv.ParseBool(value)
				if err != nil {
					return fmt.Errorf("%w: invalid -v value %q", errListUsage, value)
				}
				verbose = v
			} else {
				verbose = true
			}
		case "pr", "timeout", "log-level", "config":
			if !hasValue {
				if i+1 >= len(args) {
					return fmt.Errorf("%w: missing value for --%s", errListUsage, name)
				}
				i++
				value = args[i]
			}
			if err := applyListFlagValue(name, value, opts); err != nil {
				return err
			}
		default:
			return fmt.Errorf("%w: unknown flag %s", errListUsage, arg)
		}
	}
	return nil
}

func applyListFlagValue(name, value string, opts *listOptions) error {
	switch name {
	case "pr":
		var vals prRefFlag
		_ = vals.Set(value)
		if len(vals) == 0 {
			return fmt.Errorf("%w: empty --pr value", errListUsage)
		}
		opts.prRefs = append(opts.prRefs, vals...)
	case "timeout":
		d, err := time.ParseDuration(value)
		if err != nil {
			return fmt.Errorf("%w: invalid --timeout %q: %v", errListUsage, value, err)
		}
		opts.timeout = d
	case "log-level":
		logLevelFlag = value
	case "config":
		configPath = value
	}
	return nil
}

func resolveLogLevel(flagValue string, verboseOn bool) (slog.Level, error) {
	if strings.TrimSpace(flagValue) != "" {
		switch strings.ToLower(strings.TrimSpace(flagValue)) {
		case "debug":
			return slog.LevelDebug, nil
		case "info":
			return slog.LevelInfo, nil
		case "warn", "warning":
			return slog.LevelWarn, nil
		case "error":
			return slog.LevelError, nil
		default:
			return 0, fmt.Errorf("invalid --log-level %q (want debug, info, warn, or error)", flagValue)
		}
	}
	if verboseOn {
		return slog.LevelDebug, nil
	}
	return slog.LevelInfo, nil
}

type listWriter struct {
	w   *bufio.Writer
	err error
}

func newListWriter(out io.Writer) *listWriter {
	if out == nil {
		out = os.Stdout
	}
	return &listWriter{w: bufio.NewWriter(out)}
}

func (l *listWriter) printf(format string, args ...any) {
	if l.err != nil {
		return
	}
	if _, err := fmt.Fprintf(l.w, format, args...); err != nil {
		l.err = err
		return
	}
	l.err = l.w.Flush()
}

func (l *listWriter) line(s string) {
	l.printf("%s\n", s)
}

func writeProgress(errOut io.Writer, msg string) {
	if errOut == nil {
		errOut = os.Stderr
	}
	_, _ = fmt.Fprintln(errOut, msg)
	if flusher, ok := errOut.(interface{ Flush() error }); ok {
		_ = flusher.Flush()
	}
}

func normalizeListContext(ctx context.Context, opts listOptions) (context.Context, context.CancelFunc, listOptions) {
	if ctx == nil {
		ctx = context.Background()
	}
	cancel := func() {}
	if opts.timeout > 0 {
		ctx, cancel = context.WithTimeout(ctx, opts.timeout)
	}
	if opts.out == nil {
		opts.out = os.Stdout
	}
	if opts.errOut == nil {
		opts.errOut = os.Stderr
	}
	if opts.resolveRef == nil {
		opts.resolveRef = resolvePullRef
	}
	if opts.maxConc <= 0 {
		opts.maxConc = github.SuggestedConcurrency()
	}
	return ctx, cancel, opts
}

func listPullRequests(ctx context.Context, githubClient mergeClient, cfg *config.Config, opts listOptions) error {
	ctx, cancel, opts := normalizeListContext(ctx, opts)
	defer cancel()

	totalStart := time.Now()
	fetchStart := time.Now()
	fetched, checksLoaded, refresh, err := fetchListCandidates(ctx, githubClient, opts)
	slog.Debug("list phase",
		"phase", "fetch_prs",
		"duration", time.Since(fetchStart).Round(time.Millisecond),
		"count", len(fetched),
		"pr_filter", len(opts.prRefs),
	)
	if err != nil {
		return fmt.Errorf("failed to get PRs: %w", err)
	}

	writeProgress(opts.errOut, fmt.Sprintf("checking %d PRs", len(fetched)))
	logMergeScan(cfg, fetched, "Finding PRs with no review requests in the current repository")

	sw := newListWriter(opts.out)
	if len(fetched) == 0 {
		sw.line("No PRs with no review requests found.")
		slog.Debug("list phase", "phase", "total", "duration", time.Since(totalStart).Round(time.Millisecond), "total", 0, "ready", 0, "not_checked", 0)
		return sw.err
	}

	candidates := filterCheapCandidates(sw, cfg, fetched)
	checkStart := time.Now()
	checkedCount, readyCount, err := checkAndStreamCandidates(ctx, sw, githubClient, cfg, candidates, checksLoaded, refresh, opts)
	notChecked := len(candidates) - checkedCount
	slog.Debug("list phase",
		"phase", "check_candidates",
		"duration", time.Since(checkStart).Round(time.Millisecond),
		"checked", checkedCount,
		"ready", readyCount,
		"not_checked", notChecked,
	)
	slog.Debug("list phase",
		"phase", "total",
		"duration", time.Since(totalStart).Round(time.Millisecond),
		"total", len(fetched),
		"ready", readyCount,
		"not_checked", notChecked,
	)
	return finishListOutput(ctx, sw, readyCount, notChecked, err)
}

func filterCheapCandidates(sw *listWriter, cfg *config.Config, fetched []models.PullRequest) []models.PullRequest {
	filterStart := time.Now()
	candidates := make([]models.PullRequest, 0, len(fetched))
	for i := range fetched {
		pr := &fetched[i]
		if eligibleCandidate(cfg, pr) {
			candidates = append(candidates, *pr)
			continue
		}
		emitVerdict(sw, cfg, pr)
	}
	slog.Debug("list phase",
		"phase", "cheap_filter",
		"duration", time.Since(filterStart).Round(time.Millisecond),
		"total", len(fetched),
		"candidates", len(candidates),
		"skipped", len(fetched)-len(candidates),
	)
	return candidates
}

func finishListOutput(ctx context.Context, sw *listWriter, readyCount, notChecked int, err error) error {
	if err != nil && !isContextTimeoutErr(ctx, err) {
		return err
	}
	if ctx.Err() != nil && notChecked > 0 {
		sw.printf("not checked: %d\n", notChecked)
		if sw.err != nil {
			return sw.err
		}
		return fmt.Errorf("listing timed out: %d PR(s) not checked: %w", notChecked, ctx.Err())
	}
	if err != nil {
		return err
	}
	if readyCount == 0 {
		sw.line("No PRs with no review requests found.")
	} else {
		sw.printf("Found %d PRs with no review requests.\n", readyCount)
	}
	return sw.err
}

func isContextTimeoutErr(ctx context.Context, err error) bool {
	if ctx != nil && ctx.Err() != nil {
		return true
	}
	return errors.Is(err, context.DeadlineExceeded) || errors.Is(err, context.Canceled)
}

func eligibleCandidate(cfg *config.Config, pr *models.PullRequest) bool {
	if pr.IsDraft || len(pr.ReviewRequests) > 0 {
		return false
	}
	switch pr.Mergeable {
	case "MERGEABLE":
		return true
	case "UNKNOWN":
		_, blocked := pr.BlocksMerge(blockLabelsFor(cfg, pr))
		return !blocked
	default:
		return false
	}
}

func fetchListCandidates(ctx context.Context, githubClient mergeClient, opts listOptions) ([]models.PullRequest, bool, func(string) (string, error), error) {
	var refs []github.PullRef
	var err error
	if len(opts.prRefs) > 0 {
		refs, err = resolvePullRefs(opts.prRefs, opts.resolveRef)
		if err != nil {
			return nil, false, nil, err
		}
		if byRef, ok := githubClient.(pullRequestsByRefReader); ok {
			prs, err := byRef.PullRequestsByRefContext(ctx, refs)
			if err != nil {
				return nil, false, nil, fmt.Errorf("load pull requests: %w", err)
			}
			return prs, true, mergeableRefreshFunc(ctx, githubClient), nil
		}
	}

	fetched, refresh, err := fetchListPullRequestsContext(ctx, githubClient)
	if err != nil {
		return nil, false, nil, err
	}
	if len(refs) > 0 {
		fetched = filterPullRequestsByRefs(fetched, refs)
	}
	return fetched, false, refresh, nil
}

func resolvePullRefs(rawRefs []string, resolveFn func(string) (string, error)) ([]github.PullRef, error) {
	refs := make([]github.PullRef, 0, len(rawRefs))
	seen := make(map[string]struct{}, len(rawRefs))
	for _, raw := range rawRefs {
		resolved, err := resolveFn(raw)
		if err != nil {
			return nil, err
		}
		ref, err := github.ParsePullRef(resolved)
		if err != nil {
			return nil, fmt.Errorf("parse pull request %q: %w", resolved, err)
		}
		key := strings.ToLower(fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number))
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		refs = append(refs, ref)
	}
	return refs, nil
}

func filterPullRequestsByRefs(prs []models.PullRequest, refs []github.PullRef) []models.PullRequest {
	want := make(map[string]struct{}, len(refs))
	for _, ref := range refs {
		want[strings.ToLower(fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number))] = struct{}{}
	}
	filtered := make([]models.PullRequest, 0, len(refs))
	for _, pr := range prs {
		owner, repo, number, ok := pullRefFromURL(pr.URL)
		if !ok {
			continue
		}
		key := strings.ToLower(fmt.Sprintf("%s/%s#%d", owner, repo, number))
		if _, match := want[key]; match {
			filtered = append(filtered, pr)
		}
	}
	return filtered
}

func pullRefFromURL(prURL string) (string, string, int, bool) {
	ref, err := github.ParsePullRef(prURL)
	if err != nil {
		return "", "", 0, false
	}
	return ref.Owner, ref.Repo, ref.Number, true
}

func fetchListPullRequestsContext(ctx context.Context, githubClient mergeClient) ([]models.PullRequest, func(string) (string, error), error) {
	if authoredCtx, ok := githubClient.(authoredPullRequestsContext); ok {
		prs, err := authoredCtx.GetAuthoredPullRequestsContext(ctx)
		if err != nil {
			return nil, nil, fmt.Errorf("get authored pull requests: %w", err)
		}
		return prs, mergeableRefreshFunc(ctx, githubClient), nil
	}
	if authored, ok := githubClient.(authoredPullRequests); ok {
		prs, err := authored.GetAuthoredPullRequests()
		if err != nil {
			return nil, nil, fmt.Errorf("get authored pull requests: %w", err)
		}
		return prs, mergeableRefreshFunc(ctx, githubClient), nil
	}
	prs, err := githubClient.GetMergeablePullRequests()
	if err != nil {
		return nil, nil, fmt.Errorf("get mergeable pull requests: %w", err)
	}
	return prs, nil, nil
}

func mergeableRefreshFunc(ctx context.Context, githubClient any) func(string) (string, error) {
	if readerCtx, ok := githubClient.(mergeableContextReader); ok {
		return func(prURL string) (string, error) {
			return readerCtx.MergeableStateContext(ctx, prURL)
		}
	}
	if reader, ok := githubClient.(mergeableReader); ok {
		return reader.MergeableState
	}
	return nil
}

func checkAndStreamCandidates(
	ctx context.Context,
	sw *listWriter,
	githubClient any,
	cfg *config.Config,
	candidates []models.PullRequest,
	checksLoaded bool,
	refresh func(string) (string, error),
	opts listOptions,
) (checkedCount, readyCount int, err error) {
	if len(candidates) == 0 {
		return 0, 0, nil
	}

	stream := func(pr *models.PullRequest) {
		emitVerdict(sw, cfg, pr)
		checkedCount++
		if listReady(pr) {
			readyCount++
		}
	}

	if checksLoaded {
		var remaining []models.PullRequest
		for i := range candidates {
			if candidates[i].Mergeable != "UNKNOWN" {
				stream(&candidates[i])
				continue
			}
			remaining = append(remaining, candidates[i])
		}
		candidates = remaining
		if len(candidates) == 0 {
			return checkedCount, readyCount, nil
		}
	}

	if batchReader, ok := githubClient.(candidateStatesReader); ok {
		err = checkCandidatesBatched(ctx, batchReader, candidates, checksLoaded, opts.sleep, stream)
		return checkedCount, readyCount, err
	}

	err = checkCandidatesPerPR(ctx, githubClient, refresh, candidates, checksLoaded, opts, stream)
	return checkedCount, readyCount, err
}

func checkCandidatesBatched(
	ctx context.Context,
	reader candidateStatesReader,
	candidates []models.PullRequest,
	checksLoaded bool,
	sleep func(time.Duration),
	stream func(*models.PullRequest),
) error {
	unknowns := candidates
	if !checksLoaded && hasMergeableCandidate(candidates) {
		var err error
		unknowns, err = runCandidateStateBatches(ctx, reader, candidates, "candidate_states", stream)
		if err != nil {
			return err
		}
	}

	const pollBudget = 2 * time.Second
	const pollDelay = time.Second
	var slept time.Duration
	for len(unknowns) > 0 && slept < pollBudget {
		if ctx.Err() != nil {
			return fmt.Errorf("candidate poll cancelled: %w", ctx.Err())
		}
		wait := min(pollDelay, pollBudget-slept)
		if err := sleepWithContext(ctx, sleep, wait); err != nil {
			return err
		}
		slept += wait

		var err error
		unknowns, err = runCandidateStateBatches(ctx, reader, unknowns, "candidate_states_refresh", stream)
		if err != nil {
			return err
		}
	}

	if ctx.Err() != nil {
		return fmt.Errorf("candidate checks cancelled: %w", ctx.Err())
	}
	for i := range unknowns {
		stream(&unknowns[i])
	}
	return nil
}

func hasMergeableCandidate(candidates []models.PullRequest) bool {
	for i := range candidates {
		if candidates[i].Mergeable == "MERGEABLE" {
			return true
		}
	}
	return false
}

func runCandidateStateBatches(
	ctx context.Context,
	reader candidateStatesReader,
	prs []models.PullRequest,
	op string,
	stream func(*models.PullRequest),
) ([]models.PullRequest, error) {
	var stillUnknown []models.PullRequest
	for start, batchIdx := 0, 0; start < len(prs); start, batchIdx = start+candidateBatchSize, batchIdx+1 {
		if ctx.Err() != nil {
			return nil, fmt.Errorf("candidate checks cancelled: %w", ctx.Err())
		}
		end := min(start+candidateBatchSize, len(prs))
		chunk := prs[start:end]
		states, err := fetchCandidateBatch(ctx, reader, chunk, op, batchIdx)
		if err != nil {
			return nil, err
		}
		for i := range chunk {
			pr := &chunk[i]
			applyCandidateState(pr, states[pr.URL])
			if pr.Mergeable != "UNKNOWN" {
				stream(pr)
				continue
			}
			stillUnknown = append(stillUnknown, *pr)
		}
	}
	return stillUnknown, nil
}

func fetchCandidateBatch(
	ctx context.Context,
	reader candidateStatesReader,
	chunk []models.PullRequest,
	op string,
	batchIdx int,
) (map[string]github.CandidateState, error) {
	urls := make([]string, len(chunk))
	for i := range chunk {
		urls[i] = chunk[i].URL
	}
	start := time.Now()
	states, err := reader.CandidateStatesContext(ctx, urls)
	slog.Debug("list call",
		"op", op,
		"batch", batchIdx,
		"count", len(urls),
		"duration", time.Since(start).Round(time.Millisecond),
		"error", err != nil,
	)
	if err != nil {
		return nil, fmt.Errorf("fetch %s: %w", op, err)
	}
	return states, nil
}

func applyCandidateState(pr *models.PullRequest, state github.CandidateState) {
	if state.Mergeable != "" {
		pr.Mergeable = state.Mergeable
	}
	pr.StatusCheckRollup = state.StatusCheckRollup
}

type candidateCheckResult struct {
	pr  models.PullRequest
	err error
}

func checkCandidatesPerPR(
	ctx context.Context,
	githubClient any,
	refresh func(string) (string, error),
	candidates []models.PullRequest,
	checksLoaded bool,
	opts listOptions,
	stream func(*models.PullRequest),
) error {
	checksReader, hasChecks := githubClient.(statusChecksReader)
	checksCtxReader, hasCtxChecks := githubClient.(statusChecksContextReader)
	if checksLoaded {
		hasChecks = false
		hasCtxChecks = false
	}

	if refresh == nil && !hasChecks && !hasCtxChecks {
		return streamCandidatesDirectly(ctx, candidates, stream)
	}

	limit := min(max(1, opts.maxConc), len(candidates))
	resultsCh := startCandidateWorkers(ctx, candidates, limit, func(pr models.PullRequest) candidateCheckResult {
		checked, err := checkSingleCandidate(ctx, pr, refresh, checksReader, checksCtxReader, hasChecks, hasCtxChecks, opts.sleep)
		return candidateCheckResult{pr: checked, err: err}
	})
	return collectCandidateResults(ctx, resultsCh, stream)
}

func streamCandidatesDirectly(ctx context.Context, candidates []models.PullRequest, stream func(*models.PullRequest)) error {
	for i := range candidates {
		if ctx.Err() != nil {
			return fmt.Errorf("candidate checks cancelled: %w", ctx.Err())
		}
		stream(&candidates[i])
	}
	return nil
}

func startCandidateWorkers(
	ctx context.Context,
	candidates []models.PullRequest,
	limit int,
	check func(models.PullRequest) candidateCheckResult,
) <-chan candidateCheckResult {
	workCh := make(chan models.PullRequest, len(candidates))
	for i := range candidates {
		workCh <- candidates[i]
	}
	close(workCh)

	resultsCh := make(chan candidateCheckResult, len(candidates))
	var wg sync.WaitGroup
	for range limit {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for pr := range workCh {
				if ctx.Err() != nil {
					return
				}
				resultsCh <- check(pr)
			}
		}()
	}
	go func() {
		wg.Wait()
		close(resultsCh)
	}()
	return resultsCh
}

func collectCandidateResults(ctx context.Context, resultsCh <-chan candidateCheckResult, stream func(*models.PullRequest)) error {
	for {
		select {
		case res, ok := <-resultsCh:
			if !ok {
				return nil
			}
			if res.err != nil {
				return res.err
			}
			stream(&res.pr)
		case <-ctx.Done():
			drainCompletedResults(resultsCh, stream)
			return fmt.Errorf("candidate checks cancelled: %w", ctx.Err())
		}
	}
}

func drainCompletedResults(resultsCh <-chan candidateCheckResult, stream func(*models.PullRequest)) {
	for {
		select {
		case res, ok := <-resultsCh:
			if !ok || res.err != nil {
				return
			}
			stream(&res.pr)
		default:
			return
		}
	}
}

func checkSingleCandidate(
	ctx context.Context,
	pr models.PullRequest,
	refresh func(string) (string, error),
	checksReader statusChecksReader,
	checksCtxReader statusChecksContextReader,
	hasChecks, hasCtxChecks bool,
	sleep func(time.Duration),
) (models.PullRequest, error) {
	if pr.Mergeable == "UNKNOWN" && refresh != nil {
		poll := mergeablePoll{
			first:  time.Second,
			max:    time.Second,
			budget: 2 * time.Second,
			sleep:  sleep,
		}
		attempt := 0
		timedRefresh := func(prURL string) (string, error) {
			attempt++
			start := time.Now()
			state, err := refresh(prURL)
			slog.Debug("list call",
				"op", "mergeable_state",
				"pr", prURL,
				"attempt", attempt,
				"state", state,
				"duration", time.Since(start).Round(time.Millisecond),
				"error", err != nil,
			)
			return state, err
		}
		state, err := resolveMergeableContext(ctx, pr.Mergeable, pr.URL, timedRefresh, poll)
		if err != nil {
			return pr, err
		}
		pr.Mergeable = state
	}
	if !listReady(&pr) {
		return pr, nil
	}
	if ctx.Err() != nil {
		return pr, fmt.Errorf("candidate checks cancelled: %w", ctx.Err())
	}
	if hasCtxChecks {
		start := time.Now()
		checks, err := checksCtxReader.StatusChecksContext(ctx, pr.URL)
		slog.Debug("list call", "op", "status_checks", "pr", pr.URL, "duration", time.Since(start).Round(time.Millisecond), "error", err != nil)
		if err != nil {
			return pr, fmt.Errorf("failed to get status checks for %s: %w", pr.URL, err)
		}
		pr.StatusCheckRollup = checks
		return pr, nil
	}
	if hasChecks {
		start := time.Now()
		checks, err := checksReader.StatusChecks(pr.URL)
		slog.Debug("list call", "op", "status_checks", "pr", pr.URL, "duration", time.Since(start).Round(time.Millisecond), "error", err != nil)
		if err != nil {
			return pr, fmt.Errorf("failed to get status checks for %s: %w", pr.URL, err)
		}
		pr.StatusCheckRollup = checks
	}
	return pr, nil
}

func sleepWithContext(ctx context.Context, sleep func(time.Duration), d time.Duration) error {
	if d <= 0 {
		if ctx != nil && ctx.Err() != nil {
			return fmt.Errorf("sleep cancelled: %w", ctx.Err())
		}
		return nil
	}
	if sleep != nil {
		sleep(d)
		if ctx != nil && ctx.Err() != nil {
			return fmt.Errorf("sleep cancelled: %w", ctx.Err())
		}
		return nil
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-timer.C:
		return nil
	case <-ctx.Done():
		return fmt.Errorf("sleep cancelled: %w", ctx.Err())
	}
}

func emitVerdict(sw *listWriter, cfg *config.Config, pr *models.PullRequest) {
	reasons := skipReasons(pr, blockLabelsFor(cfg, pr))
	if verbose && len(reasons) > 0 {
		sw.printf("skipped: %s %s\n", pr.URL, strings.Join(reasons, ", "))
	}
	if !listReady(pr) {
		return
	}
	sw.printf("  %s\n", pr.Title)
	sw.printf("         URL: %s\n", pr.URL)
	sw.printf("         Branch: %s\n", pr.HeadRefName)
	printListCITo(sw, pr, requiredChecksFor(cfg, pr))
	if label, blocked := pr.BlocksMerge(blockLabelsFor(cfg, pr)); blocked {
		sw.printf("         Blocked: label %q prevents merge\n", label)
	}
	sw.printf("\n")
}

func printListCITo(sw *listWriter, pr *models.PullRequest, required []string) {
	ci := pr.ListCIStatus(required)
	switch ci.Status {
	case models.ChecksPass:
		sw.printf("         CI: PASS (%d/%d checks passed)\n", len(required), len(required))
	case models.ChecksFail:
		sw.printf("         CI: FAIL (%d failed)\n", ci.Failed)
	case models.ChecksPending:
		sw.printf("         CI: PENDING (%d checks still running)\n", ci.Pending)
	case models.ChecksNone:
		sw.printf("         CI: NONE (no checks configured)\n")
	}
	if len(ci.Others) > 0 {
		sw.printf("         other: %s\n", strings.Join(ci.Others, ", "))
	}
}
