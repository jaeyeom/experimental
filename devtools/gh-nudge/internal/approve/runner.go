package approve

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// Actions written to the audit log.
const (
	ActionApproved     = "approved"
	ActionWouldApprove = "would_approve"
	ActionSkipped      = "skipped"
	ActionManual       = "manual"
	ActionError        = "error"
	ActionHandoff      = "handoff"
	ActionWouldHandoff = "would_handoff"
	ActionMarkViewed   = "mark_viewed"
	// ActionWouldMarkViewed is the dry-run action for files that would be marked.
	ActionWouldMarkViewed = "would_mark_viewed"
)

// Client is the gh access gh-approve needs. *github.Client implements it.
type Client interface {
	ViewerLogin() (string, error)
	SearchApprovalCandidates() ([]github.ApprovalCandidate, error)
	SearchTeamApprovalCandidates(labelSets [][]string) ([]github.ApprovalCandidate, error)
	LoadApprovalPullRequests(refs []github.PullRef) (map[string]github.BatchPullRequestResult, error)
	LoadApprovalPullRequest(ref github.PullRef) (github.ApprovalPullRequest, error)
	PullRequestHeadSHA(ref github.PullRef) (string, error)
	PullRequestDiff(ref github.PullRef) (string, error)
	PullRequestFilePatches(ref github.PullRef) ([]github.PullFile, error)
	ApprovePullRequest(ref github.PullRef, commitSHA, body string) error
	MarkFileAsViewed(ref github.PullRef, pullRequestID, path string) error
	CodeownersBlob(ref github.PullRef, baseRef string) (github.CodeownersBlob, error)
	RepoFile(ref github.PullRef, rev, path string) (string, error)
	TeamMembers(org, slug string) ([]string, error)
	ViewerTeams(org, viewer string) ([]string, error)
	TeamMemberCount(org, slug string) (int, error)
	ReviewRequestTimeline(ref github.PullRef) ([]github.ReviewRequestEvent, error)
	RequestReviewers(ref github.PullRef, logins []string) error
	RemoveRequestedReviewer(ref github.PullRef, login string) error
}

// Options are the per-run command-line choices.
type Options struct {
	// Apply approves and hands off for real. Without it, the run is a dry run.
	Apply bool
	// Explain prints every condition of every rule with the value it saw.
	Explain bool
	// PR evaluates one pull request, owner/repo#N or a URL, instead of searching.
	PR string
	// Color colors each row's and summary heading's marker and group word.
	Color bool
	// MaxDuration stops starting new pull requests after this much time.
	MaxDuration time.Duration
	// LoadConcurrency bounds concurrent uncached PR loads. Zero uses Config.LoadConcurrency.
	LoadConcurrency int
}

// LogEntry is one audit log line.
type LogEntry struct {
	Time  time.Time `json:"time"`
	PR    string    `json:"pr"`
	URL   string    `json:"url,omitempty"`
	Title string    `json:"title,omitempty"`
	// Author is the pull request author's login. Empty on lines written
	// before the field existed, and when the pull request could not be loaded.
	Author  string `json:"author,omitempty"`
	HeadSHA string `json:"headSha"`
	// Rule is the matching rule, or null when none matched.
	Rule   *string      `json:"rule"`
	Rules  []RuleResult `json:"rules"`
	Action string       `json:"action"`
	Reason string       `json:"reason,omitempty"`
	// Reviewers are the logins a hand-off requests.
	Reviewers []string `json:"reviewers,omitempty"`
	// Timeline is the loop guard's evidence: the viewer's review request
	// events, oldest first.
	Timeline []github.ReviewRequestEvent `json:"timeline,omitempty"`
	// LoopGuard is the loop guard's verdict.
	LoopGuard string `json:"loopGuard,omitempty"`
	// Steps are the hand-off writes and their results, in order.
	Steps []string `json:"steps,omitempty"`
	// Paths are the files this run marked viewed, or would mark in a dry run.
	Paths []string `json:"paths,omitempty"`
	// FileCheckPatterns maps each marked path from file_checks to the pattern that passed.
	FileCheckPatterns map[string]string `json:"fileCheckPatterns,omitempty"`
	// PathNotes maps each re-marked path to a note such as "re-mark after change".
	PathNotes map[string]string `json:"pathNotes,omitempty"`
	// Exclusions are explain lines for files a selector matched and an
	// exclusion kept visible. They are not part of the audit log.
	Exclusions []string `json:"-"`
	// GeneratedMatches maps each marked path matched via @generated to its
	// matching pattern for --explain output.
	GeneratedMatches map[string]string `json:"-"`
	// ExtraLogs are additional audit log entries (e.g. mark_viewed alongside
	// approved) written to the log for this pull request.
	ExtraLogs []LogEntry `json:"-"`
	// Timing is the --explain timing line. It is not part of the audit log.
	Timing string `json:"-"`
}

// Runner evaluates review requests and approves the ones a rule matches.
type Runner struct {
	Client Client
	Config config.ApproveConfig
	// Viewed is the mark-viewed config. It is separate from Config because
	// the pkl block is a sibling of approve.
	Viewed config.ViewedConfig
	// GeneratedLists maps <owner>/<repo> to the repository path of a YAML
	// file listing generated-file globs.
	GeneratedLists map[string]string
	// Out receives the human-readable table.
	Out io.Writer
	// Log receives one JSON line per evaluated pull request.
	Log io.Writer
	Now func() time.Time
	// RunCommand runs agent commands. Nil means ExecCommand.
	RunCommand CommandRunner
	// RunFileCheckCommand runs file_checks scripts. Nil means ExecFileCheckCommand.
	RunFileCheckCommand FileCheckRunner
	// AgentCache caches agent command decisions. Nil bypasses the cache.
	AgentCache *AgentCache
	// PRCache caches per-commit PR facts across passes. Nil bypasses the cache.
	PRCache *PRCache
	// Sleep waits during rate-limit backoff. Nil means time.Sleep.
	Sleep func(time.Duration)
	// BackoffBase is the initial secondary rate-limit backoff. Zero means defaultBackoffBase.
	BackoffBase time.Duration
	// BatchSize bounds how many candidates are loaded per batched GraphQL query.
	// Zero uses github.ApprovalPullRequestBatchSize.
	BatchSize int

	// teams caches team members for the run, keyed by lowercase org/slug.
	teams syncCache[teamMembers]
	// teamCounts caches team member counts, keyed like teams.
	teamCounts syncCache[teamCount]
	// viewerTeams caches the viewer's teams per lowercase organization.
	viewerTeams syncCache[viewerTeamSet]
	// codeowners caches parsed CODEOWNERS files, keyed by repository and base ref.
	codeowners syncCache[codeownersEntry]
	// generatedLists caches parsed generated-file globs, keyed by repository and base SHA.
	generatedLists syncCache[generatedListEntry]
	// marksLeft is how many files this run may still mark viewed.
	marksLeft int
}

// errNotRequested marks a pull request that does not request the viewer as
// an individual. Search results like that are ignored.
var errNotRequested = errors.New("not requested as an individual reviewer")

// clientGraphQLCalls returns the GraphQL call count if client tracks it.
func clientGraphQLCalls(c Client) int {
	if counter, ok := c.(interface{ GraphQLCalls() int }); ok {
		return counter.GraphQLCalls()
	}
	return 0
}

// Run evaluates the pull requests and returns an error when any of them had
// an error. A failed pull request does not stop the others.
func (r *Runner) Run(opts Options) error {
	started := r.now()
	startCalls := clientGraphQLCalls(r.Client)
	viewer, err := r.Client.ViewerLogin()
	if err != nil {
		return fmt.Errorf("read gh user: %w", err)
	}
	candidates, single, err := r.targets(opts)
	if err != nil {
		return err
	}
	// The status column holds a two-column marker, a space, and the word.
	if _, err := fmt.Fprintf(r.Out, "%-*s %-32s %-18s %-20s %s\n", statusWidth+3, "STATUS", "PR", "ACTION", "RULE", "TITLE"); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	r.marksLeft = r.Viewed.MaxMarksPerRun
	limit := opts.MaxDuration
	if limit <= 0 {
		limit = r.Config.MaxDuration
	}
	totals, err := r.processCandidates(candidates, viewer, opts, single, started, limit)
	if err != nil {
		return err
	}
	totals.graphqlCalls = clientGraphQLCalls(r.Client) - startCalls
	if err := writeSummary(r.Out, totals.reported, opts.Color); err != nil {
		return err
	}
	if err := r.writePassStats(len(candidates), started, limit, single, opts.Explain, totals); err != nil {
		return err
	}
	if totals.failed > 0 {
		return fmt.Errorf("%d pull request(s) failed", totals.failed)
	}
	return nil
}

type batchRecoveryEvent struct {
	action    string
	chunkSize int
	attempt   int
	err       string
}

type runTotals struct {
	reported     []LogEntry
	failed       int
	loaded       int
	cheapSkipped int
	stoppedAt    int
	graphqlCalls int
	batchEvents  []batchRecoveryEvent
}

type evalOutcome struct {
	cheapSkipped bool
	skip         bool
	needsAction  bool
	entry        LogEntry
	pr           github.ApprovalPullRequest
	matched      string
	viaTeam      string
	st           *prState
}

func (r *Runner) effectiveConcurrency(opts Options, single bool, n int) int {
	if single || n <= 1 {
		return 1
	}
	c := opts.LoadConcurrency
	if c <= 0 {
		c = r.Config.LoadConcurrency
	}
	if c <= 0 {
		c = config.DefaultLoadConcurrency
	}
	return c
}

type outcomeQueue struct {
	mu        sync.Mutex
	readyCond *sync.Cond
	slots     []*evalOutcome
	stoppedAt int
}

func newOutcomeQueue(n int) *outcomeQueue {
	q := &outcomeQueue{
		slots:     make([]*evalOutcome, n),
		stoppedAt: -1,
	}
	q.readyCond = sync.NewCond(&q.mu)
	return q
}

func (q *outcomeQueue) set(idx int, res *evalOutcome) {
	q.mu.Lock()
	q.slots[idx] = res
	q.readyCond.Broadcast()
	q.mu.Unlock()
}

func (q *outcomeQueue) stopAt(idx int) {
	q.mu.Lock()
	q.stoppedAt = idx
	q.readyCond.Broadcast()
	q.mu.Unlock()
}

func (q *outcomeQueue) next(idx int) (*evalOutcome, int) {
	q.mu.Lock()
	defer q.mu.Unlock()
	for q.slots[idx] == nil && (q.stoppedAt < 0 || idx < q.stoppedAt) {
		q.readyCond.Wait()
	}
	if q.stoppedAt >= 0 && idx >= q.stoppedAt {
		return nil, q.stoppedAt
	}
	return q.slots[idx], -1
}

func (r *Runner) cheapSkipOutcome(candidate github.ApprovalCandidate, explain bool) *evalOutcome {
	reason, rules := cheapFailure(r.Config.Rules, candidate)
	if reason == "" {
		return nil
	}
	st := &prState{start: r.now()}
	entry := LogEntry{
		Time:   r.now().UTC(),
		PR:     fmt.Sprintf("%s/%s#%d", candidate.Ref.Owner, candidate.Ref.Repo, candidate.Ref.Number),
		URL:    candidate.URL,
		Title:  candidate.Title,
		Author: candidate.Author,
		Action: ActionSkipped,
		Reason: reason,
		Rules:  rules,
	}
	if explain {
		entry.Timing = r.timingLine(st)
	}
	return &evalOutcome{cheapSkipped: true, entry: entry, st: st}
}

type batchLoadInfo struct {
	duration time.Duration
	index    int
	count    int
}

func (r *Runner) effectiveBatchSize() int {
	if r.BatchSize > 0 {
		return r.BatchSize
	}
	return github.ApprovalPullRequestBatchSize
}

type indexedCandidate struct {
	index int
	cand  github.ApprovalCandidate
}

func (r *Runner) dispatchCandidates(candidates []github.ApprovalCandidate, viewer string, opts Options, single bool, started time.Time, limit time.Duration, pool *loadPool, queue *outcomeQueue) []batchRecoveryEvent {
	var wg sync.WaitGroup
	defer wg.Wait()
	canStart := func() bool {
		return single || limit <= 0 || r.now().Sub(started) < limit
	}
	if single {
		r.dispatchSingle(candidates, viewer, single, canStart, pool, queue, &wg)
		return nil
	}

	var toLoad []indexedCandidate
	for i, candidate := range candidates {
		if !canStart() {
			queue.stopAt(i)
			return nil
		}
		if skipped := r.cheapSkipOutcome(candidate, opts.Explain); skipped != nil {
			queue.set(i, skipped)
			continue
		}
		toLoad = append(toLoad, indexedCandidate{index: i, cand: candidate})
	}
	if len(toLoad) == 0 {
		return nil
	}

	var events []batchRecoveryEvent
	batchSize := r.effectiveBatchSize()
	totalBatches := (len(toLoad) + batchSize - 1) / batchSize
	for b := 0; b < totalBatches; b++ {
		start := b * batchSize
		end := min(start+batchSize, len(toLoad))
		if !r.dispatchBatchChunk(toLoad[start:end], b+1, totalBatches, viewer, single, canStart, pool, queue, &wg, &events) {
			return events
		}
	}
	return events
}

func (r *Runner) dispatchSingle(candidates []github.ApprovalCandidate, viewer string, single bool, canStart func() bool, pool *loadPool, queue *outcomeQueue, wg *sync.WaitGroup) {
	for i, candidate := range candidates {
		if !canStart() || !pool.acquire(canStart) {
			queue.stopAt(i)
			return
		}
		wg.Add(1)
		go func(idx int, cand github.ApprovalCandidate) {
			defer wg.Done()
			defer pool.release()
			queue.set(idx, r.loadAndEvaluate(cand.Ref, viewer, single, pool))
		}(i, candidate)
	}
}

func (r *Runner) loadBatchOnce(refs []github.PullRef, pool *loadPool) (map[string]github.BatchPullRequestResult, error) {
	var batchResults map[string]github.BatchPullRequestResult
	batchErr := pool.callWithRetry(func() error {
		var callErr error
		batchResults, callErr = r.Client.LoadApprovalPullRequests(refs)
		return callErr //nolint:wrapcheck // caller records batch pull request load error
	})
	return batchResults, batchErr
}

func (r *Runner) dispatchBatchChunk(chunk []indexedCandidate, batchNum, totalBatches int, viewer string, single bool, canStart func() bool, pool *loadPool, queue *outcomeQueue, wg *sync.WaitGroup, events *[]batchRecoveryEvent) bool {
	if !canStart() {
		queue.stopAt(chunk[0].index)
		return false
	}
	refs := make([]github.PullRef, len(chunk))
	for j, item := range chunk {
		refs[j] = item.cand.Ref
	}
	loadStart := r.now()
	batchResults, batchErr := r.loadBatchOnce(refs, pool)
	if batchErr != nil && isTransientBatchError(batchErr) {
		*events = append(*events, batchRecoveryEvent{
			action:    "retry",
			chunkSize: len(chunk),
			attempt:   1,
			err:       batchErr.Error(),
		})
		pool.pauseTransient()
		if !canStart() {
			queue.stopAt(chunk[0].index)
			return false
		}
		batchResults, batchErr = r.loadBatchOnce(refs, pool)
		if batchErr != nil && isTransientBatchError(batchErr) && len(chunk) > 1 {
			*events = append(*events, batchRecoveryEvent{
				action:    "split",
				chunkSize: len(chunk),
				attempt:   2,
				err:       batchErr.Error(),
			})
			mid := len(chunk) / 2
			if !r.dispatchBatchChunk(chunk[:mid], batchNum, totalBatches, viewer, single, canStart, pool, queue, wg, events) {
				return false
			}
			return r.dispatchBatchChunk(chunk[mid:], batchNum, totalBatches, viewer, single, canStart, pool, queue, wg, events)
		}
	}
	bInfo := batchLoadInfo{
		duration: r.now().Sub(loadStart),
		index:    batchNum,
		count:    totalBatches,
	}
	if batchErr != nil {
		for _, item := range chunk {
			queue.set(item.index, r.evaluateLoaded(item.cand.Ref, github.ApprovalPullRequest{}, batchErr, bInfo, viewer, single, pool))
		}
		return true
	}
	for _, item := range chunk {
		if !pool.acquire(canStart) {
			queue.stopAt(item.index)
			return false
		}
		key := github.ApprovalRefKey(item.cand.Ref)
		res, ok := batchResults[key]
		if !ok {
			res.Err = fmt.Errorf("load pull request %s: missing from batch response", key)
		}
		wg.Add(1)
		go func(idx int, ref github.PullRef, bRes github.BatchPullRequestResult, info batchLoadInfo) {
			defer wg.Done()
			defer pool.release()
			queue.set(idx, r.evaluateLoaded(ref, bRes.PR, bRes.Err, info, viewer, single, pool))
		}(item.index, item.cand.Ref, res, bInfo)
	}
	return true
}

func (r *Runner) processCandidates(candidates []github.ApprovalCandidate, viewer string, opts Options, single bool, started time.Time, limit time.Duration) (runTotals, error) {
	totals := runTotals{stoppedAt: -1}
	if len(candidates) == 0 {
		return totals, nil
	}
	pool := newLoadPool(r.effectiveConcurrency(opts, single, len(candidates)), r.BackoffBase, r.now, r.Sleep)
	queue := newOutcomeQueue(len(candidates))
	var batchEvents []batchRecoveryEvent
	dispatchDone := make(chan struct{})
	go func() {
		defer close(dispatchDone)
		batchEvents = r.dispatchCandidates(candidates, viewer, opts, single, started, limit, pool, queue)
	}()

	var reportErr error
	for i := range candidates {
		res, stoppedAt := queue.next(i)
		if stoppedAt >= 0 {
			totals.stoppedAt = stoppedAt
			break
		}
		if res.cheapSkipped {
			if err := r.report(res.entry, opts); err != nil {
				reportErr = err
				break
			}
			totals.reported = append(totals.reported, res.entry)
			totals.cheapSkipped++
			continue
		}
		totals.loaded++
		if res.skip {
			continue
		}
		if res.needsAction && r.applyOutcome(res, viewer, opts, single) {
			continue
		}
		res.entry.Timing = r.timingLine(res.st)
		if res.entry.Action == ActionError {
			totals.failed++
		}
		if err := r.report(res.entry, opts); err != nil {
			reportErr = err
			break
		}
		totals.reported = append(totals.reported, res.entry)
	}
	<-dispatchDone
	totals.batchEvents = batchEvents
	if reportErr != nil {
		return totals, reportErr
	}
	r.prunePRCache(candidates, single, totals.stoppedAt)
	return totals, nil
}

func (r *Runner) prunePRCache(candidates []github.ApprovalCandidate, single bool, stoppedAt int) {
	if single || r.PRCache == nil || stoppedAt >= 0 {
		return
	}
	active := make(map[string]bool, len(candidates))
	for _, c := range candidates {
		active[prCacheKey(c.Ref)] = true
	}
	_ = r.PRCache.prune(active)
}

func (r *Runner) writePassStats(candidates int, started time.Time, limit time.Duration, single, explain bool, totals runTotals) error {
	if single {
		return nil
	}
	if explain {
		if _, err := fmt.Fprintf(r.Out, "Candidates: %d; skipped by cheap conditions: %d; fully loaded: %d; wall time: %s; GraphQL calls: %d\n", candidates, totals.cheapSkipped, totals.loaded, r.now().Sub(started), totals.graphqlCalls); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	} else {
		if _, err := fmt.Fprintf(r.Out, "Candidates: %d; skipped by cheap conditions: %d; fully loaded: %d; wall time: %s\n", candidates, totals.cheapSkipped, totals.loaded, r.now().Sub(started)); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	for _, ev := range totals.batchEvents {
		if _, err := fmt.Fprintf(r.Out, "Batch %s: chunk size %d, attempt %d, error: %s\n", ev.action, ev.chunkSize, ev.attempt, ev.err); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	if totals.stoppedAt < 0 {
		return nil
	}
	if _, err := fmt.Fprintf(r.Out, "Run limit %s reached; %d candidate(s) not evaluated.\n", limit, candidates-totals.stoppedAt); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	return nil
}

func (r *Runner) targets(opts Options) ([]github.ApprovalCandidate, bool, error) {
	if opts.PR != "" {
		ref, err := github.ParsePullRef(opts.PR)
		if err != nil {
			return nil, true, fmt.Errorf("--pr: %w", err)
		}
		return []github.ApprovalCandidate{{Ref: ref}}, true, nil
	}
	teamScopedRules := teamRules(r.Config.Rules)
	var candidates []github.ApprovalCandidate
	if len(teamScopedRules) == 0 || len(teamScopedRules) < len(r.Config.Rules) {
		individual, err := r.Client.SearchApprovalCandidates()
		if err != nil {
			return nil, false, fmt.Errorf("search review requests: %w", err)
		}
		candidates = append(candidates, individual...)
	}
	if len(teamScopedRules) > 0 {
		sets := make([][]string, 0)
		seenSets := make(map[string]bool)
		allHaveLabels := true
		for _, rule := range teamScopedRules {
			if len(rule.LabelsRequired) == 0 {
				allHaveLabels = false
				break
			}
		}
		if allHaveLabels {
			for _, rule := range teamScopedRules {
				key := strings.Join(rule.LabelsRequired, "\x00")
				if !seenSets[key] {
					seenSets[key] = true
					sets = append(sets, append([]string(nil), rule.LabelsRequired...))
				}
			}
		} else {
			sets = append(sets, []string{})
		}
		team, err := r.Client.SearchTeamApprovalCandidates(sets)
		if err != nil {
			return nil, false, fmt.Errorf("search team review requests: %w", err)
		}
		candidates = append(candidates, team...)
	}
	unique := make([]github.ApprovalCandidate, 0, len(candidates))
	seen := make(map[string]bool)
	for _, candidate := range candidates {
		key := fmt.Sprintf("%s/%s#%d", candidate.Ref.Owner, candidate.Ref.Repo, candidate.Ref.Number)
		if !seen[key] {
			seen[key] = true
			unique = append(unique, candidate)
		}
	}
	return unique, false, nil
}

// cheapFailure rejects a search candidate only when every rule fails a
// condition that can be evaluated from the search response.
func cheapFailure(rules []config.ApproveRule, candidate github.ApprovalCandidate) (string, []RuleResult) {
	pr := github.ApprovalPullRequest{
		Ref: candidate.Ref, URL: candidate.URL, Title: candidate.Title,
		Author: candidate.Author, Draft: candidate.Draft, Labels: candidate.Labels,
		Additions: candidate.Additions, Deletions: candidate.Deletions,
		ChangedFiles: candidate.ChangedFiles,
	}
	cheapNames := map[string]bool{
		"max_changed_lines": true, "max_added_lines": true, "max_files": true,
		"authors_allow": true, "authors_deny": true, "labels_required": true,
		"labels_forbidden": true, "not_draft": true,
	}
	results := make([]RuleResult, 0, len(rules))
	allFail := len(rules) > 0
	var tooLargeReason string
	for _, rule := range rules {
		result := RuleResult{Rule: rule.Name, Matched: true}
		failed := false
		for _, cond := range conditions {
			if !cond.active(rule) {
				continue
			}
			got := ConditionResult{Condition: cond.name, NotEvaluated: true, Saw: "not evaluated before per-pull-request load"}
			if cheapNames[cond.name] {
				got.Pass, got.Saw = cond.check(rule, pr)
				got.NotEvaluated = false
				if !got.Pass {
					failed, result.Matched = true, false
					if tooLargeReason == "" {
						tooLargeReason = mineCommentsSizeReason(rule, pr)
					}
				}
			}
			result.Conditions = append(result.Conditions, got)
		}
		if !failed {
			allFail = false
		}
		results = append(results, result)
	}
	if !allFail {
		return "", nil
	}
	if tooLargeReason != "" {
		return tooLargeReason, results
	}
	return "all rules failed cheap conditions", results
}

func mineCommentsSizeReason(rule config.ApproveRule, pr github.ApprovalPullRequest) string {
	if !rule.MineCommentsOnly {
		return ""
	}
	tooManyFiles := rule.MaxFiles != nil && pr.ChangedFiles > *rule.MaxFiles
	tooManyLines := rule.MaxChangedLines != nil && pr.Additions+pr.Deletions > *rule.MaxChangedLines
	if tooManyFiles || tooManyLines {
		return fmt.Sprintf("too large to check (%d files, %d changed lines)", pr.ChangedFiles, pr.Additions+pr.Deletions)
	}
	return ""
}

func mineCommentsLimitFailure(matched string, rules []config.ApproveRule, pr github.ApprovalPullRequest) string {
	if matched != "" {
		return ""
	}
	for _, rule := range rules {
		if reason := mineCommentsSizeReason(rule, pr); reason != "" {
			return reason
		}
	}
	return ""
}

// loadAndEvaluate loads one pull request's metadata and per-commit facts
// (from PRCache when valid, otherwise from GitHub) and evaluates the rules.
// Write actions (approve, handoff, mark_viewed) are deferred to applyOutcome.
func (r *Runner) loadAndEvaluate(ref github.PullRef, viewer string, single bool, pool *loadPool) *evalOutcome {
	loadStart := r.now()
	var pr github.ApprovalPullRequest
	err := pool.callWithRetry(func() error {
		var callErr error
		pr, callErr = r.Client.LoadApprovalPullRequest(ref)
		return callErr //nolint:wrapcheck // caller records pull request load error
	})
	return r.evaluateLoaded(ref, pr, err, batchLoadInfo{duration: r.now().Sub(loadStart)}, viewer, single, pool)
}

func (r *Runner) evaluateLoaded(ref github.PullRef, pr github.ApprovalPullRequest, loadErr error, bInfo batchLoadInfo, viewer string, single bool, pool *loadPool) *evalOutcome {
	st := &prState{
		start:       r.now(),
		pullRequest: bInfo.duration,
		batchIndex:  bInfo.index,
		batchCount:  bInfo.count,
	}
	entry := LogEntry{
		Time: r.now().UTC(),
		PR:   fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number),
	}
	if loadErr != nil {
		entry.Action, entry.Reason = ActionError, loadErr.Error()
		return &evalOutcome{entry: entry, st: st}
	}
	entry.URL, entry.Title, entry.HeadSHA, entry.Author = pr.URL, pr.Title, pr.HeadSHA, pr.Author

	cached, hasCached := r.restorePRCache(ref, &pr, st)

	rules, viaTeam, reason := r.scope(pr, viewer, st, pool)
	if reason != "" {
		entry.Action, entry.Reason = ActionSkipped, reason
		return &evalOutcome{entry: entry, st: st, skip: !single}
	}
	skipReason := r.skipReason(pr, viewer)
	if skipReason == "" && len(pr.Files) == 0 && pr.FilesErr == "" && pr.ChangedFiles > 0 {
		fileStart := r.now()
		var fullPR github.ApprovalPullRequest
		err := pool.callWithRetry(func() error {
			var callErr error
			fullPR, callErr = r.Client.LoadApprovalPullRequest(ref)
			return callErr //nolint:wrapcheck // caller records pull request load error
		})
		st.pullRequest += r.now().Sub(fileStart)
		if err != nil {
			entry.Action, entry.Reason = ActionError, err.Error()
			return &evalOutcome{entry: entry, st: st}
		}
		pr.Files = fullPR.Files
		pr.FilesErr = fullPR.FilesErr
		st.cacheDirty = true
	}
	var agent AgentFunc
	var loads *lazyLoads
	if skipReason == "" {
		agent = r.timedAgent(r.agent(pr), st)
		loads = r.loads(pr, viewer, st, pool)
	}
	results, matched := evaluateRules(rules, pr, agent, r.fileChecker(), loads)
	entry.Reason = mineCommentsLimitFailure(matched, rules, pr)
	if st.ownershipDone {
		pr.Ownership = st.ownership
	}
	entry.Rules = results
	if matched != "" {
		entry.Rule = &matched
	}

	r.savePRCache(ref, pr, st, cached, hasCached)

	if skipReason != "" {
		entry.Action, entry.Reason = ActionSkipped, skipReason
		st.end = r.now()
		return &evalOutcome{entry: entry, st: st}
	}
	st.end = r.now()
	return &evalOutcome{
		needsAction: true,
		entry:       entry,
		pr:          pr,
		matched:     matched,
		viaTeam:     viaTeam,
		st:          st,
	}
}

func (r *Runner) restorePRCache(ref github.PullRef, pr *github.ApprovalPullRequest, st *prState) (prCacheEntry, bool) {
	if r.PRCache == nil {
		return prCacheEntry{}, false
	}
	cached, missReason := r.PRCache.get(ref, pr.HeadSHA)
	if missReason != "" {
		st.cacheStatus = "miss (" + missReason + ")"
		st.cacheDirty = true
		return prCacheEntry{}, false
	}
	st.cacheStatus = "hit"
	r.PRCache.touch(ref)
	if len(pr.Files) == 0 && pr.FilesErr == "" && cached.HasFiles {
		pr.Files = filesWithLiveViewedState(cached.Files, pr.ViewedStates)
		pr.FilesErr = cached.FilesErr
	}
	if cached.HasPatches {
		st.patchFiles = cached.Patches
		st.patchErr = cached.PatchesErr
		st.patchesDone = true
		st.patches.used = true
		st.patches.calls = 0
	}
	if cached.HasOwnership && cached.Ownership != nil {
		st.hasCachedOwnership = true
		st.cachedOwnership = cached.Ownership
		st.cachedCodeownersSHA = cached.CodeownersBlobSHA
		st.cachedTeamsHash = cached.ViewerTeamsHash
	}
	return cached, true
}

func (r *Runner) savePRCache(ref github.PullRef, pr github.ApprovalPullRequest, st *prState, cached prCacheEntry, hasCached bool) {
	if r.PRCache == nil || !st.cacheDirty || pr.HeadSHA == "" {
		return
	}
	toSave := cached
	toSave.Owner = ref.Owner
	toSave.Repo = ref.Repo
	toSave.Number = ref.Number
	toSave.HeadSHA = pr.HeadSHA
	if pr.FilesErr == "" && len(pr.Files) > 0 {
		toSave.Files = filesWithoutViewedState(pr.Files)
		toSave.FilesErr = pr.FilesErr
		toSave.HasFiles = true
	}
	if st.patchesDone && st.patchErr == "" {
		toSave.Patches = st.patchFiles
		toSave.PatchesErr = ""
		toSave.HasPatches = true
	}
	if st.ownershipDone && st.ownership != nil && st.ownership.Err == "" {
		toSave.Ownership = st.ownership
		toSave.CodeownersBlobSHA = st.loadedCodeownersSHA
		toSave.ViewerTeamsHash = st.loadedTeamsHash
		toSave.HasOwnership = true
	}
	if !hasCached || toSave.HasFiles || toSave.HasPatches || toSave.HasOwnership {
		_ = r.PRCache.put(toSave)
	}
}

func filesWithLiveViewedState(cachedFiles []models.File, viewedStates map[string]string) []models.File {
	files := make([]models.File, len(cachedFiles))
	for i, f := range cachedFiles {
		f.ViewerViewedState = viewedStates[f.Path]
		files[i] = f
	}
	return files
}

func filesWithoutViewedState(src []models.File) []models.File {
	files := make([]models.File, len(src))
	for i, f := range src {
		f.ViewerViewedState = ""
		files[i] = f
	}
	return files
}

// applyOutcome performs sequential write actions (handoff, approve, mark_viewed)
// after a PR's facts have been loaded and evaluated.
func (r *Runner) applyOutcome(res *evalOutcome, viewer string, opts Options, single bool) bool {
	switch {
	case res.matched == "":
		res.entry.Action = ActionManual
	case r.rule(res.matched).Action == config.ActionHandoff:
		r.handoff(&res.entry, res.pr, r.rule(res.matched), viewer, opts.Apply)
	case !opts.Apply:
		res.entry.Action = ActionWouldApprove
	default:
		res.entry.Action, res.entry.Reason = r.approve(res.pr, r.rule(res.matched))
	}
	if res.viaTeam != "" && res.entry.Action == ActionManual {
		res.entry.Action = ActionSkipped
		res.entry.Reason = "requested only through " + res.viaTeam + "; no requested_via_team rule matched"
		return !single && !opts.Explain
	}
	r.markViewed(&res.entry, res.pr, viewer, opts.Apply, res.st, nil)
	return false
}

// scope returns the rules that may take pr and, for a team-only request,
// the team. reason is set when no rule may take it.
func (r *Runner) scope(pr github.ApprovalPullRequest, viewer string, st *prState, pool *loadPool) ([]config.ApproveRule, string, string) {
	if containsFold(pr.RequestedUsers, viewer) {
		return r.Config.Rules, "", ""
	}
	team, reason := r.requestingTeam(pr, viewer, st, pool)
	if team == "" {
		return nil, "", reason
	}
	return teamRules(r.Config.Rules), team, ""
}

// requestingTeam returns a requested team, as @org/slug, that includes the
// viewer, when a rule sets requested_via_team. Otherwise it returns "" and
// why the pull request is skipped.
func (r *Runner) requestingTeam(pr github.ApprovalPullRequest, viewer string, st *prState, pool *loadPool) (string, string) {
	if len(teamRules(r.Config.Rules)) == 0 {
		return "", errNotRequested.Error()
	}
	var errs []string
	for _, slug := range pr.RequestedTeams {
		in, err := r.viewerInTeam(pr.Ref.Owner, slug, viewer, st, pool)
		if err != nil {
			errs = append(errs, err.Error())
			continue
		}
		if in {
			return "@" + pr.Ref.Owner + "/" + slug, ""
		}
	}
	if len(errs) > 0 {
		return "", errNotRequested.Error() + "; " + strings.Join(errs, "; ")
	}
	return "", errNotRequested.Error()
}

// teamRules returns the rules that set requested_via_team.
func teamRules(rules []config.ApproveRule) []config.ApproveRule {
	var out []config.ApproveRule
	for _, rule := range rules {
		if rule.RequestedViaTeam {
			out = append(out, rule)
		}
	}
	return out
}

// rule returns the rule named name.
func (r *Runner) rule(name string) config.ApproveRule {
	for _, rule := range r.Config.Rules {
		if rule.Name == name {
			return rule
		}
	}
	return config.ApproveRule{}
}

// skipReason returns why pr is never approved whatever the rules say, or "".
func (r *Runner) skipReason(pr github.ApprovalPullRequest, viewer string) string {
	switch {
	case pr.State != "OPEN":
		return "pull request is " + strings.ToLower(pr.State)
	case strings.EqualFold(pr.Author, viewer):
		return "the author is you; gh-approve never approves your own pull request"
	case r.Config.SkipAlreadyApproved && latestStateOnHead(pr, viewer) == "APPROVED":
		return "already approved by you at head " + pr.HeadSHA
	}
	if login := openChangeRequestBy(pr); login != "" {
		return "open change request by " + login
	}
	return ""
}

// agent returns the AgentFunc for pr. The diff is read once, when the first
// agent command is about to run. A diff that cannot be read defers.
func (r *Runner) agent(pr github.ApprovalPullRequest) AgentFunc {
	run := r.RunCommand
	if run == nil {
		run = ExecCommand
	}
	var (
		diff    string
		diffErr error
		read    bool
	)
	return func(rule config.ApproveRule) AgentResult {
		if !read {
			diff, diffErr = r.Client.PullRequestDiff(pr.Ref)
			read = true
		}
		if diffErr != nil {
			return AgentResult{Decision: AgentDefer, Reason: diffErr.Error()}
		}
		return runAgent(run, r.AgentCache, rule, newAgentInput(pr, diff))
	}
}

func (r *Runner) approve(pr github.ApprovalPullRequest, rule config.ApproveRule) (string, string) {
	ref, evaluatedSHA := pr.Ref, pr.HeadSHA
	head, err := r.Client.PullRequestHeadSHA(ref)
	if err != nil {
		return ActionError, err.Error()
	}
	if head != evaluatedSHA {
		return ActionSkipped, fmt.Sprintf("head moved from %s to %s since evaluation", evaluatedSHA, head)
	}
	if err := r.Client.ApprovePullRequest(ref, evaluatedSHA, approvalBody(r.Config.Body, rule, pr)); err != nil {
		return ActionError, err.Error()
	}
	return ActionApproved, ""
}

// approvalBody is the rule's body, or the global body when the rule sets
// none. A body without placeholders gets the rule name appended. A body
// with placeholders is used as written, with them filled in.
func approvalBody(global string, rule config.ApproveRule, pr github.ApprovalPullRequest) string {
	if len(config.BodyPlaceholders(rule.Body)) == 0 {
		body := rule.Body
		if body == "" {
			body = global
		}
		return fmt.Sprintf("%s\n\n<!-- gh-approve: %s -->", body, rule.Name)
	}
	files, owners := myFiles(pr)
	return strings.NewReplacer(
		"{"+config.PlaceholderMyFiles+"}", strings.Join(files, ", "),
		"{"+config.PlaceholderMyTeams+"}", strings.Join(owners, ", "),
		"{"+config.PlaceholderRule+"}", rule.Name,
	).Replace(rule.Body)
}

// agentKeyText is the short cache key, or "none" when the cache was bypassed.
func agentKeyText(a AgentResult) string {
	if a.CacheKey == "" {
		return "none"
	}
	return a.CacheKey
}

func (r *Runner) report(entry LogEntry, opts Options) error {
	if err := writeLogEntry(r.Log, entry); err != nil {
		return err
	}
	for _, extra := range entry.ExtraLogs {
		if err := writeLogEntry(r.Log, extra); err != nil {
			return err
		}
	}
	row := fmt.Sprintf("%s %-32s %-18s %-20s %s\n", statusCell(entry.Action, opts.Color), entry.PR, entry.Action, ruleColumn(entry), entry.Title)
	if _, err := io.WriteString(r.Out, row+details(entry, opts.Explain)); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	return nil
}

func writeLogEntry(w io.Writer, entry LogEntry) error {
	line, err := json.Marshal(entry)
	if err != nil {
		return fmt.Errorf("encode audit log: %w", err)
	}
	if _, err := w.Write(append(line, '\n')); err != nil {
		return fmt.Errorf("write audit log: %w", err)
	}
	return nil
}

// details are the indented lines under a row.
func details(entry LogEntry, explain bool) string {
	var out strings.Builder
	if entry.Reason != "" {
		fmt.Fprintf(&out, "    %s\n", entry.Reason)
	}
	if explain && entry.Timing != "" {
		fmt.Fprintf(&out, "    %s\n", entry.Timing)
	}
	writeHandoff(&out, entry, explain)
	writeViewed(&out, entry, explain)
	writeExclusions(&out, entry, explain)
	for _, result := range entry.Rules {
		switch {
		case explain:
			writeExplain(&out, result)
		case leftForHuman(entry):
			writeFirstFailure(&out, result)
		case result.Matched && result.Agent != nil:
			fmt.Fprintf(&out, "    %s\n", result.Agent.Summary())
		}
	}
	return out.String()
}

// ruleColumn is the RULE cell. A pull request no approve rule took says manual.
func ruleColumn(entry LogEntry) string {
	if entry.Rule != nil {
		return *entry.Rule
	}
	if leftForHuman(entry) {
		return "manual"
	}
	return "-"
}

// leftForHuman reports a pull request no approve or hand-off rule took.
func leftForHuman(entry LogEntry) bool {
	if entry.Rule != nil {
		return false
	}
	switch entry.Action {
	case ActionManual, ActionMarkViewed, ActionWouldMarkViewed:
		return true
	default:
		return false
	}
}

// writeViewed writes how many files were marked, and with explain their paths.
func writeViewed(out *strings.Builder, entry LogEntry, explain bool) {
	var verb string
	switch entry.Action {
	case ActionWouldMarkViewed, ActionWouldApprove, ActionWouldHandoff:
		if len(entry.Paths) == 0 {
			return
		}
		verb = ActionWouldMarkViewed
	case ActionMarkViewed, ActionApproved, ActionHandoff:
		if len(entry.Paths) == 0 {
			return
		}
		verb = ActionMarkViewed
	default:
		return
	}
	fmt.Fprintf(out, "    %s: %d files\n", verb, len(entry.Paths))
	if !explain {
		return
	}
	for _, path := range entry.Paths {
		line := path
		if pat := entry.GeneratedMatches[path]; pat != "" {
			line = formatGeneratedMatch(path, pat)
		}
		if note := entry.PathNotes[path]; note != "" {
			line = fmt.Sprintf("%s (%s)", line, note)
		}
		fmt.Fprintf(out, "      %s\n", line)
	}
}

// writeExclusions writes, with --explain, each file an exclusion kept visible.
func writeExclusions(out *strings.Builder, entry LogEntry, explain bool) {
	if !explain {
		return
	}
	for _, line := range entry.Exclusions {
		fmt.Fprintf(out, "      %s\n", line)
	}
}

// writeHandoff writes a hand-off's reviewers, steps, and, with explain, the
// loop guard verdict.
func writeHandoff(out *strings.Builder, entry LogEntry, explain bool) {
	if len(entry.Reviewers) > 0 {
		fmt.Fprintf(out, "    reviewers: %s\n", strings.Join(entry.Reviewers, ", "))
	}
	for _, step := range entry.Steps {
		fmt.Fprintf(out, "    step: %s\n", step)
	}
	if explain && entry.LoopGuard != "" {
		fmt.Fprintf(out, "    loop guard: %s\n", entry.LoopGuard)
	}
}

// writeExplain writes every condition of result with the value it saw.
func writeExplain(out *strings.Builder, result RuleResult) {
	verdict := "no match"
	if result.Matched {
		verdict = "match"
	}
	fmt.Fprintf(out, "    rule %s: %s\n", result.Rule, verdict)
	for _, cond := range result.Conditions {
		mark := "pass"
		switch {
		case cond.NotEvaluated:
			mark = "skip"
		case !cond.Pass:
			mark = "FAIL"
		}
		fmt.Fprintf(out, "      %s %s: %s\n", mark, cond.Condition, cond.Saw)
		for _, detail := range cond.Details {
			fmt.Fprintf(out, "        %s\n", detail)
		}
		if cond.Condition == agentCondition && result.Agent != nil && result.Agent.Cache != "" {
			fmt.Fprintf(out, "        agent_cache: %s, key %s\n", result.Agent.Cache, agentKeyText(*result.Agent))
			if result.Agent.CacheError != "" {
				fmt.Fprintf(out, "        agent_cache_error: %s\n", result.Agent.CacheError)
			}
		}
	}
}

// writeFirstFailure writes why result did not match.
func writeFirstFailure(out *strings.Builder, result RuleResult) {
	first, ok := result.FirstFailure()
	switch {
	case !ok:
	case first.Condition == agentCondition && result.Agent != nil:
		fmt.Fprintf(out, "    %s: %s\n", result.Rule, result.Agent.Summary())
	default:
		fmt.Fprintf(out, "    %s: %s failed (%s)\n", result.Rule, first.Condition, first.Saw)
	}
}
