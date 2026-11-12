package scan

import (
	"context"
	"errors"
	"fmt"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

type cachedMembers struct {
	members []gh.TeamMember
	err     string
}

type coverCache struct {
	historySince time.Time
	activeSince  time.Time
	members      map[string]cachedMembers
	files        map[string][]string
	rules        map[string][]codeRule
	history      map[string]gh.PathHistory
	stored       HistoryCache
	// historyTTL > 0 keys stored history by the window's start day instead
	// of the base head; the stored cache applies the TTL.
	historyTTL time.Duration
	// batch is the path count the next history request starts at, and
	// maxBatch the most it grows to.
	batch       int
	maxBatch    int
	load        map[string]int
	loadTTL     time.Duration
	active      map[string]bool
	loadSkipped bool
	activeSkip  bool
	rulesFailed map[string]bool
	// budget counts the round's GitHub calls against maxCalls.
	budget   *gh.CallBudget
	maxCalls int
	// stop is why the round stopped early; empty while it runs.
	stop string
	// deferred are the history keys of paths left for the next round.
	deferred map[string]bool
	// listed, needed and hits count paths for the round summary.
	listed, needed, hits int
}

const (
	stopCallCap     = "call cap"
	stopRateLimited = "rate limited"
)

// errStopped unwinds team-cover work once the round stopped; see
// coverCache.stop.
var errStopped = errors.New("team-cover stopped")

func newCoverCache(cfg config.Config, now time.Time) *coverCache {
	return &coverCache{
		historySince: now.AddDate(0, 0, -cfg.TeamCoverHistoryDays),
		activeSince:  now.AddDate(0, 0, -cfg.TeamCoverActiveDays),
		members:      map[string]cachedMembers{},
		files:        map[string][]string{},
		rules:        map[string][]codeRule{},
		history:      map[string]gh.PathHistory{},
		historyTTL:   cfg.TeamCoverHistoryTTL,
		batch:        cfg.TeamCoverBatchPaths,
		maxBatch:     cfg.TeamCoverBatchPaths,
		load:         map[string]int{},
		loadTTL:      cfg.TeamCoverLoadTTL,
		active:       map[string]bool{},
		rulesFailed:  map[string]bool{},
		budget:       gh.NewCallBudget(cfg.TeamCoverMaxCalls),
		maxCalls:     cfg.TeamCoverMaxCalls,
		deferred:     map[string]bool{},
	}
}

// halted reports whether err stops the round: a rate limit, after which
// no team-cover call is made, or the call cap.
func (c *coverCache) halted(err error) bool {
	switch {
	case gh.IsRateLimited(err):
		c.stop = stopRateLimited
	case errors.Is(err, gh.ErrCallCap):
		if c.stop == "" {
			c.stop = stopCallCap
		}
	default:
		return false
	}
	return true
}

// report adds the round summary, and why the round stopped, to the round
// output.
func (c *coverCache) report(ctx context.Context, doc *Document) {
	limit := "no cap"
	if c.maxCalls > 0 {
		limit = fmt.Sprintf("cap %d", c.maxCalls)
	}
	summary := fmt.Sprintf("team-cover: %d of %d paths need history, %d cache hits, %d calls (%s)",
		c.needed, c.listed, c.hits, c.budget.Used(), limit)
	runlog.FromContext(ctx).Info(summary)
	doc.TeamCoverNotes = append(doc.TeamCoverNotes, summary)
	var msg string
	switch c.stop {
	case stopCallCap:
		msg = fmt.Sprintf("team-cover: call cap %d reached; %d paths deferred", c.maxCalls, len(c.deferred))
	case stopRateLimited:
		msg = "team-cover: stopped (rate limited)"
	default:
		return
	}
	runlog.FromContext(ctx).Warn(msg)
	doc.TeamCoverNotes = append(doc.TeamCoverNotes, msg)
	warn(doc, msg)
}

// prCover is one pull request's team-cover work between passes.
type prCover struct {
	pr PR
	// final is set when the first pass already decided the suggestions.
	final   bool
	suggest []TeamReviewSuggestion
	facts   coverFacts
	ranking bool
	history bool
}

// coverTeams runs in three passes so path history can be batched: each PR
// that needs ranking collects its files, then history is fetched once per
// repository and base for every collected path, then each PR is ranked.
// Its GitHub calls are capped by team_cover_max_calls; on the cap or a rate
// limit the round stops, and a PR not ranked yet gets no suggestions and
// waits for the next round.
func coverTeams(ctx context.Context, deps Deps, cfg config.Config, doc *Document, now time.Time) error {
	if !cfg.TeamCover || cfg.Skips(config.JobTeamCover) {
		return nil
	}
	if !teamCoverDue(ctx, deps.Rounds, cfg) {
		return nil
	}
	if !anyTeamRequest(doc.PRs, cfg) {
		return nil
	}
	client := deps.GH
	cache := newCoverCache(cfg, now)
	cache.stored = deps.History
	if cache.stored != nil && cache.stored.HistoryBatch() > 0 {
		cache.batch = min(cache.stored.HistoryBatch(), cache.maxBatch)
	}
	ctx = gh.WithCallBudget(ctx, cache.budget)
	covers, err := cache.prepareAll(ctx, client, cfg, doc)
	if err != nil {
		return err
	}
	if err := cache.loadHistory(ctx, client, doc, covers); err != nil {
		return err
	}
	if err := cache.finishAll(ctx, client, cfg, doc, covers); err != nil {
		return err
	}
	cache.report(ctx, doc)
	// A review snapshot error returns before any member call, so the cache stays empty.
	if allMemberLookupsFailed(cache) {
		warn(doc, "every team lookup failed")
	}
	return nil
}

// prepareAll runs prepare for each PR until the round stops. A PR left
// unprepared gets no suggestions.
func (c *coverCache) prepareAll(ctx context.Context, client GH, cfg config.Config, doc *Document) ([]prCover, error) {
	covers := make([]prCover, len(doc.PRs))
	for i := range doc.PRs {
		covers[i] = prCover{pr: doc.PRs[i], final: true}
	}
	for i := range doc.PRs {
		if err := ctx.Err(); err != nil {
			return nil, fmt.Errorf("team cover: %w", err)
		}
		cover, err := c.prepare(ctx, client, cfg, doc, doc.PRs[i])
		if errors.Is(err, errStopped) {
			break
		}
		if err != nil {
			return nil, err
		}
		covers[i] = cover
	}
	return covers, nil
}

// finishAll sets each PR's suggestions; a PR that must wait keeps none.
func (c *coverCache) finishAll(ctx context.Context, client GH, cfg config.Config, doc *Document, covers []prCover) error {
	for i := range covers {
		suggestions, err := c.finish(ctx, client, cfg, doc, &covers[i])
		if errors.Is(err, errStopped) {
			continue
		}
		if err != nil {
			return err
		}
		doc.PRs[i].TeamReviewSuggestions = suggestions
	}
	return nil
}

// teamCoverDue reports whether this round runs the job under
// team_cover_every. Without a counter every round runs.
func teamCoverDue(ctx context.Context, rounds RoundCounter, cfg config.Config) bool {
	if cfg.TeamCoverEvery <= 1 || rounds == nil {
		return true
	}
	n := rounds.NextRound(config.JobTeamCover)
	if (n-1)%cfg.TeamCoverEvery == 0 {
		return true
	}
	runlog.FromContext(ctx).Info("team-cover skipped this round", "round", n, "every", cfg.TeamCoverEvery)
	return false
}

// anyTeamRequest reports whether some PR has a requested team that is not
// skipped. Without one no PR can need cover, so the job makes no calls.
func anyTeamRequest(prs []PR, cfg config.Config) bool {
	for _, pr := range prs {
		org, err := repoOrg(pr.Repo)
		if err != nil {
			return true
		}
		if teams, _ := teamRequests(pr.ReviewRequests, org, cfg.TeamCoverSkipTeams); len(teams) > 0 {
			return true
		}
	}
	return false
}

func allMemberLookupsFailed(cache *coverCache) bool {
	if len(cache.members) == 0 {
		return false
	}
	for _, cached := range cache.members {
		if cached.err == "" {
			return false
		}
	}
	return true
}

// prepare is the first pass: coverage, changed files and CODEOWNERS.
func (c *coverCache) prepare(ctx context.Context, client GH, cfg config.Config, doc *Document, pr PR) (prCover, error) {
	cover := prCover{pr: pr, final: true}
	org, err := repoOrg(pr.Repo)
	if err != nil {
		return cover, err
	}
	teams, requested := teamRequests(pr.ReviewRequests, org, cfg.TeamCoverSkipTeams)
	if len(teams) == 0 {
		return cover, nil
	}
	author, reviews, snapErr, err := c.snapshot(ctx, client, doc, pr)
	if err != nil {
		return cover, err
	}
	if snapErr != "" {
		cover.suggest = failedTeams(teams, snapErr)
		return cover, nil
	}
	if author == "" {
		author = cfg.Author
	}
	memberErr, err := c.ensureMembers(ctx, client, doc, teams)
	if err != nil {
		return cover, err
	}
	facts := coverFacts{
		author:    author,
		requested: requested,
		reviews:   reviews,
		members:   c.memberLists(),
		memberErr: memberErr,
		exclude:   cfg.TeamCoverExcludeLogins,
		signals:   slices.Clone(cfg.TeamCoverSignals),
		perTeam:   cfg.TeamCoverPerTeam,
		teams:     teams,
		inactive:  map[string]bool{},
		load:      map[string]int{},
		history:   map[string]gh.PathHistory{},
	}
	cover.final = false
	if !needsRanking(facts) {
		cover.facts = facts
		return cover, nil
	}
	cover.ranking = true
	wantOwners := signalEnabled(facts.signals, "codeowners")
	cover.history = signalEnabled(facts.signals, "history") && hasCandidates(facts)
	if wantOwners || cover.history {
		files, err := c.changedFiles(ctx, client, doc, pr)
		if err != nil {
			return cover, err
		}
		facts.files = files
	}
	if wantOwners {
		if err := c.fillCodeowners(ctx, client, doc, pr, &facts); err != nil {
			return cover, err
		}
	}
	cover.facts = facts
	return cover, nil
}

// finish is the last pass: history from loadHistory, activity, load, and
// the suggestions. A PR waits (errStopped) after a rate limit, while some
// of its paths are deferred, or when the cap stops a call it needs.
func (c *coverCache) finish(ctx context.Context, client GH, cfg config.Config, doc *Document, cover *prCover) ([]TeamReviewSuggestion, error) {
	if cover.final {
		return cover.suggest, nil
	}
	if c.stop == stopRateLimited {
		return nil, errStopped
	}
	facts := &cover.facts
	if cover.ranking {
		if cover.history {
			if c.anyDeferred(cover.pr, facts.files) {
				return nil, errStopped
			}
			c.fillHistory(cover.pr, facts)
		}
		org := repoOrgMust(cover.pr.Repo)
		if err := c.fillActivity(ctx, client, cfg, doc, org, facts); err != nil {
			return nil, err
		}
		if err := c.fillLoad(ctx, client, doc, org, facts); err != nil {
			return nil, err
		}
	}
	return suggestTeams(*facts), nil
}

func (c *coverCache) fillCodeowners(ctx context.Context, client GH, doc *Document, pr PR, facts *coverFacts) error {
	key := pr.Repo + "@" + pr.Base
	if c.rulesFailed[key] {
		facts.signals = dropSignal(facts.signals, "codeowners")
		return nil
	}
	rules, ok := c.rules[key]
	if !ok {
		body, err := client.Codeowners(ctx, pr.Repo, pr.Base)
		if err != nil {
			if stop := contextStop(ctx, err); stop != nil {
				return stop
			}
			if c.halted(err) {
				return errStopped
			}
			loud(doc, fmt.Sprintf("skipping team_cover codeowners signal for %s: %s", pr.Repo, err.Error()))
			c.rulesFailed[key] = true
			facts.signals = dropSignal(facts.signals, "codeowners")
			return nil
		}
		rules = parseCODEOWNERS(body)
		c.rules[key] = rules
	}
	facts.rules = rules
	facts.members = c.memberLists()
	_, err := c.ensureMembers(ctx, client, doc, ownerTeams(rules, facts.files))
	if err != nil {
		return err
	}
	facts.members = c.memberLists()
	for team, msg := range c.memberErrs() {
		if facts.memberErr[team] == "" {
			facts.memberErr[team] = msg
		}
	}
	return nil
}

// fillHistory copies what loadHistory fetched for the PR's files. A path
// whose fetch failed is left out.
func (c *coverCache) fillHistory(pr PR, facts *coverFacts) {
	for _, file := range facts.files {
		if hist, ok := c.history[historyKey(pr.Repo, pr.Base, file)]; ok {
			facts.history[file] = hist
		}
	}
}

func historyKey(repo, base, path string) string {
	return repo + "@" + base + "\x00" + path
}

func (c *coverCache) anyDeferred(pr PR, files []string) bool {
	for _, file := range files {
		if c.deferred[historyKey(pr.Repo, pr.Base, file)] {
			return true
		}
	}
	return false
}

func (c *coverCache) deferPaths(group historyGroup, paths []string) {
	for _, path := range paths {
		c.deferred[historyKey(group.repo, group.base, path)] = true
	}
}

// historyGroup is the paths one repository and base need history for.
type historyGroup struct {
	repo, base string
	paths      []string
}

// loadHistory fetches history for every path of every PR that ranks by
// history, once per path, in one batched call per repository and base.
// With a stored cache, a path is reused while its entry is fresh: keyed by
// the window's start day under team_cover_history_ttl, else by the base
// head. Once the round stops, paths not stored are deferred. It logs how
// many listed paths need history, and the cache hits and misses.
func (c *coverCache) loadHistory(ctx context.Context, client GH, doc *Document, covers []prCover) error {
	groups := historyGroups(covers)
	c.listed = listedPaths(covers)
	for _, group := range groups {
		c.needed += len(group.paths)
	}
	if c.listed > 0 {
		runlog.FromContext(ctx).Info(fmt.Sprintf("team-cover: %d of %d paths need history", c.needed, c.listed))
	}
	if len(groups) == 0 {
		return nil
	}
	for _, group := range groups {
		if err := c.loadGroup(ctx, client, doc, group); err != nil && !errors.Is(err, errStopped) {
			return err
		}
	}
	runlog.FromContext(ctx).Info("team-cover cache", "hits", c.hits, "misses", c.needed-c.hits)
	return nil
}

// listedPaths counts the distinct paths of the PRs whose files were listed.
func listedPaths(covers []prCover) int {
	seen := map[string]struct{}{}
	for _, cover := range covers {
		for _, file := range cover.facts.files {
			seen[historyKey(cover.pr.Repo, cover.pr.Base, file)] = struct{}{}
		}
	}
	return len(seen)
}

func historyGroups(covers []prCover) []historyGroup {
	var groups []historyGroup
	index := map[string]int{}
	seen := map[string]struct{}{}
	for _, cover := range covers {
		if !cover.history {
			continue
		}
		key := cover.pr.Repo + "@" + cover.pr.Base
		i, ok := index[key]
		if !ok {
			i = len(groups)
			index[key] = i
			groups = append(groups, historyGroup{repo: cover.pr.Repo, base: cover.pr.Base})
		}
		for _, file := range cover.facts.files {
			pathKey := historyKey(cover.pr.Repo, cover.pr.Base, file)
			if _, done := seen[pathKey]; done {
				continue
			}
			seen[pathKey] = struct{}{}
			groups[i].paths = append(groups[i].paths, file)
		}
	}
	return groups
}

func (c *coverCache) loadGroup(ctx context.Context, client GH, doc *Document, group historyGroup) error {
	if len(group.paths) == 0 {
		return nil
	}
	version, ref, err := c.historyVersion(ctx, client, doc, group)
	if err != nil {
		if errors.Is(err, errStopped) {
			c.deferPaths(group, group.paths)
		}
		return err
	}
	missing := c.storedHistory(group, version)
	if len(missing) == 0 {
		return nil
	}
	if c.stop != "" {
		c.deferPaths(group, missing)
		return errStopped
	}
	got, err := client.PathHistories(ctx, group.repo, ref, missing, c.historySince, c.batch, c.maxBatch)
	if err != nil {
		if stop := contextStop(ctx, err); stop != nil {
			return stop
		}
		if c.halted(err) {
			c.deferPaths(group, missing)
			return errStopped
		}
		loud(doc, fmt.Sprintf("team cover history unavailable for %s %s: %s", group.repo, group.base, err.Error()))
		return nil
	}
	c.keepHistory(group, version, got)
	if len(got.Deferred) > 0 {
		c.deferPaths(group, got.Deferred)
		c.halted(gh.ErrCallCap)
		return errStopped
	}
	return nil
}

// storedHistory takes group's paths stored under version and returns the
// others.
func (c *coverCache) storedHistory(group historyGroup, version string) []string {
	if version == "" {
		return group.paths
	}
	var missing []string
	for _, path := range group.paths {
		if hist, ok := c.stored.PathHistory(group.repo, group.base, path, version); ok {
			c.history[historyKey(group.repo, group.base, path)] = hist
			c.hits++
			continue
		}
		missing = append(missing, path)
	}
	return missing
}

// keepHistory records what PathHistories fetched, and stores it and the
// batch size that worked.
func (c *coverCache) keepHistory(group historyGroup, version string, got gh.PathHistoryResult) {
	if got.Batch > 0 {
		c.batch = got.Batch
		if c.stored != nil {
			c.stored.PutHistoryBatch(got.Batch)
		}
	}
	for path, hist := range got.Histories {
		c.history[historyKey(group.repo, group.base, path)] = hist
		if version != "" {
			c.stored.PutPathHistory(group.repo, group.base, path, version, hist)
		}
	}
}

// historyVersion is the key the stored cache holds group's history under,
// and the ref to fetch it on. With a history TTL the key is the window's
// start day and the ref the base branch; otherwise both are the base head.
// The key is empty without a stored cache or when the head lookup fails;
// history is then fetched on the branch and not stored.
func (c *coverCache) historyVersion(ctx context.Context, client GH, doc *Document, group historyGroup) (string, string, error) {
	if c.stored == nil {
		return "", group.base, nil
	}
	if c.historyTTL > 0 {
		return "since:" + c.historySince.UTC().Format(time.DateOnly), group.base, nil
	}
	if c.stop != "" {
		return "", "", errStopped
	}
	head, err := c.baseHead(ctx, client, doc, group)
	if err != nil || head == "" {
		return "", group.base, err
	}
	return head, head, nil
}

// baseHead is the head of group's base branch, or empty when the lookup
// fails.
func (c *coverCache) baseHead(ctx context.Context, client GH, doc *Document, group historyGroup) (string, error) {
	head, err := client.BranchHead(ctx, group.repo, group.base)
	if err != nil {
		if stop := contextStop(ctx, err); stop != nil {
			return "", stop
		}
		if c.halted(err) {
			return "", errStopped
		}
		loud(doc, fmt.Sprintf("team cover base head unavailable for %s %s: %s", group.repo, group.base, err.Error()))
		return "", nil
	}
	return head, nil
}

func (c *coverCache) fillActivity(ctx context.Context, client GH, cfg config.Config, doc *Document, org string, facts *coverFacts) error {
	if cfg.TeamCoverActiveDays <= 0 || c.activeSkip {
		return nil
	}
	for _, login := range loginsToCheck(*facts) {
		inactive, err := c.inactiveLogin(ctx, client, org, login)
		if err != nil {
			if stop := contextStop(ctx, err); stop != nil {
				return stop
			}
			if c.halted(err) {
				return errStopped
			}
			c.activeSkip = true
			loud(doc, "skipping team_cover activity filter: "+err.Error())
			facts.inactive = map[string]bool{}
			return nil
		}
		if inactive {
			facts.inactive[strings.ToLower(login)] = true
		}
	}
	return nil
}

func (c *coverCache) fillLoad(ctx context.Context, client GH, doc *Document, org string, facts *coverFacts) error {
	if !signalEnabled(facts.signals, "load") {
		return nil
	}
	if c.loadSkipped {
		facts.signals = dropSignal(facts.signals, "load")
		facts.loadOn = false
		return nil
	}
	for _, login := range loginsToCheck(*facts) {
		count, ok, err := c.reviewLoad(ctx, client, doc, org, login)
		if err != nil {
			return err
		}
		if !ok {
			facts.signals = dropSignal(facts.signals, "load")
			facts.loadOn = false
			facts.load = map[string]int{}
			return nil
		}
		facts.load[login] = count
	}
	facts.loadOn = true
	return nil
}

func (c *coverCache) snapshot(ctx context.Context, client GH, doc *Document, pr PR) (string, []gh.LatestReview, string, error) {
	author, reviews, err := client.ReviewSnapshot(ctx, pr.Repo, pr.Number)
	if err != nil {
		if stop := contextStop(ctx, err); stop != nil {
			return "", nil, "", stop
		}
		if c.halted(err) {
			return "", nil, "", errStopped
		}
		msg := fmt.Sprintf("cannot list reviews of %s#%d: %s", pr.Repo, pr.Number, err.Error())
		loud(doc, msg)
		return "", nil, msg, nil
	}
	return author, reviews, "", nil
}

func (c *coverCache) ensureMembers(ctx context.Context, client GH, doc *Document, teams []string) (map[string]string, error) {
	errs := map[string]string{}
	for _, team := range teams {
		if cached, ok := c.members[team]; ok {
			if cached.err != "" {
				errs[team] = cached.err
			}
			continue
		}
		org, slug, ok := strings.Cut(team, "/")
		if !ok || org == "" || slug == "" {
			msg := fmt.Sprintf("cannot list members of %s: invalid team", team)
			c.members[team] = cachedMembers{err: msg}
			errs[team] = msg
			loud(doc, msg)
			continue
		}
		members, err := client.TeamMembers(ctx, org, slug)
		if err != nil {
			if stop := contextStop(ctx, err); stop != nil {
				return nil, stop
			}
			if c.halted(err) {
				return nil, errStopped
			}
			msg := fmt.Sprintf("cannot list members of %s: %s", team, err.Error())
			c.members[team] = cachedMembers{err: msg}
			errs[team] = msg
			loud(doc, msg)
			continue
		}
		c.members[team] = cachedMembers{members: members}
	}
	return errs, nil
}

func (c *coverCache) changedFiles(ctx context.Context, client GH, doc *Document, pr PR) ([]string, error) {
	key := fmt.Sprintf("%s#%d", pr.Repo, pr.Number)
	if files, ok := c.files[key]; ok {
		return files, nil
	}
	files, err := client.ChangedFiles(ctx, pr.Repo, pr.Number)
	if err != nil {
		if stop := contextStop(ctx, err); stop != nil {
			return nil, stop
		}
		if c.halted(err) {
			return nil, errStopped
		}
		loud(doc, fmt.Sprintf("team cover files unavailable for %s#%d: %s", pr.Repo, pr.Number, err.Error()))
		files = []string{}
	}
	slices.Sort(files)
	c.files[key] = files
	return files, nil
}

func (c *coverCache) inactiveLogin(ctx context.Context, client GH, org, login string) (bool, error) {
	key := org + "/" + strings.ToLower(login)
	if active, ok := c.active[key]; ok {
		return !active, nil
	}
	active, err := client.OrgActive(ctx, org, login, c.activeSince)
	if err != nil {
		return false, fmt.Errorf("org activity for %s: %w", login, err)
	}
	c.active[key] = active
	return !active, nil
}

func (c *coverCache) reviewLoad(ctx context.Context, client GH, doc *Document, org, login string) (int, bool, error) {
	if c.loadSkipped {
		return 0, false, nil
	}
	key := org + "/" + strings.ToLower(login)
	if count, ok := c.load[key]; ok {
		return count, true, nil
	}
	stored := c.stored != nil && c.loadTTL > 0
	if stored {
		if count, ok := c.stored.ReviewLoad(org, login); ok {
			c.load[key] = count
			return count, true, nil
		}
	}
	count, err := client.DirectReviewCount(ctx, org, login)
	if err != nil {
		if stop := contextStop(ctx, err); stop != nil {
			return 0, false, stop
		}
		if c.halted(err) {
			return 0, false, errStopped
		}
		c.loadSkipped = true
		loud(doc, "skipping team_cover load signal: "+err.Error())
		return 0, false, nil
	}
	c.load[key] = count
	if stored {
		c.stored.PutReviewLoad(org, login, count)
	}
	return count, true, nil
}

func (c *coverCache) memberLists() map[string][]gh.TeamMember {
	out := make(map[string][]gh.TeamMember, len(c.members))
	for team, cached := range c.members {
		if cached.err == "" {
			out[team] = cached.members
		}
	}
	return out
}

func (c *coverCache) memberErrs() map[string]string {
	out := map[string]string{}
	for team, cached := range c.members {
		if cached.err != "" {
			out[team] = cached.err
		}
	}
	return out
}

func teamRequests(formatted []string, org string, skip []string) ([]string, []string) {
	var teams []string
	var users []string
	seen := map[string]struct{}{}
	for _, req := range formatted {
		kind, value, ok := strings.Cut(req, ":")
		if !ok || value == "" {
			continue
		}
		if kind != "Team" {
			users = append(users, value)
			continue
		}
		team := value
		if !strings.Contains(value, "/") {
			team = org + "/" + value
		}
		if _, done := seen[team]; done || containsFold(skip, team) {
			continue
		}
		seen[team] = struct{}{}
		teams = append(teams, team)
	}
	return teams, users
}

func ownerTeams(rules []codeRule, files []string) []string {
	var teams []string
	seen := map[string]struct{}{}
	for _, file := range files {
		rule, ok := lastCodeOwner(rules, file)
		if !ok {
			continue
		}
		for _, owner := range rule.owners {
			if owner.team == "" {
				continue
			}
			if _, done := seen[owner.team]; done {
				continue
			}
			seen[owner.team] = struct{}{}
			teams = append(teams, owner.team)
		}
	}
	return teams
}

func loginsToCheck(facts coverFacts) []string {
	var out []string
	seen := map[string]struct{}{}
	for _, team := range facts.teams {
		if facts.memberErr[team] != "" {
			continue
		}
		if coveredBy(facts.members[team], facts.requested, facts.reviews) != "" {
			continue
		}
		for _, member := range facts.members[team] {
			if !keepCandidate(member, facts) {
				continue
			}
			key := strings.ToLower(member.Login)
			if _, done := seen[key]; done {
				continue
			}
			seen[key] = struct{}{}
			out = append(out, member.Login)
		}
	}
	return out
}

func failedTeams(teams []string, msg string) []TeamReviewSuggestion {
	out := make([]TeamReviewSuggestion, len(teams))
	for i, team := range teams {
		out[i] = TeamReviewSuggestion{Team: team, Suggest: []SuggestedReviewer{}, Error: msg}
	}
	return out
}

// hasCandidates reports whether some uncovered team has a member left to
// rank. Without one, history cannot change the suggestions.
func hasCandidates(facts coverFacts) bool {
	for _, team := range facts.teams {
		if facts.memberErr[team] != "" {
			continue
		}
		if coveredBy(facts.members[team], facts.requested, facts.reviews) != "" {
			continue
		}
		for _, member := range facts.members[team] {
			if keepCandidate(member, facts) {
				return true
			}
		}
	}
	return false
}

func needsRanking(facts coverFacts) bool {
	for _, team := range facts.teams {
		if facts.memberErr[team] != "" {
			continue
		}
		if coveredBy(facts.members[team], facts.requested, facts.reviews) == "" {
			return true
		}
	}
	return false
}

func signalEnabled(signals []string, name string) bool {
	return slices.Contains(signals, name)
}

func dropSignal(signals []string, name string) []string {
	out := make([]string, 0, len(signals))
	for _, signal := range signals {
		if signal != name {
			out = append(out, signal)
		}
	}
	return out
}

func repoOrg(repo string) (string, error) {
	org, _, err := splitRepo(repo)
	return org, err
}

func repoOrgMust(repo string) string {
	org, _, _ := splitRepo(repo)
	return org
}

func contextStop(ctx context.Context, err error) error {
	if err == nil || ctx.Err() == nil {
		return nil
	}
	return fmt.Errorf("team cover: %w", ctx.Err())
}

func loud(doc *Document, msg string) {
	_, _ = fmt.Fprintf(os.Stderr, "prsync: %s\n", msg)
	warn(doc, msg)
}
