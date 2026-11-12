// Package scan classifies open PRs and matches them to herdr tabs.
package scan

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

const herdrMinVersion = "0.8.0"

// Deps is the injected GitHub and herdr adapters.
type Deps struct {
	GH    GH
	Herdr Herdr
	// Remote returns the herdr client for one of config machines. Nil
	// searches only the local server.
	Remote func(machine string) Herdr
	// Marker returns the gh-merge rca_marker for one repository.
	// Nil skips merge-queue-fix.
	Marker func(owner, repo string) (string, error)
	// History keeps team-cover path history between rounds. Nil keeps it
	// for this round only.
	History HistoryCache
	// Rounds counts team-cover rounds for team_cover_every. Nil runs the
	// job every round.
	Rounds RoundCounter
	// Threads keeps each pull request's review threads between rounds.
	// Nil, or review_threads_ttl 0, fetches them every round.
	Threads ThreadCache
}

// ThreadCache stores a pull request's review threads with its head commit
// and updatedAt, and owns the expiry (review_threads_ttl).
type ThreadCache interface {
	ReviewThreads(repo string, number int, head, updatedAt string) ([]gh.Thread, bool)
	PutReviewThreads(repo string, number int, head, updatedAt string, threads []gh.Thread)
	// PruneReviewThreads drops repo's entries whose number is not in open.
	PruneReviewThreads(repo string, open []int)
}

// HistoryCache stores the path history of path on ref (the base branch),
// keyed also by a version: the history window's start day, or the head
// commit of ref when team_cover_history_ttl is 0. It owns the expiry. It
// also stores the path-history request size that last worked, each
// login's review-load count for team_cover_load_ttl, team members for
// team_cover_members_ttl, CODEOWNERS for team_cover_codeowners_ttl, and
// each pull request's changed files (per head) and review snapshot (per
// head and updatedAt).
type HistoryCache interface {
	PathHistory(repo, ref, path, version string) (gh.PathHistory, bool)
	PutPathHistory(repo, ref, path, version string, history gh.PathHistory)
	// NearestPathHistory yields fresh entries closest to expiry, oldest
	// first, and stops when yield returns false. At most n are yielded;
	// n <= 0 yields every fresh one. A cache that does not track expiry
	// yields nothing. yield must not call the cache.
	NearestPathHistory(n int, yield func(repo, ref, path, version string) bool)
	// HistoryBatch is 0 when no size is stored.
	HistoryBatch() int
	PutHistoryBatch(n int)
	ReviewLoad(org, login string) (int, bool)
	PutReviewLoad(org, login string, n int)
	TeamMembers(org, slug string) ([]gh.TeamMember, bool)
	PutTeamMembers(org, slug string, members []gh.TeamMember)
	ChangedFiles(repo string, number int, head string) ([]string, bool)
	PutChangedFiles(repo string, number int, head string, files []string)
	// Codeowners returns the body while it is fresh (ok) and, longer, the
	// path it was found at.
	Codeowners(repo, ref string) (body, path string, ok bool)
	PutCodeowners(repo, ref, path, body string)
	ReviewSnapshot(repo string, number int, head, updatedAt string) (string, []gh.LatestReview, bool)
	PutReviewSnapshot(repo string, number int, head, updatedAt, author string, reviews []gh.LatestReview)
}

// RoundCounter counts rounds of a job across runs.
type RoundCounter interface {
	// NextRound counts one more round of job and returns the count,
	// starting at 1.
	NextRound(job string) int
}

// GH is the GitHub surface scan uses.
type GH interface {
	AuthStatus(ctx context.Context) error
	UserLogin(ctx context.Context) (string, error)
	SearchOpenPRRepos(ctx context.Context, author string) (repos []string, capped bool, err error)
	ListOpenPRs(ctx context.Context, repo, author string) ([]gh.PRListItem, error)
	ViewPR(ctx context.Context, repo string, number int) (gh.PRListItem, error)
	ReviewThreads(ctx context.Context, owner, repo string, number int) ([]gh.Thread, error)
	MarkerComments(ctx context.Context, owner, repo string, number int) (gh.MarkerComments, error)
	ReviewSnapshot(ctx context.Context, repo string, number int) (author string, reviews []gh.LatestReview, err error)
	TeamMembers(ctx context.Context, org, slug string) ([]gh.TeamMember, error)
	ChangedFiles(ctx context.Context, repo string, number int) ([]string, error)
	// Codeowners tries first before GitHub's other locations and returns
	// the body and the path it was found at.
	Codeowners(ctx context.Context, repo, ref, first string) (body, path string, err error)
	BranchHead(ctx context.Context, repo, branch string) (string, error)
	PathHistories(ctx context.Context, repo, ref string, paths []string, since time.Time, batch, maxBatch int) (gh.PathHistoryResult, error)
	DirectReviewCount(ctx context.Context, org, login string) (int, error)
	OrgActive(ctx context.Context, org, login string, since time.Time) (bool, error)
}

// Target is one pull request to classify without listing its repository.
type Target struct {
	Repo   string
	Number int
}

// Herdr is the herdr surface scan uses.
type Herdr interface {
	RequireMin(ctx context.Context, minimum string) error
	TabList(ctx context.Context) ([]herdr.Tab, error)
	AgentList(ctx context.Context) ([]herdr.Agent, error)
}

// Started reports whether work began and a document should be emitted.
func Started(doc Document) bool {
	return doc.Author != "" || len(doc.Repos) > 0 || len(doc.PRs) > 0
}

// Run surveys open PRs. The returned document is the work so far; a non-nil
// error is fatal (exit 3) after the caller writes that document when Started.
func Run(ctx context.Context, deps Deps, cfg config.Config, repos []string, now time.Time) (Document, error) {
	ctx = gh.WithRollupStats(ctx)
	doc := emptyDocument(now)
	if err := deps.GH.AuthStatus(ctx); err != nil {
		return finish(doc, fmt.Errorf("gh auth: %w", err))
	}
	author, err := resolveAuthor(ctx, deps.GH, cfg.Author)
	if err != nil {
		return finish(doc, err)
	}
	doc.Author = author
	cfg.Author = author

	resolved, err := resolveRepos(ctx, deps.GH, cfg, repos, &doc)
	if err != nil {
		return finish(doc, err)
	}
	doc.Repos = resolved

	threads := newThreadLookup(deps, cfg)
	err = classifyRepos(ctx, deps.GH, threads, cfg, resolved, deps.Marker, &doc)
	threads.report(ctx)
	reportStatusChecks(ctx)
	if err != nil {
		return finish(doc, err)
	}
	if err := coverTeams(ctx, deps, cfg, &doc, now); err != nil {
		return finish(doc, err)
	}
	if err := matchTabs(ctx, deps, cfg, &doc); err != nil {
		return finish(doc, err)
	}
	return finish(doc, nil)
}

// RunTargets classifies targets with one pull-request view and review threads
// for those PRs only, then matches herdr tabs. It does not search repositories
// or list every open PR. A missing PR is omitted. An inaccessible repository
// is recorded and its PRs are omitted. Other view errors are fatal.
func RunTargets(ctx context.Context, deps Deps, cfg config.Config, targets []Target, now time.Time) (Document, error) {
	ctx = gh.WithRollupStats(ctx)
	doc := emptyDocument(now)
	if err := deps.GH.AuthStatus(ctx); err != nil {
		return finish(doc, fmt.Errorf("gh auth: %w", err))
	}
	author, err := resolveAuthor(ctx, deps.GH, cfg.Author)
	if err != nil {
		return finish(doc, err)
	}
	doc.Author = author
	cfg.Author = author

	unique := dedupeTargets(targets)
	doc.Repos = reposOf(unique)

	threads := newThreadLookup(deps, cfg)
	defer threads.report(ctx)
	defer reportStatusChecks(ctx)
	for _, target := range unique {
		if err := ctx.Err(); err != nil {
			return finish(doc, fmt.Errorf("scan: %w", err))
		}
		owner, name, err := splitRepo(target.Repo)
		if err != nil {
			return finish(doc, err)
		}
		item, err := deps.GH.ViewPR(ctx, target.Repo, target.Number)
		if err != nil {
			if errors.Is(err, gh.ErrNotFound) {
				continue
			}
			if errors.Is(err, gh.ErrInaccessible) {
				doc.InaccessibleRepos = appendUnique(doc.InaccessibleRepos, target.Repo)
				continue
			}
			return finish(doc, fmt.Errorf("view pr %s#%d: %w", target.Repo, target.Number, err))
		}
		classified, err := classifyPR(ctx, deps.GH, threads, cfg, owner, name, target.Repo, item, deps.Marker)
		if err != nil {
			return finish(doc, err)
		}
		doc.PRs = append(doc.PRs, classified)
	}
	if err := coverTeams(ctx, deps, cfg, &doc, now); err != nil {
		return finish(doc, err)
	}
	if err := matchTabs(ctx, deps, cfg, &doc); err != nil {
		return finish(doc, err)
	}
	return finish(doc, nil)
}

// reportStatusChecks logs the round's status-check cache hits and calls.
func reportStatusChecks(ctx context.Context) {
	hits, calls := gh.RollupStats(ctx)
	runlog.FromContext(ctx).Info(fmt.Sprintf("scan: status checks %d hits/%d calls", hits, calls))
}

func dedupeTargets(targets []Target) []Target {
	seen := make(map[string]struct{}, len(targets))
	out := make([]Target, 0, len(targets))
	for _, target := range targets {
		key := fmt.Sprintf("%s#%d", target.Repo, target.Number)
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		out = append(out, target)
	}
	return out
}

func reposOf(targets []Target) []string {
	repos := make([]string, 0, len(targets))
	seen := make(map[string]struct{}, len(targets))
	for _, target := range targets {
		if _, ok := seen[target.Repo]; ok {
			continue
		}
		seen[target.Repo] = struct{}{}
		repos = append(repos, target.Repo)
	}
	return repos
}

func appendUnique(list []string, value string) []string {
	for _, existing := range list {
		if existing == value {
			return list
		}
	}
	return append(list, value)
}

func finish(doc Document, err error) (Document, error) {
	doc.Summary = summarize(doc.PRs)
	return doc, err
}

func emptyDocument(now time.Time) Document {
	return Document{
		GeneratedAt:       now.UTC().Format(time.RFC3339),
		Repos:             []string{},
		PRs:               []PR{},
		InaccessibleRepos: []string{},
		Warnings:          []string{},
	}
}

func resolveAuthor(ctx context.Context, client GH, configured string) (string, error) {
	if configured != "" {
		return configured, nil
	}
	login, err := client.UserLogin(ctx)
	if err != nil {
		return "", fmt.Errorf("resolve author: %w", err)
	}
	return login, nil
}

func resolveRepos(ctx context.Context, client GH, cfg config.Config, override []string, doc *Document) ([]string, error) {
	list := override
	if len(list) == 0 {
		list = cfg.Repos
	}
	if len(list) > 0 {
		return dedupe(list), nil
	}
	found, capped, err := client.SearchOpenPRRepos(ctx, cfg.Author)
	if err != nil {
		return nil, fmt.Errorf("search prs: %w", err)
	}
	if capped {
		warn(doc, "search result hit --limit 1000; some repos may be missing")
	}
	if found == nil {
		found = []string{}
	}
	return found, nil
}

func classifyRepos(ctx context.Context, client GH, threads *threadLookup, cfg config.Config, repos []string, marker func(string, string) (string, error), doc *Document) error {
	for _, repo := range repos {
		if err := ctx.Err(); err != nil {
			return fmt.Errorf("scan: %w", err)
		}
		prs, err := client.ListOpenPRs(ctx, repo, cfg.Author)
		if err != nil {
			if errors.Is(err, gh.ErrInaccessible) {
				doc.InaccessibleRepos = append(doc.InaccessibleRepos, repo)
				continue
			}
			return fmt.Errorf("list prs %s: %w", repo, err)
		}
		owner, name, err := splitRepo(repo)
		if err != nil {
			return err
		}
		threads.prune(repo, prs)
		for _, item := range prs {
			if err := ctx.Err(); err != nil {
				return fmt.Errorf("scan: %w", err)
			}
			if item.StatusCheckError != "" {
				warn(doc, fmt.Sprintf("ci rollup unavailable for %s#%d: %s", repo, item.Number, item.StatusCheckError))
			}
			classified, err := classifyPR(ctx, client, threads, cfg, owner, name, repo, item, marker)
			if err != nil {
				return err
			}
			doc.PRs = append(doc.PRs, classified)
		}
	}
	return nil
}

func classifyPR(ctx context.Context, client GH, lookup *threadLookup, cfg config.Config, owner, name, repo string, item gh.PRListItem, marker func(string, string) (string, error)) (PR, error) {
	threads, err := lookup.fetch(ctx, client, owner, name, repo, item)
	if err != nil {
		return PR{}, fmt.Errorf("%s#%d: %w", repo, item.Number, err)
	}
	comments := blockingComments(threads, cfg.Author)
	id := ExtractID(item.Title, cfg.TitleIDPattern)
	ci := "unknown"
	if item.StatusCheckError == "" {
		ci = CIState(selectCIChecks(item.StatusCheckRollup, cfg.CIIgnoreChecks, cfg.CIOnlyChecks))
	}
	reqs := formatReviewRequests(item.ReviewRequests)
	out := PR{
		Repo:             repo,
		Number:           item.Number,
		Title:            item.Title,
		URL:              item.URL,
		State:            gh.NormalizePRState(item.State),
		Identifier:       id,
		Base:             item.BaseRefName,
		Head:             item.HeadRefName,
		HeadSHA:          item.HeadRefOid,
		UpdatedAt:        item.UpdatedAt,
		MergeStateStatus: item.MergeStateStatus,
		Mergeable:        item.Mergeable,
		IsDraft:          item.IsDraft,
		ReviewDecision:   item.ReviewDecision,
		ReviewRequests:   reqs,
		CIState:          ci,
		Unaddressed:      len(comments) > 0,
		BlockingComments: comments,
	}
	actions, err := mergeQueueActions(ctx, client, marker, owner, name, item.Number)
	if err != nil {
		return PR{}, fmt.Errorf("%s#%d: %w", repo, item.Number, err)
	}
	out.Actions = actions
	out.Bucket = Bucket(PRFacts{
		Unaddressed:    out.Unaddressed,
		CIState:        ci,
		ReviewDecision: item.ReviewDecision,
		ReviewRequests: reqs,
		IsDraft:        item.IsDraft,
		Mergeable:      item.Mergeable,
	})
	return out, nil
}

func blockingComments(threads []gh.Thread, author string) []Comment {
	out := make([]Comment, 0)
	for _, thread := range threads {
		if thread.IsResolved || len(thread.Comments) == 0 {
			continue
		}
		last := thread.Comments[len(thread.Comments)-1]
		if last.Author != nil && strings.EqualFold(last.Author.Login, author) {
			continue
		}
		login := ""
		if last.Author != nil {
			login = last.Author.Login
		}
		out = append(out, Comment{
			ThreadID:  thread.ID,
			CommentID: last.ID,
			Author:    login,
			Path:      last.Path,
			Line:      last.Line,
			URL:       last.URL,
			Body:      last.Body,
		})
	}
	return out
}

func formatReviewRequests(reqs []gh.ReviewReq) []string {
	out := make([]string, 0, len(reqs))
	for _, req := range reqs {
		switch req.Type {
		case "Team":
			slug := req.Slug
			if slug == "" {
				slug = req.Name
			}
			out = append(out, "Team:"+slug)
		default:
			out = append(out, "User:"+req.Login)
		}
	}
	return out
}

// matchTabs matches each PR to a tab on the local server or on one of
// cfg.Machines. A machine that cannot be listed adds one warning. A PR
// with no tab on any listed machine, while some machine was not listed,
// gets TabUnknown instead of a no-tab warning: its tab may be there.
// Matching always covers every machine; cfg.TabMachine then marks the PRs
// owned by another machine OutOfScope and drops their per-PR warnings.
func matchTabs(ctx context.Context, deps Deps, cfg config.Config, doc *Document) error {
	client := deps.Herdr
	if err := client.RequireMin(ctx, herdrMinVersion); err != nil {
		if errors.Is(err, herdr.ErrNotInstalled) {
			nullTabs(doc, cfg)
			warn(doc, "herdr unreachable: "+err.Error())
			return nil
		}
		return fmt.Errorf("herdr version: %w", err)
	}
	tabs, err := client.TabList(ctx)
	if err != nil {
		nullTabs(doc, cfg)
		warn(doc, "herdr unreachable: "+err.Error())
		return nil
	}
	agents, err := client.AgentList(ctx)
	if err != nil {
		nullTabs(doc, cfg)
		warn(doc, "herdr unreachable: "+err.Error())
		return nil
	}
	tabs, agents, down := addMachineTabs(ctx, deps.Remote, cfg.Machines, tabs, agents, doc)
	for i := range doc.PRs {
		pr := &doc.PRs[i]
		tab, hits, warning := match(pr.Identifier, pr.Repo, pr.Number, cfg.TabLabelTemplate, tabs, agents)
		pr.Tab = tab
		if pr.Identifier != nil && len(hits) == 0 && len(down) > 0 {
			pr.TabUnknown = TabUnknownMachineUnreachable
			pr.OutOfScope = outOfScope(cfg, down, true)
			continue
		}
		pr.OutOfScope = outOfScope(cfg, tabMachines(hits), false)
		if warning != "" && pr.OutOfScope == "" {
			warn(doc, warning)
		}
	}
	return nil
}

// tabMachines names the machines of tabs, local for the local server.
func tabMachines(tabs []herdr.Tab) []string {
	var out []string
	for _, tab := range tabs {
		machine := tab.Machine
		if machine == "" {
			machine = config.LocalMachine
		}
		if !slices.Contains(out, machine) {
			out = append(out, machine)
		}
	}
	return out
}

// outOfScope is why a PR owned by owners is not in cfg.TabMachine's run,
// or empty when it is. unknown means owners are the unreachable machines
// its tab may be on. A PR with no owner belongs to the run that includes
// untabbed PRs.
func outOfScope(cfg config.Config, owners []string, unknown bool) string {
	if cfg.TabMachine == "" || slices.Contains(owners, cfg.TabMachine) {
		return ""
	}
	switch {
	case unknown:
		return "tab may be on " + strings.Join(owners, ", ") + " (unreachable)"
	case len(owners) > 0:
		return "tab on " + strings.Join(owners, ", ")
	case cfg.IncludeUntabbed():
		return ""
	default:
		return "no tab"
	}
}

// addMachineTabs appends the tabs and agents of each machine. A machine
// that fails either list adds one warning and none of its tabs, and is
// returned in down.
func addMachineTabs(ctx context.Context, remote func(string) Herdr, machines []string, tabs []herdr.Tab, agents []herdr.Agent, doc *Document) ([]herdr.Tab, []herdr.Agent, []string) {
	if remote == nil {
		return tabs, agents, nil
	}
	var down []string
	for _, machine := range machines {
		client := remote(machine)
		mt, err := client.TabList(ctx)
		var ma []herdr.Agent
		if err == nil {
			ma, err = client.AgentList(ctx)
		}
		if err != nil {
			down = append(down, machine)
			warn(doc, fmt.Sprintf("machine %s unreachable (%s); its tabs were not searched", machine, unreachableCause(err)))
			continue
		}
		tabs = append(tabs, mt...)
		agents = append(agents, ma...)
	}
	return tabs, agents, down
}

// unreachableCause is the remotecall cause phrase, or the error text for
// a herdr failure on a machine that answered.
func unreachableCause(err error) string {
	if ue := remotecall.AsUnreachable(err); ue != nil {
		return ue.Cause
	}
	return err.Error()
}

// nullTabs clears every tab when the local server cannot be listed. Each
// PR then counts as untabbed for cfg.TabMachine.
func nullTabs(doc *Document, cfg config.Config) {
	for i := range doc.PRs {
		doc.PRs[i].Tab = nil
		doc.PRs[i].OutOfScope = outOfScope(cfg, nil, false)
	}
}

func warn(doc *Document, msg string) {
	slog.Warn(msg)
	doc.Warnings = append(doc.Warnings, msg)
}

func dedupe(repos []string) []string {
	seen := make(map[string]struct{}, len(repos))
	out := make([]string, 0, len(repos))
	for _, repo := range repos {
		if _, ok := seen[repo]; ok {
			continue
		}
		seen[repo] = struct{}{}
		out = append(out, repo)
	}
	return out
}

func splitRepo(repo string) (string, string, error) {
	owner, name, ok := strings.Cut(repo, "/")
	if !ok || owner == "" || name == "" || strings.Contains(name, "/") {
		return "", "", fmt.Errorf("invalid repo %q", repo)
	}
	return owner, name, nil
}
