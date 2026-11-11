package scan

import (
	"context"
	"fmt"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
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
	load         map[string]int
	active       map[string]bool
	loadSkipped  bool
	histSkipped  bool
	activeSkip   bool
	rulesFailed  map[string]bool
}

func newCoverCache(cfg config.Config, now time.Time) *coverCache {
	return &coverCache{
		historySince: now.AddDate(0, 0, -cfg.TeamCoverHistoryDays),
		activeSince:  now.AddDate(0, 0, -cfg.TeamCoverActiveDays),
		members:      map[string]cachedMembers{},
		files:        map[string][]string{},
		rules:        map[string][]codeRule{},
		history:      map[string]gh.PathHistory{},
		load:         map[string]int{},
		active:       map[string]bool{},
		rulesFailed:  map[string]bool{},
	}
}

func coverTeams(ctx context.Context, client GH, cfg config.Config, doc *Document, now time.Time) error {
	if !cfg.TeamCover || cfg.Skips(config.JobTeamCover) {
		return nil
	}
	cache := newCoverCache(cfg, now)
	for i := range doc.PRs {
		if err := ctx.Err(); err != nil {
			return fmt.Errorf("team cover: %w", err)
		}
		suggestions, err := cache.coverPR(ctx, client, cfg, doc, doc.PRs[i])
		if err != nil {
			return err
		}
		doc.PRs[i].TeamReviewSuggestions = suggestions
	}
	// A review snapshot error returns before any member call, so the cache stays empty.
	if allMemberLookupsFailed(cache) {
		warn(doc, "every team lookup failed")
	}
	return nil
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

func (c *coverCache) coverPR(ctx context.Context, client GH, cfg config.Config, doc *Document, pr PR) ([]TeamReviewSuggestion, error) {
	org, err := repoOrg(pr.Repo)
	if err != nil {
		return nil, err
	}
	teams, requested := teamRequests(pr.ReviewRequests, org, cfg.TeamCoverSkipTeams)
	if len(teams) == 0 {
		return nil, nil
	}
	author, reviews, snapErr, err := c.snapshot(ctx, client, doc, pr)
	if err != nil {
		return nil, err
	}
	if snapErr != "" {
		return failedTeams(teams, snapErr), nil
	}
	if author == "" {
		author = cfg.Author
	}
	memberErr, err := c.ensureMembers(ctx, client, doc, teams)
	if err != nil {
		return nil, err
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
	if err := c.fillSignals(ctx, client, cfg, doc, pr, &facts); err != nil {
		return nil, err
	}
	return suggestTeams(facts), nil
}

func (c *coverCache) fillSignals(ctx context.Context, client GH, cfg config.Config, doc *Document, pr PR, facts *coverFacts) error {
	if !needsRanking(*facts) {
		return nil
	}
	wantOwners := signalEnabled(facts.signals, "codeowners")
	wantHistory := signalEnabled(facts.signals, "history") && !c.histSkipped
	if wantOwners || wantHistory {
		files, err := c.changedFiles(ctx, client, doc, pr)
		if err != nil {
			return err
		}
		facts.files = files
	}
	if wantOwners {
		if err := c.fillCodeowners(ctx, client, doc, pr, facts); err != nil {
			return err
		}
	}
	if wantHistory {
		if err := c.fillHistory(ctx, client, doc, pr, facts); err != nil {
			return err
		}
	}
	if err := c.fillActivity(ctx, client, cfg, doc, repoOrgMust(pr.Repo), facts); err != nil {
		return err
	}
	return c.fillLoad(ctx, client, doc, repoOrgMust(pr.Repo), facts)
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

func (c *coverCache) fillHistory(ctx context.Context, client GH, doc *Document, pr PR, facts *coverFacts) error {
	for _, file := range facts.files {
		if c.histSkipped {
			facts.signals = dropSignal(facts.signals, "history")
			return nil
		}
		hist, err := c.pathHistory(ctx, client, pr, file)
		if err != nil {
			if stop := contextStop(ctx, err); stop != nil {
				return stop
			}
			if gh.IsRateLimited(err) {
				c.histSkipped = true
				loud(doc, "skipping team_cover history signal: "+err.Error())
				facts.signals = dropSignal(facts.signals, "history")
				facts.history = map[string]gh.PathHistory{}
				return nil
			}
			loud(doc, fmt.Sprintf("team cover history unavailable for %s %s: %s", pr.Repo, file, err.Error()))
			continue
		}
		facts.history[file] = hist
	}
	return nil
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
		loud(doc, fmt.Sprintf("team cover files unavailable for %s#%d: %s", pr.Repo, pr.Number, err.Error()))
		files = []string{}
	}
	slices.Sort(files)
	c.files[key] = files
	return files, nil
}

func (c *coverCache) pathHistory(ctx context.Context, client GH, pr PR, file string) (gh.PathHistory, error) {
	key := pr.Repo + "@" + pr.Base + "\x00" + file
	if hist, ok := c.history[key]; ok {
		return hist, nil
	}
	hist, err := client.PathHistory(ctx, pr.Repo, pr.Base, file, c.historySince)
	if err != nil {
		return gh.PathHistory{}, fmt.Errorf("path history for %s: %w", file, err)
	}
	c.history[key] = hist
	return hist, nil
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
	count, err := client.DirectReviewCount(ctx, org, login)
	if err != nil {
		if stop := contextStop(ctx, err); stop != nil {
			return 0, false, stop
		}
		c.loadSkipped = true
		loud(doc, "skipping team_cover load signal: "+err.Error())
		return 0, false, nil
	}
	c.load[key] = count
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
