// Package scan classifies open PRs and matches them to herdr tabs.
package scan

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

const herdrMinVersion = "0.8.0"

// Deps is the injected GitHub and herdr adapters.
type Deps struct {
	GH    GH
	Herdr Herdr
}

// GH is the GitHub surface scan uses.
type GH interface {
	AuthStatus(ctx context.Context) error
	UserLogin(ctx context.Context) (string, error)
	SearchOpenPRRepos(ctx context.Context, author string) (repos []string, capped bool, err error)
	ListOpenPRs(ctx context.Context, repo, author string) ([]gh.PRListItem, error)
	ViewPR(ctx context.Context, repo string, number int) (gh.PRListItem, error)
	ReviewThreads(ctx context.Context, owner, repo string, number int) ([]gh.Thread, error)
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

	if err := classifyRepos(ctx, deps.GH, cfg, resolved, &doc); err != nil {
		return finish(doc, err)
	}
	if err := matchTabs(ctx, deps.Herdr, cfg, &doc); err != nil {
		return finish(doc, err)
	}
	return finish(doc, nil)
}

// RunTargets classifies targets with gh pr view and review threads for those
// PRs only, then matches herdr tabs. It does not search repositories or list
// every open PR. A missing PR is omitted. An inaccessible repository is
// recorded and its PRs are omitted. Other view errors are fatal.
func RunTargets(ctx context.Context, deps Deps, cfg config.Config, targets []Target, now time.Time) (Document, error) {
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
		classified, err := classifyPR(ctx, deps.GH, cfg, owner, name, target.Repo, item)
		if err != nil {
			return finish(doc, err)
		}
		doc.PRs = append(doc.PRs, classified)
	}
	if err := matchTabs(ctx, deps.Herdr, cfg, &doc); err != nil {
		return finish(doc, err)
	}
	return finish(doc, nil)
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

func classifyRepos(ctx context.Context, client GH, cfg config.Config, repos []string, doc *Document) error {
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
		for _, item := range prs {
			if err := ctx.Err(); err != nil {
				return fmt.Errorf("scan: %w", err)
			}
			classified, err := classifyPR(ctx, client, cfg, owner, name, repo, item)
			if err != nil {
				return err
			}
			doc.PRs = append(doc.PRs, classified)
		}
	}
	return nil
}

func classifyPR(ctx context.Context, client GH, cfg config.Config, owner, name, repo string, item gh.PRListItem) (PR, error) {
	threads, err := client.ReviewThreads(ctx, owner, name, item.Number)
	if err != nil {
		return PR{}, fmt.Errorf("%s#%d: %w", repo, item.Number, err)
	}
	comments := blockingComments(threads, cfg.Author)
	id := ExtractID(item.Title, cfg.TitleIDPattern)
	ci := CIState(item.StatusCheckRollup)
	reqs := formatReviewRequests(item.ReviewRequests)
	out := PR{
		Repo:             repo,
		Number:           item.Number,
		Title:            item.Title,
		URL:              item.URL,
		Identifier:       id,
		Base:             item.BaseRefName,
		Head:             item.HeadRefName,
		HeadSHA:          item.HeadRefOid,
		MergeStateStatus: item.MergeStateStatus,
		Mergeable:        item.Mergeable,
		IsDraft:          item.IsDraft,
		ReviewDecision:   item.ReviewDecision,
		ReviewRequests:   reqs,
		CIState:          ci,
		Unaddressed:      len(comments) > 0,
		BlockingComments: comments,
	}
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

func matchTabs(ctx context.Context, client Herdr, cfg config.Config, doc *Document) error {
	if err := client.RequireMin(ctx, herdrMinVersion); err != nil {
		if errors.Is(err, herdr.ErrNotInstalled) {
			nullTabs(doc)
			warn(doc, "herdr unreachable: "+err.Error())
			return nil
		}
		return fmt.Errorf("herdr version: %w", err)
	}
	tabs, err := client.TabList(ctx)
	if err != nil {
		nullTabs(doc)
		warn(doc, "herdr unreachable: "+err.Error())
		return nil
	}
	agents, err := client.AgentList(ctx)
	if err != nil {
		nullTabs(doc)
		warn(doc, "herdr unreachable: "+err.Error())
		return nil
	}
	for i := range doc.PRs {
		tab, warning := Match(doc.PRs[i].Identifier, doc.PRs[i].Repo, doc.PRs[i].Number, cfg.TabLabelTemplate, tabs, agents)
		doc.PRs[i].Tab = tab
		if warning != "" {
			warn(doc, warning)
		}
	}
	return nil
}

func nullTabs(doc *Document) {
	for i := range doc.PRs {
		doc.PRs[i].Tab = nil
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
