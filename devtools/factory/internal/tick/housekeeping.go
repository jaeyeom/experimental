package tick

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/prlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func (e *Engine) merged(ctx context.Context, now time.Time) (map[string]struct{}, error) {
	return e.settle(ctx, now, nil)
}

func closedKey(sourceName, id string) string {
	return sourceName + "\n" + id
}

func (e *Engine) mergeSince(ctx context.Context, now time.Time) (time.Time, error) {
	last, err := e.Store.LastRunAt(ctx, runCommand)
	if err != nil {
		return time.Time{}, err //nolint:wrapcheck // propagate context cancellation
	}
	if last.IsZero() {
		last = now.Add(-defaultTickLookback)
	}
	return last.Add(-mergeOverlap), nil
}

func (e *Engine) handOff(ctx context.Context, open []gh.PR) error {
	claims, err := e.Store.ActiveClaims(ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	for _, claim := range claims {
		if claim.State != store.Started {
			continue
		}
		err := e.handOne(ctx, claim, open)
		if errors.Is(err, config.ErrSourceNotFound) {
			slog.Debug("skipping active claim for unconfigured source", "source", claim.Source, "id", claim.ID)
			continue
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func (e *Engine) handOne(ctx context.Context, claim store.Claim, open []gh.PR) error {
	if claim.OutputMode == config.OutputModeMerge {
		return nil
	}
	matched, _, _, err := e.matched(ctx, claim, open)
	if err != nil || !matched {
		return err
	}
	return e.setClaim(ctx, claim, store.HandedOff)
}

func (e *Engine) matched(ctx context.Context, claim store.Claim, prs []gh.PR) (bool, config.Source, source.Item, error) {
	found, cfg, it, err := e.claimPRs(ctx, claim, prs)
	if err != nil {
		return false, cfg, it, err
	}
	if len(found) > 0 {
		now, cerr := e.clock()
		if cerr != nil {
			return false, cfg, it, cerr
		}
		if err := e.remember(ctx, claim, found, now); err != nil {
			return false, cfg, it, err
		}
	}
	return len(found) > 0, cfg, it, nil
}

// claimPRs returns the pull requests in prs that refer to the claim's item.
func (e *Engine) claimPRs(ctx context.Context, claim store.Claim, prs []gh.PR) ([]gh.PR, config.Source, source.Item, error) {
	cfg, err := e.configFor(claim.Source)
	if err != nil {
		return nil, config.Source{}, source.Item{}, err
	}
	it, err := e.Store.Item(ctx, claim.Source, claim.ID)
	if err != nil {
		return nil, config.Source{}, source.Item{}, err //nolint:wrapcheck // propagate context cancellation
	}
	byItem, err := prsForItem(prs, it, cfg.PRTitlePattern)
	if err != nil {
		return nil, config.Source{}, source.Item{}, err
	}
	byMatch, err := gh.Matches(prs, claim.ID, claim.PRs, cfg.PRTitlePattern)
	if err != nil {
		return nil, config.Source{}, source.Item{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	return unionPulls(byItem, byMatch), cfg, it, nil
}

// observe records the pull requests in prs that refer to an active claim's
// item. It reads no new pull request state.
func (e *Engine) observe(ctx context.Context, now time.Time, prs []gh.PR) error {
	if len(prs) == 0 {
		return nil
	}
	claims, err := e.Store.ActiveClaims(ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	for _, claim := range claims {
		if claim.OutputMode == config.OutputModeMerge {
			continue
		}
		found, _, it, err := e.claimPRs(ctx, claim, prs)
		if errors.Is(err, config.ErrSourceNotFound) {
			slog.Debug("skipping active claim for unconfigured source", "source", claim.Source, "id", claim.ID)
			continue
		}
		if err != nil {
			return err
		}
		if err := e.remember(ctx, claim, found, now); err != nil {
			return err
		}
		if err := prlog.Record(ctx, e.Store, now, it, found); err != nil {
			return fmt.Errorf("tick: %w", err)
		}
	}
	return nil
}

func (e *Engine) configFor(key string) (config.Source, error) {
	for i := range e.Config.Sources {
		cfg := e.Config.Sources[i]
		got, err := sourceKey(cfg.Kind, cfg.Name)
		if err != nil {
			return config.Source{}, err
		}
		if got == key {
			return cfg, nil
		}
	}
	return config.Source{}, fmt.Errorf("source %s: %w", key, config.ErrSourceNotFound)
}

func (e *Engine) backend(key string) (source.Source, error) {
	src, ok := e.Sources[key]
	if !ok || src == nil {
		return nil, fmt.Errorf("source %s: expected a source, found none", key)
	}
	return src, nil
}

func (e *Engine) setClaim(ctx context.Context, claim store.Claim, state store.ClaimState) error {
	err := e.Store.UpdateClaim(ctx, claim.Source, claim.ID, claim.ClaimedAt, state, claim.Detail, claim.TabID, claim.WorkspaceID)
	if err != nil {
		return fmt.Errorf("tick: update claim %s/%s: %w", claim.Source, claim.ID, err)
	}
	return nil
}

func (e *Engine) clock() (time.Time, error) {
	if e.Now == nil {
		return time.Time{}, fmt.Errorf("tick: expected a clock, found nil")
	}
	now := e.Now()
	if now.IsZero() {
		return time.Time{}, fmt.Errorf("tick: expected a clock time, found zero")
	}
	return now, nil
}

func (e *Engine) remember(ctx context.Context, claim store.Claim, seen []gh.PR, now time.Time) error {
	if len(seen) == 0 {
		return nil
	}
	if now.IsZero() {
		return fmt.Errorf("tick: claim %s: expected a clock time, found zero", claim.ID)
	}
	extra := make([]store.ClaimPR, 0, len(seen))
	for _, pr := range seen {
		extra = append(extra, store.ClaimPR{
			Repo: pr.Repo, Number: pr.Number, URL: pr.URL, Title: pr.Title, Head: pr.Head,
		})
	}
	merged := store.MergeClaimPRs(claim.PRs, extra)
	if store.SameClaimPRs(claim.PRs, merged) {
		return nil
	}
	if err := e.Store.FillClaim(ctx, claim.Source, claim.ID, claim.ClaimedAt, claim.TabID, claim.WorkspaceID, merged, now); err != nil {
		return fmt.Errorf("tick: record pull request %s: %w", claim.ID, err)
	}
	return nil
}

func unionPulls(groups ...[]gh.PR) []gh.PR {
	var out []gh.PR
	seen := map[string]struct{}{}
	for _, group := range groups {
		for _, pr := range group {
			key := pr.URL
			if pr.Repo != "" && pr.Number > 0 {
				key = pr.Repo + "#" + strconv.Itoa(pr.Number)
			}
			if key == "" {
				out = append(out, pr)
				continue
			}
			if _, ok := seen[key]; ok {
				continue
			}
			seen[key] = struct{}{}
			out = append(out, pr)
		}
	}
	return out
}

func prsForItem(prs []gh.PR, it source.Item, pattern string) ([]gh.PR, error) {
	if !strings.Contains(it.ID, "#") && pattern == "" {
		return nil, fmt.Errorf("item %s: expected a pr_title_pattern, found empty", it.ID)
	}
	return gh.ForItem(prs, it, pattern), nil
}

func joinParts(parts []string) string {
	return strings.Join(parts, "; ")
}
