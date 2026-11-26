// Package adopt records a claim for an existing tab or pull request.
// It does not create a workspace or a tab, and it does not start an agent.
package adopt

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Input is everything adopt reads and writes except the request flags.
type Input struct {
	Src      source.Source    // Backend Get and Claim call.
	Cfg      config.Source    // Source configuration, including claim_status and claim_label.
	Kind     string           // "jira" or "github".
	Store    *store.Store     // Local claim rows.
	Snap     ready.Snapshot   // Items, tabs, workspaces, and open pull requests.
	Panes    []herdr.Pane     // Pane list. Used only when Request.Pane is set.
	Hostname string           // Stored on a new claim as the machine.
	Now      func() time.Time // Clock for a new claim and for filling an open one.
	// Lane is the [[lane]] name recorded on a new claim. Empty means no lanes.
	// An open claim keeps the lane it has.
	Lane string
}

// Request selects the item and how to find its tab and pull requests.
type Request struct {
	ID   string   // Item id.
	Pane string   // Pane id. The recorded tab is the tab that contains it.
	Tab  string   // Tab id or tab label. Ignored when Pane is set.
	PRs  []string // owner/repo#N or a GitHub pull request URL. Repeatable.
}

type resolvedTab struct {
	tab   herdr.Tab
	found bool
}

// Adopt records a claim for req.ID.
// A tab or a pull request must resolve. Neither leaves the item and the claim store unchanged.
// An open claim is not transitioned again and does not insert another row.
// It can fill in a tab or pull request the first call did not record.
func Adopt(ctx context.Context, in Input, req Request) error {
	if err := validate(ctx, in, req); err != nil {
		return err
	}
	it, err := snapshotItem(in, req.ID)
	if err != nil {
		return err
	}
	tab, err := resolveTab(in, req, it)
	if err != nil {
		return err
	}
	prs, err := collectPRs(in.Snap.OpenPRs, it, in.Cfg.PRTitlePattern, req.PRs)
	if err != nil {
		return err
	}
	if !tab.found && len(prs) == 0 {
		return fmt.Errorf("adopt %s: expected a tab or a pull request, found neither", req.ID)
	}
	claim, open, err := openClaim(ctx, in, req.ID)
	if err != nil {
		return err
	}
	if open {
		return fillOpen(ctx, in, claim, tab, prs)
	}
	return claimNew(ctx, in, req.ID, tab, prs)
}

func validate(ctx context.Context, in Input, req Request) error {
	if ctx == nil {
		return errors.New("adopt: expected a context, found nil")
	}
	if req.ID == "" {
		return errors.New("adopt: expected a non-empty id, found empty")
	}
	if in.Src == nil {
		return errors.New("adopt: expected a source, found nil")
	}
	if in.Store == nil {
		return errors.New("adopt: expected a store, found nil")
	}
	if in.Now == nil {
		return errors.New("adopt: expected a clock, found nil")
	}
	if in.Hostname == "" {
		return errors.New("adopt: expected a hostname, found empty")
	}
	if err := validateKind(in); err != nil {
		return err
	}
	if in.Src.Name() == "" {
		return errors.New("adopt: expected a non-empty source name, found empty")
	}
	return nil
}

func validateKind(in Input) error {
	switch in.Kind {
	case "jira":
		if in.Cfg.ClaimStatus == "" {
			return errors.New("adopt: expected claim_status, found empty")
		}
		return nil
	case "github":
		if in.Cfg.ClaimStatus == "" && in.Cfg.ClaimLabel == "" {
			return errors.New("adopt: expected claim_status or claim_label, found empty")
		}
		return nil
	default:
		return fmt.Errorf("adopt: expected kind jira or github, found %q", in.Kind)
	}
}

func snapshotItem(in Input, id string) (source.Item, error) {
	it, ok := in.Snap.Items[id]
	if !ok {
		return source.Item{}, fmt.Errorf("adopt %s: expected the item in the snapshot, found none", id)
	}
	if it.ID != id {
		return source.Item{}, fmt.Errorf("adopt %s: expected snapshot id %q, found %q", id, id, it.ID)
	}
	name := in.Src.Name()
	if it.Source != name {
		return source.Item{}, fmt.Errorf("adopt %s: expected source %q, found %q", id, name, it.Source)
	}
	return it, nil
}

func resolveTab(in Input, req Request, it source.Item) (resolvedTab, error) {
	if req.Pane != "" {
		return paneTab(in.Panes, in.Snap.Tabs, req.Pane)
	}
	if req.Tab != "" {
		return flagTab(in, it, req.Tab)
	}
	return defaultTab(in, it)
}

func paneTab(panes []herdr.Pane, tabs []herdr.Tab, paneID string) (resolvedTab, error) {
	var found []herdr.Pane
	for _, pane := range panes {
		if pane.ID == paneID {
			found = append(found, pane)
		}
	}
	if len(found) != 1 {
		return resolvedTab{}, fmt.Errorf("adopt: expected one pane %q, found %d", paneID, len(found))
	}
	if found[0].TabID == "" {
		return resolvedTab{}, fmt.Errorf("adopt: expected pane %q to name a tab, found empty", paneID)
	}
	matches := tabsWithID(tabs, found[0].TabID)
	if len(matches) != 1 {
		return resolvedTab{}, fmt.Errorf("adopt: expected one tab %q for pane %q, found %d", found[0].TabID, paneID, len(matches))
	}
	return resolvedTab{tab: matches[0], found: true}, nil
}

func flagTab(in Input, it source.Item, ref string) (resolvedTab, error) {
	matches := tabsWithID(in.Snap.Tabs, ref)
	if len(matches) > 1 {
		return resolvedTab{}, fmt.Errorf("adopt: expected one tab %q, found %d", ref, len(matches))
	}
	if len(matches) == 1 {
		return resolvedTab{tab: matches[0], found: true}, nil
	}
	tab, ok, err := labeledTab(in, it, ref)
	if err != nil {
		return resolvedTab{}, err
	}
	if !ok {
		return resolvedTab{}, fmt.Errorf("adopt: expected tab %q, found none", ref)
	}
	return resolvedTab{tab: tab, found: true}, nil
}

func defaultTab(in Input, it source.Item) (resolvedTab, error) {
	_, label := ready.TabRef(it, in.Cfg, in.Kind)
	tab, ok, err := labeledTab(in, it, label)
	if err != nil {
		return resolvedTab{}, err
	}
	return resolvedTab{tab: tab, found: ok}, nil
}

func labeledTab(in Input, it source.Item, label string) (herdr.Tab, bool, error) {
	switch in.Kind {
	case "jira":
		return herdr.FindTab(in.Snap.Tabs, "", label) //nolint:wrapcheck // internal factory error already carries context
	case "github":
		wsLabel, _ := ready.TabRef(it, in.Cfg, in.Kind)
		ws, ok, err := herdr.FindWorkspace(in.Snap.Workspaces, wsLabel)
		if err != nil || !ok {
			return herdr.Tab{}, false, err //nolint:wrapcheck // internal factory error already carries context
		}
		return herdr.FindTab(in.Snap.Tabs, ws.ID, label) //nolint:wrapcheck // internal factory error already carries context
	default:
		return herdr.Tab{}, false, fmt.Errorf("adopt: expected kind jira or github, found %q", in.Kind)
	}
}

func tabsWithID(tabs []herdr.Tab, id string) []herdr.Tab {
	var out []herdr.Tab
	for _, tab := range tabs {
		if tab.ID == id {
			out = append(out, tab)
		}
	}
	return out
}

func collectPRs(open []gh.PR, it source.Item, pattern string, refs []string) ([]store.ClaimPR, error) {
	out, err := appendPRs(nil, gh.ForItem(open, it, pattern))
	if err != nil {
		return nil, err
	}
	out, err = appendPRs(out, gh.HeadContains(open, it.ID))
	if err != nil {
		return nil, err
	}
	for _, ref := range refs {
		parsed, err := parsePRRef(ref)
		if err != nil {
			return nil, err
		}
		out, err = addPR(out, enrichPR(parsed, open))
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

func appendPRs(out []store.ClaimPR, prs []gh.PR) ([]store.ClaimPR, error) {
	for _, pr := range prs {
		var err error
		out, err = addPR(out, store.ClaimPR{
			Repo:   pr.Repo,
			Number: pr.Number,
			URL:    pr.URL,
			Title:  pr.Title,
			Head:   pr.Head,
		})
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

func addPR(out []store.ClaimPR, pr store.ClaimPR) ([]store.ClaimPR, error) {
	id := source.GitHubID(pr.Repo, pr.Number)
	if _, _, err := source.ParseGitHubID(id); err != nil {
		return nil, fmt.Errorf("adopt: pull request: %w", err)
	}
	if pr.URL == "" {
		return nil, fmt.Errorf("adopt: pull request %s: expected a URL, found empty", id)
	}
	for _, have := range out {
		if have.Repo == pr.Repo && have.Number == pr.Number {
			return out, nil
		}
	}
	return append(out, pr), nil
}

func enrichPR(pr store.ClaimPR, open []gh.PR) store.ClaimPR {
	for _, got := range open {
		if got.Repo == pr.Repo && got.Number == pr.Number {
			if got.URL != "" {
				pr.URL = got.URL
			}
			pr.Title = got.Title
			if got.Head != "" {
				pr.Head = got.Head
			}
			return pr
		}
	}
	return pr
}

func parsePRRef(ref string) (store.ClaimPR, error) {
	pr, err := gh.ParseRef(ref)
	if err != nil {
		return store.ClaimPR{}, fmt.Errorf("adopt: %w", err)
	}
	return pr, nil
}

func openClaim(ctx context.Context, in Input, id string) (store.Claim, bool, error) {
	claim, err := in.Store.LatestClaim(ctx, in.Src.Name(), id)
	if errors.Is(err, source.ErrNotFound) {
		return store.Claim{}, false, nil
	}
	if err != nil {
		return store.Claim{}, false, err //nolint:wrapcheck // propagate context cancellation
	}
	switch claim.State {
	case store.Claimed, store.Started, store.HandedOff:
		return claim, true, nil
	case store.Done, store.Released, store.Failed:
		return store.Claim{}, false, nil
	default:
		return store.Claim{}, false, fmt.Errorf("adopt %s: expected a claim state, found %q", id, claim.State)
	}
}

func fillOpen(ctx context.Context, in Input, claim store.Claim, tab resolvedTab, prs []store.ClaimPR) error {
	tabID, wsID := claim.TabID, claim.WorkspaceID
	if tabID == "" && tab.found {
		if err := requireTabIDs(claim.ID, tab.tab); err != nil {
			return err
		}
		tabID = tab.tab.ID
		wsID = tab.tab.WorkspaceID
	}
	merged := store.MergeClaimPRs(claim.PRs, prs)
	if tabID == claim.TabID && wsID == claim.WorkspaceID && store.SameClaimPRs(claim.PRs, merged) {
		return nil
	}
	now, err := clock(in)
	if err != nil {
		return err
	}
	return in.Store.FillClaim(ctx, claim.Source, claim.ID, claim.ClaimedAt, tabID, wsID, merged, now) //nolint:wrapcheck // propagate context cancellation
}

func claimNew(ctx context.Context, in Input, id string, tab resolvedTab, prs []store.ClaimPR) error {
	posted, err := claimSource(ctx, in, id)
	if err != nil {
		return err
	}
	now, err := clock(in)
	if err != nil {
		return err
	}
	row := store.Claim{
		Source:    in.Src.Name(),
		ID:        id,
		ClaimedAt: now,
		Machine:   in.Hostname,
		State:     store.Claimed,
		UpdatedAt: now,
		PRs:       prs,
		Lane:      in.Lane,
	}
	if tab.found {
		if err := requireTabIDs(id, tab.tab); err != nil {
			return err
		}
		row.TabID = tab.tab.ID
		row.WorkspaceID = tab.tab.WorkspaceID
	}
	if err := in.Store.InsertClaim(ctx, row); err != nil {
		if posted {
			return fmt.Errorf("adopt %s: insert claim: %w; item is claimed; fix and re-run, or factory release %s", id, err, id)
		}
		return fmt.Errorf("adopt %s: insert claim: %w", id, err)
	}
	return nil
}

func requireTabIDs(id string, tab herdr.Tab) error {
	if tab.ID == "" || tab.WorkspaceID == "" {
		return fmt.Errorf("adopt %s: expected tab id and workspace id, found tab %q workspace %q", id, tab.ID, tab.WorkspaceID)
	}
	return nil
}

func clock(in Input) (time.Time, error) {
	now := in.Now().UTC()
	if now.IsZero() {
		return time.Time{}, errors.New("adopt: expected a timestamp, found zero")
	}
	return now, nil
}

// claimSource posts the claim transition start would post.
// Jira always calls Claim. That call posts no transition when the issue is already in claim_status.
// GitHub adds claim_label only when it is missing. A present label skips Claim.
// The bool reports whether Claim ran, so a later insert failure can say the item is claimed.
func claimSource(ctx context.Context, in Input, id string) (bool, error) {
	live, err := in.Src.Get(ctx, id)
	if err != nil {
		return false, err //nolint:wrapcheck // propagate context cancellation
	}
	if live.ID != id {
		return false, fmt.Errorf("adopt %s: expected id %q, found %q", id, id, live.ID)
	}
	switch in.Kind {
	case "jira":
		if err := in.Src.Claim(ctx, id); err != nil {
			return false, err //nolint:wrapcheck // propagate context cancellation
		}
		return true, nil
	case "github":
		statusDone := in.Cfg.ClaimStatus == "" || live.State == in.Cfg.ClaimStatus
		labelDone := in.Cfg.ClaimLabel == "" || slices.Contains(live.Labels, in.Cfg.ClaimLabel)
		if statusDone && labelDone {
			return false, nil
		}
		if err := in.Src.Claim(ctx, id); err != nil {
			return false, err //nolint:wrapcheck // propagate context cancellation
		}
		return true, nil
	default:
		return false, fmt.Errorf("adopt: expected kind jira or github, found %q", in.Kind)
	}
}
