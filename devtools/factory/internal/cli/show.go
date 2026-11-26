package cli

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/remote"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/verify"
)

func newShowCmd(ctx context.Context) *cobra.Command {
	var noSync bool
	cmd := &cobra.Command{
		Use:           "show <id>",
		Short:         "Show one work item, its blockers, claim, tab, and pull requests",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 1 {
				return usageError{err: fmt.Errorf("show: expected 1 argument, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runShow(ctx, cmd, args[0], noSync)
		},
	}
	cmd.Flags().BoolVar(&noSync, "no-sync", false, "do not refresh a stale mirror")
	return cmd
}

func runShow(ctx context.Context, cmd *cobra.Command, id string, noSync bool) (err error) {
	if ctx == nil {
		return errors.New("show: expected a context, found nil")
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			err = errors.Join(err, fmt.Errorf("close store: %w", cerr))
		}
	}()
	if err := refreshBeforeRead(ctx, cmd, cfg, st, "", noSync); err != nil {
		return err
	}
	it, err := resolveItem(ctx, st, id, "")
	if err != nil {
		return err
	}
	srcCfg, err := configFor(cfg, it.Source)
	if err != nil {
		return err
	}
	if err := checkKind(it.Source, srcCfg.Kind); err != nil {
		return err
	}
	snap, err := loadSnapshot(ctx, st, srcCfg)
	if err != nil {
		return err
	}
	blocks, err := st.Blocks(ctx, it.Source, it.ID)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	claim, foundClaim, err := latestClaim(ctx, st, it.Source, it.ID)
	if err != nil {
		return err
	}
	tab, err := claimTab(ctx, cfg, snap, it, srcCfg, claim)
	if err != nil {
		return err
	}
	mode, prs, commit, err := loadShowOutput(ctx, cfg, snap, srcCfg, it, claim, foundClaim)
	if err != nil {
		return err
	}
	text := formatShow(it, snap, srcCfg, blocks, claim, foundClaim, tab, mode, prs, commit)
	if _, err := io.WriteString(cmd.OutOrStdout(), text); err != nil {
		return fmt.Errorf("write show: %w", err)
	}
	return nil
}

func loadShowOutput(ctx context.Context, cfg *config.Config, snap ready.Snapshot, srcCfg config.Source, it source.Item, claim store.Claim, foundClaim bool) (string, []gh.PR, *verify.LandedCommit, error) {
	effCfg := srcCfg
	var lane config.Lane
	if foundClaim && claim.Lane != "" {
		if l, ok := cfg.LaneNamed(claim.Lane); ok {
			lane = l
			effCfg = config.LaneSource(srcCfg, lane)
		}
	}
	mode := verify.EffectiveOutputMode(foundClaim, claim, effCfg)
	if mode == config.OutputModeMerge {
		commit, err := showLandedCommit(ctx, lane, effCfg, it, claim, foundClaim)
		return mode, nil, commit, err
	}
	prs, err := showPRs(ctx, srcCfg.GHBin, snap.OpenPRs, it, srcCfg.PRTitlePattern, claim.PRs)
	return mode, prs, nil, err
}

func latestClaim(ctx context.Context, st *store.Store, sourceName, id string) (store.Claim, bool, error) {
	claim, err := st.LatestClaim(ctx, sourceName, id)
	if errors.Is(err, source.ErrNotFound) {
		return store.Claim{}, false, nil
	}
	if err != nil {
		return store.Claim{}, false, err //nolint:wrapcheck // propagate context cancellation
	}
	return claim, true, nil
}

type tabView struct {
	workspace string
	label     string
	tab       herdr.Tab
	found     bool
}

// claimTab finds the item's tab on the machine of the claim's lane.
// A claim with no lane, or no claim, reads the local server.
func claimTab(ctx context.Context, cfg *config.Config, snap ready.Snapshot, it source.Item, srcCfg config.Source, claim store.Claim) (tabView, error) {
	if claim.Spawner == config.SpawnerSubagent {
		return tabView{}, nil
	}
	if claim.Lane != "" {
		var err error
		snap, err = claimSnapshot(ctx, cfg, snap, claim.Lane)
		if err != nil {
			return tabView{}, err
		}
	}
	return matchTab(snap, it, srcCfg)
}

func matchTab(snap ready.Snapshot, it source.Item, cfg config.Source) (tabView, error) {
	wsLabel, tabLabel := ready.TabRef(it, cfg, cfg.Kind)
	view := tabView{workspace: wsLabel, label: tabLabel}
	switch cfg.Kind {
	case "jira":
		tab, ok, err := herdr.FindTab(snap.Tabs, "", tabLabel)
		if err != nil {
			return tabView{}, err //nolint:wrapcheck // internal factory error already carries context
		}
		view.tab = tab
		view.found = ok
		return view, nil
	case "github":
		ws, ok, err := herdr.FindWorkspace(snap.Workspaces, wsLabel)
		if err != nil || !ok {
			return view, err //nolint:wrapcheck // internal factory error already carries context
		}
		tab, ok, err := herdr.FindTab(snap.Tabs, ws.ID, tabLabel)
		if err != nil {
			return tabView{}, err //nolint:wrapcheck // internal factory error already carries context
		}
		view.tab = tab
		view.found = ok
		return view, nil
	default:
		return tabView{}, fmt.Errorf("tab: expected kind jira or github, found %q", cfg.Kind)
	}
}

func formatShow(it source.Item, snap ready.Snapshot, srcCfg config.Source, blocks []string, claim store.Claim, foundClaim bool, tab tabView, mode string, prs []gh.PR, commit *verify.LandedCommit) string {
	var b strings.Builder
	b.WriteString(formatItem(it))
	if srcCfg.Trust.Environment != "" {
		b.WriteByte('\n')
		b.WriteString(formatTrust(it, snap, srcCfg))
	}
	b.WriteByte('\n')
	b.WriteString(formatBlockedBy(snap, it))
	b.WriteByte('\n')
	b.WriteString(formatBlocks(blocks))
	b.WriteByte('\n')
	b.WriteString(formatClaim(claim, foundClaim))
	b.WriteByte('\n')
	if !foundClaim || claim.Spawner != config.SpawnerSubagent {
		b.WriteString(formatTab(tab))
		b.WriteByte('\n')
	}
	if mode == config.OutputModeMerge {
		b.WriteString(formatLandedCommit(commit))
	} else {
		b.WriteString(formatPRs(prs))
	}
	return b.String()
}

func formatItem(it source.Item) string {
	var extra strings.Builder
	if it.Author != "" {
		bprintf(&extra, "author: %s\n", it.Author)
	}
	if it.AuthorAssociation != "" {
		bprintf(&extra, "author_association: %s\n", it.AuthorAssociation)
	}
	if it.Locked {
		extra.WriteString("locked: true\n")
	}
	return fmt.Sprintf("source: %s\nid: %s\ntitle: %s\nurl: %s\ntype: %s\nstate: %s\ndone: %t\nparent: %s\nlabels: %s\nassignee: %s\npriority: %d\ncreated: %s\nupdated: %s\n%s",
		it.Source, it.ID, it.Title, it.URL, it.Type, it.State, it.Done, it.Parent,
		strings.Join(it.Labels, ", "), it.Assignee, it.Priority, formatStamp(it.Created), formatStamp(it.Updated), extra.String())
}

func formatTrust(it source.Item, snap ready.Snapshot, cfg config.Source) string {
	var b strings.Builder
	b.WriteString("Trust:\n")
	bprintf(&b, "  environment: %s\n", cfg.Trust.Environment)
	author := it.Author
	if author == "" {
		author = "(unknown)"
	}
	if it.AuthorAssociation != "" {
		bprintf(&b, "  author: %s (%s)\n", author, it.AuthorAssociation)
	} else {
		bprintf(&b, "  author: %s\n", author)
	}
	bprintf(&b, "  locked: %t\n", it.Locked)
	check := ready.CheckTrust(it, snap, cfg)
	if check.Pass {
		b.WriteString("  status: PASS\n")
	} else {
		bprintf(&b, "  status: FAIL (%s)\n", check.Detail)
	}
	return b.String()
}

func formatBlockedBy(snap ready.Snapshot, it source.Item) string {
	var b strings.Builder
	b.WriteString("Blocked by:\n")
	if len(it.BlockedBy) == 0 {
		b.WriteString("  none\n")
		return b.String()
	}
	for _, ref := range it.BlockedBy {
		blocker, ok := snap.Items[ref.ID]
		if !ok {
			bprintf(&b, "  %s  %s\n", ref.ID, outsideStatus(snap, ref.ID))
			continue
		}
		done := "not done"
		if blocker.Done {
			done = "done"
		}
		bprintf(&b, "  %s  %s  %s\n", ref.ID, blocker.State, done)
	}
	return b.String()
}

// outsideStatus describes a blocker that is not in the mirror by the record
// the last sync stored for it.
func outsideStatus(snap ready.Snapshot, id string) string {
	rec, ok := snap.External[id]
	if !ok {
		return "not in mirror"
	}
	switch rec.Status {
	case store.ExternalFound:
		done := "not done"
		if rec.Done {
			done = "done"
		}
		assignee := "unassigned"
		if rec.Assignee != "" {
			assignee = "assigned to " + rec.Assignee
		}
		return fmt.Sprintf("%s  %s  (outside mirror, %s)", rec.State, done, assignee)
	case store.ExternalNotFound:
		return "not in mirror; not found or no access"
	default:
		return "not in mirror; not checked: " + rec.Reason
	}
}

func formatBlocks(ids []string) string {
	var b strings.Builder
	b.WriteString("Blocks:\n")
	if len(ids) == 0 {
		b.WriteString("  none\n")
		return b.String()
	}
	for _, id := range ids {
		b.WriteString("  " + id + "\n")
	}
	return b.String()
}

func formatClaim(claim store.Claim, found bool) string {
	if !found {
		return "Claim:\n  none\n"
	}
	text := fmt.Sprintf("Claim:\n  state: %s\n  claimed_at: %s\n  updated_at: %s\n  machine: %s\n  tab_id: %s\n  workspace_id: %s\n  detail: %s\n",
		claim.State, formatStamp(claim.ClaimedAt), formatStamp(claim.UpdatedAt),
		claim.Machine, claim.TabID, claim.WorkspaceID, claim.Detail)
	if claim.Lane != "" {
		text += fmt.Sprintf("  lane: %s\n", claim.Lane)
	}
	if claim.Spawner != "" {
		text += fmt.Sprintf("  spawner: %s\n", claim.Spawner)
	}
	if claim.Worktree != "" {
		text += fmt.Sprintf("  worktree: %s\n", claim.Worktree)
	}
	if claim.OutputMode != "" {
		text += fmt.Sprintf("  output: %s\n", claim.OutputMode)
	}
	return text
}

func formatTab(tab tabView) string {
	if !tab.found {
		return fmt.Sprintf("Tab:\n  workspace: %s\n  label: %s\n  none\n", tab.workspace, tab.label)
	}
	return fmt.Sprintf("Tab:\n  workspace: %s\n  label: %s\n  id: %s\n  workspace_id: %s\n  agent_status: %s\n",
		tab.workspace, tab.label, tab.tab.ID, tab.tab.WorkspaceID, tab.tab.AgentStatus)
}

func showPRs(ctx context.Context, ghBin string, open []gh.PR, it source.Item, pattern string, stored []store.ClaimPR) ([]gh.PR, error) {
	prs := prsForShow(open, it, pattern, stored)
	if len(prs) > 0 {
		return prs, nil
	}
	return prsByTitle(ctx, ghBin, it, pattern)
}

func prsByTitle(ctx context.Context, ghBin string, it source.Item, pattern string) ([]gh.PR, error) {
	_, client, err := externalClients(ghBin)
	if err != nil {
		return nil, err
	}
	found, err := client.MergedByTitle(ctx, it.ID)
	if err != nil {
		return nil, fmt.Errorf("show %s: %w", it.ID, err)
	}
	return gh.ForItem(found, it, pattern), nil
}

func prsForShow(open []gh.PR, it source.Item, pattern string, stored []store.ClaimPR) []gh.PR {
	out := append([]gh.PR{}, gh.ForItem(open, it, pattern)...)
	seen := map[string]struct{}{}
	for _, pr := range out {
		seen[prKey(pr.Repo, pr.Number)] = struct{}{}
	}
	for _, pr := range gh.HeadContains(open, it.ID) {
		if _, ok := seen[prKey(pr.Repo, pr.Number)]; ok {
			continue
		}
		seen[prKey(pr.Repo, pr.Number)] = struct{}{}
		out = append(out, pr)
	}
	for _, pr := range stored {
		key := prKey(pr.Repo, pr.Number)
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		out = append(out, storedPR(open, pr))
	}
	return out
}

func storedPR(open []gh.PR, pr store.ClaimPR) gh.PR {
	shown := gh.PR{Repo: pr.Repo, Number: pr.Number, URL: pr.URL, Title: pr.Title}
	for _, live := range open {
		if live.Repo == pr.Repo && live.Number == pr.Number {
			if live.URL != "" {
				shown.URL = live.URL
			}
			shown.Title = live.Title
			return shown
		}
	}
	return shown
}

func prKey(repo string, number int) string {
	return fmt.Sprintf("%s#%d", repo, number)
}

func showLandedCommit(ctx context.Context, lane config.Lane, cfg config.Source, it source.Item, claim store.Claim, foundClaim bool) (*verify.LandedCommit, error) {
	exec := newCommandExec()
	if exec == nil {
		return nil, errors.New("show: expected an executor, found nil")
	}
	if lane.Remote() {
		rexec, err := remote.New(exec, lane.Exec)
		if err != nil {
			return nil, fmt.Errorf("show %s: lane %s: %w", it.ID, lane.Name, err)
		}
		exec = rexec
	}
	dir := cfg.Cwd
	if foundClaim && claim.State.Active() && claim.Worktree != "" {
		dir = claim.Worktree
	}
	c, reason, err := verify.FindLinearCommit(exec)(ctx, dir, cfg.Base, it.ID, cfg.PRTitlePattern)
	if err != nil {
		return nil, fmt.Errorf("show %s: %w", it.ID, err)
	}
	if reason != "" {
		return nil, nil
	}
	return &c, nil
}

func formatLandedCommit(c *verify.LandedCommit) string {
	var b strings.Builder
	b.WriteString("Commits:\n")
	if c == nil {
		b.WriteString("  none\n")
		return b.String()
	}
	sha := c.SHA
	if len(sha) > 7 {
		sha = sha[:7]
	}
	bprintf(&b, "  %s  %s\n", sha, c.Subject)
	return b.String()
}

func formatPRs(prs []gh.PR) string {
	var b strings.Builder
	b.WriteString("PRs:\n")
	if len(prs) == 0 {
		b.WriteString("  none\n")
		return b.String()
	}
	for _, pr := range prs {
		bprintf(&b, "  %s  %s\n", pr.URL, pr.Title)
	}
	return b.String()
}
