package cli

import (
	"context"
	"errors"
	"fmt"
	"io"
	"slices"
	"strings"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

type approveOpts struct {
	apply      bool
	revoke     bool
	sourceOnly string
}

func newApproveCmd(ctx context.Context) *cobra.Command {
	var opts approveOpts
	cmd := &cobra.Command{
		Use:           "approve <id>",
		Short:         "Inspect, approve, or revoke the content-hash approval for an item",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			return oneID("approve", args)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runApprove(ctx, cmd, args[0], opts)
		},
	}
	cmd.Flags().BoolVar(&opts.apply, "apply", false, "record the approval hash and transition status (implied when dry_run is false)")
	cmd.Flags().BoolVar(&opts.revoke, "revoke", false, "clear the stored local approval for this item")
	cmd.Flags().StringVar(&opts.sourceOnly, "source", "", "limit to this source key (kind:name, for example github:repo)")
	return cmd
}

func runApprove(ctx context.Context, cmd *cobra.Command, rawID string, opts approveOpts) error {
	if ctx == nil {
		return errors.New("approve: expected a context, found nil")
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	canonicalID, lookupOnly, hintScope, err := resolveApproveScope(cfg, rawID, opts.sourceOnly)
	if err != nil {
		return err
	}
	if opts.revoke {
		return runApproveRevoke(ctx, cmd, cfg, canonicalID, lookupOnly, hintScope)
	}
	if !opts.apply && cfg.Defaults.DryRun {
		return runApproveDryRun(ctx, cmd, cfg, canonicalID, lookupOnly, hintScope)
	}
	return runApproveApply(ctx, cmd, cfg, canonicalID, lookupOnly, hintScope)
}

func resolveApproveScope(cfg *config.Config, rawID, sourceOnly string) (canonicalID, lookupOnly, hintScope string, err error) {
	if sourceOnly != "" {
		if _, err := configFor(cfg, sourceOnly); err != nil {
			return "", "", "", err
		}
	}
	dirScope, err := approveDirScope(cfg, sourceOnly)
	if err != nil {
		return "", "", "", err
	}
	hintScope = firstNonEmpty(sourceOnly, dirScope, cfg.ItemSource)
	canonicalID, urlSource, err := normalizeStartID(rawID, cfg, hintScope)
	if err != nil {
		return "", "", "", err
	}
	lookupOnly = sourceOnly
	if lookupOnly == "" && urlSource != "" {
		lookupOnly = urlSource
	}
	return canonicalID, lookupOnly, hintScope, nil
}

func runApproveApply(ctx context.Context, cmd *cobra.Command, cfg *config.Config, canonicalID, lookupOnly, hintScope string) error {
	return runWrite(ctx, cmd, "approve", func(sess *writeSession) (string, error) {
		return applyApproveSession(ctx, cmd.OutOrStdout(), sess, cfg, canonicalID, lookupOnly, hintScope)
	})
}

func applyApproveSession(ctx context.Context, out io.Writer, sess *writeSession, cfg *config.Config, canonicalID, lookupOnly, hintScope string) (string, error) {
	srcKey, srcCfg, err := resolveApproveTarget(ctx, sess.st, cfg, canonicalID, lookupOnly, hintScope, sess.srcs)
	if err != nil {
		return "approve " + canonicalID, err
	}
	src := sess.srcs[srcKey]
	live, body, hash, err := fetchApproveContent(ctx, src, canonicalID)
	if err != nil {
		return "approve " + canonicalID, err
	}
	targetStatus, statusPlan := approveStatusPlan(live.State, srcCfg)
	if err := writeApproveInspection(out, live, body, srcKey, statusPlan, hash); err != nil {
		return "approve " + canonicalID, err
	}
	if err := transitionApproveStatus(ctx, src, canonicalID, targetStatus); err != nil {
		return "approve " + canonicalID, err
	}
	if err := sess.st.SetApproval(ctx, store.Approval{
		Source:      srcKey,
		ItemID:      canonicalID,
		ContentHash: hash,
		ApprovedAt:  nowUTC(),
	}); err != nil {
		return "approve " + canonicalID, err //nolint:wrapcheck // propagate context cancellation
	}
	if err := refreshApprovedItem(ctx, sess, srcKey, canonicalID, targetStatus, live); err != nil {
		return "approved " + canonicalID, err
	}
	if _, err := fmt.Fprintf(out, "approved %s (%s)\n", canonicalID, hash); err != nil {
		return "approved " + canonicalID, fmt.Errorf("write approve: %w", err)
	}
	return "approved " + canonicalID, nil
}

func transitionApproveStatus(ctx context.Context, src source.Source, id, targetStatus string) error {
	if targetStatus == "" {
		return nil
	}
	if err := source.SetStatus(ctx, src, id, targetStatus); err != nil {
		return fmt.Errorf("approve %s: transition to %q: %w", id, targetStatus, err)
	}
	return nil
}

func refreshApprovedItem(ctx context.Context, sess *writeSession, srcKey, id, targetStatus string, live source.Item) error {
	if targetStatus != "" {
		return sess.refresh(srcKey, id)
	}
	if live.Source == srcKey {
		return sess.st.UpsertItems(ctx, srcKey, []source.Item{live}) //nolint:wrapcheck // propagate context cancellation
	}
	return nil
}

func runApproveDryRun(ctx context.Context, cmd *cobra.Command, cfg *config.Config, canonicalID, lookupOnly, hintScope string) (err error) {
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			err = errors.Join(err, fmt.Errorf("close store: %w", cerr))
		}
	}()
	deps, err := sourceDeps()
	if err != nil {
		return err
	}
	srcs, err := buildSources(cfg, deps)
	if err != nil {
		return err
	}
	srcKey, srcCfg, err := resolveApproveTarget(ctx, st, cfg, canonicalID, lookupOnly, hintScope, srcs)
	if err != nil {
		return err
	}
	live, body, hash, err := fetchApproveContent(ctx, srcs[srcKey], canonicalID)
	if err != nil {
		return err
	}
	targetStatus, statusPlan := approveStatusPlan(live.State, srcCfg)
	if err := writeApproveInspection(cmd.OutOrStdout(), live, body, srcKey, statusPlan, hash); err != nil {
		return err
	}
	msg := "dry-run: pass --apply to record approval\n"
	if targetStatus != "" {
		msg = fmt.Sprintf("dry-run: pass --apply to record approval and transition status to %s\n", targetStatus)
	}
	if _, err := io.WriteString(cmd.OutOrStdout(), msg); err != nil {
		return fmt.Errorf("write approve: %w", err)
	}
	return nil
}

func runApproveRevoke(ctx context.Context, cmd *cobra.Command, cfg *config.Config, canonicalID, lookupOnly, hintScope string) (err error) {
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			err = errors.Join(err, fmt.Errorf("close store: %w", cerr))
		}
	}()
	srcKey, _, err := resolveApproveTarget(ctx, st, cfg, canonicalID, lookupOnly, hintScope, nil)
	if err != nil {
		return err
	}
	deleted, err := st.DeleteApproval(ctx, srcKey, canonicalID)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	summary := "revoke " + canonicalID + " (none)"
	msg := fmt.Sprintf("no approval recorded for %s\n", canonicalID)
	if deleted {
		summary = "revoked " + canonicalID
		msg = fmt.Sprintf("revoked %s\n", canonicalID)
	}
	detail, err := writeDetailJSON(summary, nil)
	if err != nil {
		return err
	}
	if err := st.AppendRun(ctx, store.Run{
		StartedAt:  nowUTC(),
		Command:    "approve",
		Outcome:    "ok",
		DetailJSON: detail,
	}); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	if _, err := io.WriteString(cmd.OutOrStdout(), msg); err != nil {
		return fmt.Errorf("write approve: %w", err)
	}
	return nil
}

func approveDirScope(cfg *config.Config, sourceOnly string) (string, error) {
	if sourceOnly != "" {
		return "", nil
	}
	env := processEnv()
	dir, err := env.Getwd()
	if err != nil {
		return "", fmt.Errorf("approve: current directory: %w", err)
	}
	if key, ok := cfg.SourceForDir(dir, env); ok {
		return key, nil
	}
	return "", nil
}

func resolveApproveTarget(ctx context.Context, st *store.Store, cfg *config.Config, canonicalID, lookupOnly, hintScope string, srcs map[string]source.Source) (string, config.Source, error) {
	if it, err := resolveStartItem(ctx, st, canonicalID, lookupOnly, hintScope); err == nil {
		srcCfg, err := configFor(cfg, it.Source)
		if err != nil {
			return "", config.Source{}, err
		}
		if err := checkKind(it.Source, srcCfg.Kind); err != nil {
			return "", config.Source{}, err
		}
		return it.Source, srcCfg, nil
	} else if !strings.Contains(err.Error(), "found none") {
		return "", config.Source{}, err
	}

	if lookupOnly != "" {
		srcCfg, err := configFor(cfg, lookupOnly)
		if err != nil {
			return "", config.Source{}, err
		}
		return lookupOnly, srcCfg, nil
	}
	if repo, _, err := source.ParseGitHubID(canonicalID); err == nil {
		if matched := findGitHubSource(cfg, repo); matched != "" {
			srcCfg, err := configFor(cfg, matched)
			if err != nil {
				return "", config.Source{}, err
			}
			return matched, srcCfg, nil
		}
	}
	if hintScope != "" {
		srcCfg, err := configFor(cfg, hintScope)
		if err != nil {
			return "", config.Source{}, err
		}
		return hintScope, srcCfg, nil
	}
	if srcs != nil {
		w := &writeSession{ctx: ctx, st: st, cfg: cfg, srcs: srcs}
		resolved, err := w.resolveLive(canonicalID)
		if err != nil {
			return "", config.Source{}, err
		}
		srcCfg, err := configFor(cfg, resolved.key)
		if err != nil {
			return "", config.Source{}, err
		}
		return resolved.key, srcCfg, nil
	}
	return "", config.Source{}, fmt.Errorf("item %s: expected one item or configured source, found none", canonicalID)
}

func fetchApproveContent(ctx context.Context, src source.Source, id string) (source.Item, string, string, error) {
	if src == nil {
		return source.Item{}, "", "", fmt.Errorf("approve %s: expected a source, found nil", id)
	}
	live, body, err := source.ReadContent(ctx, src, id)
	if err != nil {
		return source.Item{}, "", "", fmt.Errorf("approve %s: %w", id, err)
	}
	hash := live.ContentHash
	if hash == "" {
		hash = source.ContentHash(live.Title, body)
		live.ContentHash = hash
	}
	return live, body, hash, nil
}

func approveStatusPlan(currentState string, cfg config.Source) (target, display string) {
	allowed := cfg.Trust.EffectiveUntrustedReadyStatuses()
	if len(allowed) == 0 {
		return "", fmt.Sprintf("%s (no transition)", currentState)
	}
	if slices.Contains(allowed, currentState) {
		return "", fmt.Sprintf("%s (already in untrusted_ready_statuses)", currentState)
	}
	return allowed[0], fmt.Sprintf("%s -> %s", currentState, allowed[0])
}

func writeApproveInspection(w io.Writer, live source.Item, body, srcKey, statusPlan, hash string) error {
	author := live.Author
	if author == "" {
		author = "(unknown)"
	}
	if live.AuthorAssociation != "" {
		author += " (" + live.AuthorAssociation + ")"
	}
	var b strings.Builder
	bprintf(&b, "id:      %s\n", live.ID)
	bprintf(&b, "source:  %s\n", srcKey)
	bprintf(&b, "author:  %s\n", author)
	bprintf(&b, "locked:  %t\n", live.Locked)
	bprintf(&b, "status:  %s\n", statusPlan)
	bprintf(&b, "hash:    %s\n", hash)
	bprintf(&b, "title:   %s\n", live.Title)
	b.WriteString("body:\n")
	if body != "" {
		b.WriteString(body)
		if !strings.HasSuffix(body, "\n") {
			b.WriteByte('\n')
		}
	}
	if _, err := io.WriteString(w, b.String()); err != nil {
		return fmt.Errorf("write approve: %w", err)
	}
	return nil
}
