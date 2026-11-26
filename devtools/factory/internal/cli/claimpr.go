package cli

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func newClaimPRCmd(ctx context.Context) *cobra.Command {
	cmd := &cobra.Command{
		Use:           "claim-pr <id> <url>",
		Short:         "Record a pull request on an active claim",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 2 {
				return usageError{err: fmt.Errorf("claim-pr: expected an id and a pull request URL, found %d arguments", len(args))}
			}
			if args[0] == "" {
				return usageError{err: errors.New("claim-pr: expected a non-empty id, found empty")}
			}
			if args[1] == "" {
				return usageError{err: errors.New("claim-pr: expected a pull request URL, found empty")}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runClaimPR(ctx, cmd, args[0], args[1])
		},
	}
	return cmd
}

func runClaimPR(ctx context.Context, cmd *cobra.Command, id, ref string) (err error) {
	if ctx == nil {
		return errors.New("claim-pr: expected a context, found nil")
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
	it, err := resolveItem(ctx, st, id, "")
	if err != nil {
		return err
	}
	claim, found, err := latestClaim(ctx, st, it.Source, it.ID)
	if err != nil {
		return err
	}
	if !found {
		return fmt.Errorf("claim-pr %s: expected an active claim, found none", it.ID)
	}
	if !claim.State.Active() {
		return fmt.Errorf("claim-pr %s: expected an active claim, found %s", it.ID, claim.State)
	}
	parsed, err := gh.ParseRef(ref)
	if err != nil {
		return fmt.Errorf("claim-pr: %w", err)
	}
	exec := newCommandExec()
	if exec == nil {
		return errors.New("claim-pr: expected an executor, found nil")
	}
	pulls, err := ghClient(exec, cfg.GHBin)
	if err != nil {
		return err
	}
	open, err := pulls.OpenByMe(ctx)
	if err != nil {
		return fmt.Errorf("claim-pr: gh open prs: %w", err)
	}
	parsed = enrichClaimPR(parsed, open)
	merged := store.MergeClaimPRs(claim.PRs, []store.ClaimPR{parsed})
	if store.SameClaimPRs(claim.PRs, merged) {
		return nil
	}
	if err := st.FillClaim(ctx, claim.Source, claim.ID, claim.ClaimedAt, claim.TabID, claim.WorkspaceID, merged, time.Now().UTC()); err != nil {
		return fmt.Errorf("claim-pr %s: %w", it.ID, err)
	}
	return nil
}

func enrichClaimPR(pr store.ClaimPR, open []gh.PR) store.ClaimPR {
	for _, got := range open {
		if got.Repo != pr.Repo || got.Number != pr.Number {
			continue
		}
		if got.URL != "" {
			pr.URL = got.URL
		}
		if got.Title != "" {
			pr.Title = got.Title
		}
		if got.Head != "" {
			pr.Head = got.Head
		}
		return pr
	}
	return pr
}
