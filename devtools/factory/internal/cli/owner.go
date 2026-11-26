package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"slices"
	"strings"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/tick"
)

func newOwnerCmd(ctx context.Context) *cobra.Command {
	var (
		asJSON bool
		only   string
	)
	cmd := &cobra.Command{
		Use:           "owner <id>",
		Short:         "Print the machine and tab whose agent last worked an item",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			return oneID("owner", args)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runOwner(ctx, cmd, args[0], only, asJSON)
		},
	}
	cmd.Flags().BoolVar(&asJSON, "json", false, "print the owner as JSON, or null when the item was never claimed")
	cmd.Flags().StringVar(&only, "source", "", "source key (kind:name) when the id is in more than one source")
	return cmd
}

// runOwner reads only the claims in the local store, so it also answers
// for an item that has left the mirror.
func runOwner(ctx context.Context, cmd *cobra.Command, id, only string, asJSON bool) (err error) {
	if ctx == nil {
		return errors.New("owner: expected a context, found nil")
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
	claims, err := st.Claims(ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	latest, found, err := latestClaimFor(claims, id, only)
	if err != nil {
		return err
	}
	out := cmd.OutOrStdout()
	if asJSON {
		var v *tick.Owner
		if found {
			o := tick.Ownership(cfg, latest)
			v = &o
		}
		enc := json.NewEncoder(out)
		enc.SetIndent("", "  ")
		if err := enc.Encode(v); err != nil {
			return fmt.Errorf("write owner: %w", err)
		}
		return nil
	}
	line := "no owner: " + id + " was never claimed"
	if found {
		o := tick.Ownership(cfg, latest)
		line = fmt.Sprintf("%s %s: machine %s, lane %s, tab %s, workspace %s, %s since %s",
			o.Source, o.ID, o.Machine, orDash(o.Lane), orDash(o.Tab), orDash(o.Workspace), o.State, o.ClaimedAt.Format("2006-01-02T15:04:05Z"))
	}
	if _, err := fmt.Fprintln(out, line); err != nil {
		return fmt.Errorf("write owner: %w", err)
	}
	return nil
}

// latestClaimFor returns the newest claim on id, in only when set. Claims
// on the same id in two sources need only.
func latestClaimFor(claims []store.Claim, id, only string) (store.Claim, bool, error) {
	var latest store.Claim
	found := false
	sources := map[string]struct{}{}
	for _, c := range claims {
		if c.ID != id || (only != "" && c.Source != only) {
			continue
		}
		sources[c.Source] = struct{}{}
		if !found || c.ClaimedAt.After(latest.ClaimedAt) {
			latest, found = c, true
		}
	}
	if len(sources) > 1 {
		names := make([]string, 0, len(sources))
		for s := range sources {
			names = append(names, s)
		}
		slices.Sort(names)
		return store.Claim{}, false, usageError{err: fmt.Errorf("owner %s: expected one source, found %s; pass --source", id, strings.Join(names, ", "))}
	}
	return latest, found, nil
}

func orDash(s string) string {
	if s == "" {
		return "-"
	}
	return s
}
