package cli

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/priority"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func newPrioritiesCmd(ctx context.Context) *cobra.Command {
	var apply bool
	var noSync bool
	var scope scopeFlags
	cmd := &cobra.Command{
		Use:           "priorities",
		Short:         "Report a lower-priority item blocking a higher-priority one",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("priorities: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runPriorities(ctx, cmd, apply, noSync, scope)
		},
	}
	cmd.Flags().BoolVar(&apply, "apply", false, "write suggested priority raises to the source")
	cmd.Flags().BoolVar(&noSync, "no-sync", false, "do not refresh a stale mirror")
	addScopeFlags(cmd, &scope)
	return cmd
}

func runPriorities(ctx context.Context, cmd *cobra.Command, apply, noSync bool, scope scopeFlags) error {
	if ctx == nil {
		return errors.New("priorities: expected a context, found nil")
	}
	if err := scope.check("priorities"); err != nil {
		return err
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	only, err := scope.resolve(cmd, cfg)
	if err != nil {
		return err
	}
	if _, err := sourcesFor(cfg, only); err != nil {
		return err
	}
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	err = refreshBeforeRead(ctx, cmd, cfg, st, only, noSync)
	var rows []priorityRow
	if err == nil {
		rows, err = collectPriorities(ctx, st, cfg, only)
	}
	if closeErr := st.Close(); closeErr != nil {
		err = errors.Join(err, fmt.Errorf("close store: %w", closeErr))
	}
	if err != nil {
		return err
	}
	if !apply || len(rows) == 0 {
		return writePriorities(cmd.OutOrStdout(), rows)
	}
	return runWrite(ctx, cmd, "priorities", func(sess *writeSession) (string, error) {
		if err := sess.raise(rows); err != nil {
			return prioritySummary(rows), err
		}
		if err := writePriorities(cmd.OutOrStdout(), rows); err != nil {
			return prioritySummary(rows), err
		}
		return prioritySummary(rows), nil
	})
}

type priorityRow struct {
	key string
	inv priority.Inversion
}

func collectPriorities(ctx context.Context, st *store.Store, cfg *config.Config, only string) ([]priorityRow, error) {
	srcs, err := sourcesFor(cfg, only)
	if err != nil {
		return nil, err
	}
	var rows []priorityRow
	for i := range srcs {
		part, err := sourcePriorities(ctx, st, srcs[i])
		if err != nil {
			return nil, err
		}
		rows = append(rows, part...)
	}
	return rows, nil
}

func sourcePriorities(ctx context.Context, st *store.Store, src config.Source) ([]priorityRow, error) {
	key, err := sourceKey(src.Kind, src.Name)
	if err != nil {
		return nil, err
	}
	if err := checkKind(key, src.Kind); err != nil {
		return nil, err
	}
	items, err := st.Items(ctx, key)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	byID, err := itemMap(items)
	if err != nil {
		return nil, err
	}
	found, err := priority.Find(byID, src)
	if err != nil {
		return nil, err //nolint:wrapcheck // internal factory error already carries context
	}
	rows := make([]priorityRow, len(found))
	for i, inv := range found {
		rows[i] = priorityRow{key: key, inv: inv}
	}
	return rows, nil
}

func itemMap(items []source.Item) (map[string]source.Item, error) {
	out := make(map[string]source.Item, len(items))
	for _, it := range items {
		if _, ok := out[it.ID]; ok {
			return nil, fmt.Errorf("priorities: expected one item %s, found a duplicate", it.ID)
		}
		out[it.ID] = it
	}
	return out, nil
}

func writePriorities(w io.Writer, rows []priorityRow) error {
	text := formatPriorities(rows)
	if text == "" {
		return nil
	}
	if _, err := io.WriteString(w, text); err != nil {
		return fmt.Errorf("write priorities: %w", err)
	}
	return nil
}

func formatPriorities(rows []priorityRow) string {
	if len(rows) == 0 {
		return ""
	}
	var b strings.Builder
	for _, row := range rows {
		bprintf(&b, "%s\n", priority.Format(row.inv))
	}
	return b.String()
}

func prioritySummary(rows []priorityRow) string {
	parts := make([]string, len(rows))
	for i, row := range rows {
		parts[i] = row.inv.BlockerID
	}
	return "priorities " + strings.Join(parts, " ")
}

func (w *writeSession) raise(rows []priorityRow) error {
	for _, row := range rows {
		if row.inv.SuggestName == "" {
			return fmt.Errorf("priorities %s: expected one name for rank %d, found none", row.inv.BlockerID, row.inv.TargetPriority)
		}
	}
	for _, row := range rows {
		src := w.srcs[row.key]
		if src == nil {
			return fmt.Errorf("priorities: expected source %s, found none", row.key)
		}
		if err := src.SetPriority(w.ctx, row.inv.BlockerID, row.inv.TargetPriority); err != nil {
			return err //nolint:wrapcheck // propagate context cancellation
		}
		if err := w.refresh(row.key, row.inv.BlockerID); err != nil {
			return err
		}
	}
	return nil
}
