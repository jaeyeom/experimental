package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/backlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

func newSyncCmd(ctx context.Context) *cobra.Command {
	var full, asJSON bool
	var scope scopeFlags
	cmd := &cobra.Command{
		Use:           "sync",
		Short:         "Refresh the local mirror from each source",
		SilenceErrors: true,
		SilenceUsage:  true,
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runSync(ctx, cmd, full, scope, asJSON)
		},
	}
	cmd.Flags().BoolVar(&full, "full", false, "resync every item and delete items the source no longer returns")
	addScopeFlags(cmd, &scope)
	cmd.Flags().BoolVar(&asJSON, "json", false, "print one JSON array with a row per source")
	return cmd
}

func runSync(ctx context.Context, cmd *cobra.Command, full bool, scope scopeFlags, asJSON bool) (err error) {
	if ctx == nil {
		return errors.New("sync: expected a context, found nil")
	}
	if err := scope.check("sync"); err != nil {
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
	path, err := databasePath(cmd)
	if err != nil {
		return err
	}
	st, err := store.Open(ctx, path)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
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
	now := nowUTC()
	results, syncErr := syncer.Sync(ctx, st, srcs, now, syncer.Options{
		Full:            full,
		Only:            only,
		FullSyncEvery:   cfg.Defaults.FullSyncEvery,
		AfterSync:       backlog.Hook(st, cfg),
		ExternalLookups: cfg.ExternalLookups(),
	})
	writeErr := errors.Join(printResults(cmd, results, asJSON), writeSyncWarnings(cmd.ErrOrStderr(), "sync", results))
	var recordErr error
	if len(results) > 0 || syncErr == nil {
		recordErr = recordSync(ctx, st, now, results)
	}
	exitErr := syncErr
	if exitErr == nil {
		exitErr = partialFailure(results)
	}
	return errors.Join(exitErr, recordErr, writeErr)
}

func openDB(ctx context.Context, cmd *cobra.Command) (*store.Store, error) {
	path, err := databasePath(cmd)
	if err != nil {
		return nil, err
	}
	return store.Open(ctx, path) //nolint:wrapcheck // propagate context cancellation
}

func databasePath(cmd *cobra.Command) (string, error) {
	if cmd == nil || cmd.Root() == nil {
		return "", errors.New("database path: expected a command, found nil")
	}
	path, err := cmd.Root().PersistentFlags().GetString("db")
	if err != nil {
		return "", fmt.Errorf("read --db flag: %w", err)
	}
	if path != "" {
		return path, nil
	}
	return store.DefaultPath(config.OSEnv()), nil
}

func printResults(cmd *cobra.Command, results []syncer.Result, asJSON bool) error {
	out := cmd.OutOrStdout()
	if asJSON {
		raw, err := json.MarshalIndent(syncDetails(results), "", "  ")
		if err != nil {
			return fmt.Errorf("sync json: %w", err)
		}
		if _, err := fmt.Fprintln(out, string(raw)); err != nil {
			return fmt.Errorf("write sync result: %w", err)
		}
		return nil
	}
	for _, res := range results {
		if _, err := fmt.Fprintln(out, formatResult(res)); err != nil {
			return fmt.Errorf("write sync result: %w", err)
		}
	}
	return nil
}

func formatResult(res syncer.Result) string {
	if res.Err != nil {
		return fmt.Sprintf("%s  error: %s", res.Source, res.Err.Error())
	}
	line := fmt.Sprintf("%s  incremental  %d items (%d removed) since %s", res.Source, res.Items, res.Removed, res.Since.UTC().Format(time.RFC3339))
	if res.Full {
		line = fmt.Sprintf("%s  full  %d items (%d removed)", res.Source, res.Items, res.Removed)
	}
	if ext := res.External; ext != nil && ext.Referenced > 0 {
		line += fmt.Sprintf("  outside blockers: %d fetched, %d done, %d skipped", ext.Fetched, ext.Done, ext.Skipped)
	}
	return line
}

// writeSyncWarnings prints one stderr line per result that carries a warning.
// A warning is not a failure: the sync still exits 0.
func writeSyncWarnings(w io.Writer, prefix string, results []syncer.Result) error {
	for _, res := range results {
		if res.Warning == "" {
			continue
		}
		if _, err := fmt.Fprintf(w, "warning: %s %s: %s\n", prefix, res.Source, res.Warning); err != nil {
			return fmt.Errorf("write sync warning: %w", err)
		}
	}
	return nil
}

func partialFailure(results []syncer.Result) error {
	parts := make([]string, 0, len(results))
	for _, res := range results {
		if res.Err != nil {
			parts = append(parts, fmt.Sprintf("%s: %s", res.Source, res.Err.Error()))
		}
	}
	if len(parts) == 0 {
		return nil
	}
	return fmt.Errorf("sync: %s", strings.Join(parts, "; "))
}

func recordSync(ctx context.Context, st *store.Store, now time.Time, results []syncer.Result) error {
	detail, err := syncDetailJSON(results)
	if err != nil {
		return err
	}
	return st.AppendRun(ctx, store.Run{ //nolint:wrapcheck // propagate context cancellation
		StartedAt:  now,
		Command:    "sync",
		Outcome:    syncOutcome(results),
		DetailJSON: detail,
	})
}

func syncOutcome(results []syncer.Result) string {
	failed := 0
	for _, res := range results {
		if res.Err != nil {
			failed++
		}
	}
	switch {
	case failed == 0:
		return "ok"
	case len(results) > 0 && failed == len(results):
		return "error"
	default:
		return "partial"
	}
}

type syncDetail struct {
	Source  string `json:"source"`
	Mode    string `json:"mode,omitempty"`
	Items   int    `json:"items"`
	Removed int    `json:"removed"`
	Since   string `json:"since,omitempty"`
	Warning string `json:"warning,omitempty"`
	Error   string `json:"error,omitempty"`
	// External is set for a source whose sync looks up blockers outside
	// the mirror.
	External *syncer.External `json:"external_blockers,omitempty"` //nolint:tagliatelle // documented factory JSON output field
}

func syncDetailJSON(results []syncer.Result) (string, error) {
	raw, err := json.Marshal(syncDetails(results))
	if err != nil {
		return "", fmt.Errorf("sync detail: %w", err)
	}
	return string(raw), nil
}

func syncDetails(results []syncer.Result) []syncDetail {
	rows := make([]syncDetail, 0, len(results))
	for _, res := range results {
		row := syncDetail{
			Source:   res.Source,
			Items:    res.Items,
			Removed:  res.Removed,
			Warning:  res.Warning,
			External: res.External,
		}
		switch {
		case res.Full:
			row.Mode = "full"
		case res.Err == nil || !res.Since.IsZero():
			row.Mode = "incremental"
			row.Since = res.Since.UTC().Format(time.RFC3339)
		}
		if res.Err != nil {
			row.Error = res.Err.Error()
		}
		rows = append(rows, row)
	}
	return rows
}
