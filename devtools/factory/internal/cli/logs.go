package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// logTimeLayout is the local timestamp on each text line.
const logTimeLayout = "2006-01-02 15:04"

func newLogsCmd(ctx context.Context) *cobra.Command {
	var (
		limit    int
		asJSON   bool
		showPath bool
		scope    scopeFlags
	)
	cmd := &cobra.Command{
		Use:           "logs",
		Short:         "Show the run history",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("logs: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runLogs(ctx, cmd, limit, asJSON, showPath, scope)
		},
	}
	cmd.Flags().IntVarP(&limit, "n", "n", 20, "maximum runs to print")
	cmd.Flags().BoolVar(&asJSON, "json", false, "print the runs as JSON")
	cmd.Flags().BoolVar(&showPath, "path", false, "print the database path and exit")
	addScopeFlags(cmd, &scope)
	return cmd
}

// maxRunsScanned bounds how many runs logs reads when it filters by source.
const maxRunsScanned = 100000

func runLogs(ctx context.Context, cmd *cobra.Command, limit int, asJSON, showPath bool, scope scopeFlags) (err error) {
	if ctx == nil {
		return errors.New("logs: expected a context, found nil")
	}
	if limit < 1 {
		return usageError{err: fmt.Errorf("logs: expected -n >= 1, found %d", limit)}
	}
	if err := scope.check("logs"); err != nil {
		return err
	}
	if showPath {
		return printLogPath(cmd)
	}
	only, err := logsScope(cmd, scope)
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
	runs, err := scopedRuns(ctx, st, only, limit)
	if err != nil {
		return err
	}
	text, err := formatLogs(runs, asJSON)
	if err != nil {
		return err
	}
	if _, err := io.WriteString(cmd.OutOrStdout(), text); err != nil {
		return fmt.Errorf("write logs: %w", err)
	}
	return nil
}

// logsScope picks the source logs filters by. logs reads no config for
// --source or --all. For the current-directory default it needs one, and
// without a loadable config it shows every run, as before.
func logsScope(cmd *cobra.Command, scope scopeFlags) (string, error) {
	if scope.only != "" || scope.all {
		return scope.only, nil
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		slog.Debug("logs: no config, so no current-directory source", "err", err)
		return "", nil
	}
	return scope.resolve(cmd, cfg)
}

// scopedRuns returns the newest limit runs, or with only set, the newest
// limit runs that belong to that source.
func scopedRuns(ctx context.Context, st *store.Store, only string, limit int) ([]store.Run, error) {
	if only == "" {
		return st.Runs(ctx, limit) //nolint:wrapcheck // propagate context cancellation
	}
	all, err := st.Runs(ctx, maxRunsScanned)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	out := make([]store.Run, 0, limit)
	for _, run := range all {
		if len(out) == limit {
			break
		}
		if runInSource(run, only) {
			out = append(out, run)
		}
	}
	return out, nil
}

// runInSource reports whether run belongs to the source key (kind:name).
// A run whose detail names sources, in any "source" field, belongs to each
// of them, by key or by bare name. A run that names none, such as tick or a
// write, is not tied to a source and is always shown.
func runInSource(run store.Run, key string) bool {
	var detail any
	if json.Unmarshal([]byte(run.DetailJSON), &detail) != nil {
		return true
	}
	named := detailSources(detail, nil)
	if len(named) == 0 {
		return true
	}
	_, name, _ := strings.Cut(key, ":")
	for _, src := range named {
		if src == key || src == name {
			return true
		}
	}
	return false
}

func detailSources(v any, out []string) []string {
	switch v := v.(type) {
	case map[string]any:
		for k, child := range v {
			if text, ok := child.(string); ok && k == "source" && text != "" {
				out = append(out, text)
				continue
			}
			out = detailSources(child, out)
		}
	case []any:
		for _, child := range v {
			out = detailSources(child, out)
		}
	}
	return out
}

func printLogPath(cmd *cobra.Command) error {
	path, err := databasePath(cmd)
	if err != nil {
		return err
	}
	if _, err := fmt.Fprintln(cmd.OutOrStdout(), path); err != nil {
		return fmt.Errorf("write logs path: %w", err)
	}
	return nil
}

func formatLogs(runs []store.Run, asJSON bool) (string, error) {
	if asJSON {
		return formatLogsJSON(runs)
	}
	var b strings.Builder
	for _, run := range runs {
		summary, err := logSummary(run)
		if err != nil {
			return "", err
		}
		if _, err := fmt.Fprintf(&b, "%s  %s   %s   %s\n",
			run.StartedAt.Local().Format(logTimeLayout), run.Command, run.Outcome, summary); err != nil {
			return "", fmt.Errorf("format logs: %w", err)
		}
	}
	return b.String(), nil
}

type logRow struct {
	StartedAt  time.Time `json:"started_at"` //nolint:tagliatelle // documented factory JSON output field
	Command    string    `json:"command"`
	Outcome    string    `json:"outcome"`
	DetailJSON string    `json:"detail_json"` //nolint:tagliatelle // documented factory JSON output field
}

func formatLogsJSON(runs []store.Run) (string, error) {
	rows := make([]logRow, 0, len(runs))
	for _, run := range runs {
		rows = append(rows, logRow{
			StartedAt:  run.StartedAt.UTC(),
			Command:    run.Command,
			Outcome:    run.Outcome,
			DetailJSON: run.DetailJSON,
		})
	}
	raw, err := json.MarshalIndent(rows, "", "  ")
	if err != nil {
		return "", fmt.Errorf("logs json: %w", err)
	}
	return string(raw) + "\n", nil
}

// logSummary reads the summary key. A missing key, or detail that is not a
// JSON object, uses the raw outcome. An empty detail_json is the same.
func logSummary(run store.Run) (string, error) {
	if run.DetailJSON == "" {
		return run.Outcome, nil
	}
	raw, err := oneJSONValue(run.DetailJSON)
	if err != nil {
		return "", err
	}
	trimmed := bytes.TrimSpace(raw)
	if len(trimmed) == 0 || trimmed[0] != '{' {
		return run.Outcome, nil
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(trimmed, &obj); err != nil {
		return "", fmt.Errorf("logs: expected detail_json to be a JSON object, found %s", trimmed)
	}
	sum, ok := obj["summary"]
	if !ok {
		return run.Outcome, nil
	}
	// encoding/json treats null as a no-op for a string. A present null is not a string.
	if bytes.Equal(bytes.TrimSpace(sum), []byte("null")) {
		return "", errors.New("logs: expected detail_json summary to be a string, found null")
	}
	var text string
	if err := json.Unmarshal(sum, &text); err != nil {
		return "", fmt.Errorf("logs: expected detail_json summary to be a string, found %s", bytes.TrimSpace(sum))
	}
	if strings.ContainsAny(text, "\r\n") {
		return "", fmt.Errorf("logs: expected a one-line summary, found %q", text)
	}
	return text, nil
}

func oneJSONValue(detail string) (json.RawMessage, error) {
	dec := json.NewDecoder(strings.NewReader(detail))
	var raw json.RawMessage
	if err := dec.Decode(&raw); err != nil {
		return nil, fmt.Errorf("logs: expected detail_json to be JSON, found %q", detail)
	}
	var extra json.RawMessage
	if err := dec.Decode(&extra); !errors.Is(err, io.EOF) {
		return nil, errors.New("logs: expected detail_json to be one JSON value, found extra data")
	}
	return raw, nil
}
