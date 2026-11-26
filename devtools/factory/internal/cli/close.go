package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/parentclose"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

type closeOptions struct {
	apply       bool
	comment     string
	commentFile string
	asJSON      bool
	noSync      bool
	scope       scopeFlags
}

func newCloseCmd(ctx context.Context) *cobra.Command {
	var opts closeOptions
	cmd := &cobra.Command{
		Use:           "close [id...]",
		Short:         "Detect and safely close open parent issues whose children are all done",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			for _, arg := range args {
				if arg == "" {
					return usageError{err: errors.New("close: expected a non-empty id, found empty")}
				}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runClose(ctx, cmd, args, opts)
		},
	}
	cmd.Flags().BoolVar(&opts.apply, "apply", false, "close eligible parent issues in the source")
	cmd.Flags().StringVar(&opts.comment, "comment", "", "comment text to post when closing")
	cmd.Flags().StringVar(&opts.commentFile, "comment-file", "", "path of a file to use as the closing comment")
	cmd.Flags().BoolVar(&opts.asJSON, "json", false, "print the result as JSON")
	cmd.Flags().BoolVar(&opts.noSync, "no-sync", false, "do not refresh a stale mirror")
	addScopeFlags(cmd, &opts.scope)
	return cmd
}

func runClose(ctx context.Context, cmd *cobra.Command, args []string, opts closeOptions) error {
	if ctx == nil {
		return errors.New("close: expected a context, found nil")
	}
	if err := opts.scope.check("close"); err != nil {
		return err
	}
	comment, err := closeComment(cmd, opts.comment, opts.commentFile)
	if err != nil {
		return err
	}
	if err := warnLiteralNewlineFor(cmd.ErrOrStderr(), "comment", "--comment-file", comment); err != nil {
		return err
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	ids := dedupeIDs(args)
	only, err := closeScope(cmd, cfg, opts.scope, len(ids) > 0)
	if err != nil {
		return err
	}
	results, err := evaluateClose(ctx, cmd, cfg, only, ids, opts.noSync)
	if err != nil {
		return err
	}
	if !opts.apply {
		return writeCloseResults(cmd.OutOrStdout(), results, opts.asJSON, false)
	}
	if len(results) == 0 {
		return writeCloseResults(cmd.OutOrStdout(), results, opts.asJSON, true)
	}
	if err := validateCloseApply(cfg, results); err != nil {
		return err
	}
	return applyClose(ctx, cmd, results, comment, opts.asJSON)
}

func closeComment(cmd *cobra.Command, comment, commentFile string) (string, error) {
	commentSet := cmd.Flags().Changed("comment")
	fileSet := cmd.Flags().Changed("comment-file")
	switch {
	case commentSet && fileSet:
		return "", usageError{err: errors.New("close: expected only one of --comment and --comment-file, found both")}
	case fileSet:
		return readCommentFile(commentFile)
	default:
		return comment, nil
	}
}

func readCommentFile(path string) (string, error) {
	if path == "" {
		return "", usageError{err: errors.New("close: expected a --comment-file path, found empty")}
	}
	raw, err := os.ReadFile(path) //nolint:gosec // G304: the path is the user-supplied --comment-file.
	if err != nil {
		return "", fmt.Errorf("close --comment-file: %w", err)
	}
	return string(raw), nil
}

func dedupeIDs(args []string) []string {
	if len(args) == 0 {
		return nil
	}
	seen := make(map[string]struct{}, len(args))
	out := make([]string, 0, len(args))
	for _, id := range args {
		if _, ok := seen[id]; ok {
			continue
		}
		seen[id] = struct{}{}
		out = append(out, id)
	}
	return out
}

func closeScope(cmd *cobra.Command, cfg *config.Config, scope scopeFlags, hasIDs bool) (string, error) {
	if hasIDs {
		if scope.only != "" {
			if _, err := sourcesFor(cfg, scope.only); err != nil {
				return "", err
			}
		}
		return scope.only, nil
	}
	only, err := scope.resolve(cmd, cfg)
	if err != nil {
		return "", err
	}
	if _, err := sourcesFor(cfg, only); err != nil {
		return "", err
	}
	return only, nil
}

func evaluateClose(ctx context.Context, cmd *cobra.Command, cfg *config.Config, only string, ids []string, noSync bool) ([]parentclose.Result, error) {
	st, err := openDB(ctx, cmd)
	if err != nil {
		return nil, err
	}
	err = refreshBeforeRead(ctx, cmd, cfg, st, only, noSync)
	var results []parentclose.Result
	if err == nil {
		if len(ids) == 0 {
			results, err = detectClosableParents(ctx, st, cfg, only)
		} else {
			results, err = evaluateNamedParents(ctx, st, ids, only)
		}
	}
	if closeErr := st.Close(); closeErr != nil {
		err = errors.Join(err, fmt.Errorf("close store: %w", closeErr))
	}
	return results, err
}

func detectClosableParents(ctx context.Context, st *store.Store, cfg *config.Config, only string) ([]parentclose.Result, error) {
	srcs, err := sourcesFor(cfg, only)
	if err != nil {
		return nil, err
	}
	finished, err := st.FinishedItems(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	var all []parentclose.Result
	for i := range srcs {
		key, err := sourceKey(srcs[i].Kind, srcs[i].Name)
		if err != nil {
			return nil, err
		}
		items, err := st.Items(ctx, key)
		if err != nil {
			return nil, err //nolint:wrapcheck // propagate context cancellation
		}
		found, err := parentclose.Find(items, finished)
		if err != nil {
			return nil, err //nolint:wrapcheck // internal factory error already carries context
		}
		all = append(all, found...)
	}
	return all, nil
}

func evaluateNamedParents(ctx context.Context, st *store.Store, ids []string, only string) ([]parentclose.Result, error) {
	finished, err := st.FinishedItems(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	itemsBySource := make(map[string][]source.Item)
	out := make([]parentclose.Result, 0, len(ids))
	for _, id := range ids {
		it, err := resolveItem(ctx, st, id, only)
		if err != nil {
			return nil, err
		}
		items, ok := itemsBySource[it.Source]
		if !ok {
			items, err = st.Items(ctx, it.Source)
			if err != nil {
				return nil, err //nolint:wrapcheck // propagate context cancellation
			}
			itemsBySource[it.Source] = items
		}
		res, err := parentclose.Evaluate(it.ID, items, finished)
		if err != nil {
			return nil, err //nolint:wrapcheck // internal factory error already carries context
		}
		out = append(out, res)
	}
	return out, nil
}

func validateCloseApply(cfg *config.Config, results []parentclose.Result) error {
	var errs []error
	for _, res := range results {
		if cfg.ItemSource != "" && res.Source != cfg.ItemSource {
			errs = append(errs, fmt.Errorf("close %s: expected source %s (%s), found %s", res.ID, cfg.ItemSource, config.ItemSourceEnv, res.Source))
			continue
		}
		if !res.Eligible {
			errs = append(errs, fmt.Errorf("close %s: %s", res.ID, res.Reason))
		}
	}
	return errors.Join(errs...)
}

func applyClose(ctx context.Context, cmd *cobra.Command, results []parentclose.Result, comment string, asJSON bool) error {
	return runWrite(ctx, cmd, "close", func(sess *writeSession) (string, error) {
		for _, res := range results {
			if src := sess.srcs[res.Source]; src == nil {
				return closeSummary(nil, results), fmt.Errorf("close %s: expected configured source %s, found none", res.ID, res.Source)
			}
		}
		closed := make([]string, 0, len(results))
		for _, res := range results {
			src := sess.srcs[res.Source]
			if err := src.Close(sess.ctx, res.ID, comment); err != nil {
				return closeSummary(closed, results), fmt.Errorf("close %s: %w", res.ID, err)
			}
			if _, err := syncer.Refresh(sess.ctx, sess.st, res.Source, src, res.ID); err != nil {
				return closeSummary(closed, results), fmt.Errorf("close %s: refresh: %w", res.ID, err)
			}
			closed = append(closed, res.ID)
		}
		if err := writeCloseResults(cmd.OutOrStdout(), results, asJSON, true); err != nil {
			return closeSummary(closed, results), err
		}
		return closeSummary(closed, results), nil
	})
}

func closeSummary(closed []string, results []parentclose.Result) string {
	if len(closed) > 0 {
		return "closed " + strings.Join(closed, " ")
	}
	ids := make([]string, len(results))
	for i, res := range results {
		ids[i] = res.ID
	}
	return "close " + strings.Join(ids, " ")
}

type closeResultJSON struct {
	Source       string   `json:"source"`
	ID           string   `json:"id"`
	Title        string   `json:"title"`
	Eligible     bool     `json:"eligible"`
	DoneChildren []string `json:"done_children"` //nolint:tagliatelle // documented factory JSON output field
	OpenChildren []string `json:"open_children"` //nolint:tagliatelle // documented factory JSON output field
	Reason       string   `json:"reason,omitempty"`
	Closed       bool     `json:"closed,omitempty"`
}

func writeCloseResults(w io.Writer, results []parentclose.Result, asJSON, applied bool) error {
	if asJSON {
		return writeCloseJSON(w, results, applied)
	}
	if len(results) == 0 {
		return nil
	}
	var b strings.Builder
	for _, res := range results {
		if applied {
			bprintf(&b, "%s\n", res.ID)
			continue
		}
		if res.Eligible {
			bprintf(&b, "%s  eligible  %d done, %d open  %s\n", res.ID, len(res.DoneChildren), len(res.OpenChildren), res.Title)
		} else {
			bprintf(&b, "%s  ineligible  %d done, %d open  %s\n", res.ID, len(res.DoneChildren), len(res.OpenChildren), res.Reason)
		}
	}
	if _, err := io.WriteString(w, b.String()); err != nil {
		return fmt.Errorf("write close: %w", err)
	}
	return nil
}

func writeCloseJSON(w io.Writer, results []parentclose.Result, applied bool) error {
	out := make([]closeResultJSON, len(results))
	for i, res := range results {
		done := res.DoneChildren
		if done == nil {
			done = []string{}
		}
		open := res.OpenChildren
		if open == nil {
			open = []string{}
		}
		out[i] = closeResultJSON{
			Source:       res.Source,
			ID:           res.ID,
			Title:        res.Title,
			Eligible:     res.Eligible,
			DoneChildren: done,
			OpenChildren: open,
			Reason:       res.Reason,
			Closed:       applied,
		}
	}
	raw, err := json.MarshalIndent(out, "", "  ")
	if err != nil {
		return fmt.Errorf("close json: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", raw); err != nil {
		return fmt.Errorf("write close json: %w", err)
	}
	return nil
}
