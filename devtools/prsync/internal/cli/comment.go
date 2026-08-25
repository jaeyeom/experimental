package cli

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/comment"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

func newCommentCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var (
		configPath string
		prs        []string
		all        bool
		goLive     bool
		readStdin  bool
		body       string
		ci         bool
	)
	cmd := &cobra.Command{
		Use:   "comment",
		Short: "Post a PR comment via gh (no herdr tab)",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, _ []string) error {
			resolved, err := resolveCommentBody(body, cmd.Flags().Changed("body"), ci)
			if err != nil {
				return &ExitError{Code: ExitUsage, Err: err}
			}
			return runComment(cmd.Context(), stdout, exec, configPath, prs, all, goLive, readStdin, resolved)
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().StringArrayVar(&prs, "pr", nil, "limit to owner/repo#N (repeatable)")
	cmd.Flags().BoolVar(&all, "all", false, "comment on every PR in the scan document")
	cmd.Flags().BoolVar(&goLive, "go", false, "post comments (default is dry-run)")
	cmd.Flags().BoolVar(&readStdin, "stdin", false, "read a scan document from stdin (otherwise self-scan)")
	cmd.Flags().StringVar(&body, "body", "", "comment body text")
	cmd.Flags().BoolVar(&ci, "ci", false, "alias for --body /ci")
	return cmd
}

func runComment(ctx context.Context, stdout io.Writer, exec executor.Executor, configPath string, prs []string, all, goLive, readStdin bool, body string) error {
	if all && len(prs) > 0 {
		return &ExitError{Code: ExitUsage, Err: errors.New("cannot combine --pr and --all")}
	}
	for _, p := range prs {
		if _, _, err := dispatch.ParsePR(p); err != nil {
			return &ExitError{Code: ExitUsage, Err: err}
		}
	}
	cfg, err := config.Load(configPath)
	if err != nil {
		return fmt.Errorf("load config: %w", err)
	}
	if goLive {
		cfg.DryRun = false
	}
	doc, err := loadScanDoc(ctx, cfg, exec, readStdin)
	if err != nil {
		return err
	}
	out, err := comment.Run(ctx, gh.NewClient(exec, cfg.GHBin), cfg, comment.Request{
		Doc:  doc,
		PRs:  prs,
		Body: body,
	}, time.Now())
	if writeErr := writeDispatchJSON(stdout, out); writeErr != nil {
		return writeErr
	}
	if err != nil {
		return scanExit(err, cfg)
	}
	return nil
}

func resolveCommentBody(body string, bodySet, ci bool) (string, error) {
	if ci && bodySet {
		return "", errors.New("cannot combine --ci and --body")
	}
	if !ci && !bodySet {
		return "", errors.New("require --body or --ci")
	}
	if ci {
		body = "/ci"
	}
	if strings.TrimSpace(body) == "" {
		return "", errors.New("--body must not be empty")
	}
	return body, nil
}
