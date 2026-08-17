package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"regexp"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

var repoFlagPattern = regexp.MustCompile(`^[^/\s]+/[^/\s]+$`)

func newScanCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var (
		configPath string
		repos      []string
		jsonFlag   bool
	)
	cmd := &cobra.Command{
		Use:   "scan",
		Short: "Survey open GitHub PRs and match them to herdr tabs",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, _ []string) error {
			_ = jsonFlag
			if err := validateRepoFlags(repos); err != nil {
				return &ExitError{Code: ExitUsage, Err: err}
			}
			cfg, err := config.Load(configPath)
			if err != nil {
				return fmt.Errorf("load config: %w", err)
			}
			deps := scan.Deps{
				GH:    gh.NewClient(exec, cfg.GHBin),
				Herdr: herdr.NewClient(exec, cfg.HerdrBin),
			}
			doc, err := scan.Run(cmd.Context(), deps, cfg, repos, time.Now())
			if scan.Started(doc) {
				if werr := writeJSON(stdout, doc); werr != nil {
					return werr
				}
			}
			if err != nil {
				return scanExit(err, cfg)
			}
			return nil
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().StringArrayVar(&repos, "repo", nil, "replace repos list (repeatable owner/repo)")
	cmd.Flags().BoolVar(&jsonFlag, "json", false, "accepted, no-op (stdout is always JSON)")
	return cmd
}

func validateRepoFlags(repos []string) error {
	for _, repo := range repos {
		if !repoFlagPattern.MatchString(repo) {
			return fmt.Errorf("invalid repo %q", repo)
		}
	}
	return nil
}

func writeJSON(w io.Writer, doc scan.Document) error {
	out, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode scan: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", out); err != nil {
		return fmt.Errorf("write scan: %w", err)
	}
	return nil
}

func scanExit(err error, cfg config.Config) error {
	if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
		return &ExitError{Code: ExitPrecondition, Err: errors.New("interrupted")}
	}
	var notFound *executor.ExecutableNotFoundError
	if errors.As(err, &notFound) {
		return &ExitError{Code: ExitPrecondition, Err: fmt.Errorf("gh binary not found (gh_bin=%q)", cfg.GHBin)}
	}
	if errors.Is(err, gh.ErrUnauthenticated) {
		return &ExitError{Code: ExitPrecondition, Err: errors.New("gh is not authenticated; run: gh auth login")}
	}
	if errors.Is(err, herdr.ErrUnsupported) {
		return &ExitError{Code: ExitPrecondition, Err: herdrUnsupportedMessage(err, cfg.HerdrBin)}
	}
	return &ExitError{Code: ExitPrecondition, Err: err}
}

func herdrUnsupportedMessage(err error, bin string) error {
	msg := err.Error()
	const older = " is older than "
	if i := strings.Index(msg, "herdr "); i >= 0 {
		rest := msg[i+len("herdr "):]
		if j := strings.Index(rest, older); j >= 0 {
			return fmt.Errorf("herdr %s is older than 0.8 (herdr_bin=%q)", rest[:j], bin)
		}
	}
	const prefix = "cannot parse herdr version from "
	if i := strings.Index(msg, prefix); i >= 0 {
		raw := strings.Trim(msg[i+len(prefix):], `"`)
		return fmt.Errorf("cannot parse herdr version from %q", raw)
	}
	return err
}
