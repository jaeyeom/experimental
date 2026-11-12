package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/rcasetting"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/roundcache"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
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
		inScope    bool
		scope      scopeFlags
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
			if err := scope.apply(cmd, &cfg); err != nil {
				return err
			}
			if cmd.Flags().Changed("in-scope") {
				cfg.ScanInScopeOnly = inScope
			}
			logStartup(cmd.Context(), "scan", cfg)
			deps, saveCache := scanDeps(cmd.Context(), exec, cfg, herdr.NewClient(exec, cfg.HerdrBin))
			now := time.Now()
			doc, err := scan.Run(cmd.Context(), deps, cfg, repos, now)
			saveCache()
			if scan.Started(doc) {
				if aerr := annotateUnresolved(&doc, cfg, now); aerr != nil && err == nil {
					err = aerr
				}
				outOfScope := scan.CountOutOfScope(doc)
				if cfg.ScanInScopeOnly {
					scan.DropOutOfScope(&doc)
				}
				if werr := writeJSON(stdout, doc); werr != nil {
					return werr
				}
				if werr := writeTeamCoverHints(cmd.ErrOrStderr(), doc); werr != nil {
					return werr
				}
				if werr := writeOutOfScopeCount(cmd.ErrOrStderr(), outOfScope, cfg.TabMachine); werr != nil {
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
	cmd.Flags().BoolVar(&inScope, "in-scope", false, "leave PRs marked out_of_scope out of the output (overrides scan_in_scope_only)")
	addScopeFlags(cmd, &scope)
	return cmd
}

// writeOutOfScopeCount writes how many PRs another tab_machine run owns,
// when any were marked.
func writeOutOfScopeCount(w io.Writer, n int, machine string) error {
	if n == 0 {
		return nil
	}
	if _, err := fmt.Fprintf(w, "%d out of scope (tab_machine=%s)\n", n, machine); err != nil {
		return fmt.Errorf("write out-of-scope count: %w", err)
	}
	return nil
}

// roundCacheFile sits next to state_file. It keeps GitHub results that a
// later round can reuse; see roundcache.
const roundCacheFile = "round-cache.json"

// scanDeps returns the scan adapters and a func that saves the round cache.
// Call the func once the scan is done.
func scanDeps(ctx context.Context, exec executor.Executor, cfg config.Config, h scan.Herdr) (scan.Deps, func()) {
	src := &rcasetting.Source{}
	logger := runlog.FromContext(ctx)
	cache, err := roundcache.Load(filepath.Join(filepath.Dir(cfg.StateFile), roundCacheFile), time.Now)
	if err != nil {
		logger.Warn("round cache ignored", "err", err.Error())
	}
	cache.SetHistoryTTL(cfg.TeamCoverHistoryTTL)
	cache.SetLoadTTL(cfg.TeamCoverLoadTTL)
	cache.SetMembersTTL(cfg.TeamCoverMembersTTL)
	cache.SetCodeownersTTL(cfg.TeamCoverCodeownersTTL)
	deps := scan.Deps{
		GH:      gh.NewClient(exec, cfg.GHBin).WithRollupCache(cache),
		Herdr:   h,
		Remote:  func(machine string) scan.Herdr { return machineHerdr(exec, cfg, machine) },
		Marker:  src.For,
		History: cache,
		Rounds:  cache,
	}
	return deps, func() {
		if err := cache.Save(); err != nil {
			logger.Warn("round cache not saved", "err", err.Error())
		}
	}
}

// machineHerdr is the herdr client for one of cfg.Machines.
func machineHerdr(exec executor.Executor, cfg config.Config, machine string) *herdr.Client {
	return herdr.NewClient(exec, cfg.HerdrBin).OnMachine(machine, cfg.MachineTimeout)
}

func validateRepoFlags(repos []string) error {
	for _, repo := range repos {
		if !repoFlagPattern.MatchString(repo) {
			return fmt.Errorf("invalid repo %q", repo)
		}
	}
	return nil
}

// writeTeamCoverHints writes each PR's team-cover lines, then the round's
// team-cover notes.
func writeTeamCoverHints(w io.Writer, doc scan.Document) error {
	for _, pr := range doc.PRs {
		logins, err := writeTeamCoverLines(w, pr)
		if err != nil {
			return err
		}
		if len(logins) == 0 {
			continue
		}
		if _, err := fmt.Fprintf(w, "gh pr edit %d --repo %s --add-reviewer %s\n", pr.Number, pr.Repo, strings.Join(logins, ",")); err != nil {
			return fmt.Errorf("write team cover hint: %w", err)
		}
	}
	for _, note := range doc.TeamCoverNotes {
		if _, err := fmt.Fprintln(w, note); err != nil {
			return fmt.Errorf("write team cover note: %w", err)
		}
	}
	return nil
}

func writeTeamCoverLines(w io.Writer, pr scan.PR) ([]string, error) {
	var logins []string
	seen := map[string]struct{}{}
	for _, team := range pr.TeamReviewSuggestions {
		line, err := teamCoverLine(pr, team)
		if err != nil {
			return nil, err
		}
		if line == "" {
			continue
		}
		if _, err := fmt.Fprintln(w, line); err != nil {
			return nil, fmt.Errorf("write team cover hint: %w", err)
		}
		for _, suggestion := range team.Suggest {
			if _, ok := seen[suggestion.Login]; ok {
				continue
			}
			seen[suggestion.Login] = struct{}{}
			logins = append(logins, suggestion.Login)
		}
	}
	return logins, nil
}

func teamCoverLine(pr scan.PR, team scan.TeamReviewSuggestion) (string, error) {
	switch {
	case team.Error != "":
		return fmt.Sprintf("%s#%d %s: uncovered: %s", pr.Repo, pr.Number, team.Team, team.Error), nil
	case team.CoveredBy != nil:
		return fmt.Sprintf("%s#%d %s: covered by %s", pr.Repo, pr.Number, team.Team, *team.CoveredBy), nil
	case team.UncoveredReason != "":
		return fmt.Sprintf("%s#%d %s: uncovered: %s", pr.Repo, pr.Number, team.Team, team.UncoveredReason), nil
	}
	var lines []string
	for _, suggestion := range team.Suggest {
		line := fmt.Sprintf("%s#%d %s: suggest %s", pr.Repo, pr.Number, team.Team, suggestion.Login)
		if len(suggestion.Reasons) > 0 {
			line += " (" + strings.Join(suggestion.Reasons, ", ") + ")"
		}
		lines = append(lines, line)
	}
	return strings.Join(lines, "\n"), nil
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

func annotateUnresolved(doc *scan.Document, cfg config.Config, now time.Time) error {
	st, err := dispatch.LoadFile(cfg.StateFile)
	if err != nil {
		return fmt.Errorf("load state: %w", err)
	}
	dispatch.MarkUnresolved(doc, st, cfg, now)
	return nil
}

func scanExit(err error, cfg config.Config) error {
	if errors.Is(err, dispatch.ErrCorruptState) {
		return &ExitError{Code: ExitUsage, Err: err}
	}
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
