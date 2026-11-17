package cli

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"time"
	"unicode"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

func newDispatchCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var (
		configPath string
		prs        []string
		all        bool
		goLive     bool
		rebase     bool
		merge      bool
		ciFix      bool
		hint       string
		force      bool
		readStdin  bool
		suffix     string
		openTabs   bool
		job        string
		runner     string
		scope      scopeFlags
	)
	cmd := &cobra.Command{
		Use:   "dispatch",
		Short: "Send a review-comment, rebase, merge, CI-fix, or merge-queue-fix prompt to the matched herdr agent",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, _ []string) error {
			ctx := dispatch.WithStatusWriter(cmd.Context(), cmd.ErrOrStderr())
			var promptSuffix *string
			if cmd.Flags().Changed("prompt-suffix") {
				promptSuffix = &suffix
			}
			applyScope := func(cfg *config.Config) error {
				if cmd.Flags().Changed("open-tabs") {
					cfg.OpenMissingTabs = openTabs
				}
				return scope.apply(cmd, cfg)
			}
			if err := validateJobFlags(cmd); err != nil {
				return err
			}
			if job != "" {
				cfg, err := loadDispatchConfig(configPath, promptSuffix, applyScope)
				if err != nil {
					return err
				}
				return runJob(ctx, stdout, exec, cfg, job, runner)
			}
			modes := dispatchModes{rebase: rebase, merge: merge, ciFix: ciFix}
			return runDispatch(ctx, stdout, exec, configPath, prs, all, goLive, modes, hint, force, readStdin, promptSuffix, applyScope)
		},
	}
	addScopeFlags(cmd, &scope)
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().StringArrayVar(&prs, "pr", nil, "fetch only owner/repo#N instead of a full scan (repeatable)")
	cmd.Flags().BoolVar(&all, "all", false, "dispatch every PR in the scan document")
	cmd.Flags().BoolVar(&goLive, "go", false, "send prompts (default is dry-run)")
	cmd.Flags().BoolVar(&rebase, "rebase", false, "dispatch a rebase-in-place prompt (skips the unaddressed-comment gate)")
	cmd.Flags().BoolVar(&merge, "merge", false, "dispatch a merge catch-up prompt: merge the base branch and push normally (skips the unaddressed-comment gate)")
	cmd.Flags().BoolVar(&ciFix, "ci-fix", false, "dispatch a CI-fix prompt (skips the unaddressed-comment gate)")
	cmd.Flags().StringVar(&hint, "hint", "", "non-authoritative CI failure hint (requires --ci-fix)")
	cmd.Flags().BoolVar(&force, "force", false, "re-dispatch even if dedupe state would skip")
	cmd.Flags().StringVar(&suffix, "prompt-suffix", "", "text appended after a blank line to every prompt (overrides dispatch_prompt_suffix)")
	cmd.Flags().BoolVar(&readStdin, "stdin", false, "read a scan document from stdin (otherwise self-scan, or fetch --pr targets)")
	cmd.Flags().StringVar(&job, "job", "", "run one agentbus job by id, live, and report it with agentbus jobs done or fail")
	cmd.Flags().StringVar(&runner, "runner", "", "the job's runner, for the report (requires --job; default $JOB_RUNNER)")
	cmd.Flags().BoolVar(&openTabs, "open-tabs", false, "open a herdr tab and start an agent for an in-scope PR with no tab (overrides open_missing_tabs)")
	return cmd
}

// promptWaker wakes a hibernated agent before its prompt under
// wake_before_prompt=auto. Tests replace it so they never run agentbus.
var promptWaker = func(exec executor.Executor) dispatch.Waker {
	return dispatch.AgentbusWaker{Exec: exec, Bin: "agentbus"}
}

// dispatchModes are the mutually exclusive prompt-mode flags.
type dispatchModes struct {
	rebase, merge, ciFix bool
}

func validateDispatchFlags(prs []string, all bool, m dispatchModes, hint string) error {
	if all && len(prs) > 0 {
		return &ExitError{Code: ExitUsage, Err: errors.New("cannot combine --pr and --all")}
	}
	if m.rebase && m.ciFix {
		return &ExitError{Code: ExitUsage, Err: errors.New("cannot combine --ci-fix and --rebase")}
	}
	if m.merge && m.rebase {
		return &ExitError{Code: ExitUsage, Err: errors.New("cannot combine --merge and --rebase")}
	}
	if m.merge && m.ciFix {
		return &ExitError{Code: ExitUsage, Err: errors.New("cannot combine --ci-fix and --merge")}
	}
	if hint != "" && !m.ciFix {
		return &ExitError{Code: ExitUsage, Err: errors.New("--hint requires --ci-fix")}
	}
	for _, p := range prs {
		if _, _, err := dispatch.ParsePR(p); err != nil {
			return &ExitError{Code: ExitUsage, Err: err}
		}
	}
	return nil
}

func runDispatch(ctx context.Context, stdout io.Writer, exec executor.Executor, configPath string, prs []string, all, goLive bool, modes dispatchModes, hint string, force, readStdin bool, promptSuffix *string, applyScope func(*config.Config) error) error {
	if err := validateDispatchFlags(prs, all, modes, hint); err != nil {
		return err
	}
	cfg, err := loadDispatchConfig(configPath, promptSuffix, applyScope)
	if err != nil {
		return err
	}
	if goLive {
		cfg.DryRun = false
	}
	if modes.rebase {
		if phrase := dispatch.RebaseSuffixConflict(cfg.PromptSuffix); phrase != "" {
			return &ExitError{Code: ExitUsage, Err: fmt.Errorf("--rebase rebases and force-pushes, but the prompt suffix says %q; use --merge or set dispatch_catchup = \"merge\"", phrase)}
		}
	}
	logStartup(ctx, "dispatch", cfg)
	out, err := dispatchPRs(ctx, exec, cfg, prs, modes, hint, force, readStdin)
	if out == nil {
		return err
	}
	if writeErr := writeDispatchJSON(stdout, *out); writeErr != nil {
		return writeErr
	}
	if err != nil {
		return dispatchExit(err, cfg)
	}
	return nil
}

// dispatchPRs loads the scan document and runs dispatch on it. The
// document is nil when dispatch did not start; err is then the exit
// error.
func dispatchPRs(ctx context.Context, exec executor.Executor, cfg config.Config, prs []string, modes dispatchModes, hint string, force, readStdin bool) (*dispatch.Document, error) {
	doc, err := loadDispatchDoc(ctx, cfg, exec, readStdin, prs)
	if err != nil {
		return nil, err
	}
	h := herdr.NewClient(exec, cfg.HerdrBin)
	ctx = withGateProbes(ctx, exec, cfg, h)
	req := dispatch.Request{
		Doc:        doc,
		PRs:        prs,
		RunnerPane: h.RunnerPane(ctx),
		Rebase:     modes.rebase,
		Merge:      modes.merge,
		CIFix:      modes.ciFix,
		Hint:       hint,
		Force:      force,
		Remote:     func(machine string) dispatch.Herdr { return machineHerdr(exec, cfg, machine) },
		Opener: func(machine string) dispatch.TabOpener {
			if machine == "" {
				return h
			}
			return machineHerdr(exec, cfg, machine)
		},
	}
	if cfg.WakeBeforePrompt == "auto" {
		req.Waker = promptWaker(exec)
	}
	out, err := dispatch.Run(ctx, h, dispatch.FileStore{Path: cfg.StateFile, Scope: cfg.TabMachine}, cfg, req, time.Now())
	return &out, err //nolint:wrapcheck // dispatchExit classifies it
}

func loadDispatchConfig(configPath string, promptSuffix *string, applyScope func(*config.Config) error) (config.Config, error) {
	cfg, err := config.Load(configPath)
	if err != nil {
		return config.Config{}, fmt.Errorf("load config: %w", err)
	}
	if err := applyScope(&cfg); err != nil {
		return config.Config{}, err
	}
	if promptSuffix != nil {
		cfg.PromptSuffix = *promptSuffix
	}
	return cfg, nil
}

func loadScanDoc(ctx context.Context, cfg config.Config, exec executor.Executor, readStdin bool) (scan.Document, error) {
	doc, fromStdin, err := readStdinScan(os.Stdin, readStdin)
	if err != nil {
		return scan.Document{}, &ExitError{Code: ExitUsage, Err: fmt.Errorf("stdin scan JSON: %w", err)}
	}
	if fromStdin {
		return doc, nil
	}
	return loadFullScan(ctx, cfg, exec)
}

// loadDispatchDoc uses a stdin scan document when one is present. Otherwise
// every --pr target is fetched on its own. --all and a bare dispatch still
// run a full scan.
func loadDispatchDoc(ctx context.Context, cfg config.Config, exec executor.Executor, readStdin bool, prs []string) (scan.Document, error) {
	doc, fromStdin, err := readStdinScan(os.Stdin, readStdin)
	if err != nil {
		return scan.Document{}, &ExitError{Code: ExitUsage, Err: fmt.Errorf("stdin scan JSON: %w", err)}
	}
	if fromStdin {
		return doc, nil
	}
	if len(prs) > 0 {
		return loadTargetedDoc(ctx, cfg, exec, prs)
	}
	return loadFullScan(ctx, cfg, exec)
}

func loadFullScan(ctx context.Context, cfg config.Config, exec executor.Executor) (scan.Document, error) {
	deps, saveCache := scanDeps(ctx, exec, cfg, herdr.NewClient(exec, cfg.HerdrBin))
	defer saveCache()
	doc, err := scan.Run(ctx, deps, cfg, nil, time.Now())
	return finishScanLoad(doc, err, cfg)
}

func loadTargetedDoc(ctx context.Context, cfg config.Config, exec executor.Executor, prs []string) (scan.Document, error) {
	targets := make([]scan.Target, 0, len(prs))
	for _, raw := range prs {
		repo, n, err := dispatch.ParsePR(raw)
		if err != nil {
			return scan.Document{}, &ExitError{Code: ExitUsage, Err: err}
		}
		targets = append(targets, scan.Target{Repo: repo, Number: n})
	}
	deps, saveCache := scanDeps(ctx, exec, cfg, herdr.NewClient(exec, cfg.HerdrBin))
	defer saveCache()
	doc, err := scan.RunTargets(ctx, deps, cfg, targets, time.Now())
	return finishScanLoad(doc, err, cfg)
}

func finishScanLoad(doc scan.Document, err error, cfg config.Config) (scan.Document, error) {
	if err != nil {
		if scan.Started(doc) {
			return doc, scanExit(err, cfg)
		}
		return scan.Document{}, scanExit(err, cfg)
	}
	return doc, nil
}

func readStdinScan(f *os.File, wantStdin bool) (scan.Document, bool, error) {
	if !wantStdin || !stdinIsScanSource(f) {
		return scan.Document{}, false, nil
	}
	return peekScanJSON(f)
}

// stdinIsScanSource reports whether f is a FIFO or regular file. Sockets,
// TTYs, and other char devices are excluded so a blocking first-byte peek
// cannot hang.
func stdinIsScanSource(f *os.File) bool {
	if f == nil {
		return false
	}
	fi, err := f.Stat()
	if err != nil {
		return false
	}
	m := fi.Mode()
	return m.IsRegular() || m&os.ModeNamedPipe != 0
}

func peekScanJSON(r io.Reader) (scan.Document, bool, error) {
	br := bufio.NewReader(r)
	for {
		b, err := br.ReadByte()
		if err != nil {
			if errors.Is(err, io.EOF) {
				return scan.Document{}, false, nil
			}
			return scan.Document{}, false, fmt.Errorf("read stdin: %w", err)
		}
		if !unicode.IsSpace(rune(b)) {
			if err := br.UnreadByte(); err != nil {
				return scan.Document{}, false, fmt.Errorf("unread stdin: %w", err)
			}
			break
		}
	}
	p, err := br.Peek(1)
	if err != nil && !errors.Is(err, io.EOF) {
		return scan.Document{}, false, fmt.Errorf("peek stdin: %w", err)
	}
	if len(p) != 1 || p[0] != '{' {
		return scan.Document{}, false, nil
	}
	var doc scan.Document
	if err := json.NewDecoder(br).Decode(&doc); err != nil {
		return scan.Document{}, true, fmt.Errorf("decode scan JSON: %w", err)
	}
	return doc, true, nil
}

func writeDispatchJSON(w io.Writer, doc dispatch.Document) error {
	out, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode dispatch: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", out); err != nil {
		return fmt.Errorf("write dispatch: %w", err)
	}
	return nil
}

func dispatchExit(err error, cfg config.Config) error {
	if errors.Is(err, dispatch.ErrCorruptState) {
		return &ExitError{Code: ExitUsage, Err: err}
	}
	if errors.Is(err, dispatch.ErrDirtyTree) {
		return &ExitError{Code: ExitUnsafe, Err: err}
	}
	if errors.Is(err, dispatch.ErrTimeout) {
		return &ExitError{Code: ExitGateTimeout, Err: err}
	}
	if errors.Is(err, dispatch.ErrSettleTimeout) {
		return &ExitError{Code: ExitGateTimeout, Err: fmt.Errorf("settle timeout after %s: target tab did not stay idle/done", cfg.DispatchTimeout)}
	}
	if errors.Is(err, dispatch.ErrLock) {
		return &ExitError{Code: ExitPrecondition, Err: err}
	}
	if errors.Is(err, herdr.ErrNotInstalled) {
		return &ExitError{Code: ExitPrecondition, Err: fmt.Errorf("herdr binary not found (herdr_bin=%q)", cfg.HerdrBin)}
	}
	return scanExit(err, cfg)
}
