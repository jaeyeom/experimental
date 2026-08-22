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
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
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
		force      bool
		readStdin  bool
	)
	cmd := &cobra.Command{
		Use:   "dispatch",
		Short: "Send unmatched review comments to the matched herdr agent",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runDispatch(cmd.Context(), stdout, exec, configPath, prs, all, goLive, rebase, force, readStdin)
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().StringArrayVar(&prs, "pr", nil, "limit to owner/repo#N (repeatable)")
	cmd.Flags().BoolVar(&all, "all", false, "dispatch every PR in the scan document")
	cmd.Flags().BoolVar(&goLive, "go", false, "send prompts (default is dry-run)")
	cmd.Flags().BoolVar(&rebase, "rebase", false, "dispatch a rebase-in-place prompt (skips the unaddressed-comment gate)")
	cmd.Flags().BoolVar(&force, "force", false, "re-dispatch even if dedupe state would skip")
	cmd.Flags().BoolVar(&readStdin, "stdin", false, "read a scan document from stdin (otherwise self-scan)")
	return cmd
}

func runDispatch(ctx context.Context, stdout io.Writer, exec executor.Executor, configPath string, prs []string, all, goLive, rebase, force, readStdin bool) error {
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
	h := herdr.NewClient(exec, cfg.HerdrBin)
	out, err := dispatch.Run(ctx, h, dispatch.FileStore{Path: cfg.StateFile}, cfg, dispatch.Request{
		Doc:        doc,
		PRs:        prs,
		RunnerPane: h.RunnerPane(ctx),
		Rebase:     rebase,
		Force:      force,
	}, time.Now())
	if writeErr := writeDispatchJSON(stdout, out); writeErr != nil {
		return writeErr
	}
	if err != nil {
		return dispatchExit(err, cfg)
	}
	return nil
}

func loadScanDoc(ctx context.Context, cfg config.Config, exec executor.Executor, readStdin bool) (scan.Document, error) {
	doc, fromStdin, err := readStdinScan(os.Stdin, readStdin)
	if err != nil {
		return scan.Document{}, &ExitError{Code: ExitUsage, Err: fmt.Errorf("stdin scan JSON: %w", err)}
	}
	if fromStdin {
		return doc, nil
	}
	deps := scan.Deps{
		GH:    gh.NewClient(exec, cfg.GHBin),
		Herdr: herdr.NewClient(exec, cfg.HerdrBin),
	}
	doc, err = scan.Run(ctx, deps, cfg, nil, time.Now())
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
	if errors.Is(err, dispatch.ErrTimeout) {
		return &ExitError{Code: ExitGateTimeout, Err: err}
	}
	if errors.Is(err, dispatch.ErrLock) {
		return &ExitError{Code: ExitPrecondition, Err: err}
	}
	if errors.Is(err, herdr.ErrNotInstalled) {
		return &ExitError{Code: ExitPrecondition, Err: fmt.Errorf("herdr binary not found (herdr_bin=%q)", cfg.HerdrBin)}
	}
	return scanExit(err, cfg)
}
