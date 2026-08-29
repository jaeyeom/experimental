package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/docsync/internal/gitchanges"
	"github.com/jaeyeom/experimental/devtools/docsync/internal/mapping"
	"github.com/jaeyeom/experimental/devtools/docsync/internal/matcher"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

type checkOpts struct {
	base       string
	files      []string
	args       []string
	configPath string
	jsonOut    bool
	exitZero   bool
}

func newCheckCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var opts checkOpts
	cmd := &cobra.Command{
		Use:   "check",
		Short: "Report docs implicated by a set of changed files",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.args = args
			return runCheck(cmd.Context(), stdout, exec, opts)
		},
	}
	cmd.Flags().StringVar(&opts.base, "base", "", "git base ref (diff <base>...HEAD)")
	cmd.Flags().StringArrayVar(&opts.files, "files", nil, "changed file (repeatable; - reads stdin)")
	cmd.Flags().StringVar(&opts.configPath, "config", "", "path to docsync.yml")
	cmd.Flags().BoolVar(&opts.jsonOut, "json", false, "emit JSON instead of the text report")
	cmd.Flags().BoolVar(&opts.exitZero, "exit-zero", false, "exit 0 even when docs are implicated")
	return cmd
}

func runCheck(ctx context.Context, stdout io.Writer, exec executor.Executor, opts checkOpts) error {
	useFiles, err := filesRequested(opts.files, opts.args)
	if err != nil {
		return err
	}
	if err := validateCheckInputs(opts.base, useFiles); err != nil {
		return err
	}
	m, err := resolveMapping(opts.configPath)
	if err != nil {
		return err
	}
	changed, err := changedFiles(ctx, exec, m, opts)
	if err != nil {
		return err
	}
	result := matcher.Match(m, changed)
	if err := writeCheckOutput(stdout, result, opts.jsonOut); err != nil {
		return err
	}
	return checkExit(result, opts.exitZero)
}

func filesRequested(fileFlags, args []string) (bool, error) {
	if len(args) == 1 && args[0] != "-" {
		return false, &ExitError{Code: ExitUsage, Err: fmt.Errorf("unexpected argument %q", args[0])}
	}
	return len(fileFlags) > 0 || (len(args) == 1 && args[0] == "-"), nil
}

func validateCheckInputs(base string, useFiles bool) error {
	if base != "" && useFiles {
		return &ExitError{Code: ExitUsage, Err: errors.New("cannot use --base with --files")}
	}
	if base == "" && !useFiles {
		return &ExitError{Code: ExitUsage, Err: errors.New("need --base or --files")}
	}
	return nil
}

func checkExit(result matcher.Result, exitZero bool) error {
	if len(result.Affected) == 0 || exitZero {
		return nil
	}
	return &ExitError{Code: ExitDocsAffected}
}

func startDir() string {
	if wd := os.Getenv("BUILD_WORKING_DIRECTORY"); wd != "" {
		return wd
	}
	wd, err := os.Getwd()
	if err != nil {
		return ""
	}
	return wd
}

func resolveMapping(configPath string) (mapping.Mapping, error) {
	path, err := resolveMappingPath(configPath)
	if err != nil {
		return mapping.Mapping{}, err
	}
	m, err := mapping.Load(path)
	if err != nil {
		return mapping.Mapping{}, wrapMappingOpenErr(configPath, err)
	}
	return m, nil
}

func resolveMappingPath(configPath string) (string, error) {
	if configPath != "" {
		return configPath, nil
	}
	found, err := mapping.Find(startDir())
	if err != nil {
		if errors.Is(err, mapping.ErrNotFound) {
			return "", &ExitError{Code: ExitPrecondition, Err: err}
		}
		return "", fmt.Errorf("find mapping: %w", err)
	}
	return found, nil
}

func wrapMappingOpenErr(configPath string, err error) error {
	if configPath != "" && errors.Is(err, fs.ErrNotExist) {
		return &ExitError{Code: ExitUsage, Err: fmt.Errorf("config file not found: %s", configPath)}
	}
	return fmt.Errorf("load mapping: %w", err)
}

func changedFiles(ctx context.Context, exec executor.Executor, m mapping.Mapping, opts checkOpts) ([]string, error) {
	if opts.base != "" {
		return filesFromGit(ctx, exec, m, opts.base)
	}
	return filesFromInput(m, opts.files, opts.args)
}

func filesFromGit(ctx context.Context, exec executor.Executor, m mapping.Mapping, base string) ([]string, error) {
	files, err := gitchanges.Changed(ctx, exec, base, m.Root)
	if err != nil {
		return nil, mapGitErr(err)
	}
	repoRoot, err := gitchanges.RepoRoot(ctx, exec, m.Root)
	if err != nil {
		return nil, mapGitErr(err)
	}
	out := make([]string, 0, len(files))
	for _, f := range files {
		abs := filepath.Join(repoRoot, filepath.FromSlash(f))
		rel, ok := mapping.Relativize(m.Root, abs)
		if !ok {
			continue
		}
		out = append(out, rel)
	}
	return out, nil
}

func filesFromInput(m mapping.Mapping, fileFlags, args []string) ([]string, error) {
	raw := make([]string, 0, len(fileFlags))
	readStdin := false
	for _, f := range fileFlags {
		if f == "-" {
			readStdin = true
			continue
		}
		raw = append(raw, f)
	}
	for _, a := range args {
		if a == "-" {
			readStdin = true
		}
	}
	if readStdin {
		extra, err := readStdinPaths()
		if err != nil {
			return nil, err
		}
		raw = append(raw, extra...)
	}
	return relativizeAll(m.Root, startDir(), raw), nil
}

func readStdinPaths() ([]string, error) {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		return nil, fmt.Errorf("read stdin: %w", err)
	}
	var parts []string
	if bytes.Contains(data, []byte{0}) {
		parts = strings.Split(string(data), "\x00")
	} else {
		parts = strings.Split(string(data), "\n")
	}
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimRight(p, "\r")
		if p == "" {
			continue
		}
		out = append(out, p)
	}
	return out, nil
}

func relativizeAll(root, cwd string, paths []string) []string {
	out := make([]string, 0, len(paths))
	for _, p := range paths {
		rel, ok := relativizeUserPath(root, cwd, p)
		if !ok {
			continue
		}
		out = append(out, rel)
	}
	return out
}

func relativizeUserPath(root, cwd, p string) (string, bool) {
	if p == "" {
		return "", false
	}
	if filepath.IsAbs(p) {
		return mapping.Relativize(root, filepath.Clean(p))
	}
	fromCwd := filepath.Clean(filepath.Join(cwd, filepath.FromSlash(p)))
	if rel, ok := mapping.Relativize(root, fromCwd); ok {
		return rel, true
	}
	fromRoot := filepath.Clean(filepath.Join(root, filepath.FromSlash(p)))
	return mapping.Relativize(root, fromRoot)
}

func mapGitErr(err error) error {
	var notFound *executor.ExecutableNotFoundError
	if errors.As(err, &notFound) {
		return &ExitError{Code: ExitPrecondition, Err: fmt.Errorf("git not found")}
	}
	if errors.Is(err, gitchanges.ErrNotRepo) {
		return &ExitError{Code: ExitPrecondition, Err: err}
	}
	return &ExitError{Code: ExitUsage, Err: err}
}

func writeCheckOutput(w io.Writer, result matcher.Result, jsonOut bool) error {
	if jsonOut {
		return writeCheckJSON(w, result)
	}
	return writeCheckText(w, result)
}

func writeCheckJSON(w io.Writer, result matcher.Result) error {
	out, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		return fmt.Errorf("encode result: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", out); err != nil {
		return fmt.Errorf("write result: %w", err)
	}
	return nil
}

func writeCheckText(w io.Writer, result matcher.Result) error {
	var b strings.Builder
	if len(result.Affected) == 0 {
		b.WriteString("All clear — no docs implicated.\n")
	} else {
		b.WriteString("Affected docs (")
		b.WriteString(strconv.Itoa(len(result.Affected)))
		b.WriteString("):\n")
		for _, a := range result.Affected {
			b.WriteString("  ")
			b.WriteString(a.Path)
			b.WriteString(sectionLabel(a.Section))
			b.WriteByte('\n')
			for _, f := range a.TriggeredBy {
				b.WriteString("    ← ")
				b.WriteString(f)
				if a.Why != "" {
					b.WriteString(" (why: ")
					b.WriteString(a.Why)
					b.WriteByte(')')
				}
				b.WriteByte('\n')
			}
		}
	}
	if _, err := io.WriteString(w, b.String()); err != nil {
		return fmt.Errorf("write result: %w", err)
	}
	return nil
}

func sectionLabel(section string) string {
	s := strings.TrimSpace(section)
	s = strings.TrimLeft(s, "#")
	s = strings.TrimSpace(s)
	if s == "" {
		return ""
	}
	return "  §" + s
}
