package dispatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
	executor "github.com/jaeyeom/go-cmdexec"
)

const probeTimeout = 10 * time.Second

// NewProcessTable is the local process table: /proc where it exists
// (Linux), else ps and lsof.
func NewProcessTable(exec executor.Executor) ProcessTable {
	if pathExists("/proc/self/stat") {
		return procFS{root: "/proc"}
	}
	return psTable{exec: exec}
}

// procFS reads a Linux /proc tree rooted at root.
type procFS struct {
	root string
}

func (p procFS) List(ctx context.Context) ([]Process, error) {
	procs, err := procinfo.ProcFS{Root: p.root}.List(ctx)
	if err != nil {
		return nil, fmt.Errorf("local processes: %w", err)
	}
	return procs, nil
}

func (p procFS) Cwd(_ context.Context, pid int) (string, error) {
	cwd, err := os.Readlink(filepath.Join(p.root, strconv.Itoa(pid), "cwd"))
	if err != nil {
		return "", fmt.Errorf("cwd of %d: %w", pid, err)
	}
	return cwd, nil
}

// psTable lists processes with ps and reads a cwd with lsof.
type psTable struct {
	exec executor.Executor
}

func (p psTable) List(ctx context.Context) ([]Process, error) {
	procs, err := procinfo.PS{Run: p.run}.List(ctx)
	if err != nil {
		return nil, fmt.Errorf("local processes: %w", err)
	}
	return procs, nil
}

func (p psTable) Cwd(ctx context.Context, pid int) (string, error) {
	out, err := p.run(ctx, "lsof", "-a", "-p", strconv.Itoa(pid), "-d", "cwd", "-Fn")
	if err != nil {
		return "", err
	}
	cwd := procinfo.ParseLsofCwd(out)
	if cwd == "" {
		return "", fmt.Errorf("lsof: no cwd for %d", pid)
	}
	return cwd, nil
}

func (p psTable) run(ctx context.Context, bin string, args ...string) (string, error) {
	res, err := p.exec.Execute(ctx, executor.ToolConfig{Command: bin, Args: args, Timeout: probeTimeout})
	if err != nil {
		return "", fmt.Errorf("run %s: %w", bin, err)
	}
	if res.ExitCode != 0 {
		return "", fmt.Errorf("%s exited %d: %s", bin, res.ExitCode, strings.TrimSpace(res.Stderr))
	}
	return res.Output, nil
}

// DecideCLI lists open decide items with `decide list --json`.
type DecideCLI struct {
	Exec executor.Executor
	Bin  string
}

// Items returns the open and the answered decide items. A missing
// binary is ErrDecideMissing.
func (d DecideCLI) Items(ctx context.Context) ([]DecideItem, error) {
	var out []DecideItem
	for _, state := range []string{"open", "answered"} {
		items, err := d.list(ctx, state)
		if err != nil {
			return nil, err
		}
		out = append(out, items...)
	}
	return out, nil
}

func (d DecideCLI) list(ctx context.Context, state string) ([]DecideItem, error) {
	res, err := d.Exec.Execute(ctx, executor.ToolConfig{Command: d.Bin, Args: []string{"list", "--json", "--state", state}, Timeout: probeTimeout})
	if err != nil {
		var notFound *executor.ExecutableNotFoundError
		if errors.As(err, &notFound) {
			return nil, fmt.Errorf("%w: %w", ErrDecideMissing, err)
		}
		return nil, fmt.Errorf("run %s: %w", d.Bin, err)
	}
	if res.ExitCode != 0 {
		return nil, fmt.Errorf("%s list exited %d: %s", d.Bin, res.ExitCode, strings.TrimSpace(res.Stderr))
	}
	var items []DecideItem
	if err := json.Unmarshal([]byte(res.Output), &items); err != nil {
		return nil, fmt.Errorf("decode decide list: %w", err)
	}
	return items, nil
}
