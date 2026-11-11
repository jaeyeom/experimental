package dispatch

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

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

func (p procFS) List(context.Context) ([]Process, error) {
	entries, err := os.ReadDir(p.root)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", p.root, err)
	}
	uptime := p.uptime()
	out := make([]Process, 0, len(entries))
	for _, e := range entries {
		pid, err := strconv.Atoi(e.Name())
		if err != nil {
			continue
		}
		proc, ok := p.read(pid, uptime)
		if ok {
			out = append(out, proc)
		}
	}
	return out, nil
}

// clockTicks is USER_HZ, the unit of a /proc/<pid>/stat start time.
// Linux fixes it at 100 on every common architecture.
const clockTicks = 100

// uptime is the system uptime from <root>/uptime, or negative.
func (p procFS) uptime() time.Duration {
	raw, err := os.ReadFile(filepath.Join(p.root, "uptime"))
	if err != nil {
		return -1
	}
	fields := strings.Fields(string(raw))
	if len(fields) == 0 {
		return -1
	}
	secs, err := strconv.ParseFloat(fields[0], 64)
	if err != nil {
		return -1
	}
	return time.Duration(secs * float64(time.Second))
}

// read parses <pid>/stat and <pid>/cmdline. A process that exits while
// it is read is skipped. Age is uptime less the start time (field 22).
func (p procFS) read(pid int, uptime time.Duration) (Process, bool) {
	dir := filepath.Join(p.root, strconv.Itoa(pid))
	stat, err := os.ReadFile(filepath.Join(dir, "stat"))
	if err != nil {
		return Process{}, false
	}
	// comm may hold spaces and parentheses; it ends at the last ')'.
	text := string(stat)
	open, closing := strings.IndexByte(text, '('), strings.LastIndexByte(text, ')')
	if open < 0 || closing < open {
		return Process{}, false
	}
	fields := strings.Fields(text[closing+1:])
	if len(fields) < 2 {
		return Process{}, false
	}
	ppid, err := strconv.Atoi(fields[1])
	if err != nil {
		return Process{}, false
	}
	proc := Process{PID: pid, PPID: ppid, Name: text[open+1 : closing], Age: -1}
	// fields[0] is field 3 (state), so field 22 (starttime) is fields[19].
	if len(fields) > 19 && uptime >= 0 {
		if ticks, err := strconv.ParseInt(fields[19], 10, 64); err == nil {
			if age := uptime - time.Duration(ticks)*time.Second/clockTicks; age >= 0 {
				proc.Age = age
			}
		}
	}
	if raw, err := os.ReadFile(filepath.Join(dir, "cmdline")); err == nil {
		raw = bytes.TrimRight(raw, "\x00")
		if len(raw) > 0 {
			proc.Argv = strings.Split(string(raw), "\x00")
		}
	}
	return proc, true
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
	out, err := p.run(ctx, "ps", "-A", "-o", "pid=,ppid=,etime=,args=")
	if err != nil {
		return nil, err
	}
	return parsePS(out), nil
}

func (p psTable) Cwd(ctx context.Context, pid int) (string, error) {
	out, err := p.run(ctx, "lsof", "-a", "-p", strconv.Itoa(pid), "-d", "cwd", "-Fn")
	if err != nil {
		return "", err
	}
	cwd := parseLsofCwd(out)
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

// parsePS parses `ps -A -o pid=,ppid=,etime=,args=`. ps joins argv
// with spaces, so Argv splits on whitespace and Name is the base of
// argv[0].
func parsePS(out string) []Process {
	var procs []Process
	for _, line := range strings.Split(out, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 3 {
			continue
		}
		pid, err1 := strconv.Atoi(fields[0])
		ppid, err2 := strconv.Atoi(fields[1])
		if err1 != nil || err2 != nil {
			continue
		}
		proc := Process{PID: pid, PPID: ppid, Age: parseEtime(fields[2])}
		if len(fields) > 3 {
			proc.Argv = fields[3:]
			proc.Name = filepath.Base(fields[3])
		}
		procs = append(procs, proc)
	}
	return procs
}

// parseEtime parses ps etime, [[dd-]hh:]mm:ss, or is negative.
func parseEtime(s string) time.Duration {
	var days int
	if d, rest, ok := strings.Cut(s, "-"); ok {
		n, err := strconv.Atoi(d)
		if err != nil {
			return -1
		}
		days, s = n, rest
	}
	parts := strings.Split(s, ":")
	if len(parts) < 2 || len(parts) > 3 {
		return -1
	}
	total := time.Duration(days) * 24 * time.Hour
	unit := time.Second
	for i := len(parts) - 1; i >= 0; i-- {
		n, err := strconv.Atoi(parts[i])
		if err != nil || n < 0 {
			return -1
		}
		total += time.Duration(n) * unit
		unit *= 60
	}
	return total
}

// parseLsofCwd reads the n (name) field of `lsof -Fn` output.
func parseLsofCwd(out string) string {
	for _, line := range strings.Split(out, "\n") {
		if name, ok := strings.CutPrefix(line, "n"); ok {
			return name
		}
	}
	return ""
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
