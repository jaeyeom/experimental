// Package procinfo reads the local process table and available memory,
// walks process trees, and holds the rule for a long-lived background
// job under a coding agent. prsync's gate and agentbus mem share it.
package procinfo

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// Process is one row of the process table.
type Process struct {
	PID  int
	PPID int
	// Name is the kernel's process name (comm), or the base of argv[0]
	// when the platform does not report one separately.
	Name string
	Argv []string
	// Age is how long the process had run when it was listed. It is
	// negative when unknown.
	Age time.Duration
	// RSS is the resident set size in bytes, or 0 when unknown.
	RSS int64
	// Cwd is the working directory when the listing reports it. The
	// tables here leave it empty.
	Cwd string
}

// Table lists the local processes in one read.
type Table interface {
	List(ctx context.Context) ([]Process, error)
}

// Runner runs one command and returns its stdout. A non-zero exit is an
// error.
type Runner func(ctx context.Context, name string, args ...string) (string, error)

// runTimeout bounds one command run by Run.
const runTimeout = 10 * time.Second

// Run is the Runner that executes name on PATH.
func Run(ctx context.Context, name string, args ...string) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, runTimeout)
	defer cancel()
	var stderr bytes.Buffer
	// The callers here run fixed programs (ps, vm_stat), not a shell line.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, name, args...)
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("%s: %w: %s", name, err, strings.TrimSpace(stderr.String()))
	}
	return string(out), nil
}

// LocalTable is this machine's process table: /proc where it exists
// (Linux), else one ps run.
func LocalTable() Table {
	if _, err := os.Stat("/proc/self/stat"); err == nil {
		return ProcFS{Root: "/proc"}
	}
	return PS{Run: Run}
}

// PSArgs are the ps arguments PS runs: every process with its parent,
// RSS in KiB, elapsed time, and command line.
var PSArgs = []string{"-A", "-o", "pid=,ppid=,rss=,etime=,args="}

// PS lists processes with one ps run.
type PS struct {
	Run Runner
}

// List runs ps once and parses it.
func (p PS) List(ctx context.Context) ([]Process, error) {
	out, err := p.Run(ctx, "ps", PSArgs...)
	if err != nil {
		return nil, fmt.Errorf("list processes: %w", err)
	}
	return ParsePS(out), nil
}

// ParsePS parses the output of `ps` with PSArgs. ps joins argv with
// spaces, so Argv splits on whitespace and Name is the base of argv[0].
// An RSS that does not parse is 0.
func ParsePS(out string) []Process {
	var procs []Process
	for _, line := range strings.Split(out, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 4 {
			continue
		}
		pid, err1 := strconv.Atoi(fields[0])
		ppid, err2 := strconv.Atoi(fields[1])
		if err1 != nil || err2 != nil {
			continue
		}
		proc := Process{PID: pid, PPID: ppid, Age: ParseEtime(fields[3])}
		if kib, err := strconv.ParseInt(fields[2], 10, 64); err == nil && kib > 0 {
			proc.RSS = kib * 1024
		}
		if len(fields) > 4 {
			proc.Argv = fields[4:]
			proc.Name = filepath.Base(fields[4])
		}
		procs = append(procs, proc)
	}
	return procs
}

// ParseEtime parses ps etime, [[dd-]hh:]mm:ss, or is negative.
func ParseEtime(s string) time.Duration {
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

// ProcFS reads a Linux /proc tree rooted at Root. PageSize is the unit
// of the RSS field; zero is this machine's page size.
type ProcFS struct {
	Root     string
	PageSize int64
}

// List reads <pid>/stat and <pid>/cmdline for every process. A process
// that exits while it is read is skipped.
func (p ProcFS) List(context.Context) ([]Process, error) {
	entries, err := os.ReadDir(p.Root)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", p.Root, err)
	}
	uptime := p.uptime()
	pageSize := p.PageSize
	if pageSize <= 0 {
		pageSize = int64(os.Getpagesize())
	}
	out := make([]Process, 0, len(entries))
	for _, e := range entries {
		pid, err := strconv.Atoi(e.Name())
		if err != nil {
			continue
		}
		if proc, ok := p.read(pid, uptime, pageSize); ok {
			out = append(out, proc)
		}
	}
	return out, nil
}

// clockTicks is USER_HZ, the unit of a /proc/<pid>/stat start time.
// Linux fixes it at 100 on every common architecture.
const clockTicks = 100

// uptime is the system uptime from <root>/uptime, or negative.
func (p ProcFS) uptime() time.Duration {
	raw, err := os.ReadFile(filepath.Join(p.Root, "uptime"))
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

// read parses <pid>/stat and <pid>/cmdline. Age is uptime less the start
// time (field 22); RSS is field 24 in pages.
func (p ProcFS) read(pid int, uptime time.Duration, pageSize int64) (Process, bool) {
	dir := filepath.Join(p.Root, strconv.Itoa(pid))
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
	// fields[0] is field 3 (state), so field N is fields[N-3].
	if len(fields) > 19 && uptime >= 0 {
		if ticks, err := strconv.ParseInt(fields[19], 10, 64); err == nil {
			if age := uptime - time.Duration(ticks)*time.Second/clockTicks; age >= 0 {
				proc.Age = age
			}
		}
	}
	if len(fields) > 21 {
		if pages, err := strconv.ParseInt(fields[21], 10, 64); err == nil && pages > 0 {
			proc.RSS = pages * pageSize
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
