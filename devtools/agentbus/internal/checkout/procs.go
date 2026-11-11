package checkout

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"time"
)

// procTimeout bounds the process listing.
const procTimeout = 20 * time.Second

// Proc is a process with its working directory.
type Proc struct {
	PID  int
	PPID int
	// Name is the program's base name, such as sleep or -zsh.
	Name string
	Args []string
	Cwd  string
}

// Label names the process for a person.
func (p Proc) Label() string {
	return fmt.Sprintf("%s (pid %d)", p.Name, p.PID)
}

// DefaultIgnoreProcs are the programs whose working directory in a
// checkout does not block adopt: interactive shells, agent CLIs, editors
// and pagers, and agentbus. lease_ignore_procs replaces it.
var DefaultIgnoreProcs = []string{
	"sh", "bash", "zsh", "fish",
	"claude", "codex", "gemini",
	"emacs", "vim", "nvim", "less",
	"agentbus",
}

// agentCLIs are the agent CLIs that may run under node or bun.
var agentCLIs = []string{"claude", "codex", "gemini"}

// Blockers are the processes whose working directory is inside root
// and that adopt must wait for: everything but the ignored programs (a
// node or bun running an ignored agent CLI counts as that CLI) and
// self's own chain of ancestors.
func Blockers(root string, procs []Proc, ignore []string, self int) []Proc {
	parent := map[int]int{}
	for _, p := range procs {
		parent[p.PID] = p.PPID
	}
	own := map[int]bool{}
	for pid := self; pid > 0 && !own[pid]; pid = parent[pid] {
		own[pid] = true
	}
	var out []Proc
	for _, p := range procs {
		if own[p.PID] || !within(p.Cwd, root) || ignored(p, ignore) {
			continue
		}
		out = append(out, p)
	}
	slices.SortFunc(out, func(a, b Proc) int { return a.PID - b.PID })
	return out
}

func ignored(p Proc, ignore []string) bool {
	name := strings.TrimPrefix(p.Name, "-")
	if slices.Contains(ignore, name) {
		return true
	}
	if name != "node" && name != "bun" {
		return false
	}
	for _, cli := range agentCLIs {
		if !slices.Contains(ignore, cli) {
			continue
		}
		for _, arg := range p.Args[min(1, len(p.Args)):] {
			if strings.Contains(arg, cli) {
				return true
			}
		}
	}
	return false
}

// ListProcs lists the processes this user can see with their working
// directories: from /proc on Linux, from lsof and ps elsewhere (macOS).
// Processes whose directory cannot be read are left out.
func ListProcs() ([]Proc, error) {
	if _, err := os.Stat("/proc/self/cwd"); err == nil {
		return procfs("/proc")
	}
	if runtime.GOOS != "darwin" {
		return nil, fmt.Errorf("listing process directories is not supported on %s", runtime.GOOS)
	}
	return lsofProcs()
}

func procfs(dir string) ([]Proc, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, fmt.Errorf("list processes: %w", err)
	}
	var out []Proc
	for _, e := range entries {
		pid, err := strconv.Atoi(e.Name())
		if err != nil {
			continue
		}
		base := filepath.Join(dir, e.Name())
		cwd, err := os.Readlink(filepath.Join(base, "cwd"))
		if err != nil {
			continue
		}
		p := Proc{PID: pid, Cwd: cwd}
		if stat, err := os.ReadFile(filepath.Join(base, "stat")); err == nil {
			// pid (comm) state ppid ...: comm may hold spaces and parens.
			if i := bytes.LastIndexByte(stat, ')'); i >= 0 {
				if f := strings.Fields(string(stat[i+1:])); len(f) > 1 {
					p.PPID, _ = strconv.Atoi(f[1])
				}
			}
		}
		if cmdline, err := os.ReadFile(filepath.Join(base, "cmdline")); err == nil && len(cmdline) > 0 {
			p.Args = strings.Split(strings.TrimRight(string(cmdline), "\x00"), "\x00")
		}
		p.Name = progName(p.Args)
		if comm, err := os.ReadFile(filepath.Join(base, "comm")); p.Name == "" && err == nil {
			p.Name = strings.TrimSpace(string(comm))
		}
		out = append(out, p)
	}
	return out, nil
}

// lsofProcs reads working directories from lsof and names and parents
// from ps.
func lsofProcs() ([]Proc, error) {
	ctx, cancel := context.WithTimeout(context.Background(), procTimeout)
	defer cancel()
	// lsof exits 1 when it cannot read some processes; its output still
	// lists the rest.
	out, err := exec.CommandContext(ctx, "lsof", "-n", "-P", "-w", "-d", "cwd", "-F", "pn").Output()
	if err != nil && len(out) == 0 {
		return nil, fmt.Errorf("lsof: %w", err)
	}
	cwds := map[int]string{}
	pid := 0
	sc := bufio.NewScanner(bytes.NewReader(out))
	for sc.Scan() {
		line := sc.Text()
		switch {
		case strings.HasPrefix(line, "p"):
			pid, _ = strconv.Atoi(line[1:])
		case strings.HasPrefix(line, "n") && pid > 0:
			cwds[pid] = line[1:]
		}
	}
	psOut, err := exec.CommandContext(ctx, "ps", "-axo", "pid=,ppid=,args=").Output()
	if err != nil {
		return nil, fmt.Errorf("ps: %w", err)
	}
	var procs []Proc
	sc = bufio.NewScanner(bytes.NewReader(psOut))
	sc.Buffer(make([]byte, 64*1024), 1024*1024)
	for sc.Scan() {
		f := strings.Fields(sc.Text())
		if len(f) < 3 {
			continue
		}
		pid, err := strconv.Atoi(f[0])
		if err != nil {
			continue
		}
		cwd, ok := cwds[pid]
		if !ok {
			continue
		}
		ppid, _ := strconv.Atoi(f[1])
		procs = append(procs, Proc{PID: pid, PPID: ppid, Name: progName(f[2:]), Args: f[2:], Cwd: cwd})
	}
	if err := sc.Err(); err != nil {
		return nil, errors.Join(errors.New("read ps output"), err)
	}
	return procs, nil
}

func progName(args []string) string {
	if len(args) == 0 {
		return ""
	}
	return filepath.Base(args[0])
}
