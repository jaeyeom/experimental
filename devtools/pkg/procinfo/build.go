package procinfo

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// The build-tool rule: a process from a build-tool list, matching none
// of the ignore patterns, whose working directory is inside a directory.
// prsync's gate and agentbus hibernate share it.

// DefaultBuildTools are the build-tool client names, matched by
// MatchName.
var DefaultBuildTools = []string{"bazel", "bazelisk", "make", "go", "cargo", "gradle", "npm", "pnpm"}

// DefaultBuildToolIgnore are the patterns (see MatchPattern) of
// build-tool processes that are servers, not clients.
var DefaultBuildToolIgnore = []string{"*--workspace_directory=*", "*A-server.jar*", "*--server*"}

// CwdFunc reads one process's working directory.
type CwdFunc func(ctx context.Context, pid int) (string, error)

// MatchName reports whether a pattern matches p's whole name or argv[0]
// base, exactly or as a glob.
func MatchName(patterns []string, p Process) bool {
	for _, pattern := range patterns {
		re := Glob(pattern, true)
		if re.MatchString(p.Name) || re.MatchString(Argv0Base(p)) {
			return true
		}
	}
	return false
}

// Inside reports whether path is dir or a directory inside it.
func Inside(path, dir string) bool {
	if path == "" || dir == "" {
		return false
	}
	rel, err := filepath.Rel(filepath.Clean(dir), filepath.Clean(path))
	if err != nil {
		return false
	}
	return rel == "." || !strings.HasPrefix(rel, "..")
}

// BuildTools are the processes in procs, lowest pid first and not in
// skip, that match tools by name, match none of ignore, and whose
// working directory is inside dir. Each has Cwd set. A process whose
// cwd cannot be read is left out.
func BuildTools(ctx context.Context, procs []Process, tools, ignore []string, skip map[int]struct{}, dir string, cwd CwdFunc) []Process {
	var out []Process
	for _, p := range SortedByPID(procs) {
		if _, ok := skip[p.PID]; ok {
			continue
		}
		if !MatchName(tools, p) {
			continue
		}
		if _, ignored := MatchingPattern(ignore, p); ignored {
			continue
		}
		where := p.Cwd
		if where == "" {
			var err error
			if where, err = cwd(ctx, p.PID); err != nil {
				continue
			}
		}
		if !Inside(where, dir) {
			continue
		}
		p.Cwd = where
		out = append(out, p)
	}
	return out
}

// Ancestors are pid and its ancestors in procs.
func Ancestors(procs []Process, pid int) map[int]struct{} {
	out := make(map[int]struct{})
	if pid <= 0 {
		return out
	}
	parent := make(map[int]int, len(procs))
	for _, p := range procs {
		parent[p.PID] = p.PPID
	}
	for pid > 0 {
		if _, seen := out[pid]; seen {
			break
		}
		out[pid] = struct{}{}
		pid = parent[pid]
	}
	return out
}

// LocalCwd reads a local process's working directory: from /proc where
// it exists (Linux), else with lsof.
func LocalCwd(ctx context.Context, pid int) (string, error) {
	if _, err := os.Stat("/proc/self/cwd"); err == nil {
		cwd, err := os.Readlink(filepath.Join("/proc", strconv.Itoa(pid), "cwd"))
		if err != nil {
			return "", fmt.Errorf("cwd of %d: %w", pid, err)
		}
		return cwd, nil
	}
	out, err := Run(ctx, "lsof", "-a", "-p", strconv.Itoa(pid), "-d", "cwd", "-Fn")
	if err != nil {
		return "", err
	}
	cwd := ParseLsofCwd(out)
	if cwd == "" {
		return "", fmt.Errorf("lsof: no cwd for %d", pid)
	}
	return cwd, nil
}

// ParseLsofCwd reads the n (name) field of `lsof -Fn` output.
func ParseLsofCwd(out string) string {
	for _, line := range strings.Split(out, "\n") {
		if name, ok := strings.CutPrefix(line, "n"); ok {
			return name
		}
	}
	return ""
}
