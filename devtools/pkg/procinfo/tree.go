package procinfo

import (
	"slices"
	"strconv"
	"strings"
)

// Children maps each pid to its children, lowest pid first.
func Children(procs []Process) map[int][]Process {
	out := make(map[int][]Process)
	for _, p := range SortedByPID(procs) {
		if p.PID != p.PPID {
			out[p.PPID] = append(out[p.PPID], p)
		}
	}
	return out
}

// SortedByPID is a copy of procs, lowest pid first.
func SortedByPID(procs []Process) []Process {
	out := slices.Clone(procs)
	slices.SortFunc(out, func(a, b Process) int { return a.PID - b.PID })
	return out
}

// Tree is the processes in procs that are one of roots or descend from
// one, each once, lowest pid first. A root not in procs is skipped.
func Tree(procs []Process, roots []int) []Process {
	byPID := make(map[int]Process, len(procs))
	for _, p := range procs {
		byPID[p.PID] = p
	}
	children := Children(procs)
	seen := make(map[int]bool)
	var out []Process
	stack := slices.Clone(roots)
	for len(stack) > 0 {
		pid := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		p, ok := byPID[pid]
		if !ok || seen[pid] {
			continue
		}
		seen[pid] = true
		out = append(out, p)
		for _, c := range children[pid] {
			stack = append(stack, c.PID)
		}
	}
	return SortedByPID(out)
}

// SumRSS is the total RSS of procs in bytes.
func SumRSS(procs []Process) int64 {
	var total int64
	for _, p := range procs {
		total += p.RSS
	}
	return total
}

// MaxCommandLine caps the length of CommandLine.
const MaxCommandLine = 160

// CommandLine is argv joined by spaces, quoting arguments with
// whitespace, and cut to MaxCommandLine runes. Without argv it is the
// name.
func CommandLine(p Process) string {
	if len(p.Argv) == 0 {
		return p.Name
	}
	parts := make([]string, len(p.Argv))
	for i, arg := range p.Argv {
		if arg == "" || strings.ContainsAny(arg, " \t\n") {
			arg = strconv.Quote(arg)
		}
		parts[i] = arg
	}
	line := strings.Join(parts, " ")
	if r := []rune(line); len(r) > MaxCommandLine {
		line = string(r[:MaxCommandLine]) + "..."
	}
	return line
}
