package procinfo

import (
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"time"
)

// The background-job rule: a child of an agent's foreground process is
// a long-lived job when it has run at least a minimum age (an unknown
// age counts as old) and matches none of the helper patterns. Only
// direct children count, so one job is one process however many it
// starts, and a helper's own children are never looked at.

// DefaultJobMinAge is how old a child must be to count as a job.
const DefaultJobMinAge = 10 * time.Second

// DefaultHelpers are an agent's usual helpers, which are never jobs:
// long-lived servers, the status-line command it reruns on each UI
// refresh, and the Claude Code sandbox's socat bridges
// (claude-http-<hex>.sock and other claude-<kind>-<hex>.sock), which
// live as long as the session. See MatchPattern for the syntax.
var DefaultHelpers = []string{
	"caffeinate", "gopls", "rust-analyzer", "clangd", "pyright*",
	"*language-server*", "*langserver*",
	"*mcp-server*", "*mcp_server*", "*modelcontextprotocol*", "*-mcp", "*-mcp *",
	"statusline", "status-line", "claude-hud",
	"claude-http-", "*UNIX-LISTEN:*claude-*.sock*",
}

// OldEnough reports whether p has run at least minAge. An unknown age
// counts as old.
func OldEnough(p Process, minAge time.Duration) bool {
	return p.Age < 0 || p.Age >= minAge
}

// IsJob reports whether p, a child of an agent, is a long-lived job.
func IsJob(p Process, minAge time.Duration, helpers []string) bool {
	if _, helper := MatchingPattern(helpers, p); helper {
		return false
	}
	return OldEnough(p, minAge)
}

// Jobs are the children of roots in procs that are jobs, lowest pid
// first. A root is never a job of another root.
func Jobs(procs []Process, roots []int, minAge time.Duration, helpers []string) []Process {
	children := Children(procs)
	var out []Process
	for _, root := range slices.Compact(slices.Sorted(slices.Values(roots))) {
		for _, c := range children[root] {
			if !slices.Contains(roots, c.PID) && IsJob(c, minAge, helpers) {
				out = append(out, c)
			}
		}
	}
	return SortedByPID(out)
}

// MatchingPattern is the first pattern that matches p (see
// MatchPattern).
func MatchingPattern(patterns []string, p Process) (string, bool) {
	for _, pattern := range patterns {
		if MatchPattern(pattern, p) {
			return pattern, true
		}
	}
	return "", false
}

// MatchPattern reports whether pattern appears anywhere in p's name,
// argv[0] base, or whole command line (argv joined by spaces). A
// pattern with *, ?, or [ is a glob, where * also matches /.
func MatchPattern(pattern string, p Process) bool {
	fields := []string{p.Name, Argv0Base(p), strings.Join(p.Argv, " ")}
	if !strings.ContainsAny(pattern, "*?[") {
		return slices.ContainsFunc(fields, func(f string) bool { return strings.Contains(f, pattern) })
	}
	re := Glob(pattern, false)
	return slices.ContainsFunc(fields, re.MatchString)
}

// Argv0Base is the base name of p's argv[0], or empty.
func Argv0Base(p Process) string {
	if len(p.Argv) == 0 {
		return ""
	}
	return filepath.Base(p.Argv[0])
}

// Glob compiles a glob: * is any run (including /), ? one rune, and
// [...] a class ([!...] negated). Anchored matches the whole string;
// otherwise the glob may match anywhere in it.
func Glob(pattern string, anchored bool) *regexp.Regexp {
	var b strings.Builder
	if anchored {
		b.WriteString("^")
	}
	runes := []rune(pattern)
	for i := 0; i < len(runes); i++ {
		switch r := runes[i]; r {
		case '*':
			b.WriteString(".*")
		case '?':
			b.WriteString(".")
		case '[':
			if class, n := globClass(runes[i:]); n > 0 {
				b.WriteString(class)
				i += n - 1
				continue
			}
			b.WriteString(regexp.QuoteMeta("["))
		default:
			b.WriteString(regexp.QuoteMeta(string(r)))
		}
	}
	if anchored {
		b.WriteString("$")
	}
	re, err := regexp.Compile(b.String())
	if err != nil {
		return regexp.MustCompile(regexp.QuoteMeta(pattern))
	}
	return re
}

// globClass converts the class at the start of runes ("[...]") to a
// regexp class, and returns how many runes it used, or 0 when the
// class is not closed.
func globClass(runes []rune) (string, int) {
	var b strings.Builder
	b.WriteString("[")
	i := 1
	if i < len(runes) && (runes[i] == '!' || runes[i] == '^') {
		b.WriteString("^")
		i++
	}
	for start := i; i < len(runes); i++ {
		switch r := runes[i]; {
		case r == ']' && i > start:
			b.WriteString("]")
			return b.String(), i + 1
		case r == '\\' && i+1 < len(runes):
			i++
			b.WriteString(regexp.QuoteMeta(string(runes[i])))
		case r == '[' || r == ']' || r == '\\' || r == '^':
			b.WriteString(`\` + string(r))
		default:
			b.WriteRune(r)
		}
	}
	return "", 0
}
