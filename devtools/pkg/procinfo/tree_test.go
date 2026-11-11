package procinfo

import (
	"slices"
	"strings"
	"testing"
	"time"
)

func proc(pid, ppid int, rss int64, age time.Duration, argv ...string) Process {
	return Process{PID: pid, PPID: ppid, RSS: rss, Age: age, Name: argv[0], Argv: argv}
}

func pids(procs []Process) []int {
	out := make([]int, len(procs))
	for i, p := range procs {
		out[i] = p.PID
	}
	return out
}

func TestTreeIsRootsAndDescendants(t *testing.T) {
	t.Parallel()

	procs := []Process{
		proc(1, 0, 10, time.Hour, "launchd"),
		proc(100, 1, 300, time.Hour, "claude"),
		proc(101, 100, 20, time.Hour, "gopls"),
		proc(102, 101, 5, time.Hour, "gopls-helper"),
		proc(103, 1, 7, time.Hour, "caffeinate"),
		proc(200, 1, 999, time.Hour, "other"),
		// A pid that is its own parent must not loop.
		proc(300, 300, 1, time.Hour, "self-parent"),
	}
	got := Tree(procs, []int{100, 103, 404, 100})
	if want := []int{100, 101, 102, 103}; !slices.Equal(pids(got), want) {
		t.Fatalf("Tree() = %v, want %v", pids(got), want)
	}
	if rss := SumRSS(got); rss != 332 {
		t.Fatalf("SumRSS() = %d, want 332", rss)
	}
	if got := Tree(procs, []int{300}); !slices.Equal(pids(got), []int{300}) {
		t.Fatalf("Tree(self-parent) = %v", pids(got))
	}
}

func TestJobsAreOldNonHelperChildrenOfTheRoots(t *testing.T) {
	t.Parallel()

	procs := []Process{
		proc(100, 1, 0, time.Hour, "claude"),
		proc(101, 100, 0, time.Hour, "sh", "-c", "bazel build //..."),
		proc(102, 101, 0, time.Hour, "bazel", "build"),
		proc(103, 100, 0, 2*time.Second, "sh", "-c", "ls"),
		proc(104, 100, 0, time.Hour, "/usr/bin/node", "/opt/github-mcp-server/index.js"),
		proc(105, 100, 0, -1, "tail", "-f", "log"),
		// A foreground process is never a job of another.
		proc(106, 100, 0, time.Hour, "caffeinate", "-i"),
	}
	got := Jobs(procs, []int{100, 106}, DefaultJobMinAge, DefaultHelpers)
	if want := []int{101, 105}; !slices.Equal(pids(got), want) {
		t.Fatalf("Jobs() = %v, want %v", pids(got), want)
	}
}

func TestIsJob(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		p    Process
		want bool
	}{
		{"old", proc(1, 0, 0, time.Minute, "sleep", "600"), true},
		{"young", proc(1, 0, 0, 9*time.Second, "sleep", "600"), false},
		{"unknown age counts as old", proc(1, 0, 0, -1, "sleep", "600"), true},
		{"helper", proc(1, 0, 0, time.Hour, "gopls"), false},
		{"mcp glob", proc(1, 0, 0, time.Hour, "node", "/x/slack-mcp"), false},
	}
	for _, tt := range tests {
		if got := IsJob(tt.p, DefaultJobMinAge, DefaultHelpers); got != tt.want {
			t.Errorf("%s: IsJob() = %v, want %v", tt.name, got, tt.want)
		}
	}
}

func TestMatchPattern(t *testing.T) {
	t.Parallel()

	p := proc(1, 0, 0, 0, "/usr/bin/node", "/opt/x/github-mcp-server/index.js")
	p.Name = "node"
	tests := []struct {
		pattern string
		want    bool
	}{
		{"node", true},
		{"*mcp-server*", true},
		{"*/opt/x/*", true},
		{"nod", true},
		{"nodx", false},
		{"python*", false},
		{"nod[ex]", true},
		{"nod[!e]", false},
	}
	for _, tt := range tests {
		if got := MatchPattern(tt.pattern, p); got != tt.want {
			t.Errorf("MatchPattern(%q) = %v, want %v", tt.pattern, got, tt.want)
		}
	}
	if pattern, ok := MatchingPattern([]string{"python*", "*mcp-server*"}, p); !ok || pattern != "*mcp-server*" {
		t.Fatalf("MatchingPattern() = %q, %v", pattern, ok)
	}
}

func TestCommandLine(t *testing.T) {
	t.Parallel()

	if got := CommandLine(proc(1, 0, 0, 0, "sh", "-c", "echo 'hi'", "")); got != `sh -c "echo 'hi'" ""` {
		t.Fatalf("CommandLine() = %q", got)
	}
	if got := CommandLine(proc(1, 0, 0, 0, "echo", "it's")); got != "echo it's" {
		t.Fatalf("CommandLine() = %q, want quotes without whitespace left alone", got)
	}
	if got := CommandLine(Process{Name: "kworker"}); got != "kworker" {
		t.Fatalf("CommandLine() = %q, want the name without argv", got)
	}
	long := CommandLine(proc(1, 0, 0, 0, "sh", strings.Repeat("x", 300)))
	if len([]rune(long)) != MaxCommandLine+3 || !strings.HasSuffix(long, "...") {
		t.Fatalf("CommandLine() = %q, want cut to %d runes", long, MaxCommandLine)
	}
}
