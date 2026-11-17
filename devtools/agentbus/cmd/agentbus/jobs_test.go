package main

import (
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func jobsWorld(t *testing.T) []string {
	t.Helper()
	env := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(startDaemon(t, dirs, daemon.Config{JobPolicy: jobs.Policy{Default: jobs.GateAuto}}))
	return env
}

func TestJobsCLIFlow(t *testing.T) {
	env := jobsWorld(t)
	stdout, _ := mustCmd(t, []string{"jobs", "enqueue", "--kind", "rebase", "--subject", "o/r#1", "--head", "abc", "--priority", "2"}, env)
	if stdout != "j-1 ready\n" {
		t.Fatalf("enqueue = %q", stdout)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "enqueue", "--kind", "rebase", "--subject", "o/r#1", "--head", "abc"}, env)
	if stdout != "j-1 ready (duplicate)\n" {
		t.Fatalf("duplicate enqueue = %q", stdout)
	}

	mustCmd(t, []string{"jobs", "hold", "--note", "release day"}, env)
	code, _, stderr := runCmd([]string{"jobs", "claim", "--runner", "tiger"}, env)
	if code != 4 || !strings.Contains(stderr, "release day") {
		t.Fatalf("claim under hold = %d %q, want 4 with the note", code, stderr)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "stats"}, env)
	if !strings.Contains(stdout, "held: yes (release day)") || !strings.Contains(stdout, "ready 1") {
		t.Fatalf("stats = %q", stdout)
	}
	mustCmd(t, []string{"jobs", "unhold"}, env)

	if code, _, _ := runCmd([]string{"jobs", "claim", "--runner", "tiger", "--kind", "merge"}, env); code != 3 {
		t.Fatalf("claim of another kind = %d, want 3 (none ready)", code)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "claim", "--runner", "tiger", "--kind", "merge", "--kind", "rebase", "--json"}, env)
	var claimed jobs.Job
	if err := json.Unmarshal([]byte(stdout), &claimed); err != nil {
		t.Fatalf("claim --json: %v\n%s", err, stdout)
	}
	if claimed.ID != "j-1" || claimed.State != jobs.StateRunning || claimed.Holder != "tiger" {
		t.Fatalf("claimed = %+v", claimed)
	}
	mustCmd(t, []string{"jobs", "heartbeat", "j-1", "--runner", "tiger"}, env)
	if code, _, stderr := runCmd([]string{"jobs", "done", "j-1", "--runner", "lion"}, env); code != 1 || !strings.Contains(stderr, "held by tiger") {
		t.Fatalf("done by another runner = %d %q", code, stderr)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "done", "j-1", "--runner", "tiger", "--note", "rebased onto main"}, env)
	if stdout != "j-1 done\n" {
		t.Fatalf("done = %q", stdout)
	}

	stdout, _ = mustCmd(t, []string{"jobs", "ls"}, env)
	if strings.Contains(stdout, "j-1") {
		t.Fatalf("ls lists a done job:\n%s", stdout)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "ls", "--all"}, env)
	if !strings.Contains(stdout, "j-1") || !strings.Contains(stdout, "done") || !strings.Contains(stdout, "o/r#1") {
		t.Fatalf("ls --all =\n%s", stdout)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "show", "j-1"}, env)
	for _, want := range []string{"id: j-1", "state: done", "kind: rebase", "subject: o/r#1", "attempts: 1/1", "note: rebased onto main"} {
		if !strings.Contains(stdout, want) {
			t.Errorf("show lacks %q:\n%s", want, stdout)
		}
	}
	stdout, _ = mustCmd(t, []string{"jobs", "log", "j-1"}, env)
	lines := strings.Split(strings.TrimSpace(stdout), "\n")
	if len(lines) != 4 || !strings.Contains(lines[1], "queued -> ready") || !strings.Contains(lines[3], "running -> done by tiger: rebased onto main") {
		t.Fatalf("log =\n%s", stdout)
	}
}

func TestJobsCLIFailWithExit(t *testing.T) {
	env := jobsWorld(t)
	mustCmd(t, []string{"jobs", "enqueue", "--kind", "ci", "--subject", "o/r#1"}, env)
	mustCmd(t, []string{"jobs", "claim", "--runner", "tiger"}, env)
	stdout, _ := mustCmd(t, []string{"jobs", "fail", "j-1", "--runner", "tiger", "--exit", "75", "--note", "flaky"}, env)
	if stdout != "j-1 failed\n" {
		t.Fatalf("fail = %q", stdout)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "show", "j-1", "--json"}, env)
	var j jobs.Job
	if err := json.Unmarshal([]byte(stdout), &j); err != nil {
		t.Fatal(err)
	}
	if j.Exit == nil || *j.Exit != 75 {
		t.Fatalf("exit = %v", j.Exit)
	}
}

func TestJobsCLIUsage(t *testing.T) {
	env := jobsWorld(t)
	for _, args := range [][]string{
		{"jobs"},
		{"jobs", "bogus"},
		{"jobs", "enqueue", "--subject", "o/r#1"},
		{"jobs", "claim"},
		{"jobs", "done"},
		{"jobs", "heartbeat", "j-1"},
		{"jobs", "fail", "j-1", "--exit", "x"},
		{"jobs", "show", "j-1", "j-2"},
		{"jobs", "enqueue", "--kind", "a", "--subject", "s", "--gate", "auto"},
	} {
		if code, _, _ := runCmd(args, env); code != 2 {
			t.Errorf("%q = %d, want 2", args, code)
		}
	}
	if code, _, stderr := runCmd([]string{"jobs", "show", "j-9"}, env); code != 1 || !strings.Contains(stderr, "no such job") {
		t.Errorf("show of a missing job = %d %q", code, stderr)
	}
}

func TestJobsLogFollowEndsWhenTheJobIsFinal(t *testing.T) {
	env := jobsWorld(t)
	mustCmd(t, []string{"jobs", "enqueue", "--kind", "rebase", "--subject", "o/r#1"}, env)
	mustCmd(t, []string{"jobs", "claim", "--runner", "tiger"}, env)
	old := jobsLogPoll
	jobsLogPoll = 10 * time.Millisecond
	t.Cleanup(func() { jobsLogPoll = old })
	done := make(chan string, 1)
	go func() {
		_, stdout, _ := runCmd([]string{"jobs", "log", "j-1", "--follow"}, env)
		done <- stdout
	}()
	time.Sleep(50 * time.Millisecond)
	mustCmd(t, []string{"jobs", "done", "j-1", "--runner", "tiger"}, env)
	select {
	case stdout := <-done:
		if !strings.Contains(stdout, "running -> done") || strings.Count(stdout, "queued -> ready") != 1 {
			t.Fatalf("log --follow =\n%s", stdout)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("log --follow did not end at done")
	}
}

func TestJobsCLIClaimHoldsTheCheckoutLeaseUntilDone(t *testing.T) {
	env := jobsWorld(t)
	mustCmd(t, []string{"jobs", "enqueue", "--kind", "rebase", "--subject", "o/r#1", "--checkout", "/src/repo"}, env)
	mustCmd(t, []string{"jobs", "claim", "--runner", "tiger-runner"}, env)
	stdout, _ := mustCmd(t, []string{"jobs", "show", "j-1"}, env)
	if !strings.Contains(stdout, "lease: local:/src/repo") {
		t.Fatalf("show lacks the lease:\n%s", stdout)
	}
	stdout, _ = mustCmd(t, []string{"lease", "status", "--all"}, env)
	if !strings.Contains(stdout, "local:/src/repo: held") || !strings.Contains(stdout, "holder: job:j-1") || !strings.Contains(stdout, "runner tiger-runner on local") {
		t.Fatalf("lease status =\n%s", stdout)
	}
	mustCmd(t, []string{"jobs", "done", "j-1", "--runner", "tiger-runner"}, env)
	stdout, _ = mustCmd(t, []string{"lease", "status", "--all"}, env)
	if !strings.Contains(stdout, "local:/src/repo: free") {
		t.Fatalf("lease status after done =\n%s", stdout)
	}
}
