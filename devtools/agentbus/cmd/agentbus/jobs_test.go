package main

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
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

func TestJobsCLIGateHandsTheJobToTheUser(t *testing.T) {
	env := jobsWorld(t)
	mustCmd(t, []string{"jobs", "enqueue", "--kind", "review", "--subject", "o/r#1", "--executor", "tab"}, env)
	mustCmd(t, []string{"jobs", "claim", "--runner", "tiger"}, env)
	if code, _, stderr := runCmd([]string{"jobs", "gate", "j-1", "--runner", "lion", "--note", "dialog"}, env); code != 1 || !strings.Contains(stderr, "held by tiger") {
		t.Fatalf("gate by another runner = %d %q", code, stderr)
	}
	stdout, _ := mustCmd(t, []string{"jobs", "gate", "j-1", "--runner", "tiger", "--note", "the target pane shows a permission prompt"}, env)
	if stdout != "j-1 gated\n" {
		t.Fatalf("gate = %q", stdout)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "show", "j-1"}, env)
	if !strings.Contains(stdout, "state: gated") || !strings.Contains(stdout, "note: the target pane shows a permission prompt") {
		t.Fatalf("show =\n%s", stdout)
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
		{"jobs", "gate", "j-1", "--runner", "tiger"},
		{"jobs", "gate", "j-1", "--note", "dialog"},
		{"jobs", "park", "j-1", "--runner", "tiger", "--saved", "park/x", "--note", "n"},
		{"jobs", "park", "j-1", "--runner", "tiger", "--decide", "d1", "--note", "n"},
		{"jobs", "park", "j-1", "--runner", "tiger", "--decide", "d1", "--saved", "park/x"},
		{"jobs", "park", "j-1", "--decide", "d1", "--saved", "park/x", "--note", "n"},
		{"jobs", "park", "j-1", "--runner", "tiger", "--ask-msg", "m-1", "--note", "n"},
		{"jobs", "park", "j-1", "--runner", "tiger", "--ask-msg", "m-1", "--decide", "d1", "--saved", "park/x", "--note", "n"},
		{"jobs", "regate", "j-1"},
		{"jobs", "regate", "--decide", "d1"},
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

func TestJobsCLIParkNeedsACleanCheckoutAndASavedRef(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}
	env := jobsWorld(t)
	repo, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	git := func(args ...string) {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", repo, "-c", "user.name=t", "-c", "user.email=t@example.com"}, args...)...) //nolint:gosec // G204: test git in a temp repo
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %v\n%s", args, err, out)
		}
	}
	git("init", "-q", "-b", "main")
	writeFile(t, filepath.Join(repo, "a.txt"), []byte("a\n"))
	git("add", "a.txt")
	git("commit", "-q", "-m", "init")

	mustCmd(t, []string{"jobs", "enqueue", "--kind", "review", "--subject", "o/r#1", "--checkout", repo, "--item", "c1", "--item", "c2"}, env)
	mustCmd(t, []string{"jobs", "claim", "--runner", "tiger"}, env)
	park := []string{"jobs", "park", "j-1", "--runner", "tiger", "--decide", "d9", "--saved", "park/r1", "--note", "c1 answered"}
	ask := []string{"jobs", "park", "j-1", "--runner", "tiger", "--ask-msg", "m-1", "--saved", "park/r1", "--note", "c1 answered"}

	for _, args := range [][]string{park, ask} {
		if code, _, stderr := runCmd(args, env); code != 1 || !strings.Contains(stderr, "park/r1") {
			t.Fatalf("%q with no such branch = %d %q", args, code, stderr)
		}
	}
	git("branch", "park/r1")
	writeFile(t, filepath.Join(repo, "a.txt"), []byte("half done\n"))
	for _, args := range [][]string{park, ask} {
		if code, _, stderr := runCmd(args, env); code != 1 || !strings.Contains(stderr, "uncommitted") {
			t.Fatalf("%q of a dirty checkout = %d %q", args, code, stderr)
		}
	}
	stdout, _ := mustCmd(t, []string{"jobs", "show", "j-1"}, env)
	if !strings.Contains(stdout, "state: running") || !strings.Contains(stdout, "lease: local:"+repo) {
		t.Fatalf("a refused park changed the job:\n%s", stdout)
	}

	git("checkout", "-q", "a.txt")
	if code, _, stderr := runCmd(ask, env); code != 1 || !strings.Contains(stderr, "m-1") {
		t.Fatalf("park on a message the bus does not have = %d %q", code, stderr)
	}
	stdout, _ = mustCmd(t, append(park, "--item", "c2"), env)
	if stdout != "j-1 done\nj-2 gated (decision d9)\n" {
		t.Fatalf("partial park = %q", stdout)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "show", "j-2"}, env)
	for _, want := range []string{"state: gated (decision d9, ", "saved: park/r1", "resume: c1 answered", "items: c2", "parent: j-1"} {
		if !strings.Contains(stdout, want) {
			t.Errorf("show of the child lacks %q:\n%s", want, stdout)
		}
	}
	stdout, _ = mustCmd(t, []string{"lease", "status", "--all"}, env)
	if !strings.Contains(stdout, "local:"+repo+": free") {
		t.Fatalf("the park kept the lease:\n%s", stdout)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "enqueue", "--kind", "review", "--subject", "o/r#1", "--head", "def", "--item", "c2"}, env)
	if stdout != "j-2 gated (duplicate)\n" {
		t.Fatalf("enqueue of a parked item at a new head = %q", stdout)
	}
	if code, _, stderr := runCmd([]string{"jobs", "regate", "j-2", "--decide", "d10"}, env); code != 1 || !strings.Contains(stderr, "agent bus message") {
		t.Fatalf("regate of a job parked on a decide item = %d %q", code, stderr)
	}
}

func TestJobsStateLabelShowsTheParkTargetAndAge(t *testing.T) {
	at := time.Date(2026, 11, 16, 9, 0, 0, 0, time.UTC)
	now := at.Add(3*time.Hour + 20*time.Minute)
	for _, tt := range []struct {
		j    jobs.Job
		want string
	}{
		{jobs.Job{State: jobs.StateParked, ParkedOn: "m-1", AskMsg: "m-1", AskTo: "manager", ParkedAt: at}, "parked (asked manager, 3h20m)"},
		{jobs.Job{State: jobs.StateGated, ParkedOn: "d9", AskMsg: "m-1", AskTo: "manager", ParkedAt: at}, "gated (decision d9, 3h20m)"},
		{jobs.Job{State: jobs.StateGated, ParkedOn: "d9"}, "gated (decision d9)"},
		{jobs.Job{State: jobs.StateReady}, "ready"},
	} {
		if got := stateLabel(tt.j, now); got != tt.want {
			t.Errorf("stateLabel = %q, want %q", got, tt.want)
		}
	}
}

func TestJobsCLIBudget(t *testing.T) {
	env := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(startDaemon(t, dirs, daemon.Config{
		JobPolicy: jobs.Policy{Default: jobs.GateAuto},
		Jobs: jobs.Config{
			Budget: jobs.BudgetConfig{Limit: 100, Window: time.Hour, Floor: 20},
			Kinds:  map[string]jobs.Kind{"merge": {Cost: 5, BelowFloor: true}},
		},
	}))
	stdout, _ := mustCmd(t, []string{"jobs", "stats"}, env)
	if !strings.Contains(stdout, "budget: 100 of 100 left, floor 20\n") {
		t.Fatalf("stats = %q", stdout)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "reserve", "--calls", "75"}, env)
	if !strings.HasPrefix(stdout, "reserved 75: 25 of 100 left, floor 20, resets ") {
		t.Fatalf("reserve = %q", stdout)
	}
	code, _, stderr := runCmd([]string{"jobs", "reserve", "--calls", "10"}, env)
	if code != 3 || !strings.Contains(stderr, "10 calls do not fit: 25 of 100 left, floor 20, resets ") {
		t.Fatalf("reserve below the floor = %d %q, want 3", code, stderr)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "reserve", "--calls", "10", "--kind", "merge", "--json"}, env)
	var r jobs.Reply
	if err := json.Unmarshal([]byte(stdout), &r); err != nil || !r.Reserved || r.Budget.Remaining != 15 {
		t.Fatalf("reserve --kind merge --json = %q, %v", stdout, err)
	}
	stdout, _ = mustCmd(t, []string{"jobs", "stats", "--json"}, env)
	var st jobs.Stats
	if err := json.Unmarshal([]byte(stdout), &st); err != nil || st.Budget == nil || st.Budget.Used != 85 {
		t.Fatalf("stats --json = %q, %v", stdout, err)
	}
	if code, _, _ := runCmd([]string{"jobs", "reserve"}, env); code != 2 {
		t.Fatalf("reserve without --calls = %d, want 2", code)
	}
}

func TestJobsStatusGolden(t *testing.T) {
	now := time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)
	var mu sync.Mutex
	advance := func(d time.Duration) {
		mu.Lock()
		defer mu.Unlock()
		now = now.Add(d)
	}
	env := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(startDaemon(t, dirs, daemon.Config{
		Now: func() time.Time {
			mu.Lock()
			defer mu.Unlock()
			return now
		},
		JobPolicy: jobs.Policy{Default: jobs.GateAuto, Kinds: map[string]jobs.Gate{"merge": jobs.GateAsk}},
	}))
	mustCmd(t, []string{"jobs", "enqueue", "--kind", "rebase", "--subject", "o/r#1", "--checkout", "/w/a"}, env)
	mustCmd(t, []string{"jobs", "claim", "--runner", "tiger-runner", "--machine", "tiger"}, env)
	advance(time.Minute)
	mustCmd(t, []string{"jobs", "enqueue", "--kind", "merge", "--subject", "o/r#2"}, env)
	advance(9 * time.Minute)
	mustCmd(t, []string{"jobs", "enqueue", "--kind", "rebase", "--subject", "o/r#3", "--machine", "tiger"}, env)
	advance(5 * time.Minute)

	text, _ := mustCmd(t, []string{"jobs", "status"}, env)
	js, _ := mustCmd(t, []string{"jobs", "status", "--json"}, env)
	got := text + js
	golden := filepath.Join("testdata", "jobs_status.golden")
	if os.Getenv("UPDATE_GOLDEN") != "" {
		writeFile(t, golden, []byte(got))
	}
	want, err := os.ReadFile(golden)
	if err != nil {
		t.Fatal(err)
	}
	if got != string(want) {
		t.Fatalf("jobs status =\n%s\nwant\n%s", got, want)
	}
}

func TestJobsStatusEmpty(t *testing.T) {
	env := jobsWorld(t)
	stdout, _ := mustCmd(t, []string{"jobs", "status"}, env)
	if stdout != "no jobs or leases\n" {
		t.Fatalf("status = %q", stdout)
	}
}
