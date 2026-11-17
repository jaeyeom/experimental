package runner

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
)

const (
	runnerName = "tiger-runner"
	machine    = "tiger"
)

type nopLog struct{}

func (nopLog) Append(eventlog.Event) error { return nil }

type clock struct {
	mu sync.Mutex
	t  time.Time
}

func (c *clock) now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.t
}

func (c *clock) add(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.t = c.t.Add(d)
}

// leases grants each checkout key to one job, like the daemon's lease
// table.
type leases struct {
	mu   sync.Mutex
	held map[string]string
}

func (l *leases) Take(j jobs.Job, m, _ string) (string, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	key := m + ":" + j.Needs.Checkout
	if h, ok := l.held[key]; ok && h != j.ID {
		return "", errors.New(key + " is held by " + h)
	}
	l.held[key] = j.ID
	return key, nil
}

func (l *leases) Free(key, id string) {
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.held[key] == id {
		delete(l.held, key)
	}
}

// storeQueue is the real job store, as the daemon would serve it to
// this runner.
type storeQueue struct {
	s     *jobs.Store
	mu    sync.Mutex
	calls []string
}

func (q *storeQueue) call(c string) {
	q.mu.Lock()
	defer q.mu.Unlock()
	q.calls = append(q.calls, c)
}

func (q *storeQueue) changes() []string {
	q.mu.Lock()
	defer q.mu.Unlock()
	var out []string
	for _, c := range q.calls {
		if !strings.HasPrefix(c, "show") && !strings.HasPrefix(c, "list") && !strings.HasPrefix(c, "held") {
			out = append(out, c)
		}
	}
	return out
}

func (q *storeQueue) Claim(_ context.Context, kinds []string) (*jobs.Job, bool, string, error) {
	q.call("claim " + strings.Join(kinds, ","))
	r, err := q.s.Claim(jobs.Args{Runner: runnerName, Machine: machine, Kinds: kinds})
	return r.Job, r.Held, r.HoldNote, wrap(err)
}

func (q *storeQueue) Heartbeat(_ context.Context, id string) error {
	q.call("heartbeat " + id)
	_, err := q.s.Heartbeat(jobs.Args{ID: id, Runner: runnerName})
	return wrap(err)
}

func (q *storeQueue) Done(_ context.Context, id, note string) error {
	q.call("done " + id)
	_, err := q.s.Done(jobs.Args{ID: id, Runner: runnerName, Note: note})
	return wrap(err)
}

func (q *storeQueue) Fail(_ context.Context, id string, exit *int, note string) error {
	q.call("fail " + id)
	_, err := q.s.Fail(jobs.Args{ID: id, Runner: runnerName, Exit: exit, Note: note})
	return wrap(err)
}

func (q *storeQueue) Gate(_ context.Context, id, note string) error {
	q.call("gate " + id)
	_, err := q.s.Gate(jobs.Args{ID: id, Runner: runnerName, Note: note})
	return wrap(err)
}

func (q *storeQueue) Show(_ context.Context, id string) (jobs.Job, error) {
	q.call("show " + id)
	r, err := q.s.Show(id)
	if err != nil {
		return jobs.Job{}, wrap(err)
	}
	return *r.Job, nil
}

func (q *storeQueue) List(_ context.Context, state jobs.State, kind string) ([]jobs.Job, error) {
	q.call("list")
	r, err := q.s.List(jobs.Args{State: string(state), Kind: kind})
	return r.Jobs, wrap(err)
}

func (q *storeQueue) Held(context.Context) (bool, string, error) {
	q.call("held")
	held, why := q.s.Held()
	return held, why, nil
}

func wrap(err error) error {
	if err != nil {
		return fmt.Errorf("store: %w", err)
	}
	return nil
}

// fakeHost runs no command: each run waits for release and exits with
// the code set for its subject.
type fakeHost struct {
	mu      sync.Mutex
	runs    [][]string
	envs    [][]string
	dirs    []string
	exits   map[string]int
	block   chan struct{}
	dirty   map[string]bool
	free    uint64
	running int
	most    int
}

func (h *fakeHost) Run(_ context.Context, argv []string, dir string, env []string) (int, error) {
	h.mu.Lock()
	h.runs = append(h.runs, argv)
	h.envs = append(h.envs, env)
	h.dirs = append(h.dirs, dir)
	h.running++
	h.most = max(h.most, h.running)
	block := h.block
	code := h.exits[envOf(env, "JOB_SUBJECT")]
	h.mu.Unlock()
	if block != nil {
		<-block
	}
	h.mu.Lock()
	h.running--
	h.mu.Unlock()
	return code, nil
}

func (h *fakeHost) Dirty(_ context.Context, checkout string) (bool, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.dirty[checkout], nil
}

func (h *fakeHost) FreeDisk(string) (uint64, error) { return h.free, nil }

func (h *fakeHost) count() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return len(h.runs)
}

func envOf(env []string, key string) string {
	for _, e := range env {
		if v, ok := strings.CutPrefix(e, key+"="); ok {
			return v
		}
	}
	return ""
}

type prompt struct{ target, key, text string }

type fakePanes struct {
	mu      sync.Mutex
	dialogs map[string]string
	dead    map[string]bool
	prompts []prompt
}

func (p *fakePanes) Live(_ context.Context, target string) (bool, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	return !p.dead[target], nil
}

func (p *fakePanes) Dialog(_ context.Context, target string) (string, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.dialogs[target], nil
}

func (p *fakePanes) Prompt(_ context.Context, target, key, text string) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.prompts = append(p.prompts, prompt{target, key, text})
	return nil
}

type world struct {
	r     *Runner
	q     *storeQueue
	host  *fakeHost
	panes *fakePanes
	clock *clock
	log   *strings.Builder
}

func newWorld(t *testing.T, cfg Config) *world {
	t.Helper()
	c := &clock{t: time.Date(2026, 11, 17, 9, 0, 0, 0, time.UTC)}
	s, err := jobs.Open(t.TempDir(), nopLog{}, c.now, jobs.Config{})
	if err != nil {
		t.Fatal(err)
	}
	s.SetPolicy(jobs.Policy{Default: jobs.GateAuto})
	s.SetLeaser(&leases{held: map[string]string{}})
	for name, k := range cfg.Kinds {
		if k.Parallel == 0 {
			k.Parallel = 1
		}
		if k.Timeout == 0 {
			k.Timeout = DefaultTimeout
		}
		cfg.Kinds[name] = k
	}
	w := &world{
		q: &storeQueue{s: s}, host: &fakeHost{exits: map[string]int{}, dirty: map[string]bool{}},
		panes: &fakePanes{dialogs: map[string]string{}, dead: map[string]bool{}}, clock: c, log: &strings.Builder{},
	}
	w.r = &Runner{Name: runnerName, Machine: machine, Config: cfg, Queue: w.q, Host: w.host, Panes: w.panes, Now: c.now, Log: w.log}
	return w
}

func (w *world) enqueue(t *testing.T, a jobs.Args) jobs.Job {
	t.Helper()
	r, err := w.q.s.Enqueue(a)
	if err != nil {
		t.Fatal(err)
	}
	return *r.Job
}

func (w *world) state(t *testing.T, id string) jobs.Job {
	t.Helper()
	r, err := w.q.s.Show(id)
	if err != nil {
		t.Fatal(err)
	}
	return *r.Job
}

// settle lets every command under way end, reports it, ticks once
// more, and reports what that tick started.
func (w *world) settle() {
	w.host.mu.Lock()
	if w.host.block != nil {
		close(w.host.block)
		w.host.block = nil
	}
	w.host.mu.Unlock()
	w.r.Wait()
	w.r.Tick(context.Background())
	w.r.Wait()
	w.r.Tick(context.Background())
}

func cliKind(parallel int) Kind {
	return Kind{Command: []string{"prsync", "rebase", "{subject}", "{head}"}, Parallel: parallel}
}

func TestCLIExitCodeIsDoneOrFailed(t *testing.T) {
	w := newWorld(t, Config{Kinds: map[string]Kind{"rebase": cliKind(2)}})
	ok := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#1", Head: "abc"})
	bad := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#2", Needs: jobs.Needs{Checkout: "/src/repo"}})
	w.host.exits["o/r#2"] = 3
	w.r.Tick(context.Background())
	w.settle()
	if got := w.state(t, ok.ID); got.State != jobs.StateDone {
		t.Fatalf("exit 0: %s, want done", got.State)
	}
	got := w.state(t, bad.ID)
	if got.State != jobs.StateFailed || got.Exit == nil || *got.Exit != 3 {
		t.Fatalf("exit 3: %s exit %v, want failed with exit 3", got.State, got.Exit)
	}
	if !slices.ContainsFunc(w.host.runs, func(a []string) bool { return slices.Equal(a, []string{"prsync", "rebase", "o/r#1", "abc"}) }) {
		t.Fatalf("runs = %q", w.host.runs)
	}
	i := slices.IndexFunc(w.host.envs, func(e []string) bool { return envOf(e, "JOB_ID") == bad.ID })
	if i < 0 || w.host.dirs[i] != "/src/repo" || envOf(w.host.envs[i], "JOB_RUNNER") != runnerName {
		t.Fatalf("checkout job ran in %q with %q", w.host.dirs, w.host.envs)
	}
}

func TestParallelLimitPerKind(t *testing.T) {
	w := newWorld(t, Config{Kinds: map[string]Kind{"rebase": cliKind(2), "approve": cliKind(1)}})
	for _, s := range []string{"o/r#1", "o/r#2", "o/r#3"} {
		w.enqueue(t, jobs.Args{Kind: "rebase", Subject: s})
	}
	w.enqueue(t, jobs.Args{Kind: "approve", Subject: "o/r#4"})
	w.host.block = make(chan struct{})
	w.r.Tick(context.Background())
	running, _ := w.q.s.List(jobs.Args{State: string(jobs.StateRunning)})
	kinds := map[string]int{}
	for _, j := range running.Jobs {
		kinds[j.Kind]++
	}
	if kinds["rebase"] != 2 || kinds["approve"] != 1 {
		t.Fatalf("running = %v, want 2 rebase and 1 approve", kinds)
	}
	w.settle()
	w.settle()
	if w.host.count() != 4 || w.host.most > 3 {
		t.Fatalf("ran %d, at most %d at once", w.host.count(), w.host.most)
	}
}

func TestCheckoutJobsRunOneAtATime(t *testing.T) {
	w := newWorld(t, Config{Kinds: map[string]Kind{"rebase": cliKind(3)}})
	a := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#1", Needs: jobs.Needs{Checkout: "/src/repo"}})
	b := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#2", Needs: jobs.Needs{Checkout: "/src/repo"}})
	c := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#3", Needs: jobs.Needs{Checkout: "/src/other"}})
	w.host.block = make(chan struct{})
	w.r.Tick(context.Background())
	if w.state(t, a.ID).State != jobs.StateRunning || w.state(t, c.ID).State != jobs.StateRunning {
		t.Fatal("the jobs on free checkouts did not start")
	}
	if got := w.state(t, b.ID).State; got != jobs.StateReady {
		t.Fatalf("second job on /src/repo is %s, want ready while the first runs", got)
	}
	w.settle()
	w.settle()
	if got := w.state(t, b.ID).State; got != jobs.StateDone {
		t.Fatalf("second job on /src/repo is %s after the first, want done", got)
	}
}

func startWorld(t *testing.T, p Pacing) *world {
	t.Helper()
	w := newWorld(t, Config{Start: p, Kinds: map[string]Kind{
		"start-item": {Command: []string{"factory", "start", "--apply", "{subject}"}, Parallel: 5, Start: true},
	}})
	for _, s := range []string{"PRJ-1", "PRJ-2", "PRJ-3"} {
		w.enqueue(t, jobs.Args{Kind: "start-item", Subject: s, Needs: jobs.Needs{Executor: "new-tab"}})
	}
	w.host.free = 50 << 30
	return w
}

func (w *world) running(t *testing.T) int {
	t.Helper()
	r, err := w.q.s.List(jobs.Args{State: string(jobs.StateRunning)})
	if err != nil {
		t.Fatal(err)
	}
	return len(r.Jobs)
}

func TestStartPacing(t *testing.T) {
	t.Run("spacing", func(t *testing.T) {
		w := startWorld(t, Pacing{MinSpacing: 5 * time.Minute})
		w.r.Tick(context.Background())
		w.r.Tick(context.Background())
		if n := w.running(t); n != 1 {
			t.Fatalf("running = %d, want 1 within the spacing", n)
		}
		if !strings.Contains(w.log.String(), "spacing 5m0s") {
			t.Fatalf("log does not say why:\n%s", w.log)
		}
		w.clock.add(5 * time.Minute)
		w.r.Tick(context.Background())
		if n := w.running(t); n != 2 {
			t.Fatalf("running = %d, want 2 after the spacing", n)
		}
	})
	t.Run("lane cap", func(t *testing.T) {
		w := startWorld(t, Pacing{LaneCap: 2})
		w.r.Tick(context.Background())
		w.r.Tick(context.Background())
		if n := w.running(t); n != 2 {
			t.Fatalf("running = %d, want the lane cap 2", n)
		}
	})
	t.Run("max in flight counts other runners", func(t *testing.T) {
		w := startWorld(t, Pacing{MaxInFlight: 2})
		if _, err := w.q.s.Claim(jobs.Args{Runner: "lion-runner", Machine: "lion"}); err != nil {
			t.Fatal(err)
		}
		w.r.Tick(context.Background())
		if n := w.running(t); n != 2 {
			t.Fatalf("running = %d, want 2 in flight in all", n)
		}
	})
	t.Run("disk free", func(t *testing.T) {
		w := startWorld(t, Pacing{MinFreeDisk: 10 << 30})
		w.host.free = 1 << 30
		w.r.Tick(context.Background())
		if n := w.running(t); n != 0 || !strings.Contains(w.log.String(), "MB free") {
			t.Fatalf("running = %d with little disk; log:\n%s", n, w.log)
		}
	})
}

func TestDirtyCheckoutIsRefusedNeverStashed(t *testing.T) {
	w := newWorld(t, Config{Kinds: map[string]Kind{
		"start-item": {Command: []string{"factory", "start", "{subject}"}, Start: true},
	}})
	j := w.enqueue(t, jobs.Args{Kind: "start-item", Subject: "PRJ-1", Needs: jobs.Needs{Checkout: "/src/repo", Executor: "new-tab"}})
	w.host.dirty["/src/repo"] = true
	w.settle()
	got := w.state(t, j.ID)
	if got.State != jobs.StateFailed || !strings.Contains(got.Note, "never stashes") {
		t.Fatalf("dirty checkout: %s %q, want failed and never stashed", got.State, got.Note)
	}
	if w.host.count() != 0 {
		t.Fatalf("ran %q on a dirty checkout", w.host.runs)
	}
}

func tabWorld(t *testing.T) (*world, jobs.Job) {
	t.Helper()
	w := newWorld(t, Config{Kinds: map[string]Kind{
		"review": {Target: "pr-{subject}", Prompt: "Answer the review comments.", Timeout: time.Hour},
	}})
	j := w.enqueue(t, jobs.Args{Kind: "review", Subject: "42", Needs: jobs.Needs{Executor: "tab"}})
	return w, j
}

func TestDialogGatesTheJobAndIsNeverAnswered(t *testing.T) {
	w, j := tabWorld(t)
	w.panes.dialogs["pr-42"] = "permission prompt"
	w.settle()
	got := w.state(t, j.ID)
	if got.State != jobs.StateGated || !strings.Contains(got.Note, "permission prompt") {
		t.Fatalf("job = %s %q, want gated naming the dialog", got.State, got.Note)
	}
	if len(w.panes.prompts) != 0 {
		t.Fatalf("prompted a pane that shows a dialog: %+v", w.panes.prompts)
	}
}

func TestTabPromptsOnceAndWaitsForTheAgent(t *testing.T) {
	w, j := tabWorld(t)
	w.settle()
	if len(w.panes.prompts) != 1 {
		t.Fatalf("prompts = %+v, want one", w.panes.prompts)
	}
	p := w.panes.prompts[0]
	if p.target != "pr-42" || p.key != "tiger:job-"+j.ID+"-1" ||
		!strings.HasPrefix(p.text, "Answer the review comments.") ||
		!strings.Contains(p.text, "agentbus jobs done "+j.ID+" --runner "+runnerName) {
		t.Fatalf("prompt = %+v", p)
	}
	w.clock.add(time.Minute)
	w.settle()
	if got := w.state(t, j.ID); got.State != jobs.StateRunning || !got.Heartbeat.Equal(w.clock.now()) {
		t.Fatalf("job = %s heartbeat %s, want running and heartbeating", got.State, got.Heartbeat)
	}
	if _, err := w.q.s.Done(jobs.Args{ID: j.ID, Runner: runnerName, Note: "answered"}); err != nil {
		t.Fatal(err)
	}
	w.settle()
	w.settle()
	if len(w.panes.prompts) != 1 || len(w.r.active) != 0 {
		t.Fatalf("prompts %d, active %d after the agent reported", len(w.panes.prompts), len(w.r.active))
	}
}

func TestAgentJobPastItsTimeoutIsLeftToTheDaemon(t *testing.T) {
	w, j := tabWorld(t)
	w.settle()
	w.clock.add(2 * time.Hour)
	before := w.state(t, j.ID).Heartbeat
	w.settle()
	if got := w.state(t, j.ID); !got.Heartbeat.Equal(before) || got.State != jobs.StateRunning {
		t.Fatalf("job = %s heartbeat %s, want running with no new heartbeat", got.State, got.Heartbeat)
	}
	if len(w.panes.prompts) != 1 {
		t.Fatalf("prompted %d times, want once", len(w.panes.prompts))
	}
}

func TestNewTabStartsAnAgentWithThePrompt(t *testing.T) {
	w := newWorld(t, Config{Kinds: map[string]Kind{
		"fix-ci": {Command: []string{"launch", "--prompt", "{prompt}"}},
	}})
	j := w.enqueue(t, jobs.Args{Kind: "fix-ci", Subject: "o/r#1", Needs: jobs.Needs{Executor: "new-tab"}})
	w.settle()
	if got := w.state(t, j.ID); got.State != jobs.StateRunning {
		t.Fatalf("job = %s, want running while its agent works", got.State)
	}
	if argv := w.host.runs[0]; !strings.Contains(argv[2], "agentbus jobs done "+j.ID) || envOf(w.host.envs[0], "JOB_PROMPT") != argv[2] {
		t.Fatalf("argv = %q", argv)
	}

	w.host.exits["o/r#2"] = 1
	bad := w.enqueue(t, jobs.Args{Kind: "fix-ci", Subject: "o/r#2", Needs: jobs.Needs{Executor: "new-tab"}})
	if _, err := w.q.s.Done(jobs.Args{ID: j.ID, Runner: runnerName}); err != nil {
		t.Fatal(err)
	}
	w.settle()
	w.settle()
	if got := w.state(t, bad.ID); got.State != jobs.StateFailed {
		t.Fatalf("failed start: %s, want failed", got.State)
	}
}

func TestDryRunMakesNoChanges(t *testing.T) {
	w := newWorld(t, Config{Kinds: map[string]Kind{"rebase": cliKind(1)}})
	j := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#1", Head: "abc"})
	w.r.DryRun = true
	w.settle()
	w.settle()
	if got := w.state(t, j.ID); got.State != jobs.StateReady {
		t.Fatalf("job = %s, want ready", got.State)
	}
	if c := w.q.changes(); len(c) != 0 || w.host.count() != 0 {
		t.Fatalf("dry-run changed the queue %q or ran %q", c, w.host.runs)
	}
	if !strings.Contains(w.log.String(), "dry-run: would claim "+j.ID+" rebase o/r#1 and run prsync rebase o/r#1 abc") {
		t.Fatalf("log:\n%s", w.log)
	}
	if strings.Count(w.log.String(), "would claim") != 1 {
		t.Fatalf("an idle poll repeats itself:\n%s", w.log)
	}
}

func TestHoldIsHonoured(t *testing.T) {
	w := newWorld(t, Config{Kinds: map[string]Kind{"rebase": cliKind(1)}})
	j := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#1"})
	if err := w.q.s.Hold("release day"); err != nil {
		t.Fatal(err)
	}
	w.settle()
	if got := w.state(t, j.ID); got.State != jobs.StateReady || !strings.Contains(w.log.String(), "on hold: release day") {
		t.Fatalf("job = %s under hold; log:\n%s", got.State, w.log)
	}
	w.r.DryRun = true
	w.settle()
	if !strings.Contains(w.log.String(), "dry-run: on hold: release day") {
		t.Fatalf("dry-run log:\n%s", w.log)
	}
}

// resumed is a fix-ci job an agent in pane tab parked on branch
// park/ci and the user's answer made ready again.
func resumed(t *testing.T, w *world, tab string) jobs.Job {
	t.Helper()
	j := w.enqueue(t, jobs.Args{Kind: "fix-ci", Subject: "o/r#1", Needs: jobs.Needs{Executor: "new-tab", Checkout: "/src/repo"}})
	if _, err := w.q.s.Claim(jobs.Args{Runner: "other-runner", Machine: machine}); err != nil {
		t.Fatal(err)
	}
	if _, err := w.q.s.Park(jobs.Args{ID: j.ID, Runner: "other-runner", Decide: "d1", Saved: "park/ci", Note: "tests fixed; lint left", Tab: tab}); err != nil {
		t.Fatal(err)
	}
	if err := w.q.s.Resume(j.ID, "d1", "answered"); err != nil {
		t.Fatal(err)
	}
	return j
}

func resumeWorld(t *testing.T) *world {
	t.Helper()
	return newWorld(t, Config{Kinds: map[string]Kind{
		"fix-ci": {Command: []string{"launch", "--branch", "{saved}", "--prompt", "{prompt}"}},
	}})
}

func TestResumeGoesBackToTheParkedTab(t *testing.T) {
	w := resumeWorld(t)
	j := resumed(t, w, "tiger/w1/p2")
	w.settle()
	if w.host.count() != 0 {
		t.Fatalf("started a new agent while the parked tab lives: %q", w.host.runs)
	}
	if len(w.panes.prompts) != 1 {
		t.Fatalf("prompts = %+v, want one", w.panes.prompts)
	}
	p := w.panes.prompts[0]
	if p.target != "tiger/w1/p2" || !strings.Contains(p.text, "park/ci") || !strings.Contains(p.text, "tests fixed; lint left") ||
		!strings.Contains(p.text, "agentbus jobs done "+j.ID) {
		t.Fatalf("prompt = %+v", p)
	}
	if got := w.state(t, j.ID); got.State != jobs.StateRunning || got.Holder != runnerName {
		t.Fatalf("job = %s by %s, want running for the agent", got.State, got.Holder)
	}
}

func TestResumeStartsANewTabFromTheSavedRefWhenTheTabIsDead(t *testing.T) {
	for _, tab := range []string{"tiger/w1/p2", ""} {
		w := resumeWorld(t)
		w.panes.dead["tiger/w1/p2"] = true
		j := resumed(t, w, tab)
		w.settle()
		if len(w.panes.prompts) != 0 {
			t.Fatalf("tab %q: prompted %+v", tab, w.panes.prompts)
		}
		if w.host.count() != 1 {
			t.Fatalf("tab %q: runs = %q, want one new agent", tab, w.host.runs)
		}
		argv, env := w.host.runs[0], w.host.envs[0]
		if argv[2] != "park/ci" || envOf(env, "JOB_SAVED") != "park/ci" || !strings.Contains(argv[4], "tests fixed; lint left") {
			t.Fatalf("tab %q: argv = %q", tab, argv)
		}
		if got := w.state(t, j.ID); got.State != jobs.StateRunning {
			t.Fatalf("tab %q: job = %s, want running while its agent works", tab, got.State)
		}
	}
}
