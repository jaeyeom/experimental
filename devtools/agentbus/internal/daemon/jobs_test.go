package daemon

import (
	"context"
	"encoding/json"
	"net"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func jobsCall(t *testing.T, sock, op string, a jobs.Args) Response {
	t.Helper()
	conn, err := net.Dial("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()
	if err := json.NewEncoder(conn).Encode(Request{Op: op, Jobs: &a}); err != nil {
		t.Fatal(err)
	}
	var resp Response
	if err := json.NewDecoder(conn).Decode(&resp); err != nil {
		t.Fatal(err)
	}
	return resp
}

func jobsDirs(t *testing.T) paths.Dirs {
	t.Helper()
	return paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
}

// autoJobs lets every job past the gate, for tests of what comes after.
var autoJobs = jobs.Policy{Default: jobs.GateAuto}

func TestJobsOpsOverTheSocket(t *testing.T) {
	dirs := jobsDirs(t)
	stop := startWith(t, dirs, Config{JobPolicy: autoJobs})
	defer stop()
	r := jobsCall(t, dirs.SocketPath, "jobs-enqueue", jobs.Args{Kind: "rebase", Subject: "o/r#1", Head: "abc"})
	if !r.OK || r.Jobs == nil || r.Jobs.Job.State != jobs.StateReady {
		t.Fatalf("enqueue = %+v", r)
	}
	id := r.Jobs.Job.ID
	if r := jobsCall(t, dirs.SocketPath, "jobs-enqueue", jobs.Args{Kind: "rebase", Subject: "o/r#1", Head: "abc"}); !r.Jobs.Duplicate {
		t.Fatalf("second enqueue = %+v, want duplicate", r.Jobs)
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-hold", jobs.Args{Note: "freeze"}); !r.OK {
		t.Fatalf("hold = %+v", r)
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-claim", jobs.Args{Runner: "r1"}); !r.Jobs.Held || r.Jobs.Job != nil {
		t.Fatalf("claim under hold = %+v", r.Jobs)
	}
	jobsCall(t, dirs.SocketPath, "jobs-unhold", jobs.Args{})
	if r := jobsCall(t, dirs.SocketPath, "jobs-claim", jobs.Args{Runner: "r1"}); r.Jobs.Job == nil || r.Jobs.Job.ID != id {
		t.Fatalf("claim = %+v", r)
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-heartbeat", jobs.Args{ID: id, Runner: "r1"}); !r.OK {
		t.Fatalf("heartbeat = %+v", r)
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-done", jobs.Args{ID: id, Runner: "r2"}); r.OK {
		t.Fatal("done by a non-holder succeeded")
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-done", jobs.Args{ID: id, Runner: "r1", Note: "rebased"}); !r.OK || r.Jobs.Job.State != jobs.StateDone {
		t.Fatalf("done = %+v", r)
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-show", jobs.Args{ID: id}); r.Jobs.Job.Note != "rebased" {
		t.Fatalf("show = %+v", r.Jobs.Job)
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-ls", jobs.Args{All: true}); len(r.Jobs.Jobs) != 1 {
		t.Fatalf("ls = %+v", r.Jobs)
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-stats", jobs.Args{}); r.Jobs.Stats.States[jobs.StateDone] != 1 {
		t.Fatalf("stats = %+v", r.Jobs.Stats)
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-bogus", jobs.Args{}); r.OK {
		t.Fatal("unknown jobs op succeeded")
	}
}

func TestJobsWatcherExpiresAndReconciles(t *testing.T) {
	dirs := jobsDirs(t)
	now := time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)
	var mu sync.Mutex
	clock := func() time.Time {
		mu.Lock()
		defer mu.Unlock()
		return now
	}
	ticks := make(chan time.Time)
	notes := make(chan string, 4)
	stop := startWith(t, dirs, Config{
		Now:       clock,
		JobTicks:  ticks,
		JobPolicy: autoJobs,
		Jobs: jobs.Config{
			HeartbeatTimeout: time.Minute,
			Kinds:            map[string]jobs.Kind{"merge": {Reconcile: []string{"check"}}},
		},
		JobHook: func(context.Context, []string, jobs.Job) (jobs.Outcome, string, error) {
			return jobs.OutcomeUnclear, "head moved", nil
		},
		Notify: func(text string) { notes <- text },
	})
	defer stop()
	id := jobsCall(t, dirs.SocketPath, "jobs-enqueue", jobs.Args{Kind: "merge", Subject: "o/r#1"}).Jobs.Job.ID
	jobsCall(t, dirs.SocketPath, "jobs-claim", jobs.Args{Runner: "r1"})
	mu.Lock()
	now = now.Add(5 * time.Minute)
	mu.Unlock()
	ticks <- now
	select {
	case text := <-notes:
		if !strings.Contains(text, id) || !strings.Contains(text, "head moved") {
			t.Fatalf("decide note = %q", text)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("no decide note for the unclear reconcile")
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-show", jobs.Args{ID: id}); r.Jobs.Job.State != jobs.StateUnknown {
		t.Fatalf("job = %s, want unknown", r.Jobs.Job.State)
	}
}

// gateDecide is a decide whose only item the user approves in the inbox.
type gateDecide struct {
	mu    sync.Mutex
	asked []string
	item  decide.Item
	done  chan string
}

func (g *gateDecide) Ask(_ context.Context, question string) (string, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.asked = append(g.asked, question)
	g.item = decide.Item{ID: "d1", State: decide.StateOpen, Messages: []decide.Message{
		{Author: "agentbus", Text: question, Options: []decide.Option{{Key: jobs.OptionApprove}, {Key: jobs.OptionReject}}},
	}}
	return "d1", nil
}

func (g *gateDecide) approve() {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.item.Messages = append(g.item.Messages, decide.Message{Author: decide.Human, OptionKey: jobs.OptionApprove, Via: decide.ViaUI})
	g.item.Answer = &decide.Answer{Value: jobs.OptionApprove, Key: jobs.OptionApprove, AnsweredBy: decide.Human, Via: decide.ViaUI}
}

func (g *gateDecide) Show(context.Context, string) (decide.Item, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.item, nil
}

func (g *gateDecide) Done(_ context.Context, _, outcome string) error {
	g.done <- outcome
	return nil
}

func TestJobsWatcherRunsTheAskGate(t *testing.T) {
	dirs := jobsDirs(t)
	ticks := make(chan time.Time)
	g := &gateDecide{done: make(chan string, 1)}
	// No policy: every kind is ask.
	stop := startWith(t, dirs, Config{JobTicks: ticks, JobDecider: g})
	defer stop()
	r := jobsCall(t, dirs.SocketPath, "jobs-enqueue", jobs.Args{Kind: "rebase", Subject: "o/r#1"})
	if r.Jobs.Job.State != jobs.StateGated {
		t.Fatalf("enqueue = %s, want gated", r.Jobs.Job.State)
	}
	id := r.Jobs.Job.ID
	ticks <- time.Now()
	waitFor(t, func() bool {
		g.mu.Lock()
		defer g.mu.Unlock()
		return len(g.asked) == 1
	})
	g.approve()
	ticks <- time.Now()
	select {
	case outcome := <-g.done:
		if !strings.HasPrefix(outcome, "approved") {
			t.Fatalf("decide done = %q", outcome)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("the approved item was not closed")
	}
	if r := jobsCall(t, dirs.SocketPath, "jobs-show", jobs.Args{ID: id}); r.Jobs.Job.State != jobs.StateReady || r.Jobs.Job.Decide != "d1" {
		t.Fatalf("job = %s %q, want ready from d1", r.Jobs.Job.State, r.Jobs.Job.Decide)
	}
}

func waitFor(t *testing.T, cond func() bool) {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	for !cond() {
		if time.Now().After(deadline) {
			t.Fatal("condition not met within 2s")
		}
		time.Sleep(5 * time.Millisecond)
	}
}

func TestJobsWatcherSendsAnomaliesToTheRoleOnce(t *testing.T) {
	dirs := jobsDirs(t)
	now := time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)
	var mu sync.Mutex
	clock := func() time.Time {
		mu.Lock()
		defer mu.Unlock()
		return now
	}
	ticks := make(chan time.Time)
	stop := startWith(t, dirs, Config{
		Now:       clock,
		JobTicks:  ticks,
		JobPolicy: autoJobs,
		Jobs:      jobs.Config{Anomalies: jobs.AnomalyConfig{Role: "coord/lead", RunnerSilent: time.Minute}},
	})
	defer stop()
	if r := jobsCall(t, dirs.SocketPath, "jobs-beat", jobs.Args{Runner: "r1", Machine: "tiger"}); !r.OK {
		t.Fatalf("beat = %+v", r)
	}
	mu.Lock()
	now = now.Add(2 * time.Minute)
	mu.Unlock()
	// The role is not registered here, so the message is rejected as an
	// unknown target; the rejection shows the daemon sent it to the role.
	toRole := func() int {
		lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Name: "coord/lead"})
		if err != nil {
			t.Fatal(err)
		}
		return len(lines)
	}
	ticks <- now
	waitFor(t, func() bool { return toRole() == 1 })
	ticks <- now
	ticks <- now
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: eventlog.EventJob})
	if err != nil {
		t.Fatal(err)
	}
	anomalies := 0
	for _, l := range lines {
		if strings.Contains(l, "anomaly runner-silent r1") {
			anomalies++
		}
	}
	if anomalies != 1 || toRole() != 1 {
		t.Fatalf("anomaly events = %d, messages to the role = %d; want one each", anomalies, toRole())
	}
}
