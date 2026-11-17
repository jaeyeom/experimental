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

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
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

func TestJobsOpsOverTheSocket(t *testing.T) {
	dirs := jobsDirs(t)
	stop := start(t, dirs)
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
		Now:      clock,
		JobTicks: ticks,
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
