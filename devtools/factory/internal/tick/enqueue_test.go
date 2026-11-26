package tick

import (
	"bytes"
	"context"
	"flag"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

var update = flag.Bool("update", false, "rewrite the golden files in testdata") //nolint:pkgvar // go test golden update flag

// TestTickEnqueueGoldenJobs covers each affinity: a remote lane label, a
// local lane label, the machine of a released claim on the mac lane, and
// an item never worked.
func TestTickEnqueueGoldenJobs(t *testing.T) {
	items := []source.Item{
		labeled("PRJ-1", "needs-mac"),
		labeled("PRJ-2", "needs-linux"),
		labeled("PRJ-3"),
		labeled("PRJ-4"),
	}
	env := newLaneEnv(t, items, false, 2)
	cfg := env.eng.Config
	cfg.Lanes[0].Checkout = "/path/on/linux/checkout"
	cfg.Sources[0].Cwd = "/path/to/source/checkout"
	seedLaneClaim(t, env, "PRJ-3", "mac", "t9")
	if err := env.st.UpdateClaim(context.Background(), "jira:acme", "PRJ-3", env.now.Add(-time.Hour), store.Released, "", "t9", "w1"); err != nil {
		t.Fatal(err)
	}
	jobs := &bytes.Buffer{}
	env.eng.Enqueue, env.eng.Jobs = true, jobs
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	golden(t, "enqueue_jobs.golden", jobs.String())
	if got := env.out.String(); got != "enqueued 4 start-item jobs\n" {
		t.Fatalf("summary = %q", got)
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v, want none", got)
	}
	if len(env.machines) != 0 || env.gitCalls != 0 {
		t.Fatalf("enqueue gated: machines %v, git calls %d", env.machines, env.gitCalls)
	}
	if run := tickRun(t, env.st); run.Outcome != outcomeEnqueued {
		t.Fatalf("outcome = %s", run.Outcome)
	}
}

func TestTickEnqueueWithoutLanes(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-7", "Ship it")}, false, false)
	env.eng.Config.Sources[0].Cwd = "/path/to/source/checkout"
	jobs := &bytes.Buffer{}
	env.eng.Enqueue, env.eng.Jobs = true, jobs
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	golden(t, "enqueue_no_lanes.golden", jobs.String())
}

func TestTickEnqueueIdle(t *testing.T) {
	env := newTickEnv(t, nil, false, false)
	jobs := &bytes.Buffer{}
	env.eng.Enqueue, env.eng.Jobs = true, jobs
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if jobs.Len() != 0 || env.out.String() != "no ready items\n" {
		t.Fatalf("jobs = %q, summary = %q", jobs.String(), env.out.String())
	}
}

func TestOwnership(t *testing.T) {
	cfg := &config.Config{Lanes: []config.Lane{{Name: "mac", Machine: "mac1"}, {Name: "linux"}}}
	at := time.Date(2026, 10, 31, 11, 0, 0, 0, time.UTC)
	tests := []struct {
		lane, want string
	}{
		{lane: "mac", want: "mac1"},        // A remote lane: its herdr machine.
		{lane: "linux", want: "test-host"}, // A local lane: the host.
		{lane: "", want: "test-host"},      // No lanes.
		{lane: "gone", want: "test-host"},  // A lane no longer configured.
	}
	for _, tc := range tests {
		got := Ownership(cfg, store.Claim{Source: "jira:acme", ID: "PRJ-3", Machine: "test-host", Lane: tc.lane, TabID: "t9", ClaimedAt: at, State: store.Started})
		if got.Machine != tc.want || got.Tab != "t9" || got.Lane != tc.lane || !got.ClaimedAt.Equal(at) {
			t.Fatalf("lane %q: owner = %+v, want machine %s", tc.lane, got, tc.want)
		}
	}
}

func golden(t *testing.T, name, got string) {
	t.Helper()
	path := filepath.Join("testdata", name)
	if *update {
		if err := os.WriteFile(path, []byte(got), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	want, err := os.ReadFile(path) //nolint:gosec // A fixed path under testdata.
	if err != nil {
		t.Fatal(err)
	}
	if got != string(want) {
		t.Fatalf("output differs from %s (rerun with -update):\n%s", path, got)
	}
}
