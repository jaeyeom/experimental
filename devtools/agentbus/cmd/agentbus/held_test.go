package main

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

const (
	heldNote     = "agentbus: held: the daemon can't reach herdr ("
	anonNote     = "sent as anonymous: the daemon couldn't identify this pane (herdr unreachable)"
	heldSender   = "herdr:local/default/w1:p1"
	heldReviewer = "herdr:local/default/w1:p2"
)

func TestSendReportsAnUnreachableTransport(t *testing.T) {
	env, _ := heldWorld(t, errors.New("herdr agent list: no herdr server is running"))
	code, stdout, stderr := runCmd([]string{"send", heldReviewer, "stuck"}, env("p1"))
	if code != 3 || !strings.Contains(stderr, heldNote) || !strings.Contains(stderr, "See: agentbus doctor") || strings.Contains(stderr, anonNote) {
		t.Fatalf("send = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}
	if messageCount(t, env("")) < 1 || !strings.Contains(stdout, "m-") {
		t.Fatalf("message was not stored: stdout %q", stdout)
	}
}

func TestSendReportsAHoldThatLandsAfterTheStore(t *testing.T) {
	env, ad := heldWorld(t, nil)
	ad.DeliverErr = &remotecall.UnreachableError{Machine: "local", Cause: "host unreachable"}
	start := time.Now()
	code, stdout, stderr := runCmd([]string{"send", heldReviewer, "soon-held"}, env("p1"))
	if elapsed := time.Since(start); code != 3 || !strings.Contains(stderr, heldNote) || !strings.Contains(stderr, "host unreachable") || !strings.Contains(stderr, "See: agentbus doctor") || strings.Contains(stdout, "queued for delivery") || elapsed >= 700*time.Millisecond {
		t.Fatalf("send = %d in %s\nstdout %q\nstderr %q", code, elapsed, stdout, stderr)
	}
	if messageCount(t, env("")) < 1 {
		t.Fatal("message was not stored")
	}
}

func TestSendTellsAnAnonymousSenderTheHold(t *testing.T) {
	env, ad, dirs := heldWorldDirs(t, errors.New("herdr agent list: down"))
	ad.PaneHandles = nil
	code, stdout, stderr := runCmd([]string{"send", heldReviewer, "from a pane herdr cannot see"}, env("p1"))
	if code != 3 || !strings.Contains(stderr, heldNote) || !strings.Contains(stderr, "See: agentbus doctor") || !strings.Contains(stderr, anonNote) {
		t.Fatalf("send = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}
	time.Sleep(20 * time.Millisecond)
	mustCmd(t, []string{"status", firstID(t, stdout)}, env(""))
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: eventlog.EventHoldNoticeSkipped})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) != 1 || !strings.Contains(lines[0], "anonymous sender") {
		t.Fatalf("hold_notice_skipped = %q", lines)
	}
}

func TestSendToAReachableTransportStaysQueued(t *testing.T) {
	env, _ := heldWorld(t, nil)
	code, stdout, stderr := runCmd([]string{"send", heldReviewer, "hello"}, env("p1"))
	if code != 0 || !strings.Contains(stdout, "queued for delivery") || strings.Contains(stderr, heldNote) {
		t.Fatalf("send = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}
}

func TestDoctorPrintsLiveHerdrDownWhenTheLogHasNoEvent(t *testing.T) {
	env, ad, dirs := heldWorldDirs(t, nil)
	mustCmd(t, []string{"send", heldReviewer, "establish"}, env("p1"))
	reason := "herdr agent list: no herdr server is running at /tmp/herdr.sock"
	if logged(t, dirs.StateDir, eventlog.EventAdapterError) {
		t.Fatal("setup logged adapter_error before herdr went down")
	}
	ad.SelfTestErr = errors.New(reason)
	code, stdout, stderr := runCmd([]string{"doctor"}, env(""))
	if code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	if !strings.Contains(stdout, "herdr transport unreachable: "+reason) {
		t.Fatalf("stdout = %s", stdout)
	}
}

func TestDoctorTrustsLiveStatusOverAStaleLog(t *testing.T) {
	env, _, dirs := heldWorldDirs(t, nil)
	if err := eventlog.Open(dirs.StateDir).Append(eventlog.Event{
		Event: eventlog.EventAdapterError, Actor: "agentbus", Name: "herdr",
		Detail: "herdr unreachable", Reason: "stale socket",
	}); err != nil {
		t.Fatal(err)
	}
	code, stdout, stderr := runCmd([]string{"doctor"}, env(""))
	if code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	if strings.Contains(stdout, "transport unreachable") {
		t.Fatalf("stale log was reported:\n%s", stdout)
	}
}

func TestDoctorFallsBackWhenTheDaemonIsDown(t *testing.T) {
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	lg := eventlog.Open(dirs.StateDir)
	for _, ev := range []eventlog.Event{
		{Event: eventlog.EventStart, Actor: "daemon", Detail: "listening"},
		{Event: eventlog.EventAdapterError, Actor: "agentbus", Name: "herdr", Detail: "herdr unreachable", Reason: "from the log"},
	} {
		if err := lg.Append(ev); err != nil {
			t.Fatal(err)
		}
	}
	orig := doctorHerdrProbe
	doctorHerdrProbe = func(context.Context, []string) error {
		return errors.New("probe: herdr agent list failed")
	}
	t.Cleanup(func() { doctorHerdrProbe = orig })

	code, stdout, stderr := runCmd([]string{"doctor"}, base)
	if code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	for _, want := range []string{
		"herdr transport unreachable: from the log",
		"herdr transport unreachable: probe: herdr agent list failed",
	} {
		if !strings.Contains(stdout, want) {
			t.Errorf("stdout lacks %q:\n%s", want, stdout)
		}
	}
	if _, err := dialDaemon(context.Background(), dirs.SocketPath, daemon.Request{Op: "ping"}); err == nil {
		t.Fatal("doctor started a daemon")
	}
}

func heldWorld(t *testing.T, down error) (func(string) []string, *fake.Adapter) {
	t.Helper()
	env, ad, _ := heldWorldDirs(t, down)
	return env, ad
}

func heldWorldDirs(t *testing.T, down error) (func(string) []string, *fake.Adapter, paths.Dirs) {
	t.Helper()
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	ad := &fake.Adapter{
		AdapterName: "herdr",
		SelfTestErr: down,
		Reach:       map[string]adapter.State{heldSender: adapter.StateIdle, heldReviewer: adapter.StateIdle},
		Result:      map[string]adapter.Result{heldSender: adapter.ResultAccepted, heldReviewer: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": heldSender, "p2": heldReviewer},
	}
	notice := time.Millisecond
	stop := startDaemon(t, dirs, daemon.Config{
		Adapters:        []adapter.Adapter{ad},
		PollInterval:    time.Hour,
		HoldNoticeAfter: &notice,
	})
	t.Cleanup(stop)
	env := func(pane string) []string {
		out := append([]string{}, base...)
		if pane != "" {
			out = append(out, "HERDR_PANE_ID="+pane)
		}
		return out
	}
	return env, ad, dirs
}

func firstID(t *testing.T, stdout string) string {
	t.Helper()
	id, _, ok := strings.Cut(strings.TrimSpace(stdout), " ")
	if !ok || !strings.HasPrefix(id, "m-") {
		t.Fatalf("stdout has no id: %q", stdout)
	}
	return id
}

func logged(t *testing.T, dir, event string) bool {
	t.Helper()
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Event: event})
	if err != nil {
		t.Fatal(err)
	}
	return len(lines) > 0
}

func TestSendToAnUnregisteredNameExitsTwo(t *testing.T) {
	env, _ := heldWorld(t, nil)
	code, stdout, stderr := runCmd([]string{"send", "ops/nobody", "hello"}, env("p1"))
	if code != 2 || stdout != "" || !strings.Contains(stderr, "ops/nobody is not registered") {
		t.Fatalf("send = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}
}

func TestReplyToASenderNoLongerRegisteredExitsTwo(t *testing.T) {
	env, _ := heldWorld(t, nil)
	mustCmd(t, []string{"register", "ops/gone", "--when", "tests"}, env("p1"))
	out, _ := mustCmd(t, []string{"send", heldReviewer, "question"}, env("p1"))
	id := firstID(t, out)
	mustCmd(t, []string{"release", "ops/gone"}, env("p1"))
	code, stdout, stderr := runCmd([]string{"reply", id, "answer"}, env("p2"))
	if code != 2 || stdout != "" || !strings.Contains(stderr, "ops/gone is not registered") {
		t.Fatalf("reply = %d\nstdout %q\nstderr %q", code, stdout, stderr)
	}
}

func TestSendAndReplyExitOneOnADaemonError(t *testing.T) {
	for _, tc := range []struct {
		name string
		resp daemon.Response
		err  error
	}{
		{name: "unreachable", err: errors.New("connect: connection refused")},
		{name: "refused", resp: daemon.Response{Error: "mailbox is full"}},
	} {
		orig := dialDaemon
		dialDaemon = func(context.Context, string, daemon.Request) (daemon.Response, error) {
			return tc.resp, tc.err
		}
		ensure := ensureDaemonUp
		ensureDaemonUp = func(context.Context, paths.Dirs, []string) error { return nil }
		for _, args := range [][]string{{"send", "ops/x", "hi"}, {"reply", "m-1", "hi"}} {
			if code, _, stderr := runCmd(args, resendEnv(t)); code != 1 {
				t.Errorf("%s %q = %d %q, want 1", tc.name, args, code, stderr)
			}
		}
		dialDaemon = orig
		ensureDaemonUp = ensure
	}
}
