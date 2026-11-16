package tui

import (
	"bytes"
	"context"
	"errors"
	"io"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

const (
	healthOK            = `{"daemon":"up","transport":"ok","problems":[]}`
	healthDaemonDown    = `{"daemon":"down","transport":"unknown","problems":["daemon unreachable: dial unix /run/agentbus.sock: connect: no such file or directory"]}`
	healthTransportDown = `{"daemon":"up","transport":"down","problems":["slack: token expired","herdr: socket gone"]}`
)

// fakeHealth answers `agentbus health --json` with out and err, the way
// decide.ExecAgentbus reports a nonzero exit that printed JSON.
type fakeHealth struct {
	out   string
	err   error
	calls int
}

func (f *fakeHealth) run(_ context.Context, args []string) ([]byte, error) {
	f.calls++
	if strings.Join(args, " ") != "health --json" {
		return nil, errors.New("unexpected agentbus " + strings.Join(args, " "))
	}
	return []byte(f.out), f.err
}

func healthModel(t *testing.T, run BusRunner) *Model {
	t.Helper()
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	m, err := prepare(Options{
		Dir:       t.TempDir(),
		In:        strings.NewReader(""),
		Out:       io.Discard,
		BusRunner: run,
	})
	if err != nil {
		t.Fatal(err)
	}
	m.width, m.height = 100, 20
	return m
}

// pollHealth runs one poll the way the program does: the tick starts
// the check, and its result goes back through Update.
func pollHealth(t *testing.T, m *Model) {
	t.Helper()
	_, cmd := m.Update(healthTickMsg{})
	if cmd == nil {
		t.Fatal("the health tick started no check")
	}
	_, next := m.Update(cmd())
	if next == nil {
		t.Fatal("a finished check scheduled no next poll")
	}
}

func screenTop(m *Model) string {
	lines := strings.Split(m.View(), "\n")
	if m.framed() {
		return visibleLine(lines[1])
	}
	return visibleLine(lines[0])
}

func TestBusDownShowsTheBannerWithTheFirstProblem(t *testing.T) {
	for _, tc := range []struct{ name, out, problem string }{
		{"daemon", healthDaemonDown, "daemon unreachable"},
		{"transport", healthTransportDown, "slack: token expired"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			bus := &fakeHealth{out: tc.out, err: errors.New("agentbus health: exit status 1")}
			m := healthModel(t, bus.run)
			pollHealth(t, m)
			top := screenTop(m)
			if !strings.Contains(top, busDownBanner) || !strings.Contains(top, tc.problem) {
				t.Fatalf("top line = %q", top)
			}
			if tc.name == "transport" && strings.Contains(top, "herdr") {
				t.Fatalf("top line shows more than the first problem: %q", top)
			}
		})
	}
}

func TestBusBannerProblemIsDimmedAndTruncated(t *testing.T) {
	bus := &fakeHealth{out: healthDaemonDown, err: errors.New("exit status 1")}
	m := healthModel(t, bus.run)
	m.width = 50
	pollHealth(t, m)
	raw := strings.Split(m.View(), "\n")[1]
	if !strings.Contains(raw, "\x1b[2m") {
		t.Fatalf("problem is not dimmed: %q", raw)
	}
	if got := len([]rune(visibleLine(raw))); got != m.width {
		t.Fatalf("banner row is %d columns, want %d: %q", got, m.width, visibleLine(raw))
	}
}

func TestBusOKHidesTheBannerAndRecoveryClearsIt(t *testing.T) {
	bus := &fakeHealth{out: healthOK}
	m := healthModel(t, bus.run)
	pollHealth(t, m)
	if strings.Contains(m.View(), busDownBanner) {
		t.Fatal("banner shown while the bus is ok")
	}
	bus.out, bus.err = healthTransportDown, errors.New("exit status 4")
	pollHealth(t, m)
	if !strings.Contains(m.View(), busDownBanner) {
		t.Fatal("banner missing while the transport is down")
	}
	bus.out, bus.err = healthOK, nil
	pollHealth(t, m)
	if strings.Contains(m.View(), busDownBanner) {
		t.Fatal("banner still shown after the bus recovered")
	}
	if bus.calls != 3 {
		t.Fatalf("health ran %d times, want 3", bus.calls)
	}
}

func TestBusBannerTakesARowFromTheList(t *testing.T) {
	bus := &fakeHealth{out: healthOK}
	m := healthModel(t, bus.run)
	pollHealth(t, m)
	before, _ := m.layoutBudget()
	bus.out = healthDaemonDown
	pollHealth(t, m)
	after, _ := m.layoutBudget()
	if after != before-1 {
		t.Fatalf("list rows = %d with the banner, %d without", after, before)
	}
	if got := len(strings.Split(m.View(), "\n")); got != m.height {
		t.Fatalf("screen is %d rows, want %d", got, m.height)
	}
}

func captureSlog(t *testing.T) *bytes.Buffer {
	t.Helper()
	var buf bytes.Buffer
	old := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&buf, &slog.HandlerOptions{Level: slog.LevelDebug})))
	t.Cleanup(func() { slog.SetDefault(old) })
	return &buf
}

func TestMissingOrFailingHealthShowsNoBannerAndLogsOnce(t *testing.T) {
	for _, tc := range []struct {
		name string
		out  string
		err  error
	}{
		{"missing", "", errors.New(`agentbus: exec: "agentbus": executable file not found in $PATH`)},
		{"unsupported", "", errors.New("usage: agentbus <command>")},
		{"garbage", "not json", nil},
	} {
		t.Run(tc.name, func(t *testing.T) {
			logs := captureSlog(t)
			bus := &fakeHealth{out: healthDaemonDown, err: errors.New("exit status 1")}
			m := healthModel(t, bus.run)
			pollHealth(t, m)
			bus.out, bus.err = tc.out, tc.err
			pollHealth(t, m)
			pollHealth(t, m)
			if strings.Contains(m.View(), busDownBanner) {
				t.Fatal("banner shown when health cannot report")
			}
			if n := strings.Count(logs.String(), "agentbus health"); n != 1 {
				t.Fatalf("logged %d times, want once:\n%s", n, logs)
			}
		})
	}
}

func TestSlowHealthDoesNotFreezeTheInbox(t *testing.T) {
	started := make(chan struct{}, 1)
	slow := func(ctx context.Context, _ []string) ([]byte, error) {
		started <- struct{}{}
		<-ctx.Done()
		return nil, ctx.Err()
	}
	m := healthModel(t, slow)
	m.healthTimeout = 20 * time.Millisecond
	_, cmd := m.Update(healthTickMsg{})
	if cmd == nil {
		t.Fatal("the health tick started no check")
	}
	select {
	case <-started:
		t.Fatal("Update ran the health command itself")
	default:
	}
	done := make(chan any, 1)
	go func() { done <- cmd() }()
	// The inbox keeps taking keys and drawing while the check runs.
	press(t, m, "j")
	_ = m.View()
	select {
	case msg := <-done:
		m.Update(msg)
	case <-time.After(5 * time.Second):
		t.Fatal("a slow health check did not give up at its timeout")
	}
	if strings.Contains(m.View(), busDownBanner) {
		t.Fatal("banner shown for a check that timed out")
	}
}

func TestInitDoesNotRunHealthOnTheUpdateLoop(t *testing.T) {
	bus := &fakeHealth{out: healthDaemonDown, err: errors.New("exit status 1")}
	m := healthModel(t, bus.run)
	if m.Init() == nil {
		t.Fatal("Init started nothing")
	}
	if bus.calls != 0 {
		t.Fatal("Init ran the health command itself")
	}
}

// installAgentbus puts a fake agentbus script first on PATH.
func installAgentbus(t *testing.T, script string) {
	t.Helper()
	bin := t.TempDir()
	if err := os.WriteFile(filepath.Join(bin, "agentbus"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin)
}

func TestHealthCommandOnPath(t *testing.T) {
	t.Run("down exits 1 with JSON", func(t *testing.T) {
		installAgentbus(t, "#!/bin/sh\n[ \"$1 $2\" = 'health --json' ] || exit 2\necho '"+healthDaemonDown+"'\nexit 1\n")
		m := healthModel(t, decide.ExecAgentbus)
		pollHealth(t, m)
		if !strings.Contains(screenTop(m), busDownBanner) {
			t.Fatalf("top line = %q", screenTop(m))
		}
	})
	t.Run("unsupported exits 2", func(t *testing.T) {
		installAgentbus(t, "#!/bin/sh\necho 'agentbus: unknown command' >&2\nexit 2\n")
		m := healthModel(t, decide.ExecAgentbus)
		pollHealth(t, m)
		if strings.Contains(m.View(), busDownBanner) {
			t.Fatal("banner shown for an agentbus without health")
		}
	})
	t.Run("missing", func(t *testing.T) {
		t.Setenv("PATH", t.TempDir())
		m := healthModel(t, decide.ExecAgentbus)
		pollHealth(t, m)
		if strings.Contains(m.View(), busDownBanner) {
			t.Fatal("banner shown without agentbus")
		}
	})
}
