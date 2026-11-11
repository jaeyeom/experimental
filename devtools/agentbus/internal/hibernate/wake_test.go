package hibernate

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
)

func TestResumeArgv(t *testing.T) {
	for _, tt := range []struct {
		name string
		argv []string
		want []string
	}{
		{"adds resume, keeps flags", []string{"claude", "--model", "opus", "--dangerously-skip-permissions"}, []string{"claude", "--resume", "s1", "--model", "opus", "--dangerously-skip-permissions"}},
		{"replaces resume", []string{"claude", "--resume", "old", "--model", "opus"}, []string{"claude", "--resume", "s1", "--model", "opus"}},
		{"replaces resume=", []string{"claude", "--resume=old", "--model", "opus"}, []string{"claude", "--resume", "s1", "--model", "opus"}},
		{"replaces -r", []string{"/usr/local/bin/claude", "-r", "old"}, []string{"/usr/local/bin/claude", "--resume", "s1"}},
		{"replaces --continue", []string{"claude", "--continue", "--model", "opus"}, []string{"claude", "--resume", "s1", "--model", "opus"}},
		{"replaces -c", []string{"claude", "-c"}, []string{"claude", "--resume", "s1"}},
		{"bare resume before a flag", []string{"claude", "--resume", "--model", "opus"}, []string{"claude", "--resume", "s1", "--model", "opus"}},
	} {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ResumeArgv("claude", tt.argv, "s1")
			if err != nil || !slices.Equal(got, tt.want) {
				t.Fatalf("ResumeArgv(%q) = %q, %v; want %q", tt.argv, got, err, tt.want)
			}
		})
	}
	for _, tt := range []struct {
		name, kind, session string
		argv                []string
	}{
		{"unknown kind", "codex", "s1", []string{"codex"}},
		{"no session", "claude", "", []string{"claude"}},
		{"no argv", "claude", "s1", nil},
	} {
		if _, err := ResumeArgv(tt.kind, tt.argv, tt.session); err == nil {
			t.Errorf("%s: ResumeArgv succeeded", tt.name)
		}
	}
}

// wakePanes is a herdr whose pane runs a shell until a command is run
// in it. The agent then shows up readyAfter agent reads later.
type wakePanes struct {
	info       herdr.ProcessInfo
	agent      adapter.Agent
	listed     bool
	ran        []string
	readyAfter int
	reads      int
	// never keeps the agent from ever getting ready.
	never bool
}

func shellInfo() herdr.ProcessInfo {
	return herdr.ProcessInfo{ShellPID: 1, ForegroundProcessGroupID: 1, ForegroundProcesses: []herdr.ForegroundProcess{{PID: 1, Name: "zsh", Argv: []string{"-zsh"}}}}
}

func (p *wakePanes) PaneProcessInfo(context.Context, string) (herdr.ProcessInfo, error) {
	return p.info, nil
}

func (p *wakePanes) PaneRun(_ context.Context, handle, command string) error {
	p.ran = append(p.ran, handle+" "+command)
	p.info = info(500, "/src/repo", "claude")
	return nil
}

func (p *wakePanes) PaneAgent(context.Context, string) (adapter.Agent, bool, error) {
	if len(p.ran) == 0 {
		return adapter.Agent{}, false, nil
	}
	p.reads++
	if p.never || p.reads < p.readyAfter {
		return adapter.Agent{Kind: "claude", Status: "working", SessionID: "s1"}, true, nil
	}
	return p.agent, p.listed, nil
}

func sleepingRecord() Record {
	return Record{
		Handle: alice, Pane: "w1:p1", Kind: "claude", Session: "s1", Cwd: "/src/repo",
		Argv: []string{"claude", "--resume", "old", "--model", "opus"}, Names: []string{"ops/x"}, Status: StatusHibernated,
	}
}

func TestWakeRelaunchesInTheCwdAndWaitsUntilReady(t *testing.T) {
	panes := &wakePanes{info: shellInfo(), agent: adapter.Agent{Kind: "claude", Status: "idle", SessionID: "s1"}, listed: true, readyAfter: 3}
	clock := &fakeClock{now: t0()}
	w := &Waker{Panes: panes, Clock: clock, Timeout: time.Minute}
	if err := w.Wake(context.Background(), sleepingRecord()); err != nil {
		t.Fatal(err)
	}
	want := []string{alice + ` cd '/src/repo' && 'claude' '--resume' 's1' '--model' 'opus'`}
	if !slices.Equal(panes.ran, want) {
		t.Fatalf("ran %q, want %q", panes.ran, want)
	}
	if panes.reads != 3 {
		t.Fatalf("read the agent %d times, want 3", panes.reads)
	}
}

func TestWakeTimesOut(t *testing.T) {
	panes := &wakePanes{info: shellInfo(), never: true}
	clock := &fakeClock{now: t0()}
	w := &Waker{Panes: panes, Clock: clock, Timeout: time.Minute}
	err := w.Wake(context.Background(), sleepingRecord())
	if err == nil || !strings.Contains(err.Error(), "not ready within 1m0s") {
		t.Fatalf("Wake() = %v", err)
	}
	if got := clock.now.Sub(t0()); got < time.Minute {
		t.Fatalf("waited %s, want the timeout", got)
	}
	if len(panes.ran) != 1 {
		t.Fatalf("ran %q, want one launch", panes.ran)
	}
}

func TestWakeWaitsForTheSameSession(t *testing.T) {
	panes := &wakePanes{info: shellInfo(), agent: adapter.Agent{Kind: "claude", Status: "idle", SessionID: "fresh"}, listed: true}
	w := &Waker{Panes: panes, Clock: &fakeClock{now: t0()}, Timeout: 10 * time.Second}
	err := w.Wake(context.Background(), sleepingRecord())
	if err == nil || !strings.Contains(err.Error(), `session "fresh", not "s1"`) {
		t.Fatalf("Wake() = %v", err)
	}
}

func TestWakeAcceptsAnUnreportedSession(t *testing.T) {
	panes := &wakePanes{info: shellInfo(), agent: adapter.Agent{Kind: "claude", Status: "done"}, listed: true}
	w := &Waker{Panes: panes, Clock: &fakeClock{now: t0()}, Timeout: 10 * time.Second}
	if err := w.Wake(context.Background(), sleepingRecord()); err != nil {
		t.Fatal(err)
	}
}

func TestWakeRefusesAReusedPane(t *testing.T) {
	panes := &wakePanes{info: herdr.ProcessInfo{ShellPID: 1, ForegroundProcessGroupID: 300, ForegroundProcesses: []herdr.ForegroundProcess{{PID: 300, Name: "vim", Argv: []string{"vim", "notes.md"}}}}}
	w := &Waker{Panes: panes, Clock: &fakeClock{now: t0()}}
	err := w.Wake(context.Background(), sleepingRecord())
	var reused *PaneReusedError
	if !errors.As(err, &reused) || !strings.Contains(err.Error(), "vim notes.md") {
		t.Fatalf("Wake() = %v, want a PaneReusedError naming vim", err)
	}
	if len(panes.ran) != 0 {
		t.Fatalf("launched into a reused pane: %q", panes.ran)
	}
}
