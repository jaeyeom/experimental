package backend

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

type stubHerdr struct {
	tabs   []herdr.Tab
	agents []herdr.Agent
	err    error
	out    herdr.PromptOutcome
}

func (s *stubHerdr) TabList(context.Context) ([]herdr.Tab, error) {
	return s.tabs, s.err
}

func (s *stubHerdr) AgentList(context.Context) ([]herdr.Agent, error) {
	return s.agents, s.err
}

func (s *stubHerdr) Prompt(context.Context, string, string, []string, time.Duration) herdr.PromptOutcome {
	return s.out
}

func TestHerdrBackend(t *testing.T) {
	t.Parallel()
	id := "PROJ-42"
	b := &HerdrBackend{
		Local: &stubHerdr{
			tabs: []herdr.Tab{
				{TabID: "t1", Label: "PROJ-42"},
				{TabID: "t2", Label: "PROJ-99"},
			},
			agents: []herdr.Agent{
				{PaneID: "p1", TabID: "t1", AgentStatus: "idle", Cwd: "/repo"},
				{PaneID: "p2", TabID: "t2", AgentStatus: "blocked", Cwd: "/repo"},
			},
			out: herdr.PromptOutcome{Status: herdr.PromptMatched, Agent: herdr.Agent{AgentStatus: "idle"}},
		},
	}

	if b.Name() != "herdr" {
		t.Fatalf("Name() = %q, want herdr", b.Name())
	}
	sessions, err := b.ListSessions(context.Background())
	if err != nil {
		t.Fatalf("ListSessions() error = %v", err)
	}
	if len(sessions) != 2 || sessions[0].Label != "PROJ-42" || !sessions[1].HasDialog {
		t.Fatalf("ListSessions() = %+v", sessions)
	}

	found, err := b.FindSession(context.Background(), scan.PR{Number: 42, Identifier: &id}, "{id}")
	if err != nil || found == nil || found.ID != "p1" {
		t.Fatalf("FindSession() = %+v, %v", found, err)
	}

	idle, err := b.ReadIdleState(context.Background(), "p1")
	if err != nil || !idle.Idle || idle.HasDialog {
		t.Fatalf("ReadIdleState(p1) = %+v, %v", idle, err)
	}
	blocked, err := b.ReadIdleState(context.Background(), "p2")
	if err != nil || blocked.Idle || !blocked.HasDialog {
		t.Fatalf("ReadIdleState(p2) = %+v, %v", blocked, err)
	}

	res := b.SendPrompt(context.Background(), "p1", "hello", time.Second)
	if res.Status != SendDone {
		t.Fatalf("SendPrompt() = %+v, want done", res)
	}
}

func TestCommandBackend(t *testing.T) {
	t.Parallel()
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("my-mux", "list-sessions").
		WillSucceed(`[{"id":"s1","label":"PROJ-1","status":"idle"}]`, 0).Build()
	mock.ExpectCommandWithArgs("my-mux", "read-idle", "s1").
		WillSucceed(`{"idle":true,"has_dialog":false,"status":"idle"}`, 0).Build()
	mock.ExpectCommandWithArgs("my-mux", "send-prompt", "s1", "5000").
		WillSucceed(`{"status":"done"}`, 0).Build()

	b := &CommandBackend{Exec: mock, Bin: "my-mux"}
	if b.Name() != "command" {
		t.Fatalf("Name() = %q, want command", b.Name())
	}
	id := "PROJ-1"
	s, err := b.FindSession(context.Background(), scan.PR{Number: 1, Identifier: &id}, "{id}")
	if err != nil || s == nil || s.ID != "s1" {
		t.Fatalf("FindSession() = %+v, %v", s, err)
	}
	st, err := b.ReadIdleState(context.Background(), "s1")
	if err != nil || !st.Idle || st.HasDialog {
		t.Fatalf("ReadIdleState() = %+v, %v", st, err)
	}
	res := b.SendPrompt(context.Background(), "s1", "rebase", 5*time.Second)
	if res.Status != SendDone {
		t.Fatalf("SendPrompt() = %+v, want done", res)
	}
}

func TestFakeBackend(t *testing.T) {
	t.Parallel()
	id := "PROJ-7"
	f := &FakeBackend{
		Sessions: []Session{
			{ID: "p7", TabID: "t7", Label: "PROJ-7", Status: "idle"},
		},
		SendResults: map[string]SendResult{
			"p7": {Status: SendTimeout, Detail: "timed out"},
		},
	}
	s, err := f.FindSession(context.Background(), scan.PR{Number: 7, Identifier: &id}, "{id}")
	if err != nil || s == nil || s.ID != "p7" {
		t.Fatalf("FindSession() = %+v, %v", s, err)
	}
	res := f.SendPrompt(context.Background(), "p7", "fix ci", time.Minute)
	if res.Status != SendTimeout {
		t.Fatalf("SendPrompt() = %+v, want timeout", res)
	}
	if len(f.Sent) != 1 || f.Sent[0].Prompt != "fix ci" {
		t.Fatalf("Sent = %+v", f.Sent)
	}

	f.ListErr = errors.New("boom")
	if _, err := f.ListSessions(context.Background()); err == nil {
		t.Fatal("expected error from ListSessions")
	}
}
