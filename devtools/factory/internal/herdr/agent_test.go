package herdr

import (
	"context"
	"testing"
	"time"
)

func TestWaitAgent(t *testing.T) {
	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "--machine", "mac1", "agent", "wait", "w1:p1", "--until", "working", "--until", "blocked", "--timeout", "15000").
		WillSucceed(`{"id":"cli:agent:wait","result":{"type":"agent_info","agent":{"pane_id":"w1:p1","agent_status":"working"}}}`, 0).Once().Build()
	got, err := NewClient(mock, testHerdrBin).OnMachine("mac1").WaitAgent(context.Background(), "w1:p1", []string{"working", "blocked"}, 15*time.Second)
	if err != nil || got != AgentStatusWorking {
		t.Fatalf("WaitAgent = %q, %v", got, err)
	}
	if _, err := NewClient(mock, testHerdrBin).WaitAgent(context.Background(), "w1:p1", nil, time.Second); err == nil {
		t.Fatal("no --until: expected an error")
	}
}

func TestReadAgentAndSendKeys(t *testing.T) {
	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "agent", "read", "w1:p1", "--source", "visible", "--lines", "60", "--format", "text").
		WillSucceed("> hello\n", 0).Once().Build()
	mock.ExpectCommandWithArgs(testHerdrBin, "agent", "send-keys", "w1:p1", "enter").WillSucceed("{}", 0).Once().Build()
	client := NewClient(mock, testHerdrBin)
	if screen, err := client.ReadAgent(context.Background(), "w1:p1", 60); err != nil || screen != "> hello\n" {
		t.Fatalf("ReadAgent = %q, %v", screen, err)
	}
	if err := client.SendKeys(context.Background(), "w1:p1", "enter"); err != nil {
		t.Fatal(err)
	}
}

func TestErrorCode(t *testing.T) {
	err := &ProcError{ExitCode: 1, Stdout: `{"error":{"code":"agent_prompt_stalled","message":"no working"}}`}
	if got := ErrorCode(err); got != CodePromptStalled {
		t.Fatalf("ErrorCode = %q", got)
	}
	if got := ErrorCode(&RunError{Bin: "herdr"}); got != "" {
		t.Fatalf("ErrorCode(run error) = %q", got)
	}
}
