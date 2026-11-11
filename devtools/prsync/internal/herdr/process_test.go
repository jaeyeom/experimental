package herdr

import (
	"context"
	"errors"
	"testing"
)

// paneProcessInfoJSON is the shape `herdr pane process-info` prints.
const paneProcessInfoJSON = `{"id":"cli:pane:process_info","result":{"process_info":{
  "foreground_process_group_id":94342,
  "foreground_processes":[
    {"argv":["caffeinate","-i"],"argv0":"caffeinate","cmdline":"caffeinate -i","cwd":"/src/repo","name":"caffeinate","pid":30296},
    {"argv":["/usr/local/bin/claude"],"argv0":"claude","cmdline":"/usr/local/bin/claude","cwd":"/src/repo","name":"2.1.319","pid":94342}
  ],
  "pane_id":"w2:pC","shell_pid":61296},"type":"pane_process_info"}}`

func TestPaneProcessInfo(t *testing.T) {
	t.Parallel()

	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "pane", "process-info", "--pane", "w2:pC").
		WillSucceed(paneProcessInfoJSON, 0).Build()
	got, err := NewClient(mock, testHerdrBin).PaneProcessInfo(context.Background(), "w2:pC")
	if err != nil {
		t.Fatalf("PaneProcessInfo() unexpected error: %v", err)
	}
	if got.PaneID != "w2:pC" || got.ShellPID != 61296 || got.ForegroundProcessGroupID != 94342 {
		t.Fatalf("PaneProcessInfo() = %+v", got)
	}
	if len(got.ForegroundProcesses) != 2 {
		t.Fatalf("foreground processes = %+v, want 2", got.ForegroundProcesses)
	}
	p := got.ForegroundProcesses[1]
	if p.PID != 94342 || p.Name != "2.1.319" || p.Cwd != "/src/repo" || len(p.Argv) != 1 || p.Argv[0] != "/usr/local/bin/claude" {
		t.Fatalf("foreground process = %+v", p)
	}
}

func TestPaneProcessInfoMissingResult(t *testing.T) {
	t.Parallel()

	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "pane", "process-info", "--pane", "w2:pC").
		WillSucceed(`{"result":{}}`, 0).Build()
	_, err := NewClient(mock, testHerdrBin).PaneProcessInfo(context.Background(), "w2:pC")
	if !errors.Is(err, ErrDegraded) {
		t.Fatalf("PaneProcessInfo() error = %v, want ErrDegraded", err)
	}
}
