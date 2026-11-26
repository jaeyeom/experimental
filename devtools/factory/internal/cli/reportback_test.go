package cli

import (
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
)

func TestStartReportBack(t *testing.T) {
	tests := []struct {
		name   string
		whoami func(*executor.MockExecutor)
		want   string
	}{
		{
			name: "registered agentbus name",
			whoami: func(m *executor.MockExecutor) {
				m.ExpectCommandWithArgs("agentbus", "whoami").WillSucceed("pm/coord\n", 0).Once().Build()
			},
			want: `prompt   Do PRJ-1, then agentbus send pm/coord "<message>"…`,
		},
		{
			name: "no agentbus",
			whoami: func(m *executor.MockExecutor) {
				m.ExpectCommand("agentbus").WillError(&executor.ExecutableNotFoundError{Command: "agentbus"}).Once().Build()
			},
			want: `prompt   Do PRJ-1, then herdr agent prompt coord-pane "<message>"…`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfgPath, cwd := writeStartConfig(t, "", "Do {id}, then {report_back}")
			dbPath := filepath.Join(t.TempDir(), "factory.db")
			seedStartItem(t, dbPath, "Fix it", true)
			useSources(t, map[string]*fake.Source{"acme": startFake()})
			mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
			tc.whoami(mock)
			stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--caller", "coord-pane", "--config", cfgPath, "--db", dbPath)
			if code != ExitOK || stderr != "" {
				t.Fatalf("exit = %d, stderr = %q", code, stderr)
			}
			if !strings.Contains(stdout, tc.want+"\n") {
				t.Fatalf("stdout =\n%s\nwant line %q", stdout, tc.want)
			}
			if err := mock.AssertExpectationsMet(); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestStartNoCallerBusPlaceholderSkipsWhoami(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "Do {id} for {caller}")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	if _, stderr, code := runCmd(t, "start", "PRJ-1", "--caller", "c", "--config", cfgPath, "--db", dbPath); code != ExitOK {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
	for _, call := range mock.Executions() {
		if call.Command == "agentbus" {
			t.Fatalf("ran agentbus %v without {caller_bus} or {report_back}", call.Args)
		}
	}
}
