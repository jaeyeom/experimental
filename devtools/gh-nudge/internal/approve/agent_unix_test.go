//go:build unix

package approve

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"golang.org/x/sys/unix"
)

func TestRunAgentTimeoutKillsProcessGroup(t *testing.T) {
	pidFile := filepath.Join(t.TempDir(), "child.pid")
	rule := shellRule(`sleep 30 & echo $! > "$1"; wait`, pidFile)
	rule.AgentTimeout = 300 * time.Millisecond
	if got := runAgent(ExecCommand, rule, sampleInput()); got.Decision != AgentDefer {
		t.Fatalf("result = %+v", got)
	}
	data, err := os.ReadFile(pidFile)
	if err != nil {
		t.Fatal(err)
	}
	pid, err := strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil {
		t.Fatal(err)
	}
	deadline := time.Now().Add(3 * time.Second)
	for unix.Kill(pid, 0) == nil {
		if time.Now().After(deadline) {
			_ = unix.Kill(pid, unix.SIGKILL)
			t.Fatalf("child %d of the agent command survived the timeout", pid)
		}
		time.Sleep(20 * time.Millisecond)
	}
}
