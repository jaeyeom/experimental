package main

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

// lockedBuffer is a log the runner writes while the test reads it.
type lockedBuffer struct {
	mu sync.Mutex
	b  bytes.Buffer
}

func (l *lockedBuffer) Write(p []byte) (int, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.b.Write(p) //nolint:errcheck // a bytes.Buffer write never fails
	return len(p), nil
}

func (l *lockedBuffer) String() string {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.b.String()
}

func TestJobsRunnerRunsACLIJobThroughTheDaemon(t *testing.T) {
	t.Parallel()
	env := jobsWorld(t)
	out := filepath.Join(t.TempDir(), "ran")
	cfg := filepath.Join(t.TempDir(), "runner.json")
	conf := `{"poll": "20ms", "kinds": {"rebase": {"command": ["sh", "-c", "echo \"$JOB_ID {subject}\" > ` + out + `"]}}}`
	if err := os.WriteFile(cfg, []byte(conf), 0o600); err != nil {
		t.Fatal(err)
	}
	mustCmd(t, []string{"jobs", "enqueue", "--kind", "rebase", "--subject", "o/r#1"}, env)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var log lockedBuffer
	done := make(chan int)
	go func() {
		done <- jobsRunner(ctx, []string{"--name", "tiger-runner", "--machine", "tiger", "--config", cfg}, env, &log)
	}()
	deadline := time.Now().Add(10 * time.Second)
	for {
		stdout, _ := mustCmd(t, []string{"jobs", "show", "j-1"}, env)
		if strings.Contains(stdout, "state: done") {
			break
		}
		if time.Now().After(deadline) {
			t.Fatalf("job not done:\n%s\nlog:\n%s", stdout, log.String())
		}
		time.Sleep(20 * time.Millisecond)
	}
	cancel()
	if code := <-done; code != 0 {
		t.Fatalf("runner exit %d; log:\n%s", code, log.String())
	}
	if data, _ := os.ReadFile(out); string(data) != "j-1 o/r#1\n" {
		t.Fatalf("command wrote %q", data)
	}
}

func TestJobsRunnerUsage(t *testing.T) {
	t.Parallel()
	env := jobsWorld(t)
	if code, _, _ := runCmd([]string{"jobs", "runner"}, env); code != 2 {
		t.Errorf("runner without --name = %d, want 2", code)
	}
	if code, _, stderr := runCmd([]string{"jobs", "runner", "--name", "r", "--config", filepath.Join(t.TempDir(), "none.json")}, env); code != 1 || !strings.Contains(stderr, "runner config") {
		t.Errorf("runner with a missing config = %d %q, want 1", code, stderr)
	}
}
