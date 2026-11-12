package github

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
	cmdexec "github.com/jaeyeom/go-cmdexec"
)

// recordingExecutor records the commands it is asked to run.
type recordingExecutor struct {
	commands  []string
	available []string
}

func (r *recordingExecutor) Execute(_ context.Context, cfg cmdexec.ToolConfig) (*cmdexec.ExecutionResult, error) {
	r.commands = append(r.commands, cfg.Command)
	return &cmdexec.ExecutionResult{}, nil
}

func (r *recordingExecutor) IsAvailable(command string) bool {
	r.available = append(r.available, command)
	return true
}

func TestGHExecutorRunsFakeFromGHBin(t *testing.T) {
	fake := filepath.Join(t.TempDir(), "fake-gh")
	if err := os.WriteFile(fake, []byte("#!/bin/sh\necho \"fake-gh $*\"\n"), 0o700); err != nil { //nolint:gosec // G306: the fake must be executable
		t.Fatal(err)
	}
	t.Setenv(ghcli.BinEnv, fake)

	out, err := cmdexec.Output(context.Background(), GHExecutor(cmdexec.NewBasicExecutor()), "gh", "pr", "list")
	if err != nil {
		t.Fatalf("Output: %v", err)
	}
	if got := strings.TrimSpace(string(out)); got != "fake-gh pr list" {
		t.Fatalf("output = %q, want %q", got, "fake-gh pr list")
	}
}

func TestGHExecutorRewritesOnlyGH(t *testing.T) {
	t.Setenv(ghcli.BinEnv, "/opt/bin/gh-throttle")
	rec := &recordingExecutor{}
	exec := GHExecutor(rec)
	ctx := context.Background()
	for _, cmd := range []string{"gh", "git"} {
		if _, err := exec.Execute(ctx, cmdexec.ToolConfig{Command: cmd}); err != nil {
			t.Fatalf("Execute(%s): %v", cmd, err)
		}
	}
	exec.IsAvailable("gh")
	if want := []string{"/opt/bin/gh-throttle", "git"}; !slices.Equal(rec.commands, want) {
		t.Fatalf("commands = %v, want %v", rec.commands, want)
	}
	if want := []string{"/opt/bin/gh-throttle"}; !slices.Equal(rec.available, want) {
		t.Fatalf("IsAvailable got %v, want %v", rec.available, want)
	}
}

func TestGHExecutorDefaultRunsGH(t *testing.T) {
	t.Setenv(ghcli.BinEnv, "")
	rec := &recordingExecutor{}
	if _, err := GHExecutor(rec).Execute(context.Background(), cmdexec.ToolConfig{Command: "gh"}); err != nil {
		t.Fatal(err)
	}
	if want := []string{"gh"}; !slices.Equal(rec.commands, want) {
		t.Fatalf("commands = %v, want %v", rec.commands, want)
	}
}
