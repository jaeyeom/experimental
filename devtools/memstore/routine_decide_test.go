package memstore

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestExecDecideSubmitterMissingBinary(t *testing.T) {
	t.Setenv("PATH", t.TempDir())
	sub := &ExecDecideSubmitter{Command: "decide-nonexistent-binary"}
	id, err := sub.Submit(context.Background(), DecideRequest{
		Owner:    "routine:daily",
		Question: "Send digest?",
		Verbatim: "Digest body",
	})
	if err != nil {
		t.Fatalf("Submit with missing binary err = %v, want nil", err)
	}
	if id != "" {
		t.Fatalf("Submit with missing binary id = %q, want empty", id)
	}
}

func TestExecDecideSubmitterFakeCLI(t *testing.T) {
	dir := t.TempDir()
	argsFile := filepath.Join(dir, "args.txt")
	fake := filepath.Join(dir, "decide")
	script := "#!/bin/sh\nprintf '%s\n' \"$@\" > " + argsFile + "\nprintf '{\"id\":\"A-042\"}\\n'\n"
	if err := os.WriteFile(fake, []byte(script), 0o700); err != nil {
		t.Fatalf("write fake decide: %v", err)
	}
	t.Setenv("PATH", dir)

	sub := &ExecDecideSubmitter{NotifyPane: "agentbus:ops"}
	id, err := sub.Submit(context.Background(), DecideRequest{
		Owner:    "routine:inbox",
		Parent:   "D-001",
		Question: "Send email?",
		Verbatim: "Hello world",
	})
	if err != nil {
		t.Fatalf("Submit err = %v", err)
	}
	if id != "A-042" {
		t.Fatalf("id = %q, want A-042", id)
	}

	rawArgs, err := os.ReadFile(argsFile)
	if err != nil {
		t.Fatalf("read args: %v", err)
	}
	got := string(rawArgs)
	for _, want := range []string{"ask", "--class", "approval", "--owner", "routine:inbox", "--verbatim", "Hello world", "--parent", "D-001", "--notify-pane", "agentbus:ops", "--json", "Send email?"} {
		if !strings.Contains(got, want) {
			t.Errorf("args %q missing %q", got, want)
		}
	}
}
