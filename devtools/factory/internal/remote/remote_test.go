package remote

import (
	"context"
	"errors"
	"slices"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestExecuteQuotesEveryArgument(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	want := []string{"-o", "BatchMode=yes", "mac1", "'git'", "'-C'", "'/src/my repo'", "'log'", `'--format=it'\''s %cI'`}
	mock.ExpectCommandWithArgs("ssh", want...).WillSucceed("ok", 0).Once().Build()
	ex, err := New(mock, []string{"ssh", "-o", "BatchMode=yes", "mac1"})
	if err != nil {
		t.Fatal(err)
	}
	res, err := ex.Execute(context.Background(), executor.ToolConfig{Command: "git", Args: []string{"-C", "/src/my repo", "log", "--format=it's %cI"}})
	if err != nil || res.Output != "ok" {
		t.Fatalf("Execute = %+v, %v", res, err)
	}
	if got := mock.Executions()[0].Args; !slices.Equal(got, want) {
		t.Fatalf("args = %q", got)
	}
}

func TestExecuteRejects(t *testing.T) {
	if _, err := New(executor.NewMockExecutor(), nil); err == nil {
		t.Fatal("empty prefix: expected an error")
	}
	if _, err := New(nil, []string{"ssh", "h"}); err == nil {
		t.Fatal("nil executor: expected an error")
	}
	ex, _ := New(executor.NewMockExecutor(), []string{"ssh", "h"})
	if _, err := ex.Execute(context.Background(), executor.ToolConfig{Command: "git", WorkingDir: "/x"}); err == nil {
		t.Fatal("working dir: expected an error")
	}
}
