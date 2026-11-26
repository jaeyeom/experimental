package gh

import (
	"context"
	"errors"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestVisibility(t *testing.T) {
	for out, want := range map[string]string{`{"visibility":"PUBLIC"}`: "PUBLIC", `{"visibility":"PRIVATE"}`: "PRIVATE"} {
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "repo", "view", "acme/widgets", "--json", "visibility").WillSucceed(out, 0).Once().Build()
		got, err := NewClient(mock, "gh").Visibility(context.Background(), "acme/widgets")
		if err != nil || got != want {
			t.Fatalf("Visibility = %q, %v; want %q", got, err, want)
		}
	}
	for _, out := range []string{`{}`, `{"visibility":"SECRET"}`, `nope`} {
		mock := executor.NewMockExecutor()
		mock.ExpectCommand("gh").WillSucceed(out, 0).Once().Build()
		if _, err := NewClient(mock, "gh").Visibility(context.Background(), "acme/widgets"); err == nil {
			t.Fatalf("output %q: expected an error", out)
		}
	}
	if _, err := NewClient(executor.NewMockExecutor(), "gh").Visibility(context.Background(), "bad"); err == nil {
		t.Fatal("bad repo: expected an error")
	}
}
