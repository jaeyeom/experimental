package github

import (
	"context"
	"fmt"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
	cmdexec "github.com/jaeyeom/go-cmdexec"
)

func TestPullRequestSnapshotsOneGraphQLCall(t *testing.T) {
	body := `{"data":{"p0":{"pullRequest":{"state":"OPEN","author":{"login":"alice"},"title":"One"}},"p1":{"pullRequest":{"state":"CLOSED","author":{"login":"bob"},"title":"Two"}}}}`
	exec := &scriptedExecutor{outputs: []string{body}}
	got, err := NewClientWithExecutor(exec).PullRequestSnapshots([]PullRef{
		{Owner: "acme", Repo: "widgets", Number: 1, Raw: "acme/widgets#1"},
		{Owner: "acme", Repo: "gadgets", Number: 2, Raw: "acme/gadgets#2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(exec.calls) != 1 {
		t.Fatalf("calls = %d, want 1", len(exec.calls))
	}
	query := strings.Join(exec.calls[0].args, " ")
	if !strings.Contains(query, "api graphql") || !strings.Contains(query, "p0:repository") || !strings.Contains(query, "p1:repository") || !strings.Contains(query, "author{login}") {
		t.Fatalf("query = %s", query)
	}
	if got["acme/widgets#1"].State != "OPEN" || got["acme/widgets#1"].Author != "alice" || got["acme/gadgets#2"].State != "CLOSED" {
		t.Fatalf("snapshots = %+v", got)
	}
}

func TestPullRequestSnapshotsSplitsAfterFifty(t *testing.T) {
	refs := make([]PullRef, 51)
	var first, second strings.Builder
	first.WriteString(`{"data":{`)
	for i := range refs {
		refs[i] = PullRef{Owner: "acme", Repo: "widgets", Number: i + 1, Raw: fmt.Sprintf("acme/widgets#%d", i+1)}
	}
	for i := 0; i < 50; i++ {
		if i > 0 {
			first.WriteByte(',')
		}
		fmt.Fprintf(&first, `"p%d":{"pullRequest":{"state":"OPEN","author":{"login":"a"},"title":"t"}}`, i)
	}
	first.WriteString(`}}`)
	second.WriteString(`{"data":{"p0":{"pullRequest":{"state":"MERGED","author":{"login":"z"},"title":"last"}}}}`)
	exec := &scriptedExecutor{outputs: []string{first.String(), second.String()}}
	got, err := NewClientWithExecutor(exec).PullRequestSnapshots(refs)
	if err != nil {
		t.Fatal(err)
	}
	if len(exec.calls) != 2 {
		t.Fatalf("calls = %d, want 2", len(exec.calls))
	}
	if strings.Contains(strings.Join(exec.calls[0].args, " "), "p50:") {
		t.Fatal("first query includes a 51st alias")
	}
	if got["acme/widgets#51"].State != "MERGED" || got["acme/widgets#1"].State != "OPEN" {
		t.Fatalf("last = %+v first = %+v", got["acme/widgets#51"], got["acme/widgets#1"])
	}
}

func TestPullRequestSnapshotsUnreadableAndGraphQLError(t *testing.T) {
	missing := `{"data":{"p0":{"pullRequest":null}},"errors":[{"type":"NOT_FOUND","path":["p0","pullRequest"],"message":"Could not resolve"}]}`
	exec := &scriptedExecutor{outputs: []string{missing}}
	got, err := NewClientWithExecutor(exec).PullRequestSnapshots([]PullRef{{Owner: "acme", Repo: "widgets", Number: 9, Raw: "acme/widgets#9"}})
	if err != nil {
		t.Fatal(err)
	}
	if !got["acme/widgets#9"].Unreadable {
		t.Fatalf("snapshot = %+v", got["acme/widgets#9"])
	}
	bad := `{"data":null,"errors":[{"type":"RATE_LIMITED","message":"slow down"}]}`
	exec = &scriptedExecutor{outputs: []string{bad}}
	if _, err := NewClientWithExecutor(exec).PullRequestSnapshots([]PullRef{{Owner: "acme", Repo: "widgets", Number: 9, Raw: "acme/widgets#9"}}); err == nil || !strings.Contains(err.Error(), "slow down") {
		t.Fatalf("err = %v", err)
	}
}

type snapshotExec struct {
	cmd  string
	args []string
	n    int
}

func (s *snapshotExec) Execute(_ context.Context, cfg cmdexec.ToolConfig) (*cmdexec.ExecutionResult, error) {
	s.n++
	s.cmd = cfg.Command
	s.args = append([]string(nil), cfg.Args...)
	return &cmdexec.ExecutionResult{Output: `{"data":{"p0":{"pullRequest":{"state":"OPEN","author":{"login":"alice"},"title":"One"}},"p1":{"pullRequest":{"state":"MERGED","author":{"login":"bob"},"title":"Two"}}}}`}, nil
}

func (s *snapshotExec) IsAvailable(string) bool { return true }

func TestPullRequestSnapshotsUsesGHBin(t *testing.T) {
	t.Setenv(ghcli.BinEnv, "/opt/bin/gh-throttle")
	exec := &snapshotExec{}
	got, err := NewClient(context.Background(), GHExecutor(exec)).PullRequestSnapshots([]PullRef{
		{Owner: "acme", Repo: "widgets", Number: 1, Raw: "acme/widgets#1"},
		{Owner: "acme", Repo: "gadgets", Number: 2, Raw: "acme/gadgets#2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	if exec.n != 1 || exec.cmd != "/opt/bin/gh-throttle" {
		t.Fatalf("calls = %d cmd = %q", exec.n, exec.cmd)
	}
	if got["acme/widgets#1"].Author != "alice" || got["acme/gadgets#2"].State != "MERGED" {
		t.Fatalf("snapshots = %+v", got)
	}
}

func TestPullRequestSnapshotsEmptyMakesNoCall(t *testing.T) {
	exec := &scriptedExecutor{}
	got, err := NewClientWithExecutor(exec).PullRequestSnapshots(nil)
	if err != nil || len(got) != 0 || len(exec.calls) != 0 {
		t.Fatalf("got %v err %v calls %d", got, err, len(exec.calls))
	}
}
