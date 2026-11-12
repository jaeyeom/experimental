package github

import (
	"errors"
	"strings"
	"testing"
	"time"
)

// quietClient returns a client over exec that records sleeps and warnings
// instead of sleeping and logging.
func quietClient(exec CommandExecutor) (*Client, *[]time.Duration, *[]string) {
	var slept []time.Duration
	var warnings []string
	client := NewClientWithExecutor(exec)
	client.sleep = func(d time.Duration) { slept = append(slept, d) }
	client.warn = func(msg string, _ ...any) { warnings = append(warnings, msg) }
	return client, &slept, &warnings
}

func TestPullRequestListRetriesEmptyStdoutOnce(t *testing.T) {
	exec := &separatedMock{calls: []ghCall{
		{stdout: "", stderr: "gh: hiccup"},
		{stdout: `[{"url":"https://github.com/o/r/pull/1"}]`},
	}}
	client, slept, warnings := quietClient(exec)
	prs, err := client.GetPendingPullRequests()
	if err != nil {
		t.Fatalf("GetPendingPullRequests() error = %v", err)
	}
	if len(prs) != 1 {
		t.Fatalf("len(prs) = %d, want 1", len(prs))
	}
	if exec.n != 2 {
		t.Fatalf("gh calls = %d, want 2", exec.n)
	}
	if len(*warnings) != 1 {
		t.Fatalf("warnings = %q, want one", *warnings)
	}
	if len(*slept) != 1 || (*slept)[0] != 2*time.Second {
		t.Fatalf("slept = %v, want [2s]", *slept)
	}
}

func TestPullRequestListEmptyTwiceIsDiagnostic(t *testing.T) {
	exec := &separatedMock{calls: []ghCall{
		{stdout: "", stderr: "gh: first line\nsecond line"},
		{stdout: "\n", stderr: "gh: first line\nsecond line"},
	}}
	client, _, warnings := quietClient(exec)
	_, err := client.GetPendingPullRequests()
	if err == nil {
		t.Fatal("expected error when stdout is empty twice")
	}
	if exec.n != 2 {
		t.Fatalf("gh calls = %d, want 2", exec.n)
	}
	if len(*warnings) != 1 {
		t.Fatalf("warnings = %q, want one", *warnings)
	}
	msg := err.Error()
	for _, want := range []string{
		"parse gh pr list --author _ --limit _ --json _ output",
		"exit 0, stdout 1 bytes",
		`stderr: "gh: first line"`,
	} {
		if !strings.Contains(msg, want) {
			t.Errorf("error = %q, want it to contain %q", msg, want)
		}
	}
	if strings.Contains(msg, "second line") {
		t.Errorf("error = %q, want only the first stderr line", msg)
	}
}

func TestEmptyStdoutErrorShowsZeroBytes(t *testing.T) {
	exec := &separatedMock{calls: []ghCall{{stderr: "gh: x"}, {stderr: "gh: x"}}}
	client, _, _ := quietClient(exec)
	_, err := client.MergeableState("https://github.com/o/r/pull/1")
	if err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(err.Error(), "exit 0, stdout 0 bytes") {
		t.Fatalf("error = %q, want exit 0, stdout 0 bytes", err)
	}
	if !strings.Contains(err.Error(), "unexpected end of JSON input") {
		t.Fatalf("error = %q, want the JSON error", err)
	}
}

func TestTruncatedJSONIsNotRetried(t *testing.T) {
	exec := &separatedMock{calls: []ghCall{{stdout: `[{"a":`}}}
	client, slept, warnings := quietClient(exec)
	_, err := client.GetPendingPullRequests()
	if err == nil {
		t.Fatal("expected error for truncated JSON")
	}
	if exec.n != 1 {
		t.Fatalf("gh calls = %d, want 1", exec.n)
	}
	if len(*warnings) != 0 || len(*slept) != 0 {
		t.Fatalf("warnings = %q, slept = %v, want none", *warnings, *slept)
	}
	msg := err.Error()
	for _, want := range []string{"stdout 6 bytes", `head "[{\"a\":"`, "unexpected end of JSON input"} {
		if !strings.Contains(msg, want) {
			t.Errorf("error = %q, want it to contain %q", msg, want)
		}
	}
}

func TestParseErrorHeadIsEscapedAndCapped(t *testing.T) {
	stdout := "\x1b[31m" + strings.Repeat("x", 300)
	exec := &separatedMock{calls: []ghCall{{stdout: stdout}}}
	client, _, _ := quietClient(exec)
	_, err := client.MergeableState("https://github.com/o/r/pull/1")
	if err == nil {
		t.Fatal("expected error")
	}
	msg := err.Error()
	if strings.Contains(msg, "\x1b") {
		t.Fatalf("error contains a raw control character: %q", msg)
	}
	if !strings.Contains(msg, `head "\x1b[31m`) {
		t.Fatalf("error = %q, want the escaped head", msg)
	}
	if strings.Contains(msg, strings.Repeat("x", 196)) {
		t.Fatalf("error = %q, want the head capped at 200 bytes", msg)
	}
	if !strings.Contains(msg, "stdout 305 bytes") {
		t.Fatalf("error = %q, want the full stdout length", msg)
	}
}

func TestGHAPIRetriesEmptyStdoutOnce(t *testing.T) {
	exec := &separatedMock{calls: []ghCall{{}, {stdout: "[]"}}}
	client, _, warnings := quietClient(exec)
	if _, _, err := client.LandedFirst("o", "r", "abc", 1); err != nil {
		t.Fatalf("LandedFirst() error = %v", err)
	}
	if exec.n != 2 || len(*warnings) != 1 {
		t.Fatalf("gh calls = %d, warnings = %q, want 2 calls and one warning", exec.n, *warnings)
	}
}

func TestGraphQLEmptyTwiceIsDiagnostic(t *testing.T) {
	exec := &separatedMock{calls: []ghCall{{}, {}}}
	client, _, _ := quietClient(exec)
	_, err := client.ViewerLogin()
	if err == nil {
		t.Fatal("expected error")
	}
	msg := err.Error()
	if !strings.Contains(msg, "parse gh api graphql -f _ output") || !strings.Contains(msg, "exit 0, stdout 0 bytes") {
		t.Fatalf("error = %q, want the redacted command and diagnostics", msg)
	}
}

func TestGHJSONNonZeroExitIsNotRetriedByHelper(t *testing.T) {
	exec := &separatedMock{calls: []ghCall{{stderr: "gh: boom", err: errExit1}}}
	client, _, warnings := quietClient(exec)
	if _, err := client.MergeableState("https://github.com/o/r/pull/1"); err == nil {
		t.Fatal("expected error")
	}
	if exec.n != 1 || len(*warnings) != 0 {
		t.Fatalf("gh calls = %d, warnings = %q, want 1 call and no warning", exec.n, *warnings)
	}
}

var errExit1 = errors.New("exit status 1")
