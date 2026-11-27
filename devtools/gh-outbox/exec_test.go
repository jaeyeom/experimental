package ghoutbox

import (
	"context"
	"encoding/json"
	"errors"
	"slices"
	"strings"
	"testing"
)

type runCall struct {
	args  []string
	stdin string
}

type fakeRunner struct {
	calls []runCall
	out   string
}

func (f *fakeRunner) run(_ context.Context, stdin []byte, args ...string) ([]byte, error) {
	f.calls = append(f.calls, runCall{args, string(stdin)})
	return []byte(f.out), nil
}

func TestGHCLICreateThread(t *testing.T) {
	f := &fakeRunner{out: `{"html_url":"https://github.com/o/r/pull/7#discussion_r1"}`}
	text := "a\n\n  b\r\n"
	url, err := GHCLI{Run: f.run}.CreateThread(context.Background(), "o/r", 7, "sha1", LineComment{Path: "a.go", Line: 3, Text: text})
	if err != nil || url != "https://github.com/o/r/pull/7#discussion_r1" {
		t.Fatalf("url = %q err = %v", url, err)
	}
	c := f.calls[0]
	if strings.Join(c.args, " ") != "api --method POST repos/o/r/pulls/7/comments --input -" {
		t.Errorf("args = %q", c.args)
	}
	var got map[string]any
	if err := json.Unmarshal([]byte(c.stdin), &got); err != nil {
		t.Fatal(err)
	}
	if got["body"] != text || got["commit_id"] != "sha1" || got["path"] != "a.go" || got["line"] != 3.0 || got["side"] != "RIGHT" {
		t.Errorf("payload = %v", got)
	}
	if _, ok := got["event"]; ok {
		t.Errorf("a thread payload has an event: %v", got)
	}
}

func TestGHCLICreateReview(t *testing.T) {
	f := &fakeRunner{out: `{"html_url":"u"}`}
	_, err := GHCLI{Run: f.run}.CreateReview(context.Background(), "o/r", 7, "sha1", "REQUEST_CHANGES", "fix\n", []LineComment{{Path: "a.go", Line: 3, Text: "here"}})
	if err != nil {
		t.Fatal(err)
	}
	c := f.calls[0]
	if strings.Join(c.args, " ") != "api --method POST repos/o/r/pulls/7/reviews --input -" {
		t.Errorf("args = %q", c.args)
	}
	want := `{"body":"fix\n","commit_id":"sha1","event":"REQUEST_CHANGES","comments":[{"body":"here","path":"a.go","line":3,"side":"RIGHT"}]}`
	if c.stdin != want {
		t.Errorf("payload = %s\nwant      %s", c.stdin, want)
	}
}

func TestGHCLICreateReviewWithMultiLineAndLeftSide(t *testing.T) {
	f := &fakeRunner{out: `{"html_url":"u"}`}
	_, err := GHCLI{Run: f.run}.CreateReview(context.Background(), "o/r", 7, "sha1", "COMMENT", "", []LineComment{
		{Path: "old.go", StartLine: 4, Line: 9, Side: "LEFT", Text: "range note"},
	})
	if err != nil {
		t.Fatal(err)
	}
	want := `{"commit_id":"sha1","event":"COMMENT","comments":[{"body":"range note","path":"old.go","line":9,"start_line":4,"side":"LEFT"}]}`
	if got := f.calls[0].stdin; got != want {
		t.Errorf("payload = %s\nwant      %s", got, want)
	}
}

func TestGHCLIEditsAndReads(t *testing.T) {
	f := &fakeRunner{out: `{"html_url":"u","body":"old","head":{"sha":"abc"}}`}
	g := GHCLI{Run: f.run}
	ctx := context.Background()
	if _, err := g.EditComment(ctx, "o/r", "5", "new"); err != nil {
		t.Fatal(err)
	}
	if _, err := g.EditReview(ctx, "o/r", 7, "6", "new"); err != nil {
		t.Fatal(err)
	}
	sha, _ := g.HeadSHA(ctx, "o/r", 7)
	body, _ := g.CommentBody(ctx, "o/r", "5")
	rbody, _ := g.ReviewBody(ctx, "o/r", 7, "6")
	if sha != "abc" || body != "old" || rbody != "old" {
		t.Errorf("sha = %q body = %q review = %q", sha, body, rbody)
	}
	want := []string{
		"api --method PATCH repos/o/r/pulls/comments/5 --input -",
		"api --method PUT repos/o/r/pulls/7/reviews/6 --input -",
		"api repos/o/r/pulls/7",
		"api repos/o/r/pulls/comments/5",
		"api repos/o/r/pulls/7/reviews/6",
	}
	for i, w := range want {
		if got := strings.Join(f.calls[i].args, " "); got != w {
			t.Errorf("call %d = %q, want %q", i, got, w)
		}
	}
	if f.calls[0].stdin != `{"body":"new"}` {
		t.Errorf("edit payload = %s", f.calls[0].stdin)
	}
}

func TestGHCLICreateReply(t *testing.T) {
	f := &fakeRunner{out: `{"html_url":"https://github.com/o/r/pull/7#discussion_r9"}`}
	url, err := GHCLI{Run: f.run}.CreateReply(context.Background(), "o/r", 7, "42", "done\n")
	if err != nil || url != "https://github.com/o/r/pull/7#discussion_r9" {
		t.Fatalf("url = %q err = %v", url, err)
	}
	c := f.calls[0]
	if strings.Join(c.args, " ") != "api --method POST repos/o/r/pulls/7/comments/42/replies --input -" {
		t.Errorf("args = %q", c.args)
	}
	if c.stdin != `{"body":"done\n"}` {
		t.Errorf("payload = %s", c.stdin)
	}

	notFound := func(_ context.Context, _ []byte, _ ...string) ([]byte, error) {
		return nil, errors.New("gh api: exit status 1: gh: Not Found (HTTP 404)")
	}
	if _, err := (GHCLI{Run: notFound}).CreateReply(context.Background(), "o/r", 7, "42", "done"); !errors.Is(err, ErrReplyNotFound) {
		t.Errorf("err = %v, want ErrReplyNotFound", err)
	}
}

func TestGHCLIReplyTarget(t *testing.T) {
	page1 := `{"data":{"repository":{"pullRequest":{"reviewThreads":{
		"pageInfo":{"hasNextPage":true,"endCursor":"C1"},
		"nodes":[{"id":"T1","isResolved":false,"path":"other.go","line":1,"originalLine":1,"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[{"databaseId":10,"author":{"login":"bob"},"body":"hi"}]}}]
	}}}}}`
	page2 := `{"data":{"repository":{"pullRequest":{"reviewThreads":{
		"pageInfo":{"hasNextPage":false,"endCursor":""},
		"nodes":[{
			"id":"T2","isResolved":true,"path":"a.go","line":null,"originalLine":12,
			"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[
				{"databaseId":42,"author":{"login":"alice"},"body":"first comment"},
				{"databaseId":43,"author":{"login":"bob"},"body":"follow-up comment"}
			]}
		}]
	}}}}}`
	var calls [][]string
	run := func(_ context.Context, _ []byte, args ...string) ([]byte, error) {
		calls = append(calls, append([]string(nil), args...))
		if slices.Contains(args, "cursor=C1") {
			return []byte(page2), nil
		}
		return []byte(page1), nil
	}
	g := GHCLI{Run: run}
	got, err := g.ReplyTarget(context.Background(), "o/r", 7, "43")
	if err != nil {
		t.Fatal(err)
	}
	if got.RootID != "42" || got.Path != "a.go" || got.Line != 12 || got.Starter != "alice" || got.CommentBody != "follow-up comment" || !got.Resolved {
		t.Errorf("got = %+v", got)
	}
	if len(calls) != 2 {
		t.Errorf("calls = %d, want 2", len(calls))
	}
	if _, err := g.ReplyTarget(context.Background(), "o/r", 7, "999"); !errors.Is(err, ErrReplyNotFound) {
		t.Errorf("missing id err = %v, want ErrReplyNotFound", err)
	}
}

func TestDecideCLIAsk(t *testing.T) {
	f := &fakeRunner{out: `{"id":"D-9"}`}
	id, err := DecideCLI{Run: f.run}.Ask(context.Background(), "pm/x", "pane-1", "Q?\n\nbody")
	if err != nil || id != "D-9" {
		t.Fatalf("id = %q err = %v", id, err)
	}
	got := strings.Join(f.calls[0].args, "|")
	want := "ask|--long-lines|--owner|pm/x|--notify-pane|pane-1|--kind|relay|--option|post:Post exactly this|--option|edit:Edit (write notes for a redraft)|--option|cancel:Cancel|--free-text|--json|Q?\n\nbody"
	if got != want {
		t.Errorf("args = %q\nwant   %q", got, want)
	}
	f.calls = nil
	if _, err := (DecideCLI{Run: f.run}).Ask(context.Background(), "pm/x", "", "Q"); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(strings.Join(f.calls[0].args, " "), "--notify-pane") {
		t.Errorf("args = %q, want no --notify-pane", f.calls[0].args)
	}
}

func TestDecideCLIAskRetriesWithoutLongLinesOnAnOlderDecide(t *testing.T) {
	var calls [][]string
	run := func(_ context.Context, _ []byte, args ...string) ([]byte, error) {
		calls = append(calls, append([]string(nil), args...))
		if slices.Contains(args, "--long-lines") {
			return nil, errors.New("decide ask: exit status 2: flag provided but not defined: -long-lines")
		}
		return []byte(`{"id":"D-9"}`), nil
	}
	id, err := DecideCLI{Run: run}.Ask(context.Background(), "pm/x", "", "Q")
	if err != nil || id != "D-9" {
		t.Fatalf("id = %q err = %v", id, err)
	}
	if len(calls) != 2 || slices.Contains(calls[1], "--long-lines") {
		t.Fatalf("calls = %q", calls)
	}
	if !slices.Equal(calls[1], slices.DeleteFunc(slices.Clone(calls[0]), func(a string) bool { return a == "--long-lines" })) {
		t.Fatalf("retry changed more than the flag: %q", calls)
	}
}

func TestAgentbusCLI(t *testing.T) {
	f := &fakeRunner{out: `[{"name":"coord/primary"},{"name":"pm/x"}]`}
	a := AgentbusCLI{Run: f.run}
	names, err := a.Names(context.Background())
	if err != nil || strings.Join(names, ",") != "coord/primary,pm/x" {
		t.Fatalf("names = %v err = %v", names, err)
	}
	if err := a.Send(context.Background(), "pm/x", "--hello\nthere"); err != nil {
		t.Fatal(err)
	}
	c := f.calls[1]
	if strings.Join(c.args, " ") != "send pm/x" || c.stdin != "--hello\nthere" {
		t.Errorf("send = %+v", c)
	}
}
