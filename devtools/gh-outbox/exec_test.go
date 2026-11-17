package ghoutbox

import (
	"context"
	"encoding/json"
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

func TestDecideCLIAsk(t *testing.T) {
	f := &fakeRunner{out: `{"id":"D-9"}`}
	id, err := DecideCLI{Run: f.run}.Ask(context.Background(), "pm/x", "pane-1", "Q?\n\nbody")
	if err != nil || id != "D-9" {
		t.Fatalf("id = %q err = %v", id, err)
	}
	got := strings.Join(f.calls[0].args, "|")
	want := "ask|--owner|pm/x|--notify-pane|pane-1|--kind|relay|--option|post:Post exactly this|--option|edit:Edit (write notes for a redraft)|--option|cancel:Cancel|--free-text|--json|Q?\n\nbody"
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
