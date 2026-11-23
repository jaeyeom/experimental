package installcheck

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

type fakeIssue struct {
	Number      int    `json:"number"`
	Title       string `json:"title"`
	State       string `json:"state"`
	StateReason string `json:"stateReason"`
	ClosedAt    string `json:"closedAt,omitempty"`
}

type fakeWorld struct {
	heads   map[string]string // url -> sha
	lsErr   map[string]error
	issues  map[string][]fakeIssue // slug -> issues
	queries map[string][]int       // slug -> numbers matched by the query
	ghErr   error
	sendErr error
	calls   []string
	sent    []string
}

func (w *fakeWorld) run(_ context.Context, name string, args ...string) ([]byte, error) {
	w.calls = append(w.calls, name+" "+strings.Join(args, " "))
	switch name {
	case "git":
		url := args[1]
		if err := w.lsErr[url]; err != nil {
			return nil, err
		}
		return []byte(w.heads[url] + "\trefs/heads/main\n"), nil
	case "gh":
		return w.gh(args)
	case "agentbus":
		if w.sendErr != nil {
			return nil, w.sendErr
		}
		w.sent = append(w.sent, args[1]+"|"+args[2])
		return nil, nil
	}
	return nil, fmt.Errorf("unexpected command %s", name)
}

func (w *fakeWorld) gh(args []string) ([]byte, error) {
	if w.ghErr != nil {
		return nil, w.ghErr
	}
	flag := func(name string) string {
		for i, a := range args {
			if a == name && i+1 < len(args) {
				return args[i+1]
			}
		}
		return ""
	}
	slug := flag("-R")
	if args[1] == "view" {
		n, _ := strconv.Atoi(args[2])
		for _, is := range w.issues[slug] {
			if is.Number == n {
				return marshal(is)
			}
		}
		return nil, errors.New("not found")
	}
	want := strings.ToUpper(flag("--state"))
	var out []fakeIssue
	for _, n := range w.queries[slug] {
		for _, is := range w.issues[slug] {
			if is.Number == n && is.State == want {
				out = append(out, is)
			}
		}
	}
	return marshal(out)
}

func marshal(v any) ([]byte, error) {
	b, err := json.Marshal(v)
	if err != nil {
		return nil, fmt.Errorf("marshal: %w", err)
	}
	return b, nil
}

func (w *fakeWorld) ghCalls() int {
	n := 0
	for _, c := range w.calls {
		if strings.HasPrefix(c, "gh ") {
			n++
		}
	}
	return n
}

func closed(n int, title, reason string) fakeIssue {
	return fakeIssue{Number: n, Title: title, State: "CLOSED", StateReason: reason, ClosedAt: "2026-11-22T12:40:00Z"}
}

const repoURL = "https://github.com/o/r"

func newWorld() *fakeWorld {
	return &fakeWorld{
		heads: map[string]string{repoURL: "aaaaaaaaaaaa"},
		issues: map[string][]fakeIssue{"o/r": {
			{Number: 123, Title: "open one", State: "OPEN"},
			{Number: 130, Title: "open two", State: "OPEN"},
		}},
		queries: map[string][]int{},
	}
}

func baseInput(w *fakeWorld, st State) Input {
	return Input{
		Config: Config{
			NotifyName: "agent",
			Repos:      []RepoConfig{{URL: repoURL, Issues: []int{123, 130}}},
		},
		State: st,
		Now:   time.Date(2026, 11, 22, 13, 0, 0, 0, time.UTC),
		Run:   w.run,
	}
}

// baseline runs once so the repo has a recorded head and baseline.
func baseline(t *testing.T, w *fakeWorld, in Input) State {
	t.Helper()
	res := Check(context.Background(), in)
	if len(res.Errors) != 0 || res.Sent {
		t.Fatalf("baseline: %+v", res)
	}
	return res.Next
}

func TestUnchangedHeadMakesNoAPICallsAndNoMessage(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.calls = nil
	res := Check(context.Background(), baseInput(w, st))
	if res.Sent || w.ghCalls() != 0 || len(w.sent) != 0 || len(res.Errors) != 0 {
		t.Fatalf("calls=%v sent=%v res=%+v", w.calls, w.sent, res)
	}
}

func TestMovedHeadNoClosedIssueUpdatesStateOnly(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.heads[repoURL] = "bbbbbbbbbbbb"
	res := Check(context.Background(), baseInput(w, st))
	if res.Sent || len(w.sent) != 0 || len(res.Errors) != 0 {
		t.Fatalf("res=%+v sent=%v", res, w.sent)
	}
	if res.Next.Repos["o/r"].Head != "bbbbbbbbbbbb" {
		t.Fatalf("head not recorded: %+v", res.Next)
	}
}

func TestTwoClosedIssuesOneMessageThenNothing(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.issues["o/r"] = []fakeIssue{closed(123, "first", "COMPLETED"), closed(130, "second", "COMPLETED")}
	res := Check(context.Background(), baseInput(w, st))
	if !res.Sent || len(w.sent) != 1 {
		t.Fatalf("want exactly one message, got %v (%+v)", w.sent, res)
	}
	msg := w.sent[0]
	for _, want := range []string{"agent|", "o/r#123 first (closed 12:40Z)", "o/r#130 second", "main: o/r bbbbbbb"} {
		if !strings.Contains(msg, want) {
			t.Errorf("message lacks %q:\n%s", want, msg)
		}
	}
	w.heads[repoURL] = "cccccccccccc"
	res = Check(context.Background(), baseInput(w, res.Next))
	if res.Sent || len(w.sent) != 1 {
		t.Fatalf("second run re-reported: %v", w.sent)
	}
}

func TestQueryAndExplicitListDedupe(t *testing.T) {
	w := newWorld()
	in := baseInput(w, State{})
	in.Config.Repos[0].Query = "is:issue author:@me"
	st := baseline(t, w, in)
	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.issues["o/r"] = []fakeIssue{closed(123, "both", "COMPLETED"), closed(130, "explicit only", "COMPLETED"), closed(7, "query only", "COMPLETED")}
	w.queries["o/r"] = []int{123, 7}
	in.State = st
	res := Check(context.Background(), in)
	if len(w.sent) != 1 {
		t.Fatalf("sent=%v res=%+v", w.sent, res)
	}
	for n, want := range map[string]int{"o/r#123 ": 1, "o/r#130 ": 1, "o/r#7 ": 1} {
		if got := strings.Count(w.sent[0], n); got != want {
			t.Errorf("%q appears %d times:\n%s", n, got, w.sent[0])
		}
	}
}

func TestNotPlannedListedSeparatelyOrSkipped(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.issues["o/r"] = []fakeIssue{closed(123, "done", "COMPLETED"), closed(130, "dropped", "NOT_PLANNED")}
	res := Check(context.Background(), baseInput(w, st))
	if !strings.Contains(w.sent[0], "not planned") || !strings.Contains(w.sent[0], "o/r#130 dropped") {
		t.Fatalf("missing not-planned section:\n%s", w.sent[0])
	}
	_ = res

	w2 := newWorld()
	in := baseInput(w2, State{})
	in.Config.SkipNotPlanned = true
	st = baseline(t, w2, in)
	w2.heads[repoURL] = "bbbbbbbbbbbb"
	w2.issues["o/r"] = []fakeIssue{{Number: 123, State: "OPEN"}, closed(130, "dropped", "NOT_PLANNED")}
	in.State = st
	res = Check(context.Background(), in)
	if res.Sent || !res.Next.Repos["o/r"].has(130) {
		t.Fatalf("skipped not-planned should be silent and recorded: %+v", res)
	}
}

func TestNetworkErrorLeavesStateAndFailsButOthersProceed(t *testing.T) {
	w := newWorld()
	const other = "https://github.com/o/other"
	w.heads[other] = "dddddddddddd"
	w.issues["o/other"] = []fakeIssue{{Number: 1, Title: "x", State: "OPEN"}}
	in := baseInput(w, State{})
	in.Config.Repos = append(in.Config.Repos, RepoConfig{URL: other, Issues: []int{1}})
	st := baseline(t, w, in)

	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.lsErr = map[string]error{repoURL: errors.New("network down")}
	w.heads[other] = "eeeeeeeeeeee"
	w.issues["o/other"] = []fakeIssue{closed(1, "landed", "COMPLETED")}
	in.State = st
	res := Check(context.Background(), in)
	if len(res.Errors) != 1 {
		t.Fatalf("errors=%v", res.Errors)
	}
	if !sameRepo(res.Next.Repos["o/r"], st.Repos["o/r"]) {
		t.Fatalf("failed repo state changed: %+v", res.Next.Repos["o/r"])
	}
	if !res.Sent || !strings.Contains(w.sent[0], "o/other#1") {
		t.Fatalf("other repo blocked: %v", w.sent)
	}

	// A gh failure also keeps that repo's head, so the next run retries.
	w2 := newWorld()
	st2 := baseline(t, w2, baseInput(w2, State{}))
	w2.heads[repoURL] = "bbbbbbbbbbbb"
	w2.ghErr = errors.New("gh down")
	res = Check(context.Background(), baseInput(w2, st2))
	if len(res.Errors) == 0 || res.Next.Repos["o/r"].Head != "aaaaaaaaaaaa" {
		t.Fatalf("gh failure advanced state: %+v", res)
	}
}

func TestFailedSendDoesNotAdvanceState(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.issues["o/r"] = []fakeIssue{closed(123, "first", "COMPLETED"), {Number: 130, State: "OPEN"}}
	w.sendErr = errors.New("bus down")
	res := Check(context.Background(), baseInput(w, st))
	if len(res.Errors) == 0 || res.Sent {
		t.Fatalf("res=%+v", res)
	}
	if res.Next.Repos["o/r"].Head != "aaaaaaaaaaaa" || res.Next.Repos["o/r"].has(123) {
		t.Fatalf("state advanced after failed send: %+v", res.Next)
	}
	w.sendErr = nil
	res = Check(context.Background(), baseInput(w, res.Next))
	if !res.Sent || len(w.sent) != 1 {
		t.Fatalf("retry did not send: %+v", res)
	}
}

func TestFailureNoticeAtMostOncePerDay(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.lsErr = map[string]error{repoURL: errors.New("network down")}
	in := baseInput(w, st)
	res := Check(context.Background(), in)
	if len(w.sent) != 1 || !strings.Contains(w.sent[0], "failed") {
		t.Fatalf("want one failure notice, got %v", w.sent)
	}
	in.State = res.Next
	in.Now = in.Now.Add(2 * time.Hour)
	Check(context.Background(), in)
	if len(w.sent) != 1 {
		t.Fatalf("second notice within a day: %v", w.sent)
	}
	in.Now = in.Now.Add(25 * time.Hour)
	Check(context.Background(), in)
	if len(w.sent) != 2 {
		t.Fatalf("no notice after a day: %v", w.sent)
	}
}

func TestDryRunSendsNothingAndKeepsState(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.issues["o/r"] = []fakeIssue{closed(123, "first", "COMPLETED"), {Number: 130, State: "OPEN"}}
	in := baseInput(w, st)
	in.DryRun = true
	res := Check(context.Background(), in)
	if len(w.sent) != 0 || res.Sent || res.Message == "" {
		t.Fatalf("res=%+v sent=%v", res, w.sent)
	}
	if !sameRepo(res.Next.Repos["o/r"], st.Repos["o/r"]) {
		t.Fatalf("dry run changed state: %+v", res.Next)
	}
}

func TestFirstRunBaselinesWithoutMessage(t *testing.T) {
	w := newWorld()
	w.issues["o/r"] = []fakeIssue{closed(123, "old", "COMPLETED"), {Number: 130, State: "OPEN"}}
	res := Check(context.Background(), baseInput(w, State{}))
	if res.Sent || !res.Next.Repos["o/r"].has(123) || res.Next.Repos["o/r"].Head != "aaaaaaaaaaaa" {
		t.Fatalf("res=%+v", res)
	}
}

func TestStateRoundTripMode(t *testing.T) {
	path := filepath.Join(t.TempDir(), "sub", "state.json")
	want := State{Repos: map[string]RepoState{"o/r": {Head: "abc", Reported: []int{1}}}}
	if err := SaveState(path, want); err != nil {
		t.Fatal(err)
	}
	info, err := os.Stat(path)
	if err != nil || info.Mode().Perm() != 0o600 {
		t.Fatalf("mode: %v %v", info, err)
	}
	got, err := LoadState(path)
	if err != nil || got.Repos["o/r"].Head != "abc" || !got.Repos["o/r"].has(1) {
		t.Fatalf("got %+v err %v", got, err)
	}
}

func TestParseConfig(t *testing.T) {
	cfg, err := ParseConfig([]byte(`notify_name = "a"
[[repo]]
url = "git@github.com:o/r.git"
issues = [1]
`))
	if err != nil {
		t.Fatal(err)
	}
	if s, _ := cfg.Repos[0].Slug(); s != "o/r" {
		t.Errorf("slug %q", s)
	}
	for _, bad := range []string{
		"[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		"notify_name='a'\n",
		"notify_name='a'\n[[repo]]\nurl='https://github.com/o/r'\n",
	} {
		if _, err := ParseConfig([]byte(bad)); err == nil {
			t.Errorf("accepted %q", bad)
		}
	}
}

func sameRepo(a, b RepoState) bool {
	return a.Head == b.Head && len(a.Reported) == len(b.Reported)
}
