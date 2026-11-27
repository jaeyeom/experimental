package installcheck

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
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
	ghErrBy map[string]error // slug -> gh error
	sendErr error
	calls   []string
	sent    []Notification
}

func (w *fakeWorld) run(_ context.Context, name string, args ...string) ([]byte, error) {
	w.calls = append(w.calls, name+" "+strings.Join(args, " "))
	if name == "git" {
		url := args[1]
		if err := w.lsErr[url]; err != nil {
			return nil, err
		}
		return []byte(w.heads[url] + "\trefs/heads/main\n"), nil
	}
	if name == "gh" || strings.HasSuffix(name, "/gh-throttle") || strings.HasSuffix(name, "/gh") {
		return w.gh(args)
	}
	return nil, fmt.Errorf("unexpected command %s", name)
}

func (w *fakeWorld) notify(_ context.Context, n Notification) error {
	if w.sendErr != nil {
		return w.sendErr
	}
	w.sent = append(w.sent, n)
	return nil
}

func (w *fakeWorld) gh(args []string) ([]byte, error) {
	if w.ghErr != nil {
		return nil, w.ghErr
	}
	flagVal := func(name string) string {
		for i, a := range args {
			if a == name && i+1 < len(args) {
				return args[i+1]
			}
		}
		return ""
	}
	slug := flagVal("-R")
	if err := w.ghErrBy[slug]; err != nil {
		return nil, err
	}
	if args[1] == "view" {
		n, _ := strconv.Atoi(args[2])
		for _, is := range w.issues[slug] {
			if is.Number == n {
				return marshal(is)
			}
		}
		return nil, errors.New("not found")
	}
	want := strings.ToUpper(flagVal("--state"))
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
		if strings.HasPrefix(c, "gh ") || strings.Contains(c, "gh-throttle ") {
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
			NotifyMode: NotifyModeStdout,
			Repos:      []RepoConfig{{URL: repoURL, Issues: []int{123, 130}}},
		},
		State:    st,
		Now:      time.Date(2026, 11, 22, 13, 0, 0, 0, time.UTC),
		Run:      w.run,
		Notifier: NotifierFunc(w.notify),
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
	if !res.Sent || len(w.sent) != 1 || w.sent[0].Kind != NotifyLanded {
		t.Fatalf("want exactly one landed message, got %v (%+v)", w.sent, res)
	}
	msg := w.sent[0].Body
	for _, want := range []string{"o/r#123 first (closed 12:40Z)", "o/r#130 second", "main: o/r bbbbbbb"} {
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
	in.Config.Repos[0].Issues = []int{123, 123, 130}
	in.Config.Repos[0].Query = "is:issue author:@me"
	st := baseline(t, w, in)
	w.calls = nil
	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.issues["o/r"] = []fakeIssue{closed(123, "both", "COMPLETED"), closed(130, "explicit only", "COMPLETED"), closed(7, "query only", "COMPLETED")}
	w.queries["o/r"] = []int{123, 7}
	in.State = st
	res := Check(context.Background(), in)
	if len(w.sent) != 1 {
		t.Fatalf("sent=%v res=%+v", w.sent, res)
	}
	for n, want := range map[string]int{"o/r#123 ": 1, "o/r#130 ": 1, "o/r#7 ": 1} {
		if got := strings.Count(w.sent[0].Body, n); got != want {
			t.Errorf("%q appears %d times:\n%s", n, got, w.sent[0].Body)
		}
	}
	if w.ghCalls() != 3 {
		t.Fatalf("ghCalls = %d, want 3 (2 unique issues + 1 query)", w.ghCalls())
	}
}

func TestNotPlannedListedSeparatelyOrSkipped(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.issues["o/r"] = []fakeIssue{closed(123, "done", "COMPLETED"), closed(130, "dropped", "NOT_PLANNED")}
	res := Check(context.Background(), baseInput(w, st))
	if !strings.Contains(w.sent[0].Body, "not planned") || !strings.Contains(w.sent[0].Body, "o/r#130 dropped") {
		t.Fatalf("missing not-planned section:\n%s", w.sent[0].Body)
	}
	if len(res.Repos) != 1 || len(res.Repos[0].NotPlanned) != 1 {
		t.Fatalf("repo result missing NotPlanned: %+v", res.Repos)
	}

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
	if res.Next.Repos["o/r"].Head != "bbbbbbbbbbbb" {
		t.Fatalf("head not updated when skipping not-planned: %+v", res.Next)
	}
}

func TestNetworkErrorLeavesStateAndFailsButOthersProceedAndRetry(t *testing.T) {
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
	if !res.Sent || !strings.Contains(w.sent[0].Body, "o/other#1") {
		t.Fatalf("other repo blocked: %v", w.sent)
	}
	if res.Next.Repos["o/other"].Head != "eeeeeeeeeeee" || !res.Next.Repos["o/other"].has(1) {
		t.Fatalf("healthy repo state not saved: %+v", res.Next.Repos["o/other"])
	}

	// Retry after network recovers: o/other makes 0 gh calls, o/r recovers and reports.
	w.calls = nil
	w.sent = nil
	w.lsErr = nil
	w.issues["o/r"] = []fakeIssue{closed(123, "recovered", "COMPLETED"), {Number: 130, State: "OPEN"}}
	in.State = res.Next
	in.Now = in.Now.Add(1 * time.Hour)
	res = Check(context.Background(), in)
	if len(res.Errors) != 0 || !res.Sent || len(w.sent) != 1 {
		t.Fatalf("retry res=%+v sent=%v", res, w.sent)
	}
	if !strings.Contains(w.sent[0].Body, "o/r#123 recovered") || strings.Contains(w.sent[0].Body, "o/other#1") {
		t.Fatalf("unexpected retry message:\n%s", w.sent[0].Body)
	}

	// A gh failure on one repo keeps that repo's head so the next run retries.
	w2 := newWorld()
	st2 := baseline(t, w2, baseInput(w2, State{}))
	w2.heads[repoURL] = "bbbbbbbbbbbb"
	w2.ghErr = errors.New("gh down")
	res = Check(context.Background(), baseInput(w2, st2))
	if len(res.Errors) == 0 || res.Next.Repos["o/r"].Head != "aaaaaaaaaaaa" {
		t.Fatalf("gh failure advanced state: %+v", res)
	}
}

func TestFailedSendDoesNotAdvanceStateOrAliasSlices(t *testing.T) {
	w := newWorld()
	w.issues["o/r"] = []fakeIssue{closed(99, "baseline", "COMPLETED"), {Number: 123, State: "OPEN"}, {Number: 130, State: "OPEN"}}
	in := baseInput(w, State{})
	in.Config.Repos[0].Issues = []int{99, 123, 130}
	st := baseline(t, w, in)

	w.heads[repoURL] = "bbbbbbbbbbbb"
	w.issues["o/r"] = []fakeIssue{closed(99, "baseline", "COMPLETED"), closed(123, "first", "COMPLETED"), {Number: 130, State: "OPEN"}}
	w.sendErr = errors.New("notifier down")
	in.State = st
	res := Check(context.Background(), in)
	if len(res.Errors) == 0 || res.Sent {
		t.Fatalf("res=%+v", res)
	}
	if res.Next.Repos["o/r"].Head != "aaaaaaaaaaaa" || res.Next.Repos["o/r"].has(123) {
		t.Fatalf("state advanced after failed send: %+v", res.Next)
	}
	// Mutating res.Next must not mutate input st.
	res.Next.Repos["o/r"] = RepoState{Head: "mutated", Reported: append(res.Next.Repos["o/r"].Reported, 999)}
	if st.Repos["o/r"].Head != "aaaaaaaaaaaa" || st.Repos["o/r"].has(999) {
		t.Fatalf("input state was mutated via aliasing: %+v", st)
	}

	w.sendErr = nil
	in.State = st
	res = Check(context.Background(), in)
	if !res.Sent || len(w.sent) != 1 || !res.Next.Repos["o/r"].has(123) {
		t.Fatalf("retry did not send: %+v", res)
	}
}

func TestForcePushedMainHandledCleanly(t *testing.T) {
	t.Run("force push with no newly closed issues updates head and preserves reported", func(t *testing.T) {
		w := newWorld()
		w.issues["o/r"] = []fakeIssue{closed(123, "already landed", "COMPLETED"), {Number: 130, State: "OPEN"}}
		st := baseline(t, w, baseInput(w, State{}))
		// Move forward once.
		w.heads[repoURL] = "bbbbbbbbbbbb"
		res := Check(context.Background(), baseInput(w, st))
		// Force-push back to an unrelated/rewritten SHA (or even back to aaaa...).
		w.heads[repoURL] = "aaaaaaaaaaaa"
		res = Check(context.Background(), baseInput(w, res.Next))
		if len(res.Errors) != 0 || res.Sent {
			t.Fatalf("force-push res=%+v", res)
		}
		if res.Next.Repos["o/r"].Head != "aaaaaaaaaaaa" || !res.Next.Repos["o/r"].has(123) {
			t.Fatalf("lost reported issue or failed to update head: %+v", res.Next.Repos["o/r"])
		}
	})

	t.Run("force push with newly closed issue reports only new issue", func(t *testing.T) {
		w := newWorld()
		w.issues["o/r"] = []fakeIssue{closed(123, "old", "COMPLETED"), {Number: 130, State: "OPEN"}}
		st := baseline(t, w, baseInput(w, State{}))
		w.heads[repoURL] = "ffffffffffff" // unrelated non-ancestor SHA
		w.issues["o/r"] = []fakeIssue{closed(123, "old", "COMPLETED"), closed(130, "new after rebase", "COMPLETED")}
		res := Check(context.Background(), baseInput(w, st))
		if len(res.Errors) != 0 || !res.Sent || len(w.sent) != 1 {
			t.Fatalf("res=%+v sent=%v", res, w.sent)
		}
		msg := w.sent[0].Body
		if strings.Contains(msg, "#123") || !strings.Contains(msg, "o/r#130 new after rebase") {
			t.Fatalf("unexpected force-push message:\n%s", msg)
		}
		if !res.Next.Repos["o/r"].has(123) || !res.Next.Repos["o/r"].has(130) || res.Next.Repos["o/r"].Head != "ffffffffffff" {
			t.Fatalf("state mismatch: %+v", res.Next.Repos["o/r"])
		}
	})
}

func TestFailureNoticeAtMostOncePerDay(t *testing.T) {
	w := newWorld()
	st := baseline(t, w, baseInput(w, State{}))
	w.lsErr = map[string]error{repoURL: errors.New("network down")}
	in := baseInput(w, st)
	res := Check(context.Background(), in)
	if len(w.sent) != 1 || w.sent[0].Kind != NotifyFailure || !strings.Contains(w.sent[0].Body, "failed") {
		t.Fatalf("want one failure notice, got %v", w.sent)
	}
	in.State = res.Next
	in.Now = in.Now.Add(2 * time.Hour)
	res = Check(context.Background(), in)
	if len(w.sent) != 1 {
		t.Fatalf("second notice within a day: %v", w.sent)
	}
	in.State = res.Next
	in.Now = in.Now.Add(25 * time.Hour)
	res = Check(context.Background(), in)
	if len(w.sent) != 2 {
		t.Fatalf("no notice after a day: %v", w.sent)
	}

	// Failed failure-notification send does not update FailureNotedAt and appends error.
	w.sendErr = errors.New("notify down")
	in.State = res.Next
	in.Now = in.Now.Add(25 * time.Hour)
	res = Check(context.Background(), in)
	if len(res.Errors) != 2 {
		t.Fatalf("expected repo error + notify error, got %v", res.Errors)
	}
	if !res.Next.FailureNotedAt.Equal(in.State.FailureNotedAt) {
		t.Fatalf("FailureNotedAt advanced despite send error: got %v want %v", res.Next.FailureNotedAt, in.State.FailureNotedAt)
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

	// Dry run also does not send failure notices.
	w.lsErr = map[string]error{repoURL: errors.New("network down")}
	res = Check(context.Background(), in)
	if len(w.sent) != 0 || len(res.Errors) != 1 {
		t.Fatalf("dry run failure sent=%v errors=%v", w.sent, res.Errors)
	}
}

func TestFirstRunBaselinesWithoutMessage(t *testing.T) {
	w := newWorld()
	w.issues["o/r"] = []fakeIssue{closed(123, "old", "COMPLETED"), {Number: 130, State: "OPEN"}}
	res := Check(context.Background(), baseInput(w, State{}))
	if res.Sent || !res.Next.Repos["o/r"].has(123) || res.Next.Repos["o/r"].Head != "aaaaaaaaaaaa" {
		t.Fatalf("res=%+v", res)
	}

	// Failed first run does not baseline the repo.
	w2 := newWorld()
	w2.ghErr = errors.New("gh rate limited")
	res2 := Check(context.Background(), baseInput(w2, State{}))
	if _, ok := res2.Next.Repos["o/r"]; ok || len(res2.Errors) == 0 {
		t.Fatalf("failed first run should not baseline: %+v", res2)
	}
}

func TestConfigurableGHBinUsedInCheckAndStatus(t *testing.T) {
	w := newWorld()
	in := baseInput(w, State{})
	in.GHBin = "/opt/bin/gh-throttle"
	st := baseline(t, w, in)
	if !slices.ContainsFunc(w.calls, func(c string) bool { return strings.HasPrefix(c, "/opt/bin/gh-throttle ") }) {
		t.Fatalf("Check did not invoke configured gh_bin, calls=%v", w.calls)
	}

	w.calls = nil
	rep, errs := InspectStatus(context.Background(), in.Config, st, w.run, "/opt/bin/gh-throttle")
	if len(errs) != 0 || rep.GHBin != "/opt/bin/gh-throttle" {
		t.Fatalf("InspectStatus rep=%+v errs=%v", rep, errs)
	}
	if !slices.ContainsFunc(w.calls, func(c string) bool { return strings.HasPrefix(c, "/opt/bin/gh-throttle ") }) {
		t.Fatalf("InspectStatus did not invoke configured gh_bin, calls=%v", w.calls)
	}
}

func TestStatusFormattingAndPartialErrors(t *testing.T) {
	w := newWorld()
	const other = "https://github.com/o/other"
	w.ghErrBy = map[string]error{"o/other": errors.New("not found")}
	cfg := Config{
		NotifyMode: NotifyModeCommand,
		NotifyCommand: []string{
			"/bin/echo",
		},
		Repos: []RepoConfig{
			{URL: repoURL, Issues: []int{123}},
			{URL: other, Issues: []int{1}},
		},
	}
	st := State{
		LastRun: time.Date(2026, 11, 22, 13, 0, 0, 0, time.UTC),
		Repos:   map[string]RepoState{"o/r": {Head: "1a2b3c4d"}},
	}
	rep, errs := InspectStatus(context.Background(), cfg, st, w.run, "gh")
	if len(errs) != 1 {
		t.Fatalf("errs = %v, want 1", errs)
	}
	text, err := FormatStatus(rep, false)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(text, "o/r main: 1a2b3c4") || !strings.Contains(text, "open: #123 open one") {
		t.Fatalf("unexpected text:\n%s", text)
	}
	js, err := FormatStatus(rep, true)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(js, `"notifyMode": "command"`) || !strings.Contains(js, `"error": "o/other: not found"`) {
		t.Fatalf("unexpected json:\n%s", js)
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

func sameRepo(a, b RepoState) bool {
	return a.Head == b.Head && slices.Equal(a.Reported, b.Reported)
}
