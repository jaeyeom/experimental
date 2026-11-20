package slackwatch

import (
	"context"
	"strings"
	"testing"
	"time"
)

// fakeInfo is users.info with canned answers and a call log.
type fakeInfo struct {
	users map[string]NamedUser
	// limited answers with a rate limit this many times first.
	limited int
	retry   time.Duration
	calls   []string
}

func (f *fakeInfo) UserInfo(_ context.Context, id string) (NamedUser, error) {
	f.calls = append(f.calls, id)
	if f.limited > 0 {
		f.limited--
		return NamedUser{}, &RateLimitError{RetryAfter: f.retry}
	}
	u, ok := f.users[id]
	if !ok {
		return NamedUser{}, ErrUserNotFound
	}
	return u, nil
}

// fakeRun is a resolver command with a canned stdout and exit code.
type fakeRun struct {
	out   string
	code  int
	err   error
	calls [][]string
}

func (f *fakeRun) run(_ context.Context, argv []string) ([]byte, int, error) {
	f.calls = append(f.calls, argv)
	return []byte(f.out), f.code, f.err
}

func testNames(t *testing.T, info UserInfoGetter, run *fakeRun) *Names {
	t.Helper()
	n := &Names{
		Store: openTestStore(t), Info: info, TTL: time.Hour, MaxWait: 10 * time.Second, Now: testNow,
		Sleep: func(context.Context, time.Duration) error { return nil },
	}
	if run != nil {
		n.Command = []string{"who", "--id", "{id}"}
		n.Run = run.run
	}
	return n
}

func TestNoticeShowsNamesAndAuthorNameVariable(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{
		NotifyTemplate: recordTemplate(f.dir, ""),
		Targets:        map[string]string{"agent-1": "pane-7"},
	})
	n.Names = testNames(t, &fakeInfo{users: map[string]NamedUser{
		"U9":  {ID: "U9", Name: "Ada Lovelace", Display: "ada"},
		"U77": {ID: "U77", Name: "Grace Hopper"},
	}}, nil)
	if err := n.Notify(context.Background(), f.note("ping <@U77> and <@U404|old>")); err != nil {
		t.Fatal(err)
	}
	got := readRecorded(t, f.dir, EventReply, "pane-7")
	if got["SLACKWATCH_AUTHOR"] != "U9" || got["SLACKWATCH_AUTHOR_NAME"] != "ada" {
		t.Errorf("author %q name %q", got["SLACKWATCH_AUTHOR"], got["SLACKWATCH_AUTHOR_NAME"])
	}
	if got["SLACKWATCH_TEXT"] != "ping @Grace Hopper and <@U404|old>" {
		t.Errorf("text %q", got["SLACKWATCH_TEXT"])
	}
	if msg := got["SLACKWATCH_MESSAGE"]; !strings.Contains(msg, "from ada (U9) in C1") || !strings.Contains(msg, "@Grace Hopper") {
		t.Errorf("message %q", msg)
	}
}

func TestNoNamesKeepsTodaysNotice(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{NotifyTemplate: recordTemplate(f.dir, ""), Targets: map[string]string{"agent-1": "pane-7"}})
	// users_info = false and no command: nothing can answer.
	off := false
	n.Names = NewNames(NamesConfig{UsersInfo: &off}, f.store, &fakeInfo{}, nil)
	if err := n.Notify(context.Background(), f.note("hi <@U77>")); err != nil {
		t.Fatal(err)
	}
	got := readRecorded(t, f.dir, EventReply, "pane-7")
	if got["SLACKWATCH_AUTHOR_NAME"] != "" || got["SLACKWATCH_TEXT"] != "hi <@U77>" || !strings.Contains(got["SLACKWATCH_MESSAGE"], "from U9 in C1") {
		t.Errorf("got %v", got)
	}
}

func TestResolverCommandContract(t *testing.T) {
	slack := map[string]NamedUser{"U1": {ID: "U1", Name: "From Slack"}}
	cases := []struct {
		name string
		run  fakeRun
		want string
		// slackCalled says whether users.info was the fallback.
		slackCalled bool
	}{
		{"exit 0 takes .name", fakeRun{out: `{"name":"From Command"}`}, "From Command", false},
		{"exit 2 is no match, fall through", fakeRun{code: 2}, "From Slack", true},
		{"other exit falls through", fakeRun{code: 1}, "From Slack", true},
		{"bad JSON falls through", fakeRun{out: "not json"}, "From Slack", true},
		{"timeout falls through", fakeRun{err: context.DeadlineExceeded}, "From Slack", true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			info := &fakeInfo{users: slack}
			run := tc.run
			n := testNames(t, info, &run)
			if got := n.Name(context.Background(), "U1"); got != tc.want {
				t.Fatalf("name %q, want %q", got, tc.want)
			}
			if len(run.calls) != 1 || strings.Join(run.calls[0], " ") != "who --id U1" {
				t.Fatalf("command calls %v", run.calls)
			}
			if (len(info.calls) > 0) != tc.slackCalled {
				t.Fatalf("users.info calls %v", info.calls)
			}
		})
	}
}

func TestRealCommandTimesOut(t *testing.T) {
	n := testNames(t, nil, nil)
	n.Command = []string{"sleep", "5"}
	n.Timeout = 50 * time.Millisecond
	start := time.Now()
	if got := n.Name(context.Background(), "U1"); got != "" {
		t.Fatalf("name %q", got)
	}
	if time.Since(start) > 3*time.Second {
		t.Fatal("the command was not stopped at its timeout")
	}
}

func TestBareIDWhenNothingKnows(t *testing.T) {
	run := &fakeRun{code: 2}
	n := testNames(t, &fakeInfo{}, run)
	if got := n.Label(context.Background(), "U5"); got != "U5" {
		t.Fatalf("label %q", got)
	}
	if got := n.Mentions(context.Background(), "x <@U5> y"); got != "x <@U5> y" {
		t.Fatalf("mentions %q", got)
	}
}

func TestCachedHitAndNoMatchMakeNoCalls(t *testing.T) {
	info := &fakeInfo{users: map[string]NamedUser{"U1": {ID: "U1", Name: "Ada"}}}
	run := &fakeRun{code: 2}
	n := testNames(t, info, run)
	ctx := context.Background()
	n.Name(ctx, "U1")
	n.Name(ctx, "U2") // no match anywhere
	calls, slackCalls := len(run.calls), len(info.calls)
	for range 3 {
		if n.Name(ctx, "U1") != "Ada" || n.Name(ctx, "U2") != "" {
			t.Fatal("cached answer changed")
		}
	}
	if len(run.calls) != calls || len(info.calls) != slackCalls {
		t.Fatalf("calls within the TTL: command %d→%d, users.info %d→%d", calls, len(run.calls), slackCalls, len(info.calls))
	}
	n.Now = func() time.Time { return testNow().Add(2 * time.Hour) }
	n.Name(ctx, "U1")
	if len(info.calls) == slackCalls {
		t.Fatal("an expired entry was not looked up again")
	}
}

func TestRateLimitedUsersInfoWaitsThenFallsBack(t *testing.T) {
	var slept []time.Duration
	info := &fakeInfo{users: map[string]NamedUser{"U1": {ID: "U1", Name: "Ada"}}, limited: 1, retry: 3 * time.Second}
	n := testNames(t, info, nil)
	n.Sleep = func(_ context.Context, d time.Duration) error { slept = append(slept, d); return nil }
	if got := n.Name(context.Background(), "U1"); got != "Ada" || len(slept) != 1 {
		t.Fatalf("name %q after sleeps %v", got, slept)
	}

	info = &fakeInfo{users: map[string]NamedUser{"U2": {ID: "U2", Name: "Bo"}}, limited: 10, retry: 4 * time.Second}
	n = testNames(t, info, nil)
	slept = nil
	n.Sleep = func(_ context.Context, d time.Duration) error { slept = append(slept, d); return nil }
	if got := n.Label(context.Background(), "U2"); got != "U2" {
		t.Fatalf("label %q, want the bare id after the wait bound", got)
	}
	var total time.Duration
	for _, d := range slept {
		total += d
	}
	if total > n.MaxWait {
		t.Fatalf("waited %s, more than %s", total, n.MaxWait)
	}
	// A rate-limited failure is not cached as no match.
	if _, _, ok, _ := n.Store.UserName("U2"); ok {
		t.Fatal("a rate-limited lookup was cached")
	}
}

func TestUsersInfoUsesDisplayThenRealName(t *testing.T) {
	info := &fakeInfo{users: map[string]NamedUser{
		"U1": {ID: "U1", Name: "Ada Lovelace", Display: "ada"},
		"U2": {ID: "U2", Name: "Bo Bot"},
	}}
	n := testNames(t, info, nil)
	if a, b := n.Name(context.Background(), "U1"), n.Name(context.Background(), "U2"); a != "ada" || b != "Bo Bot" {
		t.Fatalf("names %q %q", a, b)
	}
}

func TestLoadConfigNames(t *testing.T) {
	path := writeFile(t, "config.json", `{"names": {"command": ["who", "{id}"], "timeout": "2s", "users_info": false, "cacheTtl": "1h", "maxWait": "3s"}}`, 0o600)
	cfg, err := LoadConfig(path)
	if err != nil {
		t.Fatal(err)
	}
	n := NewNames(cfg.Names, nil, &fakeInfo{}, nil)
	if strings.Join(n.Command, " ") != "who {id}" || n.Timeout != 2*time.Second || n.Info != nil || n.TTL != time.Hour || n.MaxWait != 3*time.Second {
		t.Fatalf("names %+v", n)
	}
	n = NewNames(NamesConfig{}, nil, &fakeInfo{}, nil)
	if n.Info == nil || n.TTL != DefaultNameCacheTTL || len(n.Command) != 0 {
		t.Fatalf("defaults %+v", n)
	}
}
