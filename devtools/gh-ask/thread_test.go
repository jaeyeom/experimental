package ghask

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"log/slog"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// scriptCmd is a fake command that appends its argv, space separated, as
// one line to log and prints the content of out.
type scriptCmd struct {
	path string
	log  string
	out  string
}

func writeScript(t *testing.T, out string) scriptCmd {
	t.Helper()
	dir := t.TempDir()
	c := scriptCmd{
		path: filepath.Join(dir, "cmd.sh"),
		log:  filepath.Join(dir, "log.txt"),
		out:  filepath.Join(dir, "out.txt"),
	}
	body := fmt.Sprintf("#!/bin/sh\nprintf '%%s\\n' \"$*\" >> %q\ncat %q\n", c.log, c.out)
	if err := os.WriteFile(c.path, []byte(body), 0o700); err != nil { //nolint:gosec // G306: the fake command must be executable
		t.Fatal(err)
	}
	c.setOutput(t, out)
	return c
}

func (c scriptCmd) setOutput(t *testing.T, out string) {
	t.Helper()
	if err := os.WriteFile(c.out, []byte(out), 0o600); err != nil {
		t.Fatal(err)
	}
}

func (c scriptCmd) runs(t *testing.T) []string {
	t.Helper()
	data, err := os.ReadFile(c.log)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		t.Fatal(err)
	}
	return strings.Split(strings.TrimSuffix(string(data), "\n"), "\n")
}

type threadCmds struct {
	ask, followup, status scriptCmd
}

const openStatus = `{"state":"working","answered":false,"closed":false}`

func newThreadEnv(t *testing.T, mutate func(*Config)) (*testEnv, threadCmds) {
	t.Helper()
	cmds := threadCmds{
		ask:      writeScript(t, "created thread-aaa\n"),
		followup: writeScript(t, "ok\n"),
		status:   writeScript(t, openStatus),
	}
	env := newTestEnv(t, "", 0, func(c *Config) {
		c.Reuse = ReusePerPR
		c.AskCommand = []string{cmds.ask.path, "ask", "{pr}", "{repo}"}
		c.FollowupCommand = []string{cmds.followup.path, "--id", "{id}", "--pr", "{pr}", "--repo", "{repo}"}
		c.StatusCommand = []string{cmds.status.path, "{id}"}
		if mutate != nil {
			mutate(c)
		}
	})
	return env, cmds
}

func askID(t *testing.T, env *testEnv, req AskRequest) string {
	t.Helper()
	resp, body := env.ask(t, req)
	assertStatus(t, resp, body, http.StatusOK)
	var got struct{ ID string }
	if err := json.Unmarshal(body, &got); err != nil {
		t.Fatalf("body %s: %v", body, err)
	}
	return got.ID
}

func (e *testEnv) status(t *testing.T, id string, opts ...reqOpt) (*http.Response, []byte) {
	t.Helper()
	return e.do(t, http.MethodGet, "/status/"+id, nil, opts...)
}

func TestSecondAskOnSamePRRunsFollowup(t *testing.T) {
	stateFile := filepath.Join(t.TempDir(), "threads.json")
	env, cmds := newThreadEnv(t, func(c *Config) { c.StateFile = stateFile })
	if id := askID(t, env, validRequest()); id != "aaa" {
		t.Fatalf("first id = %q, want aaa", id)
	}
	cmds.ask.setOutput(t, "created thread-bbb\n")
	if id := askID(t, env, validRequest()); id != "aaa" {
		t.Fatalf("second id = %q, want the stored aaa", id)
	}
	if runs := cmds.ask.runs(t); len(runs) != 1 {
		t.Errorf("ask_command runs = %q, want 1", runs)
	}
	runs := cmds.followup.runs(t)
	if len(runs) != 1 || runs[0] != "--id aaa --pr 42 --repo octo/demo" {
		t.Errorf("followup_command runs = %q, want one with the stored id", runs)
	}

	other := validRequest()
	other.PR = 43
	if id := askID(t, env, other); id != "bbb" {
		t.Errorf("other PR id = %q, want a new thread bbb", id)
	}

	info, err := os.Stat(stateFile)
	if err != nil {
		t.Fatalf("state file: %v", err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Errorf("state mode = %o, want 600", perm)
	}

	// A restarted server reads the stored id back.
	restarted, cmds2 := newThreadEnv(t, func(c *Config) { c.StateFile = stateFile })
	if id := askID(t, restarted, validRequest()); id != "aaa" {
		t.Errorf("after restart id = %q, want aaa", id)
	}
	if runs := cmds2.ask.runs(t); len(runs) != 0 {
		t.Errorf("after restart ask_command runs = %q, want none", runs)
	}
	resp, body := restarted.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
}

func TestClosedStatusStartsNewThread(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	cmds.status.setOutput(t, `{"state":"done","answered":true,"closed":true}`)
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)

	cmds.ask.setOutput(t, "created thread-bbb\n")
	if id := askID(t, env, validRequest()); id != "bbb" {
		t.Fatalf("id after closed = %q, want new thread bbb", id)
	}
	if runs := cmds.followup.runs(t); len(runs) != 0 {
		t.Errorf("followup_command ran for a closed thread: %q", runs)
	}
	cmds.status.setOutput(t, openStatus)
	if id := askID(t, env, validRequest()); id != "bbb" {
		t.Fatalf("next id = %q, want bbb reused", id)
	}
	runs := cmds.followup.runs(t)
	if len(runs) != 1 || !strings.HasPrefix(runs[0], "--id bbb ") {
		t.Errorf("followup_command runs = %q, want one for bbb", runs)
	}
}

func TestReuseNoneAlwaysRunsAsk(t *testing.T) {
	env, cmds := newThreadEnv(t, func(c *Config) { c.Reuse = ReuseNone })
	for range 3 {
		askID(t, env, validRequest())
	}
	if runs := cmds.ask.runs(t); len(runs) != 3 {
		t.Errorf("ask_command runs = %d, want 3", len(runs))
	}
	if runs := cmds.followup.runs(t); len(runs) != 0 {
		t.Errorf("followup_command runs = %q, want none", runs)
	}
	if _, err := os.Stat(env.srv.cfg.StateFile); !errors.Is(err, fs.ErrNotExist) {
		t.Errorf("state file written with reuse = none (err %v)", err)
	}
	// Ids from this server still answer on /status.
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
}

func TestStatusUnknownID(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	for _, id := range []string{"zzz", "", "aaa%0A", "aaa/x", "..%2Faaa", strings.Repeat("a", maxIDBytes+1)} {
		t.Run(id, func(t *testing.T) {
			resp, body := env.status(t, id)
			assertStatus(t, resp, body, http.StatusNotFound)
		})
	}
	if runs := cmds.status.runs(t); len(runs) != 0 {
		t.Errorf("status_command ran for unknown ids: %q", runs)
	}
}

func TestStatusWithoutStatusCommand(t *testing.T) {
	env, _ := newThreadEnv(t, func(c *Config) { c.StatusCommand = nil })
	askID(t, env, validRequest())
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusNotFound)
}

func TestStatusPassesValidatedJSON(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	cmds.status.setOutput(t, `{"state":"answered","answered":true,"closed":false,"extra":"<script>"}`+"\n")
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	if got, want := strings.TrimSpace(string(body)), `{"state":"answered","answered":true,"closed":false}`; got != want {
		t.Errorf("body = %s, want %s", got, want)
	}
	if ct := resp.Header.Get("Content-Type"); !strings.HasPrefix(ct, "application/json") {
		t.Errorf("Content-Type = %q", ct)
	}
	if runs := cmds.status.runs(t); len(runs) != 1 || runs[0] != "aaa" {
		t.Errorf("status_command runs = %q, want one with the id", runs)
	}
}

func TestStatusCache(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	for range 2 {
		resp, body := env.status(t, "aaa")
		assertStatus(t, resp, body, http.StatusOK)
	}
	*env.clock = env.clock.Add(statusCacheTTL - time.Second)
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	if runs := cmds.status.runs(t); len(runs) != 1 {
		t.Fatalf("status_command runs within TTL = %d, want 1", len(runs))
	}
	*env.clock = env.clock.Add(2 * time.Second)
	resp, body = env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	if runs := cmds.status.runs(t); len(runs) != 2 {
		t.Errorf("status_command runs after TTL = %d, want 2", len(runs))
	}
}

func TestStatusInvalidOutput(t *testing.T) {
	tests := []struct {
		name string
		out  string
	}{
		{name: "not json", out: "working"},
		{name: "empty", out: ""},
		{name: "array", out: `[]`},
		{name: "missing field", out: `{"state":"x","answered":true}`},
		{name: "null field", out: `{"state":"x","answered":true,"closed":null}`},
		{name: "state not string", out: `{"state":1,"answered":true,"closed":false}`},
		{name: "answered not bool", out: `{"state":"x","answered":"yes","closed":false}`},
		{name: "trailing data", out: `{"state":"x","answered":true,"closed":false} {}`},
		{name: "control char in state", out: `{"state":"a\u0007b","answered":true,"closed":false}`},
		{name: "long state", out: `{"state":"` + strings.Repeat("s", maxStateBytes+1) + `","answered":true,"closed":false}`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			env, cmds := newThreadEnv(t, nil)
			askID(t, env, validRequest())
			cmds.status.setOutput(t, tc.out)
			resp, body := env.status(t, "aaa")
			assertStatus(t, resp, body, http.StatusBadGateway)
			// Failures are not cached.
			cmds.status.setOutput(t, openStatus)
			resp, body = env.status(t, "aaa")
			assertStatus(t, resp, body, http.StatusOK)
		})
	}
}

func TestStatusCommandFails(t *testing.T) {
	dir := t.TempDir()
	script := filepath.Join(dir, "fail.sh")
	if err := os.WriteFile(script, []byte("#!/bin/sh\necho '"+openStatus+"'\nexit 3\n"), 0o700); err != nil { //nolint:gosec // G306: the fake command must be executable
		t.Fatal(err)
	}
	env, _ := newThreadEnv(t, func(c *Config) { c.StatusCommand = []string{script, "{id}"} })
	askID(t, env, validRequest())
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusBadGateway)
}

func TestStatusRequestChecks(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	port := strconv.Itoa(env.port)
	tests := []struct {
		name   string
		method string
		opts   []reqOpt
		want   int
	}{
		{name: "no token", method: http.MethodGet, opts: []reqOpt{withoutHeader(TokenHeader)}, want: http.StatusUnauthorized},
		{name: "wrong token", method: http.MethodGet, opts: []reqOpt{withHeader(TokenHeader, "wrong")}, want: http.StatusUnauthorized},
		{name: "bad host", method: http.MethodGet, opts: []reqOpt{withHost("evil.example:" + port)}, want: http.StatusForbidden},
		{name: "bad origin", method: http.MethodGet, opts: []reqOpt{withHeader("Origin", "https://evil.example")}, want: http.StatusForbidden},
		{name: "preflight", method: http.MethodOptions, opts: []reqOpt{
			withHeader("Origin", "https://github.com"),
			withHeader("Access-Control-Request-Method", "GET"),
			withHeader("Access-Control-Request-Headers", TokenHeader),
		}, want: http.StatusMethodNotAllowed},
		{name: "post", method: http.MethodPost, want: http.StatusMethodNotAllowed},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			resp, body := env.do(t, tc.method, "/status/aaa", nil, tc.opts...)
			assertStatus(t, resp, body, tc.want)
		})
	}
	if runs := cmds.status.runs(t); len(runs) != 0 {
		t.Errorf("status_command ran for rejected requests: %q", runs)
	}
	resp, body := env.status(t, "aaa", withHeader("Origin", "https://github.com"))
	assertStatus(t, resp, body, http.StatusOK)
}

func TestAskClosedStatusWithoutPollStartsNewThread(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	if id := askID(t, env, validRequest()); id != "aaa" {
		t.Fatalf("first id = %q, want aaa", id)
	}
	cmds.status.setOutput(t, `{"state":"done","answered":true,"closed":true}`)
	cmds.ask.setOutput(t, "created thread-bbb\n")
	if id := askID(t, env, validRequest()); id != "bbb" {
		t.Fatalf("id = %q, want new thread bbb", id)
	}
	if runs := cmds.followup.runs(t); len(runs) != 0 {
		t.Errorf("followup_command ran on a closed thread: %q", runs)
	}
	if runs := cmds.ask.runs(t); len(runs) != 2 {
		t.Errorf("ask_command runs = %d, want 2", len(runs))
	}
	if runs := cmds.status.runs(t); len(runs) != 1 || runs[0] != "aaa" {
		t.Errorf("status_command runs = %q, want one check of aaa", runs)
	}
	threads, err := loadState(env.srv.cfg.StateFile)
	if err != nil {
		t.Fatal(err)
	}
	if got := threads[stateKey("octo/demo", 42)]; got.ID != "bbb" {
		t.Errorf("stored = %+v, want id bbb", got)
	}
}

func TestAskOpenStatusReusesThread(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	if id := askID(t, env, validRequest()); id != "aaa" {
		t.Fatalf("id = %q, want stored aaa", id)
	}
	runs := cmds.followup.runs(t)
	if len(runs) != 1 || runs[0] != "--id aaa --pr 42 --repo octo/demo" {
		t.Errorf("followup_command runs = %q, want one for aaa", runs)
	}
	if runs := cmds.ask.runs(t); len(runs) != 1 {
		t.Errorf("ask_command runs = %d, want 1", len(runs))
	}
	if runs := cmds.status.runs(t); len(runs) != 1 || runs[0] != "aaa" {
		t.Errorf("status_command runs = %q, want one check of aaa", runs)
	}
}

func TestAskStatusFailureStartsNewThread(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	var logs bytes.Buffer
	env.srv.logger = slog.New(slog.NewTextHandler(&logs, nil))
	askID(t, env, validRequest())
	cmds.status.setOutput(t, "not json")
	cmds.ask.setOutput(t, "created thread-bbb\n")
	if id := askID(t, env, validRequest()); id != "bbb" {
		t.Fatalf("id = %q, want new thread bbb", id)
	}
	if runs := cmds.followup.runs(t); len(runs) != 0 {
		t.Errorf("followup_command ran after a failed status: %q", runs)
	}
	if !strings.Contains(logs.String(), "starting a new thread") {
		t.Errorf("log = %q, want a line that a new thread is starting", logs.String())
	}
}

func TestAskStatusCommandExitStartsNewThread(t *testing.T) {
	dir := t.TempDir()
	script := filepath.Join(dir, "fail.sh")
	if err := os.WriteFile(script, []byte("#!/bin/sh\necho fail >&2\nexit 3\n"), 0o700); err != nil { //nolint:gosec // G306: the fake command must be executable
		t.Fatal(err)
	}
	env, cmds := newThreadEnv(t, func(c *Config) { c.StatusCommand = []string{script, "{id}"} })
	var logs bytes.Buffer
	env.srv.logger = slog.New(slog.NewTextHandler(&logs, nil))
	askID(t, env, validRequest())
	cmds.ask.setOutput(t, "created thread-bbb\n")
	if id := askID(t, env, validRequest()); id != "bbb" {
		t.Fatalf("id = %q, want new thread bbb", id)
	}
	if runs := cmds.followup.runs(t); len(runs) != 0 {
		t.Errorf("followup_command ran after a failed status: %q", runs)
	}
	if !strings.Contains(logs.String(), "starting a new thread") {
		t.Errorf("log = %q, want a line that a new thread is starting", logs.String())
	}
}

func TestAskUsesFreshStatusCache(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	*env.clock = env.clock.Add(statusCacheTTL - time.Second)
	if id := askID(t, env, validRequest()); id != "aaa" {
		t.Fatalf("id within TTL = %q, want stored aaa", id)
	}
	if runs := cmds.status.runs(t); len(runs) != 1 {
		t.Fatalf("status_command runs within TTL = %d, want 1", len(runs))
	}
	*env.clock = env.clock.Add(2 * time.Second)
	if id := askID(t, env, validRequest()); id != "aaa" {
		t.Fatalf("id after TTL = %q, want stored aaa", id)
	}
	if runs := cmds.status.runs(t); len(runs) != 2 {
		t.Errorf("status_command runs after TTL = %d, want 2", len(runs))
	}
	runs := cmds.followup.runs(t)
	if len(runs) != 2 || runs[0] != "--id aaa --pr 42 --repo octo/demo" {
		t.Errorf("followup_command runs = %q, want two for aaa", runs)
	}
}

func TestAskWithoutStatusCommandReusesByID(t *testing.T) {
	env, cmds := newThreadEnv(t, func(c *Config) { c.StatusCommand = nil })
	askID(t, env, validRequest())
	if id := askID(t, env, validRequest()); id != "aaa" {
		t.Fatalf("id = %q, want stored aaa", id)
	}
	runs := cmds.followup.runs(t)
	if len(runs) != 1 || runs[0] != "--id aaa --pr 42 --repo octo/demo" {
		t.Errorf("followup_command runs = %q, want one for aaa", runs)
	}
	if runs := cmds.ask.runs(t); len(runs) != 1 {
		t.Errorf("ask_command runs = %d, want 1", len(runs))
	}
	if runs := cmds.status.runs(t); len(runs) != 0 {
		t.Errorf("status_command runs = %q, want none", runs)
	}
}

// statusWire is the GET /status body the tests decode. It is local so a
// missing messages field fails the assertion instead of the build.
type statusWire struct {
	State    string `json:"state"`
	Answered bool   `json:"answered"`
	Closed   bool   `json:"closed"`
	Messages []struct {
		Role string    `json:"role"`
		Time time.Time `json:"time"`
		Text string    `json:"text"`
	} `json:"messages"`
}

func mustStatusJSON(t *testing.T, messages any) string {
	t.Helper()
	body := map[string]any{
		"state":    "working",
		"answered": false,
		"closed":   false,
	}
	if messages != nil {
		body["messages"] = messages
	}
	raw, err := json.Marshal(body)
	if err != nil {
		t.Fatal(err)
	}
	return string(raw)
}

func decodeStatusWire(t *testing.T, body []byte) statusWire {
	t.Helper()
	var st statusWire
	if err := json.Unmarshal(body, &st); err != nil {
		t.Fatalf("body %s: %v", body, err)
	}
	return st
}

func statusMessage(role, when, text string) map[string]string {
	return map[string]string{"role": role, "time": when, "text": text}
}

func countLinesContaining(logs, needle string) int {
	n := 0
	for line := range strings.SplitSeq(logs, "\n") {
		if strings.Contains(line, needle) {
			n++
		}
	}
	return n
}

func TestStatusMessagesPassThrough(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	cmds.status.setOutput(t, `{"state":"answered","answered":true,"closed":false,"messages":[{"role":"you","time":"2026-11-14T10:00:00Z","text":"hi","html":"<b>hi</b>"}]}`)
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	got := strings.TrimSpace(string(body))
	want := `{"state":"answered","answered":true,"closed":false,"messages":[{"role":"you","time":"2026-11-14T10:00:00Z","text":"hi"}]}`
	if got != want {
		t.Errorf("body = %s, want %s", got, want)
	}
	if runs := cmds.status.runs(t); len(runs) != 1 {
		t.Errorf("status_command runs = %d, want 1", len(runs))
	}
}

func TestStatusMessagesKeepNewest(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	base := time.Date(2026, 11, 14, 10, 0, 0, 0, time.UTC)
	msgs := make([]map[string]string, 60)
	for i := 59; i >= 0; i-- {
		msgs[59-i] = statusMessage("note", base.Add(time.Duration(i)*time.Second).Format(time.RFC3339), fmt.Sprintf("n%02d", i))
	}
	cmds.status.setOutput(t, mustStatusJSON(t, msgs))
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	st := decodeStatusWire(t, body)
	if len(st.Messages) != 50 {
		t.Fatalf("messages = %d, want the newest 50", len(st.Messages))
	}
	if st.Messages[0].Text != "n10" || st.Messages[49].Text != "n59" {
		t.Fatalf("kept %q .. %q, want n10 .. n59", st.Messages[0].Text, st.Messages[49].Text)
	}
	for i := 1; i < len(st.Messages); i++ {
		if st.Messages[i].Time.Before(st.Messages[i-1].Time) {
			t.Fatalf("messages[%d] time %s is before the previous", i, st.Messages[i].Time)
		}
	}
}

func TestStatusMessagesTruncateText(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	when := "2026-11-14T10:00:00Z"
	long := strings.Repeat("a", 16*1024+10)
	split := strings.Repeat("b", 16*1024-1) + "é"
	cmds.status.setOutput(t, mustStatusJSON(t, []map[string]string{
		statusMessage("agent", when, long),
		statusMessage("you", "2026-11-14T10:00:01Z", split),
	}))
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	st := decodeStatusWire(t, body)
	if len(st.Messages) != 2 {
		t.Fatalf("messages = %d, want 2", len(st.Messages))
	}
	if got := st.Messages[0].Text; got != strings.Repeat("a", 16*1024) {
		t.Fatalf("ascii text len = %d, want %d", len(got), 16*1024)
	}
	wantSplit := strings.Repeat("b", 16*1024-1)
	if got := st.Messages[1].Text; got != wantSplit {
		t.Fatalf("utf-8 text = %q (len %d), want %d bytes with the rune dropped", got, len(got), len(wantSplit))
	}
}

func TestStatusMessagesBodyCap(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	base := time.Date(2026, 11, 14, 10, 0, 0, 0, time.UTC)
	msgs := make([]map[string]string, 50)
	for i := range msgs {
		msgs[i] = statusMessage("note", base.Add(time.Duration(i)*time.Second).Format(time.RFC3339), fmt.Sprintf("M%03d", i)+strings.Repeat("a", 16*1024))
	}
	cmds.status.setOutput(t, mustStatusJSON(t, msgs))
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	if len(body) > 256*1024 {
		t.Fatalf("body = %d bytes, want at most 256 KiB", len(body))
	}
	st := decodeStatusWire(t, body)
	if len(st.Messages) != 15 {
		t.Fatalf("messages = %d, want 15 newest that fit", len(st.Messages))
	}
	if !strings.HasPrefix(st.Messages[0].Text, "M035") || !strings.HasPrefix(st.Messages[14].Text, "M049") {
		t.Fatalf("kept %q .. %q, want M035 .. M049", st.Messages[0].Text[:4], st.Messages[len(st.Messages)-1].Text[:4])
	}
	for _, m := range st.Messages {
		if len(m.Text) != 16*1024 {
			t.Fatalf("text len = %d, want 16 KiB", len(m.Text))
		}
	}
}

func TestStatusMessagesDropUnknownRole(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	cmds.status.setOutput(t, mustStatusJSON(t, []map[string]string{
		statusMessage("system", "2026-11-14T10:00:00Z", "nope"),
		statusMessage("agent", "2026-11-14T10:00:01Z", "yes"),
		statusMessage("note", "2026-11-14T10:00:02Z", "ok"),
	}))
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	st := decodeStatusWire(t, body)
	if len(st.Messages) != 2 || st.Messages[0].Role != "agent" || st.Messages[1].Text != "ok" {
		t.Fatalf("messages = %+v, want agent then note", st.Messages)
	}
	if runs := cmds.status.runs(t); len(runs) != 1 {
		t.Errorf("status_command runs = %d, want 1", len(runs))
	}
}

func TestStatusMessagesStripControls(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	cmds.status.setOutput(t, mustStatusJSON(t, []map[string]string{
		statusMessage("you", "2026-11-14T10:00:00Z", "a\x00b\nc\td\re"),
	}))
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	st := decodeStatusWire(t, body)
	if len(st.Messages) != 1 {
		t.Fatalf("messages = %d, want 1", len(st.Messages))
	}
	if st.Messages[0].Text != "ab\nc\tde" {
		t.Fatalf("text = %q, want controls stripped except newline and tab", st.Messages[0].Text)
	}
}

func TestStatusMessagesInvalidArrayOmitsField(t *testing.T) {
	tests := []struct {
		name     string
		messages any
	}{
		{name: "wrong type", messages: "nope"},
		{name: "element wrong type", messages: []any{"bad"}},
		{name: "missing text", messages: []map[string]string{{"role": "you", "time": "2026-11-14T10:00:00Z"}}},
		{name: "bad time", messages: []map[string]string{statusMessage("you", "yesterday", "hi")}},
		{name: "null role", messages: []map[string]any{{"role": nil, "time": "2026-11-14T10:00:00Z", "text": "hi"}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			env, cmds := newThreadEnv(t, nil)
			var logs bytes.Buffer
			env.srv.logger = slog.New(slog.NewTextHandler(&logs, nil))
			askID(t, env, validRequest())
			cmds.status.setOutput(t, mustStatusJSON(t, tc.messages))
			resp, body := env.status(t, "aaa")
			assertStatus(t, resp, body, http.StatusOK)
			if got, want := strings.TrimSpace(string(body)), `{"state":"working","answered":false,"closed":false}`; got != want {
				t.Errorf("body = %s, want %s", got, want)
			}
			if n := countLinesContaining(logs.String(), "status messages"); n != 1 {
				t.Errorf("log lines about status messages = %d, want 1; log = %s", n, logs.String())
			}
			// The cleaned result is cached: a later poll does not log again.
			resp, body = env.status(t, "aaa")
			assertStatus(t, resp, body, http.StatusOK)
			if n := countLinesContaining(logs.String(), "status messages"); n != 1 {
				t.Errorf("log lines after cache hit = %d, want 1; log = %s", n, logs.String())
			}
			if runs := cmds.status.runs(t); len(runs) != 1 {
				t.Errorf("status_command runs = %d, want 1", len(runs))
			}
		})
	}
}

func TestStatusMessagesNullIsOmitted(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	var logs bytes.Buffer
	env.srv.logger = slog.New(slog.NewTextHandler(&logs, nil))
	askID(t, env, validRequest())
	cmds.status.setOutput(t, `{"state":"working","answered":false,"closed":false,"messages":null}`)
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	if got, want := strings.TrimSpace(string(body)), `{"state":"working","answered":false,"closed":false}`; got != want {
		t.Errorf("body = %s, want %s", got, want)
	}
	if logs.Len() != 0 {
		t.Errorf("log = %q, want nothing for null messages", logs.String())
	}
	if runs := cmds.status.runs(t); len(runs) != 1 {
		t.Errorf("status_command runs = %d, want 1", len(runs))
	}
}

func (e *testEnv) thread(t *testing.T, repo, pr string, opts ...reqOpt) (*http.Response, []byte) {
	t.Helper()
	q := url.Values{}
	q.Set("repo", repo)
	q.Set("pr", pr)
	return e.do(t, http.MethodGet, "/thread?"+q.Encode(), nil, opts...)
}

func TestThreadLookupReturnsStoredID(t *testing.T) {
	stateFile := filepath.Join(t.TempDir(), "threads.json")
	env, cmds := newThreadEnv(t, func(c *Config) { c.StateFile = stateFile })
	askID(t, env, validRequest())
	resp, body := env.thread(t, "octo/demo", "42")
	assertStatus(t, resp, body, http.StatusOK)
	if got, want := strings.TrimSpace(string(body)), `{"id":"aaa"}`; got != want {
		t.Errorf("body = %s, want %s", got, want)
	}
	if runs := cmds.status.runs(t); len(runs) != 0 {
		t.Errorf("status_command runs = %q, want none", runs)
	}
	if runs := cmds.followup.runs(t); len(runs) != 0 {
		t.Errorf("followup_command runs = %q, want none", runs)
	}

	restarted, cmds2 := newThreadEnv(t, func(c *Config) { c.StateFile = stateFile })
	resp, body = restarted.thread(t, "octo/demo", "42")
	assertStatus(t, resp, body, http.StatusOK)
	if got, want := strings.TrimSpace(string(body)), `{"id":"aaa"}`; got != want {
		t.Errorf("after restart body = %s, want %s", got, want)
	}
	if runs := cmds2.ask.runs(t); len(runs) != 0 {
		t.Errorf("after restart ask_command runs = %q, want none", runs)
	}
	if runs := cmds2.status.runs(t); len(runs) != 0 {
		t.Errorf("after restart status_command runs = %q, want none", runs)
	}
}

func TestThreadLookupReturnsClosedID(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	cmds.status.setOutput(t, `{"state":"done","answered":true,"closed":true}`)
	resp, body := env.status(t, "aaa")
	assertStatus(t, resp, body, http.StatusOK)
	resp, body = env.thread(t, "octo/demo", "42")
	assertStatus(t, resp, body, http.StatusOK)
	if got, want := strings.TrimSpace(string(body)), `{"id":"aaa"}`; got != want {
		t.Errorf("body = %s, want %s", got, want)
	}
}

func TestThreadLookupMiss(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	resp, body := env.thread(t, "octo/demo", "42")
	assertStatus(t, resp, body, http.StatusNotFound)
	if runs := cmds.ask.runs(t); len(runs) != 0 {
		t.Errorf("ask_command runs = %q, want none", runs)
	}
	if runs := cmds.status.runs(t); len(runs) != 0 {
		t.Errorf("status_command runs = %q, want none", runs)
	}
}

func TestThreadLookupReuseNone(t *testing.T) {
	env, cmds := newThreadEnv(t, func(c *Config) { c.Reuse = ReuseNone })
	askID(t, env, validRequest())
	resp, body := env.thread(t, "octo/demo", "42")
	assertStatus(t, resp, body, http.StatusNotFound)
	if runs := cmds.status.runs(t); len(runs) != 0 {
		t.Errorf("status_command runs = %q, want none", runs)
	}
}

func TestThreadLookupChecks(t *testing.T) {
	env, cmds := newThreadEnv(t, nil)
	askID(t, env, validRequest())
	port := strconv.Itoa(env.port)
	tests := []struct {
		name   string
		method string
		path   string
		opts   []reqOpt
		want   int
	}{
		{name: "bad token", method: http.MethodGet, path: "/thread?repo=octo/demo&pr=42", opts: []reqOpt{withoutHeader(TokenHeader)}, want: http.StatusUnauthorized},
		{name: "repo not allowed", method: http.MethodGet, path: "/thread?repo=other/repo&pr=42", want: http.StatusForbidden},
		{name: "bad pr", method: http.MethodGet, path: "/thread?repo=octo/demo&pr=0", want: http.StatusBadRequest},
		{name: "pr not integer", method: http.MethodGet, path: "/thread?repo=octo/demo&pr=abc", want: http.StatusBadRequest},
		{name: "negative pr", method: http.MethodGet, path: "/thread?repo=octo/demo&pr=-3", want: http.StatusBadRequest},
		{name: "bad host", method: http.MethodGet, path: "/thread?repo=octo/demo&pr=42", opts: []reqOpt{withHost("evil.example:" + port)}, want: http.StatusForbidden},
		{name: "post", method: http.MethodPost, path: "/thread?repo=octo/demo&pr=42", want: http.StatusMethodNotAllowed},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			resp, body := env.do(t, tc.method, tc.path, nil, tc.opts...)
			assertStatus(t, resp, body, tc.want)
		})
	}
	if runs := cmds.status.runs(t); len(runs) != 0 {
		t.Errorf("status_command runs = %q, want none", runs)
	}
}
