package ghask

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"net/http"
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
