package ghask

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"log/slog"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"
	"time"
)

const testToken = "s3cret-token"

// fakeCommand writes a /bin/sh script that records its argv (one per
// line) to argsOut, copies the message file named by its first argument
// to msgOut, prints stdout and exits with code.
type fakeCommand struct {
	script  string
	argsOut string
	msgOut  string
}

func writeFakeCommand(t *testing.T, stdout string, code int) fakeCommand {
	t.Helper()
	dir := t.TempDir()
	fc := fakeCommand{
		script:  filepath.Join(dir, "asker.sh"),
		argsOut: filepath.Join(dir, "args.txt"),
		msgOut:  filepath.Join(dir, "msg.txt"),
	}
	body := fmt.Sprintf("#!/bin/sh\nprintf '%%s\\n' \"$@\" > %q\ncat \"$1\" > %q\nprintf '%%s' %q\nexit %d\n",
		fc.argsOut, fc.msgOut, stdout, code)
	if err := os.WriteFile(fc.script, []byte(body), 0o700); err != nil { //nolint:gosec // G306: the fake command must be executable
		t.Fatal(err)
	}
	return fc
}

type testEnv struct {
	ts    *httptest.Server
	srv   *Server
	fake  fakeCommand
	port  int
	clock *time.Time
}

func newTestEnv(t *testing.T, stdout string, code int, mutate func(*Config)) *testEnv {
	t.Helper()
	fake := writeFakeCommand(t, stdout, code)
	cfg := Config{
		Listen:         "127.0.0.1:0",
		Repos:          []string{"octo/demo"},
		AskCommand:     []string{fake.script, "{file}", "--pr", "{pr}", "--repo", "{repo}"},
		IDPattern:      regexp.MustCompile(`thread-([0-9a-f]+)`),
		RateLimit:      DefaultRateLimit,
		CommandTimeout: 30 * time.Second,
		Reuse:          ReuseNone,
		StateFile:      filepath.Join(t.TempDir(), "state", "threads.json"),
	}
	if mutate != nil {
		mutate(&cfg)
	}
	ts := httptest.NewUnstartedServer(nil)
	port := ts.Listener.Addr().(*net.TCPAddr).Port
	now := time.Date(2026, 11, 14, 10, 0, 0, 0, time.UTC)
	env := &testEnv{fake: fake, port: port, clock: &now}
	env.srv = NewServer(cfg, testToken, port)
	env.srv.now = func() time.Time { return *env.clock }
	env.srv.limiter.now = env.srv.now
	env.srv.tempDir = t.TempDir()
	env.srv.logger = slog.New(slog.DiscardHandler)
	ts.Config.Handler = env.srv
	ts.Start()
	t.Cleanup(ts.Close)
	env.ts = ts
	return env
}

type reqOpt func(*http.Request)

func withHeader(k, v string) reqOpt { return func(r *http.Request) { r.Header.Set(k, v) } }

func withoutHeader(k string) reqOpt { return func(r *http.Request) { r.Header.Del(k) } }

func withHost(h string) reqOpt { return func(r *http.Request) { r.Host = h } }

func (e *testEnv) do(t *testing.T, method, path string, body []byte, opts ...reqOpt) (*http.Response, []byte) {
	t.Helper()
	req, err := http.NewRequest(method, e.ts.URL+path, bytes.NewReader(body))
	if err != nil {
		t.Fatal(err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set(TokenHeader, testToken)
	for _, opt := range opts {
		opt(req)
	}
	client := &http.Client{Timeout: 60 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}
	assertNoCORS(t, resp)
	return resp, data
}

func (e *testEnv) ask(t *testing.T, req AskRequest, opts ...reqOpt) (*http.Response, []byte) {
	t.Helper()
	body, err := json.Marshal(req)
	if err != nil {
		t.Fatal(err)
	}
	return e.do(t, http.MethodPost, "/ask", body, opts...)
}

func assertNoCORS(t *testing.T, resp *http.Response) {
	t.Helper()
	for k := range resp.Header {
		if strings.HasPrefix(strings.ToLower(k), "access-control-") {
			t.Errorf("response has CORS header %s", k)
		}
	}
}

func wholePR(req AskRequest) AskRequest {
	req.Path = ""
	req.Side = ""
	req.StartLine = 0
	req.EndLine = 0
	req.Selection = ""
	return req
}

func assertStatus(t *testing.T, resp *http.Response, body []byte, want int) {
	t.Helper()
	if resp.StatusCode != want {
		t.Fatalf("status = %d, want %d; body %s", resp.StatusCode, want, body)
	}
}

func (e *testEnv) ran() bool {
	_, err := os.Stat(e.fake.argsOut)
	return err == nil
}

func TestAskSuccess(t *testing.T) {
	env := newTestEnv(t, "created thread-abc123\n", 0, nil)
	req := validRequest()
	resp, body := env.ask(t, req)
	assertStatus(t, resp, body, http.StatusOK)
	var got struct{ ID string }
	if err := json.Unmarshal(body, &got); err != nil || got.ID != "abc123" {
		t.Fatalf("body = %s (%v), want id abc123", body, err)
	}
	if ct := resp.Header.Get("Content-Type"); !strings.HasPrefix(ct, "application/json") {
		t.Errorf("Content-Type = %q", ct)
	}
	msg, err := os.ReadFile(env.fake.msgOut)
	if err != nil {
		t.Fatal(err)
	}
	if string(msg) != RenderMessage(req) {
		t.Errorf("message file =\n%s\nwant\n%s", msg, RenderMessage(req))
	}
	args := readArgs(t, env.fake.argsOut)
	if len(args) != 5 || args[1] != "--pr" || args[2] != "42" || args[3] != "--repo" || args[4] != "octo/demo" {
		t.Errorf("argv = %q", args)
	}
	if _, err := os.Stat(args[0]); !errors.Is(err, fs.ErrNotExist) {
		t.Errorf("message file %s still exists after the command (err %v)", args[0], err)
	}
}

func TestAskWholePR(t *testing.T) {
	env := newTestEnv(t, "created thread-abc123\n", 0, nil)
	req := wholePR(validRequest())
	resp, body := env.ask(t, req)
	assertStatus(t, resp, body, http.StatusOK)
	msg, err := os.ReadFile(env.fake.msgOut)
	if err != nil {
		t.Fatal(err)
	}
	if string(msg) != RenderMessage(req) {
		t.Errorf("message file =\n%s\nwant\n%s", msg, RenderMessage(req))
	}
	if strings.Contains(string(msg), "Code:") {
		t.Errorf("whole PR message includes a Code block:\n%s", msg)
	}
}

func TestAskPartialLocation(t *testing.T) {
	env := newTestEnv(t, "created thread-abc123\n", 0, nil)
	req := validRequest()
	req.Side = ""
	req.StartLine = 0
	req.EndLine = 0
	req.Selection = ""
	resp, body := env.ask(t, req)
	assertStatus(t, resp, body, http.StatusBadRequest)
	if env.ran() {
		t.Error("ask_command ran for a partial location")
	}
}

func readArgs(t *testing.T, path string) []string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return strings.Split(strings.TrimSuffix(string(data), "\n"), "\n")
}

func TestArgvNeverContainsPayload(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, nil)
	req := validRequest()
	req.Question = "QMARK $(touch pwned) ; rm -rf /"
	req.Selection = "SMARK `id`"
	req.Path = "dir/PMARK.go"
	req.PageURL = "https://github.com/octo/demo/pull/42/files#LMARK"
	req.HeadSHA = "deadbeefcafe"
	resp, body := env.ask(t, req)
	assertStatus(t, resp, body, http.StatusOK)
	data, err := os.ReadFile(env.fake.argsOut)
	if err != nil {
		t.Fatal(err)
	}
	for _, marker := range []string{"QMARK", "SMARK", "PMARK", "LMARK", "deadbeefcafe", "pwned", "RIGHT"} {
		if strings.Contains(string(data), marker) {
			t.Errorf("argv %q contains payload %q", data, marker)
		}
	}
}

func TestToken(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, nil)
	tests := []struct {
		name string
		opt  reqOpt
		want int
	}{
		{name: "missing", opt: withoutHeader(TokenHeader), want: http.StatusUnauthorized},
		{name: "empty", opt: withHeader(TokenHeader, ""), want: http.StatusUnauthorized},
		{name: "wrong", opt: withHeader(TokenHeader, "s3cret-tokeX"), want: http.StatusUnauthorized},
		{name: "prefix", opt: withHeader(TokenHeader, "s3cret"), want: http.StatusUnauthorized},
		{name: "right", opt: withHeader(TokenHeader, testToken), want: http.StatusOK},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			resp, body := env.ask(t, validRequest(), tc.opt)
			assertStatus(t, resp, body, tc.want)
		})
	}
}

func TestHostCheck(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, nil)
	port := strconv.Itoa(env.port)
	tests := []struct {
		host string
		want int
	}{
		{host: "127.0.0.1:" + port, want: http.StatusOK},
		{host: "localhost:" + port, want: http.StatusOK},
		{host: "evil.example:" + port, want: http.StatusForbidden},
		{host: "127.0.0.1", want: http.StatusForbidden},
		{host: "127.0.0.1:1", want: http.StatusForbidden},
		{host: "[::1]:" + port, want: http.StatusForbidden},
	}
	for _, tc := range tests {
		t.Run(tc.host, func(t *testing.T) {
			resp, body := env.ask(t, validRequest(), withHost(tc.host))
			assertStatus(t, resp, body, tc.want)
		})
	}
}

func TestOriginCheck(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, nil)
	tests := []struct {
		origin string
		want   int
	}{
		{origin: "https://github.com", want: http.StatusOK},
		{origin: "chrome-extension://abcdefghijklmnop", want: http.StatusOK},
		{origin: "moz-extension://1234-5678", want: http.StatusOK},
		{origin: "safari-web-extension://1234", want: http.StatusOK},
		{origin: "https://evil.example", want: http.StatusForbidden},
		{origin: "http://github.com", want: http.StatusForbidden},
		{origin: "https://github.com.evil.example", want: http.StatusForbidden},
		{origin: "https://gist.github.com", want: http.StatusForbidden},
		{origin: "null", want: http.StatusForbidden},
		{origin: "chrome-extension://", want: http.StatusForbidden},
	}
	for _, tc := range tests {
		t.Run(tc.origin, func(t *testing.T) {
			resp, body := env.ask(t, validRequest(), withHeader("Origin", tc.origin))
			assertStatus(t, resp, body, tc.want)
		})
	}
}

func TestPreflightAndMethods(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, nil)
	resp, body := env.do(t, http.MethodOptions, "/ask", nil,
		withHeader("Origin", "https://github.com"),
		withHeader("Access-Control-Request-Method", "POST"),
		withHeader("Access-Control-Request-Headers", TokenHeader))
	assertStatus(t, resp, body, http.StatusMethodNotAllowed)

	resp, body = env.do(t, http.MethodGet, "/ask", nil)
	assertStatus(t, resp, body, http.StatusMethodNotAllowed)

	resp, body = env.do(t, http.MethodPost, "/exec", []byte("{}"))
	assertStatus(t, resp, body, http.StatusNotFound)
	if env.ran() {
		t.Error("command ran for a rejected request")
	}
}

func TestRepoAllowlist(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, nil)
	req := validRequest()
	req.Repo = "octo/other"
	resp, body := env.ask(t, req)
	assertStatus(t, resp, body, http.StatusForbidden)
	if env.ran() {
		t.Error("command ran for a repo outside the allowlist")
	}
}

func TestSizeCaps(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, nil)
	tests := []struct {
		name   string
		mutate func(*AskRequest)
	}{
		{name: "selection", mutate: func(r *AskRequest) { r.Selection = strings.Repeat("s", MaxSelectionBytes+1) }},
		{name: "question", mutate: func(r *AskRequest) { r.Question = strings.Repeat("q", MaxQuestionBytes+1) }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			req := validRequest()
			tc.mutate(&req)
			resp, body := env.ask(t, req)
			assertStatus(t, resp, body, http.StatusRequestEntityTooLarge)
		})
	}
	t.Run("body", func(t *testing.T) {
		big := []byte(`{"question":"` + strings.Repeat("b", MaxBodyBytes) + `"}`)
		resp, body := env.do(t, http.MethodPost, "/ask", big)
		assertStatus(t, resp, body, http.StatusRequestEntityTooLarge)
	})
	if env.ran() {
		t.Error("command ran for an oversized request")
	}
}

func TestBadRequest(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, nil)
	resp, body := env.do(t, http.MethodPost, "/ask", []byte("not json"))
	assertStatus(t, resp, body, http.StatusBadRequest)
	req := validRequest()
	req.Side = "UP"
	resp, body = env.ask(t, req)
	assertStatus(t, resp, body, http.StatusBadRequest)
}

func TestRateLimit(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, func(c *Config) { c.RateLimit = 2 })
	for range 2 {
		resp, body := env.ask(t, validRequest())
		assertStatus(t, resp, body, http.StatusOK)
	}
	resp, body := env.ask(t, validRequest())
	assertStatus(t, resp, body, http.StatusTooManyRequests)

	*env.clock = env.clock.Add(time.Hour)
	resp, body = env.ask(t, validRequest())
	assertStatus(t, resp, body, http.StatusOK)
}

func TestRejectedRequestLog(t *testing.T) {
	const (
		question  = "QUESTION-SECRET"
		selection = "SELECTION-SECRET"
		pageMark  = "PAGESECRET"
		pageURL   = "https://github.com/octo/demo/pull/42/files#PAGESECRET"
		badToken  = "s3cret-tokeX"
	)
	secrets := []string{question, selection, pageMark, pageURL, badToken, testToken}

	t.Run("bad token", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		req := validRequest()
		req.Question = question
		req.Selection = selection
		req.PageURL = pageURL
		resp, body := env.ask(t, req, withHeader(TokenHeader, badToken))
		assertStatus(t, resp, body, http.StatusUnauthorized)
		assertRejectLog(t, logs, http.StatusUnauthorized, "bad token", secrets...)
	})

	t.Run("origin", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		req := validRequest()
		req.Question = question
		req.Selection = selection
		req.PageURL = pageURL
		resp, body := env.ask(t, req, withHeader("Origin", "https://evil.example"))
		assertStatus(t, resp, body, http.StatusForbidden)
		assertRejectLog(t, logs, http.StatusForbidden, "origin", secrets...)
	})

	t.Run("host", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		resp, body := env.ask(t, validRequest(), withHost("evil.example:1"))
		assertStatus(t, resp, body, http.StatusForbidden)
		assertRejectLog(t, logs, http.StatusForbidden, "host", secrets...)
	})

	t.Run("preflight", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		resp, body := env.do(t, http.MethodOptions, "/ask", nil,
			withHeader("Origin", "https://github.com"),
			withHeader("Access-Control-Request-Headers", TokenHeader))
		assertStatus(t, resp, body, http.StatusMethodNotAllowed)
		assertRejectLog(t, logs, http.StatusMethodNotAllowed, "preflight", secrets...)
	})

	t.Run("repo not allowed", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		req := validRequest()
		req.Repo = "octo/other"
		req.Question = question
		req.Selection = selection
		resp, body := env.ask(t, req)
		assertStatus(t, resp, body, http.StatusForbidden)
		line := assertRejectLog(t, logs, http.StatusForbidden, "repo not allowed", secrets...)
		if !strings.Contains(line, "repo=octo/other") || !strings.Contains(line, "pr=42") {
			t.Errorf("log = %q, want repo and pr", line)
		}
	})

	t.Run("rate limited", func(t *testing.T) {
		env, logs := rejectLogEnv(t, func(c *Config) { c.RateLimit = 1 })
		req := validRequest()
		req.Question = question
		req.Selection = selection
		req.PageURL = pageURL
		resp, body := env.ask(t, req)
		assertStatus(t, resp, body, http.StatusOK)
		if logs.Len() != 0 {
			t.Fatalf("log = %q, want nothing for a successful ask", logs.String())
		}
		resp, body = env.ask(t, req)
		assertStatus(t, resp, body, http.StatusTooManyRequests)
		line := assertRejectLog(t, logs, http.StatusTooManyRequests, "rate limited", secrets...)
		if !strings.Contains(line, "repo=octo/demo") || !strings.Contains(line, "pr=42") {
			t.Errorf("log = %q, want repo and pr", line)
		}
	})

	t.Run("too large", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		req := validRequest()
		req.Selection = strings.Repeat("S", MaxSelectionBytes+1)
		resp, body := env.ask(t, req)
		assertStatus(t, resp, body, http.StatusRequestEntityTooLarge)
		line := assertRejectLog(t, logs, http.StatusRequestEntityTooLarge, "too large", "SSSS")
		if len(line) > 400 {
			t.Errorf("log line is %d bytes, want a short reason", len(line))
		}
	})

	t.Run("unknown id", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		resp, body := env.do(t, http.MethodGet, "/status/"+testToken, nil)
		assertStatus(t, resp, body, http.StatusNotFound)
		assertRejectLog(t, logs, http.StatusNotFound, "unknown id", testToken)
	})

	t.Run("method", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		resp, body := env.do(t, http.MethodGet, "/ask", nil)
		assertStatus(t, resp, body, http.StatusMethodNotAllowed)
		assertRejectLog(t, logs, http.StatusMethodNotAllowed, "method", secrets...)
	})

	t.Run("bad request", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		resp, body := env.do(t, http.MethodPost, "/ask", []byte(badToken))
		assertStatus(t, resp, body, http.StatusBadRequest)
		assertRejectLog(t, logs, http.StatusBadRequest, "bad request", secrets...)
	})

	t.Run("thread not stored", func(t *testing.T) {
		env, logs := rejectLogEnv(t, nil)
		resp, body := env.do(t, http.MethodGet, "/thread?repo=octo/demo&pr=42", nil)
		assertStatus(t, resp, body, http.StatusNotFound)
		line := assertRejectLog(t, logs, http.StatusNotFound, "not found", secrets...)
		if !strings.Contains(line, "repo=octo/demo") || !strings.Contains(line, "pr=42") {
			t.Errorf("log = %q, want repo and pr", line)
		}
	})
}

func rejectLogEnv(t *testing.T, mutate func(*Config)) (*testEnv, *bytes.Buffer) {
	t.Helper()
	env := newTestEnv(t, "thread-ab", 0, mutate)
	logs := &bytes.Buffer{}
	env.srv.logger = slog.New(slog.NewTextHandler(logs, &slog.HandlerOptions{Level: slog.LevelWarn}))
	return env, logs
}

func assertRejectLog(t *testing.T, logs *bytes.Buffer, status int, reason string, forbid ...string) string {
	t.Helper()
	text := strings.TrimRight(logs.String(), "\n")
	lines := strings.Split(text, "\n")
	if text == "" || len(lines) != 1 {
		t.Fatalf("log lines = %q, want one warn line", logs.String())
	}
	line := lines[0]
	if !strings.Contains(line, "level=WARN") || !strings.Contains(line, "msg=\"gh-ask: rejected\"") {
		t.Errorf("log = %q, want a warn rejection", line)
	}
	if !strings.Contains(line, "status="+strconv.Itoa(status)) {
		t.Errorf("log = %q, want status %d", line, status)
	}
	wantReason := "reason=" + reason
	if strings.Contains(reason, " ") {
		wantReason = `reason="` + reason + `"`
	}
	if !strings.Contains(line, wantReason) {
		t.Errorf("log = %q, want %s", line, wantReason)
	}
	for _, secret := range forbid {
		if secret != "" && strings.Contains(line, secret) {
			t.Errorf("log = %q, contains %q", line, secret)
		}
	}
	return line
}

func TestRejectedRequestsDoNotUseRateLimit(t *testing.T) {
	env := newTestEnv(t, "thread-ab", 0, func(c *Config) { c.RateLimit = 1 })
	for range 3 {
		resp, body := env.ask(t, validRequest(), withHeader(TokenHeader, "wrong"))
		assertStatus(t, resp, body, http.StatusUnauthorized)
	}
	resp, body := env.ask(t, validRequest())
	assertStatus(t, resp, body, http.StatusOK)
}

func TestCommandFailures(t *testing.T) {
	tests := []struct {
		name   string
		stdout string
		code   int
	}{
		{name: "exit status", stdout: "thread-ab", code: 3},
		{name: "no id", stdout: "queued, no id", code: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			env := newTestEnv(t, tc.stdout, tc.code, nil)
			resp, body := env.ask(t, validRequest())
			assertStatus(t, resp, body, http.StatusBadGateway)
			var got struct{ Error string }
			if err := json.Unmarshal(body, &got); err != nil || got.Error == "" {
				t.Errorf("body = %s, want a short error", body)
			}
		})
	}
}

func TestCommandTimeout(t *testing.T) {
	dir := t.TempDir()
	script := filepath.Join(dir, "slow.sh")
	if err := os.WriteFile(script, []byte("#!/bin/sh\nexec sleep 30\n"), 0o700); err != nil { //nolint:gosec // G306: the fake command must be executable
		t.Fatal(err)
	}
	env := newTestEnv(t, "", 0, func(c *Config) {
		c.AskCommand = []string{script, "{file}"}
		c.CommandTimeout = 200 * time.Millisecond
	})
	start := time.Now()
	resp, body := env.ask(t, validRequest())
	assertStatus(t, resp, body, http.StatusBadGateway)
	if elapsed := time.Since(start); elapsed > 20*time.Second {
		t.Errorf("request took %v, want the command killed at its timeout", elapsed)
	}
}
