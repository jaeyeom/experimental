package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

func writeExec(t *testing.T, path, body string) string {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o700); err != nil { //nolint:gosec // G306: the fake gh must be executable
		t.Fatal(err)
	}
	return path
}

func TestFindGHSkipsItself(t *testing.T) {
	dir := t.TempDir()
	self := writeExec(t, filepath.Join(dir, "bin", "gh-throttle"), "")
	if err := os.MkdirAll(filepath.Join(dir, "shim"), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(self, filepath.Join(dir, "shim", "gh")); err != nil {
		t.Fatal(err)
	}
	realGH := writeExec(t, filepath.Join(dir, "real", "gh"), "")
	env := map[string]string{"PATH": strings.Join([]string{filepath.Join(dir, "empty"), filepath.Join(dir, "shim"), filepath.Join(dir, "real")}, string(os.PathListSeparator))}
	got, err := findGH(func(k string) string { return env[k] }, self)
	if err != nil || got != realGH {
		t.Fatalf("findGH = %q, %v; want %q", got, err, realGH)
	}

	env["GH_THROTTLE_GH"] = "/opt/gh"
	if got, err := findGH(func(k string) string { return env[k] }, self); err != nil || got != "/opt/gh" {
		t.Fatalf("findGH with GH_THROTTLE_GH = %q, %v", got, err)
	}

	env["GH_THROTTLE_GH"] = filepath.Join(dir, "shim", "gh")
	if _, err := findGH(func(k string) string { return env[k] }, self); err == nil {
		t.Fatal("findGH accepted GH_THROTTLE_GH pointing at gh-throttle itself")
	}

	delete(env, "GH_THROTTLE_GH")
	env["PATH"] = filepath.Join(dir, "shim")
	if _, err := findGH(func(k string) string { return env[k] }, self); err == nil {
		t.Fatal("findGH found only itself and did not fail")
	}
}

type harness struct {
	env    map[string]string
	stdout bytes.Buffer
	stderr bytes.Buffer
}

func newHarness(t *testing.T, ghBody string) *harness {
	t.Helper()
	dir := t.TempDir()
	return &harness{env: map[string]string{
		"GH_THROTTLE_GH":  writeExec(t, filepath.Join(dir, "gh"), ghBody),
		"XDG_RUNTIME_DIR": filepath.Join(dir, "run"),
		"XDG_STATE_HOME":  filepath.Join(dir, "state"),
	}}
}

func (h *harness) run(args []string, stdin string) int {
	return run(args, func(k string) string { return h.env[k] }, "/nonexistent/gh-throttle", strings.NewReader(stdin), &h.stdout, &h.stderr)
}

func TestPassesStdioAndExitCodeThrough(t *testing.T) {
	h := newHarness(t, "cat\necho \"args: $*\" >&2\nexit 3\n")
	code := h.run([]string{"issue", "comment", "1", "--body-file", "-"}, "the body\n")
	if code != 3 {
		t.Fatalf("exit %d, want 3", code)
	}
	if h.stdout.String() != "the body\n" || h.stderr.String() != "args: issue comment 1 --body-file -\n" {
		t.Fatalf("stdout %q stderr %q", h.stdout.String(), h.stderr.String())
	}
	if _, err := os.Stat(filepath.Join(h.env["XDG_RUNTIME_DIR"], "gh-throttle", "slots.lock")); err != nil {
		t.Fatalf("no slot lock: %v", err)
	}
}

func TestOffPassesThroughWithoutLock(t *testing.T) {
	h := newHarness(t, "cat\nexit 2\n")
	h.env["GH_THROTTLE_OFF"] = "1"
	if code := h.run([]string{"pr", "merge", "--body-file", "-"}, "in\n"); code != 2 || h.stdout.String() != "in\n" {
		t.Fatalf("exit %d stdout %q", code, h.stdout.String())
	}
	if _, err := os.Stat(filepath.Join(h.env["XDG_RUNTIME_DIR"], "gh-throttle")); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("GH_THROTTLE_OFF=1 touched the state dir: %v", err)
	}
}

func TestNestedCallPassesThrough(t *testing.T) {
	h := newHarness(t, "echo \"held=$GH_THROTTLE_HELD\"\n")
	if code := h.run([]string{"pr", "view"}, ""); code != 0 || h.stdout.String() != "held=1\n" {
		t.Fatalf("exit %d stdout %q; want gh to see GH_THROTTLE_HELD=1", code, h.stdout.String())
	}
	h.stdout.Reset()
	h.env["GH_THROTTLE_HELD"] = "1"
	h.env["XDG_RUNTIME_DIR"] = filepath.Join(t.TempDir(), "other")
	if code := h.run([]string{"pr", "view"}, ""); code != 0 {
		t.Fatalf("exit %d", code)
	}
	if _, err := os.Stat(filepath.Join(h.env["XDG_RUNTIME_DIR"], "gh-throttle")); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("a call under a held slot took a lock: %v", err)
	}
}

func TestBackoffIsLogged(t *testing.T) {
	state := filepath.Join(t.TempDir(), "count")
	h := newHarness(t, `count=$(cat "`+state+`" 2>/dev/null || echo 0)
echo $((count + 1)) > "`+state+`"
if [ $count -eq 0 ]; then echo "secondary rate limit" >&2; exit 1; fi
echo ok
`)
	h.env["GH_THROTTLE_BACKOFF"] = "0.3"
	if code := h.run([]string{"pr", "edit", "1", "--title", "secret"}, ""); code != 0 || h.stdout.String() != "ok\n" || h.stderr.String() != "" {
		t.Fatalf("exit %d stdout %q stderr %q", code, h.stdout.String(), h.stderr.String())
	}
	raw, err := os.ReadFile(filepath.Join(h.env["XDG_STATE_HOME"], "gh-throttle", "log.jsonl"))
	if err != nil {
		t.Fatal(err)
	}
	lines := strings.Split(strings.TrimSpace(string(raw)), "\n")
	var e struct {
		PID    int    `json:"pid"`
		Args   string `json:"args"`
		Reason string `json:"reason"`
		WaitMS int64  `json:"waitMs"`
	}
	if len(lines) != 1 || json.Unmarshal([]byte(lines[0]), &e) != nil {
		t.Fatalf("log = %q, want one JSON line", raw)
	}
	if e.PID != os.Getpid() || e.Args != "pr edit _ --title _" || e.Reason != "backoff" || e.WaitMS < 300 {
		t.Fatalf("log entry = %+v", e)
	}

	h.stdout.Reset()
	if code := h.run([]string{"--stats"}, ""); code != 0 || !strings.Contains(h.stdout.String(), "1 waits in the last 1h0m0s") {
		t.Fatalf("--stats exit %d:\n%s", code, h.stdout.String())
	}
}

func TestBadConfigFails(t *testing.T) {
	h := newHarness(t, "echo should not run\n")
	h.env["GH_THROTTLE_MAX_CONCURRENT"] = "zero"
	if code := h.run([]string{"pr", "view"}, ""); code == 0 || !strings.Contains(h.stderr.String(), "GH_THROTTLE_MAX_CONCURRENT") {
		t.Fatalf("exit %d stderr %q", code, h.stderr.String())
	}
}

func TestCacheServesRepeatedReads(t *testing.T) {
	count := filepath.Join(t.TempDir(), "count")
	h := newHarness(t, `n=$(cat "`+count+`" 2>/dev/null || echo 0)
echo $((n + 1)) > "`+count+`"
echo '{"title":"t"}'
`)
	h.env["GH_THROTTLE_CACHE_TTL"] = "1h"
	h.env["GH_REPO"] = "o/r"
	view := []string{"pr", "view", "1", "--json", "title"}
	runs := func() string {
		raw, _ := os.ReadFile(count)
		return strings.TrimSpace(string(raw))
	}
	for range 2 {
		if code := h.run(view, ""); code != 0 {
			t.Fatalf("exit %d stderr %q", code, h.stderr.String())
		}
	}
	if runs() != "1" || h.stdout.String() != "{\"title\":\"t\"}\n{\"title\":\"t\"}\n" {
		t.Fatalf("gh ran %s times, stdout %q; want one run and the output twice", runs(), h.stdout.String())
	}

	h.env["GH_THROTTLE_NO_CACHE"] = "1"
	h.run(view, "")
	delete(h.env, "GH_THROTTLE_NO_CACHE")
	if runs() != "2" {
		t.Fatalf("gh ran %s times; GH_THROTTLE_NO_CACHE=1 must run gh", runs())
	}

	h.run([]string{"pr", "close", "1"}, "")
	h.run(view, "")
	if runs() != "4" {
		t.Fatalf("gh ran %s times; a mutation of o/r must clear its cache", runs())
	}

	h.stdout.Reset()
	if code := h.run([]string{"--stats"}, ""); code != 0 || !strings.Contains(h.stdout.String(), "1 cache hits") {
		t.Fatalf("--stats exit %d:\n%s", code, h.stdout.String())
	}
}

func TestCacheOffByDefault(t *testing.T) {
	count := filepath.Join(t.TempDir(), "count")
	h := newHarness(t, `echo x >> "`+count+`"
echo '{}'
`)
	h.env["GH_REPO"] = "o/r"
	for range 2 {
		h.run([]string{"pr", "view", "1", "--json", "title"}, "")
	}
	if raw, _ := os.ReadFile(count); string(raw) != "x\nx\n" {
		t.Fatalf("gh runs %q, want 2", raw)
	}
	if _, err := os.Stat(filepath.Join(h.env["XDG_RUNTIME_DIR"], "gh-throttle", "cache")); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("cache off created the cache dir: %v", err)
	}
}

func TestCallerChainSkipsShells(t *testing.T) {
	procs := []procinfo.Process{
		{PID: 1, PPID: 0, Name: "launchd"},
		{PID: 10, PPID: 1, Name: "prsync", Argv: []string{"/usr/local/bin/prsync", "run"}},
		{PID: 20, PPID: 10, Name: "bash"},
		{PID: 30, PPID: 20, Name: "sh"},
	}
	caller, chain := callerChain(procs, 30)
	if caller != "prsync" || strings.Join(chain, ">") != "sh>bash>prsync" {
		t.Fatalf("callerChain = %q, %q; want prsync via sh>bash>prsync", caller, chain)
	}
	caller, chain = callerChain(procs, 10)
	if caller != "prsync" || strings.Join(chain, ">") != "prsync" {
		t.Fatalf("callerChain from the tool = %q, %q", caller, chain)
	}
	shellsOnly := []procinfo.Process{{PID: 5, PPID: 4, Name: "zsh"}, {PID: 4, PPID: 4, Name: "-bash"}}
	caller, chain = callerChain(shellsOnly, 5)
	if caller != "zsh" || strings.Join(chain, ">") != "zsh>-bash" {
		t.Fatalf("callerChain of only shells = %q, %q; want the parent shell", caller, chain)
	}
	if caller, chain = callerChain(nil, 7); caller != "" || chain != nil {
		t.Fatalf("callerChain of an unknown pid = %q, %q", caller, chain)
	}
}

func TestCallsAreLoggedAndStatsReadThem(t *testing.T) {
	h := newHarness(t, "echo '{\"data\":{\"rateLimit\":{\"cost\":1,\"remaining\":42}}}'\n")
	args := []string{"api", "graphql", "-f", "query={ rateLimit { cost remaining } viewer { login } }"}
	if code := h.run(args, ""); code != 0 {
		t.Fatalf("exit %d stderr %q", code, h.stderr.String())
	}
	raw, err := os.ReadFile(filepath.Join(h.env["XDG_STATE_HOME"], "gh-throttle", "calls.jsonl"))
	if err != nil {
		t.Fatal(err)
	}
	var e struct {
		PID       int    `json:"pid"`
		Kind      string `json:"kind"`
		Points    int    `json:"points"`
		Cwd       string `json:"cwd"`
		QueryHash string `json:"queryHash"`
		Remaining *int   `json:"rateLimitRemaining"`
	}
	if err := json.Unmarshal(raw, &e); err != nil {
		t.Fatalf("calls log %q: %v", raw, err)
	}
	wd, _ := os.Getwd()
	if e.PID != os.Getpid() || e.Kind != "graphql" || e.Points != 1 || e.Cwd != wd || e.QueryHash == "" || e.Remaining == nil || *e.Remaining != 42 {
		t.Fatalf("call entry = %s", raw)
	}

	h.stdout.Reset()
	if code := h.run([]string{"--stats", "--since", "10m", "--json"}, ""); code != 0 {
		t.Fatalf("--stats --json exit %d stderr %q", code, h.stderr.String())
	}
	var s struct {
		Calls         int `json:"calls"`
		QueriesByTime []struct {
			Key string `json:"key"`
		} `json:"queriesByTime"`
	}
	if err := json.Unmarshal(h.stdout.Bytes(), &s); err != nil || s.Calls != 1 || len(s.QueriesByTime) != 1 || s.QueriesByTime[0].Key != e.QueryHash {
		t.Fatalf("--stats --json = %s (%v)", h.stdout.String(), err)
	}
	if code := h.run([]string{"--stats", "--since", "nonsense"}, ""); code == 0 {
		t.Fatal("--stats accepted a bad --since")
	}
}
