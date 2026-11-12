package ghlimit

import (
	"bytes"
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// countingGH stands in for gh: it counts runs and prints out, or fails
// with exit code exit.
type countingGH struct {
	runs int
	out  string
	exit int
}

func (g *countingGH) Run(context.Context) (Result, error) {
	g.runs++
	if g.exit != 0 {
		return Result{ExitCode: g.exit, Stderr: []byte("boom\n")}, nil
	}
	return Result{Stdout: []byte(g.out), StdoutKept: true, StdoutLen: int64(len(g.out))}, nil
}

func newCache(t *testing.T, l *Limiter, env map[string]string) *Cache {
	t.Helper()
	return &Cache{
		Dir:        filepath.Join(l.Dir, "cache"),
		TTL:        20 * time.Second,
		Identity:   "me",
		Getenv:     func(k string) string { return env[k] },
		GitRemotes: func() (string, error) { return "", errors.New("not a git repository") },
	}
}

// cacheRun runs args through c and l with gh, returning what reached
// stdout.
func cacheRun(t *testing.T, c *Cache, l *Limiter, gh *countingGH, args ...string) (string, Result) {
	t.Helper()
	var out bytes.Buffer
	res, err := c.Run(context.Background(), l, args, gh.Run, &out)
	if err != nil {
		t.Fatal(err)
	}
	if res.StdoutKept {
		out.Write(res.Stdout)
	}
	return out.String(), res
}

var prView = []string{"pr", "view", "1", "--json", "title", "-R", "o/r"}

func TestCacheHitWithinTTL(t *testing.T) {
	clock := newFakeClock()
	l, sink := newLimiter(t, clock)
	c := newCache(t, l, nil)
	gh := &countingGH{out: `{"title":"x"}`}

	cacheRun(t, c, l, gh, prView...)
	out, res := cacheRun(t, c, l, gh, prView...)
	if gh.runs != 1 || out != `{"title":"x"}` || res.ExitCode != 0 {
		t.Fatalf("runs %d, out %q, res %+v; want one run and the cached output", gh.runs, out, res)
	}
	entries := sink.Entries()
	if len(entries) != 1 || entries[0].Reason != ReasonCacheHit || entries[0].Args != "pr view _ --json _ -R _" {
		t.Fatalf("log = %+v, want one cache_hit line", entries)
	}

	clock.Sleep(20 * time.Second)
	cacheRun(t, c, l, gh, prView...)
	if gh.runs != 2 {
		t.Fatalf("runs %d after the TTL, want 2", gh.runs)
	}
}

func TestCacheEntryIsPrivate(t *testing.T) {
	l, _ := newLimiter(t, newFakeClock())
	c := newCache(t, l, nil)
	cacheRun(t, c, l, &countingGH{out: "{}"}, prView...)
	var files []string
	err := filepath.WalkDir(c.Dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		info, err := d.Info()
		if err != nil {
			t.Fatal(err)
		}
		want := os.FileMode(0o600)
		if d.IsDir() {
			want = 0o700
		} else {
			files = append(files, path)
		}
		if info.Mode().Perm() != want {
			t.Errorf("%s mode %v, want %v", path, info.Mode().Perm(), want)
		}
		return nil
	})
	if err != nil || len(files) != 1 {
		t.Fatalf("walk: %v, files %v; want one entry", err, files)
	}
}

func TestMutationInvalidatesSameRepo(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	c := newCache(t, l, nil)
	gh := &countingGH{out: "{}"}
	other := []string{"pr", "view", "2", "--json", "title", "-R", "o/other"}

	cacheRun(t, c, l, gh, prView...)
	cacheRun(t, c, l, gh, other...)
	cacheRun(t, c, l, gh, "pr", "merge", "1", "-R", "o/r")
	if gh.runs != 3 {
		t.Fatalf("runs %d, want 3", gh.runs)
	}
	cacheRun(t, c, l, gh, prView...)
	if gh.runs != 4 {
		t.Fatalf("read after a mutation of the same repo did not run gh (runs %d)", gh.runs)
	}
	cacheRun(t, c, l, gh, other...)
	if gh.runs != 4 {
		t.Fatalf("mutation of o/r invalidated o/other (runs %d)", gh.runs)
	}
}

func TestMutationInvalidatesViaURLAndAPIPath(t *testing.T) {
	for _, mutation := range [][]string{
		{"pr", "comment", "https://github.com/o/r/pull/1", "--body", "hi"},
		{"api", "-X", "POST", "repos/o/r/issues/1/comments", "-f", "body=hi"},
		{"api", "graphql", "-f", "query=mutation { x }"},
		{"issue", "close", "3"}, // repo unknown: everything goes
	} {
		clock := newFakeClock()
		l, _ := newLimiter(t, clock)
		c := newCache(t, l, nil)
		gh := &countingGH{out: "{}"}
		cacheRun(t, c, l, gh, prView...)
		cacheRun(t, c, l, gh, mutation...)
		cacheRun(t, c, l, gh, prView...)
		if gh.runs != 3 {
			t.Errorf("%v: runs %d, want the read after it to run gh", mutation, gh.runs)
		}
	}
}

func TestFailedReadIsNotCached(t *testing.T) {
	l, _ := newLimiter(t, newFakeClock())
	c := newCache(t, l, nil)
	gh := &countingGH{exit: 1}
	cacheRun(t, c, l, gh, prView...)
	gh.exit, gh.out = 0, "{}"
	cacheRun(t, c, l, gh, prView...)
	if gh.runs != 2 {
		t.Fatalf("runs %d, want the failure not cached", gh.runs)
	}
}

func TestUnkeptStdoutIsNotCached(t *testing.T) {
	l, _ := newLimiter(t, newFakeClock())
	c := newCache(t, l, nil)
	runs := 0
	run := func(context.Context) (Result, error) { runs++; return Result{}, nil }
	for range 2 {
		if _, err := c.Run(context.Background(), l, prView, run, &bytes.Buffer{}); err != nil {
			t.Fatal(err)
		}
	}
	if runs != 2 {
		t.Fatalf("runs %d; stdout that went straight to a terminal must not be cached", runs)
	}
}

func TestCacheOffStoresNothing(t *testing.T) {
	l, _ := newLimiter(t, newFakeClock())
	c := newCache(t, l, nil)
	c.TTL = 0
	gh := &countingGH{out: "{}"}
	cacheRun(t, c, l, gh, prView...)
	cacheRun(t, c, l, gh, prView...)
	if gh.runs != 2 {
		t.Fatalf("runs %d, want 2", gh.runs)
	}
	if _, err := os.Stat(c.Dir); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("cache off created %s: %v", c.Dir, err)
	}
}

func TestBypassSkipsCache(t *testing.T) {
	l, _ := newLimiter(t, newFakeClock())
	c := newCache(t, l, nil)
	gh := &countingGH{out: "{}"}
	cacheRun(t, c, l, gh, prView...)
	c.Bypass = true
	cacheRun(t, c, l, gh, prView...)
	if gh.runs != 2 {
		t.Fatalf("runs %d, want the bypassed call to run gh", gh.runs)
	}
}

func TestCacheHitTakesNoSlotOrGap(t *testing.T) {
	clock := newFakeClock()
	l, sink := newLimiter(t, clock)
	c := newCache(t, l, nil)
	gh := &countingGH{out: "{}"}
	cacheRun(t, c, l, gh, prView...)
	cacheRun(t, c, l, gh, "pr", "merge", "1", "-R", "o/other")

	release, _, err := l.acquireSlot(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	defer release()
	before := len(clock.slept)
	cacheRun(t, c, l, gh, prView...)
	if gh.runs != 2 || len(clock.slept) != before {
		t.Fatalf("runs %d, slept %v; want a hit without waiting", gh.runs, clock.slept[before:])
	}
	for _, e := range sink.Entries() {
		if e.Reason != ReasonCacheHit {
			t.Fatalf("log %+v; want only cache_hit", sink.Entries())
		}
	}
}

func TestReadBeforeInvalidationIsStale(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	c := newCache(t, l, nil)
	gh := &countingGH{out: "{}"}
	// A read that started before a concurrent mutation finished stores
	// after it; it must not be served.
	slow := func(ctx context.Context) (Result, error) {
		clock.Sleep(time.Second)
		if _, err := c.Run(ctx, &Limiter{Dir: t.TempDir(), Config: l.Config, Now: clock.Now, Sleep: clock.Sleep},
			[]string{"pr", "edit", "1", "-R", "o/r"}, gh.Run, &bytes.Buffer{}); err != nil {
			t.Fatal(err)
		}
		return gh.Run(ctx)
	}
	if _, err := c.Run(context.Background(), l, prView, slow, &bytes.Buffer{}); err != nil {
		t.Fatal(err)
	}
	cacheRun(t, c, l, gh, prView...)
	if gh.runs != 3 {
		t.Fatalf("runs %d, want the read that overlapped a mutation not served", gh.runs)
	}
}

func TestCacheReadRepo(t *testing.T) {
	remotes := map[string]string{
		"one":      "remote.origin.url git@github.com:Me/Proj.git\n",
		"resolved": "remote.origin.url https://github.com/me/fork\nremote.upstream.url ssh://git@github.com/org/proj.git\nremote.upstream.gh-resolved base\n",
		"two":      "remote.origin.url git@github.com:me/fork.git\nremote.upstream.url git@github.com:org/proj.git\n",
	}
	tests := []struct {
		name    string
		args    []string
		ghRepo  string
		remotes string
		want    string
		ok      bool
	}{
		{"view with -R", prView, "", "", "github.com/o/r", true},
		{"view with --repo=", []string{"pr", "view", "1", "--json=title", "--repo=host.example/o/r"}, "", "", "host.example/o/r", true},
		{"view by URL", []string{"pr", "view", "https://github.com/A/B/pull/3", "--json", "title"}, "", "", "github.com/a/b", true},
		{"view GH_REPO", []string{"pr", "view", "1", "--json", "title"}, "x/y", "", "github.com/x/y", true},
		{"view one remote", []string{"pr", "view", "1", "--json", "title"}, "", remotes["one"], "github.com/me/proj", true},
		{"view gh-resolved", []string{"pr", "view", "1", "--json", "title"}, "", remotes["resolved"], "github.com/org/proj", true},
		{"view ambiguous remotes", []string{"pr", "view", "1", "--json", "title"}, "", remotes["two"], "", false},
		{"view unknown repo", []string{"pr", "view", "1", "--json", "title"}, "", "", "", false},
		{"view current branch", []string{"pr", "view", "--json", "title", "-R", "o/r"}, "", "", "", false},
		{"view without json", []string{"pr", "view", "1", "-R", "o/r"}, "", "", "", false},
		{"view web", []string{"pr", "view", "1", "--json", "title", "-R", "o/r", "--web"}, "", "", "", false},
		{"list", []string{"pr", "list", "--json", "number", "-s", "open", "-L", "50", "-R", "o/r"}, "", "", "github.com/o/r", true},
		{"list unknown flag", []string{"pr", "list", "--json", "number", "--frob", "-R", "o/r"}, "", "", "", false},
		{"api repo path", []string{"api", "repos/o/r/pulls?state=open"}, "", "", "github.com/o/r", true},
		{"api explicit GET", []string{"api", "-X", "GET", "/repos/o/r/pulls", "-f", "state=open"}, "", "", "github.com/o/r", true},
		{"api hostname", []string{"api", "--hostname", "ghe.example", "repos/o/r"}, "", "", "ghe.example/o/r", true},
		{"api placeholders", []string{"api", "repos/{owner}/{repo}/pulls"}, "x/y", "", "github.com/x/y", true},
		{"api placeholders unknown", []string{"api", "repos/{owner}/{repo}/pulls"}, "", "", "", false},
		{"api branch placeholder", []string{"api", "repos/{owner}/{repo}/branches/{branch}"}, "x/y", "", "", false},
		{"api no repo", []string{"api", "user"}, "", "", "", true},
		{"api POST", []string{"api", "repos/o/r/issues", "-f", "title=t"}, "", "", "", false},
		{"api graphql", []string{"api", "graphql", "-f", "query={ viewer { login } }"}, "", "", "", false},
		{"api input", []string{"api", "repos/o/r", "--input", "body.json"}, "", "", "", false},
		{"issue view", []string{"issue", "view", "1", "--json", "title", "-R", "o/r"}, "", "", "", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			env := map[string]string{"GH_REPO": tt.ghRepo}
			c := &Cache{
				Getenv: func(k string) string { return env[k] },
				GitRemotes: func() (string, error) {
					if tt.remotes == "" {
						return "", errors.New("no remotes")
					}
					return tt.remotes, nil
				},
			}
			got, ok := c.readRepo(tt.args)
			if got != tt.want || ok != tt.ok {
				t.Fatalf("readRepo(%q) = %q, %v; want %q, %v", tt.args, got, ok, tt.want, tt.ok)
			}
		})
	}
}

func TestCacheIdentityDependsOnToken(t *testing.T) {
	home := t.TempDir()
	env := map[string]string{"HOME": home}
	getenv := func(k string) string { return env[k] }
	a := CacheIdentity(getenv)
	env["GH_TOKEN"] = "t1"
	b := CacheIdentity(getenv)
	if err := os.MkdirAll(filepath.Join(home, ".config", "gh"), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(home, ".config", "gh", "hosts.yml"), []byte("github.com:\n  user: me\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	c := CacheIdentity(getenv)
	if a == b || b == c || strings.Contains(c, "t1") {
		t.Fatalf("identities %q %q %q; want each distinct and no raw token", a, b, c)
	}
}

func TestExecRunnerKeepsStdout(t *testing.T) {
	gh := fakeGH(t, "echo hello\n")
	var out bytes.Buffer
	r, err := NewExecRunner(gh, nil, strings.NewReader(""), &out, &bytes.Buffer{})
	if err != nil {
		t.Fatal(err)
	}
	r.KeepStdout = true
	res, err := r.Run(context.Background())
	if err != nil || !res.StdoutKept || string(res.Stdout) != "hello\n" || out.String() != "hello\n" {
		t.Fatalf("Run = %+v, %v; stdout %q", res, err, out.String())
	}
}
