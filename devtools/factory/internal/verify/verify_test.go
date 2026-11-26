package verify

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestCheckPass(t *testing.T) {
	fails, err := Check(context.Background(), passInput(t))
	if err != nil {
		t.Fatal(err)
	}
	if len(fails) != 0 {
		t.Fatalf("failures = %#v", fails)
	}
}

func TestCheckUsesClaimWorktree(t *testing.T) {
	in := passInput(t)
	const wt = "/repo-wt/PRJ-1"
	in.Claim.Worktree = wt
	var statusDir, baseDir, stashDir string
	in.Status = func(_ context.Context, dir string) (string, error) {
		statusDir = dir
		return "", nil
	}
	in.Base = func(_ context.Context, dir, _ string) (time.Time, error) {
		baseDir = dir
		return in.Now.Add(-24 * time.Hour), nil
	}
	in.Stash = func(_ context.Context, dir string) (int, error) {
		stashDir = dir
		return 0, nil
	}
	fails, err := Check(context.Background(), in)
	if err != nil {
		t.Fatal(err)
	}
	if len(fails) != 0 {
		t.Fatalf("failures = %#v", fails)
	}
	if statusDir != wt || baseDir != wt || stashDir != wt {
		t.Fatalf("dirs = status:%q base:%q stash:%q, want all %q", statusDir, baseDir, stashDir, wt)
	}
}

func TestCheckReportsEveryFailure(t *testing.T) {
	in := passInput(t)
	in.PRs[0].Number = 9
	in.View = func(context.Context, string, int) (gh.PRView, error) {
		return gh.PRView{
			Draft: false, Labels: []string{"bug"}, Reviewers: []string{"ada"},
			Body: "## Summary\n\n<!-- hint only -->\n\n## Test plan\n\n",
		}, nil
	}
	in.Status = func(context.Context, string) (string, error) { return " M a.go\n", nil }
	in.Base = func(context.Context, string, string) (time.Time, error) {
		return in.Now.Add(-BaseMaxAge - time.Hour), nil
	}
	n := 2
	in.Claim.StashCount = &n
	in.Stash = func(context.Context, string) (int, error) { return 0, nil }
	fails, err := Check(context.Background(), in)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"draft: expected a draft, found a ready pull request",
		"labels: expected none, found bug",
		"reviewers: expected none, found ada",
		`section "## Summary": expected content, found none`,
		`section "## Test plan": expected content, found none`,
		"checkout: expected a clean status, found M a.go",
		"base: expected a merge-base within 168h0m0s",
		"stash: expected 2, found 0",
	} {
		if !containsLine(fails, want) {
			t.Fatalf("missing %q in %#v", want, fails)
		}
	}
}

func TestCheckMissingStashAndPull(t *testing.T) {
	in := passInput(t)
	in.PRs = nil
	in.HasClaim = false
	fails, err := Check(context.Background(), in)
	if err != nil {
		t.Fatal(err)
	}
	if !containsLine(fails, "pull request: expected one, found none") || !containsLine(fails, "stash: start did not record a stash count") {
		t.Fatalf("failures = %#v", fails)
	}
}

func TestCheckNotAncestor(t *testing.T) {
	in := passInput(t)
	in.Base = func(context.Context, string, string) (time.Time, error) {
		return time.Time{}, ErrNotAncestor
	}
	fails, err := Check(context.Background(), in)
	if err != nil {
		t.Fatal(err)
	}
	if !containsLine(fails, "base: expected the merge-base to be an ancestor of origin/main, found it is not") {
		t.Fatalf("failures = %#v", fails)
	}
}

func TestCheckRequiresTemplate(t *testing.T) {
	in := passInput(t)
	in.Cfg.PRTemplate = ""
	if _, err := Check(context.Background(), in); err == nil || !strings.Contains(err.Error(), "expected pr_template, found empty") {
		t.Fatalf("error = %v", err)
	}
}

func TestSectionContentIgnoresHTMLComments(t *testing.T) {
	if hasContent([]string{"<!-- hint -->", "  ", "<!--", "hidden", "-->"}) {
		t.Fatal("comment-only section has content")
	}
	if !hasContent([]string{"<!-- hint --> ship it"}) {
		t.Fatal("text beside a comment has no content")
	}
}

func TestGitBase(t *testing.T) {
	const sha = "abc123"
	when := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "merge-base", "HEAD", "origin/main").
		WillSucceed(sha+"\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "merge-base", "--is-ancestor", sha, "origin/main").
		WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "log", "-1", "--format=%cI", sha).
		WillSucceed(when.Format(time.RFC3339)+"\n", 0).Once().Build()
	got, err := GitBase(mock)(context.Background(), "/repo", "main")
	if err != nil {
		t.Fatal(err)
	}
	if !got.Equal(when) {
		t.Fatalf("base time = %s", got)
	}

	mock = executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "merge-base", "HEAD", "origin/main").
		WillSucceed(sha+"\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", "/repo", "merge-base", "--is-ancestor", sha, "origin/main").
		WillSucceed("", 1).Once().Build()
	_, err = GitBase(mock)(context.Background(), "/repo", "main")
	if !errors.Is(err, ErrNotAncestor) {
		t.Fatalf("ancestor error = %v", err)
	}
}

func passInput(t *testing.T) Input {
	t.Helper()
	n := 0
	now := time.Date(2026, 11, 6, 12, 0, 0, 0, time.UTC)
	template := "## Summary\n\n<!-- one line -->\n\n## Test plan\n\n<!-- checks -->\n"
	return Input{
		ID:  "PRJ-1",
		Now: now,
		Cfg: config.Source{Cwd: "/repo", Base: "main", PRTemplate: "pull_request_template.md"},
		Claim: store.Claim{
			Source: "jira:acme", ID: "PRJ-1", State: store.Started, StashCount: &n,
		},
		HasClaim: true,
		PRs: []gh.PR{{
			Repo: "acme/widgets", Number: 9, Title: "[PRJ-1] ship", URL: "https://github.com/acme/widgets/pull/9",
		}},
		View: func(context.Context, string, int) (gh.PRView, error) {
			return gh.PRView{
				Draft: true,
				Body:  "## Summary\n\nshipped\n\n## Test plan\n\ngo test\n",
			}, nil
		},
		Status: func(context.Context, string) (string, error) { return "", nil },
		Stash:  func(context.Context, string) (int, error) { return 0, nil },
		Base: func(context.Context, string, string) (time.Time, error) {
			return now.Add(-24 * time.Hour), nil
		},
		ReadFile: func(name string) ([]byte, error) {
			if name != "/repo/pull_request_template.md" {
				t.Fatalf("template path = %s", name)
			}
			return []byte(template), nil
		},
	}
}

func containsLine(lines []string, part string) bool {
	for _, line := range lines {
		if strings.Contains(line, part) {
			return true
		}
	}
	return false
}

func TestCheckMergeModePass(t *testing.T) {
	n := 0
	now := time.Date(2026, 11, 6, 12, 0, 0, 0, time.UTC)
	in := Input{
		ID:       "PRJ-1",
		Now:      now,
		Cfg:      config.Source{Cwd: "/repo", Base: "main", Output: config.Output{Mode: config.OutputModeMerge}},
		Claim:    store.Claim{Source: "jira:acme", ID: "PRJ-1", State: store.Started, StashCount: &n, OutputMode: config.OutputModeMerge},
		HasClaim: true,
		Status:   func(context.Context, string) (string, error) { return "", nil },
		Stash:    func(context.Context, string) (int, error) { return 0, nil },
		Merge: func(_ context.Context, dir, base, id, _ string) ([]string, error) {
			if dir != "/repo" || base != "main" || id != "PRJ-1" {
				t.Fatalf("merge args = %q %q %q", dir, base, id)
			}
			return nil, nil
		},
	}
	fails, err := Check(context.Background(), in)
	if err != nil {
		t.Fatal(err)
	}
	if len(fails) != 0 {
		t.Fatalf("failures = %#v", fails)
	}
}

func TestCheckMergeModeFailures(t *testing.T) {
	n := 1
	now := time.Date(2026, 11, 6, 12, 0, 0, 0, time.UTC)
	in := Input{
		ID:       "PRJ-1",
		Now:      now,
		Cfg:      config.Source{Cwd: "/repo", Base: "main"},
		Claim:    store.Claim{Source: "jira:acme", ID: "PRJ-1", State: store.Started, StashCount: &n, OutputMode: config.OutputModeMerge},
		HasClaim: true,
		Status:   func(context.Context, string) (string, error) { return " M dirty.go\n", nil },
		Stash:    func(context.Context, string) (int, error) { return 0, nil },
		Merge: func(context.Context, string, string, string, string) ([]string, error) {
			return []string{"merge: expected a commit referencing PRJ-1 on origin/main, found none"}, nil
		},
	}
	fails, err := Check(context.Background(), in)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"checkout: expected a clean status, found M dirty.go",
		"merge: expected a commit referencing PRJ-1 on origin/main, found none",
		"stash: expected 1, found 0",
	} {
		if !containsLine(fails, want) {
			t.Fatalf("missing %q in %#v", want, fails)
		}
	}
}

func TestEffectiveOutputMode(t *testing.T) {
	if got := EffectiveOutputMode(true, store.Claim{OutputMode: config.OutputModeMerge}, config.Source{Output: config.Output{Mode: config.OutputModePR}}); got != config.OutputModeMerge {
		t.Fatalf("claim override = %q, want merge", got)
	}
	if got := EffectiveOutputMode(true, store.Claim{OutputMode: config.OutputModePR}, config.Source{Output: config.Output{Mode: config.OutputModeMerge}}); got != config.OutputModePR {
		t.Fatalf("claim pr override = %q, want pr", got)
	}
	if got := EffectiveOutputMode(false, store.Claim{}, config.Source{Output: config.Output{Mode: config.OutputModeMerge}}); got != config.OutputModeMerge {
		t.Fatalf("cfg fallback = %q, want merge", got)
	}
	if got := EffectiveOutputMode(false, store.Claim{}, config.Source{}); got != config.OutputModePR {
		t.Fatalf("empty fallback = %q, want pr", got)
	}
}

func TestGitMerge(t *testing.T) {
	logRecord := func(sha, parents, body string) string {
		return sha + "\x1f" + parents + "\x1f" + body + "\x1e\n"
	}

	t.Run("linear jira commit passes", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed(logRecord("aaa1111", "0000000", "[PRJ-1] ship it\n")+logRecord("0000000", "9999999", "old merge"), 0).Once().Build()
		fails, err := GitMerge(mock)(context.Background(), "/repo", "main", "PRJ-1", `\[{id}\]`)
		if err != nil || len(fails) != 0 {
			t.Fatalf("fails = %#v, err = %v", fails, err)
		}
	})

	t.Run("linear github Fixes #N in body passes", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed(logRecord("bbb2222", "aaa1111", "Ship feature\n\nFixes #42\n"), 0).Once().Build()
		fails, err := GitMerge(mock)(context.Background(), "/repo", "main", "acme/widgets#42", "")
		if err != nil || len(fails) != 0 {
			t.Fatalf("fails = %#v, err = %v", fails, err)
		}
	})

	t.Run("missing commit fails", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed(logRecord("bbb2222", "aaa1111", "[PRJ-10] other"), 0).Once().Build()
		fails, err := GitMerge(mock)(context.Background(), "/repo", "main", "PRJ-1", `\[{id}\]`)
		if err != nil || len(fails) != 1 || !strings.Contains(fails[0], "expected a commit referencing PRJ-1 on origin/main, found none") {
			t.Fatalf("fails = %#v, err = %v", fails, err)
		}
	})

	t.Run("merge commit fails", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed(logRecord("mmm3333", "0000000 aaa1111", "Merge branch 'feat'")+logRecord("aaa1111", "0000000", "[PRJ-1] ship it"), 0).Once().Build()
		fails, err := GitMerge(mock)(context.Background(), "/repo", "main", "PRJ-1", `\[{id}\]`)
		if err != nil || len(fails) != 1 || !strings.Contains(fails[0], "found merge commit mmm3333") {
			t.Fatalf("fails = %#v, err = %v", fails, err)
		}
	})

	t.Run("malformed log record errors", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed("bad-record\x1e", 0).Once().Build()
		if _, err := GitMerge(mock)(context.Background(), "/repo", "main", "PRJ-1", ""); err == nil || !strings.Contains(err.Error(), "expected 3 fields") {
			t.Fatalf("err = %v", err)
		}
	})
}

func TestFindLinearCommit(t *testing.T) {
	logRecord := func(sha, parents, body string) string {
		return sha + "\x1f" + parents + "\x1f" + body + "\x1e\n"
	}

	t.Run("returns newest linear commit", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed(logRecord("aaa11112222", "0000000", "[PRJ-1] second part\n\ndetails")+logRecord("0000000", "9999999", "[PRJ-1] first part"), 0).Once().Build()
		c, reason, err := FindLinearCommit(mock)(context.Background(), "/repo", "main", "PRJ-1", `\[{id}\]`)
		if err != nil || reason != "" {
			t.Fatalf("c = %+v, reason = %q, err = %v", c, reason, err)
		}
		if c.SHA != "aaa11112222" || c.Subject != "[PRJ-1] second part" {
			t.Fatalf("commit = %+v", c)
		}
	})

	t.Run("returns reason when missing", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("git", "-C", "/repo", "log", "origin/main", "-n", "100", "--format=%H%x1f%P%x1f%B%x1e").
			WillSucceed(logRecord("bbb2222", "aaa1111", "[PRJ-2] other"), 0).Once().Build()
		_, reason, err := FindLinearCommit(mock)(context.Background(), "/repo", "main", "PRJ-1", `\[{id}\]`)
		if err != nil || !strings.Contains(reason, "expected a commit referencing PRJ-1 on origin/main, found none") {
			t.Fatalf("reason = %q, err = %v", reason, err)
		}
	})
}
