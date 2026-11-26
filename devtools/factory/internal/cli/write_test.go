package cli

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	bearKey = "jira:acme"
	cubKey  = "jira:cub"
	ghKey   = "github:experimental"
)

func TestLinkAndUnlinkUpdateMirror(t *testing.T) {
	// The config leaves dry_run at its default, true. link still writes.
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey, "PRJ-1", "PRJ-2")
	useSources(t, map[string]*fake.Source{"acme": src})
	seedItems(t, db, fakeItems(src)...)

	stdout, stderr, code := runWriteCmd(t, cfg, db, "link", "PRJ-1", "PRJ-2")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	assertBlock(t, db, bearKey, "PRJ-1", "PRJ-2", "L-PRJ-1-PRJ-2")

	stdout, stderr, code = runWriteCmd(t, cfg, db, "unlink", "PRJ-1", "PRJ-2")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "" || stderr != "" {
		t.Fatalf("unlink stdout = %q, stderr = %q", stdout, stderr)
	}
	assertNoBlock(t, db, bearKey, "PRJ-1", "PRJ-2")

	runs := listRuns(t, db)
	if len(runs) != 2 || runs[0].Command != "unlink" || runs[1].Command != "link" {
		t.Fatalf("runs = %+v", runs)
	}
	if runs[0].Outcome != "ok" || runs[1].Outcome != "ok" {
		t.Fatalf("outcomes = %s %s", runs[0].Outcome, runs[1].Outcome)
	}
}

func TestLinkResolvesMissingMirrorID(t *testing.T) {
	cfg := writeSources(t,
		srcSpec{kind: "jira", name: "acme"},
		srcSpec{kind: "github", name: "experimental", repo: "acme/widgets"},
	)
	db := filepath.Join(t.TempDir(), "factory.db")
	jira := fakeWith(bearKey)
	gh := fakeWith(ghKey, "acme/widgets#7", "acme/widgets#8")
	useSources(t, map[string]*fake.Source{"acme": jira, "experimental": gh})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "link", "acme/widgets#7", "acme/widgets#8")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	if len(jira.Calls) != 0 {
		t.Fatalf("jira was probed: %#v", jira.Calls)
	}
	assertBlock(t, db, ghKey, "acme/widgets#7", "acme/widgets#8", "L-acme/widgets#7-acme/widgets#8")
}

func TestUnlinkMissingLink(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey, "PRJ-1", "PRJ-2")
	useSources(t, map[string]*fake.Source{"acme": src})
	seedItems(t, db, fakeItems(src)...)

	stdout, stderr, code := runWriteCmd(t, cfg, db, "unlink", "PRJ-1", "PRJ-2")
	assertExit(t, code, stdout, stderr, ExitError)
	if stdout != "" {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.HasPrefix(stderr, "factory:") {
		t.Fatalf("stderr = %q", stderr)
	}
	assertNoBlock(t, db, bearKey, "PRJ-1", "PRJ-2")
}

func TestFilePrintsOnlyID(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey, "PRJ-2")
	useSources(t, map[string]*fake.Source{"acme": src})
	seedItems(t, db, fakeItems(src)...)

	stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--source", bearKey,
		"--title", "Follow up",
		"--body", "because",
		"--parent", "PRJ-9",
		"--label", "a",
		"--label", "b",
		"--blocks", "PRJ-2",
	)
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "FAKE-1\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	call := oneCall(t, src, "Create")
	wantArgs := []string{"Follow up", "because", "PRJ-9", "a,b", "PRJ-2", "", ""}
	if strings.Join(call.Args, "|") != strings.Join(wantArgs, "|") {
		t.Fatalf("Create args = %#v", call.Args)
	}
	st := reopen(t, db)
	got, err := st.Item(context.Background(), bearKey, "FAKE-1")
	if err != nil {
		t.Fatal(err)
	}
	if got.Title != "Follow up" || got.Parent != "PRJ-9" || strings.Join(got.Labels, ",") != "a,b" {
		t.Fatalf("item = %+v", got)
	}
	assertBlock(t, db, bearKey, "FAKE-1", "PRJ-2", "L-FAKE-1-PRJ-2")
	runs := listRuns(t, db)
	if len(runs) != 1 || runs[0].Command != "file" || runs[0].Outcome != "ok" {
		t.Fatalf("run = %+v", runs)
	}
}

func TestFileBodyFile(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey)
	useSources(t, map[string]*fake.Source{"acme": src})
	bodyPath := filepath.Join(t.TempDir(), "body.txt")
	if err := os.WriteFile(bodyPath, []byte("hello\nthere"), 0o600); err != nil {
		t.Fatal(err)
	}

	stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--source", bearKey, "--title", "T", "--body-file", bodyPath)
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "FAKE-1\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	call := oneCall(t, src, "Create")
	if len(call.Args) < 2 || call.Args[1] != "hello\nthere" {
		t.Fatalf("Create args = %#v", call.Args)
	}

	stdout, stderr, code = runWriteCmd(t, cfg, db, "file", "--source", bearKey, "--title", "T", "--body", "x", "--body-file", bodyPath)
	assertExit(t, code, stdout, stderr, ExitUsage)
	if stdout != "" || !strings.Contains(stderr, "only one of --body and --body-file") {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	if strings.HasPrefix(stderr, "factory:") {
		t.Fatalf("usage error printed as a command error: %s", stderr)
	}
}

func TestFileItemSource(t *testing.T) {
	t.Run("no item source files into --source", func(t *testing.T) {
		cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"}, srcSpec{kind: "jira", name: "cub"})
		db := filepath.Join(t.TempDir(), "factory.db")
		acme, cub := fakeWith(bearKey), fakeWith(cubKey)
		useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})
		stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--title", "T", "--source", cubKey)
		assertExit(t, code, stdout, stderr, ExitOK)
		if stdout != "FAKE-1\n" || methodCount(cub, "Create") != 1 || methodCount(acme, "Create") != 0 {
			t.Fatalf("stdout = %q, acme = %#v, cub = %#v", stdout, acme.Calls, cub.Calls)
		}
	})

	t.Run("item source is the default", func(t *testing.T) {
		t.Setenv("FACTORY_ITEM_SOURCE", cubKey)
		cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"}, srcSpec{kind: "jira", name: "cub"})
		db := filepath.Join(t.TempDir(), "factory.db")
		acme, cub := fakeWith(bearKey), fakeWith(cubKey)
		useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})
		stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--title", "T")
		assertExit(t, code, stdout, stderr, ExitOK)
		if methodCount(cub, "Create") != 1 || methodCount(acme, "Create") != 0 {
			t.Fatalf("acme = %#v, cub = %#v", acme.Calls, cub.Calls)
		}
	})

	t.Run("a different --source is refused outright", func(t *testing.T) {
		t.Setenv("FACTORY_ITEM_SOURCE", bearKey)
		cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"}, srcSpec{kind: "jira", name: "cub"})
		db := filepath.Join(t.TempDir(), "factory.db")
		acme, cub := fakeWith(bearKey), fakeWith(cubKey)
		useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})
		stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--title", "T", "--source", cubKey)
		assertExit(t, code, stdout, stderr, ExitError)
		if !strings.Contains(stderr, "file: this item is from jira:acme; factory never files into jira:cub") {
			t.Fatalf("stderr = %q", stderr)
		}
		if methodCount(acme, "Create")+methodCount(cub, "Create") != 0 {
			t.Fatalf("filed anyway: acme = %#v, cub = %#v", acme.Calls, cub.Calls)
		}
	})

	t.Run("item source not configured here", func(t *testing.T) {
		t.Setenv("FACTORY_ITEM_SOURCE", "jira:x")
		cfg := writeSources(t, srcSpec{kind: "github", name: "experimental", repo: "acme/widgets"})
		db := filepath.Join(t.TempDir(), "factory.db")
		gh := fakeWith(ghKey)
		useSources(t, map[string]*fake.Source{"experimental": gh})
		for _, args := range [][]string{{"--title", "T"}, {"--title", "T", "--source", ghKey}} {
			stdout, stderr, code := runWriteCmd(t, cfg, db, append([]string{"file"}, args...)...)
			assertExit(t, code, stdout, stderr, ExitError)
			if !strings.Contains(stderr, "file: source jira:x is not configured on this machine; report back to your coordinator instead") {
				t.Fatalf("args %v: stderr = %q", args, stderr)
			}
		}
		if methodCount(gh, "Create") != 0 {
			t.Fatalf("filed anyway: %#v", gh.Calls)
		}
	})

	t.Run("own public repo files with a warning", func(t *testing.T) {
		t.Setenv("FACTORY_ITEM_SOURCE", ghKey)
		cfg := writeSources(t, srcSpec{kind: "github", name: "experimental", repo: "acme/widgets"})
		db := filepath.Join(t.TempDir(), "factory.db")
		gh := fakeWith(ghKey)
		useSources(t, map[string]*fake.Source{"experimental": gh})
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "repo", "view", "acme/widgets", "--json", "visibility").
			WillSucceed(`{"visibility":"PUBLIC"}`, 0).Once().Build()
		useExec(t, mock)
		stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--title", "T")
		assertExit(t, code, stdout, stderr, ExitOK)
		if stdout != "FAKE-1\n" || methodCount(gh, "Create") != 1 {
			t.Fatalf("stdout = %q, calls = %#v", stdout, gh.Calls)
		}
		if stderr != "factory: warning: acme/widgets is a public repository; this follow-up will be public\n" {
			t.Fatalf("stderr = %q", stderr)
		}
	})
}

func TestFileSourceRequired(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"}, srcSpec{kind: "jira", name: "cub"})
	acme := fakeWith(bearKey, "PRJ-1")
	cub := fakeWith(cubKey, "PRJ-9")
	useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})

	t.Run("no ids", func(t *testing.T) {
		db := filepath.Join(t.TempDir(), "factory.db")
		stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--title", "T")
		assertSourceRequired(t, code, stdout, stderr)
		if methodCount(acme, "Create")+methodCount(cub, "Create") != 0 {
			t.Fatalf("created despite --source required: %#v %#v", acme.Calls, cub.Calls)
		}
	})

	t.Run("ids in different sources", func(t *testing.T) {
		db := filepath.Join(t.TempDir(), "factory.db")
		seedItems(t, db, fakeItems(acme)...)
		seedItems(t, db, fakeItems(cub)...)
		before := len(acme.Calls) + len(cub.Calls)
		stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--title", "T", "--blocks", "PRJ-1", "--blocked-by", "PRJ-9")
		assertSourceRequired(t, code, stdout, stderr)
		if len(acme.Calls)+len(cub.Calls) != before {
			t.Fatalf("sources were called: %#v %#v", acme.Calls, cub.Calls)
		}
	})

	t.Run("bare name", func(t *testing.T) {
		db := filepath.Join(t.TempDir(), "factory.db")
		stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--title", "T", "--source", "acme")
		assertExit(t, code, stdout, stderr, ExitUsage)
		if stdout != "" || !strings.Contains(stderr, cubKey) || !strings.Contains(stderr, `found "acme"`) {
			t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
		}
	})
}

func TestReleaseUpdatesClaim(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey, "PRJ-1")
	if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
		t.Fatal(err)
	}
	useSources(t, map[string]*fake.Source{"acme": src})
	seedItems(t, db, fakeItems(fakeWith(bearKey, "PRJ-1"))...)
	older := time.Date(2026, 11, 1, 8, 0, 0, 0, time.UTC)
	newer := older.Add(time.Hour)
	seedClaim(t, db, store.Claim{
		Source: bearKey, ID: "PRJ-2", ClaimedAt: older, State: store.Claimed,
		Detail: "older", TabID: "old-tab", UpdatedAt: older,
	})
	seedClaim(t, db, store.Claim{
		Source: bearKey, ID: "PRJ-1", ClaimedAt: newer, State: store.Started,
		Detail: "newer", TabID: "new-tab", WorkspaceID: "ws", Machine: "host", UpdatedAt: newer,
	})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "release", "PRJ-1")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	if methodCount(src, "Release") != 1 {
		t.Fatalf("calls = %#v", src.Calls)
	}
	st := reopen(t, db)
	latest, err := st.LatestClaim(context.Background(), bearKey, "PRJ-1")
	if err != nil {
		t.Fatal(err)
	}
	if latest.State != store.Released || !latest.ClaimedAt.Equal(newer) ||
		latest.Detail != "newer" || latest.TabID != "new-tab" || latest.WorkspaceID != "ws" {
		t.Fatalf("latest = %+v", latest)
	}
	active, err := st.ActiveClaims(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(active) != 1 || !active[0].ClaimedAt.Equal(older) || active[0].State != store.Claimed || active[0].Detail != "older" {
		t.Fatalf("active = %+v", active)
	}
	runs := listRuns(t, db)
	if len(runs) != 1 || runs[0].Command != "release" || runs[0].Outcome != "ok" {
		t.Fatalf("run = %+v", runs)
	}
}

func TestReleaseWithoutClaim(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey, "PRJ-1")
	if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
		t.Fatal(err)
	}
	useSources(t, map[string]*fake.Source{"acme": src})
	seedItems(t, db, fakeItems(src)...)

	stdout, stderr, code := runWriteCmd(t, cfg, db, "release", "PRJ-1")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	_, err := reopen(t, db).LatestClaim(context.Background(), bearKey, "PRJ-1")
	if !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("claim error = %v, want not found", err)
	}
}

func TestReleaseWorktreeCleanAndDirty(t *testing.T) {
	t.Run("clean_worktree_removed", func(t *testing.T) {
		cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
		db := filepath.Join(t.TempDir(), "factory.db")
		src := fakeWith(bearKey, "PRJ-1")
		if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
			t.Fatal(err)
		}
		useSources(t, map[string]*fake.Source{"acme": src})
		seedItems(t, db, fakeItems(src)...)
		wtDir := filepath.Join(t.TempDir(), "wt-PRJ-1")
		seedClaim(t, db, store.Claim{
			Source: bearKey, ID: "PRJ-1", ClaimedAt: day1, State: store.Started,
			Machine: "host", UpdatedAt: day1, Worktree: wtDir,
		})

		loadedCfg, err := config.Load(cfg, config.OSEnv())
		if err != nil {
			t.Fatal(err)
		}
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("git", "-C", wtDir, "status", "--porcelain").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("git", "-C", loadedCfg.Sources[0].Cwd, "worktree", "remove", wtDir).WillSucceed("", 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runWriteCmd(t, cfg, db, "release", "PRJ-1")
		assertExit(t, code, stdout, stderr, ExitOK)
		if stdout != "" || stderr != "" {
			t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("dirty_worktree_preserved_with_warning", func(t *testing.T) {
		cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
		db := filepath.Join(t.TempDir(), "factory.db")
		src := fakeWith(bearKey, "PRJ-1")
		if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
			t.Fatal(err)
		}
		useSources(t, map[string]*fake.Source{"acme": src})
		seedItems(t, db, fakeItems(src)...)
		wtDir := filepath.Join(t.TempDir(), "wt-PRJ-1")
		seedClaim(t, db, store.Claim{
			Source: bearKey, ID: "PRJ-1", ClaimedAt: day1, State: store.Started,
			Machine: "host", UpdatedAt: day1, Worktree: wtDir,
		})

		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("git", "-C", wtDir, "status", "--porcelain").WillSucceed(" M dirty.go\n", 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runWriteCmd(t, cfg, db, "release", "PRJ-1")
		assertExit(t, code, stdout, stderr, ExitOK)
		if !strings.Contains(stderr, wtDir) || !strings.Contains(stderr, "uncommitted") {
			t.Fatalf("stderr missing dirty worktree warning naming %q: %q", wtDir, stderr)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})
}

func TestWriteUnknownID(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"}, srcSpec{kind: "jira", name: "cub"})
	acme := fakeWith(bearKey, "PRJ-1", "PRJ-2")
	cub := fakeWith(cubKey, "PRJ-1")
	useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})

	t.Run("not in mirror", func(t *testing.T) {
		db := filepath.Join(t.TempDir(), "factory.db")
		stdout, stderr, code := runWriteCmd(t, cfg, db, "release", "PRJ-404")
		assertExit(t, code, stdout, stderr, ExitUsage)
		if stdout != "" || !strings.Contains(stderr, "expected one item, found none") {
			t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
		}
		if strings.HasPrefix(stderr, "factory:") {
			t.Fatalf("usage error printed as a command error: %s", stderr)
		}
		if methodCount(acme, "Release")+methodCount(cub, "Release") != 0 {
			t.Fatalf("released an unknown id: %#v %#v", acme.Calls, cub.Calls)
		}
	})

	t.Run("not a source id", func(t *testing.T) {
		db := filepath.Join(t.TempDir(), "factory.db")
		stdout, stderr, code := runWriteCmd(t, cfg, db, "link", "nope", "PRJ-1")
		assertExit(t, code, stdout, stderr, ExitUsage)
		if stdout != "" || !strings.Contains(stderr, "expected a Jira key or owner/repo#number") {
			t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
		}
	})

	t.Run("ambiguous", func(t *testing.T) {
		db := filepath.Join(t.TempDir(), "factory.db")
		seedItems(t, db, fakeItems(acme)...)
		seedItems(t, db, fakeItems(cub)...)
		stdout, stderr, code := runWriteCmd(t, cfg, db, "link", "PRJ-1", "PRJ-2")
		assertExit(t, code, stdout, stderr, ExitUsage)
		if stdout != "" || !strings.Contains(stderr, "expected one source") {
			t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
		}
		if methodCount(acme, "Link")+methodCount(cub, "Link") != 0 {
			t.Fatalf("linked an ambiguous id: %#v %#v", acme.Calls, cub.Calls)
		}
	})

	t.Run("bad github id", func(t *testing.T) {
		db := filepath.Join(t.TempDir(), "factory.db")
		stdout, stderr, code := runWriteCmd(t, cfg, db, "release", "acme/widgets#01")
		assertExit(t, code, stdout, stderr, ExitUsage)
		if stdout != "" || !strings.Contains(stderr, "expected owner/repo#<number>") {
			t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
		}
	})
}

func TestWriteUsage(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	useSources(t, map[string]*fake.Source{"acme": fakeWith(bearKey, "PRJ-1")})

	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "link arity", args: []string{"link", "PRJ-1"}, want: "expected blocker and blocked"},
		{name: "release arity", args: []string{"release"}, want: "expected one id"},
		{name: "file args", args: []string{"file", "--title", "T", "extra"}, want: "expected no arguments"},
		{name: "empty title", args: []string{"file", "--title", ""}, want: "non-empty --title"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			stdout, stderr, code := runWriteCmd(t, cfg, db, tc.args...)
			assertExit(t, code, stdout, stderr, ExitUsage)
			if stdout != "" || !strings.Contains(stderr, tc.want) {
				t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
			}
		})
	}

	stdout, stderr, code := runWriteCmd(t, cfg, db, "file")
	assertExit(t, code, stdout, stderr, ExitUsage)
	if stdout != "" || !strings.Contains(stderr, "title") {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
}

type srcSpec struct {
	kind string
	name string
	repo string
}

func writeSources(t *testing.T, specs ...srcSpec) string {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var body strings.Builder
	for _, spec := range specs {
		switch spec.kind {
		case "jira":
			fmt.Fprintf(&body, `
[[source]]
name = %q
kind = "jira"
site = "https://example.atlassian.net"
email = "dev@example.com"
token_file = %q
jql = "project = PRJ"
claim_status = "In Progress"
cwd = %q
base = "main"
start_prompt = "work {id}"
`, spec.name, token, cwd)
		case "github":
			fmt.Fprintf(&body, `
[[source]]
name = %q
kind = "github"
repo = %q
cwd = %q
base = "main"
start_prompt = "work {id}"
`, spec.name, spec.repo, cwd)
		default:
			t.Fatalf("source kind: expected jira or github, found %q", spec.kind)
		}
	}
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body.String()), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func fakeWith(key string, ids ...string) *fake.Source {
	now := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	items := make(map[string]source.Item, len(ids))
	for _, id := range ids {
		items[id] = item(key, id, now)
	}
	return &fake.Source{SourceName: key, Account: "acct", Items: items}
}

func fakeItems(s *fake.Source) []source.Item {
	out := make([]source.Item, 0, len(s.Items))
	for _, it := range s.Items {
		out = append(out, it)
	}
	return out
}

func seedItems(t *testing.T, dbPath string, items ...source.Item) {
	t.Helper()
	ctx := context.Background()
	st, err := store.Open(ctx, dbPath)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := st.Close(); err != nil {
			t.Errorf("close: %v", err)
		}
	}()
	grouped := map[string][]source.Item{}
	for _, it := range items {
		grouped[it.Source] = append(grouped[it.Source], it)
	}
	for key, group := range grouped {
		if err := st.UpsertItems(ctx, key, group); err != nil {
			t.Fatal(err)
		}
	}
}

func seedClaim(t *testing.T, dbPath string, claim store.Claim) {
	t.Helper()
	ctx := context.Background()
	st, err := store.Open(ctx, dbPath)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := st.Close(); err != nil {
			t.Errorf("close: %v", err)
		}
	}()
	if err := st.InsertClaim(ctx, claim); err != nil {
		t.Fatal(err)
	}
}

func runWriteCmd(t *testing.T, cfg, db string, args ...string) (string, string, int) {
	t.Helper()
	full := make([]string, 0, len(args)+4)
	full = append(full, args[0], "--config", cfg, "--db", db)
	full = append(full, args[1:]...)
	return runCmd(t, full...)
}

func assertExit(t *testing.T, code int, stdout, stderr string, want int) {
	t.Helper()
	if code != want {
		t.Fatalf("exit = %d, want %d, stdout = %q, stderr = %s", code, want, stdout, stderr)
	}
}

func assertSourceRequired(t *testing.T, code int, stdout, stderr string) {
	t.Helper()
	assertExit(t, code, stdout, stderr, ExitUsage)
	if stdout != "" || !strings.Contains(stderr, "--source required") {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	if strings.HasPrefix(stderr, "factory:") {
		t.Fatalf("usage error printed as a command error: %s", stderr)
	}
}

func assertBlock(t *testing.T, db, key, blocker, blocked, linkID string) {
	t.Helper()
	ctx := context.Background()
	st := reopen(t, db)
	got, err := st.Item(ctx, key, blocked)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.BlockedBy) != 1 || got.BlockedBy[0].ID != blocker || got.BlockedBy[0].LinkID != linkID {
		t.Fatalf("blocked-by = %+v", got.BlockedBy)
	}
	blocks, err := st.Blocks(ctx, key, blocker)
	if err != nil {
		t.Fatal(err)
	}
	if len(blocks) != 1 || blocks[0] != blocked {
		t.Fatalf("blocks = %#v", blocks)
	}
}

func assertNoBlock(t *testing.T, db, key, blocker, blocked string) {
	t.Helper()
	ctx := context.Background()
	st := reopen(t, db)
	got, err := st.Item(ctx, key, blocked)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.BlockedBy) != 0 {
		t.Fatalf("blocked-by = %+v", got.BlockedBy)
	}
	blocks, err := st.Blocks(ctx, key, blocker)
	if err != nil {
		t.Fatal(err)
	}
	if len(blocks) != 0 {
		t.Fatalf("blocks = %#v", blocks)
	}
}

func listRuns(t *testing.T, db string) []store.Run {
	t.Helper()
	runs, err := reopen(t, db).Runs(context.Background(), 10)
	if err != nil {
		t.Fatal(err)
	}
	return runs
}

func oneCall(t *testing.T, src *fake.Source, method string) fake.Call {
	t.Helper()
	var found []fake.Call
	for _, call := range src.Calls {
		if call.Method == method {
			found = append(found, call)
		}
	}
	if len(found) != 1 {
		t.Fatalf("%s calls = %#v", method, found)
	}
	return found[0]
}

func methodCount(src *fake.Source, method string) int {
	n := 0
	for _, call := range src.Calls {
		if call.Method == method {
			n++
		}
	}
	return n
}

func TestFileProject(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey)
	useSources(t, map[string]*fake.Source{"acme": src})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--source", bearKey, "--title", "T", "--project", "ABC")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "FAKE-1\n" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	call := oneCall(t, src, "Create")
	if len(call.Args) != 7 || call.Args[6] != "ABC" {
		t.Fatalf("Create args = %#v, want project ABC", call.Args)
	}
}

func TestFileProjectIgnoredForGitHub(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "github", name: "experimental", repo: "acme/widgets"})
	db := filepath.Join(t.TempDir(), "factory.db")
	gh := fakeWith(ghKey)
	useSources(t, map[string]*fake.Source{"experimental": gh})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("gh", "repo", "view", "acme/widgets", "--json", "visibility").
		WillSucceed(`{"visibility":"PRIVATE"}`, 0).Once().Build()
	useExec(t, mock)

	stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--source", ghKey, "--title", "T", "--project", "ABC")
	assertExit(t, code, stdout, stderr, ExitOK)
	want := "factory: warning: --project ABC is ignored for github:experimental, which has no projects\n"
	if stdout != "FAKE-1\n" || stderr != want {
		t.Fatalf("stdout = %q, stderr = %q, want %q", stdout, stderr, want)
	}
}

func TestFileWarnsOnLiteralBackslashN(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey)
	useSources(t, map[string]*fake.Source{"acme": src})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--source", bearKey, "--title", "T", "--body", `a\nb`)
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "FAKE-1\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if stderr != literalNewlineWarning+"\n" {
		t.Fatalf("stderr = %q", stderr)
	}
	call := oneCall(t, src, "Create")
	if len(call.Args) < 2 || call.Args[1] != `a\nb` {
		t.Fatalf("Create args = %#v, want the body unchanged", call.Args)
	}
}

func TestFileNoWarningWithRealNewline(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	src := fakeWith(bearKey)
	useSources(t, map[string]*fake.Source{"acme": src})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "file", "--source", bearKey, "--title", "T", "--body", "a\nb and a literal \\n")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stderr != "" {
		t.Fatalf("stderr = %q, want no warning", stderr)
	}
}
