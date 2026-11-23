package migrationimmutability

import (
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
)

// repo is a throwaway git repository with a fake origin: refs/remotes/origin/*
// are written directly, so no network or second clone is needed.
type repo struct {
	t   *testing.T
	dir string
}

func gitEnv() []string {
	return append(os.Environ(),
		"GIT_AUTHOR_NAME=t", "GIT_AUTHOR_EMAIL=t@example.com",
		"GIT_COMMITTER_NAME=t", "GIT_COMMITTER_EMAIL=t@example.com",
		"GIT_CONFIG_NOSYSTEM=1", "GIT_CONFIG_GLOBAL=/dev/null",
	)
}

func (r *repo) git(args ...string) string {
	r.t.Helper()
	cmd := exec.Command("git", append([]string{"-C", r.dir}, args...)...)
	cmd.Env = gitEnv()
	out, err := cmd.CombinedOutput()
	if err != nil {
		r.t.Fatalf("git %v: %v\n%s", args, err, out)
	}
	return strings.TrimSpace(string(out))
}

func (r *repo) write(path, content string) {
	r.t.Helper()
	full := filepath.Join(r.dir, path)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		r.t.Fatal(err)
	}
	if err := os.WriteFile(full, []byte(content), 0o600); err != nil {
		r.t.Fatal(err)
	}
}

func (r *repo) remove(path string) {
	r.t.Helper()
	if err := os.Remove(filepath.Join(r.dir, path)); err != nil {
		r.t.Fatal(err)
	}
}

func (r *repo) commit(msg string) {
	r.t.Helper()
	r.git("add", "-A")
	r.git("commit", "-q", "--allow-empty", "-m", msg)
}

const (
	djangoMig  = "app/migrations/0001_init.py"
	sqlMig     = "db/migrations/001_schema.sql"
	prismaMig  = "web/prisma/migrations/20240101_init/migration.sql"
	alembicMig = "svc/migrations/versions/abc123_init.py"
	initPy     = "app/migrations/__init__.py"
)

// newRepo returns a repo whose main branch holds one migration per built-in
// pattern, published as origin/main (and origin/HEAD), checked out on a
// feature branch "feat" with no upstream.
func newRepo(t *testing.T) *repo {
	t.Helper()
	r := &repo{t: t, dir: t.TempDir()}
	r.git("init", "-q", "-b", "main")
	r.git("config", "remote.origin.url", "file:///nonexistent")
	r.git("config", "remote.origin.fetch", "+refs/heads/*:refs/remotes/origin/*")
	for _, p := range []string{djangoMig, sqlMig, prismaMig, alembicMig, initPy} {
		r.write(p, "-- "+p+"\n")
	}
	r.write("README.md", "hi\n")
	r.commit("init")
	r.git("update-ref", "refs/remotes/origin/main", "HEAD")
	r.git("symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main")
	r.git("checkout", "-q", "-b", "feat")
	return r
}

func (r *repo) check(mutate func(*Config)) (*Report, error) {
	cfg := Config{
		Root:     r.dir,
		Patterns: DefaultPatterns(),
		Baseline: &baseline.File{Version: baseline.CurrentVersion, Entries: map[string]baseline.Entry{}},
		Getenv:   func(string) string { return "" },
	}
	if mutate != nil {
		mutate(&cfg)
	}
	return Check(cfg)
}

func (r *repo) mustCheck(mutate func(*Config)) *Report {
	r.t.Helper()
	rep, err := r.check(mutate)
	if err != nil {
		r.t.Fatalf("Check: %v", err)
	}
	return rep
}

func blockedSummary(rep *Report) []string {
	var out []string
	for _, f := range rep.Blocked() {
		s := f.Path + " " + f.Status
		if f.Symlink {
			s += " symlink"
		}
		out = append(out, s)
	}
	return out
}

func assertBlocked(t *testing.T, rep *Report, want ...string) {
	t.Helper()
	got := blockedSummary(rep)
	if len(want) == 0 {
		want = nil
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("blocked = %q, want %q", got, want)
	}
}

func TestParseNameStatus(t *testing.T) {
	out := "M\x00a b/migrations/0001_x.py\x00D\x00데이터/migrations/1.sql\x00"
	got, err := parseNameStatus(out)
	if err != nil {
		t.Fatal(err)
	}
	want := []change{{"M", "a b/migrations/0001_x.py"}, {"D", "데이터/migrations/1.sql"}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("parseNameStatus = %v, want %v", got, want)
	}
	if _, err := parseNameStatus("M\x00"); err == nil {
		t.Error("odd field count accepted")
	}
}

func TestCheckCleanBranch(t *testing.T) {
	r := newRepo(t)
	rep := r.mustCheck(nil)
	if rep.BaseRef != "origin/main" {
		t.Errorf("BaseRef = %q, want origin/main", rep.BaseRef)
	}
	if len(rep.Findings) != 0 {
		t.Errorf("Findings = %+v, want none", rep.Findings)
	}
}

func TestCheckChanges(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(r *repo)
		want   []string
	}{
		{"add new migration", func(r *repo) {
			r.write("app/migrations/0002_more.py", "x\n")
			r.commit("add")
		}, nil},
		{"modify committed", func(r *repo) {
			r.write(djangoMig, "changed\n")
			r.commit("edit")
		}, []string{djangoMig + " modified"}},
		{"modify uncommitted", func(r *repo) {
			r.write(sqlMig, "changed\n")
		}, []string{sqlMig + " modified"}},
		{"modify prisma and alembic", func(r *repo) {
			r.write(prismaMig, "changed\n")
			r.write(alembicMig, "changed\n")
		}, []string{alembicMig + " modified", prismaMig + " modified"}},
		{"delete", func(r *repo) {
			r.git("rm", "-q", sqlMig)
			r.commit("rm")
		}, []string{sqlMig + " deleted"}},
		{"rename", func(r *repo) {
			r.git("mv", djangoMig, "app/migrations/0002_init.py")
			r.commit("mv")
		}, []string{djangoMig + " deleted"}},
		{"replace with symlink", func(r *repo) {
			r.remove(sqlMig)
			if err := os.Symlink("../../README.md", filepath.Join(r.dir, sqlMig)); err != nil {
				r.t.Fatal(err)
			}
			r.commit("ln")
		}, []string{sqlMig + " type changed symlink"}},
		{"add symlink", func(r *repo) {
			if err := os.Symlink("../../README.md", filepath.Join(r.dir, "db/migrations/002.sql")); err != nil {
				r.t.Fatal(err)
			}
			r.commit("ln")
		}, []string{"db/migrations/002.sql added symlink"}},
		{"non-migration beside migrations", func(r *repo) {
			r.write(initPy, "changed\n")
			r.write("README.md", "changed\n")
		}, nil},
		{"edit migration added on branch", func(r *repo) {
			r.write("app/migrations/0002_more.py", "x\n")
			r.commit("add")
			r.write("app/migrations/0002_more.py", "y\n")
			r.commit("edit")
		}, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := newRepo(t)
			tt.mutate(r)
			assertBlocked(t, r.mustCheck(nil), tt.want...)
		})
	}
}

func TestCheckExclude(t *testing.T) {
	r := newRepo(t)
	r.write(sqlMig, "changed\n")
	rep := r.mustCheck(func(c *Config) { c.Excludes = []string{"db/**"} })
	assertBlocked(t, rep)
	if _, err := r.check(func(c *Config) { c.Excludes = []string{"[bad"} }); err == nil {
		t.Error("invalid exclude glob accepted")
	}
}

func TestRecordTransitionsThenLaterEditStillBlocked(t *testing.T) {
	r := newRepo(t)
	bf := &baseline.File{Version: baseline.CurrentVersion, Entries: map[string]baseline.Entry{}}
	withBF := func(c *Config) { c.Baseline = bf }

	r.write(djangoMig, "reformatted\n")
	r.remove(sqlMig)
	if err := os.Symlink("x", filepath.Join(r.dir, "db/migrations/009.sql")); err != nil {
		t.Fatal(err)
	}
	rep := r.mustCheck(withBF)
	if err := rep.RecordTransitions(bf, "", ""); err == nil {
		t.Error("RecordTransitions with empty reason succeeded")
	}
	if err := rep.RecordTransitions(bf, "formatter sweep", "#7"); err != nil {
		t.Fatal(err)
	}
	if _, ok := bf.Entries["db/migrations/009.sql"]; ok {
		t.Error("symlink finding recorded in baseline")
	}
	tr := bf.Entries[djangoMig].Transitions
	if len(tr) != 1 || tr[0].Reason != "formatter sweep" || tr[0].Issue != "#7" || tr[0].After == baseline.TransitionDeleted {
		t.Errorf("djangoMig transitions = %+v", tr)
	}
	if got := bf.Entries[sqlMig].Transitions; len(got) != 1 || got[0].After != baseline.TransitionDeleted {
		t.Errorf("sqlMig transitions = %+v", got)
	}

	rep = r.mustCheck(withBF)
	assertBlocked(t, rep, "db/migrations/009.sql added symlink")
	if n := len(rep.Findings); n != 3 {
		t.Errorf("findings = %d, want 3 (2 allowed + symlink)", n)
	}

	// A later, different edit to the same file is not covered.
	r.write(djangoMig, "reformatted again\n")
	assertBlocked(t, r.mustCheck(withBF), djangoMig+" modified", "db/migrations/009.sql added symlink")
}

func TestResolveBase(t *testing.T) {
	t.Run("explicit base", func(t *testing.T) {
		r := newRepo(t)
		r.write(djangoMig, "changed\n")
		r.commit("edit")
		r.git("branch", "other")
		rep := r.mustCheck(func(c *Config) { c.Base = "other" })
		if rep.BaseRef != "other" {
			t.Errorf("BaseRef = %q", rep.BaseRef)
		}
		assertBlocked(t, rep)
	})
	t.Run("GITHUB_BASE_REF", func(t *testing.T) {
		r := newRepo(t)
		r.git("update-ref", "refs/remotes/origin/release", "HEAD")
		rep := r.mustCheck(func(c *Config) {
			c.Getenv = func(k string) string {
				if k == "GITHUB_BASE_REF" {
					return "release"
				}
				return ""
			}
		})
		if rep.BaseRef != "origin/release" {
			t.Errorf("BaseRef = %q, want origin/release", rep.BaseRef)
		}
	})
	t.Run("upstream with different name", func(t *testing.T) {
		r := newRepo(t)
		r.git("update-ref", "refs/remotes/origin/release", "HEAD")
		r.git("branch", "-q", "--set-upstream-to=origin/release")
		if rep := r.mustCheck(nil); rep.BaseRef != "origin/release" {
			t.Errorf("BaseRef = %q, want origin/release", rep.BaseRef)
		}
	})
	t.Run("same-name upstream skipped", func(t *testing.T) {
		r := newRepo(t)
		r.write("app/migrations/0002_more.py", "x\n")
		r.commit("add")
		r.git("update-ref", "refs/remotes/origin/feat", "HEAD")
		r.git("branch", "-q", "--set-upstream-to=origin/feat")
		r.write("app/migrations/0002_more.py", "y\n")
		rep := r.mustCheck(nil)
		if rep.BaseRef != "origin/main" {
			t.Errorf("BaseRef = %q, want origin/main", rep.BaseRef)
		}
		assertBlocked(t, rep)
	})
	t.Run("on main diffs unpushed", func(t *testing.T) {
		r := newRepo(t)
		r.git("checkout", "-q", "main")
		r.git("branch", "-q", "--set-upstream-to=origin/main")
		r.write(djangoMig, "changed\n")
		rep := r.mustCheck(nil)
		if rep.BaseRef != "origin/main" {
			t.Errorf("BaseRef = %q", rep.BaseRef)
		}
		assertBlocked(t, rep, djangoMig+" modified")
	})
	t.Run("detached HEAD uses origin/HEAD", func(t *testing.T) {
		r := newRepo(t)
		r.git("checkout", "-q", "--detach")
		if rep := r.mustCheck(nil); rep.BaseRef != "origin/main" {
			t.Errorf("BaseRef = %q", rep.BaseRef)
		}
	})
}

func TestResolveBaseErrors(t *testing.T) {
	t.Run("no base available", func(t *testing.T) {
		r := newRepo(t)
		r.git("symbolic-ref", "--delete", "refs/remotes/origin/HEAD")
		_, err := r.check(nil)
		if err == nil || !strings.Contains(err.Error(), "--base") {
			t.Errorf("err = %v, want hint about --base", err)
		}
	})
	t.Run("missing explicit base", func(t *testing.T) {
		r := newRepo(t)
		_, err := r.check(func(c *Config) { c.Base = "origin/nope" })
		if err == nil || !strings.Contains(err.Error(), "origin/nope") {
			t.Errorf("err = %v", err)
		}
	})
	t.Run("no shared history", func(t *testing.T) {
		r := newRepo(t)
		r.git("checkout", "-q", "--orphan", "lonely")
		r.commit("orphan")
		_, err := r.check(func(c *Config) { c.Base = "main" })
		if err == nil || !strings.Contains(err.Error(), "no history") {
			t.Errorf("err = %v, want no shared history", err)
		}
	})
}

func TestResolveBaseShallowHint(t *testing.T) {
	src := newRepo(t)
	src.git("checkout", "-q", "main")
	src.git("commit", "-q", "--allow-empty", "-m", "main 2")
	src.git("checkout", "-q", "feat")
	src.git("commit", "-q", "--allow-empty", "-m", "feat 2")

	dir := t.TempDir()
	cmd := exec.Command("git", "clone", "-q", "--depth", "1", "--no-single-branch", "-b", "feat", "file://"+src.dir, dir)
	cmd.Env = gitEnv()
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("clone: %v\n%s", err, out)
	}
	r := &repo{t: t, dir: dir}
	_, err := r.check(func(c *Config) { c.Base = "origin/main" })
	if err == nil || !strings.Contains(err.Error(), "shallow") || !strings.Contains(err.Error(), "fetch-depth: 0") {
		t.Errorf("err = %v, want shallow hint", err)
	}
}

func TestCheckVacuous(t *testing.T) {
	r := newRepo(t)
	_, err := r.check(func(c *Config) { c.Excludes = []string{"**"} })
	if err == nil || !strings.Contains(err.Error(), "vacuously") {
		t.Errorf("err = %v, want vacuous error", err)
	}
	_, err = r.check(func(c *Config) { c.Patterns = nil })
	if err == nil || !strings.Contains(err.Error(), "vacuously") {
		t.Errorf("no patterns: err = %v, want vacuous error", err)
	}
}

func TestCheckFromSubdirectory(t *testing.T) {
	r := newRepo(t)
	r.write(sqlMig, "changed\n")
	rep := r.mustCheck(func(c *Config) { c.Root = filepath.Join(r.dir, "app") })
	assertBlocked(t, rep, sqlMig+" modified")
	want, err := filepath.EvalSymlinks(r.dir)
	if err != nil {
		t.Fatal(err)
	}
	if got, _ := filepath.EvalSymlinks(rep.TopLevel); got != want {
		t.Errorf("TopLevel = %q, want %q", rep.TopLevel, want)
	}
}
