package scan

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

type tempWaiveRepo struct {
	t       *testing.T
	dir     string
	baseTip string
}

func newTempWaiveRepo(t *testing.T) *tempWaiveRepo {
	t.Helper()
	dir := t.TempDir()
	r := &tempWaiveRepo{t: t, dir: dir}
	r.git("init", "-q", "-b", "main")
	r.git("config", "user.email", "test@example.com")
	r.git("config", "user.name", "Test User")
	r.write("shared.txt", "line1\nline2\n")
	r.write("other.txt", "orig\n")
	r.git("add", ".")
	r.git("commit", "-q", "-m", "base0")
	r.baseTip = r.rev("HEAD")
	return r
}

func (r *tempWaiveRepo) git(args ...string) string {
	r.t.Helper()
	cmd := exec.Command("git", append([]string{"-C", r.dir}, args...)...) //nolint:gosec // test helper running fixed git binary
	out, err := cmd.CombinedOutput()
	if err != nil {
		r.t.Fatalf("git %v failed: %v\n%s", args, err, out)
	}
	return strings.TrimSpace(string(out))
}

func (r *tempWaiveRepo) write(name, body string) {
	r.t.Helper()
	path := filepath.Join(r.dir, name)
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		r.t.Fatal(err)
	}
}

func (r *tempWaiveRepo) rev(ref string) string {
	r.t.Helper()
	return r.git("rev-parse", ref)
}

func (r *tempWaiveRepo) cleanBaseMerge() (oldHead, newHead string) {
	r.t.Helper()
	r.git("checkout", "-q", "-b", "feat")
	r.write("feat.txt", "feature change\n")
	r.git("add", "feat.txt")
	r.git("commit", "-q", "-m", "feat work")
	oldHead = r.rev("HEAD")

	r.git("checkout", "-q", "main")
	r.write("other.txt", "updated on base\n")
	r.git("add", "other.txt")
	r.git("commit", "-q", "-m", "base advance")
	r.baseTip = r.rev("HEAD")

	r.git("checkout", "-q", "feat")
	r.git("merge", "-q", "--no-ff", "--no-edit", "main")
	newHead = r.rev("HEAD")
	return oldHead, newHead
}

func TestClassifyGitRepoFourScenarios(t *testing.T) {
	t.Parallel()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not found")
	}

	t.Run("clean base merge is base-only", func(t *testing.T) {
		t.Parallel()
		r := newTempWaiveRepo(t)
		oldHead, newHead := r.cleanBaseMerge()

		// A second clean base merge also remains base-only.
		r.git("checkout", "-q", "main")
		r.write("other2.txt", "more base work\n")
		r.git("add", "other2.txt")
		r.git("commit", "-q", "-m", "base advance 2")
		r.git("checkout", "-q", "feat")
		r.git("merge", "-q", "--no-ff", "--no-edit", "main")
		secondMerge := r.rev("HEAD")

		for _, head := range []string{newHead, secondMerge} {
			got, err := ClassifyGitRepo(context.Background(), r.dir, "main", oldHead, head)
			if err != nil {
				t.Fatalf("ClassifyGitRepo() error = %v", err)
			}
			if got != WaiveClassBaseOnly {
				t.Fatalf("class = %q, want %q", got, WaiveClassBaseOnly)
			}
		}
	})

	t.Run("base merge with conflict edits is author changes", func(t *testing.T) {
		t.Parallel()
		r := newTempWaiveRepo(t)
		r.git("checkout", "-q", "-b", "feat")
		r.write("shared.txt", "feat-line1\nline2\n")
		r.git("add", "shared.txt")
		r.git("commit", "-q", "-m", "feat edit shared")
		oldHead := r.rev("HEAD")

		r.git("checkout", "-q", "main")
		r.write("shared.txt", "base-line1\nline2\n")
		r.git("add", "shared.txt")
		r.git("commit", "-q", "-m", "base edit shared")

		r.git("checkout", "-q", "feat")
		cmd := exec.Command("git", "-C", r.dir, "merge", "--no-ff", "--no-commit", "main") //nolint:gosec // test helper
		_ = cmd.Run()
		r.write("shared.txt", "resolved-line1\nline2\n")
		r.git("add", "shared.txt")
		r.git("commit", "-q", "--no-edit")
		newHead := r.rev("HEAD")

		got, err := ClassifyGitRepo(context.Background(), r.dir, "main", oldHead, newHead)
		if err != nil {
			t.Fatalf("ClassifyGitRepo() error = %v", err)
		}
		if got != WaiveClassAuthorChanges {
			t.Fatalf("class = %q, want %q", got, WaiveClassAuthorChanges)
		}
	})

	t.Run("author commit after base merge is author changes", func(t *testing.T) {
		t.Parallel()
		r := newTempWaiveRepo(t)
		oldHead, _ := r.cleanBaseMerge()

		r.write("feat.txt", "feature change v2\n")
		r.git("add", "feat.txt")
		r.git("commit", "-q", "-m", "author follow-up")
		newHead := r.rev("HEAD")

		got, err := ClassifyGitRepo(context.Background(), r.dir, "main", oldHead, newHead)
		if err != nil {
			t.Fatalf("ClassifyGitRepo() error = %v", err)
		}
		if got != WaiveClassAuthorChanges {
			t.Fatalf("class = %q, want %q", got, WaiveClassAuthorChanges)
		}
	})

	t.Run("rebase or force-push onto new base is author changes", func(t *testing.T) {
		t.Parallel()
		r := newTempWaiveRepo(t)
		r.git("checkout", "-q", "-b", "feat")
		r.write("feat.txt", "feature change\n")
		r.git("add", "feat.txt")
		r.git("commit", "-q", "-m", "feat work")
		oldHead := r.rev("HEAD")

		r.git("checkout", "-q", "main")
		r.write("other.txt", "updated on base\n")
		r.git("add", "other.txt")
		r.git("commit", "-q", "-m", "base advance")

		r.git("checkout", "-q", "feat")
		r.git("rebase", "-q", "main")
		newHead := r.rev("HEAD")

		got, err := ClassifyGitRepo(context.Background(), r.dir, "main", oldHead, newHead)
		if err != nil {
			t.Fatalf("ClassifyGitRepo() error = %v", err)
		}
		if got != WaiveClassAuthorChanges {
			t.Fatalf("class = %q, want %q", got, WaiveClassAuthorChanges)
		}
	})

	t.Run("merge bringing in non-base branch is author changes", func(t *testing.T) {
		t.Parallel()
		r := newTempWaiveRepo(t)
		r.git("checkout", "-q", "-b", "feat")
		r.write("feat.txt", "feature change\n")
		r.git("add", "feat.txt")
		r.git("commit", "-q", "-m", "feat work")
		oldHead := r.rev("HEAD")

		r.git("checkout", "-q", "-b", "side", "main")
		r.write("side.txt", "side work\n")
		r.git("add", "side.txt")
		r.git("commit", "-q", "-m", "side work")

		r.git("checkout", "-q", "feat")
		r.git("merge", "-q", "--no-ff", "--no-edit", "side")
		newHead := r.rev("HEAD")

		got, err := ClassifyGitRepo(context.Background(), r.dir, "main", oldHead, newHead)
		if err != nil {
			t.Fatalf("ClassifyGitRepo() error = %v", err)
		}
		if got != WaiveClassAuthorChanges {
			t.Fatalf("class = %q, want %q", got, WaiveClassAuthorChanges)
		}
	})
}
