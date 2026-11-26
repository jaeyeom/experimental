package forkkeeper

import (
	"context"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func assertTripwireHit(t *testing.T, hits []TripwireHit, pattern string) {
	t.Helper()
	re := regexp.MustCompile(pattern)
	for _, h := range hits {
		if re.MatchString(h.String()) {
			return
		}
	}
	t.Fatalf("expected tripwire matching %q, got %v", pattern, hits)
}

func assertTripwireQuiet(t *testing.T, hits []TripwireHit) {
	t.Helper()
	if len(hits) != 0 {
		t.Fatalf("expected no tripwires, got %v", hits)
	}
}

func TestTripwiresHarmless(t *testing.T) {
	fx := newTestFixture(t)
	u0 := fx.uref("refs/heads/main")
	fx.recordHead(u0)
	fx.fcommit("mine.txt", "my patch")
	fx.ucommit("README", "more docs")

	w, r, cand := fx.prepTripwireRepo()
	hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireQuiet(t, hits)
}

func TestTripwiresKnownHostQuiet(t *testing.T) {
	fx := newTestFixture(t)
	fx.ucommit("README", "see https://example.org/docs")
	fx.forkAtUpstream()
	fx.recordHead(fx.uref("refs/heads/main"))
	fx.ucommit("notes.txt", "mirror at https://example.org/other")

	w, r, cand := fx.prepTripwireRepo()
	hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireQuiet(t, hits)
}

func TestTripwiresNewHost(t *testing.T) {
	fx := newTestFixture(t)
	fx.recordHead(fx.uref("refs/heads/main"))
	fx.ucommit("lisp/x.el", `(url-retrieve "https://evil.example.net:8443/p")`)

	w, r, cand := fx.prepTripwireRepo()
	hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireHit(t, hits, `url.*evil\.example\.net`)
}

func TestTripwiresSCPHost(t *testing.T) {
	fx := newTestFixture(t)
	fx.recordHead(fx.uref("refs/heads/main"))
	fx.ucommit("notes.txt", "clone git@evil.example.net:x/y.git")

	w, r, cand := fx.prepTripwireRepo()
	hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireHit(t, hits, `url.*evil\.example\.net`)
}

func TestTripwiresBlob(t *testing.T) {
	fx := newTestFixture(t)
	fx.recordHead(fx.uref("refs/heads/main"))
	fx.ucommit("data.txt", strings.Repeat("QUJD", 30))

	w, r, cand := fx.prepTripwireRepo()
	hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireHit(t, hits, `blob.*data\.txt`)
}

func TestTripwiresBinary(t *testing.T) {
	fx := newTestFixture(t)
	fx.recordHead(fx.uref("refs/heads/main"))
	if err := os.WriteFile(filepath.Join(fx.useed, "tool.bin"), []byte{0, 1, 2}, 0o600); err != nil {
		t.Fatal(err)
	}
	fx.mustRun(fx.useed, "git", "add", "tool.bin")
	fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "bin")
	fx.mustRun(fx.useed, "git", "push", "-q", "origin", "HEAD:main")

	w, r, cand := fx.prepTripwireRepo()
	hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireHit(t, hits, `binary.*tool\.bin`)
}

func TestTripwiresCIOrInstall(t *testing.T) {
	files := []string{
		".github/workflows/ci.yml",
		"Makefile",
		"install.sh",
		"scripts/setup-env.sh",
		".githooks/pre-commit",
	}
	for _, f := range files {
		t.Run(f, func(t *testing.T) {
			fx := newTestFixture(t)
			fx.recordHead(fx.uref("refs/heads/main"))
			fx.ucommit(f, "echo hi")

			w, r, cand := fx.prepTripwireRepo()
			hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
			if err != nil {
				t.Fatal(err)
			}
			assertTripwireHit(t, hits, `ci-or-install.*`+regexp.QuoteMeta(f))
		})
	}
}

func TestTripwiresManifest(t *testing.T) {
	files := []string{
		"package.json",
		"go.mod",
		"Cargo.lock",
		"requirements-dev.txt",
		"herdr-pkg.el",
		"Eask",
	}
	for _, f := range files {
		t.Run(f, func(t *testing.T) {
			fx := newTestFixture(t)
			fx.recordHead(fx.uref("refs/heads/main"))
			fx.ucommit(f, "dep")

			w, r, cand := fx.prepTripwireRepo()
			hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
			if err != nil {
				t.Fatal(err)
			}
			assertTripwireHit(t, hits, `manifest.*`+regexp.QuoteMeta(f))
		})
	}
}

func TestTripwiresRangeFlagsAndTags(t *testing.T) {
	t.Run("first-run", func(t *testing.T) {
		fx := newTestFixture(t)
		fx.ucommit("README", "docs")

		w, r, cand := fx.prepTripwireRepo()
		hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
		if err != nil {
			t.Fatal(err)
		}
		assertTripwireHit(t, hits, `range.*first_run`)
	})

	t.Run("rewritten", func(t *testing.T) {
		fx := newTestFixture(t)
		fx.ucommit("README", "one")
		fx.forkAtUpstream()
		fx.recordHead(fx.uref("refs/heads/main"))
		fx.mustRun(fx.useed, "git", "reset", "-q", "--hard", "HEAD~1")
		fx.ucommit("README", "two")

		w, r, cand := fx.prepTripwireRepo()
		hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
		if err != nil {
			t.Fatal(err)
		}
		assertTripwireHit(t, hits, `range.*rewritten`)
	})

	t.Run("moved-tag", func(t *testing.T) {
		fx := newTestFixture(t)
		u0 := fx.uref("refs/heads/main")
		fx.recordHead(u0)
		fx.mustRun("", "git", "--git-dir="+fx.originGit, "update-ref", "refs/fork-keeper/upstream-tags/v1", u0)
		u1 := fx.ucommit("README", "docs")
		fx.mustRun("", "git", "--git-dir="+fx.upstreamGit, "update-ref", "refs/tags/v1", u1)

		w, r, cand := fx.prepTripwireRepo()
		hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
		if err != nil {
			t.Fatal(err)
		}
		assertTripwireHit(t, hits, `tags.*v1`)
	})
}

func TestTripwiresStack(t *testing.T) {
	fx := newTestFixture(t)
	fx.recordHead(fx.uref("refs/heads/main"))
	fx.fcommit("mine.txt", "my patch")
	fx.ucommit("README", "docs")

	w, r, _ := fx.prepTripwireRepo()
	gitDir := filepath.Join(w, ".git")

	// Changed user patch.
	f, err := os.OpenFile(filepath.Join(w, "mine.txt"), os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		t.Fatal(err)
	}
	_, _ = f.WriteString("sneaky\n")
	_ = f.Close()
	fx.mustRun(w, "git", "commit", "-q", "-a", "--amend", "--no-edit")
	amended := fx.mustRun(w, "git", "rev-parse", "HEAD")

	hits, err := CheckTripwires(context.Background(), gitDir, r, amended)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireHit(t, hits, `stack.*differ`)

	// Extra candidate commit.
	w2, r2, _ := fx.prepTripwireRepo()
	gitDir2 := filepath.Join(w2, ".git")
	if err := os.WriteFile(filepath.Join(w2, "extra.txt"), []byte("extra\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	fx.mustRun(w2, "git", "add", "extra.txt")
	fx.mustRun(w2, "git", "commit", "-q", "-m", "extra")
	extraCand := fx.mustRun(w2, "git", "rev-parse", "HEAD")

	hits, err = CheckTripwires(context.Background(), gitDir2, r2, extraCand)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireHit(t, hits, `stack.*differ`)

	// Candidate not based on upstream_new.
	hits, err = CheckTripwires(context.Background(), gitDir2, r2, r2.UpstreamOld)
	if err != nil {
		t.Fatal(err)
	}
	assertTripwireHit(t, hits, `stack.*not based`)
}

func TestTripwiresFileMode(t *testing.T) {
	t.Run("chmod-executable", func(t *testing.T) {
		fx := newTestFixture(t)
		fx.ucommit("helper.sh", "#!/bin/sh\necho hi")
		fx.forkAtUpstream()
		fx.recordHead(fx.uref("refs/heads/main"))

		if err := os.Chmod(filepath.Join(fx.useed, "helper.sh"), 0o755); err != nil {
			t.Fatal(err)
		}
		fx.mustRun(fx.useed, "git", "add", "helper.sh")
		fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "chmod +x")
		fx.mustRun(fx.useed, "git", "push", "-q", "origin", "HEAD:main")

		w, r, cand := fx.prepTripwireRepo()
		hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
		if err != nil {
			t.Fatal(err)
		}
		assertTripwireHit(t, hits, `mode.*helper\.sh \(100644 -> 100755\)`)
	})

	t.Run("new-executable", func(t *testing.T) {
		fx := newTestFixture(t)
		fx.recordHead(fx.uref("refs/heads/main"))

		//nolint:gosec // Test fixture intentionally creates an executable script.
		if err := os.WriteFile(filepath.Join(fx.useed, "run.sh"), []byte("#!/bin/sh\necho hi\n"), 0o755); err != nil {
			t.Fatal(err)
		}
		fx.mustRun(fx.useed, "git", "add", "run.sh")
		fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "add executable")
		fx.mustRun(fx.useed, "git", "push", "-q", "origin", "HEAD:main")

		w, r, cand := fx.prepTripwireRepo()
		hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
		if err != nil {
			t.Fatal(err)
		}
		assertTripwireHit(t, hits, `mode.*run\.sh \(000000 -> 100755\)`)
	})

	t.Run("new-symlink", func(t *testing.T) {
		fx := newTestFixture(t)
		fx.recordHead(fx.uref("refs/heads/main"))

		if err := os.Symlink("README", filepath.Join(fx.useed, "link")); err != nil {
			t.Fatal(err)
		}
		fx.mustRun(fx.useed, "git", "add", "link")
		fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "add symlink")
		fx.mustRun(fx.useed, "git", "push", "-q", "origin", "HEAD:main")

		w, r, cand := fx.prepTripwireRepo()
		hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
		if err != nil {
			t.Fatal(err)
		}
		assertTripwireHit(t, hits, `mode.*link \(000000 -> 120000\)`)
	})

	t.Run("delete-regular-quiet", func(t *testing.T) {
		fx := newTestFixture(t)
		fx.ucommit("old.txt", "bye")
		fx.forkAtUpstream()
		fx.recordHead(fx.uref("refs/heads/main"))

		fx.mustRun(fx.useed, "git", "rm", "-q", "old.txt")
		fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "remove old")
		fx.mustRun(fx.useed, "git", "push", "-q", "origin", "HEAD:main")

		w, r, cand := fx.prepTripwireRepo()
		hits, err := CheckTripwires(context.Background(), filepath.Join(w, ".git"), r, cand)
		if err != nil {
			t.Fatal(err)
		}
		assertTripwireQuiet(t, hits)
	})
}
