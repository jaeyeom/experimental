package forkkeeper

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestSplitCommand(t *testing.T) {
	got, err := SplitCommand(`my-reviewer --prompt "hello world" --tag='a b' --esc=foo\ bar`)
	if err != nil {
		t.Fatalf("SplitCommand: %v", err)
	}
	want := []string{"my-reviewer", "--prompt", "hello world", "--tag=a b", "--esc=foo bar"}
	if !slices.Equal(got, want) {
		t.Fatalf("got %v, want %v", got, want)
	}

	for _, bad := range []string{"", "   ", `foo "unclosed`, `foo 'unclosed`, `foo\`} {
		if _, err := SplitCommand(bad); err == nil {
			t.Errorf("SplitCommand(%q) expected error, got nil", bad)
		}
	}
}

func TestStandaloneReviewer(t *testing.T) {
	t.Run("clean-rebase", func(t *testing.T) {
		fx := newTestFixture(t)
		u0 := fx.uref("refs/heads/main")
		fx.recordHead(u0)
		fx.fcommit("mine.txt", "my patch")
		fx.ucommit("README", "upstream update")

		w := fx.fetchOnlyWorkdir()
		gitDir := filepath.Join(w, ".git")
		r, err := ComputeRange(context.Background(), gitDir, "upstream", "main")
		if err != nil {
			t.Fatal(err)
		}
		outDir := filepath.Join(fx.root, "out")
		if err := os.MkdirAll(outDir, 0o755); err != nil {
			t.Fatal(err)
		}

		res, err := (StandaloneReviewer{}).Review(context.Background(), ReviewInput{
			Fork:    Fork{Name: "demo", Repo: "me/demo", Branch: "main"},
			WorkDir: w,
			OutDir:  outDir,
			GitDir:  gitDir,
			Range:   r,
		})
		if err != nil {
			t.Fatalf("StandaloneReviewer.Review: %v", err)
		}
		if !IsValidSHA40(res.CandidateHead) {
			t.Fatalf("invalid candidate SHA: %q", res.CandidateHead)
		}

		v, ok, msg := ValidateVerdictFile(filepath.Join(outDir, "verdict.json"), res.CandidateHead, &r)
		if !ok {
			t.Fatalf("ValidateVerdictFile failed: %s", msg)
		}
		if v.Verdict != VerdictNeedsReview {
			t.Fatalf("verdict = %q, want %q", v.Verdict, VerdictNeedsReview)
		}
		report, err := os.ReadFile(filepath.Join(outDir, "report.md"))
		if err != nil {
			t.Fatalf("read report.md: %v", err)
		}
		if !strings.Contains(string(report), "upstream: README") {
			t.Errorf("report.md missing upstream commit:\n%s", report)
		}
	})

	t.Run("conflict-returns-error", func(t *testing.T) {
		fx := newTestFixture(t)
		fx.recordHead(fx.uref("refs/heads/main"))
		fx.fcommit("README", "fork line")
		fx.ucommit("README", "upstream line")

		w := fx.fetchOnlyWorkdir()
		gitDir := filepath.Join(w, ".git")
		r, err := ComputeRange(context.Background(), gitDir, "upstream", "main")
		if err != nil {
			t.Fatal(err)
		}
		outDir := filepath.Join(fx.root, "out")
		if err := os.MkdirAll(outDir, 0o755); err != nil {
			t.Fatal(err)
		}

		_, err = (StandaloneReviewer{}).Review(context.Background(), ReviewInput{
			Fork:    Fork{Name: "demo", Repo: "me/demo", Branch: "main"},
			WorkDir: w,
			OutDir:  outDir,
			GitDir:  gitDir,
			Range:   r,
		})
		if err == nil || !strings.Contains(err.Error(), "cherry-pick") {
			t.Fatalf("expected cherry-pick conflict error, got %v", err)
		}
	})
}

func TestCommandReviewerReadOnlyStdoutAndPlaceholders(t *testing.T) {
	fx := newTestFixture(t)
	fx.recordHead(fx.uref("refs/heads/main"))
	fx.fcommit("mine.txt", "my patch")
	fx.ucommit("README", "upstream update")

	w := fx.fetchOnlyWorkdir()
	gitDir := filepath.Join(w, ".git")
	r, err := ComputeRange(context.Background(), gitDir, "upstream", "main")
	if err != nil {
		t.Fatal(err)
	}

	outDir := filepath.Join(fx.root, "out")
	cacheDir := filepath.Join(fx.root, "cache")
	privDir := filepath.Join(fx.root, "priv")
	for _, d := range []string{outDir, cacheDir, privDir} {
		if err := os.MkdirAll(d, 0o755); err != nil {
			t.Fatal(err)
		}
	}
	rangeFile := filepath.Join(privDir, "range.json")
	rangeBytes, err := json.Marshal(r)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(rangeFile, rangeBytes, 0o600); err != nil {
		t.Fatal(err)
	}
	diffFile := filepath.Join(privDir, "upstream.diff")
	if err := os.WriteFile(diffFile, []byte("diff\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	schemaFile := filepath.Join(privDir, "verdict-schema.json")
	if err := os.WriteFile(schemaFile, []byte(VerdictOutputSchema), 0o600); err != nil {
		t.Fatal(err)
	}
	promptFile := filepath.Join(privDir, "prompt.md")

	// Read-only reviewer script: reads stdin prompt, inspects FORK_KEEPER_CANDIDATE_HEAD
	// and FORK_KEEPER_RANGE_FILE, checks placeholders, and emits verdict JSON on stdout.
	reviewerScript := filepath.Join(fx.root, "ro-reviewer.sh")
	script := `#!/bin/sh
set -eu
prompt=$(cat)
case "$prompt" in
  *"me/demo"*) ;;
  *) echo "missing repo in stdin prompt" >&2; exit 2 ;;
esac
test -n "$FORK_KEEPER_CANDIDATE_HEAD"
test -f "$FORK_KEEPER_RANGE_FILE"
test -f "$FORK_KEEPER_DIFF_FILE"
test -f "$FORK_KEEPER_SCHEMA_FILE"
test "$1" = "me/demo"
test "$2" = "main"
printf '{"schema":1,"verdict":"clean","candidate_head":"%s","range":%s,"conflicts":[],"findings":[],"summary":"stdout review ok"}\n' \
  "$FORK_KEEPER_CANDIDATE_HEAD" "$(cat "$FORK_KEEPER_RANGE_FILE")"
`
	//nolint:gosec // Test reviewer script must be executable.
	if err := os.WriteFile(reviewerScript, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}

	rev := &CommandReviewer{
		Command: []string{reviewerScript, "{repo}", "{branch}"},
	}
	res, err := rev.Review(context.Background(), ReviewInput{
		Fork:        Fork{Name: "demo", Repo: "me/demo", Branch: "main"},
		WorkDir:     w,
		OutDir:      outDir,
		GitDir:      gitDir,
		CacheDir:    cacheDir,
		LogPath:     filepath.Join(cacheDir, "agent.log"),
		SandboxMode: "none",
		Range:       r,
		RangeFile:   rangeFile,
		DiffFile:    diffFile,
		PromptFile:  promptFile,
		SchemaFile:  schemaFile,
	})
	if err != nil {
		t.Fatalf("CommandReviewer.Review: %v", err)
	}
	v, ok, msg := ValidateVerdictFile(filepath.Join(outDir, "verdict.json"), res.CandidateHead, &r)
	if !ok {
		t.Fatalf("ValidateVerdictFile failed: %s", msg)
	}
	if v.Verdict != VerdictClean || v.Summary != "stdout review ok" {
		t.Fatalf("unexpected verdict: %+v", v)
	}
	report, err := os.ReadFile(filepath.Join(outDir, "report.md"))
	if err != nil || strings.TrimSpace(string(report)) != "stdout review ok" {
		t.Fatalf("report.md = %q, err=%v", string(report), err)
	}
}
