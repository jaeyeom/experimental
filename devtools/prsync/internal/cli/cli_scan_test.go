package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/ghlimit"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/loop"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestScanBadRepo(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--repo", "not-a-repo"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "invalid repo") {
		t.Fatalf("stderr = %q", stderr.String())
	}
	if stdout.Len() != 0 {
		t.Fatalf("stdout = %q, want empty", stdout.String())
	}
}

func TestScanGHMissing(t *testing.T) {
	t.Parallel()

	cfgPath := writeScanConfig(t, "gh_bin=prsync-test-missing-gh\nherdr_bin=herdr\nauthor=alice\nrepos=acme/widgets\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	if !strings.Contains(stderr.String(), "gh binary not found") {
		t.Fatalf("stderr = %q", stderr.String())
	}
	if stdout.Len() != 0 {
		t.Fatalf("stdout = %q, want empty before work starts", stdout.String())
	}
}

func TestScanFixtureHappyPath(t *testing.T) {
	t.Parallel()

	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath, "--json"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}

	var doc scan.Document
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if doc.Author != "alice" {
		t.Fatalf("author = %q, want alice", doc.Author)
	}
	if len(doc.PRs) != 1 || doc.PRs[0].Number != 123 {
		t.Fatalf("prs = %+v", doc.PRs)
	}
	if doc.PRs[0].Bucket != "needs_you" {
		t.Fatalf("bucket = %q, want needs_you", doc.PRs[0].Bucket)
	}
	if doc.PRs[0].HeadSHA != "abc123def456" {
		t.Fatalf("head_sha = %q, want abc123def456", doc.PRs[0].HeadSHA)
	}
	if doc.PRs[0].MergeStateStatus != "CLEAN" {
		t.Fatalf("merge_state_status = %q, want CLEAN", doc.PRs[0].MergeStateStatus)
	}
	if doc.PRs[0].State != "open" {
		t.Fatalf("state = %q, want open", doc.PRs[0].State)
	}
	if doc.PRs[0].Mergeable != "MERGEABLE" {
		t.Fatalf("mergeable = %q, want MERGEABLE", doc.PRs[0].Mergeable)
	}
	if doc.PRs[0].Tab == nil || doc.PRs[0].Tab.TabID != "w2:tC" {
		t.Fatalf("tab = %+v, want w2:tC", doc.PRs[0].Tab)
	}
	if doc.PRs[0].Tab.PaneID == nil || *doc.PRs[0].Tab.PaneID != "w2:pC" {
		t.Fatalf("pane_id = %v, want w2:pC", doc.PRs[0].Tab.PaneID)
	}
	want := scan.Summary{Total: 1, TabHere: 1}
	if doc.Summary != want {
		t.Fatalf("summary = %+v, want %+v", doc.Summary, want)
	}
	if strings.Contains(stdout.String(), "team_review_suggestions") {
		t.Fatalf("stdout = %s, team_cover off must not add team_review_suggestions", stdout.String())
	}
}

func TestScanTeamCoverSuggests(t *testing.T) {
	t.Parallel()

	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, ""+
		"gh_bin="+ghBin+"\n"+
		"herdr_bin="+herdrBin+"\n"+
		"author=alice\n"+
		"repos=acme/widgets\n"+
		"team_cover=true\n"+
		"team_cover_exclude_logins=left\n"+
		"team_cover_signals=codeowners,history,load\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.Document
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if doc.Summary.UncoveredTeams == nil || *doc.Summary.UncoveredTeams != 1 {
		t.Fatalf("uncovered = %v", doc.Summary.UncoveredTeams)
	}
	if len(doc.PRs) != 1 || len(doc.PRs[0].TeamReviewSuggestions) != 1 {
		t.Fatalf("suggestions = %+v", doc.PRs[0].TeamReviewSuggestions)
	}
	got := doc.PRs[0].TeamReviewSuggestions[0]
	if got.Team != "acme/platform-reviewers" || got.CoveredBy != nil || got.Error != "" {
		t.Fatalf("team = %+v", got)
	}
	if len(got.Suggest) != 1 || got.Suggest[0].Login != "carol" {
		t.Fatalf("suggest = %+v, want carol", got.Suggest)
	}
	reasons := strings.Join(got.Suggest[0].Reasons, ", ")
	if !strings.Contains(reasons, "codeowner: /src/foo/special.go") || !strings.Contains(reasons, "5 open direct requests") {
		t.Fatalf("reasons = %#v", got.Suggest[0].Reasons)
	}
	hint := stderr.String()
	if strings.Contains(hint, "unavailable") {
		t.Fatalf("stderr = %q, want base head and batched history to load", hint)
	}
	if !strings.Contains(hint, "acme/widgets#123 acme/platform-reviewers: suggest carol") {
		t.Fatalf("stderr = %q", hint)
	}
	if !strings.Contains(hint, "gh pr edit 123 --repo acme/widgets --add-reviewer carol") {
		t.Fatalf("stderr = %q", hint)
	}
	if !regexp.MustCompile(`(?m)^team-cover: paths \d+/\d+ need history \(\d+ hits\); team members \d+ hits/\d+ calls; files \d+/\d+; reviews \d+/\d+; refresh \d+; calls \d+ \(cap 40\)$`).MatchString(hint) {
		t.Fatalf("stderr = %q, want the team-cover summary line", hint)
	}
	if strings.Contains(stdout.String(), "need history") {
		t.Fatalf("stdout = %s, want the summary only on stderr", stdout.String())
	}
}

func TestTeamCoverHintsEndWithNotes(t *testing.T) {
	t.Parallel()

	doc := scan.Document{TeamCoverNotes: []string{
		"team-cover: paths 300/359 need history (0 hits); team members 0 hits/1 calls; files 0/1; reviews 0/1; refresh 0; calls 40 (cap 40)",
		"team-cover: call cap 40 reached during history; 120 paths deferred",
	}}
	var out bytes.Buffer
	if err := writeTeamCoverHints(&out, doc); err != nil {
		t.Fatal(err)
	}
	want := "" +
		"team-cover: paths 300/359 need history (0 hits); team members 0 hits/1 calls; files 0/1; reviews 0/1; refresh 0; calls 40 (cap 40)\n" +
		"team-cover: call cap 40 reached during history; 120 paths deferred\n"
	if out.String() != want {
		t.Fatalf("hints =\n%s\nwant\n%s", out.String(), want)
	}
}

func TestTeamCoverHintsNameUncoveredReason(t *testing.T) {
	t.Parallel()

	bob := "bob"
	doc := scan.Document{PRs: []scan.PR{{
		Repo:   "acme/widgets",
		Number: 7,
		TeamReviewSuggestions: []scan.TeamReviewSuggestion{
			{Team: "acme/covered", CoveredBy: &bob, Suggest: []scan.SuggestedReviewer{}},
			{Team: "acme/gone", Suggest: []scan.SuggestedReviewer{}, UncoveredReason: "all 2 members excluded (1 author, 1 inactive)"},
			{Team: "acme/missing", Suggest: []scan.SuggestedReviewer{}, Error: "cannot list members of acme/missing: HTTP 403"},
			{Team: "acme/later", Suggest: []scan.SuggestedReviewer{}, UncoveredReason: "skipped: call cap"},
			{Team: "acme/live", Suggest: []scan.SuggestedReviewer{{Login: "carol", Reasons: []string{"history partial (1 of 5 paths deferred)"}}}},
		},
	}}}
	var out bytes.Buffer
	if err := writeTeamCoverHints(&out, doc); err != nil {
		t.Fatal(err)
	}
	want := "" +
		"acme/widgets#7 acme/covered: covered by bob\n" +
		"acme/widgets#7 acme/gone: uncovered: all 2 members excluded (1 author, 1 inactive)\n" +
		"acme/widgets#7 acme/missing: uncovered: cannot list members of acme/missing: HTTP 403\n" +
		"acme/widgets#7 acme/later: uncovered: skipped: call cap\n" +
		"acme/widgets#7 acme/live: suggest carol (history partial (1 of 5 paths deferred))\n" +
		"gh pr edit 7 --repo acme/widgets --add-reviewer carol\n"
	if out.String() != want {
		t.Fatalf("hints =\n%s\nwant\n%s", out.String(), want)
	}
}

func TestScanHerdrMissingDegrades(t *testing.T) {
	t.Parallel()

	ghBin, _ := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin=prsync-test-missing-herdr\nauthor=alice\nrepos=acme/widgets\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.Document
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.PRs) != 1 || doc.PRs[0].Tab != nil {
		t.Fatalf("tab = %+v, want nil", doc.PRs[0].Tab)
	}
	if len(doc.Warnings) == 0 || !strings.Contains(doc.Warnings[0], "herdr unreachable") {
		t.Fatalf("warnings = %v", doc.Warnings)
	}
	want := scan.Summary{Total: 1, OffMachine: 1}
	if doc.Summary != want {
		t.Fatalf("summary = %+v, want %+v", doc.Summary, want)
	}
}

// TestScanSecondRoundSkipsUnchangedRollupView runs two rounds against one
// state_file. The pull request's head and updatedAt do not change, and its
// checks are settled, so the second round reuses the stored rollup.
func TestScanSecondRoundSkipsUnchangedRollupView(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	viewLog := filepath.Join(t.TempDir(), "view.log")
	t.Setenv("GH_FAKE_VIEW_LOG", viewLog)
	stateDir := t.TempDir()
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n"+
		"state_file="+filepath.Join(stateDir, "state.json")+"\n")
	views := func() int {
		data, err := os.ReadFile(viewLog)
		if err != nil {
			return 0
		}
		return strings.Count(string(data), "statusCheckRollup")
	}
	for round, want := range []int{1, 1} {
		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
		if code != ExitOK {
			t.Fatalf("round %d: exit = %d, stderr=%q", round, code, stderr.String())
		}
		var doc scan.Document
		if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
			t.Fatalf("round %d: json: %v", round, err)
		}
		if len(doc.PRs) != 1 || doc.PRs[0].CIState != "green" {
			t.Fatalf("round %d: prs = %+v, want one green PR", round, doc.PRs)
		}
		if got := views(); got != want {
			t.Fatalf("round %d: rollup views = %d, want %d", round, got, want)
		}
	}
	if _, err := os.Stat(filepath.Join(stateDir, roundCacheFile)); err != nil {
		t.Fatalf("round cache: %v", err)
	}
}

func writeScanConfig(t *testing.T, body string) string {
	t.Helper()
	if !strings.Contains(body, "state_file=") {
		body += "state_file=" + filepath.Join(t.TempDir(), "state.json") + "\n"
	}
	dir := t.TempDir()
	path := filepath.Join(dir, "prsync.config")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func fixtureBins(t *testing.T) (ghBin, herdrBin string) {
	t.Helper()
	ghBin, err := filepath.Abs(filepath.Join("testdata", "fixtures", "gh-fake.sh"))
	if err != nil {
		t.Fatal(err)
	}
	herdrBin, err = filepath.Abs(filepath.Join("testdata", "fixtures", "herdr-fake.sh"))
	if err != nil {
		t.Fatal(err)
	}
	return ghBin, herdrBin
}

func TestScanAuthStatusClassificationAndCallCount(t *testing.T) {
	t.Parallel()

	_, herdrBin := fixtureBins(t)
	cases := []struct {
		name           string
		authStderr     string
		authExit       int
		wantCode       int
		wantStderr     string
		forbidInStderr string
	}{
		{
			name:       "not logged in",
			authStderr: "You are not logged into any GitHub hosts. To log in, run: gh auth login",
			authExit:   1,
			wantCode:   ExitPrecondition,
			wantStderr: "gh is not authenticated; run: gh auth login",
		},
		{
			name:       "invalid token",
			authStderr: "github.com\n  X Failed to log in to github.com account alice (keyring)\n  - The token in keyring is invalid.\n  - To re-authenticate, run: gh auth login -h github.com",
			authExit:   1,
			wantCode:   ExitPrecondition,
			wantStderr: "gh is not authenticated; run: gh auth login",
		},
		{
			name:           "direct secondary rate limit",
			authStderr:     "HTTP 403: You have exceeded a secondary rate limit. Please wait a few minutes before you try again.",
			authExit:       1,
			wantCode:       ExitRateLimited,
			wantStderr:     "GitHub rate limit; retry later",
			forbidInStderr: "gh auth login",
		},
		{
			name:           "abuse detection",
			authStderr:     "You have triggered an abuse detection mechanism",
			authExit:       1,
			wantCode:       ExitRateLimited,
			wantStderr:     "GitHub rate limit; retry later",
			forbidInStderr: "gh auth login",
		},
		{
			name:           "http 429",
			authStderr:     "HTTP/2.0 429 Too Many Requests",
			authExit:       1,
			wantCode:       ExitRateLimited,
			wantStderr:     "GitHub rate limit; retry later",
			forbidInStderr: "gh auth login",
		},
		{
			name:           "gh-throttle cooldown exit 75",
			authStderr:     fmt.Sprintf("gh-throttle: rate limit cooldown active (30s remaining; not an auth failure) [%s]", ghlimit.RateLimitMarker),
			authExit:       ghlimit.ExitRateLimited,
			wantCode:       ExitRateLimited,
			wantStderr:     "GitHub rate limit; retry later",
			forbidInStderr: "gh auth login",
		},
		{
			name:           "network failure",
			authStderr:     "X github.com: error connecting to api.github.com\n  check your internet connection or https://githubstatus.com",
			authExit:       1,
			wantCode:       ExitTransient,
			wantStderr:     "error connecting to api.github.com",
			forbidInStderr: "gh auth login",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			dir := t.TempDir()
			authLog := filepath.Join(dir, "auth-calls.log")
			stderrFile := filepath.Join(dir, "auth-stderr.txt")
			if err := os.WriteFile(stderrFile, []byte(tc.authStderr+"\n"), 0o600); err != nil {
				t.Fatal(err)
			}
			ghScript := filepath.Join(dir, "gh-auth-fail.sh")
			script := fmt.Sprintf(`#!/bin/sh
if [ "${1-}" = "auth" ] && [ "${2-}" = "status" ]; then
  printf 'auth status\n' >> %q
  cat %q >&2
  exit %d
fi
printf 'unexpected gh call: %%s\n' "$*" >&2
exit 99
`, authLog, stderrFile, tc.authExit)
			if err := os.WriteFile(ghScript, []byte(script), 0o700); err != nil {
				t.Fatal(err)
			}

			cfgPath := writeScanConfig(t, "gh_bin="+ghScript+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")
			var stdout, stderr bytes.Buffer
			code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
			if code != tc.wantCode {
				t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, tc.wantCode, stderr.String(), stdout.String())
			}
			if !strings.Contains(stderr.String(), tc.wantStderr) {
				t.Fatalf("stderr = %q, want substring %q", stderr.String(), tc.wantStderr)
			}
			if tc.forbidInStderr != "" && strings.Contains(stderr.String(), tc.forbidInStderr) {
				t.Fatalf("stderr = %q, must not contain %q", stderr.String(), tc.forbidInStderr)
			}
			if stdout.Len() != 0 {
				t.Fatalf("stdout = %q, want empty before work starts", stdout.String())
			}
			rawLog, err := os.ReadFile(authLog)
			if err != nil {
				t.Fatalf("read auth log: %v", err)
			}
			if calls := strings.Count(string(rawLog), "auth status\n"); calls != 1 {
				t.Fatalf("auth status calls = %d, want 1", calls)
			}
		})
	}
}

func TestOneAuthStatusCallPerRun(t *testing.T) {
	t.Parallel()

	realGH, herdrBin := fixtureBins(t)

	newCountingGH := func(t *testing.T, authLog string) string {
		t.Helper()
		wrapper := filepath.Join(t.TempDir(), "gh-wrap.sh")
		script := fmt.Sprintf(`#!/bin/sh
if [ "${1-}" = "auth" ] && [ "${2-}" = "status" ]; then
  printf 'auth status\n' >> %q
fi
exec %q "$@"
`, authLog, realGH)
		if err := os.WriteFile(wrapper, []byte(script), 0o700); err != nil {
			t.Fatal(err)
		}
		return wrapper
	}

	t.Run("scan runs auth status once", func(t *testing.T) {
		t.Parallel()
		authLog := filepath.Join(t.TempDir(), "auth.log")
		cfgPath := writeScanConfig(t, "gh_bin="+newCountingGH(t, authLog)+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")
		var stdout, stderr bytes.Buffer
		if code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor()); code != ExitOK {
			t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
		}
		raw, err := os.ReadFile(authLog)
		if err != nil {
			t.Fatal(err)
		}
		if calls := strings.Count(string(raw), "auth status\n"); calls != 1 {
			t.Fatalf("auth status calls = %d, want 1", calls)
		}
	})

	t.Run("dispatch with multiple targets runs auth status once", func(t *testing.T) {
		t.Parallel()
		authLog := filepath.Join(t.TempDir(), "auth.log")
		cfgPath := writeScanConfig(t, "gh_bin="+newCountingGH(t, authLog)+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")
		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{
			"dispatch", "--config", cfgPath, "--pr", "acme/widgets#123", "--pr", "acme/widgets#124",
		}, &stdout, &stderr, executor.NewBasicExecutor())
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
		}
		raw, err := os.ReadFile(authLog)
		if err != nil {
			t.Fatal(err)
		}
		if calls := strings.Count(string(raw), "auth status\n"); calls != 1 {
			t.Fatalf("auth status calls = %d, want 1", calls)
		}
	})

	t.Run("loop runs auth status once per round and surfaces rate limit", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		authLog := filepath.Join(dir, "auth.log")
		marker := filepath.Join(dir, "rate-limited-once")
		ghScript := filepath.Join(dir, "gh-loop.sh")
		script := fmt.Sprintf(`#!/bin/sh
if [ "${1-}" = "auth" ] && [ "${2-}" = "status" ]; then
  printf 'auth status\n' >> %q
  if [ ! -f %q ]; then
    touch %q
    printf 'gh-throttle: rate limit cooldown active (5s remaining; not an auth failure) [%s]\n' >&2
    exit %d
  fi
  exit 0
fi
exec %q "$@"
`, authLog, marker, marker, ghlimit.RateLimitMarker, ghlimit.ExitRateLimited, realGH)
		if err := os.WriteFile(ghScript, []byte(script), 0o700); err != nil {
			t.Fatal(err)
		}
		cfgPath := writeScanConfig(t, "gh_bin="+ghScript+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n")
		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{"loop", "--config", cfgPath, "--max-rounds", "2", "--dry-run"}, &stdout, &stderr, executor.NewBasicExecutor())
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
		}
		lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
		if len(lines) != 2 {
			t.Fatalf("round lines = %d, want 2:\n%s", len(lines), stdout.String())
		}
		var r1, r2 loop.RoundResult
		if err := json.Unmarshal([]byte(lines[0]), &r1); err != nil {
			t.Fatalf("decode round 1: %v", err)
		}
		if err := json.Unmarshal([]byte(lines[1]), &r2); err != nil {
			t.Fatalf("decode round 2: %v", err)
		}
		if !strings.Contains(r1.Err, "GitHub rate limit; retry later") || strings.Contains(r1.Err, "gh auth login") {
			t.Fatalf("round 1 err = %q, want GitHub rate limit; retry later", r1.Err)
		}
		if r2.Err != "" || r2.Scanned != 1 {
			t.Fatalf("round 2 = %+v, want clean scan", r2)
		}
		raw, err := os.ReadFile(authLog)
		if err != nil {
			t.Fatal(err)
		}
		if calls := strings.Count(string(raw), "auth status\n"); calls != 2 {
			t.Fatalf("auth status calls = %d, want 2 (1 per round)", calls)
		}
	})
}
