package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

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
