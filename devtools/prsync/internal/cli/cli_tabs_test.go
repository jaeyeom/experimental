package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestTabsRequiresOrphans(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "requires --orphans") {
		t.Fatalf("stderr = %q", stderr.String())
	}
	if stdout.Len() != 0 {
		t.Fatalf("stdout = %q, want empty", stdout.String())
	}
}

func TestTabsOrphansFixtureMerged(t *testing.T) {
	t.Parallel()

	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--config", cfgPath, "--json"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}

	var doc scan.OrphanDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if doc.Author != "alice" {
		t.Fatalf("author = %q, want alice", doc.Author)
	}
	if len(doc.OrphanTabs) != 1 {
		t.Fatalf("orphan_tabs = %+v, want 1", doc.OrphanTabs)
	}
	got := doc.OrphanTabs[0]
	if got.TabID != "w2:tC" || got.Ticket != "PROJ-123" || got.Bucket != scan.BucketMerged {
		t.Fatalf("orphan = %+v", got)
	}
	if got.PR == nil || got.PR.Number != 777 || got.PR.State != "merged" {
		t.Fatalf("pr = %+v", got.PR)
	}
	if got.PR.MergedAt == nil || *got.PR.MergedAt != "2026-08-18T22:19:28Z" {
		t.Fatalf("merged_at = %v", got.PR.MergedAt)
	}
}

func TestTabsOrphansEmptySearchIsNoPR(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("GH_FAKE_SEARCH_EMPTY", "1")
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.OrphanDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.OrphanTabs) != 1 || doc.OrphanTabs[0].Bucket != scan.BucketNoPR {
		t.Fatalf("orphan_tabs = %+v, want 1 no_pr", doc.OrphanTabs)
	}
}

func TestTabsOrphansStdinReuseSkipsMatchedTab(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
	restore := swapStdin(t, string(mustScanJSON(t, stdinEligibleDoc())))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--stdin", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.OrphanDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty (tab already has an open PR in the scan)", doc.OrphanTabs)
	}
}

func TestTabsOrphansStdinDraftIsNotOrphan(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	// Without the scan row, empty search would classify this tab as no_pr.
	t.Setenv("GH_FAKE_SEARCH_EMPTY", "1")
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
	doc := stdinEligibleDoc()
	doc.PRs[0].IsDraft = true
	restore := swapStdin(t, string(mustScanJSON(t, doc)))
	defer restore()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--stdin", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var got scan.OrphanDocument
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(got.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want empty (open draft PR is not an orphan)", got.OrphanTabs)
	}
}

func TestTabsGoRequiresCloseMerged(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "--go requires --close-merged") {
		t.Fatalf("stderr = %q", stderr.String())
	}
	if stdout.Len() != 0 {
		t.Fatalf("stdout = %q, want empty", stdout.String())
	}
}

func TestTabsCloseMergedDryRun(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "close")
	jiraLog := filepath.Join(t.TempDir(), "jira.log")
	t.Setenv("HERDR_FAKE_TAB_CLOSE_SENTINEL", sentinel)
	t.Setenv("JIRA_FAKE_LOG", jiraLog)
	t.Setenv("JIRA_FAKE_FAIL", "")
	t.Setenv("JIRA_FAKE_STRICT", "")
	cfgPath := closeMergedConfig(t, ghBin, herdrBin)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if !doc.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if len(doc.Results) != 1 {
		t.Fatalf("results = %+v, want 1", doc.Results)
	}
	got := doc.Results[0]
	if got.TabID != "w2:tC" || got.Ticket != "PROJ-123" || got.Action != scan.ActionWouldClose {
		t.Fatalf("result = %+v", got)
	}
	if _, err := os.Stat(sentinel); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("herdr tab close was invoked on dry-run: %v", err)
	}
	if doc.Unblock.Status != scan.UnblockNotRun || doc.Unblock.Text != "" || len(doc.Unblock.Lines) != 0 {
		t.Fatalf("unblock = %+v, want not_run", doc.Unblock)
	}
	if _, err := os.Stat(jiraLog); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("jira was invoked on dry-run: %v", err)
	}
}

func TestTabsCloseMergedGoCloses(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "close")
	t.Setenv("HERDR_FAKE_TAB_CLOSE_SENTINEL", sentinel)
	t.Setenv("JIRA_FAKE_FAIL", "")
	t.Setenv("JIRA_FAKE_STRICT", "")
	cfgPath := closeMergedConfig(t, ghBin, herdrBin)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--go", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if doc.DryRun {
		t.Fatal("dry_run = true, want false")
	}
	if len(doc.Results) != 1 || doc.Results[0].Action != scan.ActionClosed || doc.Results[0].TabID != "w2:tC" {
		t.Fatalf("results = %+v, want closed w2:tC", doc.Results)
	}
	body, err := os.ReadFile(sentinel)
	if err != nil {
		t.Fatalf("herdr tab close was not invoked: %v", err)
	}
	if !strings.Contains(string(body), "w2:tC") {
		t.Fatalf("close args = %q, want w2:tC", body)
	}
	if doc.Unblock.Status != scan.UnblockOK || doc.Unblock.Text != "nothing newly unblocked" || len(doc.Unblock.Lines) != 0 {
		t.Fatalf("unblock = %+v, want nothing newly unblocked", doc.Unblock)
	}
}

func TestTabsCloseMergedGoSkippedNotFound(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("HERDR_FAKE_TAB_CLOSE", "not_found")
	cfgPath := closeMergedConfig(t, ghBin, herdrBin)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--go", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.Results) != 1 || doc.Results[0].Action != scan.ActionSkippedNotFound {
		t.Fatalf("results = %+v, want skipped_not_found", doc.Results)
	}
	if doc.Unblock.Status != scan.UnblockNotRun || len(doc.Unblock.Lines) != 0 {
		t.Fatalf("unblock = %+v, want not_run", doc.Unblock)
	}
}

func TestTabsCloseMergedAllNoPRRefuses(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("GH_FAKE_SEARCH_EMPTY", "1")
	cfgPath := closeMergedConfig(t, ghBin, herdrBin)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	var doc scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.Results) != 0 {
		t.Fatalf("results = %+v, want empty (refused)", doc.Results)
	}
	if len(doc.Warnings) == 0 {
		t.Fatalf("warnings empty, want close-merged refused: %s", stdout.String())
	}
	if !strings.Contains(stderr.String(), "every orphan is no_pr") {
		t.Fatalf("stderr = %q, want all-no_pr refusal", stderr.String())
	}
	if doc.Unblock.Status != scan.UnblockNotRun {
		t.Fatalf("unblock = %+v, want not_run", doc.Unblock)
	}
}

func TestTabsCloseMergedForceAllNoPR(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("GH_FAKE_SEARCH_EMPTY", "1")
	cfgPath := closeMergedConfig(t, ghBin, herdrBin)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--force", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.Results) != 0 {
		t.Fatalf("results = %+v, want empty (nothing merged)", doc.Results)
	}
	if doc.Unblock.Status != scan.UnblockNotRun || len(doc.Unblock.Lines) != 0 {
		t.Fatalf("unblock = %+v, want not_run", doc.Unblock)
	}
}

func TestTabsCloseMergedGoFailureExit3(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("HERDR_FAKE_TAB_CLOSE", "fail")
	cfgPath := closeMergedConfig(t, ghBin, herdrBin)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--go", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	var doc scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.Results) != 1 || doc.Results[0].Action != scan.ActionFailed {
		t.Fatalf("results = %+v, want failed", doc.Results)
	}
	if doc.Unblock.Status != scan.UnblockNotRun {
		t.Fatalf("unblock = %+v, want not_run when nothing closed", doc.Unblock)
	}
}

func TestTabsCloseMergedGoJiraFailureKeepsExit(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("JIRA_FAKE_FAIL", "1")
	t.Setenv("JIRA_FAKE_STRICT", "")
	cfgPath := closeMergedConfig(t, ghBin, herdrBin)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--go", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitOK, stderr.String(), stdout.String())
	}
	var doc scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.Results) != 1 || doc.Results[0].Action != scan.ActionClosed {
		t.Fatalf("results = %+v, want closed", doc.Results)
	}
	if doc.Unblock.Status != scan.UnblockFailed || len(doc.Unblock.Lines) != 0 {
		t.Fatalf("unblock = %+v, want failed with no lines", doc.Unblock)
	}
	if !strings.Contains(doc.Unblock.Text, "unblock check could not run:") || strings.Contains(doc.Unblock.Text, "clear to start") {
		t.Fatalf("text = %q, want a failed check and no freed ticket", doc.Unblock.Text)
	}
}

func TestTabsCloseMergedLaterCloseFailureStillReportsUnblock(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	dir := t.TempDir()
	tabList := filepath.Join(dir, "tabs.json")
	agentList := filepath.Join(dir, "agents.json")
	searchFile := filepath.Join(dir, "search.json")
	issueDir := filepath.Join(dir, "issues")
	if err := os.MkdirAll(issueDir, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, tabList, `{
  "result": {
    "tabs": [
      {"tab_id": "w2:t1", "workspace_id": "w2", "label": "PROJ-1", "agent_status": "idle", "pane_count": 1},
      {"tab_id": "w2:t2", "workspace_id": "w2", "label": "PROJ-2", "agent_status": "idle", "pane_count": 1}
    ]
  }
}
`)
	writeFile(t, agentList, `{
  "result": {
    "agents": [
      {"pane_id": "w2:p1", "tab_id": "w2:t1", "agent": "codex", "agent_status": "idle", "state_change_seq": 1, "revision": 1},
      {"pane_id": "w2:p2", "tab_id": "w2:t2", "agent": "codex", "agent_status": "idle", "state_change_seq": 1, "revision": 1}
    ]
  }
}
`)
	writeFile(t, searchFile, `[
  {
    "number": 101,
    "title": "[PROJ-1] Land the blocker",
    "url": "https://github.com/acme/widgets/pull/101",
    "state": "merged",
    "isDraft": false,
    "closedAt": "2026-08-18T22:19:28Z",
    "repository": {"nameWithOwner": "acme/widgets"}
  },
  {
    "number": 102,
    "title": "[PROJ-2] Other merge",
    "url": "https://github.com/acme/widgets/pull/102",
    "state": "merged",
    "isDraft": false,
    "closedAt": "2026-08-18T22:19:28Z",
    "repository": {"nameWithOwner": "acme/widgets"}
  }
]
`)
	writeFile(t, filepath.Join(issueDir, "PROJ-1.json"), `{"key":"PROJ-1","fields":{"summary":"Land the blocker","status":{"name":"indeterminate","statusCategory":{"key":"indeterminate"}},"issuelinks":[{"type":{"name":"Blocks","inward":"is blocked by","outward":"blocks"},"outwardIssue":{"key":"PROJ-U","fields":{"summary":"Add the button","status":{"name":"indeterminate","statusCategory":{"key":"indeterminate"}}}}}]}}`)
	writeFile(t, filepath.Join(issueDir, "PROJ-U.json"), `{"key":"PROJ-U","fields":{"summary":"Add the button","status":{"name":"indeterminate","statusCategory":{"key":"indeterminate"}},"issuelinks":[{"type":{"name":"Blocks","inward":"is blocked by","outward":"blocks"},"inwardIssue":{"key":"PROJ-1","fields":{"summary":"Land the blocker","status":{"name":"indeterminate","statusCategory":{"key":"indeterminate"}}}}}]}}`)

	t.Setenv("HERDR_FAKE_TAB_LIST", tabList)
	t.Setenv("HERDR_FAKE_AGENT_LIST", agentList)
	t.Setenv("HERDR_FAKE_TAB_CLOSE", "fail-second")
	t.Setenv("HERDR_FAKE_CLOSE_COUNT", filepath.Join(dir, "close-count"))
	t.Setenv("GH_FAKE_SEARCH_FILE", searchFile)
	t.Setenv("GH_FAKE_SEARCH_EMPTY", "")
	t.Setenv("JIRA_FAKE_DIR", issueDir)
	t.Setenv("JIRA_FAKE_STRICT", "1")
	t.Setenv("JIRA_FAKE_FAIL", "")
	cfgPath := closeMergedConfig(t, ghBin, herdrBin)

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--go", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	var doc scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if len(doc.Results) != 2 {
		t.Fatalf("results = %+v, want closed then failed", doc.Results)
	}
	if doc.Results[0].Action != scan.ActionClosed || doc.Results[0].Ticket != "PROJ-1" {
		t.Fatalf("first = %+v, want closed PROJ-1", doc.Results[0])
	}
	if doc.Results[1].Action != scan.ActionFailed || doc.Results[1].Ticket != "PROJ-2" {
		t.Fatalf("second = %+v, want failed PROJ-2", doc.Results[1])
	}
	if doc.Unblock.Status != scan.UnblockOK || len(doc.Unblock.Lines) != 1 {
		t.Fatalf("unblock = %+v, want one clear line", doc.Unblock)
	}
	line := doc.Unblock.Lines[0]
	if line.Kind != scan.UnblockClear || line.Ticket != "PROJ-U" || line.Summary != "Add the button" || len(line.BlockedBy) != 1 || line.BlockedBy[0] != "PROJ-1" {
		t.Fatalf("line = %+v, want PROJ-U clear after PROJ-1", line)
	}
	const wantText = "Newly unblocked by this round\nPROJ-U \"Add the button\" — was blocked only by PROJ-1 (just merged); now clear to start"
	if doc.Unblock.Text != wantText {
		t.Fatalf("text = %q, want %q", doc.Unblock.Text, wantText)
	}
}

func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
}

func jiraFakeBin(t *testing.T) string {
	t.Helper()
	bin, err := filepath.Abs(filepath.Join("testdata", "fixtures", "jira-fake.sh"))
	if err != nil {
		t.Fatal(err)
	}
	return bin
}

func closeMergedConfig(t *testing.T, ghBin, herdrBin string) string {
	t.Helper()
	return writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\njira_bin="+jiraFakeBin(t)+"\nauthor=alice\n")
}

func TestTabsOrphansHerdrMissingExit3(t *testing.T) {
	t.Parallel()

	ghBin, _ := fixtureBins(t)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin=prsync-test-missing-herdr\nauthor=alice\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
	}
	if !strings.Contains(stderr.String(), "herdr binary not found") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}
