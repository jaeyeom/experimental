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
	t.Setenv("HERDR_FAKE_TAB_CLOSE_SENTINEL", sentinel)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
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
}

func TestTabsCloseMergedGoCloses(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	sentinel := filepath.Join(t.TempDir(), "close")
	t.Setenv("HERDR_FAKE_TAB_CLOSE_SENTINEL", sentinel)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
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
}

func TestTabsCloseMergedGoSkippedNotFound(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("HERDR_FAKE_TAB_CLOSE", "not_found")
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
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
}

func TestTabsCloseMergedAllNoPRRefuses(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("GH_FAKE_SEARCH_EMPTY", "1")
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
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
}

func TestTabsCloseMergedForceAllNoPR(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("GH_FAKE_SEARCH_EMPTY", "1")
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
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
}

func TestTabsCloseMergedGoFailureExit3(t *testing.T) {
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("HERDR_FAKE_TAB_CLOSE", "fail")
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\n")
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
