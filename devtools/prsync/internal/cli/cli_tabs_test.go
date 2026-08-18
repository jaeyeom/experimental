package cli

import (
	"bytes"
	"context"
	"encoding/json"
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
