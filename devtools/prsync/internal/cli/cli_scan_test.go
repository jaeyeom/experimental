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
	if doc.PRs[0].Tab == nil || doc.PRs[0].Tab.TabID != "w2:tC" {
		t.Fatalf("tab = %+v, want w2:tC", doc.PRs[0].Tab)
	}
	if doc.PRs[0].Tab.PaneID == nil || *doc.PRs[0].Tab.PaneID != "w2:pC" {
		t.Fatalf("pane_id = %v, want w2:pC", doc.PRs[0].Tab.PaneID)
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
}

func writeScanConfig(t *testing.T, body string) string {
	t.Helper()
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
