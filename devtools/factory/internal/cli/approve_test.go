package cli

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func writeApproveConfig(t *testing.T, dryRun bool, guards string) (string, string) {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	cfgPath := filepath.Join(dir, "factory.toml")
	cfg := fmt.Sprintf(`
[defaults]
dry_run = %t

[[source]]
name = "widgets"
kind = "github"
repo = "acme/widgets"
cwd = %q
base = "main"
require_labels = ["agent-ready"]
start_prompt = "Work on {id}"

  [source.trust]
  environment = "untrusted"
  guards = %s
`, dryRun, cwd, guards)
	if err := os.WriteFile(cfgPath, []byte(cfg), 0o600); err != nil {
		t.Fatal(err)
	}
	return cfgPath, cwd
}

func TestApproveDryRunAndApplyAndRevoke(t *testing.T) {
	cfgPath, _ := writeApproveConfig(t, true, `["approved_hash"]`)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	const issueID = "acme/widgets#42"
	const title = "External feature request"
	const body = "Line 1\nLine 2"
	wantHash := source.ContentHash(title, body)

	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := source.Item{
			Source:            "github:widgets",
			ID:                issueID,
			Title:             title,
			URL:               "https://github.com/acme/widgets/issues/42",
			Type:              "Issue",
			State:             "Backlog",
			Labels:            []string{"agent-ready"},
			Author:            "stranger",
			AuthorAssociation: "NONE",
			Locked:            false,
			ContentHash:       wantHash,
			Created:           day1,
			Updated:           day1,
		}
		if err := st.UpsertItems(ctx, "github:widgets", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "github:widgets", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "github:widgets", "maintainer"); err != nil {
			t.Fatal(err)
		}
	})

	src := &fake.Source{
		SourceName: "github:widgets",
		Account:    "maintainer",
		Items: map[string]source.Item{
			issueID: {
				Source:            "github:widgets",
				ID:                issueID,
				Title:             title,
				URL:               "https://github.com/acme/widgets/issues/42",
				Type:              "Issue",
				State:             "Backlog",
				Labels:            []string{"agent-ready"},
				Author:            "stranger",
				AuthorAssociation: "NONE",
				Locked:            false,
				Created:           day1,
				Updated:           day1,
			},
		},
		Bodies: map[string]string{issueID: body},
	}
	useSources(t, map[string]*fake.Source{"widgets": src})

	// 1. Dry-run prints title, body, hash, and planned Todo transition without writing.
	stdout, stderr, code := runCmd(t, "approve", issueID, "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("dry-run exit = %d, stderr = %s\nstdout = %s", code, stderr, stdout)
	}
	for _, want := range []string{
		"id:      " + issueID,
		"status:  Backlog -> Todo",
		"hash:    " + wantHash,
		"title:   " + title,
		"body:\n" + body,
		"dry-run:",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("dry-run stdout missing %q:\n%s", want, stdout)
		}
	}
	if src.Items[issueID].State != "Backlog" {
		t.Fatalf("dry-run mutated state to %q", src.Items[issueID].State)
	}
	assertNoApproval(t, dbPath, "github:widgets", issueID)

	// 2. --apply records approval in local store, transitions status to Todo, and updates mirror.
	stdout, stderr, code = runCmd(t, "approve", issueID, "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("apply exit = %d, stderr = %s\nstdout = %s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "approved "+issueID+" ("+wantHash+")") {
		t.Fatalf("apply stdout =\n%s", stdout)
	}
	if src.Items[issueID].State != "Todo" {
		t.Fatalf("after apply state = %q, want Todo", src.Items[issueID].State)
	}
	gotApproval := readApproval(t, dbPath, "github:widgets", issueID)
	if gotApproval.ContentHash != wantHash {
		t.Fatalf("stored hash = %q, want %q", gotApproval.ContentHash, wantHash)
	}

	// Next & show now see the item as ready with PASS trust.
	useHerdrGH(t, githubWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, issueID) {
		t.Fatalf("next after approve exit=%d stderr=%q stdout=\n%s", code, stderr, stdout)
	}
	useHerdrGH(t, githubWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "show", issueID, "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, "approved_hash: "+wantHash) || !strings.Contains(stdout, "status: PASS") {
		t.Fatalf("show after approve exit=%d stderr=%q stdout=\n%s", code, stderr, stdout)
	}

	// 3. Re-running approve when already in Todo does not call SetStatus again.
	src.Calls = nil
	stdout, stderr, code = runCmd(t, "approve", issueID, "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("re-approve exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "status:  Todo (already in untrusted_ready_statuses)") {
		t.Fatalf("re-approve stdout =\n%s", stdout)
	}
	for _, c := range src.Calls {
		if c.Method == "SetStatus" {
			t.Fatalf("unexpected SetStatus call when already Todo: %#v", src.Calls)
		}
	}

	// 4. --revoke clears the stored local approval without touching remote source.
	src.Calls = nil
	stdout, stderr, code = runCmd(t, "approve", "--revoke", issueID, "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("revoke exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "revoked "+issueID) {
		t.Fatalf("revoke stdout = %q", stdout)
	}
	if len(src.Calls) != 0 {
		t.Fatalf("revoke made unexpected source calls: %#v", src.Calls)
	}
	assertNoApproval(t, dbPath, "github:widgets", issueID)

	// Next now blocks the item again as not approved.
	useHerdrGH(t, githubWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "next", "--explain", issueID, "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, "not approved") {
		t.Fatalf("explain after revoke exit=%d stderr=%q stdout=\n%s", code, stderr, stdout)
	}
}

func TestApproveImpliedApplyWhenDryRunFalse(t *testing.T) {
	cfgPath, _ := writeApproveConfig(t, false, `["approved_hash"]`)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	const issueID = "acme/widgets#9"
	wantHash := source.ContentHash("Title", "Body")

	src := &fake.Source{
		SourceName: "github:widgets",
		Account:    "maintainer",
		Items: map[string]source.Item{
			issueID: {
				Source:            "github:widgets",
				ID:                issueID,
				Title:             "Title",
				State:             "Backlog",
				Author:            "stranger",
				AuthorAssociation: "NONE",
			},
		},
		Bodies: map[string]string{issueID: "Body"},
	}
	useSources(t, map[string]*fake.Source{"widgets": src})

	// Item is not in mirror yet; approve still resolves it live and writes.
	stdout, stderr, code := runCmd(t, "approve", issueID, "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s\nstdout = %s", code, stderr, stdout)
	}
	if src.Items[issueID].State != "Todo" {
		t.Fatalf("state = %q, want Todo", src.Items[issueID].State)
	}
	if got := readApproval(t, dbPath, "github:widgets", issueID); got.ContentHash != wantHash {
		t.Fatalf("approval hash = %q, want %q", got.ContentHash, wantHash)
	}
}

func readApproval(t *testing.T, dbPath, srcName, id string) store.Approval {
	t.Helper()
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			t.Errorf("close store: %v", cerr)
		}
	}()
	a, err := st.Approval(context.Background(), srcName, id)
	if err != nil {
		t.Fatalf("Approval(%s, %s): %v", srcName, id, err)
	}
	return a
}

func assertNoApproval(t *testing.T, dbPath, srcName, id string) {
	t.Helper()
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			t.Errorf("close store: %v", cerr)
		}
	}()
	if _, err := st.Approval(context.Background(), srcName, id); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Approval(%s, %s) = %v, want ErrNotFound", srcName, id, err)
	}
}
