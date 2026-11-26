package cli

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func closeItem(src, id, title, parent string, done bool) source.Item {
	state := "To Do"
	if done {
		state = "Done"
	}
	return source.Item{
		Source:  src,
		ID:      id,
		Title:   title,
		Type:    "Task",
		State:   state,
		Done:    done,
		Parent:  parent,
		Created: day1,
		Updated: day1,
	}
}

func seedCloseDB(t *testing.T, dbPath string, itemsBySource map[string][]source.Item) {
	t.Helper()
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		for srcKey, items := range itemsBySource {
			if err := st.UpsertItems(ctx, srcKey, items); err != nil {
				t.Fatal(err)
			}
			if err := st.SetSyncState(ctx, srcKey, day1, day1); err != nil {
				t.Fatal(err)
			}
			if err := st.SetMe(ctx, srcKey, "alice"); err != nil {
				t.Fatal(err)
			}
		}
	})
}

func TestCloseDetectTextAndJSON(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	items := []source.Item{
		closeItem(bearKey, "PRJ-1", "Ready Epic", "", false),
		closeItem(bearKey, "PRJ-2", "Done child 1", "PRJ-1", true),
		closeItem(bearKey, "PRJ-3", "Done child 2", "PRJ-1", true),
		closeItem(bearKey, "PRJ-4", "In-progress Epic", "", false),
		closeItem(bearKey, "PRJ-5", "Open child", "PRJ-4", false),
		closeItem(bearKey, "PRJ-6", "Leaf task", "", false),
	}
	seedCloseDB(t, db, map[string][]source.Item{bearKey: items})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "close")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	const want = "PRJ-1  eligible  2 done, 0 open  Ready Epic\n"
	if stdout != want {
		t.Fatalf("stdout =\n%q\nwant:\n%q", stdout, want)
	}

	stdout, stderr, code = runWriteCmd(t, cfg, db, "close", "--json")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	var got []closeResultJSON
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("unmarshal: %v\n%s", err, stdout)
	}
	if len(got) != 1 || got[0].ID != "PRJ-1" || !got[0].Eligible || len(got[0].DoneChildren) != 2 || len(got[0].OpenChildren) != 0 || got[0].Closed {
		t.Fatalf("json = %+v", got)
	}
}

func TestCloseDetectEmptyExitsZeroAndPrintsNothing(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	items := []source.Item{
		closeItem(bearKey, "PRJ-1", "Still open", "", false),
		closeItem(bearKey, "PRJ-2", "Open child", "PRJ-1", false),
	}
	seedCloseDB(t, db, map[string][]source.Item{bearKey: items})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "close")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}

	stdout, stderr, code = runWriteCmd(t, cfg, db, "close", "--json")
	assertExit(t, code, stdout, stderr, ExitOK)
	if strings.TrimSpace(stdout) != "[]" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
}

func TestCloseScopeAndNoSync(t *testing.T) {
	outer := t.TempDir()
	inner := filepath.Join(outer, "cub")
	if err := os.MkdirAll(inner, 0o750); err != nil {
		t.Fatal(err)
	}
	cfg := writeNextConfig(t,
		nextSource{name: "acme", cwd: outer, extra: "max_age = \"5m\"\n"},
		nextSource{name: "cub", cwd: inner, extra: "max_age = \"5m\"\n"},
	)
	db := filepath.Join(t.TempDir(), "factory.db")
	acmeItems := []source.Item{
		closeItem(bearKey, "PRJ-1", "Acme parent", "", false),
		closeItem(bearKey, "PRJ-2", "Acme child", "PRJ-1", true),
	}
	cubItems := []source.Item{
		closeItem(cubKey, "CUB-1", "Cub parent", "", false),
		closeItem(cubKey, "CUB-2", "Cub child", "CUB-1", true),
	}
	seedCloseDB(t, db, map[string][]source.Item{
		bearKey: acmeItems,
		cubKey:  cubItems,
	})

	// Advance clock past max_age so auto-sync would trigger unless --no-sync is set.
	prevNow := nowUTC
	nowUTC = func() time.Time { return day1.Add(10 * time.Minute) }
	t.Cleanup(func() { nowUTC = prevNow })

	acmeSrc := &fake.Source{SourceName: bearKey, Account: "alice", Items: copyItems(acmeItems)}
	cubSrc := &fake.Source{SourceName: cubKey, Account: "alice", Items: copyItems(cubItems)}
	useSources(t, map[string]*fake.Source{"acme": acmeSrc, "cub": cubSrc})

	// Scoped to current directory with --no-sync: does not call Sync.
	inDir(t, outer)
	stdout, stderr, code := runWriteCmd(t, cfg, db, "close", "--no-sync")
	assertExit(t, code, stdout, stderr, ExitOK)
	if !strings.Contains(stderr, bearLine) {
		t.Fatalf("stderr = %q, want %q", stderr, bearLine)
	}
	if !strings.Contains(stdout, "PRJ-1") || strings.Contains(stdout, "CUB-1") {
		t.Fatalf("stdout = %q", stdout)
	}
	if methodCount(acmeSrc, "Sync")+methodCount(cubSrc, "Sync") != 0 {
		t.Fatalf("unexpected Sync with --no-sync: acme=%v cub=%v", acmeSrc.Calls, cubSrc.Calls)
	}

	// Without --no-sync: auto-syncs stale source in scope.
	stdout, stderr, code = runWriteCmd(t, cfg, db, "close")
	assertExit(t, code, stdout, stderr, ExitOK)
	if !strings.Contains(stdout, "PRJ-1") || strings.Contains(stdout, "CUB-1") {
		t.Fatalf("stdout = %q", stdout)
	}
	if methodCount(acmeSrc, "Sync") != 1 || methodCount(cubSrc, "Sync") != 0 {
		t.Fatalf("Sync counts: acme=%d cub=%d", methodCount(acmeSrc, "Sync"), methodCount(cubSrc, "Sync"))
	}

	// --source picks cub even from outer dir.
	stdout, stderr, code = runWriteCmd(t, cfg, db, "close", "--source", cubKey, "--no-sync")
	assertExit(t, code, stdout, stderr, ExitOK)
	if !strings.Contains(stdout, "CUB-1") || strings.Contains(stdout, "PRJ-1") || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}

	// --all shows both.
	stdout, stderr, code = runWriteCmd(t, cfg, db, "close", "--all", "--no-sync")
	assertExit(t, code, stdout, stderr, ExitOK)
	if !strings.Contains(stdout, "PRJ-1") || !strings.Contains(stdout, "CUB-1") || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
}

func TestCloseEvaluateNamedIDsDryRun(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	items := []source.Item{
		closeItem(bearKey, "PRJ-1", "Ready parent", "", false),
		closeItem(bearKey, "PRJ-2", "Done child", "PRJ-1", true),
		closeItem(bearKey, "PRJ-3", "Blocked parent", "", false),
		closeItem(bearKey, "PRJ-4", "Open child", "PRJ-3", false),
		closeItem(bearKey, "PRJ-5", "Leaf", "", false),
		closeItem(bearKey, "PRJ-6", "Done parent", "", true),
		closeItem(bearKey, "PRJ-7", "Done child of done", "PRJ-6", true),
	}
	seedCloseDB(t, db, map[string][]source.Item{bearKey: items})
	src := &fake.Source{SourceName: bearKey, Items: copyItems(items)}
	useSources(t, map[string]*fake.Source{"acme": src})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "close", "PRJ-1", "PRJ-3", "PRJ-5", "PRJ-6")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	for _, want := range []string{
		"PRJ-1  eligible  1 done, 0 open  Ready parent",
		"PRJ-3  ineligible  0 done, 1 open  1 open child: PRJ-4",
		"PRJ-5  ineligible  0 done, 0 open  no children in local cache",
		"PRJ-6  ineligible  1 done, 0 open  already done",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("stdout missing %q:\n%s", want, stdout)
		}
	}
	if methodCount(src, "Close") != 0 {
		t.Fatalf("dry-run called Close: %#v", src.Calls)
	}

	stdout, stderr, code = runWriteCmd(t, cfg, db, "close", "PRJ-3", "--json")
	assertExit(t, code, stdout, stderr, ExitOK)
	var got []closeResultJSON
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("unmarshal: %v\n%s", err, stdout)
	}
	if len(got) != 1 || got[0].Eligible || got[0].Reason != "1 open child: PRJ-4" || len(got[0].OpenChildren) != 1 || got[0].OpenChildren[0] != "PRJ-4" {
		t.Fatalf("json = %+v", got)
	}
}

func TestCloseApplyWithCommentAndCommentFile(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	items := []source.Item{
		closeItem(bearKey, "PRJ-1", "Parent 1", "", false),
		closeItem(bearKey, "PRJ-2", "Child 1", "PRJ-1", true),
		closeItem(bearKey, "PRJ-10", "Parent 2", "", false),
		closeItem(bearKey, "PRJ-11", "Child 2", "PRJ-10", true),
	}
	seedCloseDB(t, db, map[string][]source.Item{bearKey: items})
	src := &fake.Source{SourceName: bearKey, Items: copyItems(items)}
	useSources(t, map[string]*fake.Source{"acme": src})

	// Apply with --comment.
	stdout, stderr, code := runWriteCmd(t, cfg, db, "close", "PRJ-1", "--apply", "--comment", "All subtasks done.")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "PRJ-1\n" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	call := oneCall(t, src, "Close")
	if len(call.Args) != 2 || call.Args[0] != "PRJ-1" || call.Args[1] != "All subtasks done." {
		t.Fatalf("Close args = %#v", call.Args)
	}
	if got := src.Comments["PRJ-1"]; len(got) != 1 || got[0] != "All subtasks done." {
		t.Fatalf("Comments = %#v", src.Comments)
	}
	// Mirror row refreshed to Done.
	st := reopen(t, db)
	mirrored, err := st.Item(context.Background(), bearKey, "PRJ-1")
	if err != nil {
		t.Fatal(err)
	}
	if !mirrored.Done {
		t.Fatalf("mirrored PRJ-1 Done = false, want true")
	}
	// Run recorded in runs table.
	runs := listRuns(t, db)
	if len(runs) != 1 || runs[0].Command != "close" || runs[0].Outcome != "ok" {
		t.Fatalf("runs = %+v", runs)
	}

	// Apply with --comment-file.
	commentPath := filepath.Join(t.TempDir(), "comment.txt")
	if err := os.WriteFile(commentPath, []byte("Line 1\nLine 2\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	stdout, stderr, code = runWriteCmd(t, cfg, db, "close", "PRJ-10", "--apply", "--comment-file", commentPath)
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "PRJ-10\n" || stderr != "" {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	if got := src.Comments["PRJ-10"]; len(got) != 1 || got[0] != "Line 1\nLine 2\n" {
		t.Fatalf("Comments[PRJ-10] = %#v", got)
	}
}

func TestCloseCommentValidationAndLiteralNewlineWarning(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	items := []source.Item{
		closeItem(bearKey, "PRJ-1", "Parent 1", "", false),
		closeItem(bearKey, "PRJ-2", "Child 1", "PRJ-1", true),
	}
	seedCloseDB(t, db, map[string][]source.Item{bearKey: items})
	src := &fake.Source{SourceName: bearKey, Items: copyItems(items)}
	useSources(t, map[string]*fake.Source{"acme": src})

	commentPath := filepath.Join(t.TempDir(), "comment.txt")
	if err := os.WriteFile(commentPath, []byte("from file"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Both --comment and --comment-file is a usage error (exit 2).
	stdout, stderr, code := runWriteCmd(t, cfg, db, "close", "PRJ-1", "--apply", "--comment", "inline", "--comment-file", commentPath)
	assertExit(t, code, stdout, stderr, ExitUsage)
	if stdout != "" || !strings.Contains(stderr, "only one of --comment and --comment-file") {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	if methodCount(src, "Close") != 0 {
		t.Fatalf("called Close on usage error: %#v", src.Calls)
	}

	// Literal \n in --comment warns on stderr.
	stdout, stderr, code = runWriteCmd(t, cfg, db, "close", "PRJ-1", "--apply", "--comment", `line1\nline2`)
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "PRJ-1\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	const wantWarn = `factory: warning: comment contains a literal \n and no newline; use --comment-file or $'...' quoting` + "\n"
	if stderr != wantWarn {
		t.Fatalf("stderr = %q, want %q", stderr, wantWarn)
	}
}

func TestCloseApplyNoArgsClosesAllEligibleInScope(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	items := []source.Item{
		closeItem(bearKey, "PRJ-1", "Parent 1", "", false),
		closeItem(bearKey, "PRJ-2", "Child 1", "PRJ-1", true),
		closeItem(bearKey, "PRJ-3", "Parent 2", "", false),
		closeItem(bearKey, "PRJ-4", "Child 2", "PRJ-3", true),
		closeItem(bearKey, "PRJ-5", "Still open parent", "", false),
		closeItem(bearKey, "PRJ-6", "Open child", "PRJ-5", false),
	}
	seedCloseDB(t, db, map[string][]source.Item{bearKey: items})
	src := &fake.Source{SourceName: bearKey, Items: copyItems(items)}
	useSources(t, map[string]*fake.Source{"acme": src})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "close", "--apply")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	if stdout != "PRJ-1\nPRJ-3\n" {
		t.Fatalf("stdout = %q, want PRJ-1 and PRJ-3", stdout)
	}
	if methodCount(src, "Close") != 2 {
		t.Fatalf("Close count = %d, want 2: %#v", methodCount(src, "Close"), src.Calls)
	}
	if len(src.Comments) != 0 {
		t.Fatalf("expected no comments when --comment is empty, got %#v", src.Comments)
	}
	st := reopen(t, db)
	for _, id := range []string{"PRJ-1", "PRJ-3"} {
		it, err := st.Item(context.Background(), bearKey, id)
		if err != nil {
			t.Fatal(err)
		}
		if !it.Done {
			t.Fatalf("mirrored %s Done = false, want true", id)
		}
	}
	runs := listRuns(t, db)
	if len(runs) != 1 || runs[0].Command != "close" || runs[0].Outcome != "ok" {
		t.Fatalf("runs = %+v", runs)
	}
}

func TestCloseApplyRefusalsMakeZeroWrites(t *testing.T) {
	cfg := writeSources(t,
		srcSpec{kind: "jira", name: "acme"},
		srcSpec{kind: "jira", name: "cub"},
	)
	db := filepath.Join(t.TempDir(), "factory.db")
	acmeItems := []source.Item{
		closeItem(bearKey, "PRJ-1", "Eligible parent", "", false),
		closeItem(bearKey, "PRJ-2", "Done child", "PRJ-1", true),
		closeItem(bearKey, "PRJ-3", "Parent with open children", "", false),
		closeItem(bearKey, "PRJ-4", "Open child A", "PRJ-3", false),
		closeItem(bearKey, "PRJ-5", "Open child B", "PRJ-3", false),
		closeItem(bearKey, "PRJ-6", "Leaf issue", "", false),
		closeItem(bearKey, "PRJ-7", "Already done parent", "", true),
		closeItem(bearKey, "PRJ-8", "Done child", "PRJ-7", true),
	}
	cubItems := []source.Item{
		closeItem(cubKey, "CUB-1", "Cub eligible parent", "", false),
		closeItem(cubKey, "CUB-2", "Cub done child", "CUB-1", true),
	}
	seedCloseDB(t, db, map[string][]source.Item{
		bearKey: acmeItems,
		cubKey:  cubItems,
	})

	tests := []struct {
		name       string
		envSource  string
		args       []string
		wantStderr []string
	}{
		{
			name:       "open children lists open child IDs",
			args:       []string{"close", "PRJ-3", "--apply"},
			wantStderr: []string{"PRJ-3", "2 open children: PRJ-4, PRJ-5"},
		},
		{
			name:       "leaf issue with no children",
			args:       []string{"close", "PRJ-6", "--apply"},
			wantStderr: []string{"PRJ-6", "no children in local cache"},
		},
		{
			name:       "already done item",
			args:       []string{"close", "PRJ-7", "--apply"},
			wantStderr: []string{"PRJ-7", "already done"},
		},
		{
			name:       "unknown ID",
			args:       []string{"close", "PRJ-999", "--apply"},
			wantStderr: []string{"PRJ-999", "found none"},
		},
		{
			name:       "batch with one eligible and one ineligible writes nothing",
			args:       []string{"close", "PRJ-1", "PRJ-3", "--apply"},
			wantStderr: []string{"PRJ-3", "PRJ-4"},
		},
		{
			name:       "mismatched FACTORY_ITEM_SOURCE on explicit ID",
			envSource:  bearKey,
			args:       []string{"close", "CUB-1", "--apply"},
			wantStderr: []string{"CUB-1", bearKey, cubKey},
		},
		{
			name:       "mismatched FACTORY_ITEM_SOURCE on detect-all apply",
			envSource:  bearKey,
			args:       []string{"close", "--all", "--apply"},
			wantStderr: []string{"CUB-1", bearKey, cubKey},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if tc.envSource != "" {
				t.Setenv("FACTORY_ITEM_SOURCE", tc.envSource)
			}
			acme := &fake.Source{SourceName: bearKey, Items: copyItems(acmeItems)}
			cub := &fake.Source{SourceName: cubKey, Items: copyItems(cubItems)}
			useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})

			stdout, stderr, code := runWriteCmd(t, cfg, db, tc.args...)
			assertExit(t, code, stdout, stderr, ExitError)
			if stdout != "" {
				t.Fatalf("stdout = %q, want empty", stdout)
			}
			for _, want := range tc.wantStderr {
				if !strings.Contains(stderr, want) {
					t.Fatalf("stderr = %q, missing %q", stderr, want)
				}
			}
			if methodCount(acme, "Close")+methodCount(cub, "Close") != 0 {
				t.Fatalf("Close was called on refusal: acme=%#v cub=%#v", acme.Calls, cub.Calls)
			}
		})
	}
}

func TestCloseHasNoForceFlag(t *testing.T) {
	stdout, stderr, code := runCmd(t, "close", "PRJ-1", "--apply", "--force")
	assertExit(t, code, stdout, stderr, ExitUsage)
	if !strings.Contains(stderr, "unknown flag: --force") {
		t.Fatalf("stderr = %q", stderr)
	}
}
