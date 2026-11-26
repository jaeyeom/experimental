package cli

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func writeMultiSourceStartConfig(t *testing.T, topClaim string, jiraCwd, ghCwd string) string {
	t.Helper()
	dir := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	body := fmt.Sprintf(`
[lease]
gate = "off"
%s
[[source]]
name = "acme"
kind = "jira"
site = "https://example.atlassian.net"
email = "dev@example.com"
token_file = %q
jql = "project = PRJ"
claim_status = "In Progress"
cwd = %q
base = "main"
start_prompt = "Implement {id} {title}"

[[source]]
name = "widgets"
kind = "github"
repo = "acme/widgets"
cwd = %q
base = "main"
start_prompt = "Fix {id}"
`, topClaim, token, jiraCwd, ghCwd)
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func seedMultiSourceStartItems(t *testing.T, dbPath string, ghItem source.Item) {
	t.Helper()
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		jiraIt := source.Item{
			Source: "jira:acme", ID: "PRJ-1", Title: "Fix it",
			URL: "https://example.atlassian.net/browse/PRJ-1", Type: "Task", State: "To Do",
		}
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{jiraIt}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "ada"); err != nil {
			t.Fatal(err)
		}
		if err := st.UpsertItems(ctx, "github:widgets", []source.Item{ghItem}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "github:widgets", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "github:widgets", "ada"); err != nil {
			t.Fatal(err)
		}
	})
}

func TestStartSourceContainmentItemSource(t *testing.T) {
	jiraCwd := t.TempDir()
	ghCwd := t.TempDir()
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	ghItem := source.Item{
		Source: "github:widgets", ID: "acme/widgets#42", Title: "Widget fix",
		URL: "https://github.com/acme/widgets/issues/42", Type: "issue", State: "OPEN",
	}
	seedMultiSourceStartItems(t, dbPath, ghItem)

	// With default source_containment = true, FACTORY_ITEM_SOURCE refuses starting another source.
	t.Setenv("FACTORY_ITEM_SOURCE", "jira:acme")
	cfgPath := writeMultiSourceStartConfig(t, "", jiraCwd, ghCwd)
	inDir(t, t.TempDir())
	_, stderr, code := runCmd(t, "start", "acme/widgets#42", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, "FACTORY_ITEM_SOURCE") || !strings.Contains(stderr, "jira:acme") {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}

	// Even --source cannot bypass FACTORY_ITEM_SOURCE when source_containment = true.
	_, stderr, code = runCmd(t, "start", "acme/widgets#42", "--source", "github:widgets", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, "FACTORY_ITEM_SOURCE") {
		t.Fatalf("--source bypass exit = %d, stderr = %q", code, stderr)
	}

	// With source_containment = false, starting another source is allowed.
	cfgOff := writeMultiSourceStartConfig(t, "[claim]\nsource_containment = false\n", jiraCwd, ghCwd)
	useSources(t, map[string]*fake.Source{
		"acme":    startFake(),
		"widgets": {SourceName: "github:widgets", Account: "ada", ClaimStatus: "In progress", Items: map[string]source.Item{ghItem.ID: ghItem}},
	})
	startMock(t, rankWorkspaces, emptyTabs, false, ghCwd)
	stdout, stderr, code := runCmd(t, "start", "acme/widgets#42", "--config", cfgOff, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, "acme/widgets#42 → In progress") {
		t.Fatalf("containment=false exit=%d stderr=%q stdout=\n%s", code, stderr, stdout)
	}
}

func TestStartSourceContainmentGitRepoAndShorthands(t *testing.T) {
	jiraCwd := t.TempDir()
	ghCwd := t.TempDir()
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	ghItem := source.Item{
		Source: "github:widgets", ID: "acme/widgets#42", Title: "Widget fix",
		URL: "https://github.com/acme/widgets/issues/42", Type: "issue", State: "OPEN",
	}
	seedMultiSourceStartItems(t, dbPath, ghItem)
	cfgPath := writeMultiSourceStartConfig(t, "", jiraCwd, ghCwd)
	useSources(t, map[string]*fake.Source{
		"acme":    startFake(),
		"widgets": {SourceName: "github:widgets", Account: "ada", ClaimStatus: "In progress", Items: map[string]source.Item{ghItem.ID: ghItem}},
	})

	// Inside ghCwd: 42, #42, and GitHub issue URL all resolve to acme/widgets#42.
	inDir(t, ghCwd)
	for _, arg := range []string{"42", "#42", "https://github.com/acme/widgets/issues/42"} {
		t.Run("github_"+arg, func(t *testing.T) {
			startMock(t, rankWorkspaces, emptyTabs, false, ghCwd)
			stdout, stderr, code := runCmd(t, "start", arg, "--config", cfgPath, "--db", dbPath)
			if code != ExitOK || stderr != "" || !strings.Contains(stdout, "acme/widgets#42 → In progress") {
				t.Fatalf("arg=%q exit=%d stderr=%q stdout=\n%s", arg, code, stderr, stdout)
			}
		})
	}

	// Inside ghCwd: starting a Jira item from another source is refused unless --source is passed.
	_, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, "expected source github:widgets") {
		t.Fatalf("cross-source exit=%d stderr=%q", code, stderr)
	}

	// Inside jiraCwd: Jira URL resolves to PRJ-1.
	inDir(t, jiraCwd)
	startMock(t, rankWorkspaces, emptyTabs, false, jiraCwd)
	stdout, stderr, code := runCmd(t, "start", "https://example.atlassian.net/browse/PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, "PRJ-1 → In Progress") {
		t.Fatalf("jira url exit=%d stderr=%q stdout=\n%s", code, stderr, stdout)
	}

	// Inside an unconfigured git repo (via XDG config, no --config): refused unless --source is passed.
	xdgDir := t.TempDir()
	xdgCfgDir := filepath.Join(xdgDir, "factory")
	if err := os.MkdirAll(xdgCfgDir, 0o750); err != nil {
		t.Fatal(err)
	}
	rawCfg, err := os.ReadFile(cfgPath) //nolint:gosec // test temp file
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(xdgCfgDir, "factory.toml"), rawCfg, 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("XDG_CONFIG_HOME", xdgDir)
	unconfiguredRepo := t.TempDir()
	if err := os.Mkdir(filepath.Join(unconfiguredRepo, ".git"), 0o750); err != nil {
		t.Fatal(err)
	}
	inDir(t, unconfiguredRepo)
	_, stderr, code = runCmd(t, "start", "PRJ-1", "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, unconfiguredRepo) || !strings.Contains(stderr, "factory init") || !strings.Contains(stderr, "--source") {
		t.Fatalf("unconfigured repo exit=%d stderr=%q", code, stderr)
	}
	startMock(t, rankWorkspaces, emptyTabs, false, jiraCwd)
	stdout, stderr, code = runCmd(t, "start", "PRJ-1", "--source", "jira:acme", "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, "PRJ-1 → In Progress") {
		t.Fatalf("unconfigured repo with --source exit=%d stderr=%q stdout=\n%s", code, stderr, stdout)
	}
}

func TestStartForceTakingOwnershipCLI(t *testing.T) {
	jiraCwd := t.TempDir()
	ghCwd := t.TempDir()
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	ghItem := source.Item{
		Source: "github:widgets", ID: "acme/widgets#42", Title: "Widget fix",
		URL: "https://github.com/acme/widgets/issues/42", Type: "issue", State: "In progress",
		Assignee: "bob",
	}
	seedMultiSourceStartItems(t, dbPath, ghItem)
	cfgPath := writeMultiSourceStartConfig(t, "[claim]\nnickname = \"mac-studio\"\n", jiraCwd, ghCwd)
	ghSrc := &fake.Source{
		SourceName:  "github:widgets",
		Account:     "ada",
		ClaimStatus: "In progress",
		Items:       map[string]source.Item{ghItem.ID: ghItem},
	}
	useSources(t, map[string]*fake.Source{"acme": startFake(), "widgets": ghSrc})
	inDir(t, ghCwd)

	// Blocked without --force-taking-ownership (even with --force).
	startMock(t, rankWorkspaces, emptyTabs, false, ghCwd)
	_, stderr, code := runCmd(t, "start", "#42", "--force", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused || !strings.Contains(stderr, "bob") {
		t.Fatalf("--force without takeover exit=%d stderr=%q", code, stderr)
	}

	// Succeeds in dry-run with --force-taking-ownership and prints takeover warning + comment preview.
	startMock(t, rankWorkspaces, emptyTabs, false, ghCwd)
	stdout, stderr, code := runCmd(t, "start", "#42", "--force-taking-ownership", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("dry-run takeover exit=%d stderr=%q stdout=\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "warning: --force-taking-ownership takes over acme/widgets#42") ||
		!strings.Contains(stdout, "comment  Claimed by claude on mac-studio at <timestamp>.") {
		t.Fatalf("dry-run takeover stdout =\n%s", stdout)
	}
	if len(ghSrc.Comments[ghItem.ID]) != 0 {
		t.Fatalf("dry-run posted comments: %#v", ghSrc.Comments)
	}

	// Apply with --force-taking-ownership and subagent spawner replaces assignee and posts claim comment.
	cfgSub := writeMultiSourceStartConfig(t, "[claim]\nnickname = \"mac-studio\"\n\n[worker]\nspawner = \"subagent\"\n", jiraCwd, ghCwd)
	mock := startMock(t, rankWorkspaces, emptyTabs, false, ghCwd)
	mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	stdout, stderr, code = runCmd(t, "start", "#42", "--apply", "--force-taking-ownership", "--config", cfgSub, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("apply takeover exit=%d stderr=%q stdout=\n%s", code, stderr, stdout)
	}
	if !slices.Equal(ghSrc.LastClaimOptions[ghItem.ID].RemoveAssignees, []string{"bob"}) {
		t.Fatalf("RemoveAssignees = %#v, want [bob]", ghSrc.LastClaimOptions[ghItem.ID].RemoveAssignees)
	}
	if len(ghSrc.Comments[ghItem.ID]) != 1 || !strings.HasPrefix(ghSrc.Comments[ghItem.ID][0], "Claimed by claude on mac-studio at ") {
		t.Fatalf("comments = %#v", ghSrc.Comments[ghItem.ID])
	}
}

func TestStartClosedDoneRefusedEvenWithBothForceFlagsCLI(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := source.Item{
			Source: "jira:acme", ID: "PRJ-1", Title: "Already done",
			URL: "https://example.test/browse/PRJ-1", Type: "Task", State: "Done", Done: true,
		}
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "ada"); err != nil {
			t.Fatal(err)
		}
	})
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	_, stderr, code := runCmd(t, "start", "PRJ-1", "--force", "--force-taking-ownership", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused || !strings.Contains(stderr, "done") {
		t.Fatalf("exit=%d stderr=%q", code, stderr)
	}
	assertNoClaimCall(t, src)
}
