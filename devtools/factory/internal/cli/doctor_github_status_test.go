package cli

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const doctorGitHubStatuses = `{"data":{"repository":{"planningStatuses":{"nodes":[` +
	`{"id":"IS_kgAC","name":"Backlog","category":{"name":"Backlog","state":"OPEN"}},` +
	`{"id":"IS_kgAD","name":"Todo","category":{"name":"Planned","state":"OPEN"}},` +
	`{"id":"IS_kgAE","name":"In progress","category":{"name":"Started","state":"OPEN"}},` +
	`{"id":"IS_kgAI","name":"In review","category":{"name":"Started","state":"OPEN"}},` +
	`{"id":"IS_kgAF","name":"Done","category":{"name":"Completed","state":"CLOSED"}},` +
	`{"id":"IS_kgAG","name":"Not planned","category":{"name":"Not planned","state":"CLOSED"}},` +
	`{"id":"IS_kgAH","name":"Duplicate","category":{"name":"Duplicate","state":"CLOSED"}}` +
	`]}}}}`

const doctorGitHubFields = `{"data":{"repository":{"issueFields":{"nodes":[` +
	`{"__typename":"IssueFieldSingleSelect","id":"IFSS_1","name":"Priority","options":[{"id":"OPT_0","name":"Urgent"},{"id":"OPT_1","name":"High"},{"id":"OPT_2","name":"Medium"},{"id":"OPT_3","name":"Low"}]},` +
	`{"__typename":"IssueFieldDate","id":"IFD_1","name":"Start date"},` +
	`{"__typename":"IssueFieldDate","id":"IFD_2","name":"Target date"}` +
	`]}}}}`

func writeGitHubDoctorConfig(t *testing.T, extra string) string {
	t.Helper()
	dir := t.TempDir()
	cfgPath := filepath.Join(dir, "factory.toml")
	content := fmt.Sprintf(`[[source]]
name           = "widgets"
kind           = "github"
repo           = "acme/widgets"
cwd            = %q
base           = "main"
require_labels = ["agent-ready"]
start_prompt   = "Fix {id}"
%s
[source.trust]
environment    = "trusted"
`, dir, extra)
	if err := os.WriteFile(cfgPath, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	return cfgPath
}

func expectGitHubTrustClean(mock *executor.MockExecutor) {
	mock.ExpectCommand("gh").WillSucceed(`{"data":{"repository":{"isPrivate":true,"visibility":"PRIVATE","owner":{"login":"acme","__typename":"Organization"}}}}`, 0).Once().Build()
	mock.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/collaborators?per_page=100").
		WillSucceed(`[{"login":"alice","role_name":"admin","permissions":{"admin":true,"maintain":true,"push":true,"triage":true,"pull":true}}]`, 0).Once().Build()
	mock.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/teams?per_page=100").
		WillSucceed(`[]`, 0).Once().Build()
}

func seedGitHubDoctorDB(t *testing.T, items ...source.Item) string {
	t.Helper()
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if len(items) > 0 {
			if err := st.UpsertItems(ctx, "github:widgets", items); err != nil {
				t.Fatal(err)
			}
		}
		if err := st.SetSyncState(ctx, "github:widgets", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "github:widgets", "alice"); err != nil {
			t.Fatal(err)
		}
	})
	return dbPath
}

func TestDoctorGitHubStatus(t *testing.T) {
	t.Run("native statuses supported passes", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubDoctorConfig(t, "")
		dbPath := seedGitHubDoctorDB(t,
			rankItem("github:widgets", "acme/widgets#1", "ready", "Issue", "Todo", "", "", 2, false, day1, []string{"agent-ready"}),
		)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubStatuses, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubFields, 0).Once().Build()
		expectGitHubTrustClean(mock)
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != ExitOK || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "ok    github-status") || !strings.Contains(stdout, "gh 2.105.0; native issue statuses supported (acme/widgets)") {
			t.Fatalf("stdout:\n%s", stdout)
		}
		if !strings.Contains(stdout, "ok    github-fields") || !strings.Contains(stdout, "native issue fields available (acme/widgets: Priority, Start date, Target date)") {
			t.Fatalf("stdout:\n%s", stdout)
		}
	})

	t.Run("legacy claim_label on status-capable repo warns to migrate", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubDoctorConfig(t, `claim_label = "in-progress"`)
		dbPath := seedGitHubDoctorDB(t)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubStatuses, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubFields, 0).Once().Build()
		expectGitHubTrustClean(mock)
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "warn  github-status") || !strings.Contains(stdout, `uses claim_label "in-progress"`) || !strings.Contains(stdout, "remove claim_label") {
			t.Fatalf("stdout:\n%s", stdout)
		}
	})

	t.Run("legacy priority_labels and defer_label_prefix warn when native fields exist", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubDoctorConfig(t, "priority_labels = { P0 = 0 }\ndefer_label_prefix = \"not-before:\"\n")
		dbPath := seedGitHubDoctorDB(t)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubStatuses, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubFields, 0).Once().Build()
		expectGitHubTrustClean(mock)
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		for _, want := range []string{
			"warn  github-fields",
			`uses priority_labels, but native issue field "Priority" is available`,
			`uses defer_label_prefix "not-before:", but native issue field "Start date" is available`,
			"remove priority_labels",
			"remove defer_label_prefix",
		} {
			if !strings.Contains(stdout, want) {
				t.Fatalf("missing %q\n%s", want, stdout)
			}
		}
	})

	t.Run("old gh version fails", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubDoctorConfig(t, "")
		dbPath := seedGitHubDoctorDB(t)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 1.14.0 (2021-08-01)\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != 2 || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "fail  github-status") || !strings.Contains(stdout, "1.14.0 is older than 2.0.0") {
			t.Fatalf("stdout:\n%s", stdout)
		}
	})

	t.Run("offline checks gh version and skips live repo check", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubDoctorConfig(t, "")
		dbPath := seedGitHubDoctorDB(t)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--offline", "--config", cfgPath, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "warn  github-status") || !strings.Contains(stdout, "gh 2.105.0; repo status and field checks skipped (--offline)") {
			t.Fatalf("stdout:\n%s", stdout)
		}
	})

	t.Run("unavailable native statuses fails with claim_label hint", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubDoctorConfig(t, "")
		dbPath := seedGitHubDoctorDB(t)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(`{"data":{"repository":{"planningStatuses":{"nodes":[]}}}}`, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(`{"data":{"repository":{"issueFields":{"nodes":[]}}}}`, 0).Once().Build()
		expectGitHubTrustClean(mock)
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != 2 || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "fail  github-status") || !strings.Contains(stdout, "native issue statuses unavailable") || !strings.Contains(stdout, `set claim_label = "in-progress"`) {
			t.Fatalf("stdout:\n%s", stdout)
		}
	})

	t.Run("invalid claim_status fails listing available statuses", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubDoctorConfig(t, `claim_status = "Doing"`)
		dbPath := seedGitHubDoctorDB(t)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubStatuses, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubFields, 0).Once().Build()
		expectGitHubTrustClean(mock)
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != 2 || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "fail  github-status") || !strings.Contains(stdout, `claim_status "Doing" not in repository statuses`) || !strings.Contains(stdout, "In progress") {
			t.Fatalf("stdout:\n%s", stdout)
		}
	})

	t.Run("detects dropped claim on GitHub native status", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubDoctorConfig(t, "")
		dbPath := seedGitHubDoctorDB(t,
			rankItem("github:widgets", "acme/widgets#42", "dropped", "Issue", "In progress", "", "alice", 2, false, day1, []string{"agent-ready"}),
		)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubStatuses, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubFields, 0).Once().Build()
		expectGitHubTrustClean(mock)
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "warn  dropped-claim") || !strings.Contains(stdout, "acme/widgets#42 is In progress with no tab and no open PR") {
			t.Fatalf("stdout:\n%s", stdout)
		}
	})
}
