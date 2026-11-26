package cli

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/doctor"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func writeGitHubTrustConfig(t *testing.T, cwd, extra string) string {
	t.Helper()
	dir := t.TempDir()
	if cwd == "" {
		cwd = dir
	}
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
`, cwd, extra)
	if err := os.WriteFile(cfgPath, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	return cfgPath
}

func TestDoctorTrustClassificationAndThreats(t *testing.T) {
	t.Run("unclassified github source warns offline and notes public repo online", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubTrustConfig(t, "", "")
		dbPath := seedGitHubDoctorDB(t)

		mockOffline := executor.NewMockExecutor()
		mockOffline.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mockOffline.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mockOffline.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mockOffline.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mockOffline.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mockOffline)

		stdout, stderr, code := runCmd(t, "doctor", "--offline", "--config", cfgPath, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("offline code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "warn  trust-environment") || !strings.Contains(stdout, "trust.environment is unset") {
			t.Fatalf("offline stdout:\n%s", stdout)
		}

		mockOnline := executor.NewMockExecutor()
		mockOnline.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mockOnline.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mockOnline.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mockOnline.ExpectCommand("gh").WillSucceed(doctorGitHubStatuses, 0).Once().Build()
		mockOnline.ExpectCommand("gh").WillSucceed(doctorGitHubFields, 0).Once().Build()
		mockOnline.ExpectCommand("gh").WillSucceed(`{"data":{"repository":{"isPrivate":false,"visibility":"PUBLIC","owner":{"login":"acme","__typename":"Organization"}}}}`, 0).Once().Build()
		mockOnline.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/collaborators?per_page=100").
			WillSucceed(`[{"login":"alice","role_name":"admin","permissions":{"admin":true,"maintain":true,"push":true,"triage":true,"pull":true}}]`, 0).Once().Build()
		mockOnline.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/teams?per_page=100").
			WillSucceed(`[]`, 0).Once().Build()
		mockOnline.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mockOnline.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mockOnline.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mockOnline)

		stdout, stderr, code = runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("online code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "warn  trust-environment") || !strings.Contains(stdout, "public repository and trust.environment is unset") {
			t.Fatalf("online stdout:\n%s", stdout)
		}
		if err := mockOnline.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("public repo marked trusted and risky untrusted config emit warnings", func(t *testing.T) {
		doctorToolOK(t)
		cfgTrusted := writeGitHubTrustConfig(t, "", "[source.trust]\nenvironment = \"trusted\"\n")
		dbPath := seedGitHubDoctorDB(t)

		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubStatuses, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubFields, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(`{"data":{"repository":{"isPrivate":false,"visibility":"PUBLIC","owner":{"login":"alice","__typename":"User"}}}}`, 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/collaborators?per_page=100").
			WillSucceed(`[{"login":"alice","role_name":"admin","permissions":{"admin":true,"maintain":true,"push":true,"triage":true,"pull":true}}]`, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgTrusted, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "warn  trust-config") || !strings.Contains(stdout, `public GitHub repository marked trust.environment = "trusted"`) {
			t.Fatalf("stdout:\n%s", stdout)
		}

		cfgRisky := writeGitHubTrustConfig(t, "", `
[source.trust]
environment = "untrusted"
untrusted_ready_statuses = ["OPEN", "Backlog"]
guards = []
trusted_author_associations = ["OWNER", "CONTRIBUTOR"]
`)
		mock2 := executor.NewMockExecutor()
		mock2.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock2.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock2.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock2.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock2.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock2)

		stdout, stderr, code = runCmd(t, "doctor", "--offline", "--json", "--config", cfgRisky, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("risky code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		var out toolcli.DoctorOutput
		if err := json.Unmarshal([]byte(stdout), &out); err != nil {
			t.Fatalf("json: %v\n%s", err, stdout)
		}
		var trustWarnings int
		for _, c := range out.Checks {
			if c.Name == doctor.CheckTrustConfig && c.Status == toolcli.StatusWarn {
				trustWarnings++
			}
		}
		if trustWarnings != 3 {
			t.Fatalf("trustWarnings = %d, want 3 in %+v", trustWarnings, out.Checks)
		}
	})

	t.Run("local checkout scan detects risky template and workflow and passes when clean", func(t *testing.T) {
		doctorToolOK(t)
		cwd := t.TempDir()
		tmplDir := filepath.Join(cwd, ".github", "ISSUE_TEMPLATE")
		wfDir := filepath.Join(cwd, ".github", "workflows")
		if err := os.MkdirAll(tmplDir, 0o750); err != nil {
			t.Fatal(err)
		}
		if err := os.MkdirAll(wfDir, 0o750); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(tmplDir, "bug.yml"), []byte("name: Bug\nlabels: [bug, agent-ready]\n"), 0o600); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(wfDir, "triage.yml"), []byte("on:\n  issues:\n    types: [opened]\njobs:\n  triage:\n    runs-on: ubuntu-latest\n    steps:\n      - run: gh issue edit 1 --add-label agent-ready\n"), 0o600); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(wfDir, "ci.yml"), []byte("on: push\njobs:\n  test:\n    runs-on: ubuntu-latest\n    steps:\n      - run: make check\n"), 0o600); err != nil {
			t.Fatal(err)
		}

		cfgPath := writeGitHubTrustConfig(t, cwd, "[source.trust]\nenvironment = \"untrusted\"\n")
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
		for _, want := range []string{
			"warn  github-automation",
			".github/ISSUE_TEMPLATE/bug.yml",
			".github/workflows/triage.yml",
		} {
			if !strings.Contains(stdout, want) {
				t.Fatalf("missing %q in:\n%s", want, stdout)
			}
		}
		if strings.Contains(stdout, ".github/workflows/ci.yml") {
			t.Fatalf("false positive on ci.yml:\n%s", stdout)
		}
	})

	t.Run("graceful degradation on 403 insufficient scope collaborator lookup", func(t *testing.T) {
		doctorToolOK(t)
		cfgPath := writeGitHubTrustConfig(t, "", "[source.trust]\nenvironment = \"untrusted\"\n")
		dbPath := seedGitHubDoctorDB(t)

		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh", "auth", "status").WillSucceed("ok\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "--version").WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubStatuses, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(doctorGitHubFields, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(`{"data":{"repository":{"isPrivate":false,"visibility":"PUBLIC","owner":{"login":"acme","__typename":"User"}}}}`, 0).Once().Build()
		mock.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/collaborators?per_page=100").
			WillFail("Must have push access to view repository collaborators. (HTTP 403)", 1).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		useExec(t, mock)

		stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
		if code != ExitOK || stderr != "" {
			t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "ok    github-permissions") || !strings.Contains(stdout, "403") {
			t.Fatalf("stdout:\n%s", stdout)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("unset jira environment warns and trusted jira passes cleanly", func(t *testing.T) {
		doctorToolOK(t)
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedDB(t, dbPath, func(st *store.Store) {
			ctx := t.Context()
			if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
				t.Fatal(err)
			}
			if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
				t.Fatal(err)
			}
		})

		unsetCfg := writeNextConfig(t, nextSource{name: "acme"})
		useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
		stdout, stderr, code := runCmd(t, "doctor", "--config", unsetCfg, "--db", dbPath)
		if code != 1 || stderr != "" {
			t.Fatalf("unset code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if !strings.Contains(stdout, "warn  trust-environment") || !strings.Contains(stdout, "trust.environment is unset") {
			t.Fatalf("unset stdout:\n%s", stdout)
		}

		trustedCfg := writeDoctorConfig(t, nextSource{name: "acme"})
		useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
		stdout, stderr, code = runCmd(t, "doctor", "--config", trustedCfg, "--db", dbPath)
		if code != ExitOK || stderr != "" {
			t.Fatalf("trusted code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
		}
		if strings.Contains(stdout, "trust-") {
			t.Fatalf("unexpected trust finding on trusted jira:\n%s", stdout)
		}
	})
}
