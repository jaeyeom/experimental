package doctor

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestTrustClassificationChecks(t *testing.T) {
	t.Parallel()

	t.Run("unset trust.environment warns for jira", func(t *testing.T) {
		t.Parallel()
		in := baseInput(nil)
		in.Config.Trust = config.Trust{}
		got := checkOnly(t, in, CheckTrustEnvironment)
		if len(got) != 1 {
			t.Fatalf("got %d findings, want 1: %#v", len(got), got)
		}
		f := got[0]
		if f.Severity != SeverityWarn {
			t.Fatalf("severity = %q, want warn", f.Severity)
		}
		if !strings.Contains(f.Message, "trust.environment is unset") || !strings.Contains(f.Suggestion, `"trusted"`) || !strings.Contains(f.Suggestion, `"untrusted"`) {
			t.Fatalf("unexpected finding: %+v", f)
		}
	})

	t.Run("unset trust.environment warns offline and notes public or private online", func(t *testing.T) {
		t.Parallel()
		inOffline := githubInput(nil)
		inOffline.Config.Trust = config.Trust{}
		gotOffline := checkOnly(t, inOffline, CheckTrustEnvironment)
		if len(gotOffline) != 1 || gotOffline[0].Severity != SeverityWarn {
			t.Fatalf("offline got = %#v", gotOffline)
		}

		inPublic := githubInput(nil)
		inPublic.Config.Trust = config.Trust{}
		inPublic.TrustAudit = &TrustAudit{VisibilityChecked: true, IsPrivate: false, Visibility: "PUBLIC"}
		gotPublic := checkOnly(t, inPublic, CheckTrustEnvironment)
		if len(gotPublic) != 1 || gotPublic[0].Severity != SeverityWarn || !strings.Contains(gotPublic[0].Message, "public repository") {
			t.Fatalf("public got = %#v", gotPublic)
		}

		inPrivate := githubInput(nil)
		inPrivate.Config.Trust = config.Trust{}
		inPrivate.TrustAudit = &TrustAudit{VisibilityChecked: true, IsPrivate: true, Visibility: "PRIVATE"}
		gotPrivate := checkOnly(t, inPrivate, CheckTrustEnvironment)
		if len(gotPrivate) != 1 || gotPrivate[0].Severity != SeverityWarn || !strings.Contains(gotPrivate[0].Message, "private repository") {
			t.Fatalf("private got = %#v", gotPrivate)
		}
	})

	t.Run("trusted jira and private github pass cleanly with no noise", func(t *testing.T) {
		t.Parallel()
		jiraIn := baseInput(map[string]source.Item{
			"EPIC-1": jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
			"PRJ-1":  jiraItem("PRJ-1", "Task", "To Do", "EPIC-1", false, []string{"agent-ready"}),
		})
		jiraIn.Config.Trust = config.Trust{Environment: config.TrustEnvTrusted}
		assertClean(t, jiraIn)

		ghIn := githubInput(map[string]source.Item{
			"acme/widgets#1": {ID: "acme/widgets#1", Type: "Issue", State: "OPEN", Labels: []string{"agent-ready"}},
		})
		ghIn.Config.Trust = config.Trust{Environment: config.TrustEnvTrusted}
		ghIn.TrustAudit = &TrustAudit{
			VisibilityChecked:    true,
			IsPrivate:            true,
			Visibility:           "PRIVATE",
			OwnerLogin:           "acme",
			OwnerType:            "Organization",
			CollaboratorsChecked: true,
			Collaborators: []CollaboratorPermission{
				{Login: "alice", RoleName: "admin", Admin: true, Maintain: true, Push: true, Triage: true},
			},
		}
		assertClean(t, ghIn)
	})
}

func TestTrustConfigThreatChecks(t *testing.T) {
	t.Parallel()

	t.Run("public github repo marked trusted emits warning", func(t *testing.T) {
		t.Parallel()
		in := githubInput(nil)
		in.Config.Trust = config.Trust{Environment: config.TrustEnvTrusted}
		in.TrustAudit = &TrustAudit{VisibilityChecked: true, IsPrivate: false, Visibility: "PUBLIC"}
		got := checkOnly(t, in, CheckTrustConfig)
		if len(got) != 1 || got[0].Severity != SeverityWarn || !strings.Contains(got[0].Message, `public GitHub repository marked trust.environment = "trusted"`) {
			t.Fatalf("got = %#v", got)
		}
	})

	t.Run("untrusted with OPEN or Backlog, empty guards, and broad associations warns", func(t *testing.T) {
		t.Parallel()
		in := githubInput(nil)
		in.Config.Trust = config.Trust{
			Environment:               config.TrustEnvUntrusted,
			UntrustedReadyStatuses:    []string{"Todo", "Backlog", "OPEN"},
			Guards:                    []string{},
			TrustedAuthorAssociations: []string{"OWNER", "CONTRIBUTOR", "NONE"},
		}
		got := checkOnly(t, in, CheckTrustConfig)
		if len(got) != 3 {
			t.Fatalf("got %d findings, want 3: %#v", len(got), got)
		}
		joined := FormatText(got)
		for _, want := range []string{
			"untrusted_ready_statuses includes Backlog, OPEN",
			"trust.guards is empty",
			"trusted_author_associations includes broad non-maintainer associations (CONTRIBUTOR, NONE)",
		} {
			if !strings.Contains(joined, want) {
				t.Fatalf("missing %q in:\n%s", want, joined)
			}
		}
	})

	t.Run("well-configured untrusted source passes cleanly", func(t *testing.T) {
		t.Parallel()
		in := githubInput(nil)
		in.Config.Trust = config.Trust{
			Environment:               config.TrustEnvUntrusted,
			UntrustedReadyStatuses:    []string{"Todo"},
			Guards:                    []string{config.TrustGuardApprovedHash},
			TrustedAuthorAssociations: []string{"OWNER"},
		}
		in.TrustAudit = &TrustAudit{
			VisibilityChecked:    true,
			IsPrivate:            false,
			Visibility:           "PUBLIC",
			OwnerLogin:           "alice",
			OwnerType:            "User",
			CollaboratorsChecked: true,
			Collaborators: []CollaboratorPermission{
				{Login: "alice", RoleName: "admin", Admin: true, Push: true, Triage: true},
			},
		}
		assertClean(t, in)
	})
}

func TestGitHubPermissionsAndAutomationChecks(t *testing.T) {
	t.Parallel()

	t.Run("warns on non-owner collaborators and teams with triage/write access", func(t *testing.T) {
		t.Parallel()
		in := githubInput(nil)
		in.Config.Trust = config.Trust{
			Environment:               config.TrustEnvUntrusted,
			UntrustedReadyStatuses:    []string{"Todo"},
			Guards:                    []string{config.TrustGuardLocked},
			TrustedAuthorAssociations: []string{"OWNER"},
		}
		in.TrustAudit = &TrustAudit{
			VisibilityChecked:    true,
			IsPrivate:            false,
			Visibility:           "PUBLIC",
			OwnerLogin:           "acme",
			OwnerType:            "Organization",
			CollaboratorsChecked: true,
			Collaborators: []CollaboratorPermission{
				{Login: "alice", RoleName: "admin", Admin: true, Push: true, Triage: true},
				{Login: "bob", RoleName: "triage", Triage: true},
				{Login: "carol", RoleName: "read"},
			},
			TeamsChecked: true,
			Teams: []TeamPermission{
				{Name: "Triage", Slug: "triage-team", Permission: "triage", Triage: true},
				{Name: "Readers", Slug: "readers", Permission: "pull"},
			},
		}
		got := checkOnly(t, in, CheckGitHubPermissions)
		if len(got) != 1 || got[0].Severity != SeverityWarn {
			t.Fatalf("got = %#v", got)
		}
		if !strings.Contains(got[0].Message, "bob (triage)") || !strings.Contains(got[0].Message, "team:triage-team (triage)") {
			t.Fatalf("unexpected message: %+v", got[0])
		}
		if strings.Contains(got[0].Message, "alice") || strings.Contains(got[0].Message, "carol") || strings.Contains(got[0].Message, "readers") {
			t.Fatalf("unexpected actor in message: %+v", got[0])
		}
	})

	t.Run("degrades gracefully with info when collaborator lookup skipped", func(t *testing.T) {
		t.Parallel()
		in := githubInput(nil)
		in.Config.Trust = config.Trust{
			Environment:               config.TrustEnvUntrusted,
			UntrustedReadyStatuses:    []string{"Todo"},
			Guards:                    []string{config.TrustGuardLocked},
			TrustedAuthorAssociations: []string{"OWNER"},
		}
		in.TrustAudit = &TrustAudit{
			VisibilityChecked:       true,
			IsPrivate:               false,
			Visibility:              "PUBLIC",
			CollaboratorsSkipReason: "insufficient token scope or permission (403)",
		}
		got := checkOnly(t, in, CheckGitHubPermissions)
		if len(got) != 1 || got[0].Severity != SeverityInfo || !strings.Contains(got[0].Message, "403") {
			t.Fatalf("got = %#v", got)
		}
	})

	t.Run("detects risky issue templates and workflows and ignores safe files", func(t *testing.T) {
		t.Parallel()
		in := githubInput(nil)
		in.Config.Trust = config.Trust{
			Environment:               config.TrustEnvUntrusted,
			UntrustedReadyStatuses:    []string{"Todo"},
			Guards:                    []string{config.TrustGuardLocked},
			TrustedAuthorAssociations: []string{"OWNER"},
		}
		in.TrustAudit = &TrustAudit{
			RepoFiles: []RepoFile{
				{
					Path:    ".github/ISSUE_TEMPLATE/config.yml",
					Content: "blank_issues_enabled: true\n",
				},
				{
					Path:    ".github/ISSUE_TEMPLATE/bug.md",
					Content: "---\nname: Bug\nlabels: [bug, agent-ready]\n---\nMentioning Todo in body should not trigger.\n",
				},
				{
					Path:    ".github/ISSUE_TEMPLATE/safe.md",
					Content: "---\nname: Question\nlabels: [question]\n---\nPlease wait for Todo triage.\n",
				},
				{
					Path:    ".github/ISSUE_TEMPLATE/form.yml",
					Content: "name: Feature\nlabels:\n  - enhancement\n  - agent-ready\n",
				},
				{
					Path:    ".github/workflows/ci.yml",
					Content: "on:\n  push:\n    branches: [main]\njobs:\n  test:\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo agent-ready Todo\n",
				},
				{
					Path:    ".github/workflows/closed-only.yml",
					Content: "on:\n  issues:\n    types: [closed]\njobs:\n  cleanup:\n    runs-on: ubuntu-latest\n    steps:\n      - run: gh issue edit 1 --add-label agent-ready\n",
				},
				{
					Path:    ".github/workflows/welcome.yml",
					Content: "on:\n  issues:\n    types: [opened]\njobs:\n  greet:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: actions/github-script@v7\n        with:\n          script: github.rest.issues.createComment({body: 'Thanks!'})\n",
				},
				{
					Path:    ".github/workflows/auto-label.yml",
					Content: "on:\n  issues:\n    types: [opened]\njobs:\n  triage:\n    runs-on: ubuntu-latest\n    steps:\n      - run: gh issue edit \"$URL\" --add-label \"agent-ready\"\n",
				},
				{
					Path:    ".github/workflows/auto-status.yml",
					Content: "on: issues\njobs:\n  triage:\n    runs-on: ubuntu-latest\n    steps:\n      - run: |\n          gh api graphql -f query='mutation { updateIssue(input: {id: $id, planningStatusId: \"Todo\"}) { issue { id } } }'\n",
				},
			},
		}
		got := checkOnly(t, in, CheckGitHubAutomation)
		if len(got) != 4 {
			t.Fatalf("got %d findings, want 4: %#v", len(got), got)
		}
		paths := make([]string, 0, len(got))
		for _, f := range got {
			if f.Severity != SeverityWarn {
				t.Fatalf("severity = %q, want warn: %+v", f.Severity, f)
			}
			paths = append(paths, f.Items[0])
		}
		wantPaths := []string{
			".github/ISSUE_TEMPLATE/bug.md",
			".github/ISSUE_TEMPLATE/form.yml",
			".github/workflows/auto-label.yml",
			".github/workflows/auto-status.yml",
		}
		for i, want := range wantPaths {
			if paths[i] != want {
				t.Fatalf("paths = %v, want %v", paths, wantPaths)
			}
			if !strings.Contains(got[i].Message, want) {
				t.Fatalf("finding %d message missing path %q: %+v", i, want, got[i])
			}
		}
	})
}
