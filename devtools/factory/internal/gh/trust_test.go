package gh

import (
	"context"
	"errors"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestClientRepoTrustInfo(t *testing.T) {
	t.Run("decodes public and private repository metadata", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("gh",
			"api", "graphql",
			"-f", "query="+repoTrustQuery,
			"-f", "owner=acme",
			"-f", "name=widgets",
		).WillSucceed(`{"data":{"repository":{"isPrivate":false,"visibility":"PUBLIC","owner":{"login":"acme","__typename":"Organization"}}}}`, 0).Once().Build()

		got, err := NewClient(mock, "gh").RepoTrustInfo(context.Background(), "acme/widgets")
		if err != nil {
			t.Fatalf("RepoTrustInfo() error = %v", err)
		}
		if got.IsPrivate || got.Visibility != VisibilityPublic || got.OwnerLogin != "acme" || got.OwnerType != "Organization" {
			t.Fatalf("got = %+v", got)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("derives visibility when optional fields omitted", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommand("gh").WillSucceed(`{"data":{"repository":{"isPrivate":true}}}`, 0).Once().Build()

		got, err := NewClient(mock, "gh").RepoTrustInfo(context.Background(), "alice/private-repo")
		if err != nil {
			t.Fatalf("RepoTrustInfo() error = %v", err)
		}
		if !got.IsPrivate || got.Visibility != VisibilityPrivate {
			t.Fatalf("got = %+v", got)
		}
	})

	t.Run("rejects invalid repo and graphql errors", func(t *testing.T) {
		if _, err := NewClient(executor.NewMockExecutor(), "gh").RepoTrustInfo(context.Background(), "bad"); err == nil || !strings.Contains(err.Error(), "expected repository owner/repo") {
			t.Fatalf("err = %v", err)
		}
		mock := executor.NewMockExecutor()
		mock.ExpectCommand("gh").WillSucceed(`{"errors":[{"message":"Could not resolve to a Repository"}]}`, 0).Once().Build()
		if _, err := NewClient(mock, "gh").RepoTrustInfo(context.Background(), "acme/missing"); err == nil || !strings.Contains(err.Error(), "Could not resolve") {
			t.Fatalf("err = %v", err)
		}
	})
}

func TestClientCollaborators(t *testing.T) {
	t.Run("decodes collaborator permissions", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		payload := `[` +
			`{"login":"alice","role_name":"admin","permissions":{"admin":true,"maintain":true,"push":true,"triage":true,"pull":true}},` +
			`{"login":"bob","role_name":"triage","permissions":{"admin":false,"maintain":false,"push":false,"triage":true,"pull":true}}` +
			`]`
		mock.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/collaborators?per_page=100").
			WillSucceed(payload, 0).Once().Build()

		got, err := NewClient(mock, "gh").Collaborators(context.Background(), "acme/widgets")
		if err != nil {
			t.Fatalf("Collaborators() error = %v", err)
		}
		if len(got) != 2 {
			t.Fatalf("len = %d, want 2", len(got))
		}
		if got[0].Login != "alice" || !got[0].Admin || got[1].Login != "bob" || !got[1].Triage || got[1].Push {
			t.Fatalf("got = %+v", got)
		}
	})

	t.Run("wraps 403 and insufficient scope as ErrPermissionDenied", func(t *testing.T) {
		for _, msg := range []string{
			"gh: Must have push access to view repository collaborators. (HTTP 403)",
			"gh: Resource not accessible by integration (HTTP 403)",
			"insufficient token scope",
		} {
			mock := executor.NewMockExecutor()
			mock.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/collaborators?per_page=100").
				WillFail(msg, 1).Once().Build()
			_, err := NewClient(mock, "gh").Collaborators(context.Background(), "acme/widgets")
			if !errors.Is(err, ErrPermissionDenied) {
				t.Fatalf("msg=%q err=%v, want ErrPermissionDenied", msg, err)
			}
		}
	})
}

func TestClientRepoTeams(t *testing.T) {
	t.Run("decodes team permissions", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		payload := `[{"name":"Triage Team","slug":"triage-team","permission":"triage","permissions":{"admin":false,"maintain":false,"push":false,"triage":true,"pull":true}}]`
		mock.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/teams?per_page=100").
			WillSucceed(payload, 0).Once().Build()

		got, err := NewClient(mock, "gh").RepoTeams(context.Background(), "acme/widgets")
		if err != nil {
			t.Fatalf("RepoTeams() error = %v", err)
		}
		if len(got) != 1 || got[0].Slug != "triage-team" || !got[0].Triage || got[0].Push {
			t.Fatalf("got = %+v", got)
		}
	})

	t.Run("wraps 403 as ErrPermissionDenied", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("gh", "api", "repos/acme/widgets/teams?per_page=100").
			WillFail("HTTP 403: Forbidden", 1).Once().Build()
		if _, err := NewClient(mock, "gh").RepoTeams(context.Background(), "acme/widgets"); !errors.Is(err, ErrPermissionDenied) {
			t.Fatalf("err = %v, want ErrPermissionDenied", err)
		}
	})
}
