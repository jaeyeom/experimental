package gh

import (
	"context"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestClientIssueFields(t *testing.T) {
	t.Run("decodes starter fields and skips unsupported field types", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		payload := `{"data":{"repository":{"issueFields":{"nodes":[` +
			`{"__typename":"IssueFieldSingleSelect","id":"IFSS_1","name":"Priority","options":[{"id":"OPT_0","name":"Urgent"},{"id":"OPT_1","name":"High"}]},` +
			`{"__typename":"IssueFieldDate","id":"IFD_1","name":"Start date"},` +
			`{"__typename":"IssueFieldDate","id":"IFD_2","name":"Target date"},` +
			`{"__typename":"IssueFieldText"}` +
			`]}}}}`
		mock.ExpectCommandWithArgs("gh",
			"api", "graphql",
			"-f", "query="+issueFieldsQuery,
			"-f", "owner=acme",
			"-f", "name=widgets",
		).WillSucceed(payload, 0).Once().Build()

		got, err := NewClient(mock, "gh").IssueFields(context.Background(), "acme/widgets")
		if err != nil {
			t.Fatalf("IssueFields() error = %v", err)
		}
		if len(got) != 3 {
			t.Fatalf("len = %d, want 3 (%+v)", len(got), got)
		}
		if got[0].Name != "Priority" || got[0].TypeName != IssueFieldTypeSingleSelect || len(got[0].Options) != 2 || got[0].Options[0].Name != "Urgent" {
			t.Fatalf("got[0] = %+v", got[0])
		}
		if got[1].Name != "Start date" || got[1].TypeName != IssueFieldTypeDate {
			t.Fatalf("got[1] = %+v", got[1])
		}
		if got[2].Name != "Target date" || got[2].TypeName != IssueFieldTypeDate {
			t.Fatalf("got[2] = %+v", got[2])
		}
	})

	t.Run("rejects graphql error", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("gh",
			"api", "graphql",
			"-f", "query="+issueFieldsQuery,
			"-f", "owner=acme",
			"-f", "name=widgets",
		).WillSucceed(`{"errors":[{"message":"Field 'issueFields' doesn't exist"}]}`, 0).Once().Build()

		if _, err := NewClient(mock, "gh").IssueFields(context.Background(), "acme/widgets"); err == nil || !strings.Contains(err.Error(), "issueFields") {
			t.Fatalf("error = %v", err)
		}
	})

	t.Run("rejects bad repo", func(t *testing.T) {
		if _, err := NewClient(executor.NewMockExecutor(), "gh").IssueFields(context.Background(), "nope"); err == nil || !strings.Contains(err.Error(), "expected repository owner/repo") {
			t.Fatalf("error = %v", err)
		}
	})
}
