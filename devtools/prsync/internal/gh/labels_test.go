package gh

import (
	"context"
	"slices"
	"strings"
	"testing"
)

func TestOpenPRQueriesAskForLabelsOnlyWithLabels(t *testing.T) {
	t.Parallel()

	for _, query := range []string{openPRSearchQuery, openPRViewQuery} {
		if strings.Contains(query, "labels(") {
			t.Fatalf("default query asks for labels: %s", query)
		}
	}
	for _, query := range []string{openPRSearchQueryFor(true), openPRViewQueryFor(true)} {
		if !strings.Contains(query, labelSelection) {
			t.Fatalf("query lacks labels: %s", query)
		}
	}
}

func TestListOpenPRsWithLabels(t *testing.T) {
	t.Parallel()

	body := openPRSearchJSON(`{
		"number":123,
		"state":"OPEN",
		"headRefOid":"abc123",
		"labels":{"nodes":[{"name":"needs-linux"},{"name":"bug"}]}
	}`)
	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, openPRSearchArgsFor(openPRSearchQueryFor(true), "acme/widgets", "alice", openPRSearchPage, "")...).
		WillSucceed(body, 0).Build()
	expectStatusChecks(mock, 123, `{"statusCheckRollup":[]}`).Once().Build()
	got, err := NewClient(mock, testGHBin).WithLabels().ListOpenPRs(context.Background(), "acme/widgets", "alice")
	if err != nil {
		t.Fatalf("ListOpenPRs() error = %v", err)
	}
	if len(got) != 1 || !slices.Equal(got[0].Labels, []string{"needs-linux", "bug"}) {
		t.Fatalf("labels = %+v", got)
	}
}
