package gh

import (
	"context"
	"strings"
	"testing"
)

func TestView(t *testing.T) {
	t.Parallel()

	const body = `{
		"isDraft": true,
		"title": "[PRJ-1] ship",
		"body": "## Summary\n\nshipped\n",
		"url": "https://github.com/acme/widgets/pull/9",
		"baseRefName": "main",
		"labels": [{"name": "bug"}],
		"reviews": [{"author": {"login": "ada"}, "state": "COMMENTED"}],
		"reviewRequests": [{"login": "octo"}]
	}`
	mock := newGHMock()
	args := []string{"pr", "view", "9", "--repo", "acme/widgets", "--json", viewFields}
	mock.ExpectCommandWithArgs(testGHBin, args...).WillSucceed(body, 0).Once().Build()
	got, err := NewClient(mock, testGHBin).View(context.Background(), "acme/widgets", 9)
	if err != nil {
		t.Fatal(err)
	}
	if !got.Draft || got.Title != "[PRJ-1] ship" || got.Base != "main" || got.Body == "" {
		t.Fatalf("view = %+v", got)
	}
	if strings.Join(got.Labels, ",") != "bug" || strings.Join(got.Reviewers, ",") != "ada,octo" {
		t.Fatalf("labels = %#v reviewers = %#v", got.Labels, got.Reviewers)
	}
	if _, err := NewClient(mock, testGHBin).View(context.Background(), "acme/widgets", 0); err == nil || !strings.Contains(err.Error(), "found 0") {
		t.Fatalf("number error = %v", err)
	}
}
