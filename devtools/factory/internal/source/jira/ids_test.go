package jira

import (
	"context"
	"net/http"
	"reflect"
	"strings"
	"testing"
)

func TestIDsPaginatesKeysOnly(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {
			{status: http.StatusOK, body: []byte(`{"issues":[{"key":"PRJ-1"},{"key":"PRJ-2"}],"isLast":false,"nextPageToken":"page-2"}`)},
			{status: http.StatusOK, body: []byte(`{"issues":[{"key":"PRJ-3"},{"key":"PRJ-2"}],"isLast":true}`)},
		},
	})
	src, _ := fake.open(testJQL)

	got, err := src.IDs(context.Background())
	if err != nil {
		t.Fatalf("IDs() error = %v", err)
	}
	if want := []string{"PRJ-1", "PRJ-2", "PRJ-3"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("IDs() = %v, want %v", got, want)
	}
	recs := fake.requests()
	assertAuth(t, recs)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	first := decodePost(t, recs[0])
	second := decodePost(t, recs[1])
	wantJQL := "(" + testJQL + ") ORDER BY updated ASC"
	if first.JQL != wantJQL || second.JQL != wantJQL {
		t.Fatalf("JQL = %q and %q, want the full-sync JQL %q", first.JQL, second.JQL, wantJQL)
	}
	if !reflect.DeepEqual(first.Fields, []string{"key"}) || !reflect.DeepEqual(second.Fields, []string{"key"}) {
		t.Fatalf("fields = %v and %v, want [key]", first.Fields, second.Fields)
	}
	if first.NextPageToken != nil {
		t.Fatalf("first nextPageToken = %q, want omitted", *first.NextPageToken)
	}
	if second.NextPageToken == nil || *second.NextPageToken != "page-2" {
		t.Fatalf("second nextPageToken = %v, want page-2", second.NextPageToken)
	}
}

func TestIDsEmptyResult(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {
			{status: http.StatusOK, body: []byte(`{"issues":[],"isLast":true}`)},
		},
	})
	src, _ := fake.open(testJQL)

	got, err := src.IDs(context.Background())
	if err != nil {
		t.Fatalf("IDs() error = %v", err)
	}
	if got == nil || len(got) != 0 {
		t.Fatalf("IDs() = %#v, want an empty non-nil slice", got)
	}
}

func TestIDsRejectsMissingKey(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {
			{status: http.StatusOK, body: []byte(`{"issues":[{"id":"10001"}],"isLast":true}`)},
		},
	})
	src, _ := fake.open(testJQL)

	_, err := src.IDs(context.Background())
	if err == nil || !strings.Contains(err.Error(), "expected an issue key, found empty") {
		t.Fatalf("IDs() error = %v", err)
	}
}

func TestIDsHTTPError(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {
			{status: http.StatusBadRequest, body: []byte(`{"errorMessages":["bad jql"]}`)},
		},
	})
	src, _ := fake.open(testJQL)

	_, err := src.IDs(context.Background())
	if err == nil || !strings.Contains(err.Error(), "400") {
		t.Fatalf("IDs() error = %v, want the HTTP status", err)
	}
}

func TestIDsRequiresIssues(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {okFixture(t, "search_no_issues.json")},
	})
	src, _ := fake.open(testJQL)

	if _, err := src.IDs(context.Background()); err == nil || !strings.Contains(err.Error(), "expected issues") {
		t.Fatalf("IDs() error = %v", err)
	}
}
