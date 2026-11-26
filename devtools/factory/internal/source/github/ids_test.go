package github

import (
	"context"
	"slices"
	"strings"
	"testing"
)

func TestIDsQuery(t *testing.T) {
	for _, part := range []string{
		"query($owner:String!,$name:String!,$after:String)",
		"issues(first:100,after:$after,states:[OPEN,CLOSED])",
		"pageInfo{hasNextPage endCursor}",
		"nodes{number}",
	} {
		if !strings.Contains(idsQuery, part) {
			t.Fatalf("idsQuery missing %s\n%s", part, idsQuery)
		}
	}
	if strings.Contains(idsQuery, "since") {
		t.Fatalf("idsQuery filters by since: %s", idsQuery)
	}
	if strings.Count(idsQuery, "{") != strings.Count(idsQuery, "}") {
		t.Fatalf("idsQuery braces unbalanced: %s", idsQuery)
	}
}

func TestIDsTwoPages(t *testing.T) {
	const cursor = "cursor-page-2"
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, idsArgs("acme", "widgets", "")...).
		WillSucceed(issuesPage(t, true, cursor, map[string]any{"number": 7}, map[string]any{"number": 8}), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, idsArgs("acme", "widgets", cursor)...).
		WillSucceed(issuesPage(t, false, "", map[string]any{"number": 11}), 0).Once().Build()

	got, err := newTestSource(mock).IDs(context.Background())
	if err != nil {
		t.Fatalf("IDs() error = %v", err)
	}
	assertArgs(t, mock, [][]string{idsArgs("acme", "widgets", ""), idsArgs("acme", "widgets", cursor)})
	want := []string{"acme/widgets#7", "acme/widgets#8", "acme/widgets#11"}
	if !slices.Equal(got, want) {
		t.Fatalf("IDs() = %v, want %v", got, want)
	}
}

func TestIDsEmptyRepository(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, idsArgs("acme", "widgets", "")...).
		WillSucceed(issuesPage(t, false, ""), 0).Once().Build()

	got, err := newTestSource(mock).IDs(context.Background())
	if err != nil {
		t.Fatalf("IDs() error = %v", err)
	}
	if got == nil || len(got) != 0 {
		t.Fatalf("IDs() = %#v, want an empty non-nil slice", got)
	}
}

func TestIDsRejectsBadNumber(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, idsArgs("acme", "widgets", "")...).
		WillSucceed(issuesPage(t, false, "", map[string]any{"number": "seven"}), 0).Once().Build()

	_, err := newTestSource(mock).IDs(context.Background())
	if err == nil || !strings.Contains(err.Error(), "expected number integer") {
		t.Fatalf("IDs() error = %v", err)
	}
}

func TestIDsRepeatedCursor(t *testing.T) {
	const cursor = "same"
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, idsArgs("acme", "widgets", "")...).
		WillSucceed(issuesPage(t, true, cursor, map[string]any{"number": 1}), 0).Once().Build()
	mock.ExpectCommandWithArgs(testBin, idsArgs("acme", "widgets", cursor)...).
		WillSucceed(issuesPage(t, true, cursor, map[string]any{"number": 2}), 0).Once().Build()

	_, err := newTestSource(mock).IDs(context.Background())
	if err == nil || !strings.Contains(err.Error(), "expected a new cursor") {
		t.Fatalf("IDs() error = %v", err)
	}
}

func TestIDsCommandFailure(t *testing.T) {
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, idsArgs("acme", "widgets", "")...).
		WillFail("HTTP 401: Bad credentials", 1).Once().Build()

	_, err := newTestSource(mock).IDs(context.Background())
	if err == nil || !strings.Contains(err.Error(), "Bad credentials") {
		t.Fatalf("IDs() error = %v", err)
	}
}
