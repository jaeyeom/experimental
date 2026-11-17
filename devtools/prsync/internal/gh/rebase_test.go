package gh

import (
	"context"
	"errors"
	"strings"
	"testing"
)

func TestRebaseBranchUsesTheAPI(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, prIDArgs("acme", "widgets", 12)...).
		WillSucceed(`{"data":{"repository":{"pullRequest":{"id":"PR_1"}}}}`, 0).Build()
	mock.ExpectCommandWithArgs(testGHBin, rebaseBranchArgs("PR_1", "abc123")...).
		WillSucceed(`{"data":{"updatePullRequestBranch":{"pullRequest":{"headRefOid":"def456"}}}}`, 0).Build()

	got, err := NewClient(mock, testGHBin).RebaseBranch(context.Background(), "acme/widgets", 12, "abc123")
	if err != nil {
		t.Fatalf("RebaseBranch() error = %v", err)
	}
	if got != "def456" {
		t.Fatalf("head = %q, want def456", got)
	}
	if !strings.Contains(updateBranchMutation, "updateMethod:REBASE") || !strings.Contains(updateBranchMutation, "expectedHeadOid:$head") {
		t.Fatalf("mutation = %s", updateBranchMutation)
	}
}

func TestRebaseBranchConflict(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, prIDArgs("acme", "widgets", 12)...).
		WillSucceed(`{"data":{"repository":{"pullRequest":{"id":"PR_1"}}}}`, 0).Build()
	mock.ExpectCommandWithArgs(testGHBin, rebaseBranchArgs("PR_1", "abc123")...).
		WillSucceed(`{"data":{"updatePullRequestBranch":null},"errors":[{"message":"Rebase failed: merge conflict between base and head"}]}`, 1).Build()

	_, err := NewClient(mock, testGHBin).RebaseBranch(context.Background(), "acme/widgets", 12, "abc123")
	if !errors.Is(err, ErrConflict) {
		t.Fatalf("error = %v, want ErrConflict", err)
	}
}

func TestRebaseBranchOtherErrorIsNotAConflict(t *testing.T) {
	t.Parallel()

	mock := newGHMock()
	mock.ExpectCommandWithArgs(testGHBin, prIDArgs("acme", "widgets", 12)...).
		WillSucceed(`{"data":{"repository":{"pullRequest":{"id":"PR_1"}}}}`, 0).Build()
	mock.ExpectCommandWithArgs(testGHBin, rebaseBranchArgs("PR_1", "abc123")...).
		WillFail("HTTP 502: Bad Gateway", 1).Build()

	_, err := NewClient(mock, testGHBin).RebaseBranch(context.Background(), "acme/widgets", 12, "abc123")
	if err == nil || errors.Is(err, ErrConflict) {
		t.Fatalf("error = %v, want a non-conflict error", err)
	}
	if n := len(mock.GetCallHistory()); n != 2 {
		t.Fatalf("calls = %d, want 2: the mutation runs once, never retried", n)
	}
}
