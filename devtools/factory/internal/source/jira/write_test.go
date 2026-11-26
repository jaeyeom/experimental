package jira

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"log/slog"
	"net/http"
	"reflect"
	"strconv"
	"strings"
	"sync"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const (
	claimTransitions = `{
		"transitions": [
			{"id": "11", "name": "Start progress", "to": {"name": "In Progress"}},
			{"id": "21", "name": "Backlog", "to": {"name": "To Do"}}
		]
	}`
	missTransitions = `{
		"transitions": [
			{"id": "11", "name": "Start", "to": {"name": "in progress"}},
			{"id": "21", "name": "Stop", "to": {"name": "To Do"}}
		]
	}`
	// The transition named Backlog targets In Progress. Release must match
	// to.name, and it must prefer ReadyStatuses order over this list order.
	releaseTransitions = `{
		"transitions": [
			{"id": "31", "name": "Backlog", "to": {"name": "In Progress"}},
			{"id": "21", "name": "Do it", "to": {"name": "To Do"}},
			{"id": "41", "name": "Queue", "to": {"name": "Backlog"}}
		]
	}`
	laterTransitions = `{
		"transitions": [
			{"id": "31", "name": "Backlog", "to": {"name": "In Progress"}},
			{"id": "21", "name": "Do it", "to": {"name": "To Do"}}
		]
	}`
	blockedIssue = `{
		"key": "PRJ-8",
		"fields": {
			"summary": "Need the writer",
			"status": {"name": "To Do", "statusCategory": {"key": "new"}},
			"issuetype": {"name": "Task"},
			"issuelinks": [
				{"id": "10001", "type": {"name": "Blocks"}, "inwardIssue": {"key": "PRJ-3"}},
				{"id": "10002", "type": {"name": "Blocks"}, "outwardIssue": {"key": "PRJ-9"}}
			],
			"created": "2026-01-02T03:04:05.000-0700",
			"updated": "2026-01-03T04:05:06.000-0700"
		}
	}`
	epicParent     = `{"key":"PRJ-1","fields":{"issuetype":{"name":"Epic","subtask":false,"hierarchyLevel":1}}}`
	createdKey     = `{"id":"10099","key":"PRJ-99","self":"https://example.atlassian.net/rest/api/3/issue/10099"}`
	claimIssueToDo = `{
		"key": "PRJ-1",
		"fields": {
			"summary": "Ship it",
			"status": {"name": "To Do", "statusCategory": {"key": "new"}},
			"issuetype": {"name": "Task"},
			"created": "2026-01-02T03:04:05.000-0700",
			"updated": "2026-01-03T04:05:06.000-0700"
		}
	}`
	claimIssueInProgress = `{
		"key": "PRJ-1",
		"fields": {
			"summary": "Ship it",
			"status": {"name": "In Progress", "statusCategory": {"key": "indeterminate"}},
			"issuetype": {"name": "Task"},
			"created": "2026-01-02T03:04:05.000-0700",
			"updated": "2026-01-03T04:05:06.000-0700"
		}
	}`
)

func TestClaim(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-1":              {jsonBody(http.StatusOK, claimIssueToDo)},
		http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions":  {jsonBody(http.StatusOK, claimTransitions)},
		http.MethodPost + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusNoContent, "")},
	})
	src.cfg.ClaimStatus = "In Progress"
	src.cfg.ClaimAssign = false
	if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
		t.Fatalf("Claim() error = %v", err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 3 {
		t.Fatalf("requests = %d, want 3", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
	assertEmptyCall(t, recs[1], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
	assertJSONCall(t, recs[2], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"11"}}`)
}

func TestClaimNoTransition(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-1":             {jsonBody(http.StatusOK, claimIssueToDo)},
		http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusOK, missTransitions)},
	})
	src.cfg.ClaimStatus = "In Progress"
	err := src.Claim(context.Background(), "PRJ-1")
	const want = `jira claim PRJ-1: expected a transition to one of "In Progress", found "in progress", "To Do"`
	if err == nil || err.Error() != want {
		t.Fatalf("Claim() error = %v, want %s", err, want)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
	assertEmptyCall(t, recs[1], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
}

func TestClaimAssign(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-1":              {jsonBody(http.StatusOK, claimIssueToDo)},
		http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions":  {jsonBody(http.StatusOK, claimTransitions)},
		http.MethodPost + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusNoContent, "")},
		http.MethodGet + " " + myselfPath:                        {okFixture(t, "myself.json")},
		http.MethodPut + " /rest/api/3/issue/PRJ-1/assignee":     {jsonBody(http.StatusNoContent, "")},
	})
	src.cfg.ClaimStatus = "In Progress"
	src.cfg.ClaimAssign = true
	if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
		t.Fatalf("Claim() error = %v", err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 5 {
		t.Fatalf("requests = %d, want 5", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
	assertEmptyCall(t, recs[1], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
	assertJSONCall(t, recs[2], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"11"}}`)
	assertEmptyCall(t, recs[3], http.MethodGet, myselfPath)
	assertJSONCall(t, recs[4], http.MethodPut, "/rest/api/3/issue/PRJ-1/assignee", `{"accountId":"`+testAccount+`"}`)
}

func TestClaimAlreadyInStatus(t *testing.T) {
	t.Run("no assign", func(t *testing.T) {
		src, fake := writeSource(t, map[string][]response{
			http.MethodGet + " /rest/api/3/issue/PRJ-1": {jsonBody(http.StatusOK, claimIssueInProgress)},
		})
		src.cfg.ClaimStatus = "In Progress"
		src.cfg.ClaimAssign = false
		if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 1 {
			t.Fatalf("requests = %d, want 1", len(recs))
		}
		assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
	})
	t.Run("assign", func(t *testing.T) {
		src, fake := writeSource(t, map[string][]response{
			http.MethodGet + " /rest/api/3/issue/PRJ-1":          {jsonBody(http.StatusOK, claimIssueInProgress)},
			http.MethodGet + " " + myselfPath:                    {okFixture(t, "myself.json")},
			http.MethodPut + " /rest/api/3/issue/PRJ-1/assignee": {jsonBody(http.StatusNoContent, "")},
		})
		src.cfg.ClaimStatus = "In Progress"
		src.cfg.ClaimAssign = true
		if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 3 {
			t.Fatalf("requests = %d, want 3", len(recs))
		}
		assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
		assertEmptyCall(t, recs[1], http.MethodGet, myselfPath)
		assertJSONCall(t, recs[2], http.MethodPut, "/rest/api/3/issue/PRJ-1/assignee", `{"accountId":"`+testAccount+`"}`)
	})
}

func TestClaimChoosesTransition(t *testing.T) {
	const both = `{
		"transitions": [
			{"id": "21", "name": "start work", "to": {"name": "In Progress"}},
			{"id": "11", "name": "In Progress", "to": {"name": "In Progress"}},
			{"id": "51", "name": "Done", "to": {"name": "Done"}}
		]
	}`
	const neither = `{
		"transitions": [
			{"id": "21", "name": "start work", "to": {"name": "In Progress"}},
			{"id": "11", "name": "begin", "to": {"name": "In Progress"}}
		]
	}`
	const numeric = `{
		"transitions": [
			{"id": "10", "name": "later", "to": {"name": "In Progress"}},
			{"id": "2", "name": "earlier", "to": {"name": "In Progress"}}
		]
	}`
	const lexical = `{
		"transitions": [
			{"id": "b", "name": "second", "to": {"name": "In Progress"}},
			{"id": "a", "name": "first", "to": {"name": "In Progress"}}
		]
	}`
	const elsewhere = `{
		"transitions": [
			{"id": "21", "name": "start work", "to": {"name": "In Progress"}},
			{"id": "11", "name": "begin", "to": {"name": "In Progress"}},
			{"id": "51", "name": "Done", "to": {"name": "Done"}}
		]
	}`
	tests := []struct {
		name       string
		body       string
		configured string
		wantID     string
		wantErr    string
		wantLog    []string
		quiet      bool
	}{
		{
			name:   "name equals claim status",
			body:   both,
			wantID: "11",
			quiet:  true,
		},
		{
			name:    "smallest integer id",
			body:    numeric,
			wantID:  "2",
			wantLog: []string{"2", "10", "earlier", "later"},
		},
		{
			name:    "smallest lexicographic id",
			body:    lexical,
			wantID:  "a",
			wantLog: []string{"a", "b", "first", "second"},
		},
		{
			name:       "claim_transition id",
			body:       neither,
			configured: "21",
			wantID:     "21",
			quiet:      true,
		},
		{
			name:       "claim_transition name",
			body:       neither,
			configured: "start work",
			wantID:     "21",
			quiet:      true,
		},
		{
			name:       "claim_transition does not land in claim status",
			body:       elsewhere,
			configured: "51",
			wantErr:    `expected claim_transition "51" to land in "In Progress"`,
		},
		{
			name:       "claim_transition missing",
			body:       neither,
			configured: "nope",
			wantErr:    `expected claim_transition "nope" to match one transition to "In Progress", found none`,
		},
		{
			name:    "unset claim_transition warns",
			body:    neither,
			wantID:  "11",
			wantLog: []string{"11", "21", "begin", "start work"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var logs bytes.Buffer
			prev := slog.Default()
			slog.SetDefault(slog.New(slog.NewTextHandler(&logs, nil)))
			t.Cleanup(func() { slog.SetDefault(prev) })

			queues := map[string][]response{
				http.MethodGet + " /rest/api/3/issue/PRJ-1":             {jsonBody(http.StatusOK, claimIssueToDo)},
				http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusOK, tc.body)},
			}
			if tc.wantErr == "" {
				queues[http.MethodPost+" /rest/api/3/issue/PRJ-1/transitions"] = []response{jsonBody(http.StatusNoContent, "")}
			}
			src, fake := writeSource(t, queues)
			src.cfg.ClaimStatus = "In Progress"
			src.cfg.ClaimTransition = tc.configured
			src.cfg.ClaimAssign = false
			err := src.Claim(context.Background(), "PRJ-1")
			recs := finishWrite(t, fake)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Claim() error = %v, want %s", err, tc.wantErr)
				}
				if len(recs) != 2 {
					t.Fatalf("requests = %d, want 2", len(recs))
				}
				assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
				assertEmptyCall(t, recs[1], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
				return
			}
			if err != nil {
				t.Fatalf("Claim() error = %v", err)
			}
			if len(recs) != 3 {
				t.Fatalf("requests = %d, want 3", len(recs))
			}
			assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
			assertEmptyCall(t, recs[1], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
			assertJSONCall(t, recs[2], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"`+tc.wantID+`"}}`)
			if tc.quiet && logs.Len() != 0 {
				t.Fatalf("log = %q, want none", logs.String())
			}
			for _, part := range tc.wantLog {
				if !strings.Contains(logs.String(), part) {
					t.Fatalf("log = %q, want %q", logs.String(), part)
				}
			}
		})
	}
}

func TestRelease(t *testing.T) {
	tests := []struct {
		name        string
		wantID      string
		ready       []string
		transitions string
	}{
		{
			name:        "first ready status wins over transition order",
			wantID:      "41",
			ready:       []string{"Backlog", "To Do"},
			transitions: releaseTransitions,
		},
		{
			name:        "later ready status when the earlier one is absent",
			wantID:      "21",
			ready:       []string{"Backlog", "To Do"},
			transitions: laterTransitions,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			src, fake := writeSource(t, map[string][]response{
				http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions":  {jsonBody(http.StatusOK, tc.transitions)},
				http.MethodPost + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusNoContent, "")},
				http.MethodPut + " /rest/api/3/issue/PRJ-1/assignee":     {jsonBody(http.StatusNoContent, "")},
			})
			src.cfg.ReadyStatuses = tc.ready
			if err := src.Release(context.Background(), "PRJ-1"); err != nil {
				t.Fatalf("Release() error = %v", err)
			}
			recs := finishWrite(t, fake)
			if len(recs) != 3 {
				t.Fatalf("requests = %d, want 3", len(recs))
			}
			assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
			assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"`+tc.wantID+`"}}`)
			assertJSONCall(t, recs[2], http.MethodPut, "/rest/api/3/issue/PRJ-1/assignee", `{"accountId":null}`)
		})
	}
}

func TestReleaseNoTransition(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions": {
			jsonBody(http.StatusOK, `{"transitions":[{"id":"31","name":"Stop","to":{"name":"In Progress"}},{"id":"51","name":"Finish","to":{"name":"Done"}}]}`),
		},
	})
	src.cfg.ReadyStatuses = []string{"Backlog", "To Do"}
	err := src.Release(context.Background(), "PRJ-1")
	const want = `jira release PRJ-1: expected a transition to one of "Backlog", "To Do", found "In Progress", "Done"`
	if err == nil || err.Error() != want {
		t.Fatalf("Release() error = %v, want %s", err, want)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 1 {
		t.Fatalf("requests = %d, want 1", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
}

func TestReleaseRequiresOneTransition(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions": {
			jsonBody(http.StatusOK, `{"transitions":[{"id":"41","name":"Backlog","to":{"name":"Backlog"}},{"id":"42","name":"Queue","to":{"name":"Backlog"}}]}`),
		},
	})
	src.cfg.ReadyStatuses = []string{"Backlog", "To Do"}
	err := src.Release(context.Background(), "PRJ-1")
	const want = `jira release PRJ-1: expected one transition to "Backlog", found 2`
	if err == nil || err.Error() != want {
		t.Fatalf("Release() error = %v, want %s", err, want)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 1 {
		t.Fatalf("requests = %d, want 1", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
}

func TestFinishUnset(t *testing.T) {
	src := New(config.Source{Name: "jira:acme"}, nil)
	if got := src.Name(); got != "jira:acme" {
		t.Fatalf("Name() = %q", got)
	}
	err := src.Finish(context.Background(), "PRJ-1")
	if !errors.Is(err, source.ErrNotSupported) {
		t.Fatalf("Finish() error = %v, want ErrNotSupported", err)
	}
}

func TestFinish(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions": {
			jsonBody(http.StatusOK, `{"transitions":[{"id":"51","name":"Done","to":{"name":"Done"}},{"id":"21","name":"To Do","to":{"name":"To Do"}}]}`),
		},
		http.MethodPost + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusNoContent, "")},
	})
	src.cfg.FinishStatus = "Done"
	if err := src.Finish(context.Background(), "PRJ-1"); err != nil {
		t.Fatalf("Finish() error = %v", err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
	assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"51"}}`)
}

func TestCloseUnset(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{})
	src.cfg.FinishStatus = ""
	err := src.Close(context.Background(), "PRJ-1", "All done.")
	if !errors.Is(err, source.ErrNotSupported) {
		t.Fatalf("Close() error = %v, want ErrNotSupported", err)
	}
	if recs := fake.requests(); len(recs) != 0 {
		t.Fatalf("requests = %d, want 0", len(recs))
	}
}

func TestCloseWithComment(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions": {
			jsonBody(http.StatusOK, `{"transitions":[{"id":"51","name":"Done","to":{"name":"Done"}},{"id":"21","name":"To Do","to":{"name":"To Do"}}]}`),
		},
		http.MethodPost + " /rest/api/3/issue/PRJ-1/comment":     {jsonBody(http.StatusCreated, `{"id":"10001"}`)},
		http.MethodPost + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusNoContent, "")},
	})
	src.cfg.FinishStatus = "Done"
	if err := src.Close(context.Background(), "PRJ-1", "Closed with **all** subtasks done."); err != nil {
		t.Fatalf("Close() error = %v", err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 3 {
		t.Fatalf("requests = %d, want 3", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
	const wantComment = `{"body":{"type":"doc","version":1,"content":[{"type":"paragraph","content":[{"type":"text","text":"Closed with "},{"type":"text","text":"all","marks":[{"type":"strong"}]},{"type":"text","text":" subtasks done."}]}]}}`
	assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue/PRJ-1/comment", wantComment)
	assertJSONCall(t, recs[2], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"51"}}`)
}

func TestCloseWithoutComment(t *testing.T) {
	for _, tc := range []struct {
		name    string
		comment string
	}{
		{name: "empty", comment: ""},
		{name: "whitespace", comment: "  \n\t  "},
	} {
		t.Run(tc.name, func(t *testing.T) {
			src, fake := writeSource(t, map[string][]response{
				http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions": {
					jsonBody(http.StatusOK, `{"transitions":[{"id":"51","name":"Done","to":{"name":"Done"}}]}`),
				},
				http.MethodPost + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusNoContent, "")},
			})
			src.cfg.FinishStatus = "Done"
			if err := src.Close(context.Background(), "PRJ-1", tc.comment); err != nil {
				t.Fatalf("Close() error = %v", err)
			}
			recs := finishWrite(t, fake)
			if len(recs) != 2 {
				t.Fatalf("requests = %d, want 2", len(recs))
			}
			assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
			assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"51"}}`)
		})
	}
}

func TestCloseTransitionErrorsSkipComment(t *testing.T) {
	tests := []struct {
		name        string
		transitions string
		want        string
	}{
		{
			name:        "missing transition",
			transitions: `{"transitions":[{"id":"21","name":"To Do","to":{"name":"To Do"}}]}`,
			want:        `jira close PRJ-1: expected a transition to one of "Done", found "To Do"`,
		},
		{
			name:        "ambiguous transition",
			transitions: `{"transitions":[{"id":"51","name":"Done","to":{"name":"Done"}},{"id":"52","name":"Complete","to":{"name":"Done"}}]}`,
			want:        `jira close PRJ-1: expected one transition to "Done", found 2`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			src, fake := writeSource(t, map[string][]response{
				http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions": {jsonBody(http.StatusOK, tc.transitions)},
			})
			src.cfg.FinishStatus = "Done"
			err := src.Close(context.Background(), "PRJ-1", "Closing comment")
			if err == nil || err.Error() != tc.want {
				t.Fatalf("Close() error = %v, want %s", err, tc.want)
			}
			recs := finishWrite(t, fake)
			if len(recs) != 1 {
				t.Fatalf("requests = %d, want 1", len(recs))
			}
			assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
		})
	}
}

func TestLinkBody(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodPost + " /rest/api/3/issueLink": {jsonBody(http.StatusCreated, `{}`)},
	})
	src.cfg.BlocksLinkType = "Blocks"
	if err := src.Link(context.Background(), "PRJ-3", "PRJ-8"); err != nil {
		t.Fatalf("Link() error = %v", err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 1 {
		t.Fatalf("requests = %d, want 1", len(recs))
	}
	const want = `{"type":{"name":"Blocks"},"inwardIssue":{"key":"PRJ-3"},"outwardIssue":{"key":"PRJ-8"}}`
	assertJSONCall(t, recs[0], http.MethodPost, "/rest/api/3/issueLink", want)
	if string(recs[0].body) != want {
		t.Fatalf("body = %s, want %s", recs[0].body, want)
	}
}

func TestUnlink(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-8":        {jsonBody(http.StatusOK, blockedIssue)},
		http.MethodDelete + " /rest/api/3/issueLink/10001": {jsonBody(http.StatusNoContent, "")},
	})
	if err := src.Unlink(context.Background(), "PRJ-3", "PRJ-8"); err != nil {
		t.Fatalf("Unlink() error = %v", err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-8")
	if recs[0].rawQuery != "fields="+strings.Join(issueFields, ",") {
		t.Fatalf("query = %q", recs[0].rawQuery)
	}
	assertEmptyCall(t, recs[1], http.MethodDelete, "/rest/api/3/issueLink/10001")
}

func TestUnlinkNotFound(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-8": {jsonBody(http.StatusOK, blockedIssue)},
	})
	err := src.Unlink(context.Background(), "PRJ-9", "PRJ-8")
	if !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Unlink() error = %v, want ErrNotFound", err)
	}
	if !strings.Contains(err.Error(), "PRJ-9 does not block PRJ-8") {
		t.Fatalf("Unlink() error = %v", err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 1 {
		t.Fatalf("requests = %d, want 1", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-8")
}

func TestCreateParentDefault(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-2436": {jsonBody(http.StatusOK, epicParent)},
		http.MethodPost + " /rest/api/3/issue":         {jsonBody(http.StatusCreated, createdKey)},
	})
	src.cfg.CreateProject = ""
	src.cfg.DefaultParent = "PRJ-2436"
	src.cfg.CreateIssueType = "Task"
	req := source.CreateRequest{
		Title:  "Ship the writer",
		Body:   "Ship the writer.\nNow.\n\nKeep the log.",
		Labels: []string{"agent-ready", "backend"},
	}
	id, err := src.Create(context.Background(), req)
	if err != nil || id != "PRJ-99" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-2436")
	recs = recs[1:]
	const want = `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship the writer","labels":["agent-ready","backend"],"parent":{"key":"PRJ-2436"},"description":{"type":"doc","version":1,"content":[{"type":"paragraph","content":[{"type":"text","text":"Ship the writer."},{"type":"hardBreak"},{"type":"text","text":"Now."}]},{"type":"paragraph","content":[{"type":"text","text":"Keep the log."}]}]}}}`
	assertJSONCall(t, recs[0], http.MethodPost, "/rest/api/3/issue", want)
}

func TestCreateProjectDerivation(t *testing.T) {
	tests := []struct {
		name          string
		parent        string
		fallback      string
		createProject string
		flagProject   string
		blockedBy     string
		want          string
	}{
		{
			name:     "explicit parent derives the project",
			parent:   "EDGE-7",
			fallback: "PRJ-2436",
			want:     `{"fields":{"project":{"key":"EDGE"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[],"parent":{"key":"EDGE-7"}}}`,
		},
		{
			name:          "create project wins",
			parent:        "EDGE-7",
			createProject: "PRJ",
			want:          `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[],"parent":{"key":"EDGE-7"}}}`,
		},
		{
			name:        "flag project with no parent",
			flagProject: "ABC",
			want:        `{"fields":{"project":{"key":"ABC"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[]}}`,
		},
		{
			name:          "flag project beats create project and parent",
			parent:        "EDGE-7",
			createProject: "PRJ",
			flagProject:   "ABC",
			want:          `{"fields":{"project":{"key":"ABC"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[],"parent":{"key":"EDGE-7"}}}`,
		},
		{
			name:      "blocked-by key gives the project",
			blockedBy: "ABC-12",
			want:      `{"fields":{"project":{"key":"ABC"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[]}}`,
		},
		{
			name:          "project only omits parent",
			createProject: "PRJ",
			want:          `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[]}}`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			queues := map[string][]response{
				http.MethodPost + " /rest/api/3/issue": {jsonBody(http.StatusCreated, createdKey)},
			}
			if tc.parent != "" {
				queues[http.MethodGet+" /rest/api/3/issue/"+tc.parent] = []response{jsonBody(http.StatusOK, epicParent)}
			}
			req := source.CreateRequest{Title: "Ship it", Parent: tc.parent, Project: tc.flagProject}
			if tc.blockedBy != "" {
				req.BlockedBy = []string{tc.blockedBy}
				queues[http.MethodPost+" /rest/api/3/issueLink"] = []response{jsonBody(http.StatusCreated, `{}`)}
			}
			src, fake := writeSource(t, queues)
			src.cfg.CreateProject = tc.createProject
			src.cfg.DefaultParent = tc.fallback
			id, err := src.Create(context.Background(), req)
			if err != nil || id != "PRJ-99" {
				t.Fatalf("Create() = %q, %v", id, err)
			}
			recs := finishWrite(t, fake)
			if tc.parent != "" {
				if len(recs) != 2 {
					t.Fatalf("requests = %d, want 2", len(recs))
				}
				assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/"+tc.parent)
				recs = recs[1:]
			}
			if tc.blockedBy != "" {
				if len(recs) != 2 {
					t.Fatalf("requests = %d, want 2", len(recs))
				}
				recs = recs[:1]
			}
			if len(recs) != 1 {
				t.Fatalf("requests = %d, want 1", len(recs))
			}
			assertJSONCall(t, recs[0], http.MethodPost, "/rest/api/3/issue", tc.want)
		})
	}
}

func TestCreateLinks(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-1": {jsonBody(http.StatusOK, epicParent)},
		http.MethodPost + " /rest/api/3/issue":      {jsonBody(http.StatusCreated, createdKey)},
		http.MethodPost + " /rest/api/3/issueLink":  {jsonBody(http.StatusCreated, `{}`), jsonBody(http.StatusCreated, `{}`)},
	})
	src.cfg.CreateProject = "PRJ"
	id, err := src.Create(context.Background(), source.CreateRequest{
		Title:     "Ship the writer",
		Parent:    "PRJ-1",
		Blocks:    []string{"PRJ-2"},
		BlockedBy: []string{"PRJ-3"},
	})
	if err != nil || id != "PRJ-99" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 4 {
		t.Fatalf("requests = %d, want 4", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
	recs = recs[1:]
	assertJSONCall(t, recs[0], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship the writer","labels":[],"parent":{"key":"PRJ-1"}}}`)
	assertExactLink(t, recs[1], `{"type":{"name":"Blocks"},"inwardIssue":{"key":"PRJ-99"},"outwardIssue":{"key":"PRJ-2"}}`)
	assertExactLink(t, recs[2], `{"type":{"name":"Blocks"},"inwardIssue":{"key":"PRJ-3"},"outwardIssue":{"key":"PRJ-99"}}`)
}

func TestCreateLinkFails(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/EDGE-7": {jsonBody(http.StatusOK, epicParent)},
		http.MethodPost + " /rest/api/3/issue":       {jsonBody(http.StatusCreated, createdKey)},
		http.MethodPost + " /rest/api/3/issueLink": {
			{status: http.StatusBadRequest, body: []byte(`{"errorMessages":["No such link type"]}`)},
		},
	})
	src.cfg.CreateProject = "PRJ"
	id, err := src.Create(context.Background(), source.CreateRequest{
		Title:     "Ship the writer",
		Parent:    "EDGE-7",
		Blocks:    []string{"PRJ-2"},
		BlockedBy: []string{"PRJ-3"},
	})
	if id != "PRJ-99" {
		t.Fatalf("Create() key = %q, want PRJ-99", id)
	}
	if err == nil || !strings.Contains(err.Error(), "issue PRJ-99 exists but is missing links") || !strings.Contains(err.Error(), "400") {
		t.Fatalf("Create() error = %v", err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 3 {
		t.Fatalf("requests = %d, want 3", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/EDGE-7")
	recs = recs[1:]
	assertJSONCall(t, recs[0], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship the writer","labels":[],"parent":{"key":"EDGE-7"}}}`)
	assertExactLink(t, recs[1], `{"type":{"name":"Blocks"},"inwardIssue":{"key":"PRJ-99"},"outwardIssue":{"key":"PRJ-2"}}`)
}

// TestLinkRoundTrip posts a Blocks link, then reads the blocked issue back.
// The fake applies the live Jira rule, not the request builder: on POST,
// inwardIssue is the blocker and outwardIssue is the blocked issue. On that
// blocked issue's own issuelinks, inwardIssue is the blocker. decode keeps
// that inwardIssue as BlockedBy.
func TestLinkRoundTrip(t *testing.T) {
	const blocker = "PRJ-3"
	const blocked = "PRJ-8"
	var mu sync.Mutex
	var posted []issueLinkBody
	src, fake := writeSource(t, map[string][]response{})
	fake.fallback = func(rec recorded) (response, bool) {
		switch {
		case rec.method == http.MethodPost && rec.path == issueLinkPath:
			var body issueLinkBody
			if err := json.Unmarshal(rec.body, &body); err != nil {
				return jsonBody(http.StatusBadRequest, `{"errorMessages":["expected an issue link object"]}`), true
			}
			mu.Lock()
			posted = append(posted, body)
			mu.Unlock()
			return jsonBody(http.StatusCreated, `{}`), true
		case rec.method == http.MethodGet && strings.HasPrefix(rec.path, "/rest/api/3/issue/"):
			key := strings.TrimPrefix(rec.path, "/rest/api/3/issue/")
			if key == "" || strings.Contains(key, "/") {
				return response{}, false
			}
			mu.Lock()
			raw, err := blockedIssueView(key, posted)
			mu.Unlock()
			if err != nil {
				return jsonBody(http.StatusInternalServerError, `{"errorMessages":["expected to encode the issue"]}`), true
			}
			return response{status: http.StatusOK, body: raw}, true
		default:
			return response{}, false
		}
	}
	if err := src.Link(context.Background(), blocker, blocked); err != nil {
		t.Fatalf("Link() error = %v", err)
	}
	got, err := src.Get(context.Background(), blocked)
	if err != nil {
		t.Fatalf("Get(%s) error = %v", blocked, err)
	}
	if len(got.BlockedBy) != 1 || got.BlockedBy[0].ID != blocker {
		t.Fatalf("BlockedBy = %+v, want %s", got.BlockedBy, blocker)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	const want = `{"type":{"name":"Blocks"},"inwardIssue":{"key":"PRJ-3"},"outwardIssue":{"key":"PRJ-8"}}`
	assertExactLink(t, recs[0], want)
	assertEmptyCall(t, recs[1], http.MethodGet, "/rest/api/3/issue/"+blocked)
}

// blockedIssueView is the GET body Jira returns for key after these POSTs.
// A stored inwardIssue blocks the stored outwardIssue. The blocked issue
// therefore lists that inwardIssue, and the blocker lists an outwardIssue
// that this read path ignores.
func blockedIssueView(key string, posted []issueLinkBody) ([]byte, error) {
	links := make([]jiraLink, 0)
	for i, body := range posted {
		link := jiraLink{
			ID:   strconv.Itoa(10001 + i),
			Type: body.Type,
		}
		switch key {
		case body.OutwardIssue.Key:
			link.InwardIssue = &jiraRef{Key: body.InwardIssue.Key}
		case body.InwardIssue.Key:
			link.OutwardIssue = &jiraRef{Key: body.OutwardIssue.Key}
		default:
			continue
		}
		links = append(links, link)
	}
	issue := jiraIssue{
		Key: key,
		Fields: jiraFields{
			Summary:    key,
			Status:     &jiraStatus{Name: "To Do", StatusCategory: &jiraCategory{Key: "new"}},
			IssueType:  &jiraNamed{Name: "Task"},
			IssueLinks: links,
			Created:    "2026-01-02T03:04:05.000-0700",
			Updated:    "2026-01-03T04:05:06.000-0700",
		},
	}
	return json.Marshal(issue) //nolint:wrapcheck // test helper delegates directly
}

func TestCreateMissingKey(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodPost + " /rest/api/3/issue": {jsonBody(http.StatusCreated, `{"id":"10099"}`)},
	})
	src.cfg.CreateProject = "PRJ"
	id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it"})
	if id != "" || err == nil || !strings.Contains(err.Error(), "expected key") {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 1 {
		t.Fatalf("requests = %d, want 1", len(recs))
	}
	assertJSONCall(t, recs[0], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[]}}`)
}

func TestCreateRejects(t *testing.T) {
	ctx := context.Background()
	tests := []struct {
		name string
		src  *Source
		req  source.CreateRequest
		want string
	}{
		{
			name: "empty summary",
			src:  New(config.Source{CreateIssueType: "Task", CreateProject: "PRJ"}, nil),
			req:  source.CreateRequest{},
			want: "expected a summary, found empty",
		},
		{
			name: "empty label",
			src:  New(config.Source{CreateIssueType: "Task", CreateProject: "PRJ"}, nil),
			req:  source.CreateRequest{Title: "Ship it", Labels: []string{"ok", ""}},
			want: "expected a label, found empty",
		},
		{
			name: "empty issue type",
			src:  New(config.Source{CreateProject: "PRJ"}, nil),
			req:  source.CreateRequest{Title: "Ship it"},
			want: "expected create_issue_type, found empty",
		},
		{
			name: "no project",
			src:  New(config.Source{Name: "acme", CreateIssueType: "Task"}, nil),
			req:  source.CreateRequest{Title: "Ship it"},
			want: "jira create: no project for the new item; pass --project KEY or --parent KEY, or set create_project for source jira:acme",
		},
		{
			name: "bad parent",
			src:  New(config.Source{CreateIssueType: "Task", DefaultParent: "EDGE-1"}, nil),
			req:  source.CreateRequest{Title: "Ship it", Parent: "PRJ"},
			want: `expected a parent key PROJECT-NUMBER, found "PRJ"`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			id, err := tc.src.Create(ctx, tc.req)
			if id != "" || err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Create() = %q, %v, want %s", id, err, tc.want)
			}
		})
	}
}

func TestCreateNilContext(t *testing.T) {
	src := New(config.Source{CreateIssueType: "Task", CreateProject: "PRJ"}, nil)
	var ctx context.Context
	id, err := src.Create(ctx, source.CreateRequest{Title: "Ship it"})
	if id != "" || err == nil || !strings.Contains(err.Error(), "expected a context, found nil") {
		t.Fatalf("Create() = %q, %v", id, err)
	}
}

func TestWriteRejects(t *testing.T) {
	var nilCtx context.Context
	ctx := context.Background()
	bare := New(config.Source{}, nil)
	ready := New(config.Source{
		ClaimStatus:    "In Progress",
		FinishStatus:   "Done",
		ReadyStatuses:  []string{"To Do"},
		BlocksLinkType: "Blocks",
	}, nil)
	checks := []struct {
		name string
		err  error
		want string
	}{
		{name: "claim nil context", err: ready.Claim(nilCtx, "PRJ-1"), want: "expected a context, found nil"},
		{name: "claim empty key", err: ready.Claim(ctx, ""), want: "expected an issue key, found empty"},
		{name: "claim empty status", err: bare.Claim(ctx, "PRJ-1"), want: "expected claim_status, found empty"},
		{name: "release nil context", err: ready.Release(nilCtx, "PRJ-1"), want: "expected a context, found nil"},
		{name: "release empty key", err: ready.Release(ctx, ""), want: "expected an issue key, found empty"},
		{name: "finish nil context", err: ready.Finish(nilCtx, "PRJ-1"), want: "expected a context, found nil"},
		{name: "finish empty key", err: ready.Finish(ctx, ""), want: "expected an issue key, found empty"},
		{name: "close nil context", err: ready.Close(nilCtx, "PRJ-1", "done"), want: "expected a context, found nil"},
		{name: "close empty key", err: ready.Close(ctx, "", "done"), want: "expected an issue key, found empty"},
		{name: "link nil context", err: ready.Link(nilCtx, "PRJ-1", "PRJ-2"), want: "expected a context, found nil"},
		{name: "link empty blocker", err: ready.Link(ctx, "", "PRJ-2"), want: "expected a blocker key, found empty"},
		{name: "link empty blocked", err: ready.Link(ctx, "PRJ-1", ""), want: "expected a blocked key, found empty"},
		{name: "link empty type", err: bare.Link(ctx, "PRJ-1", "PRJ-2"), want: "expected blocks_link_type, found empty"},
		{name: "unlink nil context", err: ready.Unlink(nilCtx, "PRJ-1", "PRJ-2"), want: "expected a context, found nil"},
		{name: "unlink empty blocker", err: ready.Unlink(ctx, "", "PRJ-2"), want: "expected a blocker key, found empty"},
		{name: "unlink empty blocked", err: ready.Unlink(ctx, "PRJ-1", ""), want: "expected a blocked key, found empty"},
	}
	for _, tc := range checks {
		t.Run(tc.name, func(t *testing.T) {
			if tc.err == nil || !strings.Contains(tc.err.Error(), tc.want) {
				t.Fatalf("error = %v, want %s", tc.err, tc.want)
			}
		})
	}
}

func TestSetPriority(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodPut + " /rest/api/3/issue/PRJ-10": {jsonBody(http.StatusNoContent, "")},
	})
	if err := src.SetPriority(context.Background(), "PRJ-10", 0); err != nil {
		t.Fatal(err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 1 {
		t.Fatalf("calls = %d", len(recs))
	}
	assertJSONCall(t, recs[0], http.MethodPut, "/rest/api/3/issue/PRJ-10", `{"fields":{"priority":{"name":"Highest"}}}`)
}

func TestSetPriorityRejectsUnnamedRank(t *testing.T) {
	src := New(config.Source{Kind: "jira"}, nil)
	err := src.SetPriority(context.Background(), "PRJ-10", 9)
	if err == nil || !strings.Contains(err.Error(), "expected one name for rank 9") || !strings.Contains(err.Error(), "found none") {
		t.Fatalf("error = %v", err)
	}
}

func writeSource(t *testing.T, queues map[string][]response) (*Source, *jiraFake) {
	t.Helper()
	fake := startJira(t, queues)
	src, clk := fake.open(testJQL)
	t.Cleanup(func() {
		if waits := clk.durations(); len(waits) != 0 {
			t.Errorf("sleeps = %v, want none", waits)
		}
	})
	src.cfg.CreateIssueType = "Task"
	src.cfg.CreateAssignee = config.CreateAssigneeUnassigned
	src.cfg.BlocksLinkType = "Blocks"
	return src, fake
}

func finishWrite(t *testing.T, fake *jiraFake) []recorded {
	t.Helper()
	recs := fake.requests()
	assertAuth(t, recs)
	return recs
}

func jsonBody(status int, body string) response {
	return response{status: status, body: []byte(body)}
}

func assertEmptyCall(t *testing.T, rec recorded, method, path string) {
	t.Helper()
	if rec.method != method || rec.path != path {
		t.Fatalf("call = %s %s, want %s %s", rec.method, rec.path, method, path)
	}
	if len(rec.body) != 0 {
		t.Fatalf("body = %s, want empty", rec.body)
	}
	if rec.contentType != "" {
		t.Fatalf("Content-Type = %q, want empty", rec.contentType)
	}
}

func assertJSONCall(t *testing.T, rec recorded, method, path, want string) {
	t.Helper()
	if rec.method != method || rec.path != path {
		t.Fatalf("call = %s %s, want %s %s", rec.method, rec.path, method, path)
	}
	if rec.contentType != "application/json" {
		t.Fatalf("Content-Type = %q", rec.contentType)
	}
	assertDecodedJSON(t, rec.body, []byte(want))
}

func assertExactLink(t *testing.T, rec recorded, want string) {
	t.Helper()
	assertJSONCall(t, rec, http.MethodPost, "/rest/api/3/issueLink", want)
	if string(rec.body) != want {
		t.Fatalf("body = %s, want %s", rec.body, want)
	}
}

func assertDecodedJSON(t *testing.T, got, want []byte) {
	t.Helper()
	var gotVal any
	var wantVal any
	if err := json.Unmarshal(got, &gotVal); err != nil {
		t.Fatalf("decode body: %v\n%s", err, got)
	}
	if err := json.Unmarshal(want, &wantVal); err != nil {
		t.Fatalf("decode want: %v\n%s", err, want)
	}
	if !reflect.DeepEqual(gotVal, wantVal) {
		t.Fatalf("body = %s, want %s", got, want)
	}
}

const (
	taskParent    = `{"key":"ABC-12","fields":{"issuetype":{"name":"Task","subtask":false,"hierarchyLevel":0}}}`
	subtaskParent = `{"key":"ABC-12","fields":{"issuetype":{"name":"Sub-task","subtask":true,"hierarchyLevel":-1}}}`
	abcProject    = `{"key":"ABC","issueTypes":[{"name":"Task","subtask":false},{"name":"Epic","subtask":false},{"name":"Sub-task","subtask":true}]}`
)

func TestCreateUnderEpicSendsCreateIssueType(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/ABC-1": {jsonBody(http.StatusOK, epicParent)},
		http.MethodPost + " /rest/api/3/issue":      {jsonBody(http.StatusCreated, createdKey)},
	})
	src.cfg.CreateProject = ""
	id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it", Parent: "ABC-1"})
	if err != nil || id != "PRJ-99" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/ABC-1")
	if recs[0].rawQuery != "fields=issuetype" {
		t.Fatalf("query = %q, want fields=issuetype", recs[0].rawQuery)
	}
	assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"ABC"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[],"parent":{"key":"ABC-1"}}}`)
}

func TestCreateUnderTaskSendsSubtaskType(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/ABC-12": {jsonBody(http.StatusOK, taskParent)},
		http.MethodGet + " /rest/api/3/project/ABC":  {jsonBody(http.StatusOK, abcProject)},
		http.MethodPost + " /rest/api/3/issue":       {jsonBody(http.StatusCreated, createdKey)},
	})
	src.cfg.CreateProject = ""
	id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it", Parent: "ABC-12"})
	if err != nil || id != "PRJ-99" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 3 {
		t.Fatalf("requests = %d, want 3", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/ABC-12")
	assertEmptyCall(t, recs[1], http.MethodGet, "/rest/api/3/project/ABC")
	assertJSONCall(t, recs[2], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"ABC"},"issuetype":{"name":"Sub-task"},"summary":"Ship it","labels":[],"parent":{"key":"ABC-12"}}}`)
}

func TestCreateUnderTaskUsesConfiguredSubtaskType(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/ABC-12": {jsonBody(http.StatusOK, taskParent)},
		http.MethodPost + " /rest/api/3/issue":       {jsonBody(http.StatusCreated, createdKey)},
	})
	src.cfg.CreateProject = ""
	src.cfg.CreateSubtaskIssueType = "Subtask"
	id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it", Parent: "ABC-12"})
	if err != nil || id != "PRJ-99" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"ABC"},"issuetype":{"name":"Subtask"},"summary":"Ship it","labels":[],"parent":{"key":"ABC-12"}}}`)
}

func TestCreateRefusesBeforeCreateCall(t *testing.T) {
	tests := []struct {
		name    string
		queues  map[string][]response
		want    string
		wantReq int
	}{
		{
			name: "parent is a subtask",
			queues: map[string][]response{
				http.MethodGet + " /rest/api/3/issue/ABC-12": {jsonBody(http.StatusOK, subtaskParent)},
			},
			want:    "jira create: cannot file under ABC-12 (Sub-task): Jira does not allow children of a subtask",
			wantReq: 1,
		},
		{
			name: "project has no subtask type",
			queues: map[string][]response{
				http.MethodGet + " /rest/api/3/issue/ABC-12": {jsonBody(http.StatusOK, taskParent)},
				http.MethodGet + " /rest/api/3/project/ABC":  {jsonBody(http.StatusOK, `{"issueTypes":[{"name":"Task","subtask":false}]}`)},
			},
			want:    "jira create: cannot file under ABC-12 (Task): project ABC has no subtask issue type",
			wantReq: 2,
		},
		{
			name: "project has two subtask types",
			queues: map[string][]response{
				http.MethodGet + " /rest/api/3/issue/ABC-12": {jsonBody(http.StatusOK, taskParent)},
				http.MethodGet + " /rest/api/3/project/ABC":  {jsonBody(http.StatusOK, `{"issueTypes":[{"name":"Sub-task","subtask":true},{"name":"Bug fix","subtask":true}]}`)},
			},
			want:    `jira create: cannot file under ABC-12 (Task): expected one subtask issue type in project ABC, found "Sub-task", "Bug fix"; set create_subtask_issue_type`,
			wantReq: 2,
		},
		{
			name: "parent issuetype lacks hierarchyLevel",
			queues: map[string][]response{
				http.MethodGet + " /rest/api/3/issue/ABC-12": {jsonBody(http.StatusOK, `{"fields":{"issuetype":{"name":"Task","subtask":false}}}`)},
			},
			want:    "jira create: parent ABC-12: expected issuetype name, subtask, and hierarchyLevel, found",
			wantReq: 1,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			src, fake := writeSource(t, tc.queues)
			src.cfg.CreateProject = ""
			id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it", Parent: "ABC-12"})
			if id != "" || err == nil || !strings.HasPrefix(err.Error(), tc.want) {
				t.Fatalf("Create() = %q, %v, want error %q", id, err, tc.want)
			}
			recs := finishWrite(t, fake)
			if len(recs) != tc.wantReq {
				t.Fatalf("requests = %d, want %d", len(recs), tc.wantReq)
			}
			for _, rec := range recs {
				if rec.method == http.MethodPost {
					t.Fatalf("unexpected create call %s %s", rec.method, rec.path)
				}
			}
		})
	}
}

const meAccount = "5b10ac8d82e05b22cc7d4ef5"

func TestCreateAssignsMeByDefault(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " " + myselfPath:      {okFixture(t, "myself.json")},
		http.MethodPost + " /rest/api/3/issue": {jsonBody(http.StatusCreated, createdKey)},
	})
	src.cfg.CreateProject = "PRJ"
	src.cfg.CreateAssignee = config.CreateAssigneeMe
	id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it"})
	if err != nil || id != "PRJ-99" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	assertEmptyCall(t, recs[0], http.MethodGet, myselfPath)
	assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[],"assignee":{"accountId":"`+meAccount+`"}}}`)
}

func TestCreateUnassignedSendsNoAssignee(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodPost + " /rest/api/3/issue": {jsonBody(http.StatusCreated, createdKey)},
	})
	src.cfg.CreateProject = "PRJ"
	src.cfg.CreateAssignee = config.CreateAssigneeUnassigned
	if id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it"}); err != nil || id != "PRJ-99" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 1 {
		t.Fatalf("requests = %d, want 1", len(recs))
	}
	assertJSONCall(t, recs[0], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[]}}`)
}

func TestCreateAssigneeNotOnScreenAssignsAfterCreate(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " " + myselfPath: {okFixture(t, "myself.json")},
		http.MethodPost + " /rest/api/3/issue": {
			jsonBody(http.StatusBadRequest, `{"errorMessages":[],"errors":{"assignee":"Field 'assignee' cannot be set. It is not on the appropriate screen, or unknown."}}`),
			jsonBody(http.StatusCreated, createdKey),
		},
		http.MethodPut + " /rest/api/3/issue/PRJ-99/assignee": {{status: http.StatusNoContent}},
	})
	src.cfg.CreateProject = "PRJ"
	src.cfg.CreateAssignee = config.CreateAssigneeMe
	id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it"})
	if err != nil || id != "PRJ-99" {
		t.Fatalf("Create() = %q, %v", id, err)
	}
	recs := finishWrite(t, fake)
	if len(recs) != 4 {
		t.Fatalf("requests = %d, want 4", len(recs))
	}
	assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[],"assignee":{"accountId":"`+meAccount+`"}}}`)
	assertJSONCall(t, recs[2], http.MethodPost, "/rest/api/3/issue", `{"fields":{"project":{"key":"PRJ"},"issuetype":{"name":"Task"},"summary":"Ship it","labels":[]}}`)
	assertJSONCall(t, recs[3], http.MethodPut, "/rest/api/3/issue/PRJ-99/assignee", `{"accountId":"`+meAccount+`"}`)
}

func TestCreateOtherFieldErrorIsNotRetried(t *testing.T) {
	src, fake := writeSource(t, map[string][]response{
		http.MethodGet + " " + myselfPath: {okFixture(t, "myself.json")},
		http.MethodPost + " /rest/api/3/issue": {
			jsonBody(http.StatusBadRequest, `{"errorMessages":[],"errors":{"assignee":"bad","summary":"bad"}}`),
		},
	})
	src.cfg.CreateProject = "PRJ"
	src.cfg.CreateAssignee = config.CreateAssigneeMe
	id, err := src.Create(context.Background(), source.CreateRequest{Title: "Ship it"})
	if id != "" || err == nil || !strings.Contains(err.Error(), "400") {
		t.Fatalf("Create() = %q, %v, want a 400 error", id, err)
	}
	if recs := finishWrite(t, fake); len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
}
