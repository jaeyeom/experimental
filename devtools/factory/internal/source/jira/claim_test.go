package jira

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

type roundTripFunc func(*http.Request) (*http.Response, error)

func (f roundTripFunc) RoundTrip(r *http.Request) (*http.Response, error) {
	return f(r)
}

type noEmpty204Recorder struct {
	*httptest.ResponseRecorder
}

func (w *noEmpty204Recorder) Write(p []byte) (int, error) {
	if len(p) == 0 && w.Code == http.StatusNoContent {
		return 0, nil
	}
	return w.ResponseRecorder.Write(p) //nolint:wrapcheck // test helper delegates directly
}

// writeSourceInMemory runs jiraFake in-process via an http.RoundTripper so
// tests never bind or dial a loopback TCP socket.
func writeSourceInMemory(t *testing.T, queues map[string][]response) (*Source, *jiraFake) {
	t.Helper()
	fake := &jiraFake{t: t, queues: queues}
	client := &http.Client{
		Transport: roundTripFunc(func(r *http.Request) (*http.Response, error) {
			if r.Body == nil {
				r.Body = http.NoBody
			}
			rec := &noEmpty204Recorder{ResponseRecorder: httptest.NewRecorder()}
			fake.ServeHTTP(rec, r)
			return rec.Result(), nil
		}),
	}
	cfg := config.Source{
		Name:            "acme",
		Kind:            "jira",
		Site:            "https://example.atlassian.net",
		Email:           testEmail,
		Token:           testToken,
		JQL:             testJQL,
		BlocksLinkType:  "Blocks",
		CreateIssueType: "Task",
		CreateAssignee:  config.CreateAssigneeUnassigned,
	}
	src := New(cfg, client)
	src.sleep = func(time.Duration) {}
	return src, fake
}

func boolPtr(v bool) *bool { return &v }

func TestClaimConfigurableBehavior(t *testing.T) {
	t.Run("assign true mark_in_progress false only assigns me", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{
			http.MethodGet + " " + myselfPath:                    {okFixture(t, "myself.json")},
			http.MethodPut + " /rest/api/3/issue/PRJ-1/assignee": {{status: http.StatusNoContent}},
		})
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipAny,
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
		}
		if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 2 {
			t.Fatalf("requests = %d, want 2", len(recs))
		}
		assertEmptyCall(t, recs[0], http.MethodGet, myselfPath)
		assertJSONCall(t, recs[1], http.MethodPut, "/rest/api/3/issue/PRJ-1/assignee", `{"accountId":"`+testAccount+`"}`)
	})

	t.Run("assign false mark_in_progress true transitions and adds optional claim label", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{
			http.MethodGet + " /rest/api/3/issue/PRJ-1":              {jsonBody(http.StatusOK, claimIssueToDo)},
			http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions":  {jsonBody(http.StatusOK, claimTransitions)},
			http.MethodPost + " /rest/api/3/issue/PRJ-1/transitions": {{status: http.StatusNoContent}},
			http.MethodPut + " /rest/api/3/issue/PRJ-1":              {{status: http.StatusNoContent}},
		})
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Status:         "In Progress",
			Label:          "agent-active",
		}
		if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 4 {
			t.Fatalf("requests = %d, want 4", len(recs))
		}
		assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1")
		assertEmptyCall(t, recs[1], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
		assertJSONCall(t, recs[2], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"11"}}`)
		assertJSONCall(t, recs[3], http.MethodPut, "/rest/api/3/issue/PRJ-1", `{"update":{"labels":[{"add":"agent-active"}]}}`)
	})

	t.Run("mark_in_progress true with label only and no status skips transitions", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{
			http.MethodPut + " /rest/api/3/issue/PRJ-1":          {{status: http.StatusNoContent}},
			http.MethodGet + " " + myselfPath:                    {okFixture(t, "myself.json")},
			http.MethodPut + " /rest/api/3/issue/PRJ-1/assignee": {{status: http.StatusNoContent}},
		})
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Status:         "",
			Label:          "agent-active",
		}
		if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 3 {
			t.Fatalf("requests = %d, want 3", len(recs))
		}
		assertJSONCall(t, recs[0], http.MethodPut, "/rest/api/3/issue/PRJ-1", `{"update":{"labels":[{"add":"agent-active"}]}}`)
		assertEmptyCall(t, recs[1], http.MethodGet, myselfPath)
		assertJSONCall(t, recs[2], http.MethodPut, "/rest/api/3/issue/PRJ-1/assignee", `{"accountId":"`+testAccount+`"}`)
	})

	t.Run("assign false and mark_in_progress false is a no-op", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{})
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
		}
		if err := src.Claim(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		if recs := fake.requests(); len(recs) != 0 {
			t.Fatalf("requests = %d, want 0", len(recs))
		}
	})

	t.Run("ClaimWithOptions overrides config", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{
			http.MethodGet + " " + myselfPath:                    {okFixture(t, "myself.json")},
			http.MethodPut + " /rest/api/3/issue/PRJ-1/assignee": {{status: http.StatusNoContent}},
		})
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Status:         "In Progress",
		}
		err := src.ClaimWithOptions(context.Background(), "PRJ-1", source.ClaimOptions{
			Assign:         boolPtr(true),
			MarkInProgress: boolPtr(false),
		})
		if err != nil {
			t.Fatalf("ClaimWithOptions() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 2 {
			t.Fatalf("requests = %d, want 2", len(recs))
		}
		assertEmptyCall(t, recs[0], http.MethodGet, myselfPath)
		assertJSONCall(t, recs[1], http.MethodPut, "/rest/api/3/issue/PRJ-1/assignee", `{"accountId":"`+testAccount+`"}`)
	})
}

func TestReleaseMirrorsClaimConfig(t *testing.T) {
	t.Run("assign false mark_in_progress true removes label and skips unassign", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{
			http.MethodGet + " /rest/api/3/issue/PRJ-1/transitions":  {jsonBody(http.StatusOK, releaseTransitions)},
			http.MethodPost + " /rest/api/3/issue/PRJ-1/transitions": {{status: http.StatusNoContent}},
			http.MethodPut + " /rest/api/3/issue/PRJ-1":              {{status: http.StatusNoContent}},
		})
		src.cfg.ReadyStatuses = []string{"Backlog", "To Do"}
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Status:         "In Progress",
			Label:          "agent-active",
		}
		if err := src.Release(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Release() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 3 {
			t.Fatalf("requests = %d, want 3", len(recs))
		}
		assertEmptyCall(t, recs[0], http.MethodGet, "/rest/api/3/issue/PRJ-1/transitions")
		assertJSONCall(t, recs[1], http.MethodPost, "/rest/api/3/issue/PRJ-1/transitions", `{"transition":{"id":"41"}}`)
		assertJSONCall(t, recs[2], http.MethodPut, "/rest/api/3/issue/PRJ-1", `{"update":{"labels":[{"remove":"agent-active"}]}}`)
	})

	t.Run("assign true mark_in_progress false only clears assignee", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{
			http.MethodPut + " /rest/api/3/issue/PRJ-1/assignee": {{status: http.StatusNoContent}},
		})
		src.cfg.ReadyStatuses = []string{"Backlog", "To Do"}
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
			Label:          "agent-active",
		}
		if err := src.Release(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Release() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 1 {
			t.Fatalf("requests = %d, want 1", len(recs))
		}
		assertJSONCall(t, recs[0], http.MethodPut, "/rest/api/3/issue/PRJ-1/assignee", `{"accountId":null}`)
	})

	t.Run("both disabled is a no-op", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{})
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
		}
		if err := src.Release(context.Background(), "PRJ-1"); err != nil {
			t.Fatalf("Release() error = %v", err)
		}
		if recs := fake.requests(); len(recs) != 0 {
			t.Fatalf("requests = %d, want 0", len(recs))
		}
	})
}

func TestPostClaimComment(t *testing.T) {
	t.Run("posts ADF comment", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{
			http.MethodPost + " /rest/api/3/issue/PRJ-1/comment": {jsonBody(http.StatusCreated, `{"id":"100"}`)},
		})
		if err := src.PostClaimComment(context.Background(), "PRJ-1", "Claimed by claude on mac1"); err != nil {
			t.Fatalf("PostClaimComment() error = %v", err)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 1 {
			t.Fatalf("requests = %d, want 1", len(recs))
		}
		assertJSONCall(t, recs[0], http.MethodPost, "/rest/api/3/issue/PRJ-1/comment",
			`{"body":{"type":"doc","version":1,"content":[{"type":"paragraph","content":[{"type":"text","text":"Claimed by claude on mac1"}]}]}}`)
	})

	t.Run("rejects empty body", func(t *testing.T) {
		src, fake := writeSourceInMemory(t, map[string][]response{})
		if err := src.PostClaimComment(context.Background(), "PRJ-1", "  \t "); err == nil || !strings.Contains(err.Error(), "found empty") {
			t.Fatalf("PostClaimComment() error = %v", err)
		}
		if recs := fake.requests(); len(recs) != 0 {
			t.Fatalf("requests = %d, want 0", len(recs))
		}
	})
}

func TestLatestClaimComment(t *testing.T) {
	t.Run("returns latest ADF claim comment across pages", func(t *testing.T) {
		page1 := `{
			"startAt": 0,
			"maxResults": 1,
			"total": 3,
			"comments": [
				{
					"author": {"accountId": "acct-1"},
					"created": "2026-11-24T01:00:00.000+0000",
					"body": {
						"type": "doc",
						"version": 1,
						"content": [{"type": "paragraph", "content": [{"type": "text", "text": "Claimed by claude on mac1"}]}]
					}
				}
			]
		}`
		page2 := `{
			"startAt": 1,
			"maxResults": 2,
			"total": 3,
			"comments": [
				{
					"author": {"accountId": "acct-2"},
					"created": "2026-11-24T02:00:00.000+0000",
					"body": {
						"type": "doc",
						"version": 1,
						"content": [{"type": "paragraph", "content": [{"type": "text", "text": "Not a claim comment"}]}]
					}
				},
				{
					"author": {"accountId": "acct-3"},
					"created": "2026-11-24T03:00:00.000+0000",
					"body": {
						"type": "doc",
						"version": 1,
						"content": [{"type": "paragraph", "content": [{"type": "text", "text": "Claimed by claude on mac2"}]}]
					}
				}
			]
		}`
		src, fake := writeSourceInMemory(t, map[string][]response{
			http.MethodGet + " /rest/api/3/issue/PRJ-1/comment": {
				jsonBody(http.StatusOK, page1),
				jsonBody(http.StatusOK, page2),
			},
		})
		got, err := src.LatestClaimComment(context.Background(), "PRJ-1")
		if err != nil {
			t.Fatalf("LatestClaimComment() error = %v", err)
		}
		if got == nil {
			t.Fatal("LatestClaimComment() = nil, want comment")
			return
		}
		wantTime := time.Date(2026, 11, 24, 3, 0, 0, 0, time.UTC)
		if got.Author != "acct-3" || !got.CreatedAt.Equal(wantTime) || got.Body != "Claimed by claude on mac2" {
			t.Fatalf("LatestClaimComment() = %+v", got)
		}
		recs := finishWrite(t, fake)
		if len(recs) != 2 {
			t.Fatalf("requests = %d, want 2", len(recs))
		}
		if recs[0].rawQuery != "" || recs[1].rawQuery != "startAt=1" {
			t.Fatalf("queries = %q, %q", recs[0].rawQuery, recs[1].rawQuery)
		}
	})

	t.Run("returns nil when no claim comment exists", func(t *testing.T) {
		body := `{"comments":[{"author":{"accountId":"acct-1"},"created":"2026-11-24T01:00:00.000+0000","body":{"type":"doc","version":1,"content":[{"type":"paragraph","content":[{"type":"text","text":"General update"}]}]}}]}`
		src, _ := writeSourceInMemory(t, map[string][]response{
			http.MethodGet + " /rest/api/3/issue/PRJ-1/comment": {jsonBody(http.StatusOK, body)},
		})
		got, err := src.LatestClaimComment(context.Background(), "PRJ-1")
		if err != nil {
			t.Fatalf("LatestClaimComment() error = %v", err)
		}
		if got != nil {
			t.Fatalf("LatestClaimComment() = %+v, want nil", got)
		}
	})

	t.Run("rejects malformed comments body", func(t *testing.T) {
		body := `{"comments":[{"author":{"accountId":"acct-1"},"created":"2026-11-24T01:00:00.000+0000","body":123}]}`
		src, _ := writeSourceInMemory(t, map[string][]response{
			http.MethodGet + " /rest/api/3/issue/PRJ-1/comment": {jsonBody(http.StatusOK, body)},
		})
		if _, err := src.LatestClaimComment(context.Background(), "PRJ-1"); err == nil || !strings.Contains(err.Error(), "expected ADF object or string") {
			t.Fatalf("LatestClaimComment() error = %v", err)
		}
	})
}
