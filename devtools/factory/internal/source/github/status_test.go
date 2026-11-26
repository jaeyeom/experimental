package github

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func planningStatusesJSON(t *testing.T, statuses ...map[string]any) string {
	t.Helper()
	if statuses == nil {
		statuses = []map[string]any{}
	}
	return mustJSON(t, map[string]any{
		"data": map[string]any{
			"repository": map[string]any{
				"planningStatuses": map[string]any{
					"nodes": statuses,
				},
			},
		},
	})
}

func defaultPlanningStatusesJSON(t *testing.T) string {
	t.Helper()
	return planningStatusesJSON(t,
		map[string]any{"id": "IS_kgAC", "name": "Backlog"},
		map[string]any{"id": "IS_kgAD", "name": "Todo"},
		map[string]any{"id": "IS_kgAE", "name": "In progress"},
		map[string]any{"id": "IS_kgAI", "name": "In review"},
		map[string]any{"id": "IS_kgAF", "name": "Done"},
		map[string]any{"id": "IS_kgAG", "name": "Not planned"},
		map[string]any{"id": "IS_kgAH", "name": "Duplicate"},
	)
}

func issueResponse(t *testing.T, node map[string]any) string {
	t.Helper()
	return mustJSON(t, map[string]any{
		"data": map[string]any{
			"repository": map[string]any{
				"issue": node,
			},
		},
	})
}

func setStatusJSON(t *testing.T, nodeID, statusName string) string {
	t.Helper()
	return mustJSON(t, map[string]any{
		"data": map[string]any{
			"setIssueStatus": map[string]any{
				"issue": map[string]any{
					"id":     nodeID,
					"status": map[string]any{"name": statusName},
				},
			},
		},
	})
}

func TestSyncMapsNativeIssueStatus(t *testing.T) {
	todoIssue := issueNode(1, map[string]any{
		"title":  "Ready work",
		"state":  "OPEN",
		"status": map[string]any{"name": "Todo"},
	})
	inProgressIssue := issueNode(2, map[string]any{
		"title":  "Active work",
		"state":  "OPEN",
		"status": map[string]any{"name": "In progress"},
	})
	doneIssue := issueNode(3, map[string]any{
		"title":  "Finished work",
		"state":  "CLOSED",
		"status": map[string]any{"name": "Done"},
	})
	notPlannedIssue := issueNode(4, map[string]any{
		"title":  "Skipped work",
		"state":  "CLOSED",
		"status": map[string]any{"name": "Not planned"},
	})
	nullOpenIssue := issueNode(5, map[string]any{
		"title":  "Legacy open",
		"state":  "OPEN",
		"status": nil,
	})

	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillSucceed(issuesPage(t, false, "", todoIssue, inProgressIssue, doneIssue, notPlannedIssue, nullOpenIssue), 0).Once().Build()

	got, err := newTestSource(mock).Sync(context.Background(), time.Time{})
	if err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	want := []struct {
		id    string
		state string
		done  bool
	}{
		{"acme/widgets#1", "Todo", false},
		{"acme/widgets#2", "In progress", false},
		{"acme/widgets#3", "Done", true},
		{"acme/widgets#4", "Not planned", true},
		{"acme/widgets#5", "OPEN", false},
	}
	if len(got) != len(want) {
		t.Fatalf("len = %d, want %d", len(got), len(want))
	}
	for i, w := range want {
		if got[i].ID != w.id || got[i].State != w.state || got[i].Done != w.done {
			t.Errorf("got[%d] = (%s, %q, done=%v), want (%s, %q, done=%v)",
				i, got[i].ID, got[i].State, got[i].Done, w.id, w.state, w.done)
		}
	}
}

func TestSyncRejectsMalformedStatus(t *testing.T) {
	tests := []struct {
		name   string
		status any
	}{
		{name: "string instead of object", status: "Todo"},
		{name: "empty status name", status: map[string]any{"name": ""}},
		{name: "missing name", status: map[string]any{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			mock := newMock()
			node := issueNode(7, map[string]any{"state": "OPEN", "status": tc.status})
			mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
				WillSucceed(issuesPage(t, false, "", node), 0).Once().Build()
			if _, err := newTestSource(mock).Sync(context.Background(), time.Time{}); err == nil {
				t.Fatalf("expected error for status %#v", tc.status)
			}
		})
	}
}

func TestClaimAndReleaseNativeStatus(t *testing.T) {
	t.Run("claim assigns first then sets native status", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.ReadyStatuses = config.DefaultGitHubReadyStatuses()
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Status:         config.DefaultGitHubClaimStatus,
		}

		edit := src.editArgs(7, "--add-assignee", "@me")
		ps := planningStatusesArgs("acme", "widgets")
		get := getArgs("acme", "widgets", 7)
		set := setIssueStatusArgs("I_node7", "IS_kgAE")

		mock.ExpectCommandWithArgs(testBin, edit...).WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs(testBin, ps...).WillSucceed(defaultPlanningStatusesJSON(t), 0).Once().Build()
		mock.ExpectCommandWithArgs(testBin, get...).WillSucceed(
			issueResponse(t, issueNode(7, map[string]any{"id": "I_node7", "state": "OPEN", "status": map[string]any{"name": "Todo"}})),
			0,
		).Once().Build()
		mock.ExpectCommandWithArgs(testBin, set...).WillSucceed(setStatusJSON(t, "I_node7", "In progress"), 0).Once().Build()

		if err := src.Claim(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		assertArgs(t, mock, [][]string{edit, ps, get, set})

		// Second operation reuses cached planning statuses: release unassigns and moves to Todo.
		relEdit := src.editArgs(7, "--remove-assignee", "@me")
		relSet := setIssueStatusArgs("I_node7", "IS_kgAD")
		mock.ExpectCommandWithArgs(testBin, relEdit...).WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs(testBin, get...).WillSucceed(
			issueResponse(t, issueNode(7, map[string]any{"id": "I_node7", "state": "OPEN", "status": map[string]any{"name": "In progress"}})),
			0,
		).Once().Build()
		mock.ExpectCommandWithArgs(testBin, relSet...).WillSucceed(setStatusJSON(t, "I_node7", "Todo"), 0).Once().Build()

		if err := src.Release(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Release() error = %v", err)
		}
		assertArgs(t, mock, [][]string{edit, ps, get, set, relEdit, get, relSet})
	})

	t.Run("release uses first non-OPEN ready_statuses entry", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.ReadyStatuses = []string{"OPEN", "Backlog", "Todo"}
		src.cfg.Claim = config.Claim{
			Assign:         false,
			MarkInProgress: true,
			Status:         "In progress",
		}
		ps := planningStatusesArgs("acme", "widgets")
		get := getArgs("acme", "widgets", 7)
		set := setIssueStatusArgs("I_node7", "IS_kgAC")

		mock.ExpectCommandWithArgs(testBin, ps...).WillSucceed(defaultPlanningStatusesJSON(t), 0).Once().Build()
		mock.ExpectCommandWithArgs(testBin, get...).WillSucceed(
			issueResponse(t, issueNode(7, map[string]any{"id": "I_node7", "state": "OPEN", "status": map[string]any{"name": "In progress"}})),
			0,
		).Once().Build()
		mock.ExpectCommandWithArgs(testBin, set...).WillSucceed(setStatusJSON(t, "I_node7", "Backlog"), 0).Once().Build()

		if err := src.Release(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Release() error = %v", err)
		}
		assertArgs(t, mock, [][]string{ps, get, set})
	})

	t.Run("claim skips setIssueStatus when already in target status", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         false,
			MarkInProgress: true,
			Status:         "In progress",
		}
		ps := planningStatusesArgs("acme", "widgets")
		get := getArgs("acme", "widgets", 7)

		mock.ExpectCommandWithArgs(testBin, ps...).WillSucceed(defaultPlanningStatusesJSON(t), 0).Once().Build()
		mock.ExpectCommandWithArgs(testBin, get...).WillSucceed(
			issueResponse(t, issueNode(7, map[string]any{"id": "I_node7", "state": "OPEN", "status": map[string]any{"name": "In progress"}})),
			0,
		).Once().Build()

		if err := src.Claim(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		assertArgs(t, mock, [][]string{ps, get})
	})

	t.Run("claim fails loud when configured status is not in repository", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         false,
			MarkInProgress: true,
			Status:         "Doing",
		}
		ps := planningStatusesArgs("acme", "widgets")
		mock.ExpectCommandWithArgs(testBin, ps...).WillSucceed(defaultPlanningStatusesJSON(t), 0).Once().Build()

		err := src.Claim(context.Background(), "acme/widgets#7")
		if err == nil || !strings.Contains(err.Error(), `found "Doing"`) || !strings.Contains(err.Error(), "In progress") {
			t.Fatalf("Claim() error = %v", err)
		}
	})

	t.Run("claim fails loud when repository has no planning statuses", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         false,
			MarkInProgress: true,
			Status:         "In progress",
		}
		ps := planningStatusesArgs("acme", "widgets")
		mock.ExpectCommandWithArgs(testBin, ps...).WillSucceed(planningStatusesJSON(t), 0).Once().Build()

		err := src.Claim(context.Background(), "acme/widgets#7")
		if err == nil || !strings.Contains(err.Error(), "no native issue planning statuses") {
			t.Fatalf("Claim() error = %v", err)
		}
	})

	t.Run("claim options mark_in_progress false skips status update", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         true,
			MarkInProgress: true,
			Status:         "In progress",
		}
		edit := src.editArgs(7, "--add-assignee", "@me")
		mock.ExpectCommandWithArgs(testBin, edit...).WillSucceed("", 0).Once().Build()

		err := src.ClaimWithOptions(context.Background(), "acme/widgets#7", source.ClaimOptions{
			MarkInProgress: boolPtr(false),
		})
		if err != nil {
			t.Fatalf("ClaimWithOptions() error = %v", err)
		}
		assertArgs(t, mock, [][]string{edit})
	})
}
