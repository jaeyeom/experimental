package github

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func issueFieldValuesConn(nodes ...any) map[string]any {
	return map[string]any{"nodes": nodeList(nodes...)}
}

func singleSelectFieldVal(fieldName, optionName string) map[string]any {
	return map[string]any{
		"__typename": "IssueFieldSingleSelectValue",
		"name":       optionName,
		"field":      map[string]any{"name": fieldName},
	}
}

func dateFieldVal(fieldName, dateText string) map[string]any {
	return map[string]any{
		"__typename": "IssueFieldDateValue",
		"value":      dateText,
		"field":      map[string]any{"name": fieldName},
	}
}

func starterIssueFieldsJSON(t *testing.T) string {
	t.Helper()
	return mustJSON(t, map[string]any{
		"data": map[string]any{
			"repository": map[string]any{
				"issueFields": map[string]any{
					"nodes": []map[string]any{
						{
							"__typename": "IssueFieldSingleSelect",
							"id":         "IFSS_prio",
							"name":       "Priority",
							"options": []map[string]any{
								{"id": "OPT_urgent", "name": "Urgent"},
								{"id": "OPT_high", "name": "High"},
								{"id": "OPT_medium", "name": "Medium"},
								{"id": "OPT_low", "name": "Low"},
							},
						},
						{
							"__typename": "IssueFieldDate",
							"id":         "IFD_start",
							"name":       "Start date",
						},
						{
							"__typename": "IssueFieldDate",
							"id":         "IFD_target",
							"name":       "Target date",
						},
						{
							"__typename": "IssueFieldSingleSelect",
							"id":         "IFSS_effort",
							"name":       "Effort",
							"options": []map[string]any{
								{"id": "OPT_e_high", "name": "High"},
								{"id": "OPT_e_med", "name": "Medium"},
								{"id": "OPT_e_low", "name": "Low"},
							},
						},
					},
				},
			},
		},
	})
}

func TestSyncMapsNativeIssueFields(t *testing.T) {
	cfg := config.Source{
		Name:             "github:experimental",
		Kind:             "github",
		Repo:             "acme/widgets",
		PriorityField:    config.DefaultGitHubPriorityField,
		DeferField:       config.DefaultGitHubDeferField,
		DueField:         config.DefaultGitHubDueField,
		DeferLabelPrefix: "not-before:",
		PriorityLabels:   map[string]int{"P1": 1},
	}
	nativeIssue := issueNode(1, map[string]any{
		"title":     "With starter fields",
		"milestone": map[string]any{"dueOn": "2026-12-01T00:00:00Z"},
		"labels":    labelConn("P1", "not-before:2026-12-05"),
		"issueFieldValues": issueFieldValuesConn(
			singleSelectFieldVal("Priority", "Urgent"),
			dateFieldVal("Start date", "2026-12-10"),
			dateFieldVal("Target date", "2026-12-20"),
			singleSelectFieldVal("Effort", "Low"),
			map[string]any{"__typename": "IssueFieldTextValue"},
		),
	})
	fallbackIssue := issueNode(2, map[string]any{
		"title":            "Fallback to labels and milestone",
		"milestone":        map[string]any{"dueOn": "2026-12-01T00:00:00Z"},
		"labels":           labelConn("P1", "not-before:2026-12-05"),
		"issueFieldValues": issueFieldValuesConn(),
	})

	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillSucceed(issuesPage(t, false, "", nativeIssue, fallbackIssue), 0).Once().Build()

	got, err := New(cfg, mock, testBin).Sync(context.Background(), time.Time{})
	if err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("len = %d, want 2", len(got))
	}

	// Native fields win over P1 label and milestone dueOn; later not-before wins.
	wantNotBefore := time.Date(2026, 12, 10, 0, 0, 0, 0, time.UTC)
	wantDue := time.Date(2026, 12, 20, 0, 0, 0, 0, time.UTC)
	if got[0].Priority != 0 || !sameDue(got[0].NotBefore, &wantNotBefore) || !sameDue(got[0].Due, &wantDue) {
		t.Fatalf("native item = pri:%d notBefore:%v due:%v", got[0].Priority, got[0].NotBefore, got[0].Due)
	}

	// Unset native fields fall back cleanly to labels and milestone.dueOn.
	wantFallbackNB := time.Date(2026, 12, 5, 0, 0, 0, 0, time.UTC)
	wantFallbackDue := time.Date(2026, 12, 1, 0, 0, 0, 0, time.UTC)
	if got[1].Priority != 1 || !sameDue(got[1].NotBefore, &wantFallbackNB) || !sameDue(got[1].Due, &wantFallbackDue) {
		t.Fatalf("fallback item = pri:%d notBefore:%v due:%v", got[1].Priority, got[1].NotBefore, got[1].Due)
	}
}

func TestSyncRejectsMalformedFieldDate(t *testing.T) {
	cfg := config.Source{
		Name:       "github:experimental",
		Kind:       "github",
		Repo:       "acme/widgets",
		DeferField: config.DefaultGitHubDeferField,
	}
	node := issueNode(7, map[string]any{
		"issueFieldValues": issueFieldValuesConn(dateFieldVal("Start date", "tomorrow")),
	})
	mock := newMock()
	mock.ExpectCommandWithArgs(testBin, fullSyncArgs("")...).
		WillSucceed(issuesPage(t, false, "", node), 0).Once().Build()
	if _, err := New(cfg, mock, testBin).Sync(context.Background(), time.Time{}); err == nil || !strings.Contains(err.Error(), `field "Start date"`) {
		t.Fatalf("error = %v", err)
	}
}

func TestSetPriorityNativeField(t *testing.T) {
	t.Run("writes single-select Priority option and caches repo fields", func(t *testing.T) {
		mock := newMock()
		cfg := config.Source{
			Name:          "github:experimental",
			Kind:          "github",
			Repo:          "acme/widgets",
			PriorityField: config.DefaultGitHubPriorityField,
		}
		src := New(cfg, mock, testBin)

		fieldsCall := issueFieldsArgs("acme", "widgets")
		get7 := getArgs("acme", "widgets", 7)
		set7 := setIssueFieldValueArgs("I_node7", "IFSS_prio", "OPT_urgent")
		setResp := `{"data":{"setIssueFieldValue":{"issue":{"id":"I_node7"}}}}`

		mock.ExpectCommandWithArgs(testBin, fieldsCall...).WillSucceed(starterIssueFieldsJSON(t), 0).Once().Build()
		mock.ExpectCommandWithArgs(testBin, get7...).WillSucceed(
			issueResponse(t, issueNode(7, map[string]any{"id": "I_node7"})),
			0,
		).Once().Build()
		mock.ExpectCommandWithArgs(testBin, set7...).WillSucceed(setResp, 0).Once().Build()

		if err := src.SetPriority(context.Background(), "acme/widgets#7", 0); err != nil {
			t.Fatalf("SetPriority(0) error = %v", err)
		}

		// Second call reuses cached issueFields.
		get8 := getArgs("acme", "widgets", 8)
		set8 := setIssueFieldValueArgs("I_node8", "IFSS_prio", "OPT_high")
		mock.ExpectCommandWithArgs(testBin, get8...).WillSucceed(
			issueResponse(t, issueNode(8, map[string]any{"id": "I_node8"})),
			0,
		).Once().Build()
		mock.ExpectCommandWithArgs(testBin, set8...).WillSucceed(`{"data":{"setIssueFieldValue":{"issue":{"id":"I_node8"}}}}`, 0).Once().Build()

		if err := src.SetPriority(context.Background(), "acme/widgets#8", 1); err != nil {
			t.Fatalf("SetPriority(1) error = %v", err)
		}
		assertArgs(t, mock, [][]string{fieldsCall, get7, set7, get8, set8})
	})

	t.Run("fails loud when Priority field is missing on repo", func(t *testing.T) {
		mock := newMock()
		cfg := config.Source{
			Name:          "github:experimental",
			Kind:          "github",
			Repo:          "acme/widgets",
			PriorityField: config.DefaultGitHubPriorityField,
		}
		fieldsCall := issueFieldsArgs("acme", "widgets")
		mock.ExpectCommandWithArgs(testBin, fieldsCall...).WillSucceed(
			`{"data":{"repository":{"issueFields":{"nodes":[]}}}}`, 0,
		).Once().Build()

		err := New(cfg, mock, testBin).SetPriority(context.Background(), "acme/widgets#7", 0)
		if err == nil || !strings.Contains(err.Error(), `no single-select issue field "Priority"`) {
			t.Fatalf("error = %v", err)
		}
	})

	t.Run("fails loud when option name is not in repo Priority field", func(t *testing.T) {
		mock := newMock()
		cfg := config.Source{
			Name:          "github:experimental",
			Kind:          "github",
			Repo:          "acme/widgets",
			PriorityField: config.DefaultGitHubPriorityField,
			PriorityRanks: map[string]int{"Urgent": 9, "P0": 0},
		}
		fieldsCall := issueFieldsArgs("acme", "widgets")
		mock.ExpectCommandWithArgs(testBin, fieldsCall...).WillSucceed(starterIssueFieldsJSON(t), 0).Once().Build()

		err := New(cfg, mock, testBin).SetPriority(context.Background(), "acme/widgets#7", 0)
		if err == nil || !strings.Contains(err.Error(), `found "P0"`) || !strings.Contains(err.Error(), "Urgent") {
			t.Fatalf("error = %v", err)
		}
	})
}
