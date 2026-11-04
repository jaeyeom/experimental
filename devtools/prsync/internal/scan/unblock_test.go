package scan

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/jira"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestUnblockReports(t *testing.T) {
	mergedAt := "2026-08-18T22:19:28Z"
	closed := func(ticket string) CloseItem {
		return CloseItem{
			TabID: "w2:tA", Ticket: ticket, Action: ActionClosed,
			PR: &OrphanPR{State: "merged", MergedAt: &mergedAt},
		}
	}
	unmerged := CloseItem{
		TabID: "w2:tA", Ticket: "AP-1", Action: ActionClosed,
		PR: &OrphanPR{State: "closed"},
	}
	failedClose := CloseItem{TabID: "w2:tB", Ticket: "AP-9", Action: ActionFailed, Detail: "tab close failed"}

	tests := []struct {
		name      string
		results   []CloseItem
		issues    map[string]string
		fail      bool
		missing   bool
		want      UnblockReport
		wantCalls []string
	}{
		{
			name:    "one ticket clear to start",
			results: []CloseItem{closed("AP-1")},
			issues: map[string]string{
				"AP-1": jiraIssue("AP-1", "Land the blocker", "indeterminate", blocksLink("AP-2", "Add the button", "indeterminate")),
				"AP-2": jiraIssue("AP-2", "Add the button", "indeterminate", blockedByLink("AP-1", "Land the blocker", "indeterminate")),
			},
			want: UnblockReport{
				Status: UnblockOK,
				Text:   "Newly unblocked by this round\nAP-2 \"Add the button\" — was blocked only by AP-1 (just merged); now clear to start",
				Lines: []UnblockLine{{
					Kind: UnblockClear, Ticket: "AP-2", Summary: "Add the button", BlockedBy: []string{"AP-1"},
				}},
			},
			wantCalls: []string{"AP-1", "AP-2"},
		},
		{
			name:    "sibling still blocked by another open ticket",
			results: []CloseItem{closed("AP-1")},
			issues: map[string]string{
				"AP-1": jiraIssue("AP-1", "Land the blocker", "indeterminate",
					blocksLink("AP-2", "Add the button", "indeterminate"),
					blocksLink("AP-4", "Still waiting", "indeterminate"),
				),
				"AP-2": jiraIssue("AP-2", "Add the button", "indeterminate", blockedByLink("AP-1", "Land the blocker", "indeterminate")),
				"AP-4": jiraIssue("AP-4", "Still waiting", "indeterminate",
					blockedByLink("AP-1", "Land the blocker", "indeterminate"),
					blockedByLink("AP-9", "Other work", "indeterminate"),
				),
			},
			want: UnblockReport{
				Status: UnblockOK,
				Text: "Newly unblocked by this round\n" +
					"AP-2 \"Add the button\" — was blocked only by AP-1 (just merged); now clear to start\n" +
					"AP-4 \"Still waiting\" still blocked by AP-9",
				Lines: []UnblockLine{
					{Kind: UnblockClear, Ticket: "AP-2", Summary: "Add the button", BlockedBy: []string{"AP-1"}},
					{Kind: UnblockStillBlocked, Ticket: "AP-4", Summary: "Still waiting", BlockedBy: []string{"AP-9"}},
				},
			},
		},
		{
			name:    "done ticket is omitted",
			results: []CloseItem{closed("AP-1")},
			issues: map[string]string{
				"AP-1": jiraIssue("AP-1", "Land the blocker", "indeterminate", blocksLink("AP-2", "Already shipped", "done")),
			},
			want:      okNothing(),
			wantCalls: []string{"AP-1"},
		},
		{
			name:      "null merged_at is not delivered",
			results:   []CloseItem{unmerged},
			want:      okNothing(),
			wantCalls: []string{},
		},
		{
			name:    "two delivered tickets were the only blockers",
			results: []CloseItem{closed("AP-1"), closed("AP-3")},
			issues: map[string]string{
				"AP-1": jiraIssue("AP-1", "First", "indeterminate", blocksLink("AP-5", "Start this", "indeterminate")),
				"AP-3": jiraIssue("AP-3", "Second", "indeterminate", blocksLink("AP-5", "Start this", "indeterminate")),
				"AP-5": jiraIssue("AP-5", "Start this", "indeterminate",
					blockedByLink("AP-3", "Second", "indeterminate"),
					blockedByLink("AP-1", "First", "indeterminate"),
				),
			},
			want: UnblockReport{
				Status: UnblockOK,
				Text:   "Newly unblocked by this round\nAP-5 \"Start this\" — was blocked only by AP-1, AP-3 (just merged); now clear to start",
				Lines: []UnblockLine{{
					Kind: UnblockClear, Ticket: "AP-5", Summary: "Start this", BlockedBy: []string{"AP-1", "AP-3"},
				}},
			},
		},
		{
			name:      "no closed row does not run",
			results:   []CloseItem{{TabID: "w2:tA", Ticket: "AP-1", Action: ActionWouldClose}},
			want:      notRun(),
			wantCalls: []string{},
		},
		{
			name:    "jira view failure",
			results: []CloseItem{closed("AP-1")},
			fail:    true,
			want: UnblockReport{
				Status: UnblockFailed,
				Text:   "unblock check could not run: jira issue view AP-1: jira-fake: view failed",
				Lines:  []UnblockLine{},
			},
		},
		{
			name:    "link missing type",
			results: []CloseItem{closed("AP-1")},
			issues: map[string]string{
				"AP-1": `{"key":"AP-1","fields":{"summary":"Land","status":{"statusCategory":{"key":"indeterminate"}},"issuelinks":[{"id":"1"}]}}`,
			},
			want: UnblockReport{
				Status: UnblockFailed,
				Lines:  []UnblockLine{},
			},
		},
		{
			name:    "fetched issue missing status",
			results: []CloseItem{closed("AP-1")},
			issues: map[string]string{
				"AP-1": jiraIssue("AP-1", "Land the blocker", "indeterminate", blocksLink("AP-2", "Add the button", "")),
				"AP-2": `{"key":"AP-2","fields":{"summary":"Add the button","issuelinks":[]}}`,
			},
			want: UnblockReport{
				Status: UnblockFailed,
				Text:   "unblock check could not run: issue AP-2 missing status",
				Lines:  []UnblockLine{},
			},
		},
		{
			name:    "missing jira binary",
			results: []CloseItem{closed("AP-1")},
			missing: true,
			want: UnblockReport{
				Status: UnblockFailed,
				Lines:  []UnblockLine{},
			},
		},
		{
			name:    "later close failure still reports the row that closed",
			results: []CloseItem{closed("AP-1"), failedClose},
			issues: map[string]string{
				"AP-1": jiraIssue("AP-1", "Land the blocker", "indeterminate", blocksLink("AP-2", "Add the button", "indeterminate")),
				"AP-2": jiraIssue("AP-2", "Add the button", "indeterminate", blockedByLink("AP-1", "Land the blocker", "indeterminate")),
			},
			want: UnblockReport{
				Status: UnblockOK,
				Text:   "Newly unblocked by this round\nAP-2 \"Add the button\" — was blocked only by AP-1 (just merged); now clear to start",
				Lines: []UnblockLine{{
					Kind: UnblockClear, Ticket: "AP-2", Summary: "Add the button", BlockedBy: []string{"AP-1"},
				}},
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			dir := t.TempDir()
			logPath := filepath.Join(dir, "jira.log")
			issueDir := filepath.Join(dir, "issues")
			if err := os.MkdirAll(issueDir, 0o755); err != nil {
				t.Fatal(err)
			}
			for key, body := range tc.issues {
				if err := os.WriteFile(filepath.Join(issueDir, key+".json"), []byte(body), 0o600); err != nil {
					t.Fatal(err)
				}
			}
			t.Setenv("JIRA_FAKE_LOG", logPath)
			t.Setenv("JIRA_FAKE_DIR", issueDir)
			if tc.fail {
				t.Setenv("JIRA_FAKE_FAIL", "1")
			} else {
				t.Setenv("JIRA_FAKE_FAIL", "")
			}
			if tc.issues != nil && !tc.fail && !tc.missing {
				t.Setenv("JIRA_FAKE_STRICT", "1")
			} else {
				t.Setenv("JIRA_FAKE_STRICT", "")
			}

			bin := writeJiraFake(t)
			if tc.missing {
				bin = filepath.Join(dir, "missing-jira")
			}
			doc := CloseDocument{DryRun: false, Results: tc.results}
			AttachUnblock(context.Background(), jira.NewClient(executor.NewBasicExecutor(), bin), &doc)
			if doc.Unblock.Status != tc.want.Status {
				t.Fatalf("status = %q, want %q (text %q)", doc.Unblock.Status, tc.want.Status, doc.Unblock.Text)
			}
			if tc.want.Text != "" && doc.Unblock.Text != tc.want.Text {
				t.Fatalf("text = %q, want %q", doc.Unblock.Text, tc.want.Text)
			}
			if tc.want.Status == UnblockFailed && len(doc.Unblock.Lines) != 0 {
				t.Fatalf("lines = %+v, want none", doc.Unblock.Lines)
			}
			if tc.want.Lines != nil && !unblockLinesEqual(doc.Unblock.Lines, tc.want.Lines) {
				t.Fatalf("lines = %+v, want %+v", doc.Unblock.Lines, tc.want.Lines)
			}
			gotCalls := readLines(t, logPath)
			if tc.wantCalls != nil && !sameStrings(gotCalls, tc.wantCalls) {
				t.Fatalf("jira calls = %v, want %v", gotCalls, tc.wantCalls)
			}
		})
	}
}

func notRun() UnblockReport {
	return UnblockReport{Status: UnblockNotRun, Text: "", Lines: []UnblockLine{}}
}

func unblockLinesEqual(got, want []UnblockLine) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range got {
		if got[i].Kind != want[i].Kind || got[i].Ticket != want[i].Ticket || got[i].Summary != want[i].Summary {
			return false
		}
		if !sameStrings(got[i].BlockedBy, want[i].BlockedBy) {
			return false
		}
	}
	return true
}

func sameStrings(got, want []string) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range got {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}

func readLines(t *testing.T, path string) []string {
	t.Helper()
	body, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return []string{}
	}
	if err != nil {
		t.Fatal(err)
	}
	if len(body) == 0 {
		return []string{}
	}
	text := string(body)
	if text[len(text)-1] == '\n' {
		text = text[:len(text)-1]
	}
	if text == "" {
		return []string{}
	}
	var out []string
	start := 0
	for i := 0; i < len(text); i++ {
		if text[i] == '\n' {
			out = append(out, text[start:i])
			start = i + 1
		}
	}
	out = append(out, text[start:])
	return out
}

func writeJiraFake(t *testing.T) string {
	t.Helper()
	src := filepath.Join("testdata", "fixtures", "jira-fake.sh")
	body, err := os.ReadFile(src)
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(t.TempDir(), "jira")
	if err := os.WriteFile(path, body, 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, 0o755); err != nil { //nolint:gosec // fake jira must be executable
		t.Fatal(err)
	}
	return path
}

func jiraIssue(key, summary, category string, links ...string) string {
	joined := ""
	for i, link := range links {
		if i > 0 {
			joined += ","
		}
		joined += link
	}
	status := ""
	if category != "" {
		status = `,"status":{"name":"` + category + `","statusCategory":{"key":"` + category + `"}}`
	}
	return `{"key":"` + key + `","fields":{"summary":"` + summary + `"` + status + `,"issuelinks":[` + joined + `]}}`
}

func blocksLink(key, summary, category string) string {
	return linkSide("outwardIssue", key, summary, category)
}

func blockedByLink(key, summary, category string) string {
	return linkSide("inwardIssue", key, summary, category)
}

func linkSide(field, key, summary, category string) string {
	status := ""
	if category != "" {
		status = `,"status":{"name":"` + category + `","statusCategory":{"key":"` + category + `"}}`
	}
	return `{"type":{"name":"Blocks","inward":"is blocked by","outward":"blocks"},"` + field + `":{"key":"` + key + `","fields":{"summary":"` + summary + `"` + status + `}}}`
}
