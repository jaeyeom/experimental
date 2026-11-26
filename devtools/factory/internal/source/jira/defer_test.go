package jira

import (
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestNotBeforeField(t *testing.T) {
	s := &Source{cfg: config.Source{Name: "jira:acme", DeferField: "customfield_123"}}
	tests := []struct {
		name, fields string
		want         string // YYYY-MM-DD, or empty for nil
		wantErr      string
	}{
		{name: "date", fields: `{"customfield_123": "2026-12-01"}`, want: "2026-12-01"},
		{name: "datetime", fields: `{"customfield_123": "2026-12-01T23:30:00.000+0900"}`, want: "2026-12-01"},
		{name: "null", fields: `{"customfield_123": null}`},
		{name: "missing", fields: `{}`},
		{name: "bad", fields: `{"customfield_123": "soon"}`, wantErr: `field "customfield_123" (defer_field): expected YYYY-MM-DD`},
		{name: "not a string", fields: `{"customfield_123": {"value": 1}}`, wantErr: "expected a date or datetime string or null"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var issue jiraIssue
			if err := json.Unmarshal([]byte(`{"key":"PRJ-1","fields":`+tc.fields+`}`), &issue); err != nil {
				t.Fatal(err)
			}
			got, err := s.notBefore(issue)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			switch {
			case tc.want == "" && got != nil:
				t.Fatalf("got %v, want nil", got)
			case tc.want != "" && (got == nil || got.Format(time.DateOnly) != tc.want || got.Location() != time.UTC):
				t.Fatalf("got %v, want %s UTC", got, tc.want)
			}
		})
	}
	if got := s.fields(); got[len(got)-1] != "customfield_123" {
		t.Fatalf("fields = %v, want the defer field requested", got)
	}
	if got := (&Source{}).fields(); len(got) != len(issueFields) {
		t.Fatalf("fields without defer_field = %v", got)
	}
}
