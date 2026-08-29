package matcher

import (
	"encoding/json"
	"reflect"
	"testing"

	"github.com/jaeyeom/experimental/devtools/docsync/internal/mapping"
)

func TestMatch(t *testing.T) {
	t.Parallel()

	authRule := mapping.Rule{
		Match: []string{"internal/auth/**/*.go"},
		Docs: []mapping.Doc{{
			Path:    "docs/api-reference.md",
			Section: "## Authentication",
			Why:     "token TTL + claim shape",
		}},
	}
	schemaRule := mapping.Rule{
		Match: []string{"config/schema.go"},
		Docs:  []mapping.Doc{{Path: "docs/configuration.md", Why: "documented option list"}},
	}
	dbRule := mapping.Rule{
		Match: []string{"migrations/*.sql", "db/{models,queries}.go"},
		Docs:  []mapping.Doc{{Path: "docs/data-model.md", Section: "## Schema", Why: "table/column reference"}},
	}

	tests := []struct {
		name    string
		rules   []mapping.Rule
		changed []string
		want    Result
	}{
		{
			name:    "union across rules",
			rules:   []mapping.Rule{authRule, schemaRule},
			changed: []string{"internal/auth/token.go", "config/schema.go"},
			want: Result{
				Affected: []Affected{
					{Path: "docs/api-reference.md", Section: "## Authentication", Why: "token TTL + claim shape", TriggeredBy: []string{"internal/auth/token.go"}},
					{Path: "docs/configuration.md", Why: "documented option list", TriggeredBy: []string{"config/schema.go"}},
				},
				ChangedFiles:   []string{"internal/auth/token.go", "config/schema.go"},
				UnmatchedFiles: []string{},
			},
		},
		{
			name:    "dedup by path and section merges triggered_by",
			rules:   []mapping.Rule{authRule, {Match: []string{"internal/auth/token.go"}, Docs: []mapping.Doc{{Path: "docs/api-reference.md", Section: "## Authentication", Why: "ignored later why"}}}},
			changed: []string{"internal/auth/token.go", "internal/auth/session.go"},
			want: Result{
				Affected: []Affected{
					{Path: "docs/api-reference.md", Section: "## Authentication", Why: "token TTL + claim shape", TriggeredBy: []string{"internal/auth/token.go", "internal/auth/session.go"}},
				},
				ChangedFiles:   []string{"internal/auth/token.go", "internal/auth/session.go"},
				UnmatchedFiles: []string{},
			},
		},
		{
			name:    "doublestar recursive",
			rules:   []mapping.Rule{authRule},
			changed: []string{"internal/auth/nested/foo.go"},
			want: Result{
				Affected: []Affected{
					{Path: "docs/api-reference.md", Section: "## Authentication", Why: "token TTL + claim shape", TriggeredBy: []string{"internal/auth/nested/foo.go"}},
				},
				ChangedFiles:   []string{"internal/auth/nested/foo.go"},
				UnmatchedFiles: []string{},
			},
		},
		{
			name:    "brace expansion",
			rules:   []mapping.Rule{dbRule},
			changed: []string{"db/models.go", "db/queries.go", "migrations/001.sql"},
			want: Result{
				Affected: []Affected{
					{Path: "docs/data-model.md", Section: "## Schema", Why: "table/column reference", TriggeredBy: []string{"db/models.go", "db/queries.go", "migrations/001.sql"}},
				},
				ChangedFiles:   []string{"db/models.go", "db/queries.go", "migrations/001.sql"},
				UnmatchedFiles: []string{},
			},
		},
		{
			name:    "root-anchored exact path does not float",
			rules:   []mapping.Rule{schemaRule},
			changed: []string{"src/config/schema.go", "config/schema.go"},
			want: Result{
				Affected: []Affected{
					{Path: "docs/configuration.md", Why: "documented option list", TriggeredBy: []string{"config/schema.go"}},
				},
				ChangedFiles:   []string{"src/config/schema.go", "config/schema.go"},
				UnmatchedFiles: []string{"src/config/schema.go"},
			},
		},
		{
			name:    "explicit **/ prefix floats",
			rules:   []mapping.Rule{{Match: []string{"**/config/schema.go"}, Docs: []mapping.Doc{{Path: "docs/configuration.md"}}}},
			changed: []string{"config/schema.go", "src/config/schema.go", "config/schema_test.go"},
			want: Result{
				Affected: []Affected{
					{Path: "docs/configuration.md", TriggeredBy: []string{"config/schema.go", "src/config/schema.go"}},
				},
				ChangedFiles:   []string{"config/schema.go", "src/config/schema.go", "config/schema_test.go"},
				UnmatchedFiles: []string{"config/schema_test.go"},
			},
		},
		{
			name:    "leading slash on pattern still matches",
			rules:   []mapping.Rule{{Match: []string{"/config/schema.go"}, Docs: []mapping.Doc{{Path: "docs/configuration.md"}}}},
			changed: []string{"config/schema.go"},
			want: Result{
				Affected:       []Affected{{Path: "docs/configuration.md", TriggeredBy: []string{"config/schema.go"}}},
				ChangedFiles:   []string{"config/schema.go"},
				UnmatchedFiles: []string{},
			},
		},
		{
			name:    "no match lists unmatched",
			rules:   []mapping.Rule{schemaRule},
			changed: []string{"README.md"},
			want: Result{
				Affected:       []Affected{},
				ChangedFiles:   []string{"README.md"},
				UnmatchedFiles: []string{"README.md"},
			},
		},
		{
			name:    "empty changed",
			rules:   []mapping.Rule{schemaRule},
			changed: nil,
			want: Result{
				Affected:       []Affected{},
				ChangedFiles:   []string{},
				UnmatchedFiles: []string{},
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := Match(mapping.Mapping{Rules: tc.rules}, tc.changed)
			if !reflect.DeepEqual(got, tc.want) {
				gb, _ := json.MarshalIndent(got, "", "  ")
				wb, _ := json.MarshalIndent(tc.want, "", "  ")
				t.Fatalf("Match() mismatch\ngot:\n%s\nwant:\n%s", gb, wb)
			}
		})
	}
}
