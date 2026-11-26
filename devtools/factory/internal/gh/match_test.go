package gh

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestTitleMatchesMetacharacters(t *testing.T) {
	t.Parallel()

	const pattern = `\[{id}\]`
	tests := []struct {
		name    string
		id      string
		title   string
		want    bool
		wantErr []string
	}{
		{name: "literal plus and dot", id: `A.P-1+`, title: `fix [A.P-1+] now`, want: true},
		{name: "dot is not a wildcard", id: `A.P-1+`, title: `fix [AxP-1+] now`, want: false},
		{name: "plus is not a quantifier", id: `A.P-1+`, title: `fix [A.P-11] now`, want: false},
		{name: "brackets around id", id: "PRJ-1", title: "prefix [PRJ-1] suffix", want: true},
		{name: "different id", id: "PRJ-1", title: "prefix [PRJ-10] suffix", want: false},
		{name: "invalid pattern", id: "PRJ-1", title: "prefix [PRJ-1]", wantErr: []string{"expected", "found", "regexp", "[{id}"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			pattern := pattern
			if tc.wantErr != nil {
				pattern = "[{id}"
			}
			got, err := TitleMatches(pattern, tc.id, tc.title)
			if len(tc.wantErr) > 0 {
				if err == nil {
					t.Fatal("TitleMatches() error = nil, want an error")
				}
				msg := err.Error()
				for _, part := range tc.wantErr {
					if !strings.Contains(msg, part) {
						t.Fatalf("TitleMatches() error = %q, want it to contain %q", msg, part)
					}
				}
				return
			}
			if err != nil {
				t.Fatalf("TitleMatches() unexpected error: %v", err)
			}
			if got != tc.want {
				t.Fatalf("TitleMatches(%q, %q) = %v, want %v", tc.id, tc.title, got, tc.want)
			}
		})
	}
}

func TestForItem(t *testing.T) {
	t.Parallel()

	prs := []PR{
		{Number: 1, Title: "fix [PRJ-10] and more", Repo: "acme/widgets"},
		{Number: 2, Title: "fix [A.P-1+] now", Repo: "acme/widgets"},
		{Number: 3, Title: "unrelated", ClosingIssues: []string{"acme/widgets#9", "acme/other#3"}},
		{Number: 4, Title: "[acme/widgets#9] in the title", ClosingIssues: []string{"acme/widgets#10"}},
		{Number: 5, Title: "fix [PRJ-1] now", Repo: "acme/widgets"},
	}

	t.Run("jira matches title", func(t *testing.T) {
		t.Parallel()
		got := ForItem(prs, source.Item{ID: `A.P-1+`}, `\[{id}\]`)
		if len(got) != 1 || got[0].Number != 2 {
			t.Fatalf("ForItem() = %+v, want PR 2", got)
		}
	})

	t.Run("github matches closing issues", func(t *testing.T) {
		t.Parallel()
		got := ForItem(prs, source.Item{ID: "acme/widgets#9"}, `[`)
		if len(got) != 1 || got[0].Number != 3 {
			t.Fatalf("ForItem() = %+v, want PR 3", got)
		}
	})

	t.Run("github id is not a title match", func(t *testing.T) {
		t.Parallel()
		got := ForItem(prs, source.Item{ID: "acme/widgets#10"}, `\[{id}\]`)
		if len(got) != 1 || got[0].Number != 4 {
			t.Fatalf("ForItem() = %+v, want PR 4", got)
		}
	})

	t.Run("no match is an empty slice", func(t *testing.T) {
		t.Parallel()
		got := ForItem(prs, source.Item{ID: "PRJ-99"}, `\[{id}\]`)
		if got == nil || len(got) != 0 {
			t.Fatalf("ForItem() = %#v, want empty slice", got)
		}
	})
}

func TestForItemInvalidPatternPanics(t *testing.T) {
	t.Parallel()

	defer func() {
		recovered := recover()
		if recovered == nil {
			t.Fatal("ForItem() recovered = nil, want a panic")
		}
		msg := recoveredString(recovered)
		if !strings.Contains(msg, "regexp") {
			t.Fatalf("panic = %q, want it to name the regexp", msg)
		}
	}()
	ForItem([]PR{{Title: "anything"}}, source.Item{ID: "PRJ-1"}, `[`)
}

func TestHeadContains(t *testing.T) {
	t.Parallel()

	prs := []PR{
		{Repo: "acme/widgets", Number: 1, Head: "feature/PRJ-1-fix", Title: "unrelated"},
		{Repo: "acme/widgets", Number: 2, Head: "feature/PRJ-2", Title: "other"},
		{Repo: "acme/widgets", Number: 3, Head: "main", Title: "[PRJ-1] titled"},
	}
	got := HeadContains(prs, "PRJ-1")
	if len(got) != 1 || got[0].Number != 1 {
		t.Fatalf("HeadContains() = %+v", got)
	}
	if empty := HeadContains(prs, ""); empty == nil || len(empty) != 0 {
		t.Fatalf("empty id = %#v, want empty slice", empty)
	}
}

func TestMatches(t *testing.T) {
	t.Parallel()

	prs := []PR{
		{Repo: "acme/widgets", Number: 1, URL: "https://github.com/acme/widgets/pull/1", Title: "unrelated", Head: "other", ClosingIssues: []string{"acme/widgets#9"}},
		{Repo: "acme/widgets", Number: 2, URL: "https://github.com/acme/widgets/pull/2", Title: "also unrelated", Head: "fix/acme/widgets#10-bug"},
		{Repo: "acme/widgets", Number: 3, URL: "https://github.com/acme/widgets/pull/3", Title: "[acme/widgets#11] titled", Head: "main"},
	}

	t.Run("closing issues with empty pattern", func(t *testing.T) {
		t.Parallel()
		got, err := Matches(prs, "acme/widgets#9", nil, "")
		if err != nil {
			t.Fatalf("Matches() error = %v", err)
		}
		if len(got) != 1 || got[0].Number != 1 {
			t.Fatalf("Matches() = %+v, want PR 1", got)
		}
	})

	t.Run("head substring with empty pattern", func(t *testing.T) {
		t.Parallel()
		got, err := Matches(prs, "acme/widgets#10", nil, "")
		if err != nil {
			t.Fatalf("Matches() error = %v", err)
		}
		if len(got) != 1 || got[0].Number != 2 {
			t.Fatalf("Matches() = %+v, want PR 2", got)
		}
	})

	t.Run("explicit pattern matches title", func(t *testing.T) {
		t.Parallel()
		got, err := Matches(prs, "acme/widgets#11", nil, `\[{id}\]`)
		if err != nil {
			t.Fatalf("Matches() error = %v", err)
		}
		if len(got) != 1 || got[0].Number != 3 {
			t.Fatalf("Matches() = %+v, want PR 3", got)
		}
	})

	t.Run("no match with empty pattern returns empty without error", func(t *testing.T) {
		t.Parallel()
		got, err := Matches(prs, "acme/widgets#99", nil, "")
		if err != nil {
			t.Fatalf("Matches() error = %v", err)
		}
		if len(got) != 0 {
			t.Fatalf("Matches() = %+v, want empty", got)
		}
	})
}

func recoveredString(recovered any) string {
	switch v := recovered.(type) {
	case string:
		return v
	case error:
		return v.Error()
	default:
		return ""
	}
}
