package scan

import (
	"regexp"
	"testing"
)

func TestExtractID(t *testing.T) {
	t.Parallel()

	defaultRe := regexp.MustCompile(`[A-Z]+-[0-9]+`)

	tests := []struct {
		name  string
		title string
		re    *regexp.Regexp
		want  *string
	}{
		{
			name:  "bracketed jira key",
			title: "[PROJ-123] foo",
			re:    defaultRe,
			want:  strPtr("PROJ-123"),
		},
		{
			name:  "first of two keys",
			title: "PROJ-1 and PROJ-2",
			re:    defaultRe,
			want:  strPtr("PROJ-1"),
		},
		{
			name:  "no match",
			title: "no key here",
			re:    defaultRe,
			want:  nil,
		},
		{
			name:  "custom regex",
			title: "ticket:42 extra",
			re:    regexp.MustCompile(`ticket:\d+`),
			want:  strPtr("ticket:42"),
		},
		{
			name:  "lowercase title with case-insensitive regex",
			title: "fix proj-9 now",
			re:    regexp.MustCompile(`(?i)proj-[0-9]+`),
			want:  strPtr("proj-9"),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := ExtractID(tc.title, tc.re)
			if !sameStringPtr(got, tc.want) {
				t.Fatalf("ExtractID(%q) = %s, want %s", tc.title, formatPtr(got), formatPtr(tc.want))
			}
		})
	}
}

func strPtr(s string) *string { return &s }

func formatPtr(s *string) string {
	if s == nil {
		return "<nil>"
	}
	return *s
}

func sameStringPtr(a, b *string) bool {
	if a == nil || b == nil {
		return a == b
	}
	return *a == *b
}
