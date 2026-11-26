package verify

import (
	"slices"
	"strings"
	"testing"
)

func sectionNames(t *testing.T, text string) []string {
	t.Helper()
	parts, err := splitSections(text)
	if err != nil {
		t.Fatal(err)
	}
	var out []string
	for _, p := range parts {
		out = append(out, p.heading)
	}
	return out
}

func TestSplitSectionsStyles(t *testing.T) {
	tests := []struct {
		name, text string
		want       []string
	}{
		{name: "atx levels", text: "# One\nx\n### Three ###\ny\n", want: []string{"One", "Three"}},
		{name: "setext", text: "Summary\n=======\nshipped\n\nTest plan\n---------\ngo test\n", want: []string{"Summary", "Test plan"}},
		{name: "mixed", text: "## Summary\nshipped\n\nTest plan\n--\ngo test\n", want: []string{"Summary", "Test plan"}},
		{name: "thematic break after blank", text: "## Summary\nshipped\n\n---\n\nmore\n", want: []string{"Summary"}},
		{name: "list item is not a title", text: "## Summary\n- item\n---\n", want: []string{"Summary"}},
		{name: "fenced code", text: "## Summary\n```\n## Not a heading\nTitle\n=====\n```\n~~~go\n# nope\n~~~\n", want: []string{"Summary"}},
		{name: "hash without space", text: "#hashtag\n## Real\n", want: []string{"Real"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := sectionNames(t, tc.text); !slices.Equal(got, tc.want) {
				t.Fatalf("headings = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestSetextUnderlineIsNotContent(t *testing.T) {
	parts, err := splitSections("Summary\n=====\n")
	if err != nil || len(parts) != 1 || hasContent(parts[0].lines) {
		t.Fatalf("parts = %+v, err = %v; want an empty Summary section", parts, err)
	}
}

func TestDuplicateHeadingAcrossStyles(t *testing.T) {
	_, err := splitSections("## Summary\na\n\nSummary\n-------\nb\n")
	if err == nil || !strings.Contains(err.Error(), `expected unique headings, found "Summary" 2 times`) {
		t.Fatalf("err = %v", err)
	}
}

func TestCheckSectionsAcrossStyles(t *testing.T) {
	atx := "## Summary\n\n<!-- one line -->\n\n## Test plan\n\n<!-- checks -->\n"
	setext := "Summary\n=======\n\n<!-- one line -->\n\nTest plan\n---------\n\n<!-- checks -->\n"
	setextBody := "Summary\n=======\nshipped\n\nTest plan\n---------\ngo test\n"
	atxBody := "## Summary\nshipped\n\n## Test plan\ngo test\n"
	mixedBody := "# Summary\nshipped\n\nTest plan\n=========\ngo test\n"
	for _, tc := range []struct{ name, template, body string }{
		{"setext body, atx template", atx, setextBody},
		{"atx body, setext template", setext, atxBody},
		{"mixed body", atx, mixedBody},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var fails []string
			if err := checkSections(tc.template, tc.body, &fails); err != nil || len(fails) != 0 {
				t.Fatalf("fails = %q, err = %v", fails, err)
			}
		})
	}
	var fails []string
	if err := checkSections(atx, "Summary\n=======\nshipped\n", &fails); err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(fails, []string{`section "## Test plan": expected the section, found none`}) {
		t.Fatalf("fails = %q", fails)
	}
}
