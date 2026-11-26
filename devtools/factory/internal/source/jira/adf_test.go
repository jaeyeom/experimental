package jira

import (
	"encoding/json"
	"testing"
)

func TestADFDescription(t *testing.T) {
	tests := []struct {
		name string
		body string
		want string
	}{
		{
			name: "heading, hard break, and paragraph",
			body: "## Problem\nline one\nline two\n\nnext paragraph",
			want: `{"type":"doc","version":1,"content":[
				{"type":"heading","attrs":{"level":2},"content":[{"type":"text","text":"Problem"}]},
				{"type":"paragraph","content":[{"type":"text","text":"line one"},{"type":"hardBreak"},{"type":"text","text":"line two"}]},
				{"type":"paragraph","content":[{"type":"text","text":"next paragraph"}]}]}`,
		},
		{
			name: "every heading level",
			body: "# one\n\n###### six",
			want: `{"type":"doc","version":1,"content":[
				{"type":"heading","attrs":{"level":1},"content":[{"type":"text","text":"one"}]},
				{"type":"heading","attrs":{"level":6},"content":[{"type":"text","text":"six"}]}]}`,
		},
		{
			name: "nested bullet list and numbered list",
			body: "- a\n  - b\n- c\n\n1. x\n2. y",
			want: `{"type":"doc","version":1,"content":[
				{"type":"bulletList","content":[
					{"type":"listItem","content":[
						{"type":"paragraph","content":[{"type":"text","text":"a"}]},
						{"type":"bulletList","content":[{"type":"listItem","content":[{"type":"paragraph","content":[{"type":"text","text":"b"}]}]}]}]},
					{"type":"listItem","content":[{"type":"paragraph","content":[{"type":"text","text":"c"}]}]}]},
				{"type":"orderedList","content":[
					{"type":"listItem","content":[{"type":"paragraph","content":[{"type":"text","text":"x"}]}]},
					{"type":"listItem","content":[{"type":"paragraph","content":[{"type":"text","text":"y"}]}]}]}]}`,
		},
		{
			name: "ordered list start",
			body: "3. x",
			want: `{"type":"doc","version":1,"content":[
				{"type":"orderedList","attrs":{"order":3},"content":[
					{"type":"listItem","content":[{"type":"paragraph","content":[{"type":"text","text":"x"}]}]}]}]}`,
		},
		{
			name: "fenced code keeps its contents verbatim",
			body: "```go\nfunc **x**() {}\n\\_ `y`\n```",
			want: `{"type":"doc","version":1,"content":[
				{"type":"codeBlock","attrs":{"language":"go"},"content":[{"type":"text","text":"func **x**() {}\n\\_ ` + "`y`" + `"}]}]}`,
		},
		{
			name: "fenced code without a language",
			body: "```\nplain\n```",
			want: `{"type":"doc","version":1,"content":[{"type":"codeBlock","content":[{"type":"text","text":"plain"}]}]}`,
		},
		{
			name: "inline code, bold, italic, and link",
			body: "use `go test` **now** or *later*, see [docs](https://example.com/d)",
			want: `{"type":"doc","version":1,"content":[{"type":"paragraph","content":[
				{"type":"text","text":"use "},
				{"type":"text","text":"go test","marks":[{"type":"code"}]},
				{"type":"text","text":" "},
				{"type":"text","text":"now","marks":[{"type":"strong"}]},
				{"type":"text","text":" or "},
				{"type":"text","text":"later","marks":[{"type":"em"}]},
				{"type":"text","text":", see "},
				{"type":"text","text":"docs","marks":[{"type":"link","attrs":{"href":"https://example.com/d"}}]}]}]}`,
		},
		{
			name: "backslash escape is unescaped",
			body: `snake\_case and \*star\*`,
			want: `{"type":"doc","version":1,"content":[{"type":"paragraph","content":[{"type":"text","text":"snake_case and *star*"}]}]}`,
		},
		{
			name: "table is kept as plain text",
			body: "| a | b |\n|---|---|\n| 1 | 2 |",
			want: `{"type":"doc","version":1,"content":[{"type":"paragraph","content":[
				{"type":"text","text":"| a | b |"},{"type":"hardBreak"},
				{"type":"text","text":"|---|---|"},{"type":"hardBreak"},
				{"type":"text","text":"| 1 | 2 |"}]}]}`,
		},
		{
			name: "html block is kept as plain text",
			body: "<div>\nhi\n</div>",
			want: `{"type":"doc","version":1,"content":[{"type":"paragraph","content":[
				{"type":"text","text":"<div>"},{"type":"hardBreak"},
				{"type":"text","text":"hi"},{"type":"hardBreak"},
				{"type":"text","text":"</div>"}]}]}`,
		},
		{
			name: "literal backslash n is not rewritten",
			body: `a\nb`,
			want: `{"type":"doc","version":1,"content":[{"type":"paragraph","content":[{"type":"text","text":"a\\nb"}]}]}`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := json.Marshal(adfDescription(tc.body))
			if err != nil {
				t.Fatal(err)
			}
			assertSameJSON(t, got, []byte(tc.want))
		})
	}
}

func TestADFDescriptionEmpty(t *testing.T) {
	for _, body := range []string{"", "  \n\n\t"} {
		if doc := adfDescription(body); doc != nil {
			t.Fatalf("adfDescription(%q) = %+v, want nil", body, doc)
		}
	}
}

func assertSameJSON(t *testing.T, got, want []byte) {
	t.Helper()
	var g, w any
	if err := json.Unmarshal(got, &g); err != nil {
		t.Fatalf("got is not JSON: %v: %s", err, got)
	}
	if err := json.Unmarshal(want, &w); err != nil {
		t.Fatalf("want is not JSON: %v: %s", err, want)
	}
	gb, _ := json.Marshal(g)
	wb, _ := json.Marshal(w)
	if string(gb) != string(wb) {
		t.Fatalf("got  %s\nwant %s", gb, wb)
	}
}

func TestADFPlainText(t *testing.T) {
	if got := adfPlainText(nil); got != "" {
		t.Fatalf("adfPlainText(nil) = %q, want empty", got)
	}
	roundTrip := "Claimed by claude on mac1\nStarting work now"
	doc := adfDescription(roundTrip)
	if got := adfPlainText(doc); got != roundTrip {
		t.Fatalf("adfPlainText(adfDescription) = %q, want %q", got, roundTrip)
	}
	rich := &adfDoc{
		Type:    "doc",
		Version: 1,
		Content: []*adfNode{
			{
				Type: "paragraph",
				Content: []*adfNode{
					{Type: "text", Text: "Claimed by "},
					{Type: "mention", Attrs: map[string]any{"text": "@alice"}},
					{Type: "text", Text: " "},
					{Type: "emoji", Attrs: map[string]any{"shortName": ":rocket:"}},
				},
			},
			{
				Type: "paragraph",
				Content: []*adfNode{
					{Type: "text", Text: "line 2"},
				},
			},
		},
	}
	want := "Claimed by @alice :rocket:\nline 2"
	if got := adfPlainText(rich); got != want {
		t.Fatalf("adfPlainText(rich) = %q, want %q", got, want)
	}
}
