package tui

import (
	"testing"

	tea "github.com/charmbracelet/bubbletea"
)

func TestFillParagraph(t *testing.T) {
	t.Parallel()
	const longWord = "supercalifragilisticexpialidocious"
	tests := []struct {
		name              string
		text              string
		line, col         int
		column, box       int
		want              string
		wantLine, wantCol int
	}{
		{
			name: "long line wraps",
			text: "alpha bravo charlie delta",
			line: 0, col: 14, column: 12, box: 80,
			want:     "alpha bravo\ncharlie\ndelta",
			wantLine: 1, wantCol: 2,
		},
		{
			name: "short paragraph rejoins",
			text: "alpha\nbravo charlie",
			line: 1, col: 0, column: 72, box: 80,
			want:     "alpha bravo charlie",
			wantLine: 0, wantCol: 6,
		},
		{
			name: "dash items stay separate",
			text: "- item one long enough to wrap here\n- item two stays put",
			line: 0, col: 2, column: 20, box: 80,
			want:     "- item one long\n  enough to wrap\n  here\n- item two stays put",
			wantLine: 0, wantCol: 2,
		},
		{
			name: "second item fills on its own",
			text: "- item one\n- item two is long enough",
			line: 1, col: 2, column: 16, box: 40,
			want:     "- item one\n- item two is\n  long enough",
			wantLine: 1, wantCol: 2,
		},
		{
			name: "star and plus items",
			text: "* star item is long enough\n+ plus item stays",
			line: 0, col: 2, column: 16, box: 40,
			want:     "* star item is\n  long enough\n+ plus item stays",
			wantLine: 0, wantCol: 2,
		},
		{
			name: "numbered items",
			text: "1. first item is long enough\n2) second stays",
			line: 0, col: 3, column: 20, box: 40,
			want:     "1. first item is\n   long enough\n2) second stays",
			wantLine: 0, wantCol: 3,
		},
		{
			name: "nested item keeps its indent",
			text: "- parent stays\n  - child item is long enough to wrap",
			line: 1, col: 4, column: 24, box: 80,
			want:     "- parent stays\n  - child item is long\n    enough to wrap",
			wantLine: 1, wantCol: 4,
		},
		{
			name: "paragraph is not merged into a list",
			text: "this is a paragraph that is long\n- list item stays",
			line: 0, col: 0, column: 20, box: 80,
			want:     "this is a paragraph\nthat is long\n- list item stays",
			wantLine: 0, wantCol: 0,
		},
		{
			name: "indented block is unchanged",
			text: "    echo hello from a long command line\n    gofmt -w .",
			line: 0, col: 4, column: 10, box: 80,
			want:     "    echo hello from a long command line\n    gofmt -w .",
			wantLine: 0, wantCol: 4,
		},
		{
			name: "tab indented block is unchanged",
			text: "\techo hello from a long command line",
			line: 0, col: 1, column: 10, box: 80,
			want:     "\techo hello from a long command line",
			wantLine: 0, wantCol: 1,
		},
		{
			name: "fenced block is unchanged",
			text: "```go\nthis line is long enough that it would wrap at ten\n```",
			line: 1, col: 5, column: 10, box: 40,
			want:     "```go\nthis line is long enough that it would wrap at ten\n```",
			wantLine: 1, wantCol: 5,
		},
		{
			name: "tilde fence is unchanged",
			text: "~~~\nthis line is long enough that it would wrap\n~~~\n",
			line: 1, col: 0, column: 10, box: 40,
			want:     "~~~\nthis line is long enough that it would wrap\n~~~\n",
			wantLine: 1, wantCol: 0,
		},
		{
			name: "paragraph outside a fence still fills",
			text: "before words\nhere now\n```\nstay long line inside\n```\n",
			line: 0, col: 0, column: 40, box: 80,
			want:     "before words here now\n```\nstay long line inside\n```\n",
			wantLine: 0, wantCol: 0,
		},
		{
			name: "over-long word stays one line",
			text: "hello " + longWord + " there",
			line: 0, col: 6, column: 10, box: 80,
			want:     "hello\n" + longWord + "\nthere",
			wantLine: 1, wantCol: 0,
		},
		{
			name: "fill column is capped to the box",
			text: "alpha bravo charlie",
			line: 0, col: 6, column: 80, box: 10,
			want:     "alpha\nbravo\ncharlie",
			wantLine: 1, wantCol: 0,
		},
		{
			name: "unset column is 72 capped to the box",
			text: "alpha bravo charlie",
			line: 0, col: 0, column: 0, box: 10,
			want:     "alpha\nbravo\ncharlie",
			wantLine: 0, wantCol: 0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, line, col := fillParagraph(tc.text, tc.line, tc.col, tc.column, tc.box)
			if got != tc.want || line != tc.wantLine || col != tc.wantCol {
				t.Fatalf("fillParagraph() = %q (%d,%d), want %q (%d,%d)", got, line, col, tc.want, tc.wantLine, tc.wantCol)
			}
		})
	}
}

func TestAltQFillsOnlyTheCursorParagraph(t *testing.T) {
	t.Parallel()
	m, _, _ := composerModel(t, composerNote)
	m.width = 120
	m.height = 40
	text := "alpha bravo\ncharlie\n\ndelta\necho"
	field, draft := m.composerField()
	*draft = text
	field.SetValue(text)
	setFieldCursor(field, 0, 6)
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'q'}, Alt: true})
	got, line, col := composerCursor(m)
	want := "alpha bravo charlie\n\ndelta\necho"
	if got != want || line != 0 || col != 6 {
		t.Fatalf("got %q cursor %d,%d want %q 0,6", got, line, col, want)
	}
	if !m.composing {
		t.Fatal("Alt+q left compose")
	}
}
