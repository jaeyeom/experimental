package orglint

import (
	"strconv"
	"strings"
	"testing"
)

// TestStructureCheckersMatchGoldens compares the structure checkers with the
// Emacs goldens.
func TestStructureCheckersMatchGoldens(t *testing.T) {
	checkGoldens(t, structureCheckers)
}

func TestStructureCheckersEdgeCases(t *testing.T) {
	tests := []struct{ name, org, want string }{
		{"closed block", "#+begin_quote\nx\n#+end_quote\n", ""},
		{"stray end", "#+end_src\n", "invalid-block:1"},
		{"end with text", "#+begin_src\n#+end_src foo\n", "invalid-block:1,invalid-block:2"},
		{"block inside example", "#+begin_example\n#+begin_src\n#+end_example\n", ""},
		{"dynamic block", "#+BEGIN: clocktable\n#+END:\n", ""},
		{"closed drawer", "* H\n:LOGBOOK:\nx\n:END:\n", ""},
		{"stray drawer end", "text\n:END:\n", "incomplete-drawer:2"},
		{"misleading entry", "* H\n:A:\n:B:\n:END:\n", "incomplete-drawer:3"},
		{"drawer in src", "#+begin_src\n:A:\n#+end_src\n", ""},
		{"unclosed drawer keeps parsing", "* H\n:A:\n* Next\n[[nope]]\n", "incomplete-drawer:2,invalid-fuzzy-link"},
		{"planning after headline", "* H\nSCHEDULED: <2026-01-01 Thu>\n", ""},
		{"planning in src", "#+begin_src\nSCHEDULED: x\n#+end_src\n", ""},
		{"planning in quote", "#+begin_quote\nDEADLINE: x\n#+end_quote\n", "misplaced-planning-info:2"},
		{"properties after planning", "* H\nCLOSED: [2026-01-01 Thu]\n:PROPERTIES:\n:A: b\n:END:\n", ""},
		{"top properties", "#+TITLE: x\n:PROPERTIES:\n:ID: a\n:END:\n", ""},
		{"special property case", "* H\n:PROPERTIES:\n:todo: x\n:END:\n", "special-property-in-properties-drawer:3"},
		{"id with colons", "* H\n:PROPERTIES:\n:ID: a:b\n:END:\n", ""},
		{"tags fine", "* H :a:b:\n", ""},
		{"keyword dual", "#+CAPTION[en]: x\n#+RESULTS:\ntext\n", ""},
		{"keyword missing colon", "  #+FOO bar\n", "invalid-keyword-syntax:1"},
		{"keyword empty name", "#+ x\n", "invalid-keyword-syntax:1"},
		{"item counter", "1. a\n2. b\n", ""},
		{"item manual counter", "3. [@3] a\n4. b\n", ""},
		{"item sublist", "1. a\n   1. x\n   3. y\n2. b\n", "item-number:3"},
		{"item lists split by blank lines", "1. a\n\n\n1. b\n", ""},
		{"unordered item", "- a\n- b\n", ""},
		{"affiliated attached", "#+NAME: x\ntext\n", ""},
		{"affiliated chain orphan", "#+NAME: x\n#+CAPTION: y\n\n", "orphaned-affiliated-keywords:1,orphaned-affiliated-keywords:2"},
		{"attr orphan", "#+attr_html: :a b\n", "orphaned-affiliated-keywords:1"},
		{"orphan results ignored", "#+RESULTS:\n", ""},
		{"misplaced heading at end of paragraph", "a\nPara** x\nb\n\nq\n", "misplaced-heading:4"},
		{"escaped heading", "a ,** x\n", ""},
	}
	for _, tc := range tests {
		d := ParseString(tc.org)
		var got []string
		for _, c := range structureCheckers {
			for _, f := range c.Check(d) {
				s := c.Name
				if strings.Contains(tc.want, c.Name+":") {
					s += ":" + itoa(f.Line)
				}
				got = append(got, s)
			}
		}
		for _, c := range linkCheckers {
			if strings.Contains(tc.want, c.Name) {
				for range c.Check(d) {
					got = append(got, c.Name)
				}
			}
		}
		if g := strings.Join(got, ","); g != tc.want {
			t.Errorf("%s: got %q, want %q", tc.name, g, tc.want)
		}
	}
}

func itoa(n int) string { return strconv.Itoa(n) }
