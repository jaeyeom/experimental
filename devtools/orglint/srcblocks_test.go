package orglint

import (
	"strings"
	"testing"
)

// TestSrcCheckersMatchGoldens compares the src block and option checkers with
// the Emacs goldens.
func TestSrcCheckersMatchGoldens(t *testing.T) {
	checkGoldens(t, srcCheckers)
}

func TestSrcCheckersEdgeCases(t *testing.T) {
	tests := []struct{ name, org, want string }{
		{"known language", "#+begin_src bash\nx\n#+end_src\n", ""},
		{"unclosed src is text", "#+begin_src\nx\n", ""},
		{"src in example", "#+begin_example\n#+begin_src\n#+end_example\n", ""},
		{"language case", "#+begin_src Python\n#+end_src\n", ""},
		{"switches", "#+begin_src bash -n 5 :results output\n#+end_src\n", ""},
		{"switch then missing colon", "#+begin_src bash -n foo\n#+end_src\n", "wrong-header-argument:1,empty-header-argument:1"},
		{"good args", "#+begin_src bash :results output replace :exports both\n#+end_src\n", ""},
		{"var with value", "#+begin_src bash :var x=1 :dir /tmp\n#+end_src\n", ""},
		{"unknown arg", "#+begin_src bash :bogus\n#+end_src\n", "wrong-header-argument:1,empty-header-argument:1"},
		{"results forbidden combination", "#+begin_src bash :results file list\n#+end_src\n", "wrong-header-value:1"},
		{"results two groups", "#+begin_src bash :results output raw\n#+end_src\n", ""},
		{"exports bad", "#+begin_src bash :exports nope\n#+end_src\n", "wrong-header-value:1"},
		{"tangle any", "#+begin_src bash :tangle foo.sh\n#+end_src\n", ""},
		{"lisp value skipped", "#+begin_src bash :results (identity \"x\")\n#+end_src\n", ""},
		{"header keyword", "#+HEADER: :bogus 1\n#+begin_src bash\n#+end_src\n", "wrong-header-argument:2"},
		{"header keyword value", "#+HEADER: :exports nope\n#+begin_src bash\n#+end_src\n", "wrong-header-value:2"},
		{"property header-args", "#+PROPERTY: header-args:bash :results output :bogus 1\n", "wrong-header-argument:1"},
		{"property header-args plus", "#+PROPERTY: header-args+ :bogus 1\n", "wrong-header-argument:1"},
		{"property deprecated", "#+property: Results output\n", "deprecated-header-syntax:1"},
		{"property dir is fine", "#+PROPERTY: dir /tmp\n", ""},
		{"drawer header-args", "* H\n:PROPERTIES:\n:header-args: :bogus 1\n:END:\n", "wrong-header-argument:3"},
		{"drawer deprecated", "* H\n:PROPERTIES:\n:EXPORTS: both\n:END:\n", "deprecated-header-syntax:3"},
		{"call ok", "#+CALL: f(x=1) :results output\n", ""},
		{"call invalid", "#+CALL: nothing\n", "invalid-babel-call-block:1"},
		{"call bracket end", "#+CALL: f() [:results output]\n", "invalid-babel-call-block:1,wrong-header-argument:1"},
		{"call empty header", "#+CALL: f() :results\n", "empty-header-argument:1,wrong-header-value:1"},
		{"call inside header", "#+CALL: f[:bogus 1]()\n", "wrong-header-argument:1"},
		{"options ok", "#+OPTIONS: toc:nil num:t H:3 \\n:t\n", ""},
		{"options list value", "#+OPTIONS: toc:(a b) bad:t\n", "unknown-options-item:1"},
		{"export block ok", "#+begin_export html\nx\n#+end_export\n", ""},
		{"export block missing", "#+begin_export\nx\n#+end_export\n", "missing-backend-in-export-block:1"},
		{"deprecated export case", "#+begin_Latex\nx\n#+end_Latex\n", "deprecated-export-blocks:1"},
		{"named result", "#+begin_src bash\n#+end_src\n#+NAME: r\n#+RESULTS:\n: x\n", "named-result:3"},
		{"unnamed result", "#+begin_src bash\n#+end_src\n#+RESULTS:\n: x\n", ""},
		{"named result after text", "text\n\n#+NAME: r\n#+RESULTS:\n: x\n", ""},
	}
	for _, tc := range tests {
		d := ParseString(tc.org)
		var got []string
		for _, c := range srcCheckers {
			for _, f := range c.Check(d) {
				got = append(got, c.Name+":"+itoa(f.Line))
			}
		}
		if want := tc.want; !sameSet(got, want) {
			t.Errorf("%s: got %q, want %q", tc.name, strings.Join(got, ","), want)
		}
	}
}

// sameSet compares comma-separated findings regardless of order.
func sameSet(got []string, want string) bool {
	var w []string
	if want != "" {
		w = strings.Split(want, ",")
	}
	if len(got) != len(w) {
		return false
	}
	count := map[string]int{}
	for _, g := range got {
		count[g]++
	}
	for _, s := range w {
		count[s]--
	}
	for _, c := range count {
		if c != 0 {
			return false
		}
	}
	return true
}
