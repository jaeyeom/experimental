package orglint

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/orglintparity"
)

// TestLinkCheckersMatchGoldens lints every parity fixture and compares the
// findings of the link checkers with the Emacs goldens.
func TestLinkCheckersMatchGoldens(t *testing.T) {
	checkGoldens(t, linkCheckers)
}

// checkGoldens lints every parity fixture and compares the findings of the
// given checkers with the matching findings in the Emacs goldens.
func checkGoldens(t *testing.T, checkers []Checker) {
	t.Helper()
	names := map[string]bool{}
	for _, c := range checkers {
		names[c.Name] = true
	}
	fixtures, _ := filepath.Glob("../linters/testdata/orglint/*.orgtest")
	if len(fixtures) == 0 {
		t.Fatal("no fixtures")
	}
	for _, fx := range fixtures {
		base := filepath.Base(fx)
		data, err := os.ReadFile(orglintparity.GoldenPath(fx))
		if err != nil {
			t.Fatal(err)
		}
		golden, err := orglintparity.Parse(strings.NewReader(string(data)))
		if err != nil {
			t.Fatal(err)
		}
		var want []orglintparity.Finding
		for _, f := range golden {
			if names[f.Checker] {
				want = append(want, f)
			}
		}
		got, err := LintFile(fx, checkers)
		if err != nil {
			t.Fatal(err)
		}
		for i := range got {
			got[i].File = base
		}
		if r := orglintparity.Diff(want, got); !r.Empty() {
			t.Errorf("%s:\n%s", base, r)
		}
	}
}

func TestLinkCheckersEdgeCases(t *testing.T) {
	tests := []struct{ name, org, want string }{
		{"percent other escape", "[[file:a%2Fb][x]]\n", "link-to-local-file"},
		{"existing file link with search", "[[file:testdata/sample.orgtest::12][x]]\n", ""},
		{"verbatim holds no links", "~[[nope]]~ and =file:x=\n", ""},
		{"headline links ignore case", "* Head\n[[head]] <<T>> [[t]]\n", ""},
		{"radio target is not a target", "<<<a>>> <<<a>>>\n", ""},
		{"fuzzy name", "#+NAME: t\n#+BEGIN_SRC sh\n#+END_SRC\n[[t]]\n", ""},
		{"inline footnote defines", "a[fn:x:def] b[fn:x]\n", ""},
		{"footnote section after two blank lines", "* Footnotes\n[fn:1] a\n\n\nstray\nsee[fn:1]\n", "extraneous-element-in-footnote-section"},
	}
	for _, tc := range tests {
		d := ParseString(tc.org)
		var got []string
		for _, c := range linkCheckers {
			for range c.Check(d) {
				got = append(got, c.Name)
			}
		}
		if g := strings.Join(got, ","); g != tc.want {
			t.Errorf("%s: got %q, want %q", tc.name, g, tc.want)
		}
	}
}
