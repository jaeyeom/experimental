package orglint

import (
	"fmt"
	"regexp"
	"slices"
	"strings"
)

var srcCheckers = []Checker{
	{"missing-language-in-src-block", checkMissingLanguage},
	{"suspicious-language-in-src-block", checkSuspiciousLanguage},
	{"empty-header-argument", checkEmptyHeaderArgument},
	{"wrong-header-argument", checkWrongHeaderArgument},
	{"wrong-header-value", checkWrongHeaderValue},
	{"deprecated-header-syntax", checkDeprecatedHeaderSyntax},
	{"invalid-babel-call-block", checkBabelCall},
	{"named-result", checkNamedResult},
	{"missing-backend-in-export-block", checkMissingBackend},
	{"deprecated-export-blocks", checkDeprecatedExportBlocks},
	{"unknown-options-item", checkUnknownOptions},
}

// The tables below were read from Org 9.7.11 (org-babel-common-header-args-w-values,
// org-babel-header-arg-names, org-export-options-alist and the options of the
// export backends org-lint loads). Language specific header arguments and
// languages known only through Emacs major modes are not covered. Add to the
// tables when a file in this repository needs more.

// headerArgNames are the header arguments Org accepts for every language.
const headerArgNames = ("cache cmdline colnames comments dir eval exports epilogue " +
	"file file-desc file-ext file-mode hlines mkdirp no-expand noeval noweb " +
	"noweb-ref noweb-sep noweb-prefix output-dir padline post prologue results " +
	"rownames sep session shebang tangle tangle-mode var wrap")

// headerValues lists the allowed values of the header arguments that have a
// closed set of them. Each argument has groups of values; at most one value of
// a group may be given.
var headerValues = map[string][][]string{
	"cache":    {{"no", "yes"}},
	"colnames": {{"nil", "no", "yes"}},
	"comments": {{"no", "link", "yes", "org", "both", "noweb"}},
	"eval":     {{"yes", "no", "no-export", "strip-export", "never-export", "eval", "never", "query"}},
	"exports":  {{"code", "results", "both", "none"}},
	"hlines":   {{"no", "yes"}},
	"mkdirp":   {{"yes", "no"}},
	"noweb":    {{"yes", "no", "tangle", "strip-tangle", "no-export", "strip-export"}},
	"padline":  {{"yes", "no"}},
	"results": {
		{"file", "list", "vector", "table", "scalar", "verbatim"},
		{"raw", "html", "latex", "org", "code", "pp", "drawer", "link", "graphics"},
		{"replace", "silent", "none", "discard", "append", "prepend"},
		{"output", "value"},
	},
	"rownames":     {{"no", "yes"}},
	"noweb-prefix": {{"no", "yes"}},
}

const optionItems = (`' * - : < H \n ^ after-closing-order after-letter-order arch author ` +
	`backaddress broken-links c compact-itemx creator d date e email expand-links f ` +
	`foldmarks from-logo html-link-use-abs-url html-postamble html-preamble ` +
	`html-scripts html-style html5-fancy inline num p phone place pri prop ` +
	`special-headings stat subject tags tasks tex timestamp title title-subject ` +
	`toc todo url |`)

const languages = ("abc applescript asymptote awk bash C c++ calc clojure comint conf coq " +
	"cpp csharp css ditaa dot elisp emacs-lisp eshell fish forth fortran gnuplot go " +
	"groovy haskell html java javascript js json julia latex ledger lilypond lisp " +
	"lua markdown matlab maxima mscgen ocaml octave org perl picolisp pkl plantuml " +
	"processing python R racket ruby rust sass scala scheme screen sed sh shell " +
	"sql sqlite stan tcl text toml typescript vala xml yaml zsh dart")

// hasWord reports whether w is one of the space separated words.
func hasWord(words, w string) bool {
	return slices.Contains(strings.Fields(words), w)
}

const exportTypes = "ASCII BEAMER HTML LATEX MAN MARKDOWN MD ODT ORG TEXINFO"

var (
	switchRE = regexp.MustCompile(`^(?:[-+]n(?: +\d+)?|-l +"[^"\n]+"|-[ikrtv])(?:\s+|$)`)
	callRE   = regexp.MustCompile(`([^()\n]+?)(?:\[(.*?)\])?\((.*?)\)[ \t]*(.*)`)
	optionRE = regexp.MustCompile(`(.+?):(\(.*?\)|\S+)?[ \t]*`)
	// Header arguments that used to be set by a property of their own name.
	// "dir" is left out because attachments use it too.
	deprecatedRE = regexp.MustCompile(`(?i)^(cache|cmdline|colnames|comments|eval|exports|epilogue|` +
		`file|file-desc|file-ext|file-mode|hlines|mkdirp|no-expand|noeval|noweb|noweb-ref|` +
		`noweb-sep|noweb-prefix|output-dir|padline|post|prologue|results|rownames|sep|` +
		`session|shebang|tangle|tangle-mode|var|wrap)[ \t]`)
	headerArgsKey = regexp.MustCompile(`(?i)^header-args(?::(\S+))?\+? *`)
)

// headerArg is a parsed header argument. Value is "" and HasValue false when
// the argument has none.
type headerArg struct {
	Name     string // as written, with the colon
	Value    string
	HasValue bool
}

// splitHeaderArgs splits header arguments at whitespace followed by a colon
// outside parentheses and quotes.
func splitHeaderArgs(s string) []string {
	var pieces []string
	depth, quoted, start := 0, false, 0
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c == '"':
			quoted = !quoted
		case quoted:
		case strings.IndexByte("([{", c) >= 0:
			depth++
		case strings.IndexByte(")]}", c) >= 0:
			depth--
		case c == ':' && depth <= 0 && i > 0 && strings.IndexByte(" \t", s[i-1]) >= 0:
			pieces = append(pieces, s[start:i])
			start = i
		}
	}
	return append(pieces, s[start:])
}

// parseHeaderArgs parses header arguments the way
// org-babel-parse-header-arguments does.
func parseHeaderArgs(s string) []headerArg {
	var out []headerArg
	for _, p := range splitHeaderArgs(strings.TrimSpace(s)) {
		f := strings.Fields(p)
		switch {
		case len(f) == 0:
		case len(f) == 1:
			out = append(out, headerArg{Name: f[0]})
		default:
			v := strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(p), f[0]))
			out = append(out, headerArg{f[0], v, true})
		}
	}
	return out
}

// srcBlock is a source block with its header arguments.
type srcBlock struct {
	Line     int
	Language string
	Headers  []string // parameters, then #+HEADER values
}

// headerHolder is anything that carries header arguments and gets reported on
// its first line.
type headerHolder struct {
	Line    int
	Headers []string
}

func (d *Document) srcBlocks() []srcBlock {
	var out []srcBlock
	for _, b := range d.Blocks {
		if b.Name != "SRC" || b.EndLine == 0 {
			continue
		}
		params := strings.TrimSpace(b.Params)
		lang := ""
		if f := strings.Fields(params); len(f) > 0 {
			lang = f[0]
			params = strings.TrimSpace(params[len(lang):])
		}
		for {
			m := switchRE.FindString(params)
			if m == "" {
				break
			}
			params = strings.TrimSpace(params[len(m):])
		}
		out = append(out, srcBlock{b.Line, lang, append([]string{params}, d.headerKeywords(b.Line)...)})
	}
	return out
}

// headerKeywords returns the values of the #+HEADER keywords directly above
// line n.
func (d *Document) headerKeywords(n int) []string {
	byLine := map[int]Keyword{}
	for _, k := range d.Keywords {
		byLine[k.Line] = k
	}
	var out []string
	for l := n - 1; l >= 1; l-- {
		k, ok := byLine[l]
		if !ok || !k.Affiliated {
			break
		}
		if k.Key == "HEADER" {
			out = append([]string{k.Value}, out...)
		}
	}
	return out
}

// babelCall is a "#+CALL:" line. Valid is false if it has no call name.
type babelCall struct {
	Line  int
	Valid bool
	End   string
	Hdrs  []string // inside header, end header
}

func (d *Document) babelCalls() []babelCall {
	var out []babelCall
	for _, k := range d.Keywords {
		if k.Key != "CALL" {
			continue
		}
		m := callRE.FindStringSubmatch(k.Value)
		if m == nil {
			out = append(out, babelCall{Line: k.Line})
			continue
		}
		end := strings.TrimSpace(m[4])
		out = append(out, babelCall{k.Line, true, end, []string{m[2], end}})
	}
	return out
}

// headerHolders returns every src block and babel call, whose header
// arguments all three header checkers inspect.
func (d *Document) headerHolders() []headerHolder {
	var out []headerHolder
	for _, b := range d.srcBlocks() {
		out = append(out, headerHolder{b.Line, b.Headers})
	}
	for _, c := range d.babelCalls() {
		out = append(out, headerHolder{c.Line, c.Hdrs})
	}
	return out
}

func checkMissingLanguage(d *Document) []Finding {
	var out []Finding
	for _, b := range d.srcBlocks() {
		if b.Language == "" {
			out = append(out, Finding{Line: b.Line, Message: "Missing language in source block"})
		}
	}
	return out
}

func checkSuspiciousLanguage(d *Document) []Finding {
	var out []Finding
	for _, b := range d.srcBlocks() {
		if b.Language != "" && !hasWord(languages, b.Language) && !hasWord(languages, strings.ToLower(b.Language)) {
			out = append(out, Finding{Line: b.Line, Message: fmt.Sprintf("Unknown source block language: '%s'", b.Language)})
		}
	}
	return out
}

func checkEmptyHeaderArgument(d *Document) []Finding {
	var out []Finding
	for _, h := range d.headerHolders() {
		for _, s := range h.Headers {
			for _, a := range parseHeaderArgs(s) {
				if !a.HasValue {
					out = append(out, Finding{Line: h.Line, Message: fmt.Sprintf("Empty value in header argument %q", a.Name)})
				}
			}
		}
	}
	return out
}

func verifyHeaderArgs(line int, s string) []Finding {
	var out []Finding
	for _, a := range parseHeaderArgs(s) {
		switch {
		case !strings.HasPrefix(a.Name, ":"):
			out = append(out, Finding{Line: line, Message: fmt.Sprintf("Missing colon in header argument %q", a.Name)})
		case !hasWord(headerArgNames, a.Name[1:]):
			out = append(out, Finding{Line: line, Message: fmt.Sprintf("Unknown header argument %q", a.Name)})
		}
	}
	return out
}

func checkWrongHeaderArgument(d *Document) []Finding {
	var out []Finding
	for _, h := range d.headerHolders() {
		for _, s := range h.Headers {
			out = append(out, verifyHeaderArgs(h.Line, s)...)
		}
	}
	for _, k := range d.Keywords {
		if k.Key != "PROPERTY" {
			continue
		}
		if m := headerArgsKey.FindString(k.Value); m != "" {
			out = append(out, verifyHeaderArgs(k.Line, k.Value[len(m):])...)
		}
	}
	for _, dr := range d.propertyDrawers() {
		for _, p := range dr.Properties {
			if m := headerArgsKey.FindString(p.Name); m != "" && m == p.Name {
				out = append(out, verifyHeaderArgs(p.Line, p.Value)...)
			}
		}
	}
	return out
}

func checkWrongHeaderValue(d *Document) []Finding {
	var out []Finding
	for _, h := range d.headerHolders() {
		for _, a := range parseHeaderArgs(strings.Join(h.Headers, " ")) {
			groups := headerValues[strings.TrimPrefix(a.Name, ":")]
			if !strings.HasPrefix(a.Name, ":") || groups == nil || lispValue(a.Value) {
				continue
			}
			values := strings.Fields(a.Value)
			if !a.HasValue {
				values = []string{"nil"} // not a string, so no group has it
			}
			out = append(out, checkValues(h.Line, a, values)...)
		}
	}
	return out
}

func lispValue(v string) bool {
	return v != "" && strings.ContainsRune("('`[", rune(v[0]))
}

// checkValues checks the values of a header argument against its groups.
func checkValues(line int, a headerArg, values []string) []Finding {
	var out []Finding
	used := map[int]string{}
	for _, v := range values {
		group := -1
		for i, g := range headerValues[a.Name[1:]] {
			if a.HasValue && contains(g, v) {
				group = i
				break
			}
		}
		switch prev, taken := used[group]; {
		case group < 0:
			out = append(out, Finding{Line: line, Message: fmt.Sprintf("Unknown value %q for header %q", v, a.Name)})
		case taken:
			out = append(out, Finding{Line: line, Message: fmt.Sprintf("Forbidden combination in header %q: %s, %s", a.Name, prev, v)})
		default:
			used[group] = v
		}
	}
	return out
}

func contains(list []string, s string) bool {
	for _, x := range list {
		if x == s {
			return true
		}
	}
	return false
}

func checkDeprecatedHeaderSyntax(d *Document) []Finding {
	var out []Finding
	for _, k := range d.Keywords {
		if m := deprecatedRE.FindStringSubmatch(k.Value); k.Key == "PROPERTY" && m != nil {
			out = append(out, Finding{Line: k.Line, Message: fmt.Sprintf("Deprecated syntax for %q.  Use header-args instead", m[1])})
		}
	}
	for _, dr := range d.propertyDrawers() {
		for _, p := range dr.Properties {
			if deprecatedRE.MatchString(p.Name + " ") {
				out = append(out, Finding{Line: p.Line, Message: fmt.Sprintf("Deprecated syntax for %q.  Use :header-args: instead", p.Name)})
			}
		}
	}
	return out
}

func checkBabelCall(d *Document) []Finding {
	var out []Finding
	for _, c := range d.babelCalls() {
		switch {
		case !c.Valid:
			out = append(out, Finding{Line: c.Line, Message: "Invalid syntax in babel call block"})
		case strings.HasPrefix(c.End, "[") && strings.HasSuffix(c.End, "]"):
			out = append(out, Finding{Line: c.Line, Message: "Babel call's end header must not be wrapped within brackets"})
		}
	}
	return out
}

func checkMissingBackend(d *Document) []Finding {
	var out []Finding
	for _, b := range d.Blocks {
		if b.Name == "EXPORT" && b.EndLine > 0 && strings.TrimSpace(b.Params) == "" {
			out = append(out, Finding{Line: b.Line, Message: "Missing backend in export block"})
		}
	}
	return out
}

func checkDeprecatedExportBlocks(d *Document) []Finding {
	var out []Finding
	for _, b := range d.Blocks {
		if b.EndLine == 0 || !hasWord(exportTypes, b.Name) {
			continue
		}
		name := blockRE.FindStringSubmatch(d.Lines[b.Line-1])[1]
		out = append(out, Finding{Line: b.Line, Message: fmt.Sprintf("Deprecated syntax for export block.  Use \"BEGIN_EXPORT %s\" instead", name)})
	}
	return out
}

func checkUnknownOptions(d *Document) []Finding {
	var out []Finding
	for _, k := range d.Keywords {
		if k.Key != "OPTIONS" {
			continue
		}
		for _, m := range optionRE.FindAllStringSubmatch(k.Value, -1) {
			if !hasWord(optionItems, m[1]) {
				out = append(out, Finding{Line: k.Line, Message: fmt.Sprintf("Unknown OPTIONS item %q", m[1])})
			}
		}
	}
	return out
}

// affiliatedChain returns the first and last line of the run of affiliated
// keywords around line n, and the value of the NAME keyword in it.
func affiliatedChain(d *Document, n int) (first, last int, name string) {
	byLine := map[int]Keyword{}
	for _, k := range d.Keywords {
		byLine[k.Line] = k
	}
	first, last = n, n
	for byLine[first-1].Affiliated {
		first--
	}
	for byLine[last+1].Affiliated {
		last++
	}
	for l := first; l <= last; l++ {
		if byLine[l].Key == "NAME" {
			name = byLine[l].Value
		}
	}
	return first, last, name
}

// checkNamedResult reports a named, empty "#+RESULTS:" that directly follows
// a source block. Links to the name break when the block is not exported.
// Results that name their source block are not checked.
func checkNamedResult(d *Document) []Finding {
	var out []Finding
	for _, k := range d.Keywords {
		if (k.Key != "RESULTS" && !strings.HasPrefix(k.Key, "RESULTS[")) || k.Value != "" {
			continue
		}
		first, last, name := affiliatedChain(d, k.Line)
		if name == "" || last >= len(d.Lines) || strings.TrimSpace(d.Lines[last]) == "" || d.isHeadlineLine(last+1) {
			continue
		}
		prev := first - 1
		for prev >= 1 && strings.TrimSpace(d.Lines[prev-1]) == "" {
			prev--
		}
		for _, b := range d.Blocks {
			if b.Name == "SRC" && b.EndLine == prev {
				out = append(out, Finding{Line: first, Message: fmt.Sprintf("Links to %q will not be valid during export unless the parent source block has :exports results or both", name)})
			}
		}
	}
	return out
}
