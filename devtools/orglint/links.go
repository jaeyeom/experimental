package orglint

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

var linkCheckers = []Checker{
	{"link-to-local-file", checkLinkToLocalFile},
	{"file-application", checkFileApplication},
	{"percent-encoding-link-escape", checkPercentEncoding},
	{"trailing-bracket-after-link", checkTrailingBracket},
	{"unclosed-brackets-in-link-description", checkDescriptionBrackets},
	{"invalid-fuzzy-link", checkFuzzyLinks},
	{"invalid-custom-id-link", checkCustomIDLinks},
	{"invalid-coderef-link", checkCoderefLinks},
	{"duplicate-custom-id", checkDuplicateCustomID},
	{"duplicate-name", checkDuplicateName},
	{"duplicate-target", checkDuplicateTarget},
	{"duplicate-footnote-definition", checkDuplicateFootnote},
	{"undefined-footnote-reference", checkUndefinedFootnote},
	{"unreferenced-footnote-definition", checkUnreferencedFootnote},
	{"extraneous-element-in-footnote-section", checkFootnoteSection},
}

// linkTypes are the link types Org knows. Other "x:y" targets are fuzzy.
var linkTypes = map[string]bool{
	"file": true, "http": true, "https": true, "ftp": true, "mailto": true,
	"news": true, "shell": true, "elisp": true, "doi": true, "info": true,
	"irc": true, "help": true, "id": true, "attachment": true, "docview": true,
	"man": true, "eww": true, "bbdb": true, "calc": true, "gnus": true,
	"mhe": true, "rmail": true, "w3m": true,
}

var (
	typedRE   = regexp.MustCompile(`^([A-Za-z][-A-Za-z0-9_.+]*):(.*)$`)
	coderefRE = regexp.MustCompile(`^\((.*)\)$`)
	remoteRE  = regexp.MustCompile(`^/[^/:]+:[^/:]*:`)
)

// parsedLink is a link split into type, path and file application, the way
// Org's link parser does.
type parsedLink struct {
	Link
	Type, Path, App string
	Bracket         bool
}

func parseLink(l Link) parsedLink {
	p := parsedLink{Link: l, Bracket: l.Kind == BracketLink, Type: "fuzzy", Path: l.Target}
	t := l.Target
	if m := typedRE.FindStringSubmatch(t); m != nil {
		typ, rest := strings.ToLower(m[1]), m[2]
		base, app, _ := strings.Cut(typ, "+")
		switch {
		case typ == "file" || (base == "file" && app != ""):
			p.Type, p.Path, p.App = "file", rest, app
		case linkTypes[typ]:
			p.Type, p.Path = typ, rest
		}
		if p.Type != "fuzzy" {
			p.Path = stripSearch(p.Type, p.Path)
			return p
		}
	}
	if l.Kind != BracketLink {
		p.Type = ""
		return p
	}
	switch {
	case strings.HasPrefix(t, "#"):
		p.Type, p.Path = "custom-id", t[1:]
	case coderefRE.MatchString(t):
		p.Type, p.Path = "coderef", coderefRE.FindStringSubmatch(t)[1]
	case strings.HasPrefix(t, "./") || strings.HasPrefix(t, "/") || strings.HasPrefix(t, "~"):
		p.Type, p.Path = "file", stripSearch("file", t)
	}
	return p
}

// stripSearch drops the "::search" option of a file link.
func stripSearch(typ, path string) string {
	if typ == "file" {
		if i := strings.Index(path, "::"); i >= 0 {
			return path[:i]
		}
	}
	return path
}

func links(d *Document) []parsedLink {
	out := make([]parsedLink, 0, len(d.Links))
	for _, l := range d.Links {
		if p := parseLink(l); p.Type != "" {
			out = append(out, p)
		}
	}
	return out
}

func checkLinkToLocalFile(d *Document) []Finding {
	var out []Finding
	for _, l := range links(d) {
		if l.Type != "file" || remoteRE.MatchString(l.Path) {
			continue
		}
		path := l.Path
		if rest, ok := strings.CutPrefix(path, "~"); ok && (rest == "" || rest[0] == '/') {
			if home, err := os.UserHomeDir(); err == nil {
				path = home + rest
			}
		}
		if !filepath.IsAbs(path) {
			path = filepath.Join(d.Dir, path)
		}
		if _, err := os.Stat(path); errors.Is(err, fs.ErrNotExist) {
			out = append(out, Finding{Line: l.Line, Message: fmt.Sprintf("Link to non-existent local file %q", l.Path)})
		}
	}
	return out
}

func checkFileApplication(d *Document) []Finding {
	var out []Finding
	for _, l := range links(d) {
		if l.App != "" {
			out = append(out, Finding{Line: l.Line, Message: fmt.Sprintf("Deprecated \"file+%s\" link type", l.App)})
		}
	}
	return out
}

var percentRE = regexp.MustCompile(`%(..)?`)

// checkPercentEncoding mirrors org-lint: a link is flagged when every "%xx"
// escape is one of the four that Org still understands, and there is one.
func checkPercentEncoding(d *Document) []Finding {
	var out []Finding
	for _, l := range links(d) {
		if !l.Bracket || !strings.Contains(l.Path, "%") {
			continue
		}
		obsolete := true
		for _, m := range percentRE.FindAllStringSubmatch(l.Path, -1) {
			switch m[1] {
			case "25", "5B", "5D", "20":
			default:
				obsolete = false
			}
		}
		if obsolete {
			out = append(out, Finding{Line: l.Line, Message: "Link escaped with obsolete percent-encoding syntax"})
		}
	}
	return out
}

func checkTrailingBracket(d *Document) []Finding {
	var out []Finding
	for _, l := range d.Links {
		text := d.Lines[l.Line-1]
		rest := strings.TrimLeft(text[l.End:], " \t")
		if strings.HasPrefix(rest, "]") {
			out = append(out, Finding{Line: l.Line, Message: "Trailing ']' after link end"})
		}
	}
	return out
}

func checkDescriptionBrackets(d *Document) []Finding {
	var out []Finding
	for _, l := range d.Links {
		if l.Desc == "" {
			continue
		}
		depth := strings.Count(l.Desc, "[") - strings.Count(l.Desc, "]")
		if depth > 0 {
			out = append(out, Finding{Line: l.Line, Message: "No closing ']' matches '[' in link description: " + l.Desc})
		}
	}
	return out
}

// words normalizes whitespace the way split-string does for link search.
func words(s string) string { return strings.Join(strings.Fields(s), " ") }

func checkFuzzyLinks(d *Document) []Finding {
	headlines, targets, others := map[string]bool{}, map[string]bool{}, map[string]bool{}
	for _, h := range d.Headlines {
		headlines[strings.ToLower(words(h.Title))] = true
	}
	for _, k := range d.Keywords {
		if k.Key == "NAME" {
			others[words(k.Value)] = true
		}
	}
	for _, t := range d.Targets {
		targets[strings.ToLower(words(t.Value))] = true
	}
	var out []Finding
	for _, l := range links(d) {
		if l.Type != "fuzzy" {
			continue
		}
		if name, ok := strings.CutPrefix(l.Path, "*"); ok {
			if !headlines[strings.ToLower(words(name))] {
				out = append(out, Finding{Line: l.Line, Message: fmt.Sprintf("Unknown fuzzy location %q", name)})
			}
		} else if w := words(l.Path); !headlines[strings.ToLower(w)] && !targets[strings.ToLower(w)] && !others[w] {
			out = append(out, Finding{Line: l.Line, Message: fmt.Sprintf("Unknown fuzzy location %q", l.Path)})
		}
	}
	return out
}

func customIDs(d *Document) []Property {
	var out []Property
	for _, dr := range d.Drawers {
		if !strings.EqualFold(dr.Name, "PROPERTIES") {
			continue
		}
		for _, p := range dr.Properties {
			if strings.EqualFold(p.Name, "CUSTOM_ID") && p.Value != "" {
				out = append(out, p)
			}
		}
	}
	return out
}

func checkCustomIDLinks(d *Document) []Finding {
	known := map[string]bool{}
	for _, p := range customIDs(d) {
		known[p.Value] = true
	}
	var out []Finding
	for _, l := range links(d) {
		if l.Type == "custom-id" && !known[l.Path] {
			out = append(out, Finding{Line: l.Line, Message: fmt.Sprintf("Unknown custom ID %q", l.Path)})
		}
	}
	return out
}

var coderefLabelRE = regexp.MustCompile(`[ \t]*\(ref:([-a-zA-Z0-9:_ ]+)\)[ \t]*$`)

func checkCoderefLinks(d *Document) []Finding {
	labels := map[string]bool{}
	for _, b := range d.Blocks {
		if (b.Name != "SRC" && b.Name != "EXAMPLE") || b.EndLine == 0 {
			continue
		}
		for _, text := range d.Lines[b.Line : b.EndLine-1] {
			if m := coderefLabelRE.FindStringSubmatch(text); m != nil {
				labels[m[1]] = true
			}
		}
	}
	var out []Finding
	for _, l := range links(d) {
		if l.Type == "coderef" && !labels[l.Path] {
			out = append(out, Finding{Line: l.Line, Message: fmt.Sprintf("Unknown coderef %q", l.Path)})
		}
	}
	return out
}

// duplicates reports every occurrence of a key that occurs more than once.
func duplicates(keys []string, lines []int, msg func(key string) string) []Finding {
	count := map[string]int{}
	for _, k := range keys {
		count[k]++
	}
	var out []Finding
	for i, k := range keys {
		if count[k] > 1 {
			out = append(out, Finding{Line: lines[i], Message: msg(k)})
		}
	}
	return out
}

func checkDuplicateCustomID(d *Document) []Finding {
	var keys []string
	var lines []int
	for _, p := range customIDs(d) {
		keys, lines = append(keys, p.Value), append(lines, p.Line)
	}
	return duplicates(keys, lines, func(k string) string { return fmt.Sprintf("Duplicate CUSTOM_ID property %q", k) })
}

func checkDuplicateName(d *Document) []Finding {
	var keys []string
	var lines []int
	for _, k := range d.Keywords {
		if k.Key == "NAME" && k.Value != "" {
			keys, lines = append(keys, k.Value), append(lines, k.Line)
		}
	}
	return duplicates(keys, lines, func(k string) string { return fmt.Sprintf("Duplicate NAME %q", k) })
}

func checkDuplicateTarget(d *Document) []Finding {
	var keys []string
	var lines []int
	for _, t := range d.Targets {
		keys, lines = append(keys, words(t.Value)), append(lines, t.Line)
	}
	return duplicates(keys, lines, func(k string) string { return "Duplicate target <<" + k + ">>" })
}

func footnoteDefs(d *Document) (keys []string, lines []int) {
	for _, f := range d.Footnotes {
		if f.Definition {
			keys, lines = append(keys, f.Label), append(lines, f.Line)
		}
	}
	return keys, lines
}

func checkDuplicateFootnote(d *Document) []Finding {
	keys, lines := footnoteDefs(d)
	return duplicates(keys, lines, func(k string) string { return fmt.Sprintf("Duplicate footnote definition %q", k) })
}

func checkUndefinedFootnote(d *Document) []Finding {
	defined := map[string]bool{}
	for _, f := range d.Footnotes {
		if f.Definition || f.Inline {
			defined[f.Label] = true
		}
	}
	var out []Finding
	for _, f := range d.Footnotes {
		if !f.Definition && !f.Inline && !defined[f.Label] {
			out = append(out, Finding{Line: f.Line, Message: fmt.Sprintf("Missing definition for footnote [%s]", f.Label)})
		}
	}
	return out
}

func checkUnreferencedFootnote(d *Document) []Finding {
	referenced := map[string]bool{}
	for _, f := range d.Footnotes {
		if !f.Definition && f.Label != "" {
			referenced[f.Label] = true
		}
	}
	var out []Finding
	for _, f := range d.Footnotes {
		if f.Definition && !referenced[f.Label] {
			out = append(out, Finding{Line: f.Line, Message: fmt.Sprintf("No reference for footnote definition [%s]", f.Label)})
		}
	}
	return out
}

// checkFootnoteSection flags a "Footnotes" section holding anything but
// footnote definitions, property drawers and comments. A definition runs
// until the next definition, a headline or two blank lines.
func checkFootnoteSection(d *Document) []Finding {
	var out []Finding
	for _, h := range d.Headlines {
		if h.Title == "Footnotes" && footnoteSectionHasExtra(d, h) {
			out = append(out, Finding{Line: h.Line, Message: "Extraneous elements in footnote section are not exported"})
		}
	}
	return out
}

func footnoteSectionHasExtra(d *Document, h Headline) bool {
	inDef, blanks := false, 0
	for n := h.Line + 1; n <= h.EndLine; n++ {
		text := d.Lines[n-1]
		switch {
		case headlineRE.MatchString(text):
			if !strings.HasPrefix(headlineRE.FindStringSubmatch(text)[2], "COMMENT") {
				return true
			}
			inDef = false
		case fnDefRE.MatchString(text):
			inDef, blanks = true, 0
		case strings.TrimSpace(text) == "":
			if blanks++; blanks >= 2 {
				inDef = false
			}
		case inDef:
			blanks = 0
		default:
			last, ok := ignorableElement(d, n)
			if !ok {
				return true
			}
			n = last
		}
	}
	return false
}

// ignorableElement reports whether the element starting at line n is a
// comment or a property drawer, and the last line it covers.
func ignorableElement(d *Document, n int) (last int, ok bool) {
	trimmed := strings.TrimSpace(d.Lines[n-1])
	if strings.HasPrefix(trimmed, "# ") || trimmed == "#" {
		return n, true
	}
	for _, b := range d.Blocks {
		if b.Line == n && b.Name == "COMMENT" && b.EndLine > 0 {
			return b.EndLine, true
		}
	}
	for _, dr := range d.Drawers {
		if dr.Line == n && strings.EqualFold(dr.Name, "PROPERTIES") {
			return max(dr.EndLine, n), true
		}
	}
	return n, false
}
