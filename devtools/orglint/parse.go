// Package orglint is a small Go org-mode parser and linter driver. It covers
// the subset of org syntax that the lint checkers need and never starts Emacs.
package orglint

import (
	"bufio"
	"fmt"
	"io"
	"regexp"
	"strings"
)

// Headline is an org headline. EndLine is the last line of its section, which
// includes subheadings.
type Headline struct {
	Line    int
	EndLine int
	Level   int
	Title   string
	Todo    string
	Tags    []string
}

// Keyword is a "#+KEY: value" line. Affiliated is true for keywords such as
// NAME and CAPTION that attach to the following element.
type Keyword struct {
	Line       int
	Key        string // upper-cased
	Value      string
	Affiliated bool
}

// Block is a "#+BEGIN_NAME params" ... "#+END_NAME" block. EndLine is zero if
// the block is not closed.
type Block struct {
	Line    int
	EndLine int
	Name    string // upper-cased, e.g. SRC or EXAMPLE
	Params  string
}

// Drawer is a ":NAME:" ... ":END:" drawer. EndLine is zero if not closed.
type Drawer struct {
	Line       int
	EndLine    int
	Name       string
	Properties []Property
}

// Property is a node property inside a PROPERTIES drawer.
type Property struct {
	Line  int
	Name  string
	Value string
}

// Planning is a line starting with SCHEDULED:, DEADLINE: or CLOSED:.
type Planning struct {
	Line int
	Text string
}

// LinkKind says how a link was written.
type LinkKind int

// Link kinds.
const (
	BracketLink LinkKind = iota // [[target][description]]
	AngleLink                   // <https://example.com>
	PlainLink                   // https://example.com
)

// Link is a link in running text.
type Link struct {
	Line   int
	Kind   LinkKind
	Target string
	Desc   string
	End    int // byte offset in the line just after the link
}

// Target is a dedicated target "<<name>>".
type Target struct {
	Line  int
	Value string
}

// Footnote is a footnote reference or definition.
type Footnote struct {
	Line       int
	Label      string
	Definition bool
	Inline     bool // "[fn:label:text]" or "[fn::text]", which define themselves
}

// ListItem is a list item. Indent is the number of leading spaces.
type ListItem struct {
	Line    int
	Indent  int
	Bullet  string
	Checked string // "", " ", "X" or "-"
}

// Document is a parsed org file.
type Document struct {
	Dir       string // directory of the file, for resolving relative file links
	Lines     []string
	Headlines []Headline
	Keywords  []Keyword
	Blocks    []Block
	Drawers   []Drawer
	Plannings []Planning
	Links     []Link
	Footnotes []Footnote
	Targets   []Target
	Items     []ListItem
}

var (
	headlineRE = regexp.MustCompile(`^(\*+) +(.*?)\s*$`)
	tagsRE     = regexp.MustCompile(`\s+:([\w@#%]+(?::[\w@#%]+)*):$`)
	keywordRE  = regexp.MustCompile(`^[ \t]*#\+([^\s:]+):(?:\s+(.*?))?\s*$`)
	blockRE    = regexp.MustCompile(`(?i)^[ \t]*#\+BEGIN_(\S+)(?:\s+(.*?))?\s*$`)
	endRE      = regexp.MustCompile(`(?i)^[ \t]*#\+END_(\S+)\s*$`)
	drawerRE   = regexp.MustCompile(`^[ \t]*:([-\w]+):\s*$`)
	propRE     = regexp.MustCompile(`^[ \t]*:([^\s:]+\+?):(?:\s+(.*?))?\s*$`)
	planningRE = regexp.MustCompile(`^[ \t]*(?:SCHEDULED|DEADLINE|CLOSED):`)
	itemRE     = regexp.MustCompile(`^([ \t]*)([-+]|\d+[.)]|[*])(?: +\[([ X-])\])?(?:\s|$)`)
	fnDefRE    = regexp.MustCompile(`^\[fn:([-\w]+)\]`)
	fnRefRE    = regexp.MustCompile(`\[fn:([-\w]*)(:[^\]]*)?\]`)
	verbatimRE = regexp.MustCompile(`(^|[\s({'"-])([=~])([^\s=~](?:[^\n]*?[^\s])?)(?:[=~])($|[\s.,;:!?)}'"\[-])`)
	targetRE   = regexp.MustCompile(`<<([^<>\n]+)>>`)
	bracketRE  = regexp.MustCompile(`\[\[((?:\\[\[\]\\]|[^\[\]\\])+)\](?:\[((?:\\.|[^\]\\]|\][^\]])+?)\])?\]`)
	angleRE    = regexp.MustCompile(`<([A-Za-z][A-Za-z0-9+.-]*:[^<>\s]+)>`)
	plainRE    = regexp.MustCompile(`(?:^|[^\w<\[(])((?:https?|ftp|file|mailto|irc|news|doi|info|shell|elisp):[^\s<>\[\]()"']*[^\s<>\[\]()"'.,;:!?])`)
)

var affiliated = map[string]bool{
	"NAME": true, "CAPTION": true, "HEADER": true, "RESULTS": true,
	"PLOT": true, "ATTR_ASCII": true, "ATTR_HTML": true, "ATTR_LATEX": true,
	"ATTR_ORG": true, "ATTR_BEAMER": true, "ATTR_TEXINFO": true,
	"ATTR_ODT": true, "ATTR_MAN": true, "ATTR_ICALENDAR": true,
}

// ParseString parses org text.
func ParseString(s string) *Document {
	d, _ := Parse(strings.NewReader(s))
	return d
}

// Parse parses org text from r. Lines are numbered from 1.
func Parse(r io.Reader) (*Document, error) {
	d := &Document{}
	sc := bufio.NewScanner(r)
	sc.Buffer(nil, 1<<24)
	for sc.Scan() {
		d.Lines = append(d.Lines, strings.TrimSuffix(sc.Text(), "\r"))
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("read org: %w", err)
	}
	d.parse()
	return d, nil
}

func (d *Document) parse() {
	openBlock, openDrawer := -1, -1
	for i, text := range d.Lines {
		n := i + 1
		if openBlock >= 0 {
			if m := endRE.FindStringSubmatch(text); m != nil && strings.EqualFold(m[1], d.Blocks[openBlock].Name) {
				d.Blocks[openBlock].EndLine = n
				openBlock = -1
			}
			continue // block contents are raw
		}
		if openDrawer >= 0 {
			if d.drawerLine(openDrawer, n, text) {
				if strings.EqualFold(strings.TrimSpace(text), ":END:") {
					openDrawer = -1
				}
				continue
			}
			openDrawer = -1 // an unclosed drawer ends at the next headline
		}
		switch d.element(n, text) {
		case openedBlock:
			openBlock = len(d.Blocks) - 1
		case openedDrawer:
			openDrawer = len(d.Drawers) - 1
		}
	}
	d.closeSections()
}

// drawerLine handles a line inside an open drawer. It returns false if the
// line is a headline, which ends an unclosed drawer.
func (d *Document) drawerLine(drawer, n int, text string) bool {
	dr := &d.Drawers[drawer]
	if strings.EqualFold(strings.TrimSpace(text), ":END:") {
		dr.EndLine = n
		return true
	}
	if headlineRE.MatchString(text) {
		return false
	}
	if m := propRE.FindStringSubmatch(text); m != nil && strings.EqualFold(dr.Name, "PROPERTIES") {
		dr.Properties = append(dr.Properties, Property{n, m[1], m[2]})
		return true
	}
	d.inline(n, text)
	return true
}

type opened int

const (
	openedNothing opened = iota
	openedBlock
	openedDrawer
)

// element records the element that starts on a line outside blocks and
// drawers.
func (d *Document) element(n int, text string) opened {
	trimmed := strings.TrimLeft(text, " \t")
	switch {
	case headlineRE.MatchString(text):
		d.addHeadline(n, text)
	case blockRE.MatchString(text):
		return d.openBlock(n, text)
	case keywordRE.MatchString(text):
		m := keywordRE.FindStringSubmatch(text)
		key := strings.ToUpper(m[1])
		d.Keywords = append(d.Keywords, Keyword{n, key, m[2], affiliated[key] || strings.HasPrefix(key, "ATTR_")})
	case strings.HasPrefix(trimmed, "# ") || trimmed == "#":
		// comment
	case strings.HasPrefix(trimmed, ": ") || trimmed == ":":
		// fixed-width area
	case drawerRE.MatchString(text) && !strings.EqualFold(trimmed, ":END:"):
		return d.openDrawer(n, text)
	case planningRE.MatchString(text):
		d.Plannings = append(d.Plannings, Planning{n, strings.TrimSpace(text)})
	default:
		d.addItemAndFootnoteDef(n, text)
		d.inline(n, text)
	}
	return openedNothing
}

// openBlock records the block that starts on a line. An unclosed block is
// plain text.
func (d *Document) openBlock(n int, text string) opened {
	m := blockRE.FindStringSubmatch(text)
	d.Blocks = append(d.Blocks, Block{Line: n, Name: strings.ToUpper(m[1]), Params: m[2]})
	if d.closes(n, func(l string) bool {
		e := endRE.FindStringSubmatch(l)
		return e != nil && strings.EqualFold(e[1], m[1])
	}, false) {
		return openedBlock
	}
	return openedNothing
}

// openDrawer records the drawer that starts on a line. An unclosed drawer is
// plain text.
func (d *Document) openDrawer(n int, text string) opened {
	m := drawerRE.FindStringSubmatch(text)
	d.Drawers = append(d.Drawers, Drawer{Line: n, Name: m[1]})
	if d.closes(n, func(l string) bool { return strings.EqualFold(strings.TrimSpace(l), ":END:") }, true) {
		return openedDrawer
	}
	return openedNothing
}

// closes reports whether a line after line n satisfies isEnd. If stopAtHeadline
// is set, the search ends at the next headline.
func (d *Document) closes(n int, isEnd func(string) bool, stopAtHeadline bool) bool {
	for _, l := range d.Lines[n:] {
		if stopAtHeadline && headlineRE.MatchString(l) {
			return false
		}
		if isEnd(l) {
			return true
		}
	}
	return false
}

func (d *Document) addHeadline(n int, text string) {
	m := headlineRE.FindStringSubmatch(text)
	h := Headline{Line: n, Level: len(m[1]), Title: m[2]}
	if t := tagsRE.FindStringSubmatch(" " + h.Title); t != nil {
		h.Tags = strings.Split(t[1], ":")
		h.Title = strings.TrimSpace(tagsRE.ReplaceAllString(" "+h.Title, ""))
	}
	if first, rest, _ := strings.Cut(h.Title, " "); isTodoWord(first) {
		h.Todo, h.Title = first, strings.TrimSpace(rest)
	}
	d.Headlines = append(d.Headlines, h)
	d.inline(n, h.Title)
}

func isTodoWord(s string) bool {
	switch s {
	case "TODO", "DONE", "NEXT", "WAIT", "WAITING", "CANCELLED", "CANCELED":
		return true
	}
	return false
}

func (d *Document) addItemAndFootnoteDef(n int, text string) {
	if m := fnDefRE.FindStringSubmatch(text); m != nil {
		d.Footnotes = append(d.Footnotes, Footnote{Line: n, Label: m[1], Definition: true})
	}
	if m := itemRE.FindStringSubmatch(text); m != nil {
		d.Items = append(d.Items, ListItem{n, len(m[1]), m[2], m[3]})
	}
}

// inline records links, targets and footnote references found in text.
func (d *Document) inline(n int, text string) {
	rest := []byte(text)
	// Verbatim and code text, =like this= or ~this~, holds no links.
	for _, m := range verbatimRE.FindAllStringSubmatchIndex(text, -1) {
		if text[m[4]] == text[m[7]] {
			for i := m[4]; i <= m[7]; i++ {
				rest[i] = ' '
			}
		}
	}
	masked := string(rest)
	blank := func(m []int) {
		for i := m[0]; i < m[1]; i++ {
			rest[i] = ' '
		}
	}
	for _, m := range bracketRE.FindAllStringSubmatchIndex(masked, -1) {
		l := Link{Line: n, Kind: BracketLink, Target: text[m[2]:m[3]], End: m[1]}
		if m[4] >= 0 {
			l.Desc = text[m[4]:m[5]]
		}
		d.Links = append(d.Links, l)
		blank(m)
	}
	for _, m := range angleRE.FindAllSubmatchIndex(rest, -1) {
		d.Links = append(d.Links, Link{Line: n, Kind: AngleLink, Target: text[m[2]:m[3]], End: m[1]})
		blank(m)
	}
	for _, m := range plainRE.FindAllSubmatchIndex(rest, -1) {
		d.Links = append(d.Links, Link{Line: n, Kind: PlainLink, Target: text[m[2]:m[3]], End: m[3]})
	}
	d.targets(n, string(rest))
	def := fnDefRE.FindStringSubmatchIndex(text)
	for _, m := range fnRefRE.FindAllStringSubmatchIndex(masked, -1) {
		if def != nil && m[0] == def[0] {
			continue // a definition, not a reference
		}
		d.Footnotes = append(d.Footnotes, Footnote{
			Line: n, Label: text[m[2]:m[3]], Inline: m[4] >= 0,
		})
	}
}

// targets records "<<name>>" targets, skipping "<<<radio>>>" targets.
func (d *Document) targets(n int, text string) {
	for _, m := range targetRE.FindAllStringSubmatchIndex(text, -1) {
		if m[0] > 0 && text[m[0]-1] == '<' || m[1] < len(text) && text[m[1]] == '>' {
			continue
		}
		v := text[m[2]:m[3]]
		if strings.TrimSpace(v) == v {
			d.Targets = append(d.Targets, Target{n, v})
		}
	}
}

// closeSections sets each headline's EndLine to the line before the next
// headline of the same or a higher level.
func (d *Document) closeSections() {
	for i := range d.Headlines {
		end := len(d.Lines)
		for _, next := range d.Headlines[i+1:] {
			if next.Level <= d.Headlines[i].Level {
				end = next.Line - 1
				break
			}
		}
		d.Headlines[i].EndLine = end
	}
}
