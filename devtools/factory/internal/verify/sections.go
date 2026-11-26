package verify

import (
	"fmt"
	"regexp"
	"strings"
)

// section is one heading and the lines under it. heading is the
// normalised text sections are matched by: markers stripped and whitespace
// trimmed, case-sensitive. display is the heading as written, for messages.
type section struct {
	heading string
	display string
	lines   []string
}

// listItem matches a list item line, which is never a setext heading title.
var listItem = regexp.MustCompile(`^([-*+]|\d+[.)])(\s|$)`)

// splitSections splits markdown text at its headings. It recognises ATX
// headings (# to ###### then a space) and setext headings (a non-blank,
// non-list line directly followed by a line of only = or only -, at least
// two). The underline is not section content. Nothing inside a fenced code
// block (``` or ~~~) is a heading. Levels are not kept: a heading matches
// another by its text, whatever its style or level.
func splitSections(text string) ([]section, error) {
	lines := strings.Split(text, "\n")
	var out []section
	var cur *section
	seen := map[string]int{}
	var scan headingScanner
	for i := 0; i < len(lines); i++ {
		heading, display, extra, ok := scan.heading(lines, i)
		if !ok {
			if cur != nil {
				cur.lines = append(cur.lines, lines[i])
			}
			continue
		}
		i += extra // A setext underline belongs to its heading.
		seen[heading]++
		if cur != nil {
			out = append(out, *cur)
		}
		cur = &section{heading: heading, display: display}
	}
	if cur != nil {
		out = append(out, *cur)
	}
	for heading, n := range seen {
		if n > 1 {
			return nil, fmt.Errorf("expected unique headings, found %q %d times", heading, n)
		}
	}
	return out, nil
}

// headingScanner tracks fenced code blocks while lines are read in order.
type headingScanner struct {
	fence string // The open fence, or "" outside a code block.
}

// heading reports whether lines[i] starts a heading. extra is how many
// following lines it consumed: 1 for a setext underline, else 0.
func (s *headingScanner) heading(lines []string, i int) (heading, display string, extra int, ok bool) {
	line := lines[i]
	if s.fence != "" {
		if closesFence(line, s.fence) {
			s.fence = ""
		}
		return "", "", 0, false
	}
	if fence := opensFence(line); fence != "" {
		s.fence = fence
		return "", "", 0, false
	}
	if heading, ok := atxHeading(line); ok {
		return heading, strings.TrimSpace(line), 0, true
	}
	if i+1 < len(lines) && setextTitle(line) && setextUnderline(lines[i+1]) {
		text := strings.TrimSpace(line)
		return text, text, 1, true
	}
	return "", "", 0, false
}

// atxHeading reads "# Title" to "###### Title", with optional closing #s.
func atxHeading(line string) (string, bool) {
	text := strings.TrimSpace(line)
	level := 0
	for level < len(text) && text[level] == '#' {
		level++
	}
	if level == 0 || level > 6 || level == len(text) || (text[level] != ' ' && text[level] != '\t') {
		return "", false
	}
	title := strings.TrimSpace(text[level:])
	title = strings.TrimSpace(strings.TrimRight(title, "#"))
	if title == "" {
		return "", false
	}
	return title, true
}

// setextTitle reports whether line can be a setext heading's title: not
// blank, not a list item, not an ATX heading, and indented at most 3 spaces.
func setextTitle(line string) bool {
	if indent(line) > 3 {
		return false
	}
	text := strings.TrimSpace(line)
	if text == "" || listItem.MatchString(text) {
		return false
	}
	_, atx := atxHeading(line)
	return !atx
}

// setextUnderline is a line of only = or only -, at least two, indented at
// most 3 spaces, ignoring trailing spaces.
func setextUnderline(line string) bool {
	if indent(line) > 3 {
		return false
	}
	text := strings.TrimSpace(line)
	if len(text) < 2 {
		return false
	}
	return strings.Trim(text, "=") == "" || strings.Trim(text, "-") == ""
}

// opensFence returns the fence (``` or ~~~, or longer) that line opens, or "".
func opensFence(line string) string {
	if indent(line) > 3 {
		return ""
	}
	text := strings.TrimSpace(line)
	for _, mark := range []byte{'`', '~'} {
		n := 0
		for n < len(text) && text[n] == mark {
			n++
		}
		if n >= 3 {
			return text[:n]
		}
	}
	return ""
}

// closesFence reports whether line closes fence: the same marker, at least
// as long, and nothing after it.
func closesFence(line, fence string) bool {
	text := strings.TrimSpace(line)
	return strings.HasPrefix(text, fence) && strings.Trim(text, fence[:1]) == ""
}

func indent(line string) int {
	return len(line) - len(strings.TrimLeft(line, " "))
}

func hasContent(lines []string) bool {
	inComment := false
	for _, line := range lines {
		if lineHasContent(line, &inComment) {
			return true
		}
	}
	return false
}

func lineHasContent(line string, inComment *bool) bool {
	text := strings.TrimSpace(line)
	for text != "" {
		if *inComment {
			end := strings.Index(text, "-->")
			if end < 0 {
				return false
			}
			text = strings.TrimSpace(text[end+3:])
			*inComment = false
			continue
		}
		start := strings.Index(text, "<!--")
		if start < 0 {
			return true
		}
		if strings.TrimSpace(text[:start]) != "" {
			return true
		}
		text = strings.TrimSpace(text[start+4:])
		*inComment = true
	}
	return false
}
