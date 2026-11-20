package tui

import (
	"regexp"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/charmbracelet/lipgloss"
)

// defaultFillColumn is the compose fill width when ui.fillColumn is unset.
const defaultFillColumn = 72

// listPrefixPattern is the marker at the start of a list item: optional
// indent, then -, *, +, or a number with . or ), then a space.
var listPrefixPattern = regexp.MustCompile(`^([ \t]*(?:[-*+]|\d+[.)])[ \t]+)`)

type fillKind int

const (
	fillPlain fillKind = iota
	fillItem
	fillKeep
)

type fillSpan struct {
	start, end int
	kind       fillKind
}

type wordSpot struct {
	word     int
	off      int
	onWord   bool
	inPrefix bool
	prefixAt int
}

// fillParagraph refills the paragraph that holds the cursor.
// column 0 means 72. A positive boxWidth caps the column.
// The returned line and column stay on the same word.
func fillParagraph(text string, line, col, column, boxWidth int) (string, int, int) {
	lines := strings.Split(text, "\n")
	if len(lines) == 0 {
		return text, 0, 0
	}
	line = clampIndex(line, len(lines))
	col = clampCol(lines[line], col)
	span, ok := spanAt(lines, line)
	if !ok || span.kind == fillKeep {
		return text, line, col
	}
	filled, relLine, relCol := refill(lines[span.start:span.end], line-span.start, col, fillWidth(column, boxWidth), span.kind)
	next := make([]string, 0, span.start+len(filled)+len(lines)-span.end)
	next = append(next, lines[:span.start]...)
	next = append(next, filled...)
	next = append(next, lines[span.end:]...)
	return strings.Join(next, "\n"), span.start + relLine, relCol
}

// fillWidth is the column Alt+q wraps to.
func fillWidth(column, boxWidth int) int {
	if column <= 0 {
		column = defaultFillColumn
	}
	if boxWidth > 0 && column > boxWidth {
		column = boxWidth
	}
	if column < 1 {
		return 1
	}
	return column
}

func clampIndex(i, n int) int {
	if n <= 0 || i < 0 {
		return 0
	}
	if i >= n {
		return n - 1
	}
	return i
}

func clampCol(line string, col int) int {
	n := utf8.RuneCountInString(line)
	if col < 0 {
		return 0
	}
	if col > n {
		return n
	}
	return col
}

func spanAt(lines []string, line int) (fillSpan, bool) {
	for _, span := range fillSpans(lines) {
		if line >= span.start && line < span.end {
			return span, true
		}
	}
	return fillSpan{}, false
}

func fillSpans(lines []string) []fillSpan {
	var spans []fillSpan
	for i := 0; i < len(lines); {
		if isBlank(lines[i]) {
			spans = append(spans, fillSpan{i, i + 1, fillKeep})
			i++
			continue
		}
		if ch, ok := fenceChar(lines[i]); ok {
			j := i + 1
			for j < len(lines) && !fenceCloses(lines[j], ch) {
				j++
			}
			if j < len(lines) {
				j++
			}
			spans = append(spans, fillSpan{i, j, fillKeep})
			i = j
			continue
		}
		j := i + 1
		for j < len(lines) && !isBlank(lines[j]) && !fenceStarts(lines[j]) {
			j++
		}
		spans = append(spans, blockSpans(lines, i, j)...)
		i = j
	}
	return spans
}

func blockSpans(lines []string, start, end int) []fillSpan {
	var spans []fillSpan
	for i := start; i < end; {
		if listPrefix(lines[i]) != "" {
			j := i + 1
			for j < end && listPrefix(lines[j]) == "" {
				j++
			}
			spans = append(spans, fillSpan{i, j, fillItem})
			i = j
			continue
		}
		j := i + 1
		for j < end && listPrefix(lines[j]) == "" {
			j++
		}
		kind := fillPlain
		if allIndented(lines[i:j]) {
			kind = fillKeep
		}
		spans = append(spans, fillSpan{i, j, kind})
		i = j
	}
	return spans
}

func isBlank(line string) bool {
	return strings.TrimSpace(line) == ""
}

func fenceStarts(line string) bool {
	_, ok := fenceChar(line)
	return ok
}

// fenceChar reports the fence rune when line opens a backtick or tilde fence.
func fenceChar(line string) (rune, bool) {
	rest := strings.TrimLeft(line, " \t")
	if len(rest) < 3 {
		return 0, false
	}
	ch, _ := utf8.DecodeRuneInString(rest)
	if ch != '`' && ch != '~' {
		return 0, false
	}
	n := 0
	for _, r := range rest {
		if r != ch {
			break
		}
		n++
	}
	if n < 3 {
		return 0, false
	}
	return ch, true
}

func fenceCloses(line string, ch rune) bool {
	rest := strings.TrimSpace(line)
	if utf8.RuneCountInString(rest) < 3 {
		return false
	}
	for _, r := range rest {
		if r != ch {
			return false
		}
	}
	return true
}

func listPrefix(line string) string {
	return listPrefixPattern.FindString(line)
}

func allIndented(lines []string) bool {
	if len(lines) == 0 {
		return false
	}
	for _, line := range lines {
		if !strings.HasPrefix(line, "  ") && !strings.HasPrefix(line, "\t") {
			return false
		}
	}
	return true
}

func refill(lines []string, relLine, col, width int, kind fillKind) ([]string, int, int) {
	prefix := ""
	if kind == fillItem {
		prefix = listPrefix(lines[0])
	}
	words := regionWords(lines, prefix)
	if len(words) == 0 {
		return lines, relLine, col
	}
	spot := locateWord(lines, relLine, col, prefix)
	filled := wrapWords(words, width, prefix)
	nLine, nCol := placeWord(filled, spot, prefix)
	return filled, nLine, nCol
}

func regionWords(lines []string, prefix string) []string {
	var words []string
	for i, line := range lines {
		if i == 0 && prefix != "" {
			line = strings.TrimPrefix(line, prefix)
		}
		words = append(words, strings.Fields(line)...)
	}
	return words
}

func wrapWords(words []string, column int, prefix string) []string {
	if len(words) == 0 {
		return nil
	}
	hang := lipgloss.Width(prefix)
	lines := []string{prefix + words[0]}
	width := lipgloss.Width(lines[0])
	for _, word := range words[1:] {
		w := lipgloss.Width(word)
		if width+1+w > column {
			pad := strings.Repeat(" ", hang)
			lines = append(lines, pad+word)
			width = lipgloss.Width(pad) + w
			continue
		}
		last := len(lines) - 1
		lines[last] += " " + word
		width += 1 + w
	}
	return lines
}

type wordAt struct {
	line, col, n int
}

func locateWord(lines []string, relLine, col int, prefix string) wordSpot {
	if relLine == 0 && col < contentStart(lines[0], prefix) {
		return wordSpot{inPrefix: true, prefixAt: col}
	}
	words := wordPositions(lines, prefix)
	for i, w := range words {
		if w.line != relLine || col < w.col {
			if w.line == relLine && col < w.col {
				return wordSpot{word: i - 1}
			}
			continue
		}
		if col <= w.col+w.n {
			return wordSpot{onWord: true, word: i, off: col - w.col}
		}
	}
	return wordSpot{word: lastWordBefore(words, relLine, col)}
}

func wordPositions(lines []string, prefix string) []wordAt {
	var words []wordAt
	for li, line := range lines {
		runes := []rune(line)
		i := 0
		if li == 0 {
			i = contentStart(line, prefix)
		}
		for i < len(runes) {
			if unicode.IsSpace(runes[i]) {
				i++
				continue
			}
			start := i
			for i < len(runes) && !unicode.IsSpace(runes[i]) {
				i++
			}
			words = append(words, wordAt{line: li, col: start, n: i - start})
		}
	}
	return words
}

func lastWordBefore(words []wordAt, line, col int) int {
	last := -1
	for i, w := range words {
		if w.line < line || (w.line == line && w.col+w.n <= col) {
			last = i
		}
	}
	return last
}

func placeWord(lines []string, spot wordSpot, prefix string) (int, int) {
	if len(lines) == 0 {
		return 0, 0
	}
	if spot.inPrefix {
		return 0, spot.prefixAt
	}
	if spot.word < 0 {
		return 0, contentStart(lines[0], prefix)
	}
	word := 0
	for li, line := range lines {
		runes := []rune(line)
		i := 0
		if li == 0 && prefix != "" {
			i = contentStart(line, prefix)
		}
		for i < len(runes) {
			if unicode.IsSpace(runes[i]) {
				i++
				continue
			}
			wstart := i
			for i < len(runes) && !unicode.IsSpace(runes[i]) {
				i++
			}
			if word == spot.word {
				off := i - wstart
				if spot.onWord && spot.off < off {
					off = spot.off
				}
				return li, wstart + off
			}
			word++
		}
	}
	last := len(lines) - 1
	return last, len([]rune(lines[last]))
}

func contentStart(line, prefix string) int {
	if prefix == "" {
		return 0
	}
	n := len([]rune(prefix))
	if n > len([]rune(line)) {
		return len([]rune(line))
	}
	return n
}

// fillComposer refills the focused composer's paragraph.
func (m *Model) fillComposer() {
	field, text := m.composerField()
	if field == nil || text == nil {
		return
	}
	syncField(field, *text)
	info := field.LineInfo()
	next, line, col := fillParagraph(*text, field.Line(), info.StartColumn+info.ColumnOffset, m.fillColumn, m.composerTextWidth())
	*text = next
	field.SetValue(next)
	setFieldCursor(field, line, col)
}

// composerTextWidth is the column the focused compose box wraps to.
// Zero means the terminal has not been measured.
func (m *Model) composerTextWidth() int {
	kind, ok := m.composerKind()
	if !ok {
		return 0
	}
	switch kind {
	case composerNote:
		return m.noteTextWidth()
	case composerReply:
		return m.labeledTextWidth("reply: ")
	case composerOutcome:
		return m.labeledTextWidth("outcome: ")
	default:
		width := m.detailInnerWidth(m.contentWidth())
		if width < 1 {
			return 0
		}
		return width
	}
}

func (m *Model) noteTextWidth() int {
	width := m.contentWidth()
	if !m.framed() || width < 2 {
		if width < 1 {
			return 0
		}
		return width
	}
	return composeDraftWidth(width-2, true)
}

func (m *Model) labeledTextWidth(label string) int {
	wrap := m.detailInnerWidth(m.contentWidth()) - len(label)
	if wrap < 1 {
		return 0
	}
	return wrap
}
