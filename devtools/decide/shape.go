package decide

import (
	"errors"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// ErrShape is returned when author text fails the inbox message shape checks.
var ErrShape = errors.New("not sent: message fails inbox shape rules")

// shapeExcerptLimit is how many runes of offending text a shape error shows.
const shapeExcerptLimit = 60

// longWordThreshold is the rune length above which a whitespace-free token
// (such as a URL, file path, identifier, or hash) counts as 0 toward line
// length limits.
const longWordThreshold = 40

// ShapeLimits configures the inbox message shape checks. A zero value disables
// that check.
type ShapeLimits struct {
	MaxLineLength      int
	MaxFirstLineLength int
	MaxParagraphLines  int
	MaxMessageChars    int
}

// ShapeLimits returns the configured message shape limits.
func (c Config) ShapeLimits() ShapeLimits {
	return ShapeLimits{
		MaxLineLength:      c.MaxLineLength(),
		MaxFirstLineLength: c.MaxFirstLineLength(),
		MaxParagraphLines:  c.MaxParagraphLines(),
		MaxMessageChars:    c.MaxMessageChars(),
	}
}

// CheckShape validates author text against limits. It checks total size,
// first-line length and completeness, paragraph length, and line length.
func CheckShape(text string, limits ShapeLimits) error {
	normalized := strings.ReplaceAll(text, "\r\n", "\n")
	lines := strings.Split(normalized, "\n")
	var failures []string

	if limits.MaxMessageChars > 0 {
		if total := utf8.RuneCountInString(text); total > limits.MaxMessageChars {
			failures = append(failures, fmt.Sprintf("message is %d characters (limit %d): %q",
				total, limits.MaxMessageChars, shapeExcerpt(normalized)))
		}
	}

	if limits.MaxFirstLineLength > 0 && len(lines) > 0 {
		first := lines[0]
		if n, excluded := measureLine(first); n > limits.MaxFirstLineLength {
			failures = append(failures, formatLineLengthFailure("first line", n, excluded, limits.MaxFirstLineLength, first))
		}
		if len(lines) > 1 {
			second := lines[1]
			if strings.TrimSpace(second) != "" && !isBulletLine(second) {
				failures = append(failures, fmt.Sprintf("first line runs into line 2 without a blank line or bullet: %q",
					shapeExcerpt(first)))
			}
		}
	}

	if limits.MaxParagraphLines > 0 {
		failures = append(failures, checkParagraphLines(lines, limits.MaxParagraphLines)...)
	}

	if limits.MaxLineLength > 0 {
		failures = append(failures, checkLineLengths(lines, limits.MaxLineLength)...)
	}

	if len(failures) == 0 {
		return nil
	}
	return fmt.Errorf(`%w:
  - %s
A person reads this in an inbox, often at a glance. Rewrite it for them:
  - First line: the verdict (the answer, decision, or what you need), readable alone, followed by a blank line or bullet.
  - Then details as short bullet lines, one point each (no long paragraphs).
  - Pass multi-line text through --text-file or stdin, and exact post drafts or forwarded user text through --verbatim-file`,
		ErrShape, strings.Join(failures, "\n  - "))
}

func checkParagraphLines(lines []string, limit int) []string {
	var failures []string
	fenced := false
	paraStart := 0
	paraLen := 0
	flush := func() {
		if paraLen > limit {
			failures = append(failures, fmt.Sprintf("paragraph at line %d is %d lines (limit %d): %q",
				paraStart, paraLen, limit, shapeExcerpt(lines[paraStart-1])))
		}
		paraLen = 0
	}
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "```") {
			flush()
			fenced = !fenced
			continue
		}
		if fenced {
			continue
		}
		if trimmed == "" || isBulletLine(line) || isQuoteLine(trimmed) {
			flush()
			continue
		}
		if paraLen == 0 {
			paraStart = i + 1
		}
		paraLen++
	}
	flush()
	return failures
}

func checkLineLengths(lines []string, limit int) []string {
	var failures []string
	fenced := false
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "```") {
			fenced = !fenced
			continue
		}
		if fenced || exemptLine(trimmed) {
			continue
		}
		if n, excluded := measureLine(line); n > limit {
			failures = append(failures, formatLineLengthFailure(fmt.Sprintf("line %d", i+1), n, excluded, limit, line))
		}
	}
	return failures
}

// measureLine returns line's rune count after excluding whitespace-free tokens
// longer than longWordThreshold runes, along with how many tokens were excluded.
func measureLine(line string) (length, excluded int) {
	runLen := 0
	for _, r := range line {
		if unicode.IsSpace(r) {
			if runLen > longWordThreshold {
				excluded++
			} else {
				length += runLen
			}
			runLen = 0
			length++
			continue
		}
		runLen++
	}
	if runLen > longWordThreshold {
		excluded++
	} else {
		length += runLen
	}
	return length, excluded
}

func formatLineLengthFailure(label string, length, excluded, limit int, line string) string {
	if excluded == 0 {
		return fmt.Sprintf("%s is %d characters (limit %d): %q", label, length, limit, shapeExcerpt(line))
	}
	word := "long words"
	if excluded == 1 {
		word = "long word"
	}
	return fmt.Sprintf("%s is %d characters after excluding %d %s (limit %d): %q",
		label, length, excluded, word, limit, shapeExcerpt(line))
}

func exemptLine(trimmed string) bool {
	if isQuoteLine(trimmed) {
		return true
	}
	isURL := strings.HasPrefix(trimmed, "https://") || strings.HasPrefix(trimmed, "http://") || strings.Contains(trimmed, "://")
	return isURL && !strings.ContainsAny(trimmed, " \t")
}

func isQuoteLine(trimmed string) bool {
	return strings.HasPrefix(trimmed, "> ") || trimmed == ">"
}

func isBulletLine(line string) bool {
	trimmed := strings.TrimSpace(line)
	switch {
	case trimmed == "":
		return false
	case trimmed == "-", trimmed == "*", trimmed == "+":
		return true
	case strings.HasPrefix(trimmed, "- "), strings.HasPrefix(trimmed, "* "), strings.HasPrefix(trimmed, "+ "):
		return true
	default:
		return isNumberedBullet(trimmed)
	}
}

func isNumberedBullet(trimmed string) bool {
	i := 0
	for i < len(trimmed) && trimmed[i] >= '0' && trimmed[i] <= '9' {
		i++
	}
	if i == 0 || i+1 >= len(trimmed) {
		return false
	}
	return (trimmed[i] == '.' || trimmed[i] == ')') && trimmed[i+1] == ' '
}

func shapeExcerpt(s string) string {
	if utf8.RuneCountInString(s) <= shapeExcerptLimit {
		return s
	}
	return string([]rune(s)[:shapeExcerptLimit]) + "…"
}

func checkOptionsShape(options []Option, limits ShapeLimits) error {
	for _, opt := range options {
		if err := CheckShape(opt.Label, limits); err != nil {
			return fmt.Errorf("option %q label: %w", opt.Key, err)
		}
	}
	return nil
}

func loadShapeLimits() (ShapeLimits, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return ShapeLimits{}, err
	}
	return cfg.ShapeLimits(), nil
}

func checkAuthorShape(text, via string) error {
	if via == ViaUI {
		return nil
	}
	limits, err := loadShapeLimits()
	if err != nil {
		return err
	}
	return CheckShape(text, limits)
}
