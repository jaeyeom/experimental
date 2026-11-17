package main

import (
	"fmt"
	"strings"
	"unicode/utf8"
)

// longLineExcerpt is how many characters of a long line the error shows.
const longLineExcerpt = 60

// checkLineLength rejects agent text with a line longer than limit
// characters. A line that is only a URL, a quoted line, and a line in a
// fenced code block do not count. A limit of 0 or less checks nothing.
func checkLineLength(text string, limit int) error {
	if limit <= 0 {
		return nil
	}
	fenced := false
	for i, line := range strings.Split(text, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "```") {
			fenced = !fenced
			continue
		}
		if fenced || exemptLine(trimmed) {
			continue
		}
		if n := utf8.RuneCountInString(line); n > limit {
			return longLineError(i+1, n, limit, line)
		}
	}
	return nil
}

func exemptLine(trimmed string) bool {
	if strings.HasPrefix(trimmed, "> ") || trimmed == ">" {
		return true
	}
	isURL := strings.HasPrefix(trimmed, "https://") || strings.HasPrefix(trimmed, "http://")
	return isURL && !strings.ContainsAny(trimmed, " \t")
}

func longLineError(num, length, limit int, line string) error {
	excerpt := line
	if utf8.RuneCountInString(line) > longLineExcerpt {
		excerpt = string([]rune(line)[:longLineExcerpt]) + "…"
	}
	return fmt.Errorf(`not sent: line %d is %d characters (limit %d): %q
A person reads this in an inbox, often at a glance. Rewrite it for them:
  - First line: the essence (the verdict, answer, or what you need), readable alone.
  - Then details as short bullet lines, one point each.
  - Pass multi-line text from a file or stdin, not one quoted string.
If the long line is really intended, resend with --long-lines`, num, length, limit, excerpt)
}
