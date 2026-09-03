package dispatch

import (
	"fmt"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// inlineLink matches [label](url) with an optional quoted title.
var inlineLink = regexp.MustCompile(`\[([^\]]*)\]\(([^)\s]+)(?:\s+"[^"]*")?\)`)

// Render substitutes prompt template variables with PR fields.
// Replacement is literal strings.ReplaceAll, longest key first.
func Render(tmpl string, pr scan.PR, cfg config.Config) string {
	id := ""
	if pr.Identifier != nil {
		id = *pr.Identifier
	}
	vars := []struct {
		key string
		val string
	}{
		{"{identifier}", id},
		{"{comments}", formatComments(pr.BlockingComments, cfg)},
		{"{number}", strconv.Itoa(pr.Number)},
		{"{title}", pr.Title},
		{"{repo}", pr.Repo},
		{"{url}", pr.URL},
		{"{head}", pr.Head},
		{"{base}", pr.Base},
	}
	sort.Slice(vars, func(i, j int) bool {
		return len(vars[i].key) > len(vars[j].key)
	})
	out := tmpl
	for _, v := range vars {
		out = strings.ReplaceAll(out, v.key, v.val)
	}
	return out
}

func formatComments(comments []scan.Comment, cfg config.Config) string {
	lines := make([]string, 0, len(comments))
	for _, c := range comments {
		lines = append(lines, formatComment(c, cfg))
	}
	return strings.Join(lines, "\n")
}

func formatComment(c scan.Comment, cfg config.Config) string {
	body := stripOverlongLinks(c.Body, cfg.CommentLinkMaxChars)
	body = capBody(body, c.URL, cfg.CommentBodyMaxChars)
	if c.Path == "" {
		return fmt.Sprintf("- (no path) — %s: %s (%s)", c.Author, body, c.URL)
	}
	if c.Line == nil {
		return fmt.Sprintf("- %s — %s: %s (%s)", c.Path, c.Author, body, c.URL)
	}
	return fmt.Sprintf("- %s:%d — %s: %s (%s)", c.Path, *c.Line, c.Author, body, c.URL)
}

func stripOverlongLinks(body string, limit int) string {
	if limit <= 0 {
		return body
	}
	return inlineLink.ReplaceAllStringFunc(body, func(match string) string {
		parts := inlineLink.FindStringSubmatch(match)
		if len(parts) < 3 {
			return match
		}
		if len(parts[2]) > limit {
			return parts[1]
		}
		return match
	})
}

func capBody(body, threadURL string, limit int) string {
	if limit <= 0 {
		return body
	}
	runes := []rune(body)
	if len(runes) <= limit {
		return body
	}
	return string(runes[:limit]) + "… [truncated, see " + threadURL + "]"
}
