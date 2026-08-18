package dispatch

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// Render substitutes prompt template variables with PR fields.
// Replacement is literal strings.ReplaceAll, longest key first.
func Render(tmpl string, pr scan.PR) string {
	id := ""
	if pr.Identifier != nil {
		id = *pr.Identifier
	}
	vars := []struct {
		key string
		val string
	}{
		{"{identifier}", id},
		{"{comments}", formatComments(pr.BlockingComments)},
		{"{number}", strconv.Itoa(pr.Number)},
		{"{title}", pr.Title},
		{"{repo}", pr.Repo},
		{"{url}", pr.URL},
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

func formatComments(comments []scan.Comment) string {
	lines := make([]string, 0, len(comments))
	for _, c := range comments {
		lines = append(lines, formatComment(c))
	}
	return strings.Join(lines, "\n")
}

func formatComment(c scan.Comment) string {
	if c.Path == "" {
		return fmt.Sprintf("- (no path) — %s: %s (%s)", c.Author, c.Body, c.URL)
	}
	if c.Line == nil {
		return fmt.Sprintf("- %s — %s: %s (%s)", c.Path, c.Author, c.Body, c.URL)
	}
	return fmt.Sprintf("- %s:%d — %s: %s (%s)", c.Path, *c.Line, c.Author, c.Body, c.URL)
}
