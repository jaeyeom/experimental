package genfiles

import (
	"errors"
	"fmt"
	"regexp"
	"strings"
	"unicode"
)

// ValidatePattern reports whether p is a pattern this package supports: the
// subset of .gitattributes glob syntax made of literal characters, `*`, `?`
// and `**`. Negation, bracket classes, escapes, quotes, whitespace and a
// trailing `/` (directory-only patterns) are rejected.
func ValidatePattern(p string) error {
	switch {
	case p == "":
		return errors.New("pattern is empty")
	case strings.HasPrefix(p, "!"):
		return fmt.Errorf("pattern %q: negation (!) is not supported", p)
	case strings.HasSuffix(p, "/"):
		return fmt.Errorf("pattern %q: a trailing / matches only directories, which hold no attributes", p)
	}
	for _, r := range p {
		switch {
		case r == '[' || r == ']':
			return fmt.Errorf("pattern %q: bracket classes are not supported", p)
		case r == '\\' || r == '"':
			return fmt.Errorf("pattern %q: escapes and quotes are not supported", p)
		case unicode.IsSpace(r) || unicode.IsControl(r):
			return fmt.Errorf("pattern %q: whitespace and control characters are not supported", p)
		}
	}
	return nil
}

// Match reports whether path, repo-relative and /-separated, matches pattern
// under .gitattributes rules. A pattern with no `/` matches the file name at
// any depth; a pattern with a `/` (a leading one included) is anchored at the
// repository root. `*` and `?` stay within one path segment, `**/` matches
// zero or more directories, and a trailing `/**` matches everything below.
// An invalid pattern matches nothing.
func Match(pattern, path string) bool {
	re, err := compile(pattern)
	return err == nil && re.MatchString(path)
}

func compile(pattern string) (*regexp.Regexp, error) {
	if err := ValidatePattern(pattern); err != nil {
		return nil, err
	}
	anchored := strings.Contains(pattern, "/")
	p := strings.TrimPrefix(pattern, "/")
	var b strings.Builder
	b.WriteString("^")
	if !anchored {
		b.WriteString("(?:.*/)?")
	}
	for i := 0; i < len(p); {
		switch {
		case strings.HasPrefix(p[i:], "**/") && (i == 0 || p[i-1] == '/'):
			b.WriteString("(?:.*/)?")
			i += 3
		case p[i:] == "**" && i > 0 && p[i-1] == '/':
			b.WriteString(".+")
			i += 2
		case p[i] == '*':
			b.WriteString("[^/]*")
			i++
		case p[i] == '?':
			b.WriteString("[^/]")
			i++
		default:
			b.WriteString(regexp.QuoteMeta(p[i : i+1]))
			i++
		}
	}
	b.WriteString("$")
	re, err := regexp.Compile(b.String())
	if err != nil {
		return nil, fmt.Errorf("pattern %q: %w", pattern, err)
	}
	return re, nil
}
