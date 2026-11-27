package outbox

import (
	"fmt"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

// decideIDPattern matches decide item ids such as D-123.
const decideIDPattern = `\bD-[0-9]+\b`

// DefaultInternalPhrases returns internal phrases refused by default when lint defaults are enabled.
func DefaultInternalPhrases() []string {
	return []string{"per the user", "on behalf of", "agent", "routing"}
}

// LintRule is one preflight text deny rule.
type LintRule struct {
	Name string
	Re   *regexp.Regexp
}

// LintHit is one preflight rule match at Location.
type LintHit struct {
	Rule     string
	Location string
	Match    string
}

// String formats h as "<location>: <match> (<rule>)".
func (h LintHit) String() string {
	return fmt.Sprintf("%s: %q (%s)", h.Location, h.Match, h.Rule)
}

// LintError reports preflight lint hits that prevented filing an approval.
type LintError struct {
	Header string
	Hits   []LintHit
}

// Error formats the lint refusal summary and each hit.
func (e *LintError) Error() string {
	header := e.Header
	if header == "" {
		header = "draft refused: it contains internal wording"
	}
	lines := make([]string, 0, len(e.Hits)+1)
	lines = append(lines, header)
	for _, h := range e.Hits {
		lines = append(lines, "  "+h.String())
	}
	return strings.Join(lines, "\n")
}

// TextField identifies one named text field to preflight-lint.
type TextField struct {
	Location string
	Text     string
	// SplitLines splits multi-line text into "<Location> line N" locations when true.
	SplitLines bool
}

// LintConfig configures the deny rules constructed by BuildLintRules.
type LintConfig struct {
	Defaults      bool
	NameRuleLabel string
	NamePattern   string
	Patterns      []string
	Phrases       []string
}

// PhraseRule matches phrase case-insensitively as whole words.
func PhraseRule(name, phrase string) LintRule {
	expr := regexp.QuoteMeta(phrase)
	if r := []rune(phrase); len(r) > 0 && isWordRune(r[0]) {
		expr = `\b` + expr
	}
	if r := []rune(phrase); len(r) > 0 && isWordRune(r[len(r)-1]) {
		expr += `\b`
	}
	return LintRule{Name: name, Re: regexp.MustCompile(`(?i)` + expr)}
}

func isWordRune(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// BuildLintRules builds deny rules from cfg and forbiddenNames.
func BuildLintRules(cfg LintConfig, forbiddenNames []string) ([]LintRule, error) {
	var rules []LintRule
	if cfg.Defaults {
		rules = append(rules, LintRule{Name: "decide id", Re: regexp.MustCompile(decideIDPattern)})
		for _, p := range DefaultInternalPhrases() {
			rules = append(rules, PhraseRule("internal phrase", p))
		}
	}
	nameLabel := cfg.NameRuleLabel
	if nameLabel == "" {
		nameLabel = "forbidden name"
	}
	for _, n := range forbiddenNames {
		if strings.TrimSpace(n) != "" {
			rules = append(rules, PhraseRule(nameLabel, n))
		}
	}
	if cfg.NamePattern != "" {
		re, err := regexp.Compile(cfg.NamePattern)
		if err != nil {
			return nil, fmt.Errorf("lint name pattern: %w", err)
		}
		rules = append(rules, LintRule{Name: nameLabel, Re: re})
	}
	for _, p := range cfg.Patterns {
		re, err := regexp.Compile(p)
		if err != nil {
			return nil, fmt.Errorf("lint patterns %q: %w", p, err)
		}
		rules = append(rules, LintRule{Name: "denied pattern", Re: re})
	}
	for _, p := range cfg.Phrases {
		rules = append(rules, PhraseRule("internal phrase", p))
	}
	return rules, nil
}

// LintFields checks every TextField against rules in order.
func LintFields(fields []TextField, rules []LintRule) []LintHit {
	var hits []LintHit
	for _, f := range fields {
		if f.SplitLines {
			for i, line := range strings.Split(f.Text, "\n") {
				hits = append(hits, LintText(line, f.Location+" line "+strconv.Itoa(i+1), rules)...)
			}
			continue
		}
		hits = append(hits, LintText(f.Text, f.Location, rules)...)
	}
	return hits
}

// LintText returns all matches in text at location sorted by match start byte.
func LintText(text, location string, rules []LintRule) []LintHit {
	type found struct {
		at  int
		hit LintHit
	}
	var all []found
	for _, r := range rules {
		for _, m := range r.Re.FindAllStringIndex(text, -1) {
			all = append(all, found{m[0], LintHit{Rule: r.Name, Location: location, Match: text[m[0]:m[1]]}})
		}
	}
	slices.SortStableFunc(all, func(a, b found) int { return a.at - b.at })
	hits := make([]LintHit, 0, len(all))
	for _, f := range all {
		hits = append(hits, f.hit)
	}
	return hits
}

// Default inbox message shape bounds aligned with decide's inbox checks.
const (
	MaxFirstLineLength = 100
	MaxLineLength      = 160
	MaxMessageChars    = 1200
)

// SingleLine collapses runs of whitespace in s into single spaces.
func SingleLine(s string) string {
	return strings.Join(strings.Fields(s), " ")
}

// TruncateRunes truncates s to at most limit runes, appending an ellipsis when truncated.
func TruncateRunes(s string, limit int) string {
	if limit <= 0 {
		return ""
	}
	if utf8.RuneCountInString(s) <= limit {
		return s
	}
	if limit == 1 {
		return "…"
	}
	return string([]rune(s)[:limit-1]) + "…"
}

// ShapeBullet formats "- [label: ]value" collapsed onto one bounded line.
func ShapeBullet(label, value string) string {
	v := SingleLine(value)
	if v == "" {
		return ""
	}
	prefix := "- "
	if label != "" {
		prefix += label + ": "
	}
	return TruncateRunes(prefix+v, MaxLineLength)
}

// ShapeLinkLine formats link as a bullet when within MaxLineLength, or a bare line otherwise.
func ShapeLinkLine(link string) string {
	if link == "" {
		return ""
	}
	if utf8.RuneCountInString("- "+link) <= MaxLineLength {
		return "- " + link
	}
	return SingleLine(link)
}

// ShapeMessage formats a first-line verdict followed by non-empty bullet lines
// within decide's inbox shape limits.
func ShapeMessage(header string, bullets ...string) string {
	h := TruncateRunes(SingleLine(header), MaxFirstLineLength)
	var kept []string
	for _, b := range bullets {
		if b != "" {
			kept = append(kept, b)
		}
	}
	if len(kept) == 0 {
		return h
	}
	msg := h + "\n\n" + strings.Join(kept, "\n")
	if utf8.RuneCountInString(msg) <= MaxMessageChars {
		return msg
	}
	for len(kept) > 1 && utf8.RuneCountInString(h+"\n\n"+strings.Join(kept, "\n")) > MaxMessageChars {
		kept = kept[:len(kept)-1]
	}
	msg = h + "\n\n" + strings.Join(kept, "\n")
	return TruncateRunes(msg, MaxMessageChars)
}
