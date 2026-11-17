package ghoutbox

import (
	"fmt"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"unicode"
)

// DefaultPhrases are internal phrases refused by default.
var DefaultPhrases = []string{"per the user", "on behalf of", "agent", "routing"}

// decideIDPattern matches ids from the local decide tool, like D-123.
const decideIDPattern = `\bD-[0-9]+\b`

// Rule is one deny rule. Name says what kind of wording it found.
type Rule struct {
	Name string
	Re   *regexp.Regexp
}

// Hit is one match of a rule. Location is "body line N" or path:line.
type Hit struct {
	Rule     string
	Location string
	Match    string
}

func (h Hit) String() string {
	return fmt.Sprintf("%s: %q (%s)", h.Location, h.Match, h.Rule)
}

// LintError is a draft the lint refused. Nothing was filed.
type LintError struct{ Hits []Hit }

func (e *LintError) Error() string {
	lines := make([]string, 0, len(e.Hits)+1)
	lines = append(lines, "draft refused: it contains internal wording")
	for _, h := range e.Hits {
		lines = append(lines, "  "+h.String())
	}
	return strings.Join(lines, "\n")
}

// phraseRule matches phrase case-insensitively as whole words.
func phraseRule(name, phrase string) Rule {
	expr := regexp.QuoteMeta(phrase)
	if r := []rune(phrase); len(r) > 0 && isWord(r[0]) {
		expr = `\b` + expr
	}
	if r := []rune(phrase); len(r) > 0 && isWord(r[len(r)-1]) {
		expr += `\b`
	}
	return Rule{Name: name, Re: regexp.MustCompile(`(?i)` + expr)}
}

func isWord(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// BuildRules makes the deny rules from cfg. busNames are the names
// agentbus list printed; they are used when cfg.AgentbusNames is set.
func BuildRules(cfg LintConfig, busNames []string) ([]Rule, error) {
	var rules []Rule
	if cfg.Defaults {
		rules = append(rules, Rule{Name: "decide id", Re: regexp.MustCompile(decideIDPattern)})
		for _, p := range DefaultPhrases {
			rules = append(rules, phraseRule("internal phrase", p))
		}
	}
	if cfg.AgentbusNames {
		for _, n := range busNames {
			if strings.TrimSpace(n) != "" {
				rules = append(rules, phraseRule("agentbus name", n))
			}
		}
	}
	if cfg.AgentPattern != "" {
		re, err := regexp.Compile(cfg.AgentPattern)
		if err != nil {
			return nil, fmt.Errorf("lint agent_pattern: %w", err)
		}
		rules = append(rules, Rule{Name: "agentbus name", Re: re})
	}
	for _, p := range cfg.Patterns {
		re, err := regexp.Compile(p)
		if err != nil {
			return nil, fmt.Errorf("lint patterns %q: %w", p, err)
		}
		rules = append(rules, Rule{Name: "denied pattern", Re: re})
	}
	for _, p := range cfg.Phrases {
		rules = append(rules, phraseRule("internal phrase", p))
	}
	return rules, nil
}

// Lint returns every rule match in the body and the line comments.
func Lint(d Draft, rules []Rule) []Hit {
	var hits []Hit
	for i, line := range strings.Split(d.Body, "\n") {
		hits = append(hits, lintText(line, "body line "+strconv.Itoa(i+1), rules)...)
	}
	for _, c := range d.Lines {
		hits = append(hits, lintText(c.Text, c.Where(), rules)...)
	}
	return hits
}

// lintText returns the hits in text in the order they appear.
func lintText(text, location string, rules []Rule) []Hit {
	type found struct {
		at  int
		hit Hit
	}
	var all []found
	for _, r := range rules {
		for _, m := range r.Re.FindAllStringIndex(text, -1) {
			all = append(all, found{m[0], Hit{Rule: r.Name, Location: location, Match: text[m[0]:m[1]]}})
		}
	}
	slices.SortStableFunc(all, func(a, b found) int { return a.at - b.at })
	hits := make([]Hit, 0, len(all))
	for _, f := range all {
		hits = append(hits, f.hit)
	}
	return hits
}
