package ghoutbox

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/pkg/outbox"
)

// DefaultPhrases are internal phrases refused by default.
var DefaultPhrases = []string{"per the user", "on behalf of", "agent", "routing"}

// Rule is one deny rule. Name says what kind of wording it found.
type Rule = outbox.LintRule

// Hit is one match of a rule. Location is "body line N" or path:line.
type Hit = outbox.LintHit

// LintError is a draft the lint refused. Nothing was filed.
type LintError = outbox.LintError

// BuildRules makes the deny rules from cfg. busNames are the names
// agentbus list printed; they are used when cfg.AgentbusNames is set.
func BuildRules(cfg LintConfig, busNames []string) ([]Rule, error) {
	var names []string
	if cfg.AgentbusNames {
		names = busNames
	}
	rules, err := outbox.BuildLintRules(outbox.LintConfig{
		Defaults:      cfg.Defaults,
		NameRuleLabel: "agentbus name",
		NamePattern:   cfg.AgentPattern,
		Patterns:      cfg.Patterns,
		Phrases:       cfg.Phrases,
	}, names)
	if err != nil {
		return nil, fmt.Errorf("gh-outbox lint: %w", err)
	}
	return rules, nil
}

// Lint returns every rule match in the body and the line comments.
func Lint(d Draft, rules []Rule) []Hit {
	fields := make([]outbox.TextField, 0, len(d.Lines)+1)
	fields = append(fields, outbox.TextField{
		Location:   "body",
		Text:       d.Body,
		SplitLines: true,
	})
	for _, c := range d.Lines {
		fields = append(fields, outbox.TextField{
			Location: c.Where(),
			Text:     c.Text,
		})
	}
	return outbox.LintFields(fields, rules)
}
