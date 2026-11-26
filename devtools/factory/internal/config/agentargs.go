package config

import (
	"errors"
	"fmt"
	"slices"
	"strconv"
	"strings"
)

const (
	// AgentArgDefault means the choice is defaults.agent_args.
	AgentArgDefault = "default"
	// AgentArgFlag means a start flag replaced the configured args.
	AgentArgFlag = "flag"
	// AgentArgRule means the first matching agent_rule replaced the defaults.
	AgentArgRule = "rule"
)

// AgentArgChoice is the agent argv for one item and where it came from.
type AgentArgChoice struct {
	Args   []string
	Source string   // AgentArgDefault, AgentArgFlag, or AgentArgRule.
	Rule   int      // 1-based rule index when Source is AgentArgRule.
	Labels []string // That rule's labels when Source is AgentArgRule.
	Name   string   // That rule's name when Source is AgentArgRule. Empty when unset.
}

// RuleName is what a claim records as the agent rule: the matched rule's
// name, else its first label, or "default" or "flag" when no rule chose
// the args.
func (c AgentArgChoice) RuleName() (string, error) {
	switch c.Source {
	case AgentArgDefault, AgentArgFlag:
		return c.Source, nil
	case AgentArgRule:
		if c.Name != "" {
			return c.Name, nil
		}
		if len(c.Labels) == 0 || c.Labels[0] == "" {
			return "", fmt.Errorf("agent args: expected rule %d to have a label, found none", c.Rule)
		}
		return c.Labels[0], nil
	default:
		return "", fmt.Errorf("agent args: expected default, flag, or rule, found %q", c.Source)
	}
}

// ModelArg returns the value of --model MODEL or --model=MODEL in args.
// No --model returns empty. --model with no value, an empty value, or two
// --model flags is an error.
func ModelArg(args []string) (string, error) {
	model := ""
	found := false
	for i := 0; i < len(args); i++ {
		value, ok := strings.CutPrefix(args[i], "--model=")
		if !ok {
			if args[i] != "--model" {
				continue
			}
			if i+1 >= len(args) {
				return "", errors.New("agent args: expected a value after --model, found the end of the args")
			}
			i++
			value = args[i]
		}
		if value == "" {
			return "", errors.New("agent args: expected a non-empty --model value, found empty")
		}
		if found {
			return "", fmt.Errorf("agent args: expected one --model, found %q and %q", model, value)
		}
		model, found = value, true
	}
	return model, nil
}

// ChooseAgentArgs picks the argv appended after -- on herdr agent start.
// A set flag replaces every rule and the defaults. Otherwise the first rule
// whose labels share any item label wins. A rule's args replace the defaults.
// An unmatched item uses defaults.
func ChooseAgentArgs(itemLabels, defaults []string, rules []AgentRule, flag []string, fromFlag bool) AgentArgChoice {
	if fromFlag {
		return AgentArgChoice{Args: copyStrings(flag), Source: AgentArgFlag}
	}
	for i, rule := range rules {
		if labelAny(rule.Labels, itemLabels) {
			return AgentArgChoice{
				Args:   copyStrings(rule.AgentArgs),
				Source: AgentArgRule,
				Rule:   i + 1,
				Labels: copyStrings(rule.Labels),
				Name:   rule.Name,
			}
		}
	}
	return AgentArgChoice{Args: copyStrings(defaults), Source: AgentArgDefault}
}

func labelAny(want, have []string) bool {
	for _, label := range want {
		if slices.Contains(have, label) {
			return true
		}
	}
	return false
}

// FormatAgentArgs is the plan and explain line for choice.
func FormatAgentArgs(choice AgentArgChoice) (string, error) {
	source, err := agentArgSource(choice)
	if err != nil {
		return "", err
	}
	shown, err := formatArgv(choice.Args)
	if err != nil {
		return "", err
	}
	if shown == "" {
		return fmt.Sprintf("%-8s (%s)\n", "args", source), nil
	}
	return fmt.Sprintf("%-8s %s (%s)\n", "args", shown, source), nil
}

func agentArgSource(choice AgentArgChoice) (string, error) {
	switch choice.Source {
	case AgentArgDefault, AgentArgFlag:
		return choice.Source, nil
	case AgentArgRule:
		if choice.Rule < 1 {
			return "", fmt.Errorf("agent args: expected a rule number, found %d", choice.Rule)
		}
		if len(choice.Labels) == 0 {
			return "", fmt.Errorf("agent args: expected rule labels, found none")
		}
		return fmt.Sprintf("rule %d labels %s", choice.Rule, strings.Join(choice.Labels, ", ")), nil
	default:
		return "", fmt.Errorf("agent args: expected default, flag, or rule, found %q", choice.Source)
	}
}

func formatArgv(args []string) (string, error) {
	if len(args) == 0 {
		return "", nil
	}
	parts := make([]string, len(args))
	for i, arg := range args {
		if arg == "" {
			return "", fmt.Errorf("agent args: expected a non-empty argument, found empty")
		}
		parts[i] = formatOneArg(arg)
	}
	return strings.Join(parts, " "), nil
}

func formatOneArg(arg string) string {
	if strings.ContainsAny(arg, " \t\n\"'\\") {
		return strconv.Quote(arg)
	}
	return arg
}
