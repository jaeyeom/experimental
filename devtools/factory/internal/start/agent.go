package start

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const maxAgentNameLen = 32

// LeaseOwner is the agent Start would launch for it: the item id and the
// herdr agent name derived from its tab label. The gate uses it to accept
// a checkout lease that this agent already holds.
func LeaseOwner(it source.Item, cfg config.Source, kind string) (agentbus.Owner, error) {
	if err := validateTabRef(it, cfg, kind); err != nil {
		return agentbus.Owner{}, err
	}
	_, tab := ready.TabRef(it, cfg, kind)
	if tab == "" {
		return agentbus.Owner{}, fmt.Errorf("start %s: expected a tab label, found empty", it.ID)
	}
	name, err := agentName(tab, kind, cfg.Repo)
	if err != nil {
		return agentbus.Owner{}, err
	}
	return agentbus.Owner{ItemID: it.ID, Agent: name}, nil
}

// agentName derives the herdr agent name from a tab label.
// The label is lowercased, characters outside [a-z0-9_-] become '-', a
// leading non-letter is prefixed, and the result is cut to 32 characters.
// A GitHub #N label uses the repository name as that prefix when repo is set.
func agentName(tabLabel, kind, repo string) (string, error) {
	name := sanitizeAgent(tabLabel)
	if !startsWithLower(name) {
		prefix, err := agentPrefix(tabLabel, kind, repo)
		if err != nil {
			return "", err
		}
		name = prefix + name
	}
	return truncateChars(name, maxAgentNameLen), nil
}

func sanitizeAgent(label string) string {
	var b strings.Builder
	b.Grow(len(label))
	for _, r := range strings.ToLower(label) {
		if isAgentRune(r) {
			b.WriteRune(r)
			continue
		}
		b.WriteByte('-')
	}
	return b.String()
}

func isAgentRune(r rune) bool {
	return (r >= 'a' && r <= 'z') || (r >= '0' && r <= '9') || r == '-' || r == '_'
}

func startsWithLower(name string) bool {
	if name == "" {
		return false
	}
	r, _ := utf8.DecodeRuneInString(name)
	return r >= 'a' && r <= 'z'
}

func agentPrefix(tabLabel, kind, repo string) (string, error) {
	if kind == "github" && githubTabLabel(tabLabel) && repo != "" {
		return repoBasename(repo)
	}
	return "a", nil
}

func githubTabLabel(label string) bool {
	number, ok := strings.CutPrefix(label, "#")
	if !ok {
		return false
	}
	return canonicalNumber(number)
}

func canonicalNumber(s string) bool {
	if s == "" || s[0] < '1' || s[0] > '9' {
		return false
	}
	for i := 1; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

func repoBasename(repo string) (string, error) {
	owner, name, ok := strings.Cut(repo, "/")
	if !ok || owner == "" || name == "" || strings.Contains(name, "/") ||
		strings.ContainsAny(owner, " \t") || strings.ContainsAny(name, " \t") {
		return "", fmt.Errorf("start: expected owner/repo, found %q", repo)
	}
	return name, nil
}

func truncateChars(s string, n int) string {
	if utf8.RuneCountInString(s) <= n {
		return s
	}
	i := 0
	for count := 0; count < n; count++ {
		_, size := utf8.DecodeRuneInString(s[i:])
		i += size
	}
	return s[:i]
}

// checkAgentName rejects an empty name and a name herdr would reject.
func checkAgentName(id, name string) error {
	if name == "" {
		return fmt.Errorf("start %s: expected an agent name, found empty", id)
	}
	if acceptedAgentName(name) {
		return nil
	}
	return fmt.Errorf("start %s: expected an agent name of 1-32 characters starting with a lowercase letter and containing only [a-z0-9_-], found %q", id, name)
}

func acceptedAgentName(name string) bool {
	n := utf8.RuneCountInString(name)
	if n < 1 || n > maxAgentNameLen {
		return false
	}
	for i, r := range name {
		if i == 0 {
			if r < 'a' || r > 'z' {
				return false
			}
			continue
		}
		if !isAgentRune(r) {
			return false
		}
	}
	return true
}
