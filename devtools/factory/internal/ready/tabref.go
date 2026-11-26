package ready

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// TabRef reports where the item's tab lives or should be created.
// An unknown kind panics. So does a GitHub id that is not owner/repo#N, and a
// GitHub repo that is not owner/repo when Workspace is empty. The signature
// returns no error, and a bad shape must not fall back to a quiet default.
func TabRef(it source.Item, cfg config.Source, kind string) (workspaceLabel, tabLabel string) {
	switch kind {
	case "jira":
		return jiraTabRef(it, cfg)
	case "github":
		return githubTabRef(it, cfg)
	default:
		panic(fmt.Sprintf("tab ref: expected kind jira or github, found %q", kind))
	}
}

func jiraTabRef(it source.Item, cfg config.Source) (string, string) {
	for _, rule := range cfg.WorkspaceRules {
		if ruleMatches(rule, it) {
			return rule.Workspace, it.ID
		}
	}
	if cfg.Workspace != "" {
		return cfg.Workspace, it.ID
	}
	return "factory", it.ID
}

// ruleMatches reports whether rule selects it.
// Parent and Label count only when they are set, so a label rule does not
// match every item whose parent is empty.
func ruleMatches(rule config.WorkspaceRule, it source.Item) bool {
	if rule.Parent != "" && rule.Parent == it.Parent {
		return true
	}
	return rule.Label != "" && slices.Contains(it.Labels, rule.Label)
}

func githubTabRef(it source.Item, cfg config.Source) (string, string) {
	_, num, err := source.ParseGitHubID(it.ID)
	if err != nil {
		panic(fmt.Sprintf("tab ref: %s", err))
	}
	tabLabel := "#" + strconv.Itoa(num)
	if cfg.Workspace != "" {
		return cfg.Workspace, tabLabel
	}
	return repoName(cfg.Repo), tabLabel
}

func repoName(repo string) string {
	owner, name, ok := strings.Cut(repo, "/")
	if !ok || owner == "" || name == "" || strings.Contains(name, "/") ||
		strings.ContainsAny(owner, " \t") || strings.ContainsAny(name, " \t") {
		panic(fmt.Sprintf("tab ref: expected owner/repo, found %q", repo))
	}
	return name
}
