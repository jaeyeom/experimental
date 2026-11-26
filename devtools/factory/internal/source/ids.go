package source

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// jiraKeyPattern is the Jira issue key shape: a project key of at least two
// characters, a hyphen, and a positive number with no leading zero.
var jiraKeyPattern = regexp.MustCompile(`^[A-Z][A-Z0-9_]+-[1-9][0-9]*$`)

// ParseGitHubID splits an id of the form owner/repo#12.
// The issue number is a positive decimal integer with no leading zero.
func ParseGitHubID(id string) (repo string, num int, err error) {
	repoPart, numStr, ok := strings.Cut(id, "#")
	if !ok || !validGitHubRepo(repoPart) || !canonicalIssueNumber(numStr) {
		return "", 0, badGitHubID(id)
	}
	n, convErr := strconv.Atoi(numStr)
	if convErr != nil {
		return "", 0, badGitHubID(id)
	}
	return repoPart, n, nil
}

// GitHubID formats a repository and issue number as owner/repo#n.
func GitHubID(repo string, n int) string {
	return repo + "#" + strconv.Itoa(n)
}

// IsJiraKey reports whether s matches a Jira issue key such as PRJ-123.
func IsJiraKey(s string) bool {
	return jiraKeyPattern.MatchString(s)
}

func badGitHubID(id string) error {
	return fmt.Errorf("github id: expected owner/repo#<number> with number >= 1 and no leading zero, found %q", id)
}

func validGitHubRepo(repo string) bool {
	owner, name, ok := strings.Cut(repo, "/")
	if !ok || owner == "" || name == "" || strings.Contains(name, "/") {
		return false
	}
	return !strings.ContainsAny(owner, " \t") && !strings.ContainsAny(name, " \t")
}

func canonicalIssueNumber(s string) bool {
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
