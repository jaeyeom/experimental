package scan

import (
	"fmt"
	"os/exec"
	"regexp"
	"strings"
)

// githubRemotePatterns parse a github.com origin URL into owner/repo.
var githubRemotePatterns = []*regexp.Regexp{
	regexp.MustCompile(`^(?:https?|ssh)://(?:[^@/]+@)?github\.com/([^/]+)/([^/]+?)(?:\.git)?/?$`),
	regexp.MustCompile(`^git@github\.com:([^/]+)/([^/]+?)(?:\.git)?$`),
}

func parseGitHubRemote(raw string) (string, bool) {
	raw = strings.TrimSpace(raw)
	for _, re := range githubRemotePatterns {
		match := re.FindStringSubmatch(raw)
		if match == nil {
			continue
		}
		return match[1] + "/" + match[2], true
	}
	return "", false
}

// GitHubRepoFromCWD returns the github.com owner/repo of cwd's origin remote.
// A non-GitHub origin, a missing origin, or a directory that is not a git
// checkout is an error.
func GitHubRepoFromCWD(cwd string) (string, error) {
	if strings.TrimSpace(cwd) == "" {
		return "", fmt.Errorf("empty cwd")
	}
	out, err := exec.Command("git", "-C", cwd, "remote", "get-url", "origin").Output()
	if err != nil {
		return "", fmt.Errorf("git remote: %w", err)
	}
	repo, ok := parseGitHubRemote(string(out))
	if !ok {
		return "", fmt.Errorf("origin is not a github.com repository: %s", strings.TrimSpace(string(out)))
	}
	return repo, nil
}
