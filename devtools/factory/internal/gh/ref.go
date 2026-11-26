package gh

import (
	"fmt"
	"net/url"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// ParseRef parses owner/repo#N or a GitHub pull request URL.
// The recorded URL is https://github.com/<repo>/pull/N.
func ParseRef(ref string) (store.ClaimPR, error) {
	if ref == "" {
		return store.ClaimPR{}, fmt.Errorf("pull request: expected owner/repo#N or a pull request URL, found empty")
	}
	if repo, n, err := source.ParseGitHubID(ref); err == nil {
		return store.ClaimPR{Repo: repo, Number: n, URL: PullURL(repo, n)}, nil
	}
	pr, ok := parsePullURL(ref)
	if !ok {
		return store.ClaimPR{}, fmt.Errorf("pull request: expected owner/repo#N or a pull request URL, found %q", ref)
	}
	return pr, nil
}

// PullURL is the canonical GitHub URL for a pull request.
func PullURL(repo string, n int) string {
	return fmt.Sprintf("https://github.com/%s/pull/%d", repo, n)
}

func parsePullURL(raw string) (store.ClaimPR, bool) {
	u, err := url.Parse(raw)
	if err != nil || !githubPRHost(u) {
		return store.ClaimPR{}, false
	}
	parts := strings.Split(strings.Trim(u.Path, "/"), "/")
	if len(parts) != 4 || parts[2] != "pull" {
		return store.ClaimPR{}, false
	}
	repo := parts[0] + "/" + parts[1]
	_, n, err := source.ParseGitHubID(repo + "#" + parts[3])
	if err != nil {
		return store.ClaimPR{}, false
	}
	return store.ClaimPR{Repo: repo, Number: n, URL: PullURL(repo, n)}, true
}

func githubPRHost(u *url.URL) bool {
	if u == nil || u.User != nil || u.RawQuery != "" || u.Fragment != "" {
		return false
	}
	if u.Scheme != "https" && u.Scheme != "http" {
		return false
	}
	host := strings.ToLower(u.Host)
	return host == "github.com" || host == "www.github.com"
}
