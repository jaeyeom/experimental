package approve

import (
	"fmt"
	"strings"

	"github.com/bmatcuk/doublestar/v4"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/pkg/genfiles"
)

type generatedListEntry struct {
	patterns []string
	err      string
}

// parseGeneratedList returns the patterns of a generated-files list. Only
// pattern is read; other keys are ignored so lists in any shape keep working.
func parseGeneratedList(data string) ([]string, error) {
	return genfiles.ParsePatterns([]byte(data)) //nolint:wrapcheck // caller formats generated list error
}

// generatedListFor returns the parsed generated-file globs for pr's repository
// at pr.BaseSHA, caching the result per repository and base SHA.
func (r *Runner) generatedListFor(pr github.ApprovalPullRequest, pool *loadPool) ([]string, string) {
	repo := pr.Ref.Owner + "/" + pr.Ref.Repo
	listPath, ok := config.LookupGeneratedList(r.GeneratedLists, repo)
	if !ok {
		return nil, fmt.Sprintf("generated list: not configured for %s", repo)
	}
	if pr.BaseSHA == "" {
		return nil, fmt.Sprintf("generated list %s: base SHA unknown", listPath)
	}
	key := strings.ToLower(repo) + "@" + pr.BaseSHA
	got, _ := r.generatedLists.getOrLoad(key, func() generatedListEntry {
		var res generatedListEntry
		var text string
		err := pool.callWithRetry(func() error {
			var callErr error
			text, callErr = r.Client.RepoFile(pr.Ref, pr.BaseSHA, listPath)
			return callErr //nolint:wrapcheck // caller formats generated list error
		})
		if err != nil {
			res.err = fmt.Sprintf("generated list %s: %v", listPath, err)
			return res
		}
		patterns, parseErr := parseGeneratedList(text)
		if parseErr != nil {
			res.err = fmt.Sprintf("generated list %s: %v", listPath, parseErr)
			return res
		}
		res.patterns = patterns
		return res
	})
	return got.patterns, got.err
}

// matchGlobsWithGenerated checks whether path matches any pattern in patterns,
// expanding @generated with pr.GeneratedPatterns. Inline patterns are checked
// before @generated.
func matchGlobsWithGenerated(patterns []string, pr github.ApprovalPullRequest, path string) (matched string, fromGen bool, ok bool) {
	for _, pattern := range patterns {
		if pattern == config.GeneratedListRef {
			continue
		}
		if hit, err := doublestar.Match(pattern, path); err == nil && hit {
			return pattern, false, true
		}
	}
	if config.UsesGeneratedList(patterns) {
		for _, gp := range pr.GeneratedPatterns {
			if genfiles.Match(gp, path) {
				return gp, true, true
			}
		}
	}
	return "", false, false
}

// formatGeneratedMatch formats a match through @generated for --explain output.
func formatGeneratedMatch(path, pattern string) string {
	return fmt.Sprintf("%s: %s %q", path, config.GeneratedListRef, pattern)
}

// generatedListError returns the failure reason when patterns uses @generated
// and pr's generated list failed to load or was not loaded.
func generatedListError(patterns []string, pr github.ApprovalPullRequest) string {
	if !config.UsesGeneratedList(patterns) {
		return ""
	}
	if pr.GeneratedErr != "" {
		return pr.GeneratedErr
	}
	if !pr.GeneratedLoaded {
		return "generated list not loaded"
	}
	return ""
}

// matchGlobsInList checks whether path matches any pattern in patterns,
// expanding @generated with generatedPatterns.
func matchGlobsInList(patterns, generatedPatterns []string, path string) (matched string, fromGen bool, ok bool) {
	for _, pattern := range patterns {
		if pattern == config.GeneratedListRef {
			continue
		}
		if hit, err := doublestar.Match(pattern, path); err == nil && hit {
			return pattern, false, true
		}
	}
	if config.UsesGeneratedList(patterns) {
		for _, gp := range generatedPatterns {
			if genfiles.Match(gp, path) {
				return gp, true, true
			}
		}
	}
	return "", false, false
}
