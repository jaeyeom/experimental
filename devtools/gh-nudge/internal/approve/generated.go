package approve

import (
	"fmt"
	"strings"

	"github.com/bmatcuk/doublestar/v4"
	"gopkg.in/yaml.v3"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

type generatedFileEntry struct {
	Pattern string `yaml:"pattern"`
}

type generatedListEntry struct {
	patterns []string
	err      string
}

// parseGeneratedList parses a YAML list of generated-file globs.
// Each entry requires a non-empty valid glob in pattern; unknown keys are ignored.
func parseGeneratedList(data string) ([]string, error) {
	var entries []generatedFileEntry
	if err := yaml.Unmarshal([]byte(data), &entries); err != nil {
		return nil, fmt.Errorf("parse yaml: %w", err)
	}
	patterns := make([]string, 0, len(entries))
	for i, entry := range entries {
		if entry.Pattern == "" {
			return nil, fmt.Errorf("entry %d: missing pattern", i+1)
		}
		if !doublestar.ValidatePattern(entry.Pattern) {
			return nil, fmt.Errorf("entry %d: invalid glob %q", i+1, entry.Pattern)
		}
		patterns = append(patterns, entry.Pattern)
	}
	return patterns, nil
}

// generatedListFor returns the parsed generated-file globs for pr's repository
// at pr.BaseSHA, caching the result per repository and base SHA.
func (r *Runner) generatedListFor(pr github.ApprovalPullRequest) ([]string, string) {
	repo := pr.Ref.Owner + "/" + pr.Ref.Repo
	listPath, ok := config.LookupGeneratedList(r.GeneratedLists, repo)
	if !ok {
		return nil, fmt.Sprintf("generated list: not configured for %s", repo)
	}
	if pr.BaseSHA == "" {
		return nil, fmt.Sprintf("generated list %s: base SHA unknown", listPath)
	}
	key := strings.ToLower(repo) + "@" + pr.BaseSHA
	if r.generatedLists == nil {
		r.generatedLists = make(map[string]generatedListEntry)
	}
	if got, cached := r.generatedLists[key]; cached {
		return got.patterns, got.err
	}
	var got generatedListEntry
	text, err := r.Client.RepoFile(pr.Ref, pr.BaseSHA, listPath)
	if err != nil {
		got.err = fmt.Sprintf("generated list %s: %v", listPath, err)
	} else {
		patterns, parseErr := parseGeneratedList(text)
		if parseErr != nil {
			got.err = fmt.Sprintf("generated list %s: %v", listPath, parseErr)
		} else {
			got.patterns = patterns
		}
	}
	r.generatedLists[key] = got
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
			if hit, err := doublestar.Match(gp, path); err == nil && hit {
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
			if hit, err := doublestar.Match(gp, path); err == nil && hit {
				return gp, true, true
			}
		}
	}
	return "", false, false
}
