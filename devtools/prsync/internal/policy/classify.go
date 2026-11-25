package policy

import (
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// ClassifyConflict classifies a PR as up-to-date, behind, clean-rebase, or
// conflicting based on its GitHub merge state and mergeability.
func ClassifyConflict(pr scan.PR) ConflictClass {
	if pr.Mergeable == "CONFLICTING" || pr.MergeStateStatus == "DIRTY" {
		return ClassConflicting
	}
	if pr.MergeStateStatus == "BEHIND" {
		if pr.Mergeable == "MERGEABLE" {
			return ClassCleanRebase
		}
		return ClassBehind
	}
	return ClassUpToDate
}

// MatchNitThreads returns unresolved review threads on pr that have no author
// reply, were authored by a configured bot, and match at least one nit/class
// pattern. Human reviewer threads are never returned.
func MatchNitThreads(pr scan.PR, bots []string, patterns []*regexp.Regexp) []scan.Comment {
	if len(bots) == 0 || len(patterns) == 0 {
		return nil
	}
	var out []scan.Comment
	for _, c := range pr.BlockingComments {
		if c.HasAuthorReply {
			continue
		}
		if !isConfiguredBot(c.Author, bots) {
			continue
		}
		if !matchesAnyPattern(c.Body, patterns) {
			continue
		}
		out = append(out, c)
	}
	return out
}

func isConfiguredBot(author string, bots []string) bool {
	if author == "" {
		return false
	}
	for _, b := range bots {
		if strings.EqualFold(author, b) {
			return true
		}
	}
	return false
}

func matchesAnyPattern(body string, patterns []*regexp.Regexp) bool {
	for _, re := range patterns {
		if re != nil && re.MatchString(body) {
			return true
		}
	}
	return false
}

func hasHoldLabel(labels, holdLabels []string) string {
	for _, label := range labels {
		for _, hold := range holdLabels {
			if strings.EqualFold(label, hold) {
				return label
			}
		}
	}
	return ""
}

func isStackedPR(pr scan.PR, defaultBranches []string) bool {
	if pr.Base == "" {
		return false
	}
	if pr.DefaultBranch != "" {
		return pr.Base != pr.DefaultBranch
	}
	if len(defaultBranches) == 0 {
		return false
	}
	for _, b := range defaultBranches {
		if pr.Base == b {
			return false
		}
	}
	return true
}
