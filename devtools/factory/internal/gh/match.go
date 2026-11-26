package gh

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// TitleMatches reports whether title matches pattern.
// Each {id} in pattern is replaced with regexp.QuoteMeta(id) before compiling.
func TitleMatches(pattern, id, title string) (bool, error) {
	rewritten := strings.ReplaceAll(pattern, "{id}", regexp.QuoteMeta(id))
	re, err := regexp.Compile(rewritten)
	if err != nil {
		return false, fmt.Errorf("title pattern: expected a valid regexp after replacing {id} in %q, found %q: %w", pattern, rewritten, err)
	}
	return re.MatchString(title), nil
}

// ForItem returns the pull requests that refer to it.
// An id containing "#" is a GitHub issue and matches ClosingIssues.
// Any other id is matched against each pull request title with titlePattern.
// An invalid titlePattern panics, because ForItem has no error result.
func ForItem(prs []PR, it source.Item, titlePattern string) []PR {
	out := []PR{}
	if strings.Contains(it.ID, "#") {
		return matchClosingIssues(prs, it.ID)
	}
	for _, pr := range prs {
		ok, err := TitleMatches(titlePattern, it.ID, pr.Title)
		if err != nil {
			panic(err)
		}
		if ok {
			out = append(out, pr)
		}
	}
	return out
}

// HeadContains returns pull requests whose head branch contains id.
// An empty id matches nothing. The match is a literal substring.
func HeadContains(prs []PR, id string) []PR {
	out := []PR{}
	if id == "" {
		return out
	}
	for _, pr := range prs {
		if strings.Contains(pr.Head, id) {
			out = append(out, pr)
		}
	}
	return out
}

// Matches returns the pull requests that are the same work as one claim.
// The first rule that matches any pull request wins:
// a recorded URL (or the same owner/repo and number), then a recorded head
// branch equal to headRefName, then ClosingIssues when id contains "#",
// then the item id in the title via pattern (when non-empty) or as a literal
// substring of the head branch name.
// An empty id skips the ClosingIssues, title, and branch-name rules.
func Matches(prs []PR, id string, recorded []store.ClaimPR, pattern string) ([]PR, error) {
	if got := matchURL(prs, recorded); len(got) > 0 {
		return got, nil
	}
	if got := matchRecordedHead(prs, recorded); len(got) > 0 {
		return got, nil
	}
	if id == "" {
		return nil, nil
	}
	if strings.Contains(id, "#") {
		if got := matchClosingIssues(prs, id); len(got) > 0 {
			return got, nil
		}
	}
	return matchTitleOrHead(prs, id, pattern)
}

func matchClosingIssues(prs []PR, id string) []PR {
	out := []PR{}
	for _, pr := range prs {
		if containsIssue(pr.ClosingIssues, id) {
			out = append(out, pr)
		}
	}
	return out
}

func matchURL(prs []PR, recorded []store.ClaimPR) []PR {
	var out []PR
	for _, pr := range prs {
		if urlRecorded(pr, recorded) {
			out = append(out, pr)
		}
	}
	return out
}

func urlRecorded(pr PR, recorded []store.ClaimPR) bool {
	for _, rec := range recorded {
		if rec.URL != "" && rec.URL == pr.URL {
			return true
		}
		if rec.Repo != "" && rec.Number > 0 && rec.Repo == pr.Repo && rec.Number == pr.Number {
			return true
		}
	}
	return false
}

func matchRecordedHead(prs []PR, recorded []store.ClaimPR) []PR {
	heads := make([]string, 0, len(recorded))
	for _, rec := range recorded {
		if rec.Head != "" {
			heads = append(heads, rec.Head)
		}
	}
	if len(heads) == 0 {
		return nil
	}
	var out []PR
	for _, pr := range prs {
		if pr.Head == "" {
			continue
		}
		for _, head := range heads {
			if pr.Head == head {
				out = append(out, pr)
				break
			}
		}
	}
	return out
}

func matchTitleOrHead(prs []PR, id, pattern string) ([]PR, error) {
	var out []PR
	for _, pr := range prs {
		var ok bool
		if pattern != "" {
			var err error
			ok, err = TitleMatches(pattern, id, pr.Title)
			if err != nil {
				return nil, err
			}
		}
		if ok || strings.Contains(pr.Head, id) {
			out = append(out, pr)
		}
	}
	return out, nil
}

func containsIssue(issues []string, id string) bool {
	for _, issue := range issues {
		if issue == id {
			return true
		}
	}
	return false
}
