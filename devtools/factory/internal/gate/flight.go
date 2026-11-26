package gate

import (
	"errors"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Flight is the in-flight count after a claim and its pull request count once.
type Flight struct {
	// InFlight is active claims plus open pull requests, minus Overlap.
	InFlight int
	// Claims is the number of active claims.
	Claims int
	// OpenPRs is the number of open pull requests.
	OpenPRs int
	// Overlap is how many open pull requests are the same work as a claim.
	Overlap int
}

// countInFlight unions active claims with open pull requests.
// A claim matches a pull request by recorded URL, then recorded head branch,
// then the item id in the title or the head branch name.
// pattern is called only when the title rule is needed.
func countInFlight(claims []store.Claim, prs []gh.PR, pattern func(string) (string, error)) (Flight, error) {
	used := make([]bool, len(prs))
	overlap := 0
	for _, claim := range claims {
		idxs, err := matchClaim(claim, prs, pattern)
		if err != nil {
			return Flight{}, err
		}
		for _, idx := range idxs {
			if used[idx] {
				continue
			}
			used[idx] = true
			overlap++
		}
	}
	return Flight{
		InFlight: len(claims) + len(prs) - overlap,
		Claims:   len(claims),
		OpenPRs:  len(prs),
		Overlap:  overlap,
	}, nil
}

func matchClaim(claim store.Claim, prs []gh.PR, pattern func(string) (string, error)) ([]int, error) {
	if len(prs) == 0 {
		return nil, nil
	}
	pat := ""
	if needsTitlePattern(claim, prs) {
		if pattern == nil {
			return nil, errors.New("in flight: expected a pr_title_pattern function, found nil")
		}
		got, err := pattern(claim.Source)
		if err != nil {
			if !errors.Is(err, config.ErrSourceNotFound) {
				return nil, err
			}
			pat = fallbackPRTitlePattern(claim)
		} else {
			pat = got
		}
	}
	found, err := gh.Matches(prs, claim.ID, claim.PRs, pat)
	if err != nil {
		return nil, err //nolint:wrapcheck // internal factory error already carries context
	}
	return indexesOf(prs, found), nil
}

func fallbackPRTitlePattern(claim store.Claim) string {
	if strings.HasPrefix(claim.Source, "github:") || strings.Contains(claim.ID, "#") {
		return ""
	}
	return config.DefaultPRTitlePattern
}

func needsTitlePattern(claim store.Claim, prs []gh.PR) bool {
	if claim.ID == "" {
		return false
	}
	if len(ghMatch(prs, claim, "")) > 0 {
		return false
	}
	return true
}

func ghMatch(prs []gh.PR, claim store.Claim, pattern string) []gh.PR {
	found, err := gh.Matches(prs, "", claim.PRs, pattern)
	if err != nil || len(found) == 0 {
		return nil
	}
	return found
}

func indexesOf(all, found []gh.PR) []int {
	used := make([]bool, len(all))
	var out []int
	for _, pr := range found {
		for i := range all {
			if used[i] || !samePull(all[i], pr) {
				continue
			}
			used[i] = true
			out = append(out, i)
			break
		}
	}
	return out
}

func samePull(a, b gh.PR) bool {
	if a.Repo != "" && a.Number > 0 && a.Repo == b.Repo && a.Number == b.Number {
		return true
	}
	return a.URL != "" && a.URL == b.URL
}

func inFlightLine(f Flight, limit int) string {
	if f.InFlight < limit {
		return ""
	}
	return fmt.Sprintf("in flight: %d (claims %d, PRs %d, overlap %d) of max %d",
		f.InFlight, f.Claims, f.OpenPRs, f.Overlap, limit)
}
