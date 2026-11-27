package installcheck

import (
	"context"
	"encoding/json"
	"fmt"
	"sort"
	"strconv"
	"strings"
	"time"
)

const ghFields = "number,title,state,stateReason,closedAt"

type ghIssue struct {
	Number      int       `json:"number"`
	Title       string    `json:"title"`
	State       string    `json:"state"`
	StateReason string    `json:"stateReason"`
	ClosedAt    time.Time `json:"closedAt"`
}

// fetchIssues returns the watched issues of repo in the given state
// ("open" or "closed"), merging the explicit list and the query. An issue
// found by both appears once.
func fetchIssues(ctx context.Context, run Runner, ghBin string, repo RepoConfig, slug, state string) ([]Issue, error) {
	if ghBin == "" {
		ghBin = DefaultGHBin
	}
	found := map[int]ghIssue{}
	seenExplicit := map[int]bool{}
	for _, n := range repo.Issues {
		if seenExplicit[n] {
			continue
		}
		seenExplicit[n] = true
		out, err := run(ctx, ghBin, "issue", "view", strconv.Itoa(n), "-R", slug, "--json", ghFields)
		if err != nil {
			return nil, err
		}
		var is ghIssue
		if err := json.Unmarshal(out, &is); err != nil {
			return nil, fmt.Errorf("parse gh issue %d: %w", n, err)
		}
		if strings.EqualFold(is.State, state) {
			found[is.Number] = is
		}
	}
	if q := strings.TrimSpace(repo.Query); q != "" {
		out, err := run(ctx, ghBin, "issue", "list", "-R", slug, "--state", state,
			"--search", q, "--limit", "200", "--json", ghFields)
		if err != nil {
			return nil, err
		}
		var list []ghIssue
		if err := json.Unmarshal(out, &list); err != nil {
			return nil, fmt.Errorf("parse gh issue list: %w", err)
		}
		for _, is := range list {
			found[is.Number] = is
		}
	}
	issues := make([]Issue, 0, len(found))
	for _, is := range found {
		issues = append(issues, Issue{
			Repo: slug, Number: is.Number, Title: is.Title, State: is.State,
			StateReason: is.StateReason, ClosedAt: is.ClosedAt,
		})
	}
	sort.Slice(issues, func(i, j int) bool { return issues[i].Number < issues[j].Number })
	return issues, nil
}
