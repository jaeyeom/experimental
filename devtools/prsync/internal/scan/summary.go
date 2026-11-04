package scan

// summarize derives a rollup from prs. Tab on each PR is the source of truth.
func summarize(prs []PR) Summary {
	out := Summary{Total: len(prs)}
	uncovered := 0
	counting := false
	for _, pr := range prs {
		switch {
		case pr.Tab != nil:
			out.TabHere++
		case pr.Identifier != nil:
			out.OffMachine++
		}
		if pr.Identifier == nil {
			out.NoIdentifier++
		}
		if pr.IsDraft {
			out.Drafts++
		}
		if pr.TeamReviewSuggestions != nil {
			counting = true
			uncovered += uncoveredTeams(pr.TeamReviewSuggestions)
		}
	}
	if counting {
		out.UncoveredTeams = &uncovered
	}
	return out
}

func uncoveredTeams(suggestions []TeamReviewSuggestion) int {
	n := 0
	for _, suggestion := range suggestions {
		if suggestion.CoveredBy == nil {
			n++
		}
	}
	return n
}
