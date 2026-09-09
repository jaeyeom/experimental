package scan

// summarize derives a rollup from prs. Tab on each PR is the source of truth.
func summarize(prs []PR) Summary {
	out := Summary{Total: len(prs)}
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
	}
	return out
}
