package installcheck

import (
	"context"
	"fmt"
	"strings"
)

// Status describes the saved state and the watched issues still open. It
// returns the text and the errors hit while asking gh; an unreachable repo
// does not hide the others.
func Status(ctx context.Context, cfg Config, st State, run Runner) (string, []error) {
	var b strings.Builder
	if st.LastRun.IsZero() {
		b.WriteString("last run: never\n")
	} else {
		fmt.Fprintf(&b, "last run: %s\n", st.LastRun.UTC().Format("2006-01-02 15:04:05Z"))
	}
	var errs []error
	for _, repo := range cfg.Repos {
		slug, err := repo.Slug()
		if err != nil {
			errs = append(errs, err)
			continue
		}
		head := st.Repos[slug].Head
		if head == "" {
			head = "(not seen yet)"
		}
		fmt.Fprintf(&b, "%s main: %s\n", slug, shortSHA(head))
		open, err := fetchIssues(ctx, run, repo, slug, "open")
		if err != nil {
			errs = append(errs, fmt.Errorf("%s: %w", slug, err))
			continue
		}
		for _, is := range open {
			fmt.Fprintf(&b, "  open: #%d %s\n", is.Number, is.Title)
		}
		if len(open) == 0 {
			b.WriteString("  no watched open issues\n")
		}
	}
	return b.String(), errs
}
