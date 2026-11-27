package installcheck

import (
	"context"
	"encoding/json"
	"fmt"
	"slices"
	"strings"
	"time"
)

// RepoStatus is one repository's recorded head and current open watched issues.
type RepoStatus struct {
	Slug       string  `json:"slug"`
	URL        string  `json:"url"`
	Head       string  `json:"head,omitempty"`
	Reported   []int   `json:"reported,omitempty"`
	OpenIssues []Issue `json:"openIssues,omitempty"`
	Error      string  `json:"error,omitempty"`
}

// StatusReport is the structured output for `install-check status`.
type StatusReport struct {
	LastRun        time.Time    `json:"lastRun,omitempty"`
	FailureNotedAt time.Time    `json:"failureNotedAt,omitempty"`
	NotifyMode     string       `json:"notifyMode"`
	NotifyTarget   string       `json:"notifyTarget"`
	GHBin          string       `json:"ghBin"`
	Repos          []RepoStatus `json:"repos"`
}

// InspectStatus reads saved state and queries gh for currently open watched issues.
// An unreachable repository does not prevent checking the remaining repositories.
func InspectStatus(ctx context.Context, cfg Config, st State, run Runner, ghBin string) (StatusReport, []error) {
	if ghBin == "" {
		ghBin = DefaultGHBin
	}
	rep := StatusReport{
		LastRun:        st.LastRun,
		FailureNotedAt: st.FailureNotedAt,
		NotifyMode:     cfg.EffectiveNotifyMode(),
		NotifyTarget:   cfg.NotifyTargetLabel(),
		GHBin:          ghBin,
		Repos:          make([]RepoStatus, 0, len(cfg.Repos)),
	}
	var errs []error
	for _, repo := range cfg.Repos {
		slug, err := repo.Slug()
		if err != nil {
			errs = append(errs, err)
			rep.Repos = append(rep.Repos, RepoStatus{URL: repo.URL, Error: err.Error()})
			continue
		}
		rs := st.Repos[slug]
		row := RepoStatus{
			Slug:     slug,
			URL:      repo.URL,
			Head:     rs.Head,
			Reported: slices.Clone(rs.Reported),
		}
		open, err := fetchIssues(ctx, run, ghBin, repo, slug, "open")
		if err != nil {
			wrapped := fmt.Errorf("%s: %w", slug, err)
			errs = append(errs, wrapped)
			row.Error = wrapped.Error()
			rep.Repos = append(rep.Repos, row)
			continue
		}
		row.OpenIssues = open
		rep.Repos = append(rep.Repos, row)
	}
	return rep, errs
}

// FormatStatus renders rep as human-readable text or formatted JSON.
func FormatStatus(rep StatusReport, asJSON bool) (string, error) {
	if asJSON {
		data, err := json.MarshalIndent(rep, "", "  ")
		if err != nil {
			return "", fmt.Errorf("encode status: %w", err)
		}
		return string(data) + "\n", nil
	}
	var b strings.Builder
	if rep.LastRun.IsZero() {
		b.WriteString("last run: never\n")
	} else {
		fmt.Fprintf(&b, "last run: %s\n", rep.LastRun.UTC().Format("2006-01-02 15:04:05Z"))
	}
	for _, r := range rep.Repos {
		name := r.Slug
		if name == "" {
			name = r.URL
		}
		head := r.Head
		if head == "" {
			head = "(not seen yet)"
		}
		fmt.Fprintf(&b, "%s main: %s\n", name, shortSHA(head))
		if r.Error != "" {
			continue
		}
		for _, is := range r.OpenIssues {
			fmt.Fprintf(&b, "  open: #%d %s\n", is.Number, is.Title)
		}
		if len(r.OpenIssues) == 0 {
			b.WriteString("  no watched open issues\n")
		}
	}
	return b.String(), nil
}

// Status describes the saved state and the watched issues still open. It
// returns the text and the errors hit while asking gh; an unreachable repo
// does not hide the others.
func Status(ctx context.Context, cfg Config, st State, run Runner) (string, []error) {
	rep, errs := InspectStatus(ctx, cfg, st, run, cfg.GHBin)
	text, err := FormatStatus(rep, false)
	if err != nil {
		errs = append(errs, err)
	}
	return text, errs
}
