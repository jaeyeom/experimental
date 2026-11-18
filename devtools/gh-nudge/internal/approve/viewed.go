package approve

import (
	"fmt"
	"slices"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// markViewed marks UNVIEWED files a viewed rule or passing file_checks
// selects. A dry run records the paths and writes nothing. The per-run cap
// stops the run from marking more.
func (r *Runner) markViewed(entry *LogEntry, pr github.ApprovalPullRequest, viewer string, apply bool) {
	if entry.Action == ActionSkipped || entry.Action == ActionError {
		return
	}
	fcPaths, fcPatterns, fcExclusions := r.fileCheckViewedPaths(entry, pr)
	entry.Exclusions = append(entry.Exclusions, fcExclusions...)

	var viewedPaths []string
	if r.Viewed.Enabled && entry.Action == ActionManual && entry.Rule == nil && !pr.Draft {
		paths, exclusions, err := r.viewedPaths(pr, viewer)
		if err != nil {
			entry.Action, entry.Reason = ActionError, err.Error()
			return
		}
		entry.Exclusions = append(entry.Exclusions, exclusions...)
		viewedPaths = paths
	}

	paths := mergeViewedPaths(pr.Files, fcPaths, viewedPaths)
	if len(paths) == 0 || r.marksLeft <= 0 {
		return
	}
	capped := false
	if len(paths) > r.marksLeft {
		paths = paths[:r.marksLeft]
		capped = true
	}
	var (
		marked []string
		err    error
	)
	if apply {
		if entry.Action == ActionManual && !r.pinHead(entry, pr) {
			return
		}
		marked, err = r.markPaths(pr, paths)
		r.marksLeft -= len(marked)
	} else {
		marked = paths
		r.marksLeft -= len(paths)
	}
	r.recordMarkedViewed(entry, marked, fcPatterns, apply, capped, err)
}

func (r *Runner) recordMarkedViewed(entry *LogEntry, marked []string, fcPatterns map[string]string, apply, capped bool, markErr error) {
	patterns := filterPatterns(fcPatterns, marked)
	if markErr != nil {
		if entry.Action == ActionManual {
			entry.Paths = marked
			entry.FileCheckPatterns = patterns
			entry.Action, entry.Reason = ActionError, markErr.Error()
		} else {
			entry.Reason = markErr.Error()
		}
		return
	}
	markAction := ActionWouldMarkViewed
	if apply {
		markAction = ActionMarkViewed
	}
	if entry.Action == ActionManual {
		entry.Action = markAction
		entry.Paths = marked
		entry.FileCheckPatterns = patterns
		if capped {
			entry.Reason = "mark cap reached"
		}
		return
	}
	entry.Paths = marked
	entry.FileCheckPatterns = patterns
	extra := LogEntry{
		Time:              entry.Time,
		PR:                entry.PR,
		URL:               entry.URL,
		Title:             entry.Title,
		Author:            entry.Author,
		HeadSHA:           entry.HeadSHA,
		Rule:              entry.Rule,
		Action:            markAction,
		Paths:             marked,
		FileCheckPatterns: patterns,
	}
	if capped {
		extra.Reason = "mark cap reached"
	}
	entry.ExtraLogs = append(entry.ExtraLogs, extra)
}

func (r *Runner) fileCheckViewedPaths(entry *LogEntry, pr github.ApprovalPullRequest) ([]string, map[string]string, []string) {
	states := make(map[string]string, len(pr.Files))
	for _, f := range pr.Files {
		states[f.Path] = f.ViewerViewedState
	}
	patterns := make(map[string]string)
	var exclusions []string
	excluded := make(map[string]bool)
	for _, res := range entry.Rules {
		if !r.rule(res.Rule).MarkViewedOnPass {
			continue
		}
		for _, pass := range res.FileCheckPasses {
			if st := states[pass.Path]; st == models.ViewerViewed || st == models.ViewerDismissed {
				continue
			}
			if glob := firstGlob(r.Viewed.NeverMark, pass.Path); glob != "" {
				if !excluded[pass.Path] {
					exclusions = append(exclusions, pass.Path+": excluded by viewed.never_mark "+glob)
					excluded[pass.Path] = true
				}
				continue
			}
			if _, exists := patterns[pass.Path]; !exists {
				patterns[pass.Path] = pass.Pattern
			}
		}
	}
	if len(patterns) == 0 {
		return nil, nil, exclusions
	}
	var paths []string
	for _, f := range pr.Files {
		if _, ok := patterns[f.Path]; ok {
			paths = append(paths, f.Path)
		}
	}
	return paths, patterns, exclusions
}

func mergeViewedPaths(files []models.File, a, b []string) []string {
	if len(a) == 0 {
		return b
	}
	if len(b) == 0 {
		return a
	}
	want := make(map[string]bool, len(a)+len(b))
	for _, p := range a {
		want[p] = true
	}
	for _, p := range b {
		want[p] = true
	}
	out := make([]string, 0, len(want))
	for _, f := range files {
		if want[f.Path] {
			out = append(out, f.Path)
		}
	}
	return out
}

func filterPatterns(patterns map[string]string, paths []string) map[string]string {
	if len(patterns) == 0 || len(paths) == 0 {
		return nil
	}
	out := make(map[string]string)
	for _, p := range paths {
		if pat, ok := patterns[p]; ok {
			out[p] = pat
		}
	}
	if len(out) == 0 {
		return nil
	}
	return out
}

// pinHead re-reads the head and skips the pull request when it moved.
// It reports whether marking may continue.
func (r *Runner) pinHead(entry *LogEntry, pr github.ApprovalPullRequest) bool {
	head, err := r.Client.PullRequestHeadSHA(pr.Ref)
	if err != nil {
		entry.Action, entry.Reason = ActionError, err.Error()
		return false
	}
	if head != pr.HeadSHA {
		entry.Action = ActionSkipped
		entry.Reason = fmt.Sprintf("head moved from %s to %s since evaluation", pr.HeadSHA, head)
		return false
	}
	return true
}

// markPaths marks each path. It returns the paths that were marked before
// the first error.
func (r *Runner) markPaths(pr github.ApprovalPullRequest, paths []string) ([]string, error) {
	var marked []string
	for _, path := range paths {
		if err := r.Client.MarkFileAsViewed(pr.Ref, pr.NodeID, path); err != nil {
			return marked, fmt.Errorf("mark %s viewed: %w", path, err)
		}
		marked = append(marked, path)
	}
	return marked, nil
}

// viewedPaths returns the UNVIEWED files a rule selects, in pull request order.
// Unreadable CODEOWNERS or team data fails the pull request closed.
func (r *Runner) viewedPaths(pr github.ApprovalPullRequest, viewer string) ([]string, []string, error) {
	rules := viewedRulesFor(r.Viewed.Rules, pr)
	if len(rules) == 0 {
		return nil, nil, nil
	}
	mine, err := r.viewedOwnership(pr, viewer, rules)
	if err != nil {
		return nil, nil, err
	}
	var paths, exclusions []string
	for _, file := range pr.Files {
		if file.ViewerViewedState != models.ViewerUnviewed {
			continue
		}
		mark, reason := viewedDecision(r.Viewed.NeverMark, rules, file.Path, mine)
		if mark {
			paths = append(paths, file.Path)
			continue
		}
		if reason != "" {
			exclusions = append(exclusions, file.Path+": "+reason)
		}
	}
	return paths, exclusions, nil
}

// viewedOwnership loads CODEOWNERS when a rule needs it. mine is nil when
// no rule needs it. A read error marks nothing.
func (r *Runner) viewedOwnership(pr github.ApprovalPullRequest, viewer string, rules []config.ViewedRule) (map[string]bool, error) {
	if !slices.ContainsFunc(rules, func(rule config.ViewedRule) bool { return rule.CodeownersNotMine }) {
		return nil, nil
	}
	if pr.FilesErr != "" {
		return nil, fmt.Errorf("read CODEOWNERS: %s", pr.FilesErr)
	}
	own := pr.Ownership
	if own == nil {
		own = r.ownershipFor(pr, viewer)
	}
	if own.Err != "" {
		return nil, fmt.Errorf("read CODEOWNERS: %s", own.Err)
	}
	if err := r.teamReadErr(); err != nil {
		return nil, err
	}
	mine := make(map[string]bool, len(own.Files))
	for _, file := range own.Files {
		mine[file.Path] = file.Mine
	}
	for _, file := range pr.Files {
		if _, ok := mine[file.Path]; !ok {
			return nil, fmt.Errorf("read CODEOWNERS: no owners for %s", file.Path)
		}
	}
	return mine, nil
}

func (r *Runner) teamReadErr() error {
	errs := map[string]error{}
	for key, got := range r.teams {
		if got.err != nil {
			errs[key] = got.err
		}
	}
	for key, got := range r.teamCounts {
		if got.err != nil && errs[key] == nil {
			errs[key] = got.err
		}
	}
	if len(errs) == 0 {
		return nil
	}
	keys := make([]string, 0, len(errs))
	for key := range errs {
		keys = append(keys, key)
	}
	slices.Sort(keys)
	return fmt.Errorf("read members of %s: %w", keys[0], errs[keys[0]])
}

func viewedRulesFor(rules []config.ViewedRule, pr github.ApprovalPullRequest) []config.ViewedRule {
	repo := pr.Ref.Owner + "/" + pr.Ref.Repo
	var out []config.ViewedRule
	for _, rule := range rules {
		if len(rule.Repos) == 0 || matchesAny(rule.Repos, repo) {
			out = append(out, rule)
		}
	}
	return out
}

// viewedDecision reports whether path is marked, or why an exclusion kept
// it visible. A reason is empty when no selector matched. never_mark wins
// over a rule's paths_none_match. mine is nil when no rule uses codeowners.
func viewedDecision(never []string, rules []config.ViewedRule, path string, mine map[string]bool) (bool, string) {
	if glob := firstGlob(never, path); glob != "" && viewedSelectorAny(rules, path, mine) {
		return false, "excluded by viewed.never_mark " + glob
	}
	var blocked string
	for _, rule := range rules {
		if !viewedSelector(rule, path, mine) {
			continue
		}
		if glob := firstGlob(rule.PathsNoneMatch, path); glob != "" {
			if blocked == "" {
				blocked = fmt.Sprintf("excluded by rule %s paths_none_match %s", rule.Name, glob)
			}
			continue
		}
		return true, ""
	}
	return false, blocked
}

// viewedSelector reports whether a rule's selectors match path, before
// paths_none_match. mine is nil when no rule uses codeowners.
func viewedSelector(rule config.ViewedRule, path string, mine map[string]bool) bool {
	if len(rule.PathsMatch) > 0 && matchesAny(rule.PathsMatch, path) {
		return true
	}
	return rule.CodeownersNotMine && mine != nil && !mine[path]
}

func viewedSelectorAny(rules []config.ViewedRule, path string, mine map[string]bool) bool {
	for _, rule := range rules {
		if viewedSelector(rule, path, mine) {
			return true
		}
	}
	return false
}
