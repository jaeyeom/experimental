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
func (r *Runner) markViewed(entry *LogEntry, pr github.ApprovalPullRequest, viewer string, apply bool, st *prState, pool *loadPool) {
	if entry.Action == ActionSkipped || entry.Action == ActionError {
		return
	}
	fcPaths, fcPatterns, fcExclusions := r.fileCheckViewedPaths(entry, pr)
	entry.Exclusions = append(entry.Exclusions, fcExclusions...)

	var viewedPaths []string
	var genMatches map[string]string
	if r.Viewed.Enabled && entry.Action == ActionManual && entry.Rule == nil && !pr.Draft {
		paths, matches, exclusions, err := r.viewedPaths(pr, viewer, st, pool)
		if err != nil {
			entry.Action, entry.Reason = ActionError, err.Error()
			return
		}
		entry.Exclusions = append(entry.Exclusions, exclusions...)
		entry.GeneratedMatches = matches
		viewedPaths = paths
		genMatches = matches
	}
	_ = genMatches

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
	notes := dismissedNotes(pr.Files, marked)
	r.recordMarkedViewed(entry, marked, fcPatterns, notes, apply, capped, err)
}

func (r *Runner) recordMarkedViewed(entry *LogEntry, marked []string, fcPatterns, notes map[string]string, apply, capped bool, markErr error) {
	patterns := filterPatterns(fcPatterns, marked)
	if markErr != nil {
		if entry.Action == ActionManual {
			entry.Paths = marked
			entry.FileCheckPatterns = patterns
			entry.PathNotes = notes
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
		entry.PathNotes = notes
		if capped {
			entry.Reason = "mark cap reached"
		}
		return
	}
	entry.Paths = marked
	entry.FileCheckPatterns = patterns
	entry.PathNotes = notes
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
		PathNotes:         notes,
	}
	if capped {
		extra.Reason = "mark cap reached"
	}
	entry.ExtraLogs = append(entry.ExtraLogs, extra)
}

const remarkAfterChangeNote = "re-mark after change"

func dismissedNotes(files []models.File, marked []string) map[string]string {
	if len(marked) == 0 {
		return nil
	}
	dismissed := make(map[string]bool)
	for _, f := range files {
		if f.ViewerViewedState == models.ViewerDismissed {
			dismissed[f.Path] = true
		}
	}
	var notes map[string]string
	for _, p := range marked {
		if dismissed[p] {
			if notes == nil {
				notes = make(map[string]string)
			}
			notes[p] = remarkAfterChangeNote
		}
	}
	return notes
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
			if st := states[pass.Path]; st == models.ViewerViewed {
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

// viewedPaths returns the UNVIEWED files a rule selects, in pull request order,
// along with any @generated patterns that matched. Unreadable CODEOWNERS, team
// data, or generated list fails the pull request closed.
func (r *Runner) viewedPaths(pr github.ApprovalPullRequest, viewer string, st *prState, pool *loadPool) ([]string, map[string]string, []string, error) {
	rules := viewedRulesFor(r.Viewed.Rules, pr)
	if len(rules) == 0 {
		return nil, nil, nil, nil
	}
	var genPatterns []string
	if viewedRulesUseGenerated(rules) {
		var genErr string
		genPatterns, genErr = r.generatedListFor(pr, pool)
		if genErr != "" {
			return nil, nil, nil, fmt.Errorf("%s", genErr)
		}
	}
	mine, err := r.viewedOwnership(pr, viewer, rules, st, pool)
	if err != nil {
		return nil, nil, nil, err
	}
	var paths, exclusions []string
	var genMatches map[string]string
	for _, file := range pr.Files {
		if file.ViewerViewedState != models.ViewerUnviewed && (!r.Viewed.RemarkDismissed || file.ViewerViewedState != models.ViewerDismissed) {
			continue
		}
		mark, genPat, reason := viewedDecision(r.Viewed.NeverMark, rules, genPatterns, file.Path, mine)
		if mark {
			paths = append(paths, file.Path)
			if genPat != "" {
				if genMatches == nil {
					genMatches = make(map[string]string)
				}
				genMatches[file.Path] = genPat
			}
			continue
		}
		if reason != "" {
			exclusions = append(exclusions, file.Path+": "+reason)
		}
	}
	return paths, genMatches, exclusions, nil
}

func viewedRulesUseGenerated(rules []config.ViewedRule) bool {
	for _, rule := range rules {
		if config.UsesGeneratedList(rule.PathsMatch) || config.UsesGeneratedList(rule.PathsNoneMatch) {
			return true
		}
	}
	return false
}

// viewedOwnership loads CODEOWNERS when a rule needs it. mine is nil when
// no rule needs it. A read error marks nothing.
func (r *Runner) viewedOwnership(pr github.ApprovalPullRequest, viewer string, rules []config.ViewedRule, st *prState, pool *loadPool) (map[string]bool, error) {
	if !slices.ContainsFunc(rules, func(rule config.ViewedRule) bool { return rule.CodeownersNotMine }) {
		return nil, nil
	}
	if pr.FilesErr != "" {
		return nil, fmt.Errorf("read CODEOWNERS: %s", pr.FilesErr)
	}
	own := pr.Ownership
	if own == nil {
		own = r.ownershipFor(pr, viewer, st, pool)
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
	for key, got := range r.teams.snapshot() {
		if got.err != nil {
			errs[key] = got.err
		}
	}
	for key, got := range r.teamCounts.snapshot() {
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

// viewedDecision reports whether path is marked, the matching @generated
// pattern if selected via @generated, or why an exclusion kept it visible.
// A reason is empty when no selector matched. never_mark wins over a rule's
// paths_none_match. mine is nil when no rule uses codeowners.
func viewedDecision(never []string, rules []config.ViewedRule, genPatterns []string, path string, mine map[string]bool) (bool, string, string) {
	if glob := firstGlob(never, path); glob != "" && viewedSelectorAny(rules, genPatterns, path, mine) {
		return false, "", "excluded by viewed.never_mark " + glob
	}
	var blocked string
	for _, rule := range rules {
		matched, genPat := viewedSelector(rule, genPatterns, path, mine)
		if !matched {
			continue
		}
		if glob, fromGen, ok := matchGlobsInList(rule.PathsNoneMatch, genPatterns, path); ok {
			if blocked == "" {
				if fromGen {
					glob = fmt.Sprintf("%s %q", config.GeneratedListRef, glob)
				}
				blocked = fmt.Sprintf("excluded by rule %s paths_none_match %s", rule.Name, glob)
			}
			continue
		}
		return true, genPat, ""
	}
	return false, "", blocked
}

// viewedSelector reports whether a rule's selectors match path, before
// paths_none_match, and the @generated pattern if matched via @generated.
// mine is nil when no rule uses codeowners.
func viewedSelector(rule config.ViewedRule, genPatterns []string, path string, mine map[string]bool) (bool, string) {
	if len(rule.PathsMatch) > 0 {
		if pat, fromGen, ok := matchGlobsInList(rule.PathsMatch, genPatterns, path); ok {
			if fromGen {
				return true, pat
			}
			return true, ""
		}
	}
	return rule.CodeownersNotMine && mine != nil && !mine[path], ""
}

func viewedSelectorAny(rules []config.ViewedRule, genPatterns []string, path string, mine map[string]bool) bool {
	for _, rule := range rules {
		if ok, _ := viewedSelector(rule, genPatterns, path, mine); ok {
			return true
		}
	}
	return false
}
