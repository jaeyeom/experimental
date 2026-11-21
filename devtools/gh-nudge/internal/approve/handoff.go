package approve

import (
	"fmt"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/codeowners"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

// reRequestedAfterHandoff is the manual reason the loop guard reports.
const reRequestedAfterHandoff = "re-requested after hand-off"

// teamMembers is one team's members, or why they could not be read.
type teamMembers struct {
	members []string
	err     error
}

// teamMembers returns the members of org/slug. They are cached per run.
func (r *Runner) teamMembers(org, slug string, st *prState, pool *loadPool) ([]string, error) {
	key := strings.ToLower(org + "/" + slug)
	got, _ := r.teams.getOrLoad(key, func() teamMembers {
		var res teamMembers
		r.measure(&st.teams, func() {
			err := pool.callWithRetry(func() error {
				var callErr error
				res.members, callErr = r.Client.TeamMembers(org, slug)
				return callErr //nolint:wrapcheck // caller records team member error
			})
			res.err = err
		})
		return res
	})
	return got.members, got.err
}

// viewerTeamSet is the slugs, lowercase, of the teams of one organization
// that include the viewer, or why they could not be read.
type viewerTeamSet struct {
	slugs map[string]bool
	hash  string
	err   error
}

// viewerTeamsOf returns the viewer's teams in org. They are read in one
// paged query per organization and cached per run.
func (r *Runner) viewerTeamsOf(org, viewer string, st *prState, pool *loadPool) viewerTeamSet {
	key := strings.ToLower(org)
	got, _ := r.viewerTeams.getOrLoad(key, func() viewerTeamSet {
		var res viewerTeamSet
		r.measure(&st.teams, func() {
			var slugs []string
			err := pool.callWithRetry(func() error {
				var callErr error
				slugs, callErr = r.Client.ViewerTeams(org, viewer)
				return callErr //nolint:wrapcheck // caller records viewer teams error
			})
			res.err = err
			if err == nil {
				res.slugs = make(map[string]bool, len(slugs))
				for _, slug := range slugs {
					res.slugs[strings.ToLower(slug)] = true
				}
				res.hash = hashViewerTeams(res.slugs, true)
			} else {
				res.hash = hashViewerTeams(nil, false)
			}
		})
		return res
	})
	return got
}

// teamCount is a team's member count, or why it could not be read.
type teamCount struct {
	n   int
	err error
}

// teamMemberCount returns how many members org/slug has, cached per run.
func (r *Runner) teamMemberCount(org, slug string, st *prState, pool *loadPool) (int, error) {
	key := strings.ToLower(org + "/" + slug)
	got, _ := r.teamCounts.getOrLoad(key, func() teamCount {
		var res teamCount
		r.measure(&st.teams, func() {
			err := pool.callWithRetry(func() error {
				var callErr error
				res.n, callErr = r.Client.TeamMemberCount(org, slug)
				return callErr //nolint:wrapcheck // caller records team count error
			})
			res.err = err
		})
		return res
	})
	return got.n, got.err
}

// viewerInTeam reports whether the viewer is in org/slug. It reads the
// viewer's teams and falls back to the team's member list when they cannot
// be read.
func (r *Runner) viewerInTeam(org, slug, viewer string, st *prState, pool *loadPool) (bool, error) {
	st.teams.used = true
	if set := r.viewerTeamsOf(org, viewer, st, pool); set.err == nil {
		return set.slugs[strings.ToLower(slug)], nil
	}
	members, err := r.teamMembers(org, slug, st, pool)
	if err != nil {
		return false, err
	}
	return containsFold(members, viewer), nil
}

// teamView is what ownership needs to know of a team.
type teamView struct {
	// viewerIn is true when the viewer is in the team.
	viewerIn bool
	// others is true when the team has a member besides the viewer.
	others bool
}

// teamViewFor says whether the viewer is in org/slug and whether it has
// other members. With the viewer's teams it reads a member count, not the
// member list.
func (r *Runner) teamViewFor(org, slug, viewer string, st *prState, pool *loadPool) (teamView, error) {
	st.teams.used = true
	if set := r.viewerTeamsOf(org, viewer, st, pool); set.err == nil {
		if !set.slugs[strings.ToLower(slug)] {
			return teamView{}, nil
		}
		n, err := r.teamMemberCount(org, slug, st, pool)
		if err != nil {
			return teamView{}, err
		}
		return teamView{viewerIn: true, others: n > 1}, nil
	}
	members, err := r.teamMembers(org, slug, st, pool)
	if err != nil {
		return teamView{}, err
	}
	return teamView{
		viewerIn: containsFold(members, viewer),
		others:   slices.ContainsFunc(members, func(m string) bool { return !strings.EqualFold(m, viewer) }),
	}, nil
}

// codeownersEntry is a parsed CODEOWNERS file, or why it could not be read.
type codeownersEntry struct {
	owners  *codeowners.Codeowners
	blobSHA string
	err     error
}

// codeownersOf returns the parsed CODEOWNERS on pr's base branch and its blob
// SHA. It is read once per repository and base ref in a run.
func (r *Runner) codeownersOf(pr github.ApprovalPullRequest, st *prState, pool *loadPool) (*codeowners.Codeowners, string, error) {
	key := strings.ToLower(pr.Ref.Owner+"/"+pr.Ref.Repo) + "@" + pr.BaseRef
	st.codeowners.used = true
	got, _ := r.codeowners.getOrLoad(key, func() codeownersEntry {
		var res codeownersEntry
		r.measure(&st.codeowners, func() {
			var blob github.CodeownersBlob
			err := pool.callWithRetry(func() error {
				var callErr error
				blob, callErr = r.Client.CodeownersBlob(pr.Ref, pr.BaseRef)
				return callErr //nolint:wrapcheck // caller records CODEOWNERS error
			})
			if err != nil {
				res.err = err
				return
			}
			res.blobSHA = blob.OID
			res.owners = codeowners.ParseSections(strings.NewReader(blob.Text))
		})
		return res
	})
	return got.owners, got.blobSHA, got.err
}

// loadOwnership reads CODEOWNERS on the base branch and decides, for every
// team that owns a changed file, whether the viewer is in it. CODEOWNERS and
// team data are cached per run.
func (r *Runner) loadOwnership(pr github.ApprovalPullRequest, viewer string, st *prState, pool *loadPool) *github.Ownership {
	if pr.FilesErr != "" {
		return &github.Ownership{Err: pr.FilesErr}
	}
	owners, blobSHA, err := r.codeownersOf(pr, st, pool)
	if err != nil {
		return &github.Ownership{Err: err.Error()}
	}
	st.loadedCodeownersSHA = blobSHA
	set := r.viewerTeamsOf(pr.Ref.Owner, viewer, st, pool)
	st.loadedTeamsHash = set.hash
	return computeOwnership(owners, pr, viewer, func(org, slug string) (teamView, error) {
		return r.teamViewFor(org, slug, viewer, st, pool)
	})
}

// computeOwnership decides, for each changed file, whether an owner is the
// viewer or a team of the viewer, and which owner keeps the file with the
// viewer. A team counts only when it has other members and is requested.
func computeOwnership(co *codeowners.Codeowners, pr github.ApprovalPullRequest, viewer string,
	team func(org, slug string) (teamView, error),
) *github.Ownership {
	out := &github.Ownership{}
	for _, file := range pr.Files {
		got := github.FileOwnership{Path: file.Path}
		var blockers, entries []string
		for _, rule := range co.RulesFor(file.Path) {
			entries = append(entries, rule.String())
		}
		got.Entry = strings.Join(entries, "; ")
		for _, owner := range co.OwnersFor(file.Path) {
			got.Owners = append(got.Owners, owner.String())
		}
		slices.Sort(got.Owners)
		for _, raw := range got.Owners {
			mine, blocker, unknown := ownerVerdict(codeowners.ParseOwner(raw), pr, viewer, team)
			got.Mine = got.Mine || mine
			if mine {
				got.MineOwners = append(got.MineOwners, raw)
			}
			if unknown {
				got.Unknown = append(got.Unknown, blocker)
			}
			if blocker != "" {
				blockers = append(blockers, blocker)
			}
		}
		got.Blocker = strings.Join(blockers, "; ")
		out.Files = append(out.Files, got)
	}
	return out
}

// ownerVerdict reports whether owner is the viewer or includes the viewer,
// and why it keeps the file with the viewer, or "". unknown is true when it
// cannot tell whether owner includes the viewer; the reason is the blocker.
func ownerVerdict(owner codeowners.Owner, pr github.ApprovalPullRequest, viewer string,
	team func(org, slug string) (teamView, error),
) (mine bool, blocker string, unknown bool) {
	switch o := owner.(type) {
	case codeowners.User:
		if strings.EqualFold(o.Name, viewer) {
			return true, o.String() + " is you", false
		}
		return false, "", false
	case codeowners.Team:
		view, err := team(o.Org, o.Name)
		if err != nil {
			return false, o.String() + ": " + err.Error(), true
		}
		if !view.viewerIn {
			return false, "", false
		}
		if !view.others {
			return true, o.String() + ": you are its only member", false
		}
		if !strings.EqualFold(o.Org, pr.Ref.Owner) || !containsFold(pr.RequestedTeams, o.Name) {
			return true, o.String() + ": includes you and is not a requested reviewer", false
		}
		return true, "", false
	default:
		return false, owner.String() + ": cannot tell whether this owner is you", true
	}
}

// loopGuard reads the viewer's review request events and reports whether
// a hand-off is allowed. Once the viewer's own account removed the viewer
// from the pull request, it is never handed off again.
func loopGuard(events []github.ReviewRequestEvent, viewer string) (string, bool) {
	removedAt := ""
	requests := 0
	for _, event := range events {
		switch {
		case event.Event == github.ReviewRequestRemoved && strings.EqualFold(event.Actor, viewer):
			if removedAt == "" {
				removedAt = event.Time
			}
		case event.Event == github.ReviewRequested:
			requests++
			if removedAt != "" {
				return fmt.Sprintf("%s: removed by you at %s, requested again by %s at %s",
					reRequestedAfterHandoff, removedAt, event.Actor, event.Time), false
			}
		}
	}
	if removedAt != "" {
		return fmt.Sprintf("%s: removed by you at %s", reRequestedAfterHandoff, removedAt), false
	}
	return fmt.Sprintf("ok: %d review request(s) of you, never removed by you", requests), true
}

// eventsFor returns the events whose reviewer is login.
func eventsFor(events []github.ReviewRequestEvent, login string) []github.ReviewRequestEvent {
	var out []github.ReviewRequestEvent
	for _, event := range events {
		if strings.EqualFold(event.Reviewer, login) {
			out = append(out, event)
		}
	}
	return out
}

// handoffSkipReason returns why a hand-off is not needed, or "".
func handoffSkipReason(pr github.ApprovalPullRequest, rule config.ApproveRule, viewer string) string {
	reviewed := func(login string) bool {
		return slices.ContainsFunc(pr.Reviews, func(r github.ApprovalReview) bool { return strings.EqualFold(r.Author, login) })
	}
	if reviewed(viewer) {
		return "you already reviewed this pull request"
	}
	for _, login := range rule.Reviewers {
		if containsFold(pr.RequestedUsers, login) {
			return login + " is already requested"
		}
		if reviewed(login) {
			return login + " already reviewed this pull request"
		}
	}
	return ""
}

// handoff decides and, with apply, performs a hand-off for a matched rule.
func (r *Runner) handoff(entry *LogEntry, pr github.ApprovalPullRequest, rule config.ApproveRule, viewer string, apply bool) {
	entry.Reviewers = slices.Clone(rule.Reviewers)
	events, err := r.Client.ReviewRequestTimeline(pr.Ref)
	if err != nil {
		entry.Action, entry.Reason = ActionError, err.Error()
		return
	}
	entry.Timeline = eventsFor(events, viewer)
	guard, ok := loopGuard(entry.Timeline, viewer)
	entry.LoopGuard = guard
	if !ok {
		entry.Action, entry.Reason = ActionManual, reRequestedAfterHandoff
		return
	}
	if reason := handoffSkipReason(pr, rule, viewer); reason != "" {
		entry.Action, entry.Reason = ActionSkipped, reason
		return
	}
	if !apply {
		entry.Action = ActionWouldHandoff
		return
	}
	head, err := r.Client.PullRequestHeadSHA(pr.Ref)
	if err != nil {
		entry.Action, entry.Reason = ActionError, err.Error()
		return
	}
	if head != pr.HeadSHA {
		entry.Action, entry.Reason = ActionSkipped, fmt.Sprintf("head moved from %s to %s since evaluation", pr.HeadSHA, head)
		return
	}
	reviewers := strings.Join(rule.Reviewers, ", ")
	if err := r.Client.RequestReviewers(pr.Ref, rule.Reviewers); err != nil {
		entry.Steps = append(entry.Steps, "request "+reviewers+": "+err.Error())
		entry.Action, entry.Reason = ActionError, err.Error()
		return
	}
	entry.Steps = append(entry.Steps, "requested "+reviewers)
	if err := r.Client.RemoveRequestedReviewer(pr.Ref, viewer); err != nil {
		entry.Steps = append(entry.Steps, "remove "+viewer+": "+err.Error())
		entry.Action = ActionError
		entry.Reason = fmt.Sprintf("requested %s, but removing you failed: %v", reviewers, err)
		return
	}
	entry.Steps = append(entry.Steps, "removed "+viewer)
	entry.Action = ActionHandoff
}
