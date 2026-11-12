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

// needsOwnership reports whether any rule sets codeowners_covered.
func needsOwnership(rules []config.ApproveRule) bool {
	return slices.ContainsFunc(rules, func(r config.ApproveRule) bool { return r.CodeownersCovered })
}

// loadOwnership reads CODEOWNERS on the base branch and the members of
// every team that owns a changed file. Team members are cached per run.
func (r *Runner) loadOwnership(pr github.ApprovalPullRequest, viewer string) *github.Ownership {
	if pr.FilesErr != "" {
		return &github.Ownership{Err: pr.FilesErr}
	}
	_, text, err := r.Client.CodeownersFile(pr.Ref, pr.BaseRef)
	if err != nil {
		return &github.Ownership{Err: err.Error()}
	}
	owners := codeowners.ParseSections(strings.NewReader(text))
	members := func(org, slug string) ([]string, error) {
		key := strings.ToLower(org + "/" + slug)
		if r.teams == nil {
			r.teams = map[string]teamMembers{}
		}
		got, ok := r.teams[key]
		if !ok {
			got.members, got.err = r.Client.TeamMembers(org, slug)
			r.teams[key] = got
		}
		return got.members, got.err
	}
	return computeOwnership(owners, pr, viewer, members)
}

// computeOwnership decides, for each changed file, whether an owner is the
// viewer or a team of the viewer, and which owner keeps the file with the
// viewer. A team counts only when it has other members and is requested.
func computeOwnership(co *codeowners.Codeowners, pr github.ApprovalPullRequest, viewer string,
	members func(org, slug string) ([]string, error),
) *github.Ownership {
	out := &github.Ownership{}
	for _, file := range pr.Files {
		got := github.FileOwnership{Path: file.Path}
		var blockers []string
		for _, owner := range co.OwnersFor(file.Path) {
			got.Owners = append(got.Owners, owner.String())
		}
		slices.Sort(got.Owners)
		for _, raw := range got.Owners {
			mine, blocker := ownerVerdict(codeowners.ParseOwner(raw), pr, viewer, members)
			got.Mine = got.Mine || mine
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
// and why it keeps the file with the viewer, or "".
func ownerVerdict(owner codeowners.Owner, pr github.ApprovalPullRequest, viewer string,
	members func(org, slug string) ([]string, error),
) (bool, string) {
	switch o := owner.(type) {
	case codeowners.User:
		if strings.EqualFold(o.Name, viewer) {
			return true, o.String() + " is you"
		}
		return false, ""
	case codeowners.Team:
		list, err := members(o.Org, o.Name)
		if err != nil {
			return false, o.String() + ": " + err.Error()
		}
		if !containsFold(list, viewer) {
			return false, ""
		}
		if !slices.ContainsFunc(list, func(m string) bool { return !strings.EqualFold(m, viewer) }) {
			return true, o.String() + ": you are its only member"
		}
		if !strings.EqualFold(o.Org, pr.Ref.Owner) || !containsFold(pr.RequestedTeams, o.Name) {
			return true, o.String() + ": includes you and is not a requested reviewer"
		}
		return true, ""
	default:
		return false, owner.String() + ": cannot tell whether this owner is you"
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
