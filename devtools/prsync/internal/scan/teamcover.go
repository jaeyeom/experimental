package scan

import (
	"fmt"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

// coverFacts is one pull request's team-cover inputs. The caller resolves
// GitHub data; suggestTeams only ranks and shares.
type coverFacts struct {
	author    string
	requested []string
	reviews   []gh.LatestReview
	files     []string
	rules     []codeRule
	members   map[string][]gh.TeamMember
	memberErr map[string]string
	history   map[string]gh.PathHistory
	load      map[string]int
	loadOn    bool
	inactive  map[string]bool
	exclude   []string
	signals   []string
	perTeam   int
	teams     []string
}

type coverCandidate struct {
	login    string
	owned    int
	patterns []string
	authored int
	reviewed int
	load     int
}

type teamPlan struct {
	name    string
	members []gh.TeamMember
	err     string
	covered string
	ranked  []coverCandidate
	chosen  []coverCandidate
}

// suggestTeams ranks uncovered teams and reuses a person across the
// smallest teams first. teams is the output order. A nil result means
// there is nothing to report.
func suggestTeams(in coverFacts) []TeamReviewSuggestion {
	if len(in.teams) == 0 {
		return nil
	}
	plans := make([]teamPlan, len(in.teams))
	for i, team := range in.teams {
		plans[i] = planTeam(in, team)
	}
	shareSuggestions(plans, in.perTeam)
	return suggestionsFrom(plans, in.signals, in.loadOn)
}

func planTeam(in coverFacts, team string) teamPlan {
	plan := teamPlan{name: team}
	if msg := in.memberErr[team]; msg != "" {
		plan.err = msg
		return plan
	}
	plan.members = in.members[team]
	plan.covered = coveredBy(plan.members, in.requested, in.reviews)
	if plan.covered != "" {
		return plan
	}
	for _, member := range plan.members {
		if !keepCandidate(member, in) {
			continue
		}
		plan.ranked = append(plan.ranked, scoreCandidate(member.Login, in))
	}
	slices.SortFunc(plan.ranked, func(a, b coverCandidate) int {
		return compareCandidates(in.signals, a, b)
	})
	return plan
}

func keepCandidate(member gh.TeamMember, in coverFacts) bool {
	login := member.Login
	if login == "" || strings.EqualFold(login, in.author) || isBotMember(member) {
		return false
	}
	if containsFold(in.exclude, login) || isApprover(in.reviews, login) {
		return false
	}
	return !in.inactive[strings.ToLower(login)]
}

func shareSuggestions(plans []teamPlan, perTeam int) {
	if perTeam < 1 {
		return
	}
	order := make([]int, len(plans))
	for i := range order {
		order[i] = i
	}
	slices.SortStableFunc(order, func(a, b int) int {
		return compareTeamSize(plans[a], plans[b])
	})
	var picked []string
	for _, index := range order {
		plan := &plans[index]
		if plan.err != "" || plan.covered != "" {
			continue
		}
		plan.chosen = pickForTeam(plan.ranked, picked, perTeam)
		for _, chosen := range plan.chosen {
			if !containsFold(picked, chosen.login) {
				picked = append(picked, chosen.login)
			}
		}
	}
}

func compareTeamSize(a, b teamPlan) int {
	if len(a.members) != len(b.members) {
		return len(a.members) - len(b.members)
	}
	return strings.Compare(strings.ToLower(a.name), strings.ToLower(b.name))
}

func pickForTeam(ranked []coverCandidate, picked []string, perTeam int) []coverCandidate {
	chosen := make([]coverCandidate, 0, perTeam)
	for _, login := range picked {
		found, ok := findCandidate(ranked, login)
		if !ok {
			continue
		}
		chosen = append(chosen, found)
		if len(chosen) == perTeam {
			return chosen
		}
	}
	for _, item := range ranked {
		if len(chosen) == perTeam {
			break
		}
		if containsCandidate(chosen, item.login) {
			continue
		}
		chosen = append(chosen, item)
	}
	return chosen
}

func suggestionsFrom(plans []teamPlan, signals []string, loadOn bool) []TeamReviewSuggestion {
	out := make([]TeamReviewSuggestion, len(plans))
	for i, plan := range plans {
		item := TeamReviewSuggestion{Team: plan.name, Suggest: []SuggestedReviewer{}}
		switch {
		case plan.err != "":
			item.Error = plan.err
		case plan.covered != "":
			item.CoveredBy = stringPtr(plan.covered)
		default:
			for _, chosen := range plan.chosen {
				item.Suggest = append(item.Suggest, SuggestedReviewer{
					Login:   chosen.login,
					Reasons: coverCandidateReasons(signals, chosen, loadOn),
				})
			}
		}
		out[i] = item
	}
	return out
}

func coveredBy(members []gh.TeamMember, requested []string, reviews []gh.LatestReview) string {
	var pending []string
	var reviewed []string
	for _, member := range members {
		if containsFold(requested, member.Login) {
			pending = append(pending, member.Login)
		}
		if hasSubmittedReview(reviews, member.Login) {
			reviewed = append(reviewed, member.Login)
		}
	}
	slices.SortFunc(pending, compareLogins)
	if len(pending) > 0 {
		return pending[0]
	}
	slices.SortFunc(reviewed, compareLogins)
	if len(reviewed) > 0 {
		return reviewed[0]
	}
	return ""
}

func scoreCandidate(login string, in coverFacts) coverCandidate {
	scored := coverCandidate{login: login, load: in.load[login]}
	seen := map[string]struct{}{}
	for _, file := range in.files {
		rule, ok := lastCodeOwner(in.rules, file)
		if ok && ownerMatches(login, rule.owners, in.members) {
			scored.owned++
			if _, done := seen[rule.pattern]; !done {
				seen[rule.pattern] = struct{}{}
				scored.patterns = append(scored.patterns, rule.pattern)
			}
		}
		hist := in.history[file]
		scored.authored += countFold(hist.Authors, login)
		scored.reviewed += countFold(hist.Reviewers, login)
	}
	return scored
}

func ownerMatches(login string, owners []codeOwner, members map[string][]gh.TeamMember) bool {
	for _, owner := range owners {
		if owner.login != "" && strings.EqualFold(owner.login, login) {
			return true
		}
		if owner.team == "" {
			continue
		}
		for _, member := range members[owner.team] {
			if strings.EqualFold(member.Login, login) {
				return true
			}
		}
	}
	return false
}

func coverCandidateReasons(signals []string, item coverCandidate, loadOn bool) []string {
	var reasons []string
	for _, signal := range signals {
		switch signal {
		case "codeowners":
			for _, pattern := range item.patterns {
				reasons = append(reasons, "codeowner: "+pattern)
			}
		case "history":
			reasons = appendHistoryReasons(reasons, item)
		case "load":
			if loadOn {
				reasons = append(reasons, countPhrase(item.load, "", "open direct request"))
			}
		}
	}
	if reasons == nil {
		return []string{}
	}
	return reasons
}

func appendHistoryReasons(reasons []string, item coverCandidate) []string {
	if item.authored > 0 {
		reasons = append(reasons, countPhrase(item.authored, "authored ", "recent change"))
	}
	if item.reviewed > 0 {
		reasons = append(reasons, countPhrase(item.reviewed, "reviewed ", "recent change"))
	}
	return reasons
}

func countPhrase(n int, prefix, singular string) string {
	word := singular
	if n != 1 {
		word += "s"
	}
	return fmt.Sprintf("%s%d %s", prefix, n, word)
}

func compareCandidates(signals []string, a, b coverCandidate) int {
	for _, signal := range signals {
		if cmp := compareSignal(signal, a, b); cmp != 0 {
			return cmp
		}
	}
	return compareLogins(a.login, b.login)
}

func compareSignal(signal string, a, b coverCandidate) int {
	switch signal {
	case "codeowners":
		return b.owned - a.owned
	case "history":
		return (b.authored + b.reviewed) - (a.authored + a.reviewed)
	case "load":
		return a.load - b.load
	default:
		return 0
	}
}

func compareLogins(a, b string) int {
	al := strings.ToLower(a)
	bl := strings.ToLower(b)
	if al != bl {
		return strings.Compare(al, bl)
	}
	return strings.Compare(a, b)
}

func findCandidate(ranked []coverCandidate, login string) (coverCandidate, bool) {
	for _, item := range ranked {
		if strings.EqualFold(item.login, login) {
			return item, true
		}
	}
	return coverCandidate{}, false
}

func containsCandidate(chosen []coverCandidate, login string) bool {
	for _, item := range chosen {
		if strings.EqualFold(item.login, login) {
			return true
		}
	}
	return false
}

func isBotMember(member gh.TeamMember) bool {
	return strings.EqualFold(member.Type, "Bot") || strings.HasSuffix(member.Login, "[bot]")
}

func isApprover(reviews []gh.LatestReview, login string) bool {
	for _, review := range reviews {
		if strings.EqualFold(review.Author.Login, login) && strings.EqualFold(review.State, "APPROVED") {
			return true
		}
	}
	return false
}

func hasSubmittedReview(reviews []gh.LatestReview, login string) bool {
	for _, review := range reviews {
		if !strings.EqualFold(review.Author.Login, login) {
			continue
		}
		state := strings.ToUpper(strings.TrimSpace(review.State))
		if state != "" && state != "PENDING" {
			return true
		}
	}
	return false
}

func containsFold(list []string, login string) bool {
	for _, item := range list {
		if strings.EqualFold(item, login) {
			return true
		}
	}
	return false
}

func countFold(list []string, login string) int {
	n := 0
	for _, item := range list {
		if strings.EqualFold(item, login) {
			n++
		}
	}
	return n
}

func stringPtr(s string) *string {
	v := s
	return &v
}
