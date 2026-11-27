package policy

import (
	"regexp"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// ClassifyConflict classifies a PR as up-to-date, behind, clean-rebase, or
// conflicting based on its GitHub merge state and mergeability.
func ClassifyConflict(pr scan.PR) ConflictClass {
	if pr.Mergeable == "CONFLICTING" || pr.MergeStateStatus == "DIRTY" {
		return ClassConflicting
	}
	if pr.MergeStateStatus == "BEHIND" {
		if pr.Mergeable == "MERGEABLE" {
			return ClassCleanRebase
		}
		return ClassBehind
	}
	return ClassUpToDate
}

// MatchNitThreads returns unresolved review threads on pr that have no author
// reply, were authored by a configured bot, and match at least one nit/class
// pattern. Human reviewer threads are never returned.
func MatchNitThreads(pr scan.PR, bots []string, patterns []*regexp.Regexp) []scan.Comment {
	if len(bots) == 0 || len(patterns) == 0 {
		return nil
	}
	var out []scan.Comment
	for _, c := range pr.BlockingComments {
		if c.HasAuthorReply {
			continue
		}
		if !isConfiguredBot(c.Author, bots) {
			continue
		}
		if !matchesAnyPattern(c.Body, patterns) {
			continue
		}
		out = append(out, c)
	}
	return out
}

func isConfiguredBot(author string, bots []string) bool {
	if author == "" {
		return false
	}
	for _, b := range bots {
		if strings.EqualFold(author, b) {
			return true
		}
	}
	return false
}

func matchesAnyPattern(body string, patterns []*regexp.Regexp) bool {
	for _, re := range patterns {
		if re != nil && re.MatchString(body) {
			return true
		}
	}
	return false
}

func hasHoldLabel(labels, holdLabels []string) string {
	for _, label := range labels {
		for _, hold := range holdLabels {
			if strings.EqualFold(label, hold) {
				return label
			}
		}
	}
	return ""
}

func isStackedPR(pr scan.PR, defaultBranches []string) bool {
	if pr.Base == "" {
		return false
	}
	if pr.DefaultBranch != "" {
		return pr.Base != pr.DefaultBranch
	}
	if len(defaultBranches) == 0 {
		return false
	}
	for _, b := range defaultBranches {
		if pr.Base == b {
			return false
		}
	}
	return true
}

// BotLoginMatches reports whether two GitHub logins refer to the same bot
// account, normalizing optional leading "@", optional trailing "[bot]", and case.
func BotLoginMatches(a, b string) bool {
	na := normalizeBotLogin(a)
	nb := normalizeBotLogin(b)
	return na != "" && na == nb
}

func normalizeBotLogin(login string) string {
	login = strings.TrimSpace(login)
	login = strings.TrimPrefix(login, "@")
	if len(login) > 5 && strings.EqualFold(login[len(login)-5:], "[bot]") {
		login = login[:len(login)-5]
	}
	return strings.ToLower(login)
}

// EvaluateBotRereview checks all four preconditions for posting a bot re-review
// comment:
//  1. The bot has reviewed the PR (at least one non-PENDING review).
//  2. The bot's latest review is not APPROVED.
//  3. Every review thread started by the bot is resolved.
//  4. No PR issue comment with the same trimmed body is newer than the PR's last commit.
func EvaluateBotRereview(snap gh.BotRereviewSnapshot, botLogin, body string) (bool, string) {
	if strings.TrimSpace(botLogin) == "" {
		return false, "empty_bot_login"
	}
	wantBody := strings.TrimSpace(body)
	if wantBody == "" {
		return false, "empty_body"
	}
	state, hasReview := botLatestReviewState(snap.Reviews, botLogin)
	if !hasReview {
		return false, "bot_has_not_reviewed"
	}
	if strings.EqualFold(state, "APPROVED") {
		return false, "latest_review_approved"
	}
	if !allBotThreadsResolved(snap.Threads, botLogin) {
		return false, "unresolved_bot_threads"
	}
	if hasNewerMatchingComment(snap.Comments, snap.LastCommitAt, wantBody) {
		return false, "comment_newer_than_last_commit"
	}
	return true, ""
}

func botLatestReviewState(reviews []gh.Review, botLogin string) (string, bool) {
	var (
		hasReview bool
		latest    gh.Review
	)
	for _, r := range reviews {
		if strings.EqualFold(r.State, "PENDING") || !BotLoginMatches(r.Author, botLogin) {
			continue
		}
		if !hasReview || !r.SubmittedAt.Before(latest.SubmittedAt) {
			latest = r
			hasReview = true
		}
	}
	return latest.State, hasReview
}

func allBotThreadsResolved(threads []gh.ReviewThreadSummary, botLogin string) bool {
	for _, th := range threads {
		if BotLoginMatches(th.StarterAuthor, botLogin) && !th.IsResolved {
			return false
		}
	}
	return true
}

func hasNewerMatchingComment(comments []gh.IssueComment, lastCommitAt time.Time, wantBody string) bool {
	for _, c := range comments {
		if strings.TrimSpace(c.Body) != wantBody {
			continue
		}
		if lastCommitAt.IsZero() || c.CreatedAt.After(lastCommitAt) {
			return true
		}
	}
	return false
}
