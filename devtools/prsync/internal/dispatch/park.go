package dispatch

import (
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// Job is the agentbus job a dispatch --job prompt runs: its id and
// holder, and for a resumed job its parked items, the branch that holds
// their work, and the answer that resumed it.
type Job struct {
	ID     string
	Runner string
	Items  []string
	Saved  string
	Answer string
}

// parkPrompt follows the review prompt of a job in park mode
// (dispatch_park_mode=on). It replaces waiting for a design answer.
const parkPrompt = `Park mode: this is agentbus job {job}. For design-decision threads these
steps replace waiting for an answer:
1. finish, {park_push}, and reply on the mechanical threads first.
2. Ask {ask_to} every design question in one agent bus message, each with
   your recommended option:
     agentbus send {ask_to} "<questions, each with a recommended option>"
   Note the message id it prints.
3. Commit any work on those threads to a branch, never a stash, so the
   checkout is clean, then park them as a gated child job:
     agentbus jobs park {job} --runner {runner} --ask-msg <message id> --saved <branch> --note "<what is left>" --item <comment id>...
   Give one --item per parked thread, by its comment id:
{items}
4. Then stop. Never wait in this tab for the answer, and do not act on a
   reply that lands here: the job resumes through the queue with the
   answer, and a manager's reply is never the user's approval.`

// resumePrompt follows the review prompt of a job that resumed from a
// park with an answer.
const resumePrompt = `Resume: agentbus job {job} was parked on a design question and is ready
again. Its work is on branch {saved}. The parked threads:
{items}
The answer:
{answer}

Get onto {saved} and apply the answer to those threads. Then
reply on each parked thread with the outcome, and
resolve it unless the reply asks the reviewer a follow-up question.
{resume_finish}`

// renderJobPrompt adds the park or resume steps of a review job to
// body, or returns body unchanged.
func renderJobPrompt(body string, cfg config.Config, req Request, pr scan.PR) string {
	j := req.Job
	if j == nil || promptKind(pr, req) != KindComment {
		return body
	}
	var tmpl string
	switch {
	case j.Answer != "":
		tmpl = resumePrompt
	case cfg.DispatchParkMode == "on":
		tmpl = parkPrompt
	default:
		return body
	}
	out := strings.NewReplacer(
		"{job}", j.ID,
		"{runner}", j.Runner,
		"{ask_to}", cfg.DispatchAskTo,
		"{saved}", j.Saved,
		"{items}", parkItems(pr, j.Items),
		"{park_push}", parkPushClause(cfg),
		"{resume_finish}", resumeFinishClause(cfg),
	).Replace(tmpl)
	// The answer is the manager's or the user's own text: substituted
	// last, so it stays verbatim.
	return body + "\n\n" + strings.ReplaceAll(out, "{answer}", j.Answer)
}

func parkPushClause(cfg config.Config) string {
	if cfg.DispatchCatchup == "merge" {
		return "push normally (do not rebase or force-push)"
	}
	return "push"
}

func resumeFinishClause(cfg config.Config) string {
	push := "Then push"
	if cfg.DispatchCatchup == "merge" {
		push = "Then push normally (do not rebase or force-push)"
	}
	if cfg.DispatchRerequest == "off" {
		return push + ". Do not request reviewers; another tool handles reviewer requests."
	}
	return push + ", and re-request review as above."
}

// parkItems lists the review comments by id, only those in items when
// it is not empty.
func parkItems(pr scan.PR, items []string) string {
	var lines []string
	for _, c := range pr.BlockingComments {
		if c.CommentID == "" || (len(items) > 0 && !slices.Contains(items, c.CommentID)) {
			continue
		}
		lines = append(lines, "   - "+c.CommentID+": "+c.URL)
	}
	for _, id := range items {
		if !slices.ContainsFunc(pr.BlockingComments, func(c scan.Comment) bool { return c.CommentID == id }) {
			lines = append(lines, "   - "+id)
		}
	}
	return strings.Join(lines, "\n")
}
