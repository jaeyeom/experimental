package jobs

import (
	"context"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// Option keys of the ask gate's decide item.
const (
	OptionApprove = "approve"
	OptionReject  = "reject"
)

// Decider files, reads, and closes the ask gate's decide items. Ask
// names no pane: the answer changes the queue only and never prompts
// an agent.
type Decider interface {
	Ask(ctx context.Context, question string) (string, error)
	Show(ctx context.Context, id string) (decide.Item, error)
	Done(ctx context.Context, id, outcome string) error
}

// RunGate runs the ask gate for every gated job. A job with no decide item
// gets one. A job whose item has the user's own answer to every
// question goes to ready; a reject from the user fails it. Anything
// else leaves it gated. Without a Decider every gated job waits.
// A job parked on an agent's decide item, or its gated child, is
// resumed once the user answers that item. Decide runs with the store
// unlocked.
func RunGate(ctx context.Context, s *Store, d Decider) {
	if d == nil {
		return
	}
	for _, j := range s.ParkedOnItems() {
		if ctx.Err() != nil {
			return
		}
		if it, err := d.Show(ctx, j.ParkedOn); err == nil && answeredByUser(it) {
			// One that moved meanwhile is left as it is.
			_ = s.Resume(j.ID, j.ParkedOn, "the user answered decide "+j.ParkedOn)
		}
	}
	for _, j := range s.Gated() {
		if ctx.Err() != nil {
			return
		}
		if j.ParkedOn != "" {
			continue
		}
		if j.Decide == "" {
			askGate(ctx, s, d, j)
			continue
		}
		it, err := d.Show(ctx, j.Decide)
		if err != nil {
			// Read again on the next pass.
			continue
		}
		v := gateVerdict(it)
		if v == waiting {
			continue
		}
		ok := v == approved
		note := "approved in decide " + j.Decide
		outcome := "approved: agentbus job " + j.ID + " is ready to run"
		if !ok {
			note = rejectNote(j.Decide, it)
			outcome = "rejected: agentbus job " + j.ID + " failed"
		}
		if s.Ungate(j.ID, j.Decide, ok, note) != nil {
			continue
		}
		// An item left open only stays in the user's inbox.
		_ = d.Done(ctx, j.Decide, outcome)
	}
}

func askGate(ctx context.Context, s *Store, d Decider, j Job) {
	id, err := d.Ask(ctx, gateText(j))
	if err != nil {
		// Ask again on the next pass.
		return
	}
	if s.SetDecide(j.ID, id) != nil {
		_ = d.Done(ctx, id, "agentbus job "+j.ID+" no longer waits on this item")
	}
}

func gateText(j Job) string {
	what := j.Kind + " " + j.Subject
	if j.Head != "" {
		what += " at " + j.Head
	}
	if n := len(j.History); n > 0 && j.History[n-1].From == StateRunning {
		last := j.History[n-1]
		return fmt.Sprintf("agentbus job %s (%s) waits for you: runner %s stopped it before it acted: %s. "+
			"The runner never answers a dialog. Settle it yourself, then approve to let a runner take the job again, or reject to fail it.",
			j.ID, what, last.Actor, last.Note)
	}
	return fmt.Sprintf("agentbus job %s (%s) waits for you: the jobs policy asks before a %s job runs. "+
		"Approve to let a runner take it, or reject to fail it. A note you write before rejecting is kept with the job.",
		j.ID, what, j.Kind)
}

type verdict int

const (
	waiting verdict = iota
	approved
	rejected
)

// gateVerdict reads the user's answers in a gate item. Each message
// with options, the ask and any follow-up an agent asked with decide
// reply, is a question. The job is approved only when every question
// has an option pick the user made in the inbox after it and before the
// next question, and the item's answer is the user's own inbox answer
// to the last one. A reject pick by the user anywhere rejects it.
// Agents, and the user's own command line, are never counted.
func gateVerdict(it decide.Item) verdict {
	questions, answered := 0, 0
	open := false
	var last string
	for _, m := range it.Messages {
		if len(m.Options) > 0 {
			questions++
			open = true
			continue
		}
		if !m.HumanUI() || m.OptionKey == "" {
			continue
		}
		if m.OptionKey == OptionReject {
			return rejected
		}
		if open {
			answered++
			open = false
		}
		last = m.OptionKey
	}
	if questions == 0 || answered < questions {
		return waiting
	}
	if a := it.Answer; !a.HumanUI() || a.Key != last {
		return waiting
	}
	return approved
}

// answeredByUser reports whether the user answered an agent's item in
// the inbox: the answer, and the last message the user wrote, came from
// the user there, not from an agent writing as the user.
func answeredByUser(it decide.Item) bool {
	if !it.Answer.HumanUI() {
		return false
	}
	for i := len(it.Messages) - 1; i >= 0; i-- {
		if m := it.Messages[i]; m.Author == decide.Human {
			return m.HumanUI()
		}
	}
	return false
}

// rejectNote is the user's own text in the item, or that it was
// rejected.
func rejectNote(id string, it decide.Item) string {
	var notes []string
	for _, m := range it.Messages {
		if m.HumanUI() && m.OptionKey == "" && strings.TrimSpace(m.Text) != "" {
			notes = append(notes, strings.TrimSpace(m.Text))
		}
	}
	note := "rejected in decide " + id
	if len(notes) > 0 {
		note += ": " + strings.Join(notes, "; ")
	}
	return note
}
