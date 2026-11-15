package decide

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"time"
)

const (
	// FollowUpOwnerGone is a stale answer whose owner pane or agent is gone.
	FollowUpOwnerGone = "owner gone"
	// FollowUpOwnerWaiting is a stale answer whose owner agent is blocked
	// on a dialog.
	FollowUpOwnerWaiting = "owner waiting on a dialog"

	actionSend = "send"
	actionGone = "gone"
	actionWait = "wait"
)

// FollowStale follows up answered items the owner has not taken up.
// now is the clock. run looks up herdr; nil runs the herdr on PATH.
// An item is stale when it is still answered, StaleAnswerAfter has
// passed since AnsweredAt, and the owner has not replied, marked it
// read, or finished it. A stale item is marked owner gone or owner
// waiting on a dialog, or re-sent once with the same delivery key.
// A new answer can be re-sent once more.
func (l *Ledger) FollowStale(now time.Time, out io.Writer, run HerdrCall) error {
	if out == nil {
		out = io.Discard
	}
	cfg, err := LoadConfig()
	if err != nil {
		return err
	}
	items, err := l.List()
	if err != nil {
		return err
	}
	var due []Item
	after := cfg.StaleAnswerAfter()
	for i := range items {
		if needsFollow(&items[i], now, after) {
			due = append(due, items[i])
		}
	}
	if len(due) == 0 {
		return nil
	}
	resolver := LocationResolver{Run: run}
	cache := map[string]agentSnap{}
	local := LocalMachine(cfg)
	var first error
	for i := range due {
		if err := l.followOne(context.Background(), &due[i], now, out, resolver, local, cache); err != nil && first == nil {
			first = err
		}
	}
	return first
}

func (l *Ledger) followOne(ctx context.Context, it *Item, now time.Time, out io.Writer, resolver LocationResolver, local string, cache map[string]agentSnap) error {
	switch ownerAction(ctx, resolver, it, local, cache) {
	case actionGone:
		_, err := l.markFollowUp(it.ID, FollowUpOwnerGone)
		return err
	case actionWait:
		_, err := l.markFollowUp(it.ID, FollowUpOwnerWaiting)
		return err
	default:
		return l.resendAnswer(it, now, out)
	}
}

func needsFollow(it *Item, now time.Time, after time.Duration) bool {
	if it == nil || it.State != StateAnswered || it.AnsweredAt.IsZero() || it.FollowUp != "" || resent(it) {
		return false
	}
	if after <= 0 {
		after = DefaultStaleAnswerAfter
	}
	if now.Before(it.AnsweredAt.Add(after)) {
		return false
	}
	if it.ReadAt != nil && !it.ReadAt.Before(it.AnsweredAt) {
		return false
	}
	return !ownerReplied(it)
}

// ownerReplied reports an owner message at or after this answer.
// The ask itself is stored at CreatedAt and is not a reply.
func ownerReplied(it *Item) bool {
	for _, msg := range it.Messages {
		if msg.Author != it.Owner || msg.Time.Before(it.AnsweredAt) {
			continue
		}
		if !msg.Time.After(it.CreatedAt) && msg.Text == it.Question {
			continue
		}
		return true
	}
	return false
}

func resent(it *Item) bool {
	for _, d := range attemptsSinceAnswer(it) {
		if d.Outcome == OutcomeResend {
			return true
		}
	}
	return false
}

func ownerAction(ctx context.Context, r LocationResolver, it *Item, local string, cache map[string]agentSnap) string {
	locs := r.Resolve(ctx, []OwnerLocation{OwnerPane(it, local)})
	if len(locs) == 0 {
		return actionSend
	}
	loc := locs[0]
	if loc.State == LocationClosed {
		return actionGone
	}
	if loc.State != LocationLive {
		return actionSend
	}
	snap := r.agents(ctx, loc.Machine, cache)
	if snap.err != nil {
		return actionSend
	}
	status, found := snap.status(loc.Pane)
	if !found {
		return actionGone
	}
	if status == "blocked" {
		return actionWait
	}
	return actionSend
}

type agentSnap struct {
	byPane map[string]string
	err    error
}

func (s agentSnap) status(pane string) (string, bool) {
	status, ok := s.byPane[pane]
	return status, ok
}

func (r LocationResolver) agents(ctx context.Context, machine string, cache map[string]agentSnap) agentSnap {
	if snap, ok := cache[machine]; ok {
		return snap
	}
	snap := r.readAgents(ctx, machine)
	cache[machine] = snap
	return snap
}

// herdrAgentList is the `herdr agent list` envelope.
type herdrAgentList struct {
	Result struct {
		Agents []struct {
			PaneID string `json:"pane_id"`      //nolint:tagliatelle // herdr wire format
			Status string `json:"agent_status"` //nolint:tagliatelle // herdr wire format
		} `json:"agents"`
	} `json:"result"`
}

func (r LocationResolver) readAgents(ctx context.Context, machine string) agentSnap {
	run := r.Run
	if run == nil {
		run = ExecHerdr
	}
	stdout, err := run(ctx, machine, []string{"agent", "list"})
	if err != nil {
		return agentSnap{err: err}
	}
	var list herdrAgentList
	if err := json.Unmarshal([]byte(stdout), &list); err != nil {
		return agentSnap{err: fmt.Errorf("decode herdr agent list: %w", err)}
	}
	byPane := map[string]string{}
	for _, agent := range list.Result.Agents {
		if agent.PaneID != "" {
			byPane[agent.PaneID] = agent.Status
		}
	}
	return agentSnap{byPane: byPane}
}

func (l *Ledger) markFollowUp(id, mark string) (Item, error) {
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.State != StateAnswered || it.FollowUp != "" {
			updated = *it
			return nil
		}
		it.FollowUp = mark
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

func (l *Ledger) resendAnswer(it *Item, now time.Time, out io.Writer) error {
	cur, err := l.Get(it.ID)
	if err != nil {
		return err
	}
	if cur.State != StateAnswered || cur.FollowUp != "" || resent(&cur) {
		return nil
	}
	target, _, nerr := notifyAttempt(&cur, out)
	msg := ""
	if nerr != nil {
		msg = nerr.Error()
	}
	_, err = l.appendDelivery(cur.ID, Delivery{
		At:      now,
		Target:  target,
		Outcome: OutcomeResend,
		Error:   msg,
	})
	return err
}
