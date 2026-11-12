package hibernate

import (
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

// IdleTracker is how long each agent has been in its state, from
// inventory snapshots. The daemon keeps one for its Seen, and so does
// the policy runner on a hub-mode client, where no daemon runs. The
// zero value is ready to use; it is not safe for concurrent use.
type IdleTracker struct {
	seen map[string]seenState
}

// seenState is the state an agent was last seen in, since when, and
// herdr's state_change_seq then.
type seenState struct {
	status string
	seq    int64
	since  time.Time
}

// Observe takes one snapshot of every agent at now. An agent keeps its
// since while its status and state_change_seq stay, and across done to
// idle, which is the user looking at a finished turn. Any other change
// starts it over. An agent no longer listed is dropped.
func (t *IdleTracker) Observe(now time.Time, agents []adapter.Agent) {
	next := map[string]seenState{}
	for _, a := range agents {
		if a.Handle == "" || strings.TrimSpace(a.Status) == "" {
			continue
		}
		s := seenState{status: a.Status, seq: a.StateSeq, since: now}
		if old, ok := t.seen[a.Handle]; ok && keepsSince(old, s) {
			s.since = old.since
		}
		next[a.Handle] = s
	}
	t.seen = next
}

// Seen is how long, at now, each agent has been in its state.
func (t *IdleTracker) Seen(now time.Time) map[string]Seen {
	out := make(map[string]Seen, len(t.seen))
	for handle, s := range t.seen {
		out[handle] = Seen{Status: s.status, For: now.Sub(s.since)}
	}
	return out
}

func keepsSince(old, cur seenState) bool {
	if old.status == cur.status && old.seq == cur.seq {
		return true
	}
	return old.status == "done" && cur.status == "idle"
}
