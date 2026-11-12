package hibernate

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

// saveInterval is how often SaveEvery writes the tracker.
const saveInterval = time.Minute

// LastTurnFunc is the time of an agent's last turn, when its kind
// records one. It is read-only and reports false when it cannot tell.
type LastTurnFunc func(adapter.Agent) (time.Time, bool)

// IdleTracker is how long each agent has been in its state, from
// inventory snapshots. The daemon keeps one for its Seen, and so does
// the policy runner on a hub-mode client, where no daemon runs. Both
// save it to their state directory so a restart keeps the idle times.
// The zero value is ready to use; it is not safe for concurrent use.
type IdleTracker struct {
	// LastTurn, when not nil, bounds since for an idle or done agent
	// the tracker has no entry for.
	LastTurn LastTurnFunc
	seen     map[string]seenState
	// loaded are the entries Load read, used by the next snapshot only.
	loaded map[string]seenState
	// started is set by the first snapshot. An agent in it was already
	// in its state before, so its since is a lower bound.
	started bool
	saved   time.Time
}

// seenState is the state an agent was last seen in, since when, and
// herdr's state_change_seq then. lowerBound is set when the agent was
// already in the state when the tracker first saw it.
type seenState struct {
	status     string
	seq        int64
	since      time.Time
	lowerBound bool
}

// Observe takes one snapshot of every agent at now. An agent keeps its
// since while its status and state_change_seq stay, and across done to
// idle, which is the user looking at a finished turn. Any other change
// starts it over. An agent no longer listed is dropped. An agent with
// no entry takes a loaded one with the same status and seq, else the
// time of its last turn when it is idle or done and LastTurn knows it.
func (t *IdleTracker) Observe(now time.Time, agents []adapter.Agent) {
	first := !t.started
	t.started = true
	next := map[string]seenState{}
	for _, a := range agents {
		if a.Handle == "" || strings.TrimSpace(a.Status) == "" {
			continue
		}
		s := seenState{status: a.Status, seq: a.StateSeq, since: now, lowerBound: first}
		if old, ok := t.seen[a.Handle]; ok {
			if keepsSince(old, s) {
				s.since, s.lowerBound = old.since, old.lowerBound
			}
		} else if old, ok := t.loaded[a.Handle]; ok && old.status == s.status && old.seq == s.seq {
			s.since, s.lowerBound = old.since, old.lowerBound
		} else if at, ok := t.lastTurn(a); ok && at.Before(now) {
			s.since, s.lowerBound = at, false
		}
		next[a.Handle] = s
	}
	t.seen = next
	t.loaded = nil
}

func (t *IdleTracker) lastTurn(a adapter.Agent) (time.Time, bool) {
	if t.LastTurn == nil || (a.Status != "idle" && a.Status != "done") {
		return time.Time{}, false
	}
	return t.LastTurn(a)
}

// Seen is how long, at now, each agent has been in its state.
func (t *IdleTracker) Seen(now time.Time) map[string]Seen {
	out := make(map[string]Seen, len(t.seen))
	for handle, s := range t.seen {
		out[handle] = s.at(now)
	}
	return out
}

func (s seenState) at(now time.Time) Seen {
	return Seen{Status: s.status, For: now.Sub(s.since), LowerBound: s.lowerBound}
}

func keepsSince(old, cur seenState) bool {
	if old.status == cur.status && old.seq == cur.seq {
		return true
	}
	return old.status == "done" && cur.status == "idle"
}

// savedState is one agent in the tracker file.
type savedState struct {
	Status     string    `json:"status"`
	Seq        int64     `json:"state_change_seq"` //nolint:tagliatelle // herdr's name
	Since      time.Time `json:"since"`
	LowerBound bool      `json:"lower_bound,omitempty"` //nolint:tagliatelle // state file shape
}

type trackerFile struct {
	Agents map[string]savedState `json:"agents"`
}

// Save writes the tracker to path, replacing it whole.
func (t *IdleTracker) Save(path string, now time.Time) error {
	doc := trackerFile{Agents: make(map[string]savedState, len(t.seen))}
	for handle, s := range t.seen {
		doc.Agents[handle] = savedState{Status: s.status, Seq: s.seq, Since: s.since, LowerBound: s.lowerBound}
	}
	data, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode idle times: %w", err)
	}
	tmp := filepath.Join(filepath.Dir(path), "."+filepath.Base(path)+".tmp")
	if err := os.WriteFile(tmp, append(data, '\n'), 0o600); err != nil {
		return fmt.Errorf("write idle times: %w", err)
	}
	if err := os.Rename(tmp, path); err != nil {
		return fmt.Errorf("replace idle times: %w", err)
	}
	t.saved = now
	return nil
}

// SaveEvery saves the tracker to path unless it saved less than a
// minute before now.
func (t *IdleTracker) SaveEvery(path string, now time.Time) error {
	if !t.saved.IsZero() && now.Sub(t.saved) < saveInterval {
		return nil
	}
	return t.Save(path, now)
}

// Load reads the entries Save wrote to path for the next snapshot,
// which keeps an entry's since only when herdr still reports the same
// handle, status, and state_change_seq. A missing file loads nothing.
func (t *IdleTracker) Load(path string) error {
	loaded, err := readTracker(path)
	if err != nil {
		return err
	}
	t.loaded = loaded
	return nil
}

func readTracker(path string) (map[string]seenState, error) {
	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read idle times: %w", err)
	}
	var doc trackerFile
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("decode idle times %s: %w", path, err)
	}
	out := make(map[string]seenState, len(doc.Agents))
	for handle, s := range doc.Agents {
		out[handle] = seenState{status: s.Status, seq: s.Seq, since: s.Since, lowerBound: s.LowerBound}
	}
	return out, nil
}

// LoadSeen is how long, at now, each of agents has been in its state,
// by the tracker another process saved to path: only the agents herdr
// still reports with the saved status and state_change_seq. A missing
// file gives none.
func LoadSeen(path string, now time.Time, agents []adapter.Agent) (map[string]Seen, error) {
	saved, err := readTracker(path)
	if err != nil {
		return nil, err
	}
	out := map[string]Seen{}
	for _, a := range agents {
		if s, ok := saved[a.Handle]; ok && s.status == a.Status && s.seq == a.StateSeq {
			out[a.Handle] = s.at(now)
		}
	}
	return out, nil
}
