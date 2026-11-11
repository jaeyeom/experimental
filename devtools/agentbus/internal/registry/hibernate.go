package registry

import (
	"context"
	"fmt"
	"maps"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
)

// livenessHibernated is the list and search word for a hibernated
// handle. Claims treat it as alive: the name stays put.
const livenessHibernated = "hibernated"

// BeginHibernation stores rec as stopping, with the names its handle
// holds now. It replaces an older record for the handle.
func (r *Registry) BeginHibernation(rec hibernate.Record) (hibernate.Record, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	rec.Names = r.heldBy(rec.Handle)
	rec.Status = hibernate.StatusStopping
	rec.Reason = ""
	r.hibernated[rec.Handle] = rec
	if err := r.save(); err != nil {
		return hibernate.Record{}, err
	}
	return rec, nil
}

// EndHibernation marks handle's record hibernated or stop-failed.
func (r *Registry) EndHibernation(handle, status, reason string) (hibernate.Record, error) {
	if status != hibernate.StatusHibernated && status != hibernate.StatusStopFailed {
		return hibernate.Record{}, fmt.Errorf("a hibernation ends %s or %s, not %q", hibernate.StatusHibernated, hibernate.StatusStopFailed, status)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	rec, ok := r.hibernated[handle]
	if !ok {
		return hibernate.Record{}, fmt.Errorf("%s has no hibernation record", handle)
	}
	rec.Status = status
	rec.Reason = reason
	r.hibernated[handle] = rec
	if err := r.save(); err != nil {
		return hibernate.Record{}, err
	}
	return rec, nil
}

// Hibernations are the hibernation records, by handle.
func (r *Registry) Hibernations() []hibernate.Record {
	r.mu.Lock()
	defer r.mu.Unlock()
	out := make([]hibernate.Record, 0, len(r.hibernated))
	for _, handle := range slices.Sorted(maps.Keys(r.hibernated)) {
		out = append(out, r.hibernated[handle])
	}
	return out
}

// Hibernated reports whether handle's agent was stopped by hibernate.
func (r *Registry) Hibernated(handle string) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.hibernatedLocked(handle)
}

func (r *Registry) hibernatedLocked(handle string) bool {
	rec, ok := r.hibernated[handle]
	return ok && rec.Status == hibernate.StatusHibernated
}

// Names are the registered names by handle.
func (r *Registry) Names() map[string][]string {
	r.mu.Lock()
	defer r.mu.Unlock()
	out := map[string][]string{}
	for name, rec := range r.names {
		out[rec.Handle] = append(out[rec.Handle], name)
	}
	for _, names := range out {
		slices.Sort(names)
	}
	return out
}

// claimLiveness is liveness for a claim: a handle with a hibernation
// record, in any status, keeps its names. The caller holds r.mu.
func (r *Registry) claimLiveness(ctx context.Context, handle string) string {
	if _, ok := r.hibernated[handle]; ok {
		return livenessHibernated
	}
	return r.liveness(ctx, handle)
}

// hibernatedRows are search rows for hibernated handles the inventory
// no longer lists, one per name. The caller holds r.mu.
func (r *Registry) hibernatedRows(query string) []Row {
	var rows []Row
	for _, handle := range slices.Sorted(maps.Keys(r.hibernated)) {
		if !r.hibernatedLocked(handle) {
			continue
		}
		if _, listed := r.agentByHandle(handle); listed {
			continue
		}
		scheme, _, _, _ := splitHandle(handle)
		for _, binding := range r.namesFor(handle) {
			rows = appendMatch(rows, query, Row{
				Name: binding.Name, When: binding.When, Liveness: livenessHibernated,
				Transport: scheme, Handle: handle,
			})
		}
	}
	return rows
}

// seenState is the state an agent was last seen in, since when, and
// herdr's state_change_seq then.
type seenState struct {
	status string
	seq    int64
	since  time.Time
}

// Seen is how long each agent in the inventory has been in its state.
func (r *Registry) Seen() map[string]hibernate.Seen {
	r.mu.Lock()
	defer r.mu.Unlock()
	now := r.now()
	out := make(map[string]hibernate.Seen, len(r.seen))
	for handle, s := range r.seen {
		out[handle] = hibernate.Seen{Status: s.status, For: now.Sub(s.since)}
	}
	return out
}

func (r *Registry) now() time.Time {
	if r.Now != nil {
		return r.Now()
	}
	return time.Now()
}

// track updates seen from every inventory snapshot. An agent keeps its
// since while its status and state_change_seq stay, and across done to
// idle, which is the user looking at a finished turn. Any other change
// starts it over. An agent no longer listed is dropped. The caller holds
// r.mu.
func (r *Registry) track() {
	now := r.now()
	next := map[string]seenState{}
	for _, agents := range r.snaps {
		for _, a := range agents {
			if a.Handle == "" || strings.TrimSpace(a.Status) == "" {
				continue
			}
			s := seenState{status: a.Status, seq: a.StateSeq, since: now}
			if old, ok := r.seen[a.Handle]; ok && keepsSince(old, s) {
				s.since = old.since
			}
			next[a.Handle] = s
		}
	}
	r.seen = next
}

func keepsSince(old, cur seenState) bool {
	if old.status == cur.status && old.seq == cur.seq {
		return true
	}
	return old.status == "done" && cur.status == "idle"
}
