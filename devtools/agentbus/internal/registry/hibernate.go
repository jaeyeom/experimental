package registry

import (
	"context"
	"fmt"
	"maps"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
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

// FailWake marks handle's record wake-failed with reason, counts the
// failure, and sets when the next wake may be tried.
func (r *Registry) FailWake(handle, reason string, retryAt time.Time) (hibernate.Record, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	rec, ok := r.hibernated[handle]
	if !ok {
		return hibernate.Record{}, fmt.Errorf("%s has no hibernation record", handle)
	}
	rec.Status = hibernate.StatusWakeFailed
	rec.Reason = reason
	rec.Failures++
	rec.RetryAt = retryAt
	r.hibernated[handle] = rec
	if err := r.save(); err != nil {
		return hibernate.Record{}, err
	}
	return rec, nil
}

// ClearHibernation drops handle's record: the agent woke, or its pane
// was reused. Its names are active again. No record is not an error.
func (r *Registry) ClearHibernation(handle string) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if _, ok := r.hibernated[handle]; !ok {
		return nil
	}
	delete(r.hibernated, handle)
	return r.save()
}

// Hibernation is handle's record.
func (r *Registry) Hibernation(handle string) (hibernate.Record, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	rec, ok := r.hibernated[handle]
	return rec, ok
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

// HibernationOfPane is the handle of the hibernation record for pane on
// machine, for a caller that holds a pane id rather than a handle.
func (r *Registry) HibernationOfPane(machine, pane string) (string, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for _, handle := range slices.Sorted(maps.Keys(r.hibernated)) {
		_, rest, _ := strings.Cut(handle, ":")
		parts := strings.SplitN(rest, "/", 3)
		if len(parts) == 3 && parts[0] == machine && parts[2] == pane {
			return handle, true
		}
	}
	return "", false
}

// Hibernated reports whether handle's agent was stopped by hibernate.
func (r *Registry) Hibernated(handle string) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.hibernatedLocked(handle)
}

func (r *Registry) hibernatedLocked(handle string) bool {
	rec, ok := r.hibernated[handle]
	return ok && hibernate.Asleep(rec.Status)
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

// Seen is how long each agent in the inventory has been in its state.
func (r *Registry) Seen() map[string]hibernate.Seen {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.seen.Seen(r.now())
}

func (r *Registry) now() time.Time {
	if r.Now != nil {
		return r.Now()
	}
	return time.Now()
}

// track updates seen from every inventory snapshot, and saves it at
// most once a minute. The caller holds r.mu.
func (r *Registry) track() {
	var all []adapter.Agent
	for _, agents := range r.snaps {
		all = append(all, agents...)
	}
	now := r.now()
	r.seen.LastTurn = r.LastTurn
	r.seen.Observe(now, all)
	// Saving is best effort: a failed save only loses idle times on a
	// restart, and the next poll tries again.
	_ = r.seen.SaveEvery(filepath.Join(r.dir, seenFile), now)
}

// saveSeen saves the idle tracker, as on shutdown.
func (r *Registry) saveSeen() {
	r.mu.Lock()
	defer r.mu.Unlock()
	if !r.haveInventory {
		return
	}
	_ = r.seen.Save(filepath.Join(r.dir, seenFile), r.now())
}
