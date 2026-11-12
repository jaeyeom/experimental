package registry

import (
	"context"
	"maps"
	"slices"
	"time"
)

// DefaultListMaxAge is how old the list liveness cache may get before a
// cached list starts a background refresh.
const DefaultListMaxAge = 15 * time.Second

// observation is one handle's list liveness and when it was seen.
type observation struct {
	word string
	at   time.Time
}

// ListCached returns the same rows as List, with liveness from the
// cache: inventory snapshots, delivery results, and earlier probes. It
// makes no adapter calls. When the last full probe is older than
// ListMaxAge it starts one background refresh under ctx, and still
// answers from the cache. A name never observed is unreachable.
func (r *Registry) ListCached(ctx context.Context) []Row {
	r.mu.Lock()
	defer r.mu.Unlock()
	if !r.liveBusy && r.now().Sub(r.liveAt) > r.listMaxAge() {
		r.liveBusy = true
		go r.refreshLive(ctx)
	}
	now := r.now()
	return r.registeredRows(func(handle string) (string, time.Time) {
		if r.hibernatedLocked(handle) {
			return livenessHibernated, now
		}
		if obs, ok := r.live[handle]; ok {
			return obs.word, obs.at
		}
		return "unreachable", time.Time{}
	})
}

// ObserveLiveness records a liveness word for handle seen outside a
// probe, such as a delivery result.
func (r *Registry) ObserveLiveness(handle, word string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.noteLive(handle, word, r.now())
}

func (r *Registry) listMaxAge() time.Duration {
	if r.ListMaxAge > 0 {
		return r.ListMaxAge
	}
	return DefaultListMaxAge
}

// noteLive stores one observation. The caller holds r.mu.
func (r *Registry) noteLive(handle, word string, at time.Time) {
	if r.live == nil {
		r.live = map[string]observation{}
	}
	r.live[handle] = observation{word: word, at: at}
}

// refreshLive probes every registered handle without holding r.mu, so a
// slow or unreachable machine does not hold up lists while it answers.
// An unreachable machine marks only its own handles unreachable.
func (r *Registry) refreshLive(ctx context.Context) {
	r.mu.Lock()
	ads, err := r.usable(ctx)
	handles := make([]string, 0, len(r.names))
	for _, rec := range r.names {
		if !r.hibernatedLocked(rec.Handle) && !slices.Contains(handles, rec.Handle) {
			handles = append(handles, rec.Handle)
		}
	}
	snaps := maps.Clone(r.snaps)
	r.mu.Unlock()

	got := make(map[string]string, len(handles))
	for _, handle := range handles {
		if err != nil {
			got[handle] = "unreachable"
			continue
		}
		got[handle] = probeLiveness(ctx, ads, handle, snaps)
	}

	r.mu.Lock()
	defer r.mu.Unlock()
	now := r.now()
	for handle, word := range got {
		r.noteLive(handle, word, now)
	}
	r.liveAt = now
	r.liveBusy = false
}
