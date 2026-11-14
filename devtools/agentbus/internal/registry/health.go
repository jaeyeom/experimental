package registry

import (
	"context"
	"fmt"
	"slices"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// Self-test retry: an adapter whose self-test fails is tested again
// after healthBackoff, and each further failure doubles the wait up to
// healthBackoffMax. A daemon that starts before the herdr server thus
// picks herdr up without a restart. Each test runs herdr afresh, so
// the binary and its socket are looked up again every time.
const (
	healthBackoff    = time.Second
	healthBackoffMax = 30 * time.Second
)

// health is each adapter's self-test result, shared by the registry
// and the mailbox. It has its own lock, so the mailbox can ask without
// the registry's.
type health struct {
	mu    sync.Mutex
	state map[string]*adapterHealth
}

// adapterHealth is one adapter's last self-test. A zero value is
// untested. err is empty after a pass; after a failure, next is when
// the adapter is tested again and wait is how long that was.
type adapterHealth struct {
	tested bool
	err    string
	wait   time.Duration
	next   time.Time
}

// usable returns the adapters whose last self-test passed. It tests
// first those never tested, and failed ones whose wait has ended. The
// first failure, or one after a pass, logs one adapter_error; a pass
// after a failure logs one adapter_reachable. Retries that keep failing
// log nothing.
func (h *health) usable(ctx context.Context, ads []adapter.Adapter, now func() time.Time, logEvent func(eventlog.Event) error) ([]adapter.Adapter, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.state == nil {
		h.state = map[string]*adapterHealth{}
	}
	out := make([]adapter.Adapter, 0, len(ads))
	for _, ad := range ads {
		st := h.state[ad.Name()]
		if st == nil {
			st = &adapterHealth{}
			h.state[ad.Name()] = st
		}
		if at := now(); !st.tested || (st.err != "" && !at.Before(st.next)) {
			if ev, changed := st.test(ctx, ad, at); changed {
				if err := logEvent(ev); err != nil {
					return nil, err
				}
			}
		}
		if st.err == "" {
			out = append(out, ad)
		}
	}
	return out, nil
}

// test runs ad's self-test at the time at and records it. changed is
// true, with the event to log, when the adapter went down or came up.
func (st *adapterHealth) test(ctx context.Context, ad adapter.Adapter, at time.Time) (eventlog.Event, bool) {
	err := ad.SelfTest(ctx)
	name := ad.Name()
	if err == nil {
		wasDown := st.err != ""
		*st = adapterHealth{tested: true}
		return eventlog.Event{
			Event:  eventlog.EventAdapterReachable,
			Actor:  "agentbus",
			Name:   name,
			Detail: name + " reachable",
		}, wasDown
	}
	wasUp := st.err == ""
	st.tested = true
	st.err = err.Error()
	st.wait = min(max(2*st.wait, healthBackoff), healthBackoffMax)
	st.next = at.Add(st.wait)
	return eventlog.Event{
		Event:  eventlog.EventAdapterError,
		Actor:  "agentbus",
		Name:   name,
		Detail: name + " unreachable",
		Reason: st.err,
	}, wasUp
}

// TransportProblems reads the event log in stateDir and returns one
// line per adapter the running daemon last found unreachable, such as
// "herdr transport unreachable: <error>". Events before the daemon's
// last start do not count.
func TransportProblems(stateDir string) ([]string, error) {
	lines, err := eventlog.Open(stateDir).Read(eventlog.Filter{})
	if err != nil {
		return nil, fmt.Errorf("read event log: %w", err)
	}
	down := map[string]string{}
	var order []string
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			continue
		}
		switch {
		case ev.Event == eventlog.EventStart:
			down = map[string]string{}
		case ev.Event == eventlog.EventAdapterError && ev.Detail == ev.Name+" unreachable":
			if !slices.Contains(order, ev.Name) {
				order = append(order, ev.Name)
			}
			down[ev.Name] = ev.Reason
		case ev.Event == eventlog.EventAdapterReachable:
			delete(down, ev.Name)
		}
	}
	var out []string
	for _, name := range order {
		if reason, ok := down[name]; ok {
			out = append(out, name+" transport unreachable: "+reason)
		}
	}
	return out, nil
}
