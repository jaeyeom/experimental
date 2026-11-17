package jobs

import (
	"cmp"
	"context"
	"fmt"
	"maps"
	"strings"
	"time"
)

// DefaultStatusInterval is the least time between two publishes of the
// status view when the jobs config names none.
const DefaultStatusInterval = time.Minute

// StatusConfig is how often the daemon may publish the status view.
type StatusConfig struct {
	// Interval is the least time between two publishes.
	Interval time.Duration
}

type statusFile struct {
	Interval string `json:"interval"`
}

func statusFromFile(f statusFile) (StatusConfig, error) {
	var c StatusConfig
	if f.Interval != "" {
		d, err := time.ParseDuration(f.Interval)
		if err != nil || d <= 0 {
			return StatusConfig{}, fmt.Errorf("status: interval %q: want a positive duration such as \"1m\"", f.Interval)
		}
		c.Interval = d
	}
	return c, nil
}

// StatusInterval is the least time between two publishes of the status
// view.
func (c Config) StatusInterval() time.Duration {
	if c.Status.Interval <= 0 {
		return DefaultStatusInterval
	}
	return c.Status.Interval
}

// StatusStates are the states the status view counts. Queued counts
// ready jobs too: a job leaves queued as soon as it passes its gate,
// and ready is where it waits for a runner.
var StatusStates = []State{StateQueued, StateRunning, StateGated, StateParked, StateUnknown}

// LeaseView is one checkout lease in the status view. Status takes
// Checkout as the lease key, <machine>:<path>, and reports the path.
type LeaseView struct {
	Checkout string `json:"checkout"`
	State    string `json:"state"`
	Holder   string `json:"holder,omitempty"`
}

// StateCount is the jobs of one machine in one state, and when the one
// that has been in it longest entered it.
type StateCount struct {
	Count  int       `json:"count"`
	Oldest time.Time `json:"oldest,omitzero"`
}

// MachineStatus is one machine's leases and job counts. A job is on the
// machine of its lease, else of its holding runner, else the machine
// it needs, which may be "any".
type MachineStatus struct {
	Machine string               `json:"machine"`
	Leases  []LeaseView          `json:"leases"`
	Jobs    map[State]StateCount `json:"jobs"`
}

// StatusView is the `jobs status --json` shape: every machine with a
// lease that is not free or a job in one of StatusStates, by name.
type StatusView struct {
	At       time.Time       `json:"at"`
	Machines []MachineStatus `json:"machines"`
}

// statusBucket is the StatusStates entry state counts in, or empty.
func statusBucket(state State) State {
	switch state {
	case StateReady, StateQueued:
		return StateQueued
	case StateRunning, StateGated, StateParked, StateUnknown:
		return state
	default:
		return ""
	}
}

// Status is the status view at now from the queue and leases, the
// daemon's lease table.
func (s *Store) Status(leases []LeaseView) StatusView {
	s.mu.Lock()
	defer s.mu.Unlock()
	machines := map[string]*MachineStatus{}
	get := func(name string) *MachineStatus {
		m := machines[name]
		if m == nil {
			m = &MachineStatus{Machine: name, Leases: []LeaseView{}, Jobs: map[State]StateCount{}}
			for _, st := range StatusStates {
				m.Jobs[st] = StateCount{}
			}
			machines[name] = m
		}
		return m
	}
	for _, l := range leases {
		machine, path, ok := strings.Cut(l.Checkout, ":")
		if !ok || l.State == "free" {
			continue
		}
		m := get(machine)
		m.Leases = append(m.Leases, LeaseView{Checkout: path, State: l.State, Holder: l.Holder})
	}
	for _, j := range s.doc.Jobs {
		bucket := statusBucket(j.State)
		if bucket == "" {
			continue
		}
		m := get(s.jobMachine(j))
		c := m.Jobs[bucket]
		since := j.entered(j.State)
		if c.Count == 0 || since.Before(c.Oldest) {
			c.Oldest = since
		}
		c.Count++
		m.Jobs[bucket] = c
	}
	v := StatusView{At: s.now().UTC(), Machines: []MachineStatus{}}
	for _, name := range sortedKeys(machines) {
		v.Machines = append(v.Machines, *machines[name])
	}
	return v
}

// jobMachine is the machine j counts on. The caller holds s.mu.
func (s *Store) jobMachine(j *Job) string {
	if machine, _, ok := strings.Cut(j.Lease, ":"); ok {
		return machine
	}
	if j.State == StateRunning || j.State == StateUnknown {
		if r, ok := s.runners[j.Holder]; ok {
			return cmp.Or(r.machine, "local")
		}
	}
	return j.Needs.Machine
}

// StatusRow is one machine's row on the decide status board.
type StatusRow struct {
	Name  string
	State string
	Text  string
}

// Row is m as a decide status agent row named jobs@<machine>: blocked
// while a job waits on the user (gated, parked, or unknown), working
// while one is queued or running, else idle. The text gives when the
// oldest job entered each state, not its age, so it changes only when
// the queue does.
func (m MachineStatus) Row() StatusRow {
	row := StatusRow{Name: "jobs@" + m.Machine, State: "idle"}
	var counts, leases []string
	for _, st := range StatusStates {
		c := m.Jobs[st]
		if c.Count == 0 {
			continue
		}
		counts = append(counts, fmt.Sprintf("%s %d since %s", st, c.Count, stamp(c.Oldest)))
		switch {
		case st == StateGated || st == StateParked || st == StateUnknown:
			row.State = "blocked"
		case row.State == "idle":
			row.State = "working"
		}
	}
	for _, l := range m.Leases {
		leases = append(leases, fmt.Sprintf("lease %s %s by %s", l.Checkout, l.State, cmp.Or(l.Holder, "nobody")))
	}
	row.Text = strings.Join(nonEmpty(strings.Join(counts, ", "), strings.Join(leases, ", ")), "; ")
	return row
}

func nonEmpty(parts ...string) []string {
	var out []string
	for _, p := range parts {
		if p != "" {
			out = append(out, p)
		}
	}
	return out
}

// StatusSink is where the status view is published, such as the decide
// status board.
type StatusSink interface {
	Set(ctx context.Context, name, state, text string) error
	Clear(ctx context.Context, name string) error
}

// StatusPublisher publishes the status view's rows to a sink, only when
// they change and at most once per interval. It is not safe for
// concurrent use.
type StatusPublisher struct {
	sink     StatusSink
	interval time.Duration
	// last are the rows the sink has, by name.
	last map[string]StatusRow
	at   time.Time
}

// NewStatusPublisher returns a publisher to sink that publishes at most
// once per interval.
func NewStatusPublisher(sink StatusSink, interval time.Duration) *StatusPublisher {
	return &StatusPublisher{sink: sink, interval: interval, last: map[string]StatusRow{}}
}

// Publish sets the rows of v that changed since the last publish and
// clears the rows of machines no longer in v. It does nothing when no
// row changed, or when the last publish was less than the interval
// before v.At; a change it skips, or a row the sink refused to set,
// goes out on a later call.
func (p *StatusPublisher) Publish(ctx context.Context, v StatusView) {
	rows := map[string]StatusRow{}
	for _, m := range v.Machines {
		r := m.Row()
		rows[r.Name] = r
	}
	if maps.Equal(rows, p.last) || (!p.at.IsZero() && v.At.Sub(p.at) < p.interval) {
		return
	}
	p.at = v.At
	for _, name := range sortedKeys(rows) {
		r := rows[name]
		if p.last[name] == r {
			continue
		}
		if err := p.sink.Set(ctx, r.Name, r.State, r.Text); err == nil {
			p.last[name] = r
		}
	}
	for _, name := range sortedKeys(p.last) {
		if _, ok := rows[name]; ok {
			continue
		}
		// A row that cannot be cleared, such as one the user cleared
		// already, is not tried again.
		_ = p.sink.Clear(ctx, name)
		delete(p.last, name)
	}
}
