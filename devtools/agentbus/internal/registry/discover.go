package registry

import (
	"context"
	"errors"
	"sort"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// defaultPoll is the builder-chosen inventory refresh interval.
// It does not expire names.
const defaultPoll = 2 * time.Second

// Row is one registered name or one reachable agent.
// An empty Name means the agent is not registered. The bus does not
// invent a logical name from a title or a label.
type Row struct {
	Name      string
	When      string
	Liveness  string
	Transport string
	Handle    string
	Label     string
	Title     string
}

type seen struct {
	transport string
	agent     adapter.Agent
}

// Poll refreshes adapter inventories until ctx is cancelled.
// A non-positive interval uses defaultPoll. A lookup that misses
// refreshes again without waiting for the next tick.
func (r *Registry) Poll(ctx context.Context, interval time.Duration) {
	if interval <= 0 {
		interval = defaultPoll
	}
	r.pollOnce(ctx)
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			r.pollOnce(ctx)
		}
	}
}

// pollOnce reads inventories without holding r.mu, so a slow machine
// does not hold up name lookups while it answers.
func (r *Registry) pollOnce(ctx context.Context) {
	if ctx.Err() != nil {
		return
	}
	r.mu.Lock()
	ads, err := r.usable(ctx)
	r.mu.Unlock()
	if err != nil {
		return
	}
	agents := make([][]adapter.Agent, len(ads))
	errs := make([]error, len(ads))
	for i, ad := range ads {
		agents[i], errs[i] = ad.Inventory(ctx)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	r.initInventory()
	for i, ad := range ads {
		if err := r.storeInventory(ad.Name(), agents[i], errs[i]); err != nil {
			return
		}
	}
	r.haveInventory = true
}

// Warnings returns the inventory warnings from the last refresh, such as
// a configured machine that could not be reached.
func (r *Registry) Warnings() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	names := make([]string, 0, len(r.warnings))
	for name := range r.warnings {
		names = append(names, name)
	}
	sort.Strings(names)
	var out []string
	for _, name := range names {
		out = append(out, r.warnings[name]...)
	}
	return out
}

// List returns registered names with --when, liveness, and transport.
func (r *Registry) List(ctx context.Context) ([]Row, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	refreshed, err := r.ensureInventory(ctx)
	if err != nil {
		return nil, err
	}
	rows := r.registeredRows(ctx)
	if refreshed || !r.missingHandle(rows) {
		return rows, nil
	}
	if err := r.refresh(ctx); err != nil {
		return nil, err
	}
	return r.registeredRows(ctx), nil
}

// Search returns every reachable agent, registered or not.
// query matches name, --when, label, and title. An empty query
// returns every reachable agent. A miss refreshes inventory once.
func (r *Registry) Search(ctx context.Context, query string) ([]Row, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if _, err := r.ensureInventory(ctx); err != nil {
		return nil, err
	}
	rows := r.searchRows(query)
	if strings.TrimSpace(query) == "" || len(rows) > 0 {
		return rows, nil
	}
	if err := r.refresh(ctx); err != nil {
		return nil, err
	}
	return r.searchRows(query), nil
}

func (r *Registry) ensureInventory(ctx context.Context) (bool, error) {
	if r.haveInventory {
		return false, nil
	}
	if err := r.refresh(ctx); err != nil {
		return false, err
	}
	return true, nil
}

func (r *Registry) refresh(ctx context.Context) error {
	ads, err := r.usable(ctx)
	if err != nil {
		return err
	}
	r.initInventory()
	for _, ad := range ads {
		agents, invErr := ad.Inventory(ctx)
		if err := r.storeInventory(ad.Name(), agents, invErr); err != nil {
			return err
		}
	}
	r.haveInventory = true
	return nil
}

func (r *Registry) initInventory() {
	if r.snaps == nil {
		r.snaps = map[string][]adapter.Agent{}
	}
	if r.invBroken == nil {
		r.invBroken = map[string]bool{}
	}
	if r.warnings == nil {
		r.warnings = map[string][]string{}
	}
}

// storeInventory keeps one adapter's agents. An InventoryWarning keeps
// the agents it came with and records its lines. The caller holds r.mu.
func (r *Registry) storeInventory(name string, agents []adapter.Agent, invErr error) error {
	var warn *adapter.InventoryWarning
	switch {
	case errors.As(invErr, &warn):
		r.warnings[name] = append([]string(nil), warn.Lines...)
	case invErr != nil:
		return r.noteInventoryError(name, invErr)
	default:
		delete(r.warnings, name)
	}
	r.invBroken[name] = false
	r.snaps[name] = append([]adapter.Agent(nil), agents...)
	r.track()
	return nil
}

func (r *Registry) noteInventoryError(name string, invErr error) error {
	if r.invBroken[name] {
		return nil
	}
	r.invBroken[name] = true
	return r.logEvent(eventlog.Event{
		Event:  eventlog.EventAdapterError,
		Actor:  "agentbus",
		Name:   name,
		Detail: "inventory refresh failed",
		Reason: invErr.Error(),
	})
}

func (r *Registry) registeredRows(ctx context.Context) []Row {
	names := make([]string, 0, len(r.names))
	for name := range r.names {
		names = append(names, name)
	}
	sort.Strings(names)
	rows := make([]Row, 0, len(names))
	for _, name := range names {
		rec := r.names[name]
		row := Row{
			Name:      name,
			When:      rec.When,
			Liveness:  r.displayLiveness(ctx, rec.Handle),
			Transport: r.transport(rec.Handle),
			Handle:    rec.Handle,
		}
		if agent, ok := r.agentByHandle(rec.Handle); ok {
			row.Label = agent.TabLabel
			row.Title = agent.Title
		}
		rows = append(rows, row)
	}
	return rows
}

func (r *Registry) missingHandle(rows []Row) bool {
	for _, row := range rows {
		if _, ok := r.agentByHandle(row.Handle); !ok {
			return true
		}
	}
	return false
}

func (r *Registry) searchRows(query string) []Row {
	query = strings.TrimSpace(query)
	var rows []Row
	for _, item := range r.cached() {
		if r.hibernatedLocked(item.agent.Handle) {
			item.agent.Status = livenessHibernated
		}
		bindings := r.namesFor(item.agent.Handle)
		if len(bindings) == 0 {
			rows = appendMatch(rows, query, agentRow(item, Binding{}))
			continue
		}
		for _, binding := range bindings {
			rows = appendMatch(rows, query, agentRow(item, binding))
		}
	}
	rows = append(rows, r.hibernatedRows(query)...)
	sort.Slice(rows, func(i, j int) bool {
		if rows[i].Handle != rows[j].Handle {
			return rows[i].Handle < rows[j].Handle
		}
		return rows[i].Name < rows[j].Name
	})
	return rows
}

func appendMatch(rows []Row, query string, row Row) []Row {
	if query != "" && !rowMatches(row, query) {
		return rows
	}
	return append(rows, row)
}

func rowMatches(row Row, query string) bool {
	return strings.Contains(row.Name, query) ||
		strings.Contains(row.When, query) ||
		strings.Contains(row.Label, query) ||
		strings.Contains(row.Title, query)
}

func agentRow(item seen, binding Binding) Row {
	return Row{
		Name:      binding.Name,
		When:      binding.When,
		Liveness:  livenessWord(item.agent.Status),
		Transport: item.transport,
		Handle:    item.agent.Handle,
		Label:     item.agent.TabLabel,
		Title:     item.agent.Title,
	}
}

func (r *Registry) cached() []seen {
	var out []seen
	names := make([]string, 0, len(r.snaps))
	for name := range r.snaps {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		for _, agent := range r.snaps[name] {
			out = append(out, seen{transport: name, agent: agent})
		}
	}
	return out
}

func (r *Registry) namesFor(handle string) []Binding {
	var out []Binding
	for name, rec := range r.names {
		if rec.Handle != handle {
			continue
		}
		out = append(out, Binding{Name: name, Handle: rec.Handle, When: rec.When})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Name < out[j].Name })
	return out
}

func (r *Registry) agentByHandle(handle string) (adapter.Agent, bool) {
	for _, agents := range r.snaps {
		for _, agent := range agents {
			if agent.Handle == handle {
				return agent, true
			}
		}
	}
	return adapter.Agent{}, false
}

func (r *Registry) transport(handle string) string {
	for name, agents := range r.snaps {
		for _, agent := range agents {
			if agent.Handle == handle {
				return name
			}
		}
	}
	scheme, _, _, ok := splitHandle(handle)
	if ok {
		return scheme
	}
	return ""
}

// displayLiveness reports alive, gone, blocked, or unreachable.
// Blocked is its own list word. Unreachable is the section 4 class
// for unknown and transport errors; claims still treat it as alive.
func (r *Registry) displayLiveness(ctx context.Context, handle string) string {
	if r.hibernatedLocked(handle) {
		return livenessHibernated
	}
	ads, err := r.usable(ctx)
	if err != nil || len(ads) == 0 {
		return "unreachable"
	}
	blocked, alive, gone, unreach := false, false, false, false
	for _, ad := range ads {
		switch oneLiveness(ctx, ad, handle, r.snaps[ad.Name()]) {
		case "blocked":
			blocked = true
		case "alive":
			alive = true
		case "gone":
			gone = true
		default:
			unreach = true
		}
	}
	return pickLiveness(blocked, alive, gone, unreach)
}

func pickLiveness(blocked, alive, gone, unreach bool) string {
	if blocked {
		return "blocked"
	}
	if alive {
		return "alive"
	}
	if unreach || !gone {
		return "unreachable"
	}
	return "gone"
}

func oneLiveness(ctx context.Context, ad adapter.Adapter, handle string, agents []adapter.Agent) string {
	if sessionReplaced(handle, agents) {
		return "gone"
	}
	state, err := ad.State(ctx, handle)
	if err != nil {
		return "unreachable"
	}
	switch state {
	case adapter.StateBlocked:
		return "blocked"
	case adapter.StateIdle, adapter.StateWorking, adapter.StateDone:
		return "alive"
	case adapter.StateNoAgent, adapter.StateGone:
		return "gone"
	default:
		return "unreachable"
	}
}

func livenessWord(status string) string {
	if status == livenessHibernated {
		return livenessHibernated
	}
	switch adapter.State(status) {
	case adapter.StateBlocked:
		return "blocked"
	case adapter.StateIdle, adapter.StateWorking, adapter.StateDone:
		return "alive"
	case adapter.StateNoAgent, adapter.StateGone:
		return "gone"
	case adapter.StateUnknown:
		return "unreachable"
	default:
		if status == "" {
			return "alive"
		}
		return "unreachable"
	}
}
