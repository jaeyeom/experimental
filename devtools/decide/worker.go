package decide

import (
	"fmt"
	"strings"
)

// Reply routes for a human reply on an item with a worker.
const (
	// RouteOwner notifies the owner only, as a reply did before workers.
	RouteOwner = "owner"
	// RouteWorker notifies the worker only.
	RouteWorker = "worker"
	// RouteBoth notifies the owner and the worker.
	RouteBoth = "both"
)

// CheckRoute reports an error unless route is RouteOwner, RouteWorker,
// or RouteBoth.
func CheckRoute(route string) error {
	switch route {
	case RouteOwner, RouteWorker, RouteBoth:
		return nil
	default:
		return fmt.Errorf("decide reply route %q must be %s, %s, or %s", route, RouteOwner, RouteWorker, RouteBoth)
	}
}

// Assign sets or clears the agent working on an item. Pane is the
// worker's notify target; empty means agentbus:<Worker>. Machine is the
// herdr machine of a pane id and is ignored for an agentbus name. By is
// who ran it and may be empty.
type Assign struct {
	Worker  string
	Pane    string
	Machine string
	Clear   bool
	By      string
}

// Assign records who is working on id while the owner stays. A done or
// withdrawn item is refused. A change appends one system message;
// clearing an item with no worker changes nothing.
func (l *Ledger) Assign(id string, change Assign) (Item, error) {
	worker := strings.TrimSpace(change.Worker)
	if !change.Clear && worker == "" {
		return Item{}, fmt.Errorf("decide worker is empty")
	}
	var updated Item
	err := l.locked(true, func(st *store) error {
		it, err := findItem(st, id)
		if err != nil {
			return err
		}
		if it.State == StateDone || it.State == StateWithdrawn {
			return fmt.Errorf("decide item %s is %s", id, it.State)
		}
		ensureMessages(it)
		old := it.Worker
		if change.Clear {
			if old == "" {
				updated = *it
				return nil
			}
			it.Worker, it.WorkerNotifyPane, it.WorkerNotifyMachine = "", "", ""
		} else {
			pane := strings.TrimSpace(change.Pane)
			if pane == "" {
				pane = BusPanePrefix + worker
			}
			it.Worker, it.WorkerNotifyPane, it.WorkerNotifyMachine = worker, pane, ""
			if !strings.HasPrefix(pane, BusPanePrefix) {
				it.WorkerNotifyMachine = strings.TrimSpace(change.Machine)
			}
		}
		it.Messages = append(it.Messages, Message{
			Author: "system",
			Time:   l.clock().UTC(),
			Text:   assignNote(old, it.Worker, change.By),
			By:     change.By,
		})
		updated = *it
		return nil
	})
	if err != nil {
		return Item{}, err
	}
	return updated, nil
}

// assignNote is the system message for one assign.
func assignNote(oldWorker, newWorker, by string) string {
	var note string
	switch {
	case newWorker == "":
		note = "unassigned: " + oldWorker + " is no longer working on it"
	case oldWorker != "" && oldWorker != newWorker:
		note = "assigned: " + newWorker + " is working on it, was " + oldWorker
	default:
		note = "assigned: " + newWorker + " is working on it"
	}
	if by != "" {
		note += " (by " + by + ")"
	}
	return note
}

// HasOtherWorker reports that someone other than the owner is working
// on the item, so a reply to the owner may miss them.
func (it *Item) HasOtherWorker() bool {
	return it.Worker != "" && it.Worker != it.Owner
}

// RouteWarning says who a plain reply reaches when another agent is
// working on the item. It is empty when there is no such worker.
func RouteWarning(it *Item) string {
	if !it.HasOtherWorker() {
		return ""
	}
	return "This reply goes to " + it.Owner + " (owner); " + it.Worker + " is working on it."
}

// ReplyTarget is one agent a human reply notifies.
type ReplyTarget struct {
	Name    string
	Pane    string
	Machine string
}

// ReplyTargets are the agents a human reply on it notifies for route.
// An empty route is RouteOwner. The owner is resolved by NotifyTarget and
// is left out when its routing was closed. RouteWorker and RouteBoth
// reach the worker only when HasOtherWorker; RouteWorker without one
// falls back to the owner.
func ReplyTargets(it *Item, cfg Config, route string) []ReplyTarget {
	worker := it.HasOtherWorker() && (route == RouteWorker || route == RouteBoth)
	var out []ReplyTarget
	if (!worker || route == RouteBoth) && !it.NotifyClosed {
		pane, machine := NotifyTarget(it, cfg)
		out = append(out, ReplyTarget{Name: it.Owner, Pane: pane, Machine: machine})
	}
	if worker {
		w := ReplyTarget{Name: it.Worker, Pane: it.WorkerNotifyPane, Machine: it.WorkerNotifyMachine}
		if len(out) == 0 || out[0].Pane != w.Pane || out[0].Machine != w.Machine {
			out = append(out, w)
		}
	}
	return out
}
