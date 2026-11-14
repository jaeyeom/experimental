package tui

import (
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// nameResolver maps a ledger name to one canonical agent. The first
// match wins: an exact bus name, a pane or handle, a unique final path
// segment, then contactAliases.
type nameResolver struct {
	exact   map[string]bool
	bus     []Target
	suffix  map[string]string
	aliases map[string]string
}

func newResolver(bus []Target, aliases map[string]string) nameResolver {
	r := nameResolver{
		exact:   map[string]bool{},
		suffix:  map[string]string{},
		aliases: aliases,
	}
	counts := map[string]int{}
	for _, tg := range bus {
		if tg.Name == "" {
			continue
		}
		r.exact[tg.Name] = true
		r.bus = append(r.bus, tg)
		counts[lastSegment(tg.Name)]++
	}
	for _, tg := range r.bus {
		seg := lastSegment(tg.Name)
		if counts[seg] == 1 && seg != tg.Name {
			r.suffix[seg] = tg.Name
		}
	}
	return r
}

func lastSegment(name string) string {
	if i := strings.LastIndex(name, "/"); i >= 0 {
		return name[i+1:]
	}
	return name
}

// canonical is the agent name this ledger name stands for. An empty
// result is not a contact. it, when set, lets NotifyPane identify the
// item's owner.
func (r nameResolver) canonical(name string, it *decide.Item) string {
	name = strings.TrimSpace(name)
	if name == "" || name == "human" || name == "system" {
		return ""
	}
	if r.exact[name] {
		return name
	}
	if bus, ok := r.paneBus(name); ok {
		return bus
	}
	if it != nil {
		if bus, ok := r.notifyBus(it.NotifyPane); ok {
			return bus
		}
	}
	if bus, ok := r.suffix[name]; ok {
		return bus
	}
	if bus := strings.TrimSpace(r.aliases[name]); bus != "" {
		return bus
	}
	return name
}

// paneBus is the unique bus name whose handle is name. name may be the
// full herdr handle or a pane id.
func (r nameResolver) paneBus(name string) (string, bool) {
	if name == "" {
		return "", false
	}
	match := ""
	for _, tg := range r.bus {
		if tg.Pane == "" {
			continue
		}
		if tg.Pane != name && !samePane(tg.Pane, name) && !samePane(name, tg.Pane) {
			continue
		}
		if match != "" && match != tg.Name {
			return "", false
		}
		match = tg.Name
	}
	return match, match != ""
}

// notifyBus reads an item NotifyPane. agentbus:<name> is that agent
// when it is a bus name or a unique suffix. A pane id uses paneBus.
func (r nameResolver) notifyBus(pane string) (string, bool) {
	pane = strings.TrimSpace(pane)
	if pane == "" {
		return "", false
	}
	if rest, ok := strings.CutPrefix(pane, busPanePrefix); ok {
		rest = strings.TrimSpace(rest)
		if r.exact[rest] {
			return rest, true
		}
		if bus, ok := r.suffix[rest]; ok {
			return bus, true
		}
		return "", false
	}
	return r.paneBus(pane)
}
