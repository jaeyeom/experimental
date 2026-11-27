package tui

import (
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// nameResolver maps a ledger name to one canonical agent. It delegates to
// decide.NameResolver and filters out non-contact names ("human", "system").
type nameResolver struct {
	res decide.NameResolver
}

func newResolver(bus []Target, aliases map[string]string) nameResolver {
	recs := make([]decide.BusRecipient, 0, len(bus))
	for _, tg := range bus {
		recs = append(recs, decide.BusRecipient{Name: tg.Name, Handle: tg.Pane})
	}
	cfg := decide.Config{ContactAliases: aliases}
	return nameResolver{res: decide.NewNameResolver(cfg, recs)}
}

// canonical is the agent name this ledger name stands for. An empty
// result is not a contact. it, when set, lets NotifyPane identify the
// item's owner.
func (r nameResolver) canonical(name string, it *decide.Item) string {
	name = strings.TrimSpace(name)
	if name == "" || name == "human" || name == "system" {
		return ""
	}
	return r.res.Resolve(name, it)
}

// notifyBus reads an item NotifyPane. agentbus:<name> is that agent
// when it is a bus name or a unique suffix. A pane id uses paneBus.
func (r nameResolver) notifyBus(pane string) (string, bool) {
	return r.res.NotifyBus(pane)
}
