package decide

import (
	"cmp"
	"slices"
	"strings"
	"time"
)

// SetContactAliases replaces the contactAliases display key and returns
// the pairs stored. Blank keys and values are dropped. Other keys and
// their order are kept. A config that does not parse or is not writable
// is left unchanged.
func SetContactAliases(aliases map[string]string) (map[string]string, error) {
	aliases = normalizeAliases(aliases)
	if err := writeConfigMember("contactAliases", aliases); err != nil {
		return nil, err
	}
	return aliases, nil
}

// normalizeAliases trims pairs and drops blanks. The result is never nil.
func normalizeAliases(aliases map[string]string) map[string]string {
	out := map[string]string{}
	for name, agent := range aliases {
		name = strings.TrimSpace(name)
		agent = strings.TrimSpace(agent)
		if name == "" || agent == "" {
			continue
		}
		out[name] = agent
	}
	return out
}

// DefaultDuplicateAgentWindow is the maximum time between filings from the
// same pane and machine that doctor flags as likely unaliased duplicates.
const DefaultDuplicateAgentWindow = time.Hour

// NameResolver resolves raw agent names (tab labels, pane handles, short
// suffixes, target names, or configured contactAliases) to canonical agent names.
type NameResolver struct {
	aliases map[string]string
	exact   map[string]bool
	bus     []BusRecipient
	suffix  map[string]string
}

// NewNameResolver builds a NameResolver from cfg (contactAliases and targets)
// and optional agentbus entries.
func NewNameResolver(cfg Config, bus []BusRecipient) NameResolver {
	r := NameResolver{
		aliases: normalizeAliases(cfg.ContactAliases),
		exact:   map[string]bool{},
		suffix:  map[string]string{},
	}
	counts := map[string]int{}
	for _, b := range bus {
		name := strings.TrimSpace(b.Name)
		if name == "" {
			continue
		}
		r.exact[name] = true
		r.bus = append(r.bus, BusRecipient{Name: name, Handle: strings.TrimSpace(b.Handle)})
		counts[lastNameSegment(name)]++
	}
	for _, b := range r.bus {
		seg := lastNameSegment(b.Name)
		if counts[seg] == 1 && seg != b.Name {
			r.suffix[seg] = b.Name
		}
	}
	for _, tg := range cfg.Targets {
		tgName := strings.TrimSpace(tg.Name)
		if tgName == "" || r.aliases[tgName] != "" {
			continue
		}
		if rec, ok := AgentbusPaneRecipient(tg.Pane); ok {
			if rec.Name != "" && rec.Name != tgName {
				r.aliases[tgName] = rec.Name
			}
			continue
		}
		if busName, ok := r.paneBus(tg.Pane); ok && busName != tgName {
			r.aliases[tgName] = busName
		}
	}
	return r
}

func lastNameSegment(name string) string {
	if i := strings.LastIndex(name, "/"); i >= 0 {
		return name[i+1:]
	}
	return name
}

func (r NameResolver) resolveAlias(name string) string {
	cur := name
	seen := map[string]bool{cur: true}
	for {
		next := strings.TrimSpace(r.aliases[cur])
		if next == "" || seen[next] {
			return cur
		}
		seen[next] = true
		cur = next
	}
}

// Resolve returns the canonical agent name for name. When it is non-nil,
// it.NotifyPane can also identify the agent via agentbus pane matching.
// Unmapped names are returned trimmed as-is.
func (r NameResolver) Resolve(name string, it *Item) string {
	name = strings.TrimSpace(name)
	if name == "" {
		return ""
	}
	if name == Human || name == "system" {
		return name
	}
	aliased := r.resolveAlias(name)
	if r.exact[aliased] {
		return aliased
	}
	if bus, ok := r.paneBus(aliased); ok {
		return bus
	}
	if it != nil {
		if bus, ok := r.NotifyBus(it.NotifyPane); ok {
			return bus
		}
	}
	if bus, ok := r.suffix[aliased]; ok {
		return bus
	}
	return aliased
}

// paneBus returns the unique bus name whose handle matches name.
func (r NameResolver) paneBus(name string) (string, bool) {
	name = strings.TrimSpace(name)
	if name == "" {
		return "", false
	}
	match := ""
	for _, b := range r.bus {
		if b.Handle == "" {
			continue
		}
		if b.Handle != name && !SamePane(b.Handle, name) && !SamePane(name, b.Handle) {
			continue
		}
		if match != "" && match != b.Name {
			return "", false
		}
		match = b.Name
	}
	return match, match != ""
}

// NotifyBus resolves pane (an agentbus:<name> target or a pane id) to a bus
// name known to r.
func (r NameResolver) NotifyBus(pane string) (string, bool) {
	pane = strings.TrimSpace(pane)
	if pane == "" {
		return "", false
	}
	if rest, ok := strings.CutPrefix(pane, BusPanePrefix); ok {
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

// DuplicateAgentWarning describes unaliased agent names that filed items from
// the same pane and machine within a short time window.
type DuplicateAgentWarning struct {
	Names   []string
	Pane    string
	Machine string
}

type paneFiling struct {
	canon string
	pane  string
	at    time.Time
}

// FindUnaliasedDuplicates detects pairs of distinct canonical agent names that
// filed items from the same pane (and machine) within window.
func FindUnaliasedDuplicates(items []Item, res NameResolver, window time.Duration) []DuplicateAgentWarning {
	if window <= 0 {
		window = DefaultDuplicateAgentWindow
	}
	byMachine := map[string][]paneFiling{}
	for i := range items {
		it := &items[i]
		if it.CreatedAt.IsZero() {
			continue
		}
		tg := ParseTarget(it.NotifyPane, it.NotifyMachine)
		if tg.Scheme != SchemeHerdr || tg.Address == "" {
			continue
		}
		raw := itemRawOwner(it)
		canon := it.AskingAgent
		if canon == "" {
			canon = raw
		}
		canon = res.Resolve(canon, it)
		if canon == "" || canon == Human || canon == "system" {
			continue
		}
		byMachine[tg.Machine] = append(byMachine[tg.Machine], paneFiling{
			canon: canon,
			pane:  tg.Address,
			at:    it.CreatedAt.UTC(),
		})
	}

	var out []DuplicateAgentWarning
	for machine, filings := range byMachine {
		out = append(out, machinePaneDuplicates(machine, filings, window)...)
	}
	slices.SortFunc(out, func(a, b DuplicateAgentWarning) int {
		if c := cmp.Compare(a.Machine, b.Machine); c != 0 {
			return c
		}
		if c := cmp.Compare(a.Pane, b.Pane); c != 0 {
			return c
		}
		return cmp.Compare(strings.Join(a.Names, ","), strings.Join(b.Names, ","))
	})
	return out
}

func machinePaneDuplicates(machine string, filings []paneFiling, window time.Duration) []DuplicateAgentWarning {
	var warnings []DuplicateAgentWarning
	for _, g := range groupPaneFilings(filings) {
		names := windowDuplicateNames(g, window)
		if len(names) < 2 {
			continue
		}
		warnings = append(warnings, DuplicateAgentWarning{
			Names:   names,
			Pane:    shortestPaneLabel(g),
			Machine: machine,
		})
	}
	return warnings
}

// groupPaneFilings groups filings whose panes match via SamePane (handles
// both bare pane IDs and full handles).
func groupPaneFilings(filings []paneFiling) [][]paneFiling {
	var groups [][]paneFiling
	for _, f := range filings {
		placed := false
		for gi := range groups {
			rep := groups[gi][0].pane
			if rep == f.pane || SamePane(rep, f.pane) || SamePane(f.pane, rep) {
				groups[gi] = append(groups[gi], f)
				placed = true
				break
			}
		}
		if !placed {
			groups = append(groups, []paneFiling{f})
		}
	}
	return groups
}

func windowDuplicateNames(g []paneFiling, window time.Duration) []string {
	slices.SortFunc(g, func(a, b paneFiling) int {
		return a.at.Compare(b.at)
	})
	dupNames := map[string]bool{}
	for i := range g {
		for j := i + 1; j < len(g); j++ {
			if g[j].at.Sub(g[i].at) > window {
				break
			}
			if g[i].canon != g[j].canon {
				dupNames[g[i].canon] = true
				dupNames[g[j].canon] = true
			}
		}
	}
	if len(dupNames) < 2 {
		return nil
	}
	names := make([]string, 0, len(dupNames))
	for n := range dupNames {
		names = append(names, n)
	}
	slices.Sort(names)
	return names
}

func shortestPaneLabel(g []paneFiling) string {
	paneLabel := g[0].pane
	for _, f := range g[1:] {
		if len(f.pane) < len(paneLabel) {
			paneLabel = f.pane
		}
	}
	return paneLabel
}
