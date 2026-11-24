package decide

import (
	"fmt"
	"io"
	"strings"
)

const (
	// SchemeHerdr delivers to a local or remote herdr pane.
	SchemeHerdr = "herdr"
	// SchemeAgentbus delivers to an agentbus recipient name.
	SchemeAgentbus = "agentbus"
	// SchemePull marks an item whose owner polls for updates instead of
	// receiving push notifications.
	SchemePull = "pull"
	// SchemeTarget delivers to a named entry in config.Targets.
	SchemeTarget = "target"

	// HerdrPanePrefix is the optional explicit prefix for a herdr pane target.
	HerdrPanePrefix = "herdr:"
	// PullPanePrefix is the prefix for a pull target with an optional detail.
	PullPanePrefix = "pull:"
	// TargetPanePrefix is the prefix for a named target in config.Targets.
	TargetPanePrefix = "target:"
)

// Target is a typed delivery destination parsed from a notify pane string
// and optional herdr machine label.
type Target struct {
	Scheme  string
	Address string
	Machine string
}

// ParseTarget parses a raw notify pane string and machine label into a typed Target.
//
// Supported forms:
//   - "" -> empty Target{}
//   - "pull" or "pull:<detail>" -> Scheme: SchemePull
//   - "agentbus:<name>" -> Scheme: SchemeAgentbus, Address: name
//   - "target:<name>" -> Scheme: SchemeTarget, Address: name
//   - "herdr:<pane>" -> Scheme: SchemeHerdr, Address: pane, Machine: machine
//   - "<scheme>:<address>" (where scheme matches [a-z0-9][a-z0-9-]* and raw is
//     not a bare herdr pane id such as "w1:p2") -> Scheme: scheme, Address: address
//   - any other non-empty string -> Scheme: SchemeHerdr, Address: pane, Machine: machine
func ParseTarget(pane, machine string) Target {
	raw := strings.TrimSpace(pane)
	if raw == "" {
		return Target{}
	}
	if raw == SchemePull {
		return Target{Scheme: SchemePull}
	}
	if detail, ok := strings.CutPrefix(raw, PullPanePrefix); ok {
		return Target{Scheme: SchemePull, Address: strings.TrimSpace(detail)}
	}
	if name, ok := strings.CutPrefix(raw, BusPanePrefix); ok {
		return Target{Scheme: SchemeAgentbus, Address: strings.TrimSpace(name)}
	}
	if name, ok := strings.CutPrefix(raw, TargetPanePrefix); ok {
		return Target{Scheme: SchemeTarget, Address: strings.TrimSpace(name)}
	}
	if addr, ok := strings.CutPrefix(raw, HerdrPanePrefix); ok {
		return Target{
			Scheme:  SchemeHerdr,
			Address: strings.TrimSpace(addr),
			Machine: strings.TrimSpace(machine),
		}
	}
	if scheme, addr, ok := strings.Cut(raw, ":"); ok && !isHerdrPaneID(raw) && isValidTransportScheme(scheme) {
		return Target{
			Scheme:  scheme,
			Address: strings.TrimSpace(addr),
			Machine: strings.TrimSpace(machine),
		}
	}
	return Target{
		Scheme:  SchemeHerdr,
		Address: raw,
		Machine: strings.TrimSpace(machine),
	}
}

// isValidTransportScheme reports whether s matches [a-z0-9][a-z0-9-]*.
func isValidTransportScheme(s string) bool {
	if s == "" {
		return false
	}
	for i := range len(s) {
		c := s[i]
		isLower := c >= 'a' && c <= 'z'
		isDigit := c >= '0' && c <= '9'
		if i == 0 {
			if !isLower && !isDigit {
				return false
			}
			continue
		}
		if !isLower && !isDigit && c != '-' {
			return false
		}
	}
	return true
}

// isReservedTransportScheme reports whether s is a built-in transport scheme.
func isReservedTransportScheme(s string) bool {
	switch s {
	case SchemeHerdr, SchemeAgentbus, SchemePull, SchemeTarget:
		return true
	default:
		return false
	}
}

// isHerdrPaneID reports whether raw looks like a bare herdr pane id of the
// form "w<digits>:p<id>" (for example, "w1:p2" or "w6:pM").
func isHerdrPaneID(raw string) bool {
	ws, pane, ok := strings.Cut(raw, ":")
	if !ok || len(ws) < 2 || ws[0] != 'w' || len(pane) < 2 || pane[0] != 'p' {
		return false
	}
	return allDigits(ws[1:]) && allPaneIDChars(pane[1:])
}

func allDigits(s string) bool {
	return s != "" && !strings.ContainsFunc(s, func(r rune) bool {
		return r < '0' || r > '9'
	})
}

func allPaneIDChars(s string) bool {
	return s != "" && !strings.ContainsFunc(s, func(r rune) bool {
		return (r < '0' || r > '9') && (r < 'a' || r > 'z') && (r < 'A' || r > 'Z') && r != '-' && r != '_'
	})
}

// String formats t using the existing wire representation where applicable:
// "agentbus:<name>", "target:<name>", "pull" (or "pull:<detail>"), the bare
// pane for herdr, or "<scheme>:<address>" for custom transport plugins.
func (t Target) String() string {
	switch t.Scheme {
	case "":
		return ""
	case SchemeAgentbus:
		return BusPanePrefix + t.Address
	case SchemeTarget:
		return TargetPanePrefix + t.Address
	case SchemePull:
		if t.Address != "" {
			return PullPanePrefix + t.Address
		}
		return SchemePull
	case SchemeHerdr:
		return t.Address
	default:
		return t.Scheme + ":" + t.Address
	}
}

// Transport delivers answers and compose notes to a parsed Target.
type Transport interface {
	DeliverAnswer(item *Item, target Target, cfg Config, out io.Writer) (Delivery, error)
	DeliverNote(item *Item, id string, target Target, note string, cfg Config) (Delivery, error)
}

func transportFor(target Target) Transport {
	switch target.Scheme {
	case "", SchemeHerdr:
		return herdrTransport{}
	case SchemeAgentbus:
		return agentbusTransport{}
	case SchemePull:
		return pullTransport{}
	case SchemeTarget:
		return targetTransport{}
	default:
		return pluginTransport{}
	}
}

type herdrTransport struct{}

func (herdrTransport) DeliverAnswer(item *Item, target Target, cfg Config, out io.Writer) (Delivery, error) {
	if target.Machine != "" {
		return notifyRemote(item, target.Address, target.Machine, cfg)
	}
	if strings.TrimSpace(cfg.NotifyTemplate) == "" {
		return Delivery{}, nil
	}
	return notifyLocal(item, target.Address, cfg, out)
}

func (herdrTransport) DeliverNote(_ *Item, id string, target Target, note string, cfg Config) (Delivery, error) {
	wire := target.Address
	if target.Machine != "" {
		wire = target.Address + "@" + target.Machine
	}
	return SentDelivery(id, wire, RunSend(MachineArgv(SendArgv(cfg.SendTemplate, target.Address, note), target.Machine)))
}

type agentbusTransport struct{}

func (agentbusTransport) DeliverAnswer(item *Item, target Target, _ Config, _ io.Writer) (Delivery, error) {
	return notifyBus(item, target.Address)
}

func (agentbusTransport) DeliverNote(_ *Item, id string, target Target, note string, _ Config) (Delivery, error) {
	return deliverBus(id, target.Address, note)
}

type pullTransport struct{}

func (pullTransport) DeliverAnswer(_ *Item, _ Target, _ Config, _ io.Writer) (Delivery, error) {
	return Delivery{}, nil
}

func (pullTransport) DeliverNote(_ *Item, _ string, _ Target, _ string, _ Config) (Delivery, error) {
	return Delivery{}, nil
}

type targetTransport struct{}

func (targetTransport) DeliverAnswer(item *Item, target Target, cfg Config, out io.Writer) (Delivery, error) {
	wire := target.String()
	tg, ok := FindTarget(cfg.Targets, target.Address)
	if !ok || target.Address == "" {
		return SentDelivery(item.ID, wire, fmt.Errorf("notify: unknown target %q", target.Address))
	}
	if len(tg.Command) > 0 {
		return deliverCommand(item, wire, tg.Command, "notify", "", out)
	}
	resolved := ParseTarget(tg.Pane, "")
	if resolved.Scheme == "" || resolved.Scheme == SchemeTarget {
		return SentDelivery(item.ID, wire, fmt.Errorf("notify: target %q has no delivery command or pane", target.Address))
	}
	return transportFor(resolved).DeliverAnswer(item, resolved, cfg, out) //nolint:wrapcheck // transport implementations already wrap delivery errors
}

func (targetTransport) DeliverNote(item *Item, id string, target Target, note string, cfg Config) (Delivery, error) {
	wire := target.String()
	tg, ok := FindTarget(cfg.Targets, target.Address)
	if !ok || target.Address == "" {
		return SentDelivery(id, wire, fmt.Errorf("decide send: unknown target %q", target.Address))
	}
	if len(tg.Command) > 0 {
		if item == nil {
			item = &Item{ID: id, NotifyPane: wire}
		}
		return deliverCommand(item, wire, tg.Command, "decide send", "human", nil)
	}
	resolved := ParseTarget(tg.Pane, "")
	if resolved.Scheme == "" || resolved.Scheme == SchemeTarget {
		return SentDelivery(id, wire, fmt.Errorf("decide send: target %q has no delivery command or pane", target.Address))
	}
	return transportFor(resolved).DeliverNote(item, id, resolved, note, cfg) //nolint:wrapcheck // transport implementations already wrap delivery errors
}
