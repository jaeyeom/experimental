package daemon

import (
	"cmp"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/dialog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// DialogEntry describes one agent pane currently showing a dialog or input prompt.
type DialogEntry struct {
	Pane    string `json:"pane"`
	Name    string `json:"agent_name"` //nolint:tagliatelle // CLI JSON shape
	Handle  string `json:"handle,omitempty"`
	Machine string `json:"machine"`
	Rule    string `json:"rule"`
	Text    string `json:"text"`
}

func (s *server) dispatchChecks(req Request) Response {
	switch req.Op {
	case "held":
		return s.held(req)
	case "asks":
		return s.asks(req)
	default:
		return s.dialogs(req)
	}
}

func (s *server) held(req Request) Response {
	olderThan, err := nonNegativeDuration(req.OlderThan, "older-than")
	if err != nil {
		return Response{Error: err.Error()}
	}
	items := s.box.HeldMessages(mailbox.HeldFilter{
		OlderThan:  olderThan,
		Reasons:    req.Reasons,
		NotReasons: req.NotReasons,
		To:         req.To,
	})
	return Response{OK: true, HeldMessages: items}
}

func (s *server) asks(req Request) Response {
	unanswered, err := nonNegativeDuration(req.OlderThan, "unanswered")
	if err != nil {
		return Response{Error: err.Error()}
	}
	items := s.box.Asks(mailbox.AskFilter{
		Unanswered: unanswered,
		From:       req.From,
		To:         req.To,
	})
	return Response{OK: true, Asks: items}
}

func nonNegativeDuration(raw, flagName string) (time.Duration, error) {
	if strings.TrimSpace(raw) == "" {
		return 0, nil
	}
	d, err := time.ParseDuration(strings.TrimSpace(raw))
	if err != nil || d < 0 {
		return 0, fmt.Errorf("%s must be a non-negative duration", flagName)
	}
	return d, nil
}

func (s *server) dialogs(req Request) Response {
	rules, err := dialog.CompileRules(mergeDialogRules(s.dialogRules, req.DialogRules))
	if err != nil {
		return Response{Error: err.Error()}
	}
	ads, err := s.reg.Usable(s.ctx)
	if err != nil {
		return Response{Error: err.Error()}
	}
	if len(ads) == 0 && len(s.adapters) > 0 {
		return Response{Error: "no adapter is reachable"}
	}
	var entries []DialogEntry
	for _, ad := range ads {
		found, err := s.adapterDialogs(ad, req.Machine, rules)
		if err != nil {
			return Response{Error: err.Error()}
		}
		entries = append(entries, found...)
	}
	slices.SortFunc(entries, func(a, b DialogEntry) int {
		return cmp.Or(cmp.Compare(a.Machine, b.Machine), cmp.Compare(a.Pane, b.Pane), cmp.Compare(a.Handle, b.Handle))
	})
	return Response{OK: true, Dialogs: entries}
}

func (s *server) adapterDialogs(ad adapter.Adapter, machineFilter string, rules []dialog.Rule) ([]DialogEntry, error) {
	sr, ok := adapter.AsScreenReader(ad)
	if !ok {
		return nil, nil
	}
	agents, err := s.inventoryForDialogs(ad, machineFilter)
	if err != nil {
		return nil, err
	}
	var out []DialogEntry
	for _, ag := range agents {
		entry, matched, err := s.agentDialog(sr, ag, machineFilter, rules)
		if err != nil {
			return nil, err
		}
		if matched {
			out = append(out, entry)
		}
	}
	return out, nil
}

func (s *server) inventoryForDialogs(ad adapter.Adapter, machineFilter string) ([]adapter.Agent, error) {
	agents, invErr := ad.Inventory(s.ctx)
	if invErr == nil {
		return agents, nil
	}
	var warn *adapter.InventoryWarning
	if !errors.As(invErr, &warn) {
		return nil, fmt.Errorf("inventory: %w", invErr)
	}
	if machineFilter != "" {
		if msg, bad := machineInventoryError(warn.Lines, machineFilter); bad {
			return nil, errors.New(msg)
		}
	}
	return agents, nil
}

func (s *server) agentDialog(sr adapter.ScreenReader, ag adapter.Agent, machineFilter string, rules []dialog.Rule) (DialogEntry, bool, error) {
	if ag.Kind == "" && ag.Status == "" {
		return DialogEntry{}, false, nil
	}
	if rec, asleep := s.reg.Hibernation(ag.Handle); asleep && hibernate.Asleep(rec.Status) {
		return DialogEntry{}, false, nil
	}
	machine, pane := agentMachineAndPane(ag)
	if machineFilter != "" && machine != machineFilter {
		return DialogEntry{}, false, nil
	}
	screen, err := sr.ReadScreen(s.ctx, ag.Handle)
	if remotecall.AsUnreachable(err) != nil {
		return DialogEntry{}, false, fmt.Errorf("read screen: %w", err)
	}
	if err != nil {
		return DialogEntry{}, false, nil
	}
	match, matched := dialog.Detect(ag.Kind, screen, rules)
	if !matched {
		return DialogEntry{}, false, nil
	}
	name := ""
	if names := s.reg.NamesOf(ag.Handle); len(names) > 0 {
		name = names[0]
	}
	return DialogEntry{
		Pane:    pane,
		Name:    name,
		Handle:  ag.Handle,
		Machine: machine,
		Rule:    match.Rule,
		Text:    match.Text,
	}, true, nil
}

func mergeDialogRules(base, extra []config.DialogRule) []config.DialogRule {
	if len(extra) == 0 {
		return base
	}
	if len(base) == 0 {
		return extra
	}
	seen := make(map[string]bool, len(base))
	out := make([]config.DialogRule, 0, len(base)+len(extra))
	for _, r := range base {
		seen[r.Name+"\x00"+r.Pattern] = true
		out = append(out, r)
	}
	for _, r := range extra {
		k := r.Name + "\x00" + r.Pattern
		if !seen[k] {
			seen[k] = true
			out = append(out, r)
		}
	}
	return out
}

func agentMachineAndPane(ag adapter.Agent) (machine, pane string) {
	m, _, p, err := hubcall.ParseHandle(ag.Handle)
	if err == nil {
		machine = m
		pane = p
	}
	if machine == "" {
		machine = hubcall.LocalMachine
	}
	if ag.PaneID != "" {
		pane = ag.PaneID
	}
	return machine, pane
}

func machineInventoryError(lines []string, machine string) (string, bool) {
	prefix := "machine " + machine + " "
	for _, line := range lines {
		if strings.HasPrefix(line, prefix) || strings.Contains(line, prefix) {
			return line, true
		}
	}
	return "", false
}
