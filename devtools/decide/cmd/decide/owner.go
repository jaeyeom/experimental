package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// The hub-mode client sends its tab label and agentbus names with an ask,
// next to DECIDE_AS_HANDLE, because the hub cannot look them up.
const (
	tabLabelEnv = "DECIDE_TAB_LABEL"
	busNamesEnv = "DECIDE_BUS_NAMES"
)

// whoamiTimeout bounds `agentbus whoami`.
const whoamiTimeout = 2 * time.Second

// agentbusWhoami returns `agentbus whoami` stdout. Tests replace it.
var agentbusWhoami = execAgentbusWhoami

func execAgentbusWhoami(ctx context.Context) (string, error) {
	res, err := remotecall.Run(ctx, "agentbus", "whoami")
	if err != nil {
		return "", fmt.Errorf("agentbus whoami: %w", err)
	}
	if res.Code != 0 || res.TimedOut {
		return "", fmt.Errorf("agentbus whoami: exit %d: %s", res.Code, strings.TrimSpace(res.Stderr))
	}
	return res.Stdout, nil
}

// parseWhoami returns the names in `agentbus whoami` output: a name line
// followed by a "when: " line for each. A caller without names prints
// only its handle, which is not a name.
func parseWhoami(out string) []string {
	lines := strings.Split(strings.TrimRight(out, "\n"), "\n")
	var names []string
	for i := 0; i+1 < len(lines); i += 2 {
		if !strings.HasPrefix(lines[i+1], "when: ") {
			return nil
		}
		names = append(names, strings.TrimSpace(lines[i]))
	}
	return names
}

// localBusNames is the agentbus names this pane holds, or none when
// agentbus is missing or fails.
func localBusNames(ctx context.Context) []string {
	ctx, cancel := context.WithTimeout(ctx, whoamiTimeout)
	defer cancel()
	out, err := agentbusWhoami(ctx)
	if err != nil {
		return nil
	}
	return parseWhoami(out)
}

// paneTabLabel is the label of pane's herdr tab on this machine, or empty
// when herdr does not list the pane.
func paneTabLabel(ctx context.Context, pane string) string {
	loc := decide.LocationResolver{}.Resolve(ctx, []decide.OwnerLocation{{Pane: pane, State: decide.LocationUnknown}})[0]
	if loc.State != decide.LocationLive {
		return ""
	}
	return loc.Tab
}

// callerEnv describes the detected calling environment for a decide command.
type callerEnv struct {
	tab           string
	busNames      []string
	sender        string
	defaultTarget decide.Target
}

// detectCaller inspects the caller environment in order:
//  1. Trusted hub-forwarded caller (DECIDE_AS_HANDLE, DECIDE_TAB_LABEL, DECIDE_BUS_NAMES).
//  2. Local herdr pane (HERDR_PANE_ID).
//  3. Outside herdr, defaulting delivery to pull.
func detectCaller(ctx context.Context) callerEnv {
	if env, ok := detectHubCaller(); ok {
		return env
	}
	if env, ok := detectHerdrCaller(ctx); ok {
		return env
	}
	return callerEnv{
		defaultTarget: decide.Target{Scheme: decide.SchemePull},
	}
}

func detectHubCaller() (callerEnv, bool) {
	caller, err := hubcall.TrustedCaller(os.Environ(), asHandleEnv)
	if err != nil || caller == "" {
		return callerEnv{}, false
	}
	names := splitNames(os.Getenv(busNamesEnv))
	tab := strings.TrimSpace(os.Getenv(tabLabelEnv))
	sender := caller
	switch {
	case len(names) > 0:
		sender = names[0]
	case tab != "":
		sender = tab
	}
	var target decide.Target
	if machine, _, pane, perr := hubcall.ParseHandle(caller); perr == nil && pane != "" {
		target = decide.Target{Scheme: decide.SchemeHerdr, Address: pane, Machine: machine}
	}
	return callerEnv{
		tab:           tab,
		busNames:      names,
		sender:        sender,
		defaultTarget: target,
	}, true
}

func detectHerdrCaller(ctx context.Context) (callerEnv, bool) {
	pane := strings.TrimSpace(os.Getenv("HERDR_PANE_ID"))
	if pane == "" {
		return callerEnv{}, false
	}
	names, handle := whoamiSender(ctx)
	tab := paneTabLabel(ctx, pane)
	sender := pane
	switch {
	case len(names) > 0:
		sender = names[0]
	case tab != "":
		sender = tab
	case handle != "":
		sender = handle
	}
	return callerEnv{
		tab:           tab,
		busNames:      names,
		sender:        sender,
		defaultTarget: decide.Target{Scheme: decide.SchemeHerdr, Address: pane},
	}, true
}

// senderOf is who runs a reply or send --id, for decide.Message.By: the
// caller's first agentbus name, else its herdr tab label, else its pane
// handle, else empty outside herdr. On the hub, a trusted hub-mode
// caller's names, tab label, and handle come from the environment the
// client sent. Locally, the handle is the one `agentbus whoami` prints,
// or HERDR_PANE_ID when that output has no handle.
func senderOf(ctx context.Context) string {
	return detectCaller(ctx).sender
}

// resolveAskNotifyPane picks the notifyPane stored on a new ask item.
// An explicit --notify-pane always wins (including "pull"). Otherwise,
// a detected herdr target is stored; outside herdr, "pull" is stored
// unless cfg.NotifyPane provides a fallback default.
func resolveAskNotifyPane(explicit string, caller callerEnv, cfg decide.Config) string {
	if strings.TrimSpace(explicit) != "" {
		return explicit
	}
	if caller.defaultTarget.Scheme == decide.SchemePull && strings.TrimSpace(cfg.NotifyPane) != "" {
		return ""
	}
	return caller.defaultTarget.String()
}

// whoamiSender reads this pane's agentbus names and, when it has none, the
// handle `agentbus whoami` prints. A failure returns neither.
func whoamiSender(ctx context.Context) (names []string, handle string) {
	ctx, cancel := context.WithTimeout(ctx, whoamiTimeout)
	defer cancel()
	out, err := agentbusWhoami(ctx)
	if err != nil {
		return nil, ""
	}
	if names = parseWhoami(out); len(names) > 0 {
		return names, ""
	}
	handle = strings.TrimSpace(out)
	if handle == "" || strings.Contains(handle, "\n") {
		return nil, ""
	}
	return nil, handle
}

// clientAskEnv is the tab label and agentbus names a hub-mode client
// sends with an ask from pane.
func clientAskEnv(ctx context.Context, pane string) []string {
	var env []string
	if tab := paneTabLabel(ctx, pane); tab != "" {
		env = append(env, tabLabelEnv+"="+tab)
	}
	if names := localBusNames(ctx); len(names) > 0 {
		env = append(env, busNamesEnv+"="+strings.Join(names, ","))
	}
	return env
}

func splitNames(raw string) []string {
	var names []string
	for _, n := range strings.Split(raw, ",") {
		if n = strings.TrimSpace(n); n != "" {
			names = append(names, n)
		}
	}
	return names
}

// askOwner is owner, or the caller's tab label when owner is empty. An
// owner that is neither the tab label nor one of the caller's agentbus
// names gets a warning on stderr. Outside herdr, owner is unchanged.
func askOwner(owner string, caller callerEnv, stderr io.Writer) string {
	if caller.tab == "" {
		return owner
	}
	if strings.TrimSpace(owner) == "" {
		return caller.tab
	}
	if owner == caller.tab {
		return owner
	}
	if slices.Contains(caller.busNames, owner) {
		return owner
	}
	list := "none"
	if len(caller.busNames) > 0 {
		list = strings.Join(caller.busNames, ", ")
	}
	fmt.Fprintf(stderr, "warning: owner %q matches neither this tab (%q) nor your agentbus names (%s); the user finds you by tab\n", owner, caller.tab, list)
	return owner
}
