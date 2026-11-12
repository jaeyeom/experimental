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

// asker is who runs an ask: the herdr tab label, empty outside herdr,
// and a lookup of its agentbus names.
type asker struct {
	tab   string
	names func() []string
}

// askerOf finds the asker. On the hub, a trusted hub-mode caller's tab
// label and names come from the environment the client sent. Otherwise
// they come from this machine's herdr pane, if any.
func askerOf(ctx context.Context) asker {
	if caller, err := hubcall.TrustedCaller(os.Environ(), asHandleEnv); err == nil && caller != "" {
		names := splitNames(os.Getenv(busNamesEnv))
		return asker{tab: strings.TrimSpace(os.Getenv(tabLabelEnv)), names: func() []string { return names }}
	}
	pane := strings.TrimSpace(os.Getenv("HERDR_PANE_ID"))
	if pane == "" {
		return asker{}
	}
	return asker{tab: paneTabLabel(ctx, pane), names: func() []string { return localBusNames(ctx) }}
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

// askOwner is owner, or the asker's tab label when owner is empty. An
// owner that is neither the tab label nor one of the asker's agentbus
// names gets a warning on stderr. Outside herdr, owner is unchanged.
func askOwner(owner string, a asker, stderr io.Writer) string {
	if a.tab == "" {
		return owner
	}
	if strings.TrimSpace(owner) == "" {
		return a.tab
	}
	if owner == a.tab {
		return owner
	}
	names := a.names()
	if slices.Contains(names, owner) {
		return owner
	}
	list := "none"
	if len(names) > 0 {
		list = strings.Join(names, ", ")
	}
	fmt.Fprintf(stderr, "warning: owner %q matches neither this tab (%q) nor your agentbus names (%s); the user finds you by tab\n", owner, a.tab, list)
	return owner
}
