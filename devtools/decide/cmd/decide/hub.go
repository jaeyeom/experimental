package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
	"golang.org/x/sys/unix"
)

// localCommands run on this machine even in hub mode.
var localCommands = map[string]bool{
	"help":    true,
	"-h":      true,
	"--help":  true,
	"skill":   true,
	"avatars": true,
}

// asHandleEnv carries the hub-mode caller's herdr handle to the hub, as
// AGENTBUS_AS_HANDLE does for agentbus.
const asHandleEnv = "DECIDE_AS_HANDLE"

// identifyTimeout bounds the local herdr call that names this pane.
const identifyTimeout = 5 * time.Second

// hubSettings is the hub and this machine's herdr label on it.
type hubSettings struct {
	hub     hubcall.Hub
	machine string
}

// hubConfig returns the hub from DECIDE_HUB or the config, and the
// machine from DECIDE_MACHINE or the config. An empty hub Target means
// hub mode is off.
func hubConfig() (hubSettings, error) {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return hubSettings{}, fmt.Errorf("decide hub: %w", err)
	}
	hub := hubcall.Hub{Target: strings.TrimSpace(cfg.Hub), Tool: "decide"}
	if env := strings.TrimSpace(os.Getenv("DECIDE_HUB")); env != "" {
		hub.Target = env
	}
	if strings.HasPrefix(hub.Target, "-") {
		return hubSettings{}, fmt.Errorf("decide hub %q must not start with -", hub.Target)
	}
	if raw := strings.TrimSpace(cfg.HubTimeout); raw != "" {
		d, err := time.ParseDuration(raw)
		if err != nil || d <= 0 {
			return hubSettings{}, fmt.Errorf("decide config hubTimeout must be a positive duration, such as \"5s\"")
		}
		hub.ConnectTimeout = d
	}
	return hubSettings{hub: hub, machine: decide.LocalMachine(cfg)}, nil
}

// maybeRunOnHub runs args on the hub when hub mode is on and reports
// whether it did.
func maybeRunOnHub(args []string, stdout, stderr io.Writer) (int, bool) {
	if len(args) == 0 || localCommands[args[0]] {
		return 0, false
	}
	settings, err := hubConfig()
	if err != nil {
		return fail(stderr, err, 1), true
	}
	if settings.hub.Target == "" {
		return 0, false
	}
	return runOnHub(settings, args, stdout, stderr), true
}

// runOnHub runs one decide command on the hub's ledger over ssh. It
// never reads or creates the local ledger. An ask from a herdr pane
// carries that pane's handle, so the hub notifies it on this machine.
func runOnHub(settings hubSettings, args []string, stdout, stderr io.Writer) int {
	hub := settings.hub
	if args[0] == "inbox" {
		fmt.Fprintf(stderr, "decide inbox: the ledger is on the hub %s; run decide inbox there\n", hub.Target)
		return 1
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	call := hubcall.Call{Args: args, Stdout: stdout, Stderr: stderr}
	if args[0] == "ask" && os.Getenv("HERDR_PANE_ID") != "" {
		caller, err := callerHandle(ctx, settings.machine)
		if err != nil {
			return fail(stderr, err, 1)
		}
		call.Env = append([]string{asHandleEnv + "=" + caller}, clientAskEnv(ctx, os.Getenv("HERDR_PANE_ID"))...)
	}
	code, err := hub.Run(ctx, call)
	if err != nil {
		fmt.Fprintf(stderr, "decide: %s\n", err)
	}
	return code
}

// callerHandle is herdr:<machine>/<session>/<pane> for this pane, the
// handle agentbus sends in hub mode.
func callerHandle(ctx context.Context, machine string) (string, error) {
	if machine == "" {
		return "", fmt.Errorf(`hub mode needs "machine": "<label>" in the decide config or DECIDE_MACHINE, this machine's herdr label on the hub`)
	}
	ctx, cancel := context.WithTimeout(ctx, identifyTimeout)
	defer cancel()
	pane := os.Getenv("HERDR_PANE_ID")
	session, err := paneSession(ctx, pane)
	if err != nil {
		return "", fmt.Errorf("identify this pane: %w", err)
	}
	return hubcall.Handle(machine, session, pane), nil
}

// paneSession is the agent session id of pane in `herdr agent list`.
func paneSession(ctx context.Context, pane string) (string, error) {
	res, err := remotecall.Run(ctx, "herdr", "agent", "list")
	if err != nil {
		return "", fmt.Errorf("herdr agent list: %w", err)
	}
	if res.Code != 0 {
		return "", fmt.Errorf("herdr agent list: exit %d: %s", res.Code, strings.TrimSpace(res.Stderr))
	}
	var list struct {
		Result struct {
			Agents []struct {
				PaneID  string `json:"pane_id"` //nolint:tagliatelle // herdr wire format
				Session struct {
					Value string `json:"value"`
				} `json:"agent_session"` //nolint:tagliatelle // herdr wire format
			} `json:"agents"`
		} `json:"result"`
	}
	if err := json.Unmarshal([]byte(res.Stdout), &list); err != nil {
		return "", fmt.Errorf("decode herdr agent list: %w", err)
	}
	for _, agent := range list.Result.Agents {
		if agent.PaneID != pane {
			continue
		}
		if agent.Session.Value == "" {
			return "", fmt.Errorf("herdr pane %s has no agent session id", pane)
		}
		return agent.Session.Value, nil
	}
	return "", fmt.Errorf("herdr pane %s is not in the agent list", pane)
}

// notifyHubCaller routes item's notifications to the hub-mode caller in
// DECIDE_AS_HANDLE. Without --notify-pane the caller's pane is stored.
// A pane, given or not, is on the caller's machine; an agentbus name is
// on none. Without a caller, item is unchanged.
func notifyHubCaller(item *decide.Item) error {
	caller, err := hubcall.TrustedCaller(os.Environ(), asHandleEnv)
	if err != nil || caller == "" {
		return err //nolint:wrapcheck // the shared trust rule names the variable
	}
	machine, _, pane, err := hubcall.ParseHandle(caller)
	if err != nil {
		return err //nolint:wrapcheck // the shared parser names the handle
	}
	if strings.TrimSpace(item.NotifyPane) == "" {
		item.NotifyPane = pane
	}
	if !strings.HasPrefix(item.NotifyPane, decide.BusPanePrefix) {
		item.NotifyMachine = machine
	}
	return nil
}
