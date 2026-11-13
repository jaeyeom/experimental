package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

// identifyTimeout bounds the local herdr call that names this pane.
const identifyTimeout = 5 * time.Second

// hubEnv overrides the config's hub for one shell.
const hubEnv = "AGENTBUS_HUB"

// identityCommands identify the caller, so hub mode passes the caller's
// handle on this machine to the hub.
var identityCommands = map[string]bool{
	"register": true,
	"describe": true,
	"whoami":   true,
	"release":  true,
	"send":     true,
	"reply":    true,
}

// localCommands run on this machine even in hub mode.
var localCommands = map[string]bool{
	"install-hooks": true,
	// lease reads lease_checkouts and git here; it refuses hub mode itself.
	"lease": true,
	// mem reports this machine's panes and processes, and asks the hub
	// itself for names and leases.
	"mem": true,
	// hibernate reads this machine's panes and processes, and opens
	// the route to the hub itself for its daemon requests.
	"hibernate": true,
	// doctor checks this machine's sockets and the route to the hub.
	"doctor": true,
	// paths prints this machine's sockets. install asks the hub to run it.
	"paths": true,
	// outbox is this machine's queue of hub sends.
	"outbox": true,
}

// stdinCommands read stdin, so hub mode forwards it.
var stdinCommands = map[string]bool{
	"send":        true,
	"hook":        true,
	"prompt-hook": true,
}

// hookCommands exit 0 when the hub cannot be reached, as they do when
// the local bus is down: a down hub must not block a tool call or prompt.
var hookCommands = map[string]bool{
	"hook":        true,
	"prompt-hook": true,
}

// hubSettings reads the hub target and this machine's label. An empty
// target means hub mode is off.
func hubSettings(env []string) (config.Config, error) {
	var cfg config.Config
	// Without HOME there is no config file to read.
	if _, err := config.Path(env); err == nil {
		if cfg, err = config.Load(env); err != nil {
			return config.Config{}, fmt.Errorf("config: %w", err)
		}
	}
	if hub := strings.TrimSpace(envGet(env, hubEnv)); hub != "" {
		if strings.HasPrefix(hub, "-") {
			return config.Config{}, fmt.Errorf("%s must not start with -", hubEnv)
		}
		cfg.Hub = hub
	}
	return cfg, nil
}

// runOnHub runs one agentbus command on cfg.Hub over ssh. It never
// touches a local daemon or local state.
func runOnHub(ctx context.Context, cfg config.Config, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	name := ""
	if len(args) > 0 {
		name = args[0]
	}
	call := hubcall.Call{Args: args, Stdout: stdout, Stderr: stderr}
	// Only these read stdin. ssh would otherwise drain the caller's stdin.
	if stdinCommands[name] {
		call.Stdin = stdin
	}
	if hookCommands[name] {
		call.Stderr = io.Discard
	}
	if name == "send" {
		var code int
		if call, code = sendFileOnStdin(call, stderr); code != 0 {
			return code
		}
	}
	if identityCommands[name] && envGet(env, "HERDR_PANE_ID") != "" {
		caller, err := callerHandle(ctx, cfg, env)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus: %s\n", err)
			return 1
		}
		call.Env = []string{herdr.AsHandleEnv + "=" + caller}
	}
	hub := hubcall.Hub{Target: cfg.Hub, Tool: "agentbus", ConnectTimeout: cfg.RemoteTimeout}
	if hookCommands[name] {
		return runHookOnHub(ctx, hub, call, env)
	}
	code, err := hub.Run(ctx, call)
	if err == nil {
		return code
	}
	fmt.Fprintf(stderr, "agentbus: %s\n", sshHint(env, cfg, err))
	return code
}

// hookTimeoutEnv overrides the hub-mode hook deadline, such as "3s".
const hookTimeoutEnv = "AGENTBUS_HOOK_TIMEOUT"

// A hook in hub mode connects within hookConnectTimeout and finishes
// within defaultHookTimeout, so a hung hub never stalls the agent.
const (
	hookConnectTimeout = 2 * time.Second
	defaultHookTimeout = 3 * time.Second
)

// runHookOnHub runs a hook on the hub under one deadline for the whole
// call. A hook that cannot reach the hub or does not finish in time
// exits 0, as it does when the local bus is down.
func runHookOnHub(ctx context.Context, hub hubcall.Hub, call hubcall.Call, env []string) int {
	total := hubHookTimeout(env)
	hub = hookHub(hub, total)
	ctx, cancel := context.WithTimeout(ctx, total)
	defer cancel()
	code, err := hub.Run(ctx, call)
	if err != nil || ctx.Err() != nil {
		return 0
	}
	return code
}

// hookHub bounds hub by the hook deadline total: ssh connects within
// hookConnectTimeout, or total when that is shorter, and the hub's
// shell starts within total.
func hookHub(hub hubcall.Hub, total time.Duration) hubcall.Hub {
	hub.ConnectTimeout = min(hookConnectTimeout, total)
	hub.Deadline = total
	return hub
}

// hubHookTimeout is AGENTBUS_HOOK_TIMEOUT, or defaultHookTimeout when it is
// unset or not a positive duration. A hook has no stderr to report a
// bad value on.
func hubHookTimeout(env []string) time.Duration {
	if d, err := time.ParseDuration(strings.TrimSpace(envGet(env, hookTimeoutEnv))); err == nil && d > 0 {
		return d
	}
	return defaultHookTimeout
}

// sendFileOnStdin reads a --file path here and sends its content on
// stdin, so the hub never needs the path.
func sendFileOnStdin(call hubcall.Call, stderr io.Writer) (hubcall.Call, int) {
	parsed, err := parseSend(call.Args[1:])
	if err != nil || parsed.file == "" {
		return call, 0
	}
	_, rest, _, _ := takeFlag(call.Args[1:], "--file")
	data, err := os.ReadFile(parsed.file)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus send: %s\n", err)
		return call, 1
	}
	call.Args = append([]string{"send"}, rest...)
	call.Stdin = strings.NewReader(string(data))
	return call, 0
}

// callerHandle is herdr:<machine>/<session>/<pane> for this pane.
func callerHandle(ctx context.Context, cfg config.Config, env []string) (string, error) {
	if cfg.Machine == "" {
		path, _ := config.Path(env)
		return "", fmt.Errorf("hub mode needs machine = \"<label>\" in %s, this machine's herdr label on the hub", path)
	}
	local, ok := cachedPaneHandle(env)
	if !ok {
		var err error
		if local, err = lookupPaneHandle(ctx, env); err != nil {
			return "", fmt.Errorf("identify this pane: %w", err)
		}
	}
	caller, err := herdr.OnMachine(local, cfg.Machine)
	if err != nil {
		return "", fmt.Errorf("identify this pane: %w", err)
	}
	return caller, nil
}

// lookupPaneHandle asks the local herdr server for this pane's handle,
// herdr:local/<session>/<pane>. A sandbox that denies the herdr socket
// gets an error naming it.
func lookupPaneHandle(ctx context.Context, env []string) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, identifyTimeout)
	defer cancel()
	local, err := herdr.LocalHandle(ctx, env)
	if err != nil {
		return "", explainBlocked(err, herdrSocket(env))
	}
	return local, nil
}

// refreshPaneHandle asks herdr for this pane's handle again and caches
// it, so later calls from a sandbox that denies the herdr socket still
// know their caller. When herdr cannot answer, the cache stays.
func refreshPaneHandle(ctx context.Context, env []string, stderr io.Writer) {
	local, err := lookupPaneHandle(ctx, env)
	if err != nil {
		return
	}
	if err := cachePaneHandle(env, local); err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
	}
}
