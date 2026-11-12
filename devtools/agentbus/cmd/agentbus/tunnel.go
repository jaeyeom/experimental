package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// tunnelProbeTimeout bounds the ping that checks the tunnel answers.
const tunnelProbeTimeout = 2 * time.Second

// tunnelCommands are the commands that only talk to the daemon, so in
// hub mode they can run here against the forwarded hub socket. log
// streams the hub's event log through the daemon.
var tunnelCommands = map[string]bool{
	"log":         true,
	"register":    true,
	"describe":    true,
	"whoami":      true,
	"release":     true,
	"list":        true,
	"search":      true,
	"send":        true,
	"reply":       true,
	"show":        true,
	"thread":      true,
	"status":      true,
	"wake":        true,
	"hook":        true,
	"prompt-hook": true,
	// lease runs here even in hub mode and opens the tunnel itself for
	// its daemon requests; see leaseHubRoute.
	"lease": true,
	// hibernate, likewise; see hibernateHubRoute.
	"hibernate": true,
	// mem, likewise, for names and leases; see memHubBus.
	"mem": true,
}

// tunnel is a forwarded hub socket and the caller handle every request
// on it carries.
type tunnel struct {
	socket string
	handle string
}

type tunnelKey struct{}

// stamp makes req a hub-socket request: the protocol version and caller
// handle, and none of this machine's environment.
func (t tunnel) stamp(req daemon.Request) daemon.Request {
	req.Protocol = daemon.ProtocolVersion
	req.Handle = t.handle
	req.Env = nil
	return req
}

func tunnelFrom(ctx context.Context) (tunnel, bool) {
	t, ok := ctx.Value(tunnelKey{}).(tunnel)
	return t, ok
}

// runHubMode runs cmd here against the hub's daemon over the tunnel when
// it can, and otherwise runs the whole command on the hub over ssh.
func runHubMode(ctx context.Context, cfg config.Config, cmd command, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	if len(args) > 0 && args[0] == "install" {
		return cmdInstallTunnel(ctx, cfg, args[1:], env, stdout, stderr)
	}
	if len(args) == 0 {
		return runOnHub(ctx, cfg, args, env, stdin, stdout, stderr)
	}
	if args[0] == "register" && envGet(env, "HERDR_PANE_ID") != "" && cfg.Machine != "" {
		refreshPaneHandle(ctx, env, stderr)
	}
	tunnelled, ok, err := openTunnel(ctx, cfg, args[0], env, stderr)
	if err != nil {
		if hookCommands[args[0]] {
			return 0
		}
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	if ok {
		return cmd.run(tunnelled, args[1:], env, stdin, stdout, stderr)
	}
	return runOnHub(ctx, cfg, args, env, stdin, stdout, stderr)
}

// openTunnel returns ctx routed to the hub over the tunnel socket when
// command name can use it, this caller is a herdr pane, and the hub
// daemon answers there in this protocol. Otherwise the caller uses the
// ssh runner, except inside a sandbox: there a tunnel socket that is
// present but fails is an error, so the call never needs ~/.ssh. A
// connect the OS denies counts as a sandbox. A protocol mismatch is
// reported on stderr.
func openTunnel(ctx context.Context, cfg config.Config, name string, env []string, stderr io.Writer) (context.Context, bool, error) {
	if !tunnelCommands[name] || cfg.Machine == "" || envGet(env, "HERDR_PANE_ID") == "" {
		return ctx, false, nil
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return ctx, false, nil
	}
	// A missing socket is the common case: check it before asking herdr.
	if _, err := os.Stat(dirs.TunnelSocketPath); err != nil {
		return ctx, false, nil
	}
	sandboxed := inSandbox(env)
	caller, err := callerHandle(ctx, cfg, env)
	if err != nil {
		if sandboxed {
			return ctx, false, err
		}
		return ctx, false, nil
	}
	t := tunnel{socket: dirs.TunnelSocketPath, handle: caller}
	err = t.ping(ctx)
	if err == nil {
		return context.WithValue(ctx, tunnelKey{}, t), true, nil
	}
	if sandboxed || deniedBySandbox(err) {
		return ctx, false, tunnelError(t.socket, err)
	}
	var mismatch *protocolMismatchError
	if errors.As(err, &mismatch) && !hookCommands[name] {
		fmt.Fprintf(stderr, "agentbus: %s\n", mismatch)
	}
	return ctx, false, nil
}

// protocolMismatchError is a hub daemon that speaks another protocol.
type protocolMismatchError struct {
	hub int
}

func (e *protocolMismatchError) Error() string {
	return fmt.Sprintf("hub daemon speaks protocol %d, this client speaks %d", e.hub, daemon.ProtocolVersion)
}

// ping checks that the hub daemon answers on the tunnel in this protocol.
func (t tunnel) ping(ctx context.Context) error {
	ctx, cancel := context.WithTimeout(ctx, tunnelProbeTimeout)
	defer cancel()
	resp, err := dialDaemon(ctx, t.socket, t.stamp(daemon.Request{Op: "ping"}))
	switch {
	case err != nil:
		return err
	case resp.Protocol != daemon.ProtocolVersion:
		return &protocolMismatchError{hub: resp.Protocol}
	case !resp.OK:
		return fmt.Errorf("ping refused: %s", resp.Error)
	}
	return nil
}

// dialBus sends one request to the daemon this command uses: the hub
// over the tunnel or, for lease requests, over ssh; or the local daemon.
// It never starts a daemon.
func dialBus(ctx context.Context, env []string, req daemon.Request) (daemon.Response, error) {
	if t, ok := tunnelFrom(ctx); ok {
		return dialDaemon(ctx, t.socket, t.stamp(req))
	}
	if r, ok := sshRouteFrom(ctx); ok {
		return r.call(ctx, req)
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return daemon.Response{}, fmt.Errorf("locate the daemon: %w", err)
	}
	return dialDaemon(ctx, dirs.SocketPath, req)
}

// streamLog prints the hub's event log lines that match filter, and with
// follow keeps printing new ones until ctx ends.
func (t tunnel) streamLog(ctx context.Context, filter eventlog.Filter, follow bool, emit func(string) error) error {
	req := daemon.Request{
		Op:     "log",
		Name:   filter.Name,
		MsgID:  filter.MsgID,
		Event:  filter.Event,
		Follow: follow,
	}
	if !filter.Since.IsZero() {
		req.Since = filter.Since.Format(time.RFC3339Nano)
	}
	err := client.Stream(ctx, t.socket, t.stamp(req), func(resp daemon.Response) error {
		return emit(resp.Text)
	})
	if err != nil {
		return fmt.Errorf("hub log: %w", err)
	}
	return nil
}
