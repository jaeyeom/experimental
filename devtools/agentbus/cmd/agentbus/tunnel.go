package main

import (
	"context"
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
	"hook":        true,
	"prompt-hook": true,
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
	if len(args) > 0 {
		if tunnelled, ok := openTunnel(ctx, cfg, args[0], env, stderr); ok {
			return cmd.run(tunnelled, args[1:], env, stdin, stdout, stderr)
		}
	}
	return runOnHub(ctx, cfg, args, env, stdin, stdout, stderr)
}

// openTunnel returns ctx routed to the hub over the tunnel socket when
// command name can use it, this caller is a herdr pane, and the hub
// daemon answers there in this protocol. Otherwise the caller uses the
// ssh runner. A protocol mismatch is reported on stderr.
func openTunnel(ctx context.Context, cfg config.Config, name string, env []string, stderr io.Writer) (context.Context, bool) {
	if !tunnelCommands[name] || cfg.Machine == "" || envGet(env, "HERDR_PANE_ID") == "" {
		return ctx, false
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return ctx, false
	}
	// A missing socket is the common case: check it before asking herdr.
	if _, err := os.Stat(dirs.TunnelSocketPath); err != nil {
		return ctx, false
	}
	caller, err := callerHandle(ctx, cfg, env)
	if err != nil {
		return ctx, false
	}
	t := tunnel{socket: dirs.TunnelSocketPath, handle: caller}
	probeCtx, cancel := context.WithTimeout(ctx, tunnelProbeTimeout)
	defer cancel()
	resp, err := dialDaemon(probeCtx, t.socket, t.stamp(daemon.Request{Op: "ping"}))
	if err != nil {
		return ctx, false
	}
	if resp.Protocol != daemon.ProtocolVersion {
		if !hookCommands[name] {
			fmt.Fprintf(stderr, "agentbus: hub daemon speaks protocol %d, this client speaks %d\n", resp.Protocol, daemon.ProtocolVersion)
		}
		return ctx, false
	}
	if !resp.OK {
		return ctx, false
	}
	return context.WithValue(ctx, tunnelKey{}, t), true
}

// dialBus sends one request to the daemon this command uses: the hub
// over the tunnel, or the local daemon. It never starts a daemon.
func dialBus(ctx context.Context, env []string, req daemon.Request) (daemon.Response, error) {
	if t, ok := tunnelFrom(ctx); ok {
		return dialDaemon(ctx, t.socket, t.stamp(req))
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
