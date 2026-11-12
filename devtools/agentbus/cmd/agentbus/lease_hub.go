package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

// In hub mode a lease command runs here, on the checkout's machine: it
// resolves the checkout, inspects the tree, and runs the git and process
// steps. Only the lease-* daemon requests, carrying what it found, go to
// the hub: over the tunnel socket when the hub answers there, else over
// ssh to `agentbus lease forward`, which hands the request to the hub's
// own daemon. The hub never runs git or looks at processes for a
// checkout on another machine.

// leaseMachine is the machine part of this machine's checkout keys:
// local on the hub, the machine label in hub mode.
func leaseMachine(cfg config.Config, env []string) (string, error) {
	if cfg.Hub == "" {
		return localMachine, nil
	}
	if cfg.Machine == "" {
		path, _ := config.Path(env)
		return "", fmt.Errorf("hub mode needs machine = \"<label>\" in %s, this machine's herdr label on the hub", path)
	}
	return cfg.Machine, nil
}

// checkoutRoot is the path part of a checkout key.
func checkoutRoot(key string) string {
	_, root, _ := strings.Cut(key, ":")
	return root
}

// sshRoute sends lease requests to the hub's daemon through `agentbus
// lease forward` over ssh. handle is the caller's hub-mode handle, empty
// outside herdr (the user).
type sshRoute struct {
	hub    hubcall.Hub
	handle string
}

type sshRouteKey struct{}

func sshRouteFrom(ctx context.Context) (sshRoute, bool) {
	r, ok := ctx.Value(sshRouteKey{}).(sshRoute)
	return r, ok
}

// hubRouted reports whether ctx sends daemon requests to the hub.
func hubRouted(ctx context.Context) bool {
	if _, ok := tunnelFrom(ctx); ok {
		return true
	}
	_, ok := sshRouteFrom(ctx)
	return ok
}

// leaseHubRoute returns ctx routed to the hub for lease requests: the
// tunnel when it answers, else ssh. bound shortens the ssh timeouts for
// lease check and the hook, which must give up within leaseHookTimeout.
func leaseHubRoute(ctx context.Context, cfg config.Config, env []string, bound bool, stderr io.Writer) (context.Context, error) {
	routed, ok, err := openTunnel(ctx, cfg, "lease", env, stderr)
	if err != nil {
		return ctx, err
	}
	if ok {
		return routed, nil
	}
	r := sshRoute{hub: hubcall.Hub{Target: cfg.Hub, Tool: "agentbus", ConnectTimeout: cfg.RemoteTimeout}}
	if bound {
		r.hub.ConnectTimeout = leaseHookTimeout / 3
		r.hub.Deadline = leaseHookTimeout / 2
	}
	if envGet(env, "HERDR_PANE_ID") != "" {
		caller, err := callerHandle(ctx, cfg, env)
		if err != nil {
			return ctx, err
		}
		r.handle = caller
	}
	return context.WithValue(ctx, sshRouteKey{}, r), nil
}

// call runs `agentbus lease forward` on the hub with req, without this
// machine's environment, on stdin, and reads the daemon's response from
// its stdout.
func (r sshRoute) call(ctx context.Context, req daemon.Request) (daemon.Response, error) {
	req.Env, req.Handle, req.Protocol = nil, "", 0
	body, err := json.Marshal(req)
	if err != nil {
		return daemon.Response{}, fmt.Errorf("encode the lease request: %w", err)
	}
	var stdout, stderr bytes.Buffer
	call := hubcall.Call{Args: []string{"lease", "forward"}, Stdin: bytes.NewReader(body), Stdout: &stdout, Stderr: &stderr}
	if r.handle != "" {
		call.Env = []string{herdr.AsHandleEnv + "=" + r.handle}
	}
	code, err := r.hub.Run(ctx, call)
	if err != nil {
		return daemon.Response{}, fmt.Errorf("%w", err)
	}
	if code != 0 {
		return daemon.Response{}, fmt.Errorf("hub %s: agentbus lease forward exited %d: %s", r.hub.Target, code, strings.TrimSpace(stderr.String()))
	}
	var resp daemon.Response
	if err := json.Unmarshal(stdout.Bytes(), &resp); err != nil {
		return daemon.Response{}, fmt.Errorf("hub %s: bad lease reply %q: %w", r.hub.Target, strings.TrimSpace(stdout.String()), err)
	}
	return resp, nil
}

// leaseForward is the hub's end of the ssh route: it reads one lease-*
// request, or one hibernate-* request of `agentbus hibernate`, on stdin, sends it to this machine's daemon with this
// process's environment, where AGENTBUS_AS_HANDLE names the caller and
// is trusted only over ssh, and prints the daemon's response as JSON. It
// exits 0 whenever the daemon answered.
func leaseForward(ctx context.Context, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	fail := func(err error) int {
		fmt.Fprintf(stderr, "agentbus lease forward: %s\n", err)
		return 1
	}
	if len(args) != 0 {
		return fail(errors.New("unexpected arguments"))
	}
	cfg, err := hubSettings(env)
	if err != nil {
		return fail(err)
	}
	if cfg.Hub != "" {
		return fail(errors.New("this machine is in hub mode itself; forward to the hub"))
	}
	var req daemon.Request
	if err := json.NewDecoder(io.LimitReader(stdin, 1<<20)).Decode(&req); err != nil {
		return fail(fmt.Errorf("read the request: %w", err))
	}
	if !strings.HasPrefix(req.Op, "lease-") && !strings.HasPrefix(req.Op, "hibernate-") {
		return fail(fmt.Errorf("only lease-* and hibernate-* requests are forwarded, not %q", req.Op))
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: req.Op, Env: env, Lease: req.Lease, Hibernate: req.Hibernate}, stderr)
	if code != 0 {
		return code
	}
	data, err := json.Marshal(resp)
	if err != nil {
		return fail(err)
	}
	fmt.Fprintf(stdout, "%s\n", data)
	return 0
}

// resolveWith turns pause --with and unshare targets that are herdr pane
// ids on this machine into hub-mode handles, since the hub cannot see
// this machine's panes. Names and handles pass through for the hub.
func resolveWith(ctx context.Context, machine string, targets, env []string) []string {
	out := make([]string, 0, len(targets))
	for _, target := range targets {
		if strings.HasPrefix(target, "herdr:") {
			out = append(out, target)
			continue
		}
		idCtx, cancel := context.WithTimeout(ctx, identifyTimeout)
		local, err := herdr.LocalHandle(idCtx, append(withoutPane(env), "HERDR_PANE_ID="+target))
		cancel()
		if err == nil {
			if handle, err := herdr.OnMachine(local, machine); err == nil {
				out = append(out, handle)
				continue
			}
		}
		out = append(out, target)
	}
	return out
}

func withoutPane(env []string) []string {
	out := make([]string, 0, len(env))
	for _, kv := range env {
		if !strings.HasPrefix(kv, "HERDR_PANE_ID=") {
			out = append(out, kv)
		}
	}
	return out
}
