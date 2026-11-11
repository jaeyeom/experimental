package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

const (
	tunnelLaunchdLabel = "com.agentbus.tunnel"
	tunnelUnitName     = "agentbus-tunnel.service"
)

// tunnelSpec is an ssh forward from this machine's tunnel socket to the
// hub daemon's hub socket.
type tunnelSpec struct {
	ssh    string
	target string
	local  string
	remote string
}

// sshArgs is the forward. StreamLocalBindUnlink replaces a stale local
// socket. ExitOnForwardFailure and ServerAliveInterval make a failed
// forward or a dead connection exit, so the supervisor restarts it.
func (s tunnelSpec) sshArgs() []string {
	return []string{
		s.ssh, "-N",
		"-o", "BatchMode=yes",
		"-o", "ExitOnForwardFailure=yes",
		"-o", "StreamLocalBindUnlink=yes",
		"-o", "ServerAliveInterval=30",
		"-L", s.local + ":" + s.remote,
		"--", s.target,
	}
}

// cmdInstallTunnel writes and loads the supervised ssh forward that
// carries hub-mode commands to the hub daemon. The daemon itself runs
// on the hub.
func cmdInstallTunnel(ctx context.Context, cfg config.Config, args, env []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintf(stderr, "agentbus install: unexpected arguments\n")
		return 2
	}
	path, err := installTunnel(cfg, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install: %s\n", err)
		return 1
	}
	if envFlag(env, "AGENTBUS_INSTALL_NO_LOAD") {
		fmt.Fprintf(stdout, "wrote %s\n", path)
		return 0
	}
	if err := loadSupervisor(ctx, runtime.GOOS, path, tunnelUnitName); err != nil {
		fmt.Fprintf(stderr, "agentbus install: wrote %s\n%s\n", path, err)
		return 1
	}
	fmt.Fprintf(stdout, "installed %s\n", path)
	return 0
}

func installTunnel(cfg config.Config, env []string) (string, error) {
	if cfg.HubSocket == "" {
		cfgPath, _ := config.Path(env)
		return "", fmt.Errorf("the daemon runs on the hub %s; to forward to it, set hub_socket = \"<path>\" in %s to the hub's $XDG_RUNTIME_DIR/agentbus/hub.sock", cfg.Hub, cfgPath)
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return "", fmt.Errorf("locate the tunnel socket: %w", err)
	}
	if strings.Contains(dirs.TunnelSocketPath, ":") {
		return "", fmt.Errorf("tunnel socket %s must not contain : for ssh -L", dirs.TunnelSocketPath)
	}
	home, err := homeFrom(env)
	if err != nil {
		return "", err
	}
	ssh, err := exec.LookPath("ssh")
	if err != nil {
		return "", fmt.Errorf("find ssh: %w", err)
	}
	if ssh, err = filepath.Abs(ssh); err != nil {
		return "", fmt.Errorf("find ssh: %w", err)
	}
	spec := tunnelSpec{ssh: ssh, target: cfg.Hub, local: dirs.TunnelSocketPath, remote: cfg.HubSocket}
	path, err := tunnelPath(runtime.GOOS, home, env)
	if err != nil {
		return "", err
	}
	body, err := tunnelBody(runtime.GOOS, spec, supervisorEnv(env))
	if err != nil {
		return "", err
	}
	// ssh -L does not create the socket's directory.
	if err := os.MkdirAll(filepath.Dir(spec.local), 0o700); err != nil {
		return "", fmt.Errorf("create tunnel socket dir: %w", err)
	}
	if err := writeAtomic(path, []byte(body), 0o644); err != nil {
		return "", err
	}
	return path, nil
}

func tunnelPath(goos, home string, env []string) (string, error) {
	return unitPath(goos, home, env, tunnelLaunchdLabel, tunnelUnitName)
}

// tunnelBody is the unit that runs the forward and restarts it when it
// exits. Each start first creates the socket's directory, which a
// reboot may clear.
func tunnelBody(goos string, spec tunnelSpec, envVars [][2]string) (string, error) {
	dir := filepath.Dir(spec.local)
	switch goos {
	case "darwin":
		// launchd has no pre-start step, so a fixed sh script makes the
		// directory and execs ssh. Paths are arguments, never script text.
		args := append([]string{"/bin/sh", "-c", `mkdir -p "$1" && shift && exec "$@"`, "sh", dir}, spec.sshArgs()...)
		return launchdPlist(tunnelLaunchdLabel, args, envVars)
	case "linux":
		return systemdUnit(systemdService{
			description: "agentbus tunnel to the hub daemon",
			pre:         []string{"/bin/mkdir", "-p", dir},
			exec:        spec.sshArgs(),
			restart:     "always",
			restartSec:  "5",
		}, envVars), nil
	default:
		return "", fmt.Errorf("agentbus install supports systemd and launchd, not %s", goos)
	}
}
