package main

import (
	"bytes"
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

const (
	tunnelLaunchdLabel = "com.agentbus.tunnel"
	tunnelUnitName     = "agentbus-tunnel.service"
	outboxLaunchdLabel = "com.agentbus.outbox"
	outboxUnitName     = "agentbus-outbox.service"
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
	if cfg.HubSocket == "" {
		socket, err := discoverHubSocket(ctx, cfg, env, stdout)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus install: %s\n", err)
			return 1
		}
		cfg.HubSocket = socket
	}
	path, err := installTunnel(cfg, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install: %s\n", err)
		return 1
	}
	outboxPath, err := installOutbox(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus install: wrote %s\n%s\n", path, err)
		return 1
	}
	units := [][2]string{{path, tunnelUnitName}, {outboxPath, outboxUnitName}}
	// The hibernation policy is opt-in: its unit is written only while
	// hibernate_policy is on or dry-run.
	if cfg.Policy().Mode != hibernate.PolicyOff {
		policy, err := installPolicy(env)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus install: wrote %s\n%s\n", path, err)
			return 1
		}
		units = append(units, [2]string{policy, policyUnitName})
	}
	for _, u := range units {
		if envFlag(env, "AGENTBUS_INSTALL_NO_LOAD") {
			fmt.Fprintf(stdout, "wrote %s\n", u[0])
			continue
		}
		if err := loadSupervisor(ctx, runtime.GOOS, u[0], u[1]); err != nil {
			fmt.Fprintf(stderr, "agentbus install: wrote %s\n%s\n", u[0], err)
			return 1
		}
		fmt.Fprintf(stdout, "installed %s\n", u[0])
	}
	return 0
}

const (
	policyLaunchdLabel = "com.agentbus.hibernate-policy"
	policyUnitName     = "agentbus-hibernate-policy.service"
	// policyLogName is the launchd job's stderr, in the state directory.
	policyLogName = "hibernate-policy.log"
)

// installPolicy writes the supervised `agentbus hibernate --policy` of
// a hub-mode client. It gets PATH, where it finds herdr and ssh.
func installPolicy(env []string) (string, error) {
	home, err := homeFrom(env)
	if err != nil {
		return "", err
	}
	exe, err := os.Executable()
	if err != nil {
		return "", fmt.Errorf("cannot find this executable: %w", err)
	}
	state, err := paths.StateDir(env)
	if err != nil {
		return "", fmt.Errorf("locate the policy log: %w", err)
	}
	if err := os.MkdirAll(state, 0o700); err != nil {
		return "", fmt.Errorf("create the state dir: %w", err)
	}
	vars := supervisorEnv(env)
	if p := envGet(env, "PATH"); p != "" {
		vars = append(vars, [2]string{"PATH", p})
	}
	path, err := policyPath(runtime.GOOS, home, env)
	if err != nil {
		return "", err
	}
	body, err := policyBody(runtime.GOOS, exe, filepath.Join(state, policyLogName), vars)
	if err != nil {
		return "", err
	}
	if err := writeAtomic(path, []byte(body), 0o644); err != nil {
		return "", err
	}
	return path, nil
}

func policyPath(goos, home string, env []string) (string, error) {
	return unitPath(goos, home, env, policyLaunchdLabel, policyUnitName)
}

// policyBody is the unit that runs `agentbus hibernate --policy` and
// restarts it when it fails. It exits 0 when hibernate_policy is off,
// and stays stopped then.
func policyBody(goos, exe, logPath string, envVars [][2]string) (string, error) {
	args := []string{exe, "hibernate", "--policy"}
	switch goos {
	case "darwin":
		return launchdJob{label: policyLaunchdLabel, args: args, env: envVars, onFailure: true, stderr: logPath}.plist()
	case "linux":
		return systemdUnit(systemdService{
			description: "agentbus hibernation policy for this hub-mode client",
			exec:        args,
			restart:     "on-failure",
			restartSec:  "30",
		}, envVars), nil
	default:
		return "", fmt.Errorf("agentbus install supports systemd and launchd, not %s", goos)
	}
}

// pathReport is the JSON of `agentbus paths --json`.
type pathReport struct {
	Socket       string `json:"socket"`
	HubSocket    string `json:"hub_socket"`    //nolint:tagliatelle // paths JSON shape
	TunnelSocket string `json:"tunnel_socket"` //nolint:tagliatelle // paths JSON shape
	StateDir     string `json:"state_dir"`     //nolint:tagliatelle // paths JSON shape
}

func cmdPaths(_ context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("paths", flag.ContinueOnError)
	fs.SetOutput(stderr)
	asJSON := fs.Bool("json", false, "print socket, hub_socket, tunnel_socket, and state_dir")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if !*asJSON || len(fs.Args()) != 0 {
		fmt.Fprintln(stderr, "usage: agentbus paths --json")
		return 2
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus paths: %s\n", err)
		return 1
	}
	body, err := json.Marshal(pathReport{
		Socket:       dirs.SocketPath,
		HubSocket:    dirs.HubSocketPath,
		TunnelSocket: dirs.TunnelSocketPath,
		StateDir:     dirs.StateDir,
	})
	if err != nil {
		fmt.Fprintf(stderr, "agentbus paths: %s\n", err)
		return 1
	}
	fmt.Fprintf(stdout, "%s\n", body)
	return 0
}

// discoverHubSocket asks the hub for its hub.sock, writes that path into
// this machine's config, and prints the line it wrote.
func discoverHubSocket(ctx context.Context, cfg config.Config, env []string, stdout io.Writer) (string, error) {
	socket, err := queryHubSocket(ctx, cfg)
	if err != nil {
		return "", hubSocketMissing(cfg, env)
	}
	if err := rememberHubSocket(env, cfg.Hub, socket); err != nil {
		return "", err
	}
	fmt.Fprint(stdout, hubSocketLine(cfg.Hub, socket))
	return socket, nil
}

func hubSocketLine(hub, path string) string {
	return fmt.Sprintf("hub_socket = %q  # discovered from %s\n", path, hub)
}

// queryHubSocket runs `agentbus paths --json` on the hub and returns its
// hub_socket. The hub's value is an absolute path with no colon.
func queryHubSocket(ctx context.Context, cfg config.Config) (string, error) {
	var buf bytes.Buffer
	hub := hubcall.Hub{Target: cfg.Hub, Tool: "agentbus", ConnectTimeout: cfg.RemoteTimeout}
	code, err := hub.Run(ctx, hubcall.Call{Args: []string{"paths", "--json"}, Stdout: &buf, Stderr: io.Discard})
	if err != nil {
		return "", fmt.Errorf("ask %s for hub_socket: %w", cfg.Hub, err)
	}
	if code != 0 {
		return "", fmt.Errorf("ask %s for hub_socket: agentbus paths --json exited %d", cfg.Hub, code)
	}
	var got pathReport
	if err := json.Unmarshal(buf.Bytes(), &got); err != nil {
		return "", fmt.Errorf("ask %s for hub_socket: %w", cfg.Hub, err)
	}
	if !strings.HasPrefix(got.HubSocket, "/") || strings.Contains(got.HubSocket, ":") {
		return "", fmt.Errorf("ask %s for hub_socket: %q", cfg.Hub, got.HubSocket)
	}
	return got.HubSocket, nil
}

func rememberHubSocket(env []string, hub, path string) error {
	cfgPath, err := config.Path(env)
	if err != nil {
		return fmt.Errorf("locate config: %w", err)
	}
	f, err := os.OpenFile(cfgPath, os.O_RDWR|os.O_APPEND, 0o600) //nolint:gosec // the config path comes from XDG or HOME
	if err != nil {
		return fmt.Errorf("open config: %w", err)
	}
	defer func() { _ = f.Close() }()
	info, err := f.Stat()
	if err != nil {
		return fmt.Errorf("stat config: %w", err)
	}
	if info.Size() > 0 {
		if _, err := f.Seek(info.Size()-1, io.SeekStart); err != nil {
			return fmt.Errorf("read config: %w", err)
		}
		last := make([]byte, 1)
		if _, err := f.Read(last); err != nil {
			return fmt.Errorf("read config: %w", err)
		}
		if last[0] != '\n' {
			if _, err := f.Write([]byte("\n")); err != nil {
				return fmt.Errorf("write hub_socket: %w", err)
			}
		}
	}
	if _, err := f.Write([]byte(hubSocketLine(hub, path))); err != nil {
		return fmt.Errorf("write hub_socket: %w", err)
	}
	return nil
}

func hubSocketMissing(cfg config.Config, env []string) error {
	cfgPath, _ := config.Path(env)
	return fmt.Errorf("the daemon runs on the hub %s; to forward to it, set hub_socket = \"<path>\" in %s to the hub's $XDG_RUNTIME_DIR/agentbus/hub.sock\nOn the hub, run `agentbus paths --json`, or `echo $XDG_RUNTIME_DIR/agentbus/hub.sock`", cfg.Hub, cfgPath)
}

func installTunnel(cfg config.Config, env []string) (string, error) {
	if cfg.HubSocket == "" {
		return "", hubSocketMissing(cfg, env)
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

// installOutbox writes the supervised `agentbus outbox flush --watch`
// of a hub-mode client. The client runs no agentbusd.
func installOutbox(env []string) (string, error) {
	home, err := homeFrom(env)
	if err != nil {
		return "", err
	}
	exe, err := os.Executable()
	if err != nil {
		return "", fmt.Errorf("cannot find this executable: %w", err)
	}
	path, err := unitPath(runtime.GOOS, home, env, outboxLaunchdLabel, outboxUnitName)
	if err != nil {
		return "", err
	}
	body, err := outboxUnitBody(runtime.GOOS, exe, supervisorEnv(env))
	if err != nil {
		return "", err
	}
	if err := writeAtomic(path, []byte(body), 0o644); err != nil {
		return "", err
	}
	return path, nil
}

func outboxUnitBody(goos, exe string, envVars [][2]string) (string, error) {
	args := []string{exe, "outbox", "flush", "--watch"}
	switch goos {
	case "darwin":
		return launchdPlist(outboxLaunchdLabel, args, envVars)
	case "linux":
		return systemdUnit(systemdService{
			description: "agentbus outbox flush for this hub-mode client",
			exec:        args,
			restart:     "always",
			restartSec:  "5",
		}, envVars), nil
	default:
		return "", fmt.Errorf("agentbus install supports systemd and launchd, not %s", goos)
	}
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
