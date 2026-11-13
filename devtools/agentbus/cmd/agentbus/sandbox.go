package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"net/url"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"golang.org/x/sys/unix"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// sandboxEnv forces sandbox detection on ("1") or off ("0").
const sandboxEnv = "AGENTBUS_SANDBOX"

// herdrSocketEnv is where herdr tells its panes the server's socket is.
const herdrSocketEnv = "HERDR_SOCKET_PATH"

// socketHint ends an error about a unix socket a sandbox denied.
const socketHint = "(sandbox? allow this unix socket)"

// inSandbox reports whether this process runs inside an agent sandbox:
// AGENTBUS_SANDBOX=1, or SANDBOX_RUNTIME, which Claude Code's sandbox
// sets, unless AGENTBUS_SANDBOX=0.
func inSandbox(env []string) bool {
	switch strings.TrimSpace(envGet(env, sandboxEnv)) {
	case "1", "true":
		return true
	case "0", "false":
		return false
	}
	return envGet(env, "SANDBOX_RUNTIME") != ""
}

// deniedBySandbox reports whether err is a connect the OS refused with
// EPERM, as a sandbox does. herdr reports it only as text.
func deniedBySandbox(err error) bool {
	return errors.Is(err, unix.EPERM) || strings.Contains(strings.ToLower(err.Error()), "operation not permitted")
}

// blockedSocketError is a connect to Socket that a sandbox denied.
type blockedSocketError struct {
	Socket string
	Err    error
}

func (e *blockedSocketError) Error() string {
	return "connect " + e.Socket + ": operation not permitted " + socketHint
}

func (e *blockedSocketError) Unwrap() error { return e.Err }

// explainBlocked names socket and what to allow when a sandbox denied
// err, and returns any other err as it is.
func explainBlocked(err error, socket string) error {
	if err == nil || !deniedBySandbox(err) {
		return err
	}
	return &blockedSocketError{Socket: socket, Err: err}
}

// herdrSocket is the herdr server's socket this pane uses, for messages.
func herdrSocket(env []string) string {
	if path := envGet(env, herdrSocketEnv); path != "" {
		return path
	}
	return "the herdr socket (" + herdrSocketEnv + " is not set)"
}

// tunnelError is a failure on the tunnel socket that a sandboxed call
// does not paper over with ssh.
func tunnelError(socket string, err error) error {
	return fmt.Errorf("reach the hub over the tunnel: %w; not falling back to ssh inside a sandbox (is the tunnel up? agentbus install restarts it)", explainBlocked(err, socket))
}

// sshHint adds what a sandbox must allow to a failed ssh call to the hub.
func sshHint(env []string, cfg config.Config, err error) error {
	var ue *hubcall.UnreachableError
	if !inSandbox(env) || (!errors.As(err, &ue) && !errors.Is(err, hubcall.ErrNoSSH)) {
		return err
	}
	tunnel := "$XDG_RUNTIME_DIR/agentbus/tunnel.sock"
	if dirs, derr := paths.FromEnv(env); derr == nil {
		tunnel = dirs.TunnelSocketPath
	}
	return fmt.Errorf("%w (sandbox? ssh needs network access to %s and read access to ~/.ssh; or use the tunnel: agentbus install, then allow the unix socket %s; see agentbus doctor --sandbox)", err, hubHost(cfg.Hub), tunnel)
}

// hubHost is the host in an ssh target such as "me@hub".
func hubHost(target string) string {
	if _, host, ok := strings.Cut(target, "@"); ok {
		return host
	}
	return target
}

// paneCachePath is the file that caches the handle of pane, or "" when
// there is no runtime directory or pane is not a usable file name.
func paneCachePath(env []string, pane string) string {
	if pane == "" || pane == "." || pane == ".." {
		return ""
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return ""
	}
	return filepath.Join(dirs.PaneCacheDir(), url.PathEscape(pane))
}

// cachedPaneHandle is this pane's herdr:local/<session>/<pane> handle
// as register cached it, when there is one.
func cachedPaneHandle(env []string) (string, bool) {
	pane := envGet(env, "HERDR_PANE_ID")
	path := paneCachePath(env, pane)
	if path == "" {
		return "", false
	}
	data, err := os.ReadFile(path) //nolint:gosec // our own cache under the runtime directory
	if err != nil {
		return "", false
	}
	handle := strings.TrimSpace(string(data))
	machine, _, cached, err := hubcall.ParseHandle(handle)
	if err != nil || machine != hubcall.LocalMachine || cached != pane {
		return "", false
	}
	return handle, true
}

// cachePaneHandle writes this pane's local handle to its 0600 cache file.
func cachePaneHandle(env []string, handle string) error {
	path := paneCachePath(env, envGet(env, "HERDR_PANE_ID"))
	if path == "" {
		return nil
	}
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("cache this pane's handle: %w", err)
	}
	tmp, err := os.CreateTemp(dir, ".pane-*")
	if err != nil {
		return fmt.Errorf("cache this pane's handle: %w", err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(handle + "\n"); err != nil {
		tmp.Close()
		return fmt.Errorf("cache this pane's handle: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("cache this pane's handle: %w", err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("cache this pane's handle: %w", err)
	}
	return nil
}

// doctorProbe tells whether the hub's ssh port answers; tests replace it.
var doctorProbe = hubcall.TCPProbe

// doctorSocket is one unix socket doctor lists and checks; path is ""
// when it is not known.
type doctorSocket struct{ label, path string }

// doctorDialTimeout bounds each socket check.
const doctorDialTimeout = time.Second

func cmdDoctor(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("doctor", flag.ContinueOnError)
	fs.SetOutput(stderr)
	sandbox := fs.Bool("sandbox", false, "print the sandbox allowlist this machine needs and test each connection")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if len(fs.Args()) != 0 {
		fmt.Fprintln(stderr, "usage: agentbus doctor [--sandbox]")
		return 2
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus doctor: %s\n", err)
		return 1
	}
	if err := reportExtraDaemons(ctx, dirs.StateDir, stdout); err != nil {
		fmt.Fprintf(stderr, "agentbus doctor: %s\n", err)
		return 1
	}
	reportStaleOutbox(env, stdout)
	reportUnverifiedHolds(dirs.StateDir, stdout)
	reportHubSocket(ctx, env, stdout)
	if !*sandbox {
		return 0
	}
	cfg, err := hubSettings(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus doctor: %s\n", err)
		return 1
	}
	sockets := []doctorSocket{
		{"herdr socket", envGet(env, herdrSocketEnv)},
		{"tunnel socket", dirs.TunnelSocketPath},
		{"daemon socket", dirs.SocketPath},
	}
	network := map[string][]string{"allowUnixSockets": {}}
	for _, s := range sockets {
		if s.path != "" {
			network["allowUnixSockets"] = append(network["allowUnixSockets"], s.path)
		}
	}
	if cfg.Hub != "" {
		network["allowedDomains"] = []string{hubHost(cfg.Hub)}
	}
	body, err := json.MarshalIndent(map[string]any{"sandbox": map[string]any{"network": network}}, "", "  ")
	if err != nil {
		fmt.Fprintf(stderr, "agentbus doctor: %s\n", err)
		return 1
	}
	fmt.Fprintf(stdout, "Claude Code sandbox settings this machine needs (merge into settings.json):\n%s\n", body)
	if cfg.Hub != "" {
		fmt.Fprintf(stdout, "allowedDomains is only for the ssh fallback to %s, which also reads ~/.ssh; the tunnel needs neither.\n", cfg.Hub)
	}
	printDoctorChecks(ctx, stdout, cfg, sockets)
	return 0
}

// reportUnverifiedHolds prints messages whose paste may have landed and
// whose screen could not be checked. A missing mailbox prints nothing.
func reportUnverifiedHolds(dir string, stdout io.Writer) {
	lines, err := mailbox.UnverifiedHolds(dir)
	if err != nil {
		return
	}
	for _, line := range lines {
		fmt.Fprintln(stdout, line)
	}
}

// reportHubSocket says when the configured hub_socket is not the path
// the hub reports. A query that fails leaves the report unchanged.
func reportHubSocket(ctx context.Context, env []string, stdout io.Writer) {
	cfg, err := hubSettings(env)
	if err != nil || cfg.Hub == "" || cfg.HubSocket == "" {
		return
	}
	got, err := queryHubSocket(ctx, cfg)
	if err != nil || got == cfg.HubSocket {
		return
	}
	fmt.Fprintf(stdout, "hub_socket: configured %s but the hub reports %s\n", cfg.HubSocket, got)
}

// printDoctorChecks reports each socket and the hub's ssh port as ok or
// not.
func printDoctorChecks(ctx context.Context, stdout io.Writer, cfg config.Config, sockets []doctorSocket) {
	fmt.Fprintln(stdout, "\nChecks:")
	for _, s := range sockets {
		if s.path == "" {
			fmt.Fprintf(stdout, "  %s: unknown (%s is not set; run this in a herdr pane)\n", s.label, herdrSocketEnv)
			continue
		}
		fmt.Fprintf(stdout, "  %s %s: %s\n", s.label, s.path, checkSocket(ctx, s.path))
	}
	switch {
	case cfg.Hub == "":
		fmt.Fprintln(stdout, "  ssh: not in hub mode")
	case doctorProbe(cfg.Hub)(ctx):
		fmt.Fprintf(stdout, "  ssh %s: ok (its ssh port answers)\n", cfg.Hub)
	default:
		fmt.Fprintf(stdout, "  ssh %s: blocked (its ssh port does not answer; sandbox? allow %s)\n", cfg.Hub, hubHost(cfg.Hub))
	}
}

// checkSocket connects to the unix socket at path and reports ok,
// missing, blocked, or the failure.
func checkSocket(ctx context.Context, path string) string {
	if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
		return "missing"
	}
	ctx, cancel := context.WithTimeout(ctx, doctorDialTimeout)
	defer cancel()
	var d net.Dialer
	conn, err := d.DialContext(ctx, "unix", path)
	if err != nil {
		if deniedBySandbox(err) {
			return "blocked " + socketHint
		}
		return "failed: " + err.Error()
	}
	conn.Close()
	return "ok"
}

// doctorProc is one process doctor may report. Start is zero when unknown.
type doctorProc struct {
	PID   int
	UID   int
	Start time.Time
	Name  string
}

// listDoctorProcs lists processes. Tests replace it.
var listDoctorProcs = func(ctx context.Context) ([]doctorProc, error) {
	out, err := procinfo.Run(ctx, "ps", "-ax", "-o", "pid=,uid=,etime=,comm=")
	if err != nil {
		return nil, fmt.Errorf("list processes: %w", err)
	}
	return parseDoctorPS(out, time.Now()), nil
}

// daemonLockHolder reports the daemon lock's holder. Tests replace it.
var daemonLockHolder = daemon.LockHolder

// parseDoctorPS parses `ps -ax -o pid=,uid=,etime=,comm=`.
func parseDoctorPS(out string, now time.Time) []doctorProc {
	var procs []doctorProc
	for _, line := range strings.Split(out, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 4 {
			continue
		}
		pid, err1 := strconv.Atoi(fields[0])
		uid, err2 := strconv.Atoi(fields[1])
		if err1 != nil || err2 != nil {
			continue
		}
		proc := doctorProc{PID: pid, UID: uid, Name: fields[3]}
		if age := procinfo.ParseEtime(fields[2]); age >= 0 {
			proc.Start = now.Add(-age).UTC().Truncate(time.Second)
		}
		procs = append(procs, proc)
	}
	return procs
}

// reportExtraDaemons writes agentbusd processes for this user other than
// the lock holder, and suggests stopping them. It does not signal them.
func reportExtraDaemons(ctx context.Context, stateDir string, stdout io.Writer) error {
	procs, err := listDoctorProcs(ctx)
	if err != nil {
		return err
	}
	holder, held, err := daemonLockHolder(stateDir)
	if err != nil {
		return err
	}
	uid := os.Getuid()
	var extra []doctorProc
	for _, p := range procs {
		if filepath.Base(p.Name) != "agentbusd" || p.UID != uid {
			continue
		}
		if held && p.PID == holder {
			continue
		}
		extra = append(extra, p)
	}
	if len(extra) == 0 {
		fmt.Fprintln(stdout, "agentbusd: no extra daemon processes")
		return nil
	}
	if held {
		fmt.Fprintf(stdout, "agentbusd processes other than the lock holder (pid %d):\n", holder)
	} else {
		fmt.Fprintln(stdout, "agentbusd processes (no lock holder):")
	}
	pids := make([]string, 0, len(extra))
	for _, p := range extra {
		when := "unknown"
		if !p.Start.IsZero() {
			when = p.Start.Format(time.RFC3339)
		}
		fmt.Fprintf(stdout, "  pid %d started %s\n", p.PID, when)
		pids = append(pids, strconv.Itoa(p.PID))
	}
	fmt.Fprintf(stdout, "Stop them with: kill %s\n", strings.Join(pids, " "))
	return nil
}
