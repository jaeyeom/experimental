package main

import (
	"context"
	"debug/buildinfo"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// herdrFacts is what doctor knows about herdr without asking the network.
type herdrFacts struct {
	GOOS          string
	UID           int
	PID           int
	Started       string
	Supervisor    string
	DaemonPATH    string
	DaemonHerdr   string
	DaemonHerdrOK bool
	ConfigHerdrOK bool
	CallerHerdr   string
	ExecNotFound  bool
	Checked       bool
}

// diagnoseHerdr reports a herdr the daemon cannot run. An executable
// AGENTBUS_HERDR or herdr_path reports nothing.
func diagnoseHerdr(f herdrFacts) string {
	if f.DaemonHerdrOK || f.ConfigHerdrOK {
		return ""
	}
	callerDir := ""
	if f.CallerHerdr != "" {
		callerDir = filepath.Dir(f.CallerHerdr)
	}
	if callerDir != "" && !pathHasDir(f.DaemonPATH, callerDir) && f.PID > 0 && f.Started != "" && f.Supervisor != "" {
		return fmt.Sprintf("agentbusd (pid %d) was started at %s by %s with a PATH that does not include %s, where herdr is.\nfix: run `agentbus install` from a login shell, then: %s\n",
			f.PID, f.Started, f.Supervisor, callerDir, restartCmd(f))
	}
	if f.CallerHerdr == "" && f.Checked {
		return "herdr is not installed or not on the login PATH.\n"
	}
	if f.ExecNotFound {
		var b strings.Builder
		b.WriteString("herdr: executable file not found in $PATH\n")
		if f.DaemonPATH != "" || f.DaemonHerdr != "" {
			fmt.Fprintf(&b, "unit PATH=%s AGENTBUS_HERDR=%s\n", f.DaemonPATH, f.DaemonHerdr)
		}
		b.WriteString("fix: re-run `agentbus install` from a shell where `which herdr` works, or set `herdr_path`.\n")
		return b.String()
	}
	return ""
}

func pathHasDir(path, dir string) bool {
	want := filepath.Clean(dir)
	for _, part := range filepath.SplitList(path) {
		if part != "" && filepath.Clean(part) == want {
			return true
		}
	}
	return false
}

func restartCmd(f herdrFacts) string {
	if f.GOOS == "darwin" {
		return fmt.Sprintf("launchctl kickstart -k gui/%d/%s", f.UID, launchdLabel)
	}
	return "systemctl --user restart agentbus"
}

// diagnoseSandbox reports a tunnel socket missing from the Claude Code
// sandbox allowlist. A missing settings file is not that case.
func diagnoseSandbox(settingsFile, socket string, allowed []string, present bool) string {
	if !present || socket == "" || slices.Contains(allowed, socket) {
		return ""
	}
	return fmt.Sprintf("sandbox allowUnixSockets in %s is missing %s.\nfix: run setup-claude-sandbox so that socket is allowed.\n", settingsFile, socket)
}

// daemonFacts is the running daemon compared with the installed binary.
type daemonFacts struct {
	Checked        bool
	Running        bool
	PID            int
	RunningBuild   string
	InstalledBuild string
	InstalledPath  string
	Restart        string
}

// diagnoseDaemon reports a daemon that is not running, or one whose
// build id differs from the binary on disk.
func diagnoseDaemon(f daemonFacts) string {
	if !f.Checked {
		return ""
	}
	if !f.Running {
		return "agentbusd is not running.\nfix: " + f.Restart + "\n"
	}
	if f.RunningBuild == "" || f.InstalledBuild == "" || f.RunningBuild == f.InstalledBuild {
		return ""
	}
	return fmt.Sprintf("agentbusd (pid %d) is running build %s, and %s is build %s.\nfix: %s\n",
		f.PID, f.RunningBuild, f.InstalledPath, f.InstalledBuild, f.Restart)
}

// daemonShow reads the supervisor's pid, start time, and environment.
// Tests replace it. A failure leaves those fields empty.
var daemonShow = defaultDaemonShow

func defaultDaemonShow(ctx context.Context) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, 2*time.Second)
	defer cancel()
	switch runtime.GOOS {
	case "linux":
		// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
		out, err := exec.CommandContext(ctx, "systemctl", "--user", "show", "agentbus.service", "-p", "MainPID,ExecMainStartTimestamp,Environment").Output() //nolint:gosec // G204: systemctl shows this user's agentbus unit
		if err != nil && len(out) == 0 {
			return "", fmt.Errorf("systemctl show agentbus: %w", err)
		}
		return string(out), nil
	case "darwin":
		target := "gui/" + strconv.Itoa(os.Getuid()) + "/" + launchdLabel
		// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
		out, err := exec.CommandContext(ctx, "launchctl", "print", target).CombinedOutput() //nolint:gosec // G204: launchctl prints this user's agentbus job
		if err != nil && len(out) == 0 {
			return "", fmt.Errorf("launchctl print agentbus: %w", err)
		}
		return string(out), nil
	default:
		return "", errors.New("no supervisor")
	}
}

func reportInstallHealth(ctx context.Context, env []string, dirs paths.Dirs, stdout io.Writer) {
	cfg, cfgErr := hubSettings(env)
	hub := ""
	if cfgErr == nil {
		hub = cfg.Hub
	}
	if msg := diagnoseHerdr(gatherHerdrFacts(ctx, env, dirs.StateDir, cfgErr == nil, cfg.HerdrPath)); msg != "" {
		fmt.Fprint(stdout, msg)
	}
	if msg := diagnoseSandbox(gatherSandbox(env, dirs.TunnelSocketPath)); msg != "" {
		fmt.Fprint(stdout, msg)
	}
	if hub == "" {
		home, err := homeFrom(env)
		if err != nil {
			home = ""
		}
		if msg := diagnoseDaemon(gatherDaemonFacts(ctx, home)); msg != "" {
			fmt.Fprint(stdout, msg)
		}
	}
}

func gatherHerdrFacts(ctx context.Context, env []string, stateDir string, haveCfg bool, herdrPath string) herdrFacts {
	f := herdrFacts{GOOS: runtime.GOOS, UID: os.Getuid(), Supervisor: supervisorName(runtime.GOOS)}
	if bin, err := lookPathIn(env, "herdr"); err == nil {
		f.CallerHerdr = bin
	}
	if haveCfg {
		f.ConfigHerdrOK = executableFile(herdrPath)
	}
	if home, err := homeFrom(env); err == nil {
		if path, err := supervisorPath(runtime.GOOS, home, env); err == nil {
			if data, err := os.ReadFile(path); err == nil {
				f.DaemonPATH, f.DaemonHerdr = parseSupervisorEnv(runtime.GOOS, string(data))
				f.Checked = true
			}
		}
	}
	if text, err := daemonShow(ctx); err == nil {
		pid, started, path, herdr := parseDaemonShow(runtime.GOOS, text)
		if pid > 0 {
			f.PID = pid
		}
		if started != "" {
			f.Started = started
		}
		if path != "" {
			f.DaemonPATH = path
		}
		if herdr != "" {
			f.DaemonHerdr = herdr
		}
	}
	f.DaemonHerdrOK = executableFile(f.DaemonHerdr)
	if lines, err := registry.TransportProblems(stateDir); err == nil {
		for _, line := range lines {
			if strings.Contains(line, "executable file not found") {
				f.ExecNotFound = true
			}
		}
	}
	return f
}

func supervisorName(goos string) string {
	if goos == "darwin" {
		return "launchd"
	}
	return "systemd"
}

func executableFile(path string) bool {
	if path == "" {
		return false
	}
	info, err := os.Stat(path)
	if err != nil || !info.Mode().IsRegular() {
		return false
	}
	return info.Mode()&0o111 != 0
}

func parseSupervisorEnv(goos, text string) (path, herdr string) {
	if goos == "darwin" {
		return plistString(text, "PATH"), plistString(text, "AGENTBUS_HERDR")
	}
	for _, line := range strings.Split(text, "\n") {
		val, ok := strings.CutPrefix(strings.TrimSpace(line), "Environment=")
		if !ok {
			continue
		}
		path, herdr = envPair(val, path, herdr)
	}
	return path, herdr
}

func parseDaemonShow(goos, text string) (pid int, started, path, herdr string) {
	if goos == "darwin" {
		return parseLaunchdPID(text), "", plistString(text, "PATH"), plistString(text, "AGENTBUS_HERDR")
	}
	for _, line := range strings.Split(text, "\n") {
		key, val, ok := strings.Cut(strings.TrimSpace(line), "=")
		if !ok {
			continue
		}
		switch key {
		case "MainPID":
			n, err := strconv.Atoi(strings.TrimSpace(val))
			if err == nil {
				pid = n
			}
		case "ExecMainStartTimestamp":
			started = strings.TrimSpace(val)
			if started == "n/a" {
				started = ""
			}
		case "Environment":
			path, herdr = envPair(val, path, herdr)
		}
	}
	return pid, started, path, herdr
}

func envPair(val, path, herdr string) (string, string) {
	for _, tok := range strings.Fields(val) {
		k, v, ok := strings.Cut(tok, "=")
		if !ok {
			continue
		}
		switch k {
		case "PATH":
			path = v
		case "AGENTBUS_HERDR":
			herdr = v
		}
	}
	return path, herdr
}

func plistString(text, key string) string {
	needle := "<key>" + key + "</key>"
	i := strings.Index(text, needle)
	if i < 0 {
		return ""
	}
	rest := text[i+len(needle):]
	start := strings.Index(rest, "<string>")
	end := strings.Index(rest, "</string>")
	if start < 0 || end < start {
		return ""
	}
	return rest[start+len("<string>") : end]
}

func parseLaunchdPID(text string) int {
	for _, line := range strings.Split(text, "\n") {
		val, ok := strings.CutPrefix(strings.TrimSpace(line), "pid = ")
		if !ok {
			continue
		}
		n, err := strconv.Atoi(strings.TrimSpace(val))
		if err == nil && n > 0 {
			return n
		}
	}
	return 0
}

func gatherSandbox(env []string, socket string) (string, string, []string, bool) {
	home, err := homeFrom(env)
	if err != nil {
		return "", socket, nil, false
	}
	path := filepath.Join(home, ".claude", "settings.json")
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return path, socket, nil, false
	}
	if err != nil {
		return path, socket, nil, false
	}
	var doc struct {
		Sandbox struct {
			Network struct {
				AllowUnixSockets []string `json:"allowUnixSockets"`
			} `json:"network"`
		} `json:"sandbox"`
	}
	if json.Unmarshal(data, &doc) != nil {
		return path, socket, nil, false
	}
	return path, socket, doc.Sandbox.Network.AllowUnixSockets, true
}

func gatherDaemonFacts(ctx context.Context, home string) daemonFacts {
	f := daemonFacts{Restart: restartCmd(herdrFacts{GOOS: runtime.GOOS, UID: os.Getuid()})}
	procs, err := listDoctorProcs(ctx)
	if err != nil {
		return daemonFacts{}
	}
	uid := os.Getuid()
	var name string
	for _, p := range procs {
		if filepath.Base(p.Name) == "agentbusd" && p.UID == uid {
			f.PID = p.PID
			name = p.Name
			break
		}
	}
	f.Checked = true
	f.Running = f.PID > 0
	if home != "" {
		f.InstalledPath = filepath.Join(home, "go", "bin", "agentbusd")
		f.InstalledBuild = binaryBuild(f.InstalledPath)
	}
	if f.Running {
		f.RunningBuild = binaryBuild(runningExe(ctx, f.PID, name))
	}
	return f
}

// binaryBuild is the VCS revision embedded in a Go binary. A missing
// file or a binary with no revision returns empty, so doctor stays quiet.
func binaryBuild(path string) string {
	if path == "" {
		return ""
	}
	info, err := buildinfo.ReadFile(path)
	if err != nil || info == nil {
		return ""
	}
	var rev, modified string
	for _, s := range info.Settings {
		switch s.Key {
		case "vcs.revision":
			rev = s.Value
		case "vcs.modified":
			modified = s.Value
		}
	}
	if rev == "" {
		return ""
	}
	if modified == "true" {
		return rev + "-modified"
	}
	return rev
}

// runningExe is the executable path of pid. name is used when it is
// already absolute. Otherwise Linux reads /proc, and other systems ask ps.
func runningExe(ctx context.Context, pid int, name string) string {
	if filepath.IsAbs(name) {
		return name
	}
	if pid <= 0 {
		return ""
	}
	if runtime.GOOS == "linux" {
		p, err := os.Readlink("/proc/" + strconv.Itoa(pid) + "/exe")
		if err == nil {
			return p
		}
	}
	out, err := procinfo.Run(ctx, "ps", "-p", strconv.Itoa(pid), "-o", "command=")
	if err != nil {
		return ""
	}
	fields := strings.Fields(out)
	if len(fields) == 0 || !filepath.IsAbs(fields[0]) {
		return ""
	}
	return fields[0]
}
