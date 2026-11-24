package web

import (
	"context"
	"encoding/xml"
	"errors"
	"fmt"
	"io/fs"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

// DoctorEnv holds injectable OS, command, and HTTP dependencies for the
// `decide doctor` web checks. Zero fields default to the real host environment.
type DoctorEnv struct {
	GOOS       string
	User       string
	UID        string
	HomeDir    string
	ConfigPath string
	Executable string
	Getenv     func(string) string
	LookPath   func(file string) (string, error)
	RunCmd     func(ctx context.Context, name string, args ...string) (stdout, stderr string, err error)
	HTTPClient *http.Client
}

func (e *DoctorEnv) withDefaults() DoctorEnv {
	out := *e
	out.fillIdentityDefaults()
	out.fillPathDefaults()
	out.fillRuntimeDefaults()
	return out
}

func (e *DoctorEnv) fillIdentityDefaults() {
	if e.GOOS == "" {
		e.GOOS = runtime.GOOS
	}
	if e.User == "" {
		if u, err := user.Current(); err == nil && u.Username != "" {
			e.User = u.Username
		} else {
			e.User = os.Getenv("USER")
		}
	}
	if e.UID == "" {
		e.UID = strconv.Itoa(os.Getuid())
	}
}

func (e *DoctorEnv) fillPathDefaults() {
	if e.HomeDir == "" {
		if home, err := os.UserHomeDir(); err == nil {
			e.HomeDir = home
		}
	}
	if e.ConfigPath == "" {
		if p, ok := decide.ConfigPath(); ok {
			e.ConfigPath = p
		}
	}
	if e.Executable == "" {
		if exe, err := os.Executable(); err == nil {
			if resolved, rerr := filepath.EvalSymlinks(exe); rerr == nil {
				exe = resolved
			}
			e.Executable = exe
		}
	}
}

func (e *DoctorEnv) fillRuntimeDefaults() {
	if e.LookPath == nil {
		e.LookPath = exec.LookPath
	}
	if e.RunCmd == nil {
		e.RunCmd = defaultRunCmd
	}
	if e.HTTPClient == nil {
		e.HTTPClient = &http.Client{Timeout: 5 * time.Second}
	}
}

func (e DoctorEnv) expandEnv() pathExpandEnv {
	return pathExpandEnv{
		HomeDir: e.HomeDir,
		User:    e.User,
		UID:     e.UID,
		Getenv:  e.Getenv,
	}
}

// IsWebConfigured reports whether `decide web` is configured via config.json
// or has an installed service unit/plist.
func IsWebConfigured(cfg decide.Config, env DoctorEnv) bool {
	if cfg.Web != (decide.WebConfig{}) {
		return true
	}
	if env.HomeDir == "" {
		return false
	}
	switch env.GOOS {
	case "linux":
		_, err := os.Stat(systemdUnitPath(env.HomeDir))
		return err == nil
	case "darwin":
		_, err := os.Stat(launchAgentPath(env.HomeDir))
		return err == nil
	default:
		return false
	}
}

// DoctorWebChecks runs the web section of `decide doctor`. It returns nil when
// web is not configured so the section is skipped quietly.
func DoctorWebChecks(ctx context.Context, env DoctorEnv, offline bool) []toolcli.CheckResult {
	e := env.withDefaults()
	cfg, err := decide.LoadConfigAt(e.ConfigPath)
	if err != nil {
		return nil
	}
	if !IsWebConfigured(cfg, e) {
		return nil
	}

	svcBin, svcArgs, svcInstalled, svcRunning := inspectWebService(ctx, e)
	eff := resolveEffectiveWebSettings(cfg, e, svcArgs)

	var health healthStatus
	var healthErr error
	if !offline && cfg.Web.PublicURL != "" {
		health, healthErr = checkHealth(ctx, e.HTTPClient, cfg.Web.PublicURL)
	}

	authProbe := authProbeUnknown
	if !offline {
		if cfg.Web.PublicURL != "" {
			authProbe = probeServerAuth(ctx, e.HTTPClient, cfg.Web.PublicURL)
		}
		if authProbe == authProbeUnknown && svcRunning && eff.Addr != "" {
			authProbe = probeServerAuth(ctx, e.HTTPClient, "http://"+eff.Addr)
		}
	}

	return []toolcli.CheckResult{
		checkWebService(svcInstalled, svcRunning),
		checkWebAddr(eff.Addr),
		checkWebToken(eff, authProbe),
		checkWebAuth(eff, authProbe),
		checkWebPublicURL(ctx, e, cfg, eff.Addr),
		checkWebHealth(cfg, offline, health.OK, healthErr),
		checkWebForwardedProto(cfg, offline, health, healthErr),
		checkWebPush(cfg),
		checkWebBinary(e, svcInstalled, svcBin),
		checkWebFollowConflict(ctx, e, svcRunning, svcArgs),
	}
}

func inspectWebService(ctx context.Context, e DoctorEnv) (bin string, args []string, installed, running bool) {
	switch e.GOOS {
	case "linux":
		data, err := os.ReadFile(systemdUnitPath(e.HomeDir))
		if err == nil {
			installed = true
		}
		if showOut, _, showErr := e.RunCmd(ctx, "systemctl", "--user", "show", "-p", "ExecStart", systemdWebServiceName); showErr == nil {
			bin, args = parseSystemdShowExecStart(showOut)
			if bin != "" {
				installed = true
			}
		}
		if bin == "" && err == nil {
			bin, args = parseSystemdExecStart(string(data))
		}
		out, _, runErr := e.RunCmd(ctx, "systemctl", "--user", "is-active", systemdWebServiceName)
		running = runErr == nil && strings.TrimSpace(out) == "active"
	case "darwin":
		data, err := os.ReadFile(launchAgentPath(e.HomeDir))
		if err == nil {
			installed = true
			bin, args = parsePlistProgramArguments(string(data))
		}
		_, _, runErr := e.RunCmd(ctx, "launchctl", "print", "gui/"+e.UID+"/"+launchAgentWebLabel)
		running = runErr == nil
	}
	return bin, args, installed, running
}

func isFollowServiceRunning(ctx context.Context, e DoctorEnv) bool {
	switch e.GOOS {
	case "linux":
		out, _, err := e.RunCmd(ctx, "systemctl", "--user", "is-active", systemdFollowServiceName)
		return err == nil && strings.TrimSpace(out) == "active"
	case "darwin":
		_, _, err := e.RunCmd(ctx, "launchctl", "print", "gui/"+e.UID+"/"+launchAgentFollowLabel)
		return err == nil
	default:
		return false
	}
}

func checkWebService(installed, running bool) toolcli.CheckResult {
	switch {
	case installed && running:
		return toolcli.CheckResult{
			Name:   "web-service",
			Status: toolcli.StatusOK,
			Detail: "decide-web service is installed and running",
		}
	case !installed:
		return toolcli.CheckResult{
			Name:   "web-service",
			Status: toolcli.StatusFail,
			Detail: "decide-web service is not installed",
			Hint:   "install and start the service; run `decide web setup`",
		}
	default:
		return toolcli.CheckResult{
			Name:   "web-service",
			Status: toolcli.StatusFail,
			Detail: "decide-web service is installed but not running",
			Hint:   "start the decide-web service; run `decide web setup`",
		}
	}
}

func checkWebAddr(addr string) toolcli.CheckResult {
	if _, _, err := net.SplitHostPort(addr); err != nil {
		return toolcli.CheckResult{
			Name:   "web-addr",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("invalid listen address %q: %v", addr, err),
			Hint:   "set a valid host:port in web.addr; run `decide web setup`",
		}
	}
	return toolcli.CheckResult{
		Name:   "web-addr",
		Status: toolcli.StatusOK,
		Detail: addr,
	}
}

func checkWebToken(eff effectiveWebSettings, probe authProbeState) toolcli.CheckResult {
	mode := eff.AuthMode
	if mode == "" {
		mode = decide.WebAuthNone
	}
	path := eff.TokenFile
	if mode == decide.WebAuthNone && path == "" {
		if probe == authProbeEnforced {
			return toolcli.CheckResult{
				Name:   "web-token",
				Status: toolcli.StatusOK,
				Detail: "token auth enforced by running server (no token file path configured)",
			}
		}
		return toolcli.CheckResult{
			Name:   "web-token",
			Status: toolcli.StatusWarn,
			Detail: "no token file configured (auth mode is none)",
			Hint:   "configure token auth with a 0600 token file; run `decide web setup`",
		}
	}
	if path == "" {
		return toolcli.CheckResult{
			Name:   "web-token",
			Status: toolcli.StatusFail,
			Detail: "web.auth.tokenFile is empty",
			Hint:   "create a 0600 token file and set web.auth.tokenFile; run `decide web setup`",
		}
	}
	info, err := os.Stat(path)
	if errors.Is(err, fs.ErrNotExist) {
		return toolcli.CheckResult{
			Name:   "web-token",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("token file not found: %s", path),
			Hint:   "create the token file with mode 0600; run `decide web setup`",
		}
	}
	if err != nil {
		return toolcli.CheckResult{
			Name:   "web-token",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("cannot stat token file %s: %v", path, err),
			Hint:   "fix token file permissions; run `decide web setup`",
		}
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		return toolcli.CheckResult{
			Name:   "web-token",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("token file %s has mode %04o (want 0600)", path, perm),
			Hint:   fmt.Sprintf("run `chmod 0600 %s` or run `decide web setup`", path),
		}
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "web-token",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("cannot read token file %s: %v", path, err),
			Hint:   "check token file readability; run `decide web setup`",
		}
	}
	if len(strings.TrimSpace(string(data))) < minTokenLen {
		return toolcli.CheckResult{
			Name:   "web-token",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("token in %s has fewer than %d characters", path, minTokenLen),
			Hint:   "generate a token with at least 16 characters; run `decide web setup`",
		}
	}
	detail := fmt.Sprintf("%s (mode 0600, valid length)", path)
	if eff.TokenSource != "" {
		detail = fmt.Sprintf("%s (mode 0600, valid length; token file from %s)", path, eff.TokenSource)
	}
	return toolcli.CheckResult{
		Name:   "web-token",
		Status: toolcli.StatusOK,
		Detail: detail,
	}
}

func checkWebAuth(eff effectiveWebSettings, probe authProbeState) toolcli.CheckResult {
	mode := eff.AuthMode
	if mode == "" {
		mode = decide.WebAuthNone
	}
	if mode == decide.WebAuthNone {
		if probe == authProbeEnforced {
			return toolcli.CheckResult{
				Name:   "web-auth",
				Status: toolcli.StatusOK,
				Detail: "token auth enforced by running server (GET /api/v1/items returned 401)",
			}
		}
		host, _, err := net.SplitHostPort(eff.Addr)
		if err == nil && !isLoopbackHost(host) {
			return toolcli.CheckResult{
				Name:   "web-auth",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("auth mode none on non-loopback address %s", eff.Addr),
				Hint:   "switch to web.auth.mode \"token\" or bind to 127.0.0.1; run `decide web setup`",
			}
		}
		return toolcli.CheckResult{
			Name:   "web-auth",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("auth mode none on loopback %s", eff.Addr),
			Hint:   "prefer token auth even behind an ingress; run `decide web setup`",
		}
	}
	detail := fmt.Sprintf("auth mode %s", mode)
	if eff.ModeSource != "" {
		detail += fmt.Sprintf(" (from %s)", eff.ModeSource)
	}
	if probe == authProbeOpen {
		return toolcli.CheckResult{
			Name:   "web-auth",
			Status: toolcli.StatusWarn,
			Detail: detail + ", but running server did not require auth on /api/v1/items",
			Hint:   "restart the decide-web service so the configured auth settings take effect",
		}
	}
	if probe == authProbeEnforced {
		detail += "; token auth enforced by running server"
	}
	return toolcli.CheckResult{
		Name:   "web-auth",
		Status: toolcli.StatusOK,
		Detail: detail,
	}
}

func checkWebPublicURL(ctx context.Context, e DoctorEnv, cfg decide.Config, addr string) toolcli.CheckResult {
	raw := strings.TrimRight(strings.TrimSpace(cfg.Web.PublicURL), "/")
	if raw == "" {
		return toolcli.CheckResult{
			Name:   "web-public-url",
			Status: toolcli.StatusFail,
			Detail: "web.publicUrl is not set",
			Hint:   "set web.publicUrl to your HTTPS ingress URL; run `decide web setup`",
		}
	}
	u, err := url.Parse(raw)
	if err != nil || !strings.EqualFold(u.Scheme, "https") || u.Host == "" {
		return toolcli.CheckResult{
			Name:   "web-public-url",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("web.publicUrl %q is not a valid https:// URL", raw),
			Hint:   "set web.publicUrl to your https:// ingress URL; run `decide web setup`",
		}
	}
	if _, err := e.LookPath("tailscale"); err == nil {
		if tsURL, _, err := tailscaleServeStatus(ctx, e.RunCmd, addr); err == nil && tsURL != "" {
			if !strings.EqualFold(raw, strings.TrimRight(tsURL, "/")) {
				return toolcli.CheckResult{
					Name:   "web-public-url",
					Status: toolcli.StatusFail,
					Detail: fmt.Sprintf("web.publicUrl %s does not match tailscale serve %s", raw, tsURL),
					Hint:   fmt.Sprintf("update web.publicUrl to %s; run `decide web setup`", tsURL),
				}
			}
			return toolcli.CheckResult{
				Name:   "web-public-url",
				Status: toolcli.StatusOK,
				Detail: fmt.Sprintf("%s (matches tailscale serve)", raw),
			}
		}
	}
	return toolcli.CheckResult{
		Name:   "web-public-url",
		Status: toolcli.StatusOK,
		Detail: raw,
	}
}

func checkWebHealth(cfg decide.Config, offline, ok bool, err error) toolcli.CheckResult {
	if offline {
		return toolcli.CheckResult{
			Name:   "web-health",
			Status: toolcli.StatusWarn,
			Detail: "skipped (--offline)",
			Hint:   "run without --offline to check public /api/v1/health; run `decide web setup`",
		}
	}
	if strings.TrimSpace(cfg.Web.PublicURL) == "" {
		return toolcli.CheckResult{
			Name:   "web-health",
			Status: toolcli.StatusFail,
			Detail: "web.publicUrl is not set",
			Hint:   "set web.publicUrl and start the ingress; run `decide web setup`",
		}
	}
	if err != nil || !ok {
		detail := fmt.Sprintf("%s/api/v1/health unreachable", strings.TrimRight(cfg.Web.PublicURL, "/"))
		if err != nil {
			detail = fmt.Sprintf("%s: %v", detail, err)
		}
		return toolcli.CheckResult{
			Name:   "web-health",
			Status: toolcli.StatusFail,
			Detail: detail,
			Hint:   "verify decide-web and the HTTPS ingress are running; run `decide web setup`",
		}
	}
	return toolcli.CheckResult{
		Name:   "web-health",
		Status: toolcli.StatusOK,
		Detail: strings.TrimRight(cfg.Web.PublicURL, "/") + "/api/v1/health reachable",
	}
}

func checkWebForwardedProto(cfg decide.Config, offline bool, health healthStatus, healthErr error) toolcli.CheckResult {
	if offline {
		return toolcli.CheckResult{
			Name:   "web-forwarded-proto",
			Status: toolcli.StatusWarn,
			Detail: "skipped (--offline)",
			Hint:   "run without --offline to verify X-Forwarded-Proto: https; run `decide web setup`",
		}
	}
	if strings.TrimSpace(cfg.Web.PublicURL) == "" {
		return toolcli.CheckResult{
			Name:   "web-forwarded-proto",
			Status: toolcli.StatusFail,
			Detail: "web.publicUrl is not set",
			Hint:   "set web.publicUrl and forward X-Forwarded-Proto: https; run `decide web setup`",
		}
	}
	if healthErr != nil || !health.OK {
		return toolcli.CheckResult{
			Name:   "web-forwarded-proto",
			Status: toolcli.StatusFail,
			Detail: "could not verify X-Forwarded-Proto: https (public health unreachable)",
			Hint:   "ensure ingress forwards X-Forwarded-Proto: https; run `decide web setup`",
		}
	}
	if !health.HasSecure {
		return toolcli.CheckResult{
			Name:   "web-forwarded-proto",
			Status: toolcli.StatusWarn,
			Detail: "server predates this check; restart the web service",
			Hint:   "restart the decide-web service so /api/v1/health reports forwarded proto status",
		}
	}
	if !health.Secure {
		return toolcli.CheckResult{
			Name:   "web-forwarded-proto",
			Status: toolcli.StatusFail,
			Detail: "X-Forwarded-Proto: https not seen through public URL (session cookie will not be Secure)",
			Hint:   "configure your ingress proxy to send X-Forwarded-Proto: https; run `decide web setup`",
		}
	}
	return toolcli.CheckResult{
		Name:   "web-forwarded-proto",
		Status: toolcli.StatusOK,
		Detail: "X-Forwarded-Proto: https seen (session cookie is Secure)",
	}
}

func checkWebPush(cfg decide.Config) toolcli.CheckResult {
	if !cfg.Web.Push.Enabled {
		return toolcli.CheckResult{
			Name:   "web-push",
			Status: toolcli.StatusOK,
			Detail: "web push disabled",
		}
	}
	if !isValidPushSubject(cfg.Web.Push.Subject) {
		return toolcli.CheckResult{
			Name:   "web-push",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("web.push.subject %q is invalid or default", cfg.Web.Push.Subject),
			Hint:   "set web.push.subject to a real mailto: or https: contact; run `decide web setup`",
		}
	}
	return toolcli.CheckResult{
		Name:   "web-push",
		Status: toolcli.StatusOK,
		Detail: fmt.Sprintf("push enabled (subject: %s)", cfg.Web.Push.Subject),
	}
}

func checkWebBinary(e DoctorEnv, installed bool, svcBin string) toolcli.CheckResult {
	if !installed || svcBin == "" {
		return toolcli.CheckResult{
			Name:   "web-binary",
			Status: toolcli.StatusFail,
			Detail: "decide-web service binary path not found",
			Hint:   "install the service using the current decide binary; run `decide web setup`",
		}
	}
	installedBin := resolveBinaryPath(e)
	expandedSvc := expandServicePath(svcBin, e.expandEnv())
	resolvedSvc := evalSymlinksOrClean(expandedSvc)
	if installedBin == "" {
		return toolcli.CheckResult{
			Name:   "web-binary",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("service uses %s (could not resolve installed decide path)", resolvedSvc),
			Hint:   "ensure decide is on PATH; run `decide web setup`",
		}
	}
	if resolvedSvc != installedBin {
		return toolcli.CheckResult{
			Name:   "web-binary",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("service runs %s, installed decide is %s", resolvedSvc, installedBin),
			Hint:   fmt.Sprintf("update the service to %s; run `decide web setup`", installedBin),
		}
	}
	return toolcli.CheckResult{
		Name:   "web-binary",
		Status: toolcli.StatusOK,
		Detail: resolvedSvc,
	}
}

func resolveBinaryPath(e DoctorEnv) string {
	cand := e.Executable
	if p, err := e.LookPath("decide"); err == nil && p != "" {
		cand = p
	}
	if cand == "" {
		return ""
	}
	if resolved, err := filepath.EvalSymlinks(cand); err == nil {
		return resolved
	}
	return cand
}

func checkWebFollowConflict(ctx context.Context, e DoctorEnv, webRunning bool, svcArgs []string) toolcli.CheckResult {
	if !webRunning || parseWebServiceFlags(svcArgs).NoFollow {
		return toolcli.CheckResult{
			Name:   "web-follow-conflict",
			Status: toolcli.StatusOK,
			Detail: "no conflicting decide follow loop running",
		}
	}
	if isFollowServiceRunning(ctx, e) {
		return toolcli.CheckResult{
			Name:   "web-follow-conflict",
			Status: toolcli.StatusFail,
			Detail: "both decide-web (with follow loop) and decide-follow are running",
			Hint:   "stop/disable decide-follow or add --no-follow to decide web; run `decide web setup`",
		}
	}
	return toolcli.CheckResult{
		Name:   "web-follow-conflict",
		Status: toolcli.StatusOK,
		Detail: "no conflicting decide follow loop running",
	}
}

type pathExpandEnv struct {
	HomeDir string
	User    string
	UID     string
	Getenv  func(string) string
}

// expandServicePath expands systemd specifiers (%h, %u, %U, %%), environment
// references ($VAR, ${VAR}), and a leading ~ in a path from a service unit or
// config.
func expandServicePath(raw string, env pathExpandEnv) string {
	s := strings.TrimSpace(raw)
	if s == "" {
		return ""
	}
	s = expandSystemdSpecifiers(s, env)
	s = expandEnvVars(s, env)
	s = expandTilde(s, env)
	return filepath.Clean(s)
}

func expandSystemdSpecifiers(s string, env pathExpandEnv) string {
	var b strings.Builder
	b.Grow(len(s))
	for i := 0; i < len(s); i++ {
		if s[i] == '%' && i+1 < len(s) {
			if rep, ok := systemdSpecifierValue(s[i+1], env); ok {
				b.WriteString(rep)
				i++
				continue
			}
		}
		b.WriteByte(s[i])
	}
	return b.String()
}

func systemdSpecifierValue(spec byte, env pathExpandEnv) (string, bool) {
	switch spec {
	case '%':
		return "%", true
	case 'h':
		return env.HomeDir, env.HomeDir != ""
	case 'u':
		return env.User, env.User != ""
	case 'U':
		return env.UID, env.UID != ""
	default:
		return "", false
	}
}

func expandEnvVars(s string, env pathExpandEnv) string {
	getenv := env.Getenv
	if getenv == nil {
		getenv = func(k string) string {
			if k == "HOME" && env.HomeDir != "" {
				return env.HomeDir
			}
			if k == "USER" && env.User != "" {
				return env.User
			}
			return os.Getenv(k)
		}
	}
	return os.Expand(s, getenv)
}

func expandTilde(s string, env pathExpandEnv) string {
	if env.HomeDir == "" {
		return s
	}
	switch {
	case s == "~":
		return env.HomeDir
	case strings.HasPrefix(s, "~/"):
		return filepath.Join(env.HomeDir, s[2:])
	case env.User != "" && s == "~"+env.User:
		return env.HomeDir
	case env.User != "" && strings.HasPrefix(s, "~"+env.User+"/"):
		return filepath.Join(env.HomeDir, s[len(env.User)+2:])
	default:
		return s
	}
}

func evalSymlinksOrClean(p string) string {
	if p == "" {
		return ""
	}
	if resolved, err := filepath.EvalSymlinks(p); err == nil {
		return resolved
	}
	return filepath.Clean(p)
}

type webServiceFlags struct {
	Addr                string
	TokenFile           string
	AuthMode            string
	AllowInsecureUnauth bool
	NoFollow            bool
}

func parseWebServiceFlags(args []string) webServiceFlags {
	var flags webServiceFlags
	i := 0
	if len(args) > 0 && args[0] == "web" {
		i = 1
	}
	for ; i < len(args); i++ {
		arg := args[i]
		if arg == "--" {
			break
		}
		if !strings.HasPrefix(arg, "-") || arg == "-" {
			continue
		}
		name, val, hasEq := strings.Cut(strings.TrimLeft(arg, "-"), "=")
		nextVal := func() string {
			if hasEq {
				return val
			}
			if i+1 < len(args) {
				i++
				return args[i]
			}
			return ""
		}
		applyWebServiceFlag(&flags, name, hasEq, val, nextVal)
	}
	return flags
}

func applyWebServiceFlag(flags *webServiceFlags, name string, hasEq bool, val string, nextVal func() string) {
	switch name {
	case "token-file":
		flags.TokenFile = nextVal()
	case "addr":
		flags.Addr = nextVal()
	case "auth-mode", "auth":
		flags.AuthMode = nextVal()
	case "no-follow":
		flags.NoFollow = !hasEq || val == "true" || val == "1"
	case "allow-insecure-unauth":
		flags.AllowInsecureUnauth = !hasEq || val == "true" || val == "1"
	}
}

type effectiveWebSettings struct {
	Addr        string
	AuthMode    string
	TokenFile   string
	ModeSource  string
	TokenSource string
}

func resolveEffectiveWebSettings(cfg decide.Config, e DoctorEnv, svcArgs []string) effectiveWebSettings {
	expandEnv := e.expandEnv()
	eff := effectiveWebSettings{
		Addr:      cfg.WebAddr(),
		AuthMode:  cfg.Web.Auth.Mode,
		TokenFile: expandServicePath(cfg.Web.Auth.TokenFile, expandEnv),
	}
	if eff.AuthMode != "" {
		eff.ModeSource = "config"
	}
	if eff.TokenFile != "" {
		eff.TokenSource = "config"
	}

	svcSource := serviceFlagSource(e.GOOS)
	flags := parseWebServiceFlags(svcArgs)
	if flags.Addr != "" {
		eff.Addr = flags.Addr
	}
	if flags.AuthMode != "" {
		eff.AuthMode = flags.AuthMode
		eff.ModeSource = svcSource
	}
	if flags.TokenFile != "" {
		eff.AuthMode = decide.WebAuthToken
		eff.TokenFile = expandServicePath(flags.TokenFile, expandEnv)
		eff.ModeSource = svcSource
		eff.TokenSource = svcSource
	}
	if eff.AuthMode == "" {
		eff.AuthMode = decide.WebAuthNone
	}
	return eff
}

func serviceFlagSource(goos string) string {
	switch goos {
	case "linux":
		return "service ExecStart"
	case "darwin":
		return "service ProgramArguments"
	default:
		return "service"
	}
}

type authProbeState int

const (
	authProbeUnknown authProbeState = iota
	authProbeEnforced
	authProbeOpen
)

func probeServerAuth(ctx context.Context, client *http.Client, baseURL string) authProbeState {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, strings.TrimRight(baseURL, "/")+"/api/v1/items", nil)
	if err != nil {
		return authProbeUnknown
	}
	resp, err := client.Do(req)
	if err != nil {
		return authProbeUnknown
	}
	defer resp.Body.Close()
	switch resp.StatusCode {
	case http.StatusUnauthorized:
		return authProbeEnforced
	case http.StatusOK:
		return authProbeOpen
	default:
		return authProbeUnknown
	}
}

func parseSystemdExecStart(unit string) (string, []string) {
	var bin string
	var args []string
	for _, line := range strings.Split(unit, "\n") {
		line = strings.TrimSpace(line)
		val, ok := strings.CutPrefix(line, "ExecStart=")
		if !ok {
			continue
		}
		val = strings.TrimSpace(val)
		if val == "" {
			bin, args = "", nil
			continue
		}
		fields := splitCommandArgs(strings.TrimLeft(val, "-@:+!"))
		if len(fields) > 0 {
			bin, args = fields[0], fields[1:]
		}
	}
	return bin, args
}

func parseSystemdShowExecStart(out string) (string, []string) {
	var bin string
	var args []string
	for _, line := range strings.Split(out, "\n") {
		line = strings.TrimSpace(line)
		val, ok := strings.CutPrefix(line, "ExecStart=")
		if !ok {
			continue
		}
		val = strings.TrimSpace(val)
		if val == "" {
			bin, args = "", nil
			continue
		}
		if strings.HasPrefix(val, "{") && strings.HasSuffix(val, "}") {
			inner := strings.TrimSpace(val[1 : len(val)-1])
			var pathVal, argvVal string
			for _, part := range strings.Split(inner, " ; ") {
				part = strings.TrimSpace(part)
				if p, ok := strings.CutPrefix(part, "path="); ok {
					pathVal = strings.TrimSpace(p)
				} else if a, ok := strings.CutPrefix(part, "argv[]="); ok {
					argvVal = strings.TrimSpace(a)
				}
			}
			fields := splitCommandArgs(argvVal)
			switch {
			case len(fields) > 0:
				bin = fields[0]
				if pathVal != "" {
					bin = pathVal
				}
				args = fields[1:]
			case pathVal != "":
				bin, args = pathVal, nil
			}
			continue
		}
		fields := splitCommandArgs(strings.TrimLeft(val, "-@:+!"))
		if len(fields) > 0 {
			bin, args = fields[0], fields[1:]
		}
	}
	return bin, args
}

type argSplitter struct {
	args     []string
	cur      strings.Builder
	quote    byte // 0, '\'', or '"'
	escaped  bool
	hasToken bool
}

func (a *argSplitter) feed(c byte) {
	if a.escaped {
		a.cur.WriteByte(c)
		a.escaped = false
		a.hasToken = true
		return
	}
	if a.quote != 0 {
		switch {
		case a.quote == '"' && c == '\\':
			a.escaped = true
		case c == a.quote:
			a.quote = 0
		default:
			a.cur.WriteByte(c)
		}
		return
	}
	switch c {
	case '\\':
		a.escaped = true
		a.hasToken = true
	case '\'', '"':
		a.quote = c
		a.hasToken = true
	case ' ', '\t', '\n', '\r':
		a.flush()
	default:
		a.cur.WriteByte(c)
		a.hasToken = true
	}
}

func (a *argSplitter) flush() {
	if a.hasToken {
		a.args = append(a.args, a.cur.String())
		a.cur.Reset()
		a.hasToken = false
	}
}

func splitCommandArgs(s string) []string {
	var sp argSplitter
	for i := 0; i < len(s); i++ {
		sp.feed(s[i])
	}
	sp.flush()
	return sp.args
}

func parsePlistProgramArguments(plist string) (string, []string) {
	dec := xml.NewDecoder(strings.NewReader(plist))
	for {
		tok, err := dec.Token()
		if err != nil {
			return "", nil
		}
		se, ok := tok.(xml.StartElement)
		if !ok || se.Name.Local != "key" {
			continue
		}
		var key string
		if err := dec.DecodeElement(&key, &se); err != nil || key != "ProgramArguments" {
			continue
		}
		args := readNextPlistArray(dec)
		if len(args) == 0 {
			return "", nil
		}
		return args[0], args[1:]
	}
}

func readNextPlistArray(dec *xml.Decoder) []string {
	for {
		tok, err := dec.Token()
		if err != nil {
			return nil
		}
		se, ok := tok.(xml.StartElement)
		if !ok {
			continue
		}
		if se.Name.Local != "array" {
			return nil
		}
		var arr struct {
			Strings []string `xml:"string"`
		}
		if err := dec.DecodeElement(&arr, &se); err != nil {
			return nil
		}
		return arr.Strings
	}
}
