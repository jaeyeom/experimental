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
	"path/filepath"
	"runtime"
	"slices"
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
	UID        string
	HomeDir    string
	ConfigPath string
	Executable string
	LookPath   func(file string) (string, error)
	RunCmd     func(ctx context.Context, name string, args ...string) (stdout, stderr string, err error)
	HTTPClient *http.Client
}

func (e *DoctorEnv) withDefaults() DoctorEnv {
	out := *e
	if out.GOOS == "" {
		out.GOOS = runtime.GOOS
	}
	if out.UID == "" {
		out.UID = strconv.Itoa(os.Getuid())
	}
	if out.HomeDir == "" {
		if home, err := os.UserHomeDir(); err == nil {
			out.HomeDir = home
		}
	}
	if out.ConfigPath == "" {
		if p, ok := decide.ConfigPath(); ok {
			out.ConfigPath = p
		}
	}
	if out.Executable == "" {
		if exe, err := os.Executable(); err == nil {
			if resolved, rerr := filepath.EvalSymlinks(exe); rerr == nil {
				exe = resolved
			}
			out.Executable = exe
		}
	}
	if out.LookPath == nil {
		out.LookPath = exec.LookPath
	}
	if out.RunCmd == nil {
		out.RunCmd = defaultRunCmd
	}
	if out.HTTPClient == nil {
		out.HTTPClient = &http.Client{Timeout: 5 * time.Second}
	}
	return out
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

	var healthOK, healthSecure bool
	var healthErr error
	if !offline && cfg.Web.PublicURL != "" {
		healthOK, healthSecure, healthErr = checkHealth(ctx, e.HTTPClient, cfg.Web.PublicURL)
	}

	return []toolcli.CheckResult{
		checkWebService(svcInstalled, svcRunning),
		checkWebAddr(cfg),
		checkWebToken(cfg),
		checkWebAuth(cfg),
		checkWebPublicURL(ctx, e, cfg),
		checkWebHealth(cfg, offline, healthOK, healthErr),
		checkWebForwardedProto(cfg, offline, healthOK, healthSecure, healthErr),
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

func checkWebAddr(cfg decide.Config) toolcli.CheckResult {
	addr := cfg.WebAddr()
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

func checkWebToken(cfg decide.Config) toolcli.CheckResult {
	mode := cfg.Web.Auth.Mode
	if mode == "" {
		mode = decide.WebAuthNone
	}
	path := cfg.Web.Auth.TokenFile
	if mode == decide.WebAuthNone && path == "" {
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
	return toolcli.CheckResult{
		Name:   "web-token",
		Status: toolcli.StatusOK,
		Detail: fmt.Sprintf("%s (mode 0600, valid length)", path),
	}
}

func checkWebAuth(cfg decide.Config) toolcli.CheckResult {
	mode := cfg.Web.Auth.Mode
	if mode == "" {
		mode = decide.WebAuthNone
	}
	addr := cfg.WebAddr()
	if mode == decide.WebAuthNone {
		host, _, err := net.SplitHostPort(addr)
		if err == nil && !isLoopbackHost(host) {
			return toolcli.CheckResult{
				Name:   "web-auth",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("auth mode none on non-loopback address %s", addr),
				Hint:   "switch to web.auth.mode \"token\" or bind to 127.0.0.1; run `decide web setup`",
			}
		}
		return toolcli.CheckResult{
			Name:   "web-auth",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("auth mode none on loopback %s", addr),
			Hint:   "prefer token auth even behind an ingress; run `decide web setup`",
		}
	}
	return toolcli.CheckResult{
		Name:   "web-auth",
		Status: toolcli.StatusOK,
		Detail: fmt.Sprintf("auth mode %s", mode),
	}
}

func checkWebPublicURL(ctx context.Context, e DoctorEnv, cfg decide.Config) toolcli.CheckResult {
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
		if tsURL, _, err := tailscaleServeStatus(ctx, e.RunCmd, cfg.WebAddr()); err == nil && tsURL != "" {
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

func checkWebForwardedProto(cfg decide.Config, offline, healthOK, secure bool, healthErr error) toolcli.CheckResult {
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
	if healthErr != nil || !healthOK {
		return toolcli.CheckResult{
			Name:   "web-forwarded-proto",
			Status: toolcli.StatusFail,
			Detail: "could not verify X-Forwarded-Proto: https (public health unreachable)",
			Hint:   "ensure ingress forwards X-Forwarded-Proto: https; run `decide web setup`",
		}
	}
	if !secure {
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
	resolvedSvc := svcBin
	if r, err := filepath.EvalSymlinks(svcBin); err == nil {
		resolvedSvc = r
	}
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
	if !webRunning || slices.Contains(svcArgs, "--no-follow") {
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

func parseSystemdExecStart(unit string) (string, []string) {
	for _, line := range strings.Split(unit, "\n") {
		line = strings.TrimSpace(line)
		val, ok := strings.CutPrefix(line, "ExecStart=")
		if !ok {
			continue
		}
		fields := strings.Fields(strings.TrimLeft(val, "-@:+!"))
		if len(fields) == 0 {
			return "", nil
		}
		return fields[0], fields[1:]
	}
	return "", nil
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
