package web

import (
	"bufio"
	"bytes"
	"context"
	"crypto/rand"
	"encoding/json"
	"encoding/xml"
	"errors"
	"fmt"
	"io"
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
)

const (
	systemdWebServiceName    = "decide-web.service"
	systemdFollowServiceName = "decide-follow.service"
	launchAgentWebLabel      = "com.github.jaeyeom.decide-web"
	launchAgentFollowLabel   = "com.github.jaeyeom.decide-follow"
)

// SetupOptions controls `decide web setup`.
type SetupOptions struct {
	Yes    bool
	DryRun bool
}

// SetupEnv holds injectable OS, command, HTTP, and I/O dependencies for
// `decide web setup`. Zero fields default to the real host environment.
type SetupEnv struct {
	GOOS       string
	User       string
	UID        string
	HomeDir    string
	ConfigPath string
	Executable string
	LookPath   func(file string) (string, error)
	RunCmd     func(ctx context.Context, name string, args ...string) (stdout, stderr string, err error)
	RunSudoCmd func(ctx context.Context, name string, args ...string) error
	HTTPClient *http.Client
	Rand       io.Reader
	Stdin      io.Reader
	Stdout     io.Writer
	Stderr     io.Writer
}

func (e *SetupEnv) withDefaults() (SetupEnv, error) {
	out := *e
	out.fillIdentityDefaults()
	if err := out.fillPathDefaults(); err != nil {
		return out, err
	}
	out.fillRuntimeDefaults()
	return out, nil
}

func (e *SetupEnv) fillIdentityDefaults() {
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

func (e *SetupEnv) fillPathDefaults() error {
	if e.HomeDir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return fmt.Errorf("resolve home directory: %w", err)
		}
		e.HomeDir = home
	}
	if e.ConfigPath == "" {
		p, ok := decide.ConfigPath()
		if !ok {
			return fmt.Errorf("resolve decide config path")
		}
		e.ConfigPath = p
	}
	if e.Executable == "" {
		exe, err := os.Executable()
		if err != nil {
			return fmt.Errorf("resolve executable path: %w", err)
		}
		if resolved, err := filepath.EvalSymlinks(exe); err == nil {
			exe = resolved
		}
		e.Executable = exe
	}
	return nil
}

func (e *SetupEnv) fillRuntimeDefaults() {
	if e.Stdin == nil {
		e.Stdin = os.Stdin
	}
	if e.Stdout == nil {
		e.Stdout = os.Stdout
	}
	if e.Stderr == nil {
		e.Stderr = os.Stderr
	}
	if e.LookPath == nil {
		e.LookPath = exec.LookPath
	}
	if e.RunCmd == nil {
		e.RunCmd = defaultRunCmd
	}
	if e.RunSudoCmd == nil {
		e.RunSudoCmd = defaultRunSudoCmd(e.Stdin, e.Stdout, e.Stderr)
	}
	if e.HTTPClient == nil {
		e.HTTPClient = &http.Client{Timeout: 5 * time.Second}
	}
	if e.Rand == nil {
		e.Rand = rand.Reader
	}
}

func defaultRunCmd(ctx context.Context, name string, args ...string) (string, string, error) {
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, name, args...) //nolint:gosec // G204: fixed system commands invoked by setup/doctor
	var outBuf, errBuf bytes.Buffer
	cmd.Stdout = &outBuf
	cmd.Stderr = &errBuf
	err := cmd.Run()
	return outBuf.String(), errBuf.String(), err
}

func defaultRunSudoCmd(stdin io.Reader, stdout, stderr io.Writer) func(context.Context, string, ...string) error {
	return func(ctx context.Context, name string, args ...string) error {
		// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
		cmd := exec.CommandContext(ctx, name, args...) //nolint:gosec // G204: explicit user-confirmed sudo command
		cmd.Stdin = stdin
		cmd.Stdout = stdout
		cmd.Stderr = stderr
		return cmd.Run()
	}
}

type wizard struct {
	env  SetupEnv
	opts SetupOptions
	in   *bufio.Reader
}

func (w *wizard) printf(format string, args ...any) {
	_, _ = fmt.Fprintf(w.env.Stdout, format, args...)
}

func (w *wizard) confirm(prompt string, isSudo bool) (bool, error) {
	if w.opts.DryRun {
		w.printf("[dry-run] Would ask: %s [y/N]\n", prompt)
		return false, nil
	}
	if !isSudo && w.opts.Yes {
		w.printf("%s [y/N]: y (--yes)\n", prompt)
		return true, nil
	}
	w.printf("%s [y/N]: ", prompt)
	line, err := w.in.ReadString('\n')
	if err != nil && !errors.Is(err, io.EOF) {
		return false, fmt.Errorf("read confirmation: %w", err)
	}
	ans := strings.ToLower(strings.TrimSpace(line))
	return ans == "y" || ans == "yes", nil
}

func (w *wizard) readLine(prompt string) (string, error) {
	w.printf("%s", prompt)
	line, err := w.in.ReadString('\n')
	if err != nil && !errors.Is(err, io.EOF) {
		return "", fmt.Errorf("read input: %w", err)
	}
	return strings.TrimSpace(line), nil
}

// RunSetup runs the interactive `decide web setup` wizard.
func RunSetup(ctx context.Context, env SetupEnv, opts SetupOptions) error {
	resolved, err := env.withDefaults()
	if err != nil {
		return err
	}
	w := &wizard{
		env:  resolved,
		opts: opts,
		in:   bufio.NewReader(resolved.Stdin),
	}

	cfg, err := decide.LoadConfigAt(w.env.ConfigPath)
	if err != nil {
		return fmt.Errorf("load decide config: %w", err)
	}
	desired := cfg.Web
	addr := cfg.WebAddr()

	// Step a: Token file.
	tokenPath, err := w.stepTokenFile(&desired)
	if err != nil {
		return err
	}

	// Step b: Service (systemd user unit on Linux, LaunchAgent on macOS).
	if err := w.stepService(ctx, tokenPath); err != nil {
		return err
	}

	// Step c: Ingress (Tailscale Serve or Caddy/Cloudflare instructions).
	if err := w.stepIngress(ctx, addr, &desired); err != nil {
		return err
	}

	// Step d: Write web config (including web.publicUrl) to config.json after backup.
	if err := w.stepConfig(&cfg, desired); err != nil {
		return err
	}

	// Step e: Web Push (optional).
	if err := w.stepPush(&cfg, &desired); err != nil {
		return err
	}

	// Step f: Final health check and phone install steps.
	w.stepFinalCheck(ctx, addr, desired.PublicURL)
	return nil
}

func (w *wizard) stepTokenFile(desired *decide.WebConfig) (string, error) {
	w.printf("\n==> Step 1: Token file\n")
	tokenPath := desired.Auth.TokenFile
	if tokenPath == "" {
		tokenPath = filepath.Join(filepath.Dir(w.env.ConfigPath), "web-token")
	}

	info, err := os.Stat(tokenPath)
	switch {
	case err == nil:
		done, cerr := w.checkExistingTokenFile(tokenPath, info.Mode().Perm(), desired)
		if cerr != nil || done {
			return tokenPath, cerr
		}
	case errors.Is(err, fs.ErrNotExist):
		w.printf("Plan: create token file at %s (mode 0600, 64-character random token)\n", tokenPath)
	default:
		return "", fmt.Errorf("stat token file %s: %w", tokenPath, err)
	}

	ok, err := w.confirm(fmt.Sprintf("Create token file at %s?", tokenPath), false)
	if err != nil || !ok {
		return tokenPath, err
	}
	if err := os.MkdirAll(filepath.Dir(tokenPath), 0o700); err != nil {
		return "", fmt.Errorf("create token directory: %w", err)
	}
	if _, err := generateToken(tokenPath, w.env.Rand); err != nil {
		return "", err
	}
	w.printf("Created token file at %s (mode 0600).\n", tokenPath)
	desired.Auth.Mode = decide.WebAuthToken
	desired.Auth.TokenFile = tokenPath
	return tokenPath, nil
}

func (w *wizard) checkExistingTokenFile(tokenPath string, perm fs.FileMode, desired *decide.WebConfig) (bool, error) {
	data, err := os.ReadFile(tokenPath)
	if err != nil {
		return false, fmt.Errorf("read token file %s: %w", tokenPath, err)
	}
	if len(strings.TrimSpace(string(data))) < minTokenLen {
		w.printf("Plan: regenerate token file at %s (mode 0600, current token is shorter than %d characters)\n", tokenPath, minTokenLen)
		return false, nil
	}
	if perm == 0o600 {
		w.printf("Already done: token file exists at %s (mode 0600).\n", tokenPath)
	} else {
		w.printf("Plan: chmod 0600 %s (currently %04o)\n", tokenPath, perm)
		ok, cerr := w.confirm("Fix token file permissions to 0600?", false)
		if cerr != nil {
			return false, cerr
		}
		if ok {
			if err := os.Chmod(tokenPath, 0o600); err != nil {
				return false, fmt.Errorf("chmod token file %s: %w", tokenPath, err)
			}
			w.printf("Updated permissions on %s to 0600.\n", tokenPath)
		}
	}
	desired.Auth.Mode = decide.WebAuthToken
	desired.Auth.TokenFile = tokenPath
	return true, nil
}

func (w *wizard) stepService(ctx context.Context, tokenPath string) error {
	w.printf("\n==> Step 2: Background service\n")
	switch w.env.GOOS {
	case "linux":
		return w.stepServiceLinux(ctx, tokenPath)
	case "darwin":
		return w.stepServiceDarwin(ctx, tokenPath)
	default:
		w.printf("Automatic service installation is not supported on %s.\n", w.env.GOOS)
		return nil
	}
}

func systemdUnitPath(homeDir string) string {
	return filepath.Join(homeDir, ".config", "systemd", "user", systemdWebServiceName)
}

func systemdUnitContent(exe, tokenPath string) string {
	return fmt.Sprintf(`[Unit]
Description=decide web inbox

[Service]
ExecStart=%s web --token-file %s
Restart=on-failure

[Install]
WantedBy=default.target
`, exe, tokenPath)
}

func (w *wizard) stepServiceLinux(ctx context.Context, tokenPath string) error {
	unitPath := systemdUnitPath(w.env.HomeDir)
	want := systemdUnitContent(w.env.Executable, tokenPath)

	existing, err := os.ReadFile(unitPath)
	unitMatches := err == nil && string(existing) == want
	activeOut, _, activeErr := w.env.RunCmd(ctx, "systemctl", "--user", "is-active", systemdWebServiceName)
	isActive := activeErr == nil && strings.TrimSpace(activeOut) == "active"

	if unitMatches && isActive {
		w.printf("Already done: systemd user service %s is installed and active.\n", systemdWebServiceName)
	} else if err := w.installSystemdUnit(ctx, unitPath, want, unitMatches); err != nil {
		return err
	}
	return w.stepLingerLinux(ctx)
}

func (w *wizard) installSystemdUnit(ctx context.Context, unitPath, want string, unitMatches bool) error {
	if !unitMatches {
		w.printf("Plan: write %s:\n%s", unitPath, want)
		w.printf("Plan: run `systemctl --user daemon-reload`\n")
	}
	w.printf("Plan: run `systemctl --user enable --now %s`\n", systemdWebServiceName)
	ok, err := w.confirm(fmt.Sprintf("Install and start %s?", systemdWebServiceName), false)
	if err != nil || !ok {
		return err
	}
	if !unitMatches {
		if err := os.MkdirAll(filepath.Dir(unitPath), 0o755); err != nil {
			return fmt.Errorf("create systemd user dir: %w", err)
		}
		if err := os.WriteFile(unitPath, []byte(want), 0o600); err != nil {
			return fmt.Errorf("write %s: %w", unitPath, err)
		}
		if _, stderr, err := w.env.RunCmd(ctx, "systemctl", "--user", "daemon-reload"); err != nil {
			return fmt.Errorf("systemctl daemon-reload: %w (%s)", err, strings.TrimSpace(stderr))
		}
	}
	if _, stderr, err := w.env.RunCmd(ctx, "systemctl", "--user", "enable", "--now", systemdWebServiceName); err != nil {
		return fmt.Errorf("systemctl enable --now: %w (%s)", err, strings.TrimSpace(stderr))
	}
	w.printf("Installed and started %s.\n", systemdWebServiceName)
	return nil
}

func (w *wizard) stepLingerLinux(ctx context.Context) error {
	// Linger check: requires sudo, so always asks even with --yes.
	lingerOut, _, lingerErr := w.env.RunCmd(ctx, "loginctl", "show-user", w.env.User, "--property=Linger", "--value")
	if lingerErr == nil && strings.EqualFold(strings.TrimSpace(lingerOut), "yes") {
		w.printf("Already done: linger is enabled for %s.\n", w.env.User)
		return nil
	}
	w.printf("Plan: run `sudo loginctl enable-linger %s`\n", w.env.User)
	ok, err := w.confirm(fmt.Sprintf("Run `sudo loginctl enable-linger %s`?", w.env.User), true)
	if err != nil || !ok {
		return err
	}
	if err := w.env.RunSudoCmd(ctx, "sudo", "loginctl", "enable-linger", w.env.User); err != nil {
		return fmt.Errorf("sudo loginctl enable-linger: %w", err)
	}
	w.printf("Enabled linger for %s.\n", w.env.User)
	return nil
}

func launchAgentPath(homeDir string) string {
	return filepath.Join(homeDir, "Library", "LaunchAgents", launchAgentWebLabel+".plist")
}

func launchAgentLogPath(homeDir string) string {
	return filepath.Join(homeDir, "Library", "Logs", "decide-web.log")
}

func launchAgentContent(exe, tokenPath, logPath string) string {
	return fmt.Sprintf(`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>%s</string>
  <key>ProgramArguments</key>
  <array>
    <string>%s</string>
    <string>web</string>
    <string>--token-file</string>
    <string>%s</string>
  </array>
  <key>RunAtLoad</key>
  <true/>
  <key>KeepAlive</key>
  <true/>
  <key>StandardErrorPath</key>
  <string>%s</string>
</dict>
</plist>
`, launchAgentWebLabel, xmlEscape(exe), xmlEscape(tokenPath), xmlEscape(logPath))
}

func xmlEscape(s string) string {
	var buf bytes.Buffer
	_ = xml.EscapeText(&buf, []byte(s))
	return buf.String()
}

func (w *wizard) stepServiceDarwin(ctx context.Context, tokenPath string) error {
	plistPath := launchAgentPath(w.env.HomeDir)
	logPath := launchAgentLogPath(w.env.HomeDir)
	want := launchAgentContent(w.env.Executable, tokenPath, logPath)
	domain := "gui/" + w.env.UID
	serviceTarget := domain + "/" + launchAgentWebLabel

	existing, err := os.ReadFile(plistPath)
	plistMatches := err == nil && string(existing) == want
	_, _, loadedErr := w.env.RunCmd(ctx, "launchctl", "print", serviceTarget)
	isLoaded := loadedErr == nil

	if plistMatches && isLoaded {
		w.printf("Already done: LaunchAgent %s is installed and loaded.\n", launchAgentWebLabel)
		return nil
	}
	if !plistMatches {
		w.printf("Plan: write %s:\n%s", plistPath, want)
	}
	if isLoaded {
		w.printf("Plan: run `launchctl bootout %s`\n", serviceTarget)
	}
	w.printf("Plan: run `launchctl bootstrap %s %s`\n", domain, plistPath)
	ok, err := w.confirm(fmt.Sprintf("Install and load LaunchAgent %s?", launchAgentWebLabel), false)
	if err != nil {
		return err
	}
	if !ok {
		return nil
	}
	if !plistMatches {
		if err := os.MkdirAll(filepath.Dir(plistPath), 0o755); err != nil {
			return fmt.Errorf("create LaunchAgents dir: %w", err)
		}
		if err := os.WriteFile(plistPath, []byte(want), 0o600); err != nil {
			return fmt.Errorf("write %s: %w", plistPath, err)
		}
	}
	if isLoaded {
		_, _, _ = w.env.RunCmd(ctx, "launchctl", "bootout", serviceTarget)
	}
	if _, stderr, err := w.env.RunCmd(ctx, "launchctl", "bootstrap", domain, plistPath); err != nil {
		return fmt.Errorf("launchctl bootstrap: %w (%s)", err, strings.TrimSpace(stderr))
	}
	w.printf("Installed and loaded LaunchAgent %s.\n", launchAgentWebLabel)
	return nil
}

func (w *wizard) stepIngress(ctx context.Context, addr string, desired *decide.WebConfig) error {
	w.printf("\n==> Step 3: HTTPS ingress\n")
	if _, err := w.env.LookPath("tailscale"); err == nil {
		return w.stepIngressTailscale(ctx, addr, desired)
	}
	w.printf("tailscale not found on PATH. Use Caddy or Cloudflare Tunnel to publish http://%s over HTTPS:\n\n", addr)
	w.printf("Caddy:\n  decide.example.com {\n    reverse_proxy %s {\n      flush_interval -1\n    }\n  }\n\n", addr)
	w.printf("Cloudflare Tunnel:\n  cloudflared tunnel --url http://%s\n\n", addr)
	if desired.PublicURL != "" {
		w.printf("Already done: web.publicUrl is set to %s.\n", desired.PublicURL)
		return nil
	}
	if w.opts.DryRun {
		w.printf("[dry-run] Would ask for web.publicUrl.\n")
		return nil
	}
	if w.opts.Yes {
		w.printf("Skipping public URL prompt (--yes); set web.publicUrl in %s once ingress is ready.\n", w.env.ConfigPath)
		return nil
	}
	raw, err := w.readLine("Public HTTPS URL (leave blank to skip): ")
	if err != nil {
		return err
	}
	if raw != "" {
		desired.PublicURL = strings.TrimRight(raw, "/")
	}
	return nil
}

func (w *wizard) stepIngressTailscale(ctx context.Context, addr string, desired *decide.WebConfig) error {
	domains, err := tailscaleCertDomains(ctx, w.env.RunCmd)
	if err != nil {
		return fmt.Errorf("check tailscale HTTPS certificates: %w", err)
	}
	if len(domains) == 0 {
		w.printf("Tailscale HTTPS certificates are not enabled on this tailnet.\n")
		w.printf("Enable MagicDNS and HTTPS Certificates in the Tailscale admin console (DNS settings), then re-run `decide web setup`.\n")
		return nil
	}
	certDomain := domains[0]
	expectedURL := "https://" + certDomain

	servedURL, servingAddr, err := tailscaleServeStatus(ctx, w.env.RunCmd, addr)
	if err == nil && servingAddr {
		if servedURL != "" {
			expectedURL = servedURL
		}
		w.printf("Already done: Tailscale Serve is forwarding %s -> http://%s.\n", expectedURL, addr)
		desired.PublicURL = expectedURL
		return nil
	}

	op, _ := tailscaleOperator(ctx, w.env.RunCmd)
	if op != w.env.User {
		w.printf("Plan: run `sudo tailscale set --operator=%s`\n", w.env.User)
		ok, cerr := w.confirm(fmt.Sprintf("Run `sudo tailscale set --operator=%s`?", w.env.User), true)
		if cerr != nil {
			return cerr
		}
		if ok {
			if err := w.env.RunSudoCmd(ctx, "sudo", "tailscale", "set", "--operator="+w.env.User); err != nil {
				return fmt.Errorf("sudo tailscale set --operator: %w", err)
			}
			w.printf("Granted Tailscale operator permission to %s.\n", w.env.User)
		}
	}

	target := "http://" + addr
	w.printf("Plan: run `tailscale serve --bg %s`\n", target)
	ok, err := w.confirm(fmt.Sprintf("Run `tailscale serve --bg %s`?", target), false)
	if err != nil {
		return err
	}
	if ok {
		if _, stderr, err := w.env.RunCmd(ctx, "tailscale", "serve", "--bg", target); err != nil {
			return fmt.Errorf("tailscale serve: %w (%s)", err, strings.TrimSpace(stderr))
		}
		w.printf("Configured Tailscale Serve: %s -> %s.\n", expectedURL, target)
		desired.PublicURL = expectedURL
	}
	return nil
}

func (w *wizard) stepConfig(cfg *decide.Config, desired decide.WebConfig) error {
	w.printf("\n==> Step 4: Configuration (%s)\n", w.env.ConfigPath)
	if cfg.Web == desired {
		w.printf("Already done: %s is up to date.\n", w.env.ConfigPath)
		return nil
	}
	w.printf("Plan: back up %s to %s.bak (if present) and update web config:\n", w.env.ConfigPath, w.env.ConfigPath)
	if desired.PublicURL != "" {
		w.printf("  web.publicUrl = %q\n", desired.PublicURL)
	}
	if desired.Auth.Mode != "" {
		w.printf("  web.auth.mode = %q\n", desired.Auth.Mode)
	}
	if desired.Auth.TokenFile != "" {
		w.printf("  web.auth.tokenFile = %q\n", desired.Auth.TokenFile)
	}
	ok, err := w.confirm(fmt.Sprintf("Update %s?", w.env.ConfigPath), false)
	if err != nil {
		return err
	}
	if !ok {
		return nil
	}
	if err := decide.SetWebConfigAt(w.env.ConfigPath, desired); err != nil {
		return fmt.Errorf("update decide config: %w", err)
	}
	cfg.Web = desired
	w.printf("Updated %s.\n", w.env.ConfigPath)
	return nil
}

func (w *wizard) stepPush(cfg *decide.Config, desired *decide.WebConfig) error {
	w.printf("\n==> Step 5: Web Push (optional)\n")
	if cfg.Web.Push.Enabled && isValidPushSubject(cfg.Web.Push.Subject) {
		w.printf("Already done: Web Push is enabled (subject: %s).\n", cfg.Web.Push.Subject)
		return nil
	}
	if w.opts.DryRun {
		w.printf("[dry-run] Would ask whether to enable Web Push and prompt for a mailto: or https: subject.\n")
		return nil
	}
	if w.opts.Yes {
		if isValidPushSubject(desired.Push.Subject) {
			desired.Push.Enabled = true
			if err := decide.SetWebConfigAt(w.env.ConfigPath, *desired); err != nil {
				return fmt.Errorf("update web push config: %w", err)
			}
			cfg.Web = *desired
			w.printf("Enabled Web Push (subject: %s).\n", desired.Push.Subject)
		} else {
			w.printf("Skipping optional Web Push (--yes; configure web.push.subject with a real mailto: or https: URL to enable).\n")
		}
		return nil
	}

	ok, err := w.confirm("Enable Web Push notifications?", false)
	if err != nil {
		return err
	}
	if !ok {
		return nil
	}
	for {
		subj, err := w.readLine("VAPID contact (mailto:you@example.com or https://...): ")
		if err != nil {
			return err
		}
		if subj == "" {
			w.printf("Web Push skipped (no contact entered).\n")
			return nil
		}
		if !isValidPushSubject(subj) {
			w.printf("Refused %q: must be a real mailto: or https: URL (not %s).\n", subj, DefaultVAPIDSubject)
			continue
		}
		desired.Push.Enabled = true
		desired.Push.Subject = subj
		if err := decide.SetWebConfigAt(w.env.ConfigPath, *desired); err != nil {
			return fmt.Errorf("update web push config: %w", err)
		}
		cfg.Web = *desired
		w.printf("Enabled Web Push (subject: %s).\n", subj)
		return nil
	}
}

func (w *wizard) stepFinalCheck(ctx context.Context, addr, publicURL string) {
	w.printf("\n==> Step 6: Final check\n")
	loopbackURL := "http://" + addr
	if w.opts.DryRun {
		w.printf("[dry-run] Would check %s/api/v1/health\n", loopbackURL)
		if publicURL != "" {
			w.printf("[dry-run] Would check %s/api/v1/health\n", publicURL)
		}
		return
	}

	if ok, _, err := checkHealth(ctx, w.env.HTTPClient, loopbackURL); err != nil || !ok {
		w.printf("Loopback health (%s/api/v1/health): FAIL (%v)\n", loopbackURL, err)
	} else {
		w.printf("Loopback health (%s/api/v1/health): ok\n", loopbackURL)
	}

	if publicURL != "" {
		if ok, secure, err := checkHealth(ctx, w.env.HTTPClient, publicURL); err != nil || !ok {
			w.printf("Public health (%s/api/v1/health): FAIL (%v)\n", publicURL, err)
		} else if !secure {
			w.printf("Public health (%s/api/v1/health): ok (warning: X-Forwarded-Proto: https not seen)\n", publicURL)
		} else {
			w.printf("Public health (%s/api/v1/health): ok (HTTPS/Secure cookie verified)\n", publicURL)
		}
	}

	openURL := publicURL
	if openURL == "" {
		openURL = loopbackURL
	}
	w.printf("\nAddress: %s\n", openURL)
	w.printf("Phone install:\n")
	w.printf("  iPhone: open in Safari > Share > Add to Home Screen, then open the app and turn on notifications in Settings.\n")
	w.printf("  Android: open in Chrome > menu > Install app (or Add to Home screen).\n")
}

// isValidPushSubject reports whether raw is a real mailto: or https: VAPID
// subject, rejecting empty values and localhost defaults like
// mailto:decide@localhost.
func isValidPushSubject(raw string) bool {
	s := strings.TrimSpace(raw)
	if s == "" || strings.EqualFold(s, DefaultVAPIDSubject) {
		return false
	}
	u, err := url.Parse(s)
	if err != nil {
		return false
	}
	switch strings.ToLower(u.Scheme) {
	case "mailto":
		addr := strings.TrimSpace(u.Opaque)
		local, domain, ok := strings.Cut(addr, "@")
		if !ok || local == "" || domain == "" {
			return false
		}
		d := strings.ToLower(domain)
		return d != "localhost" && !strings.HasSuffix(d, ".localhost") && strings.Contains(d, ".")
	case "https":
		host := strings.ToLower(u.Hostname())
		return host != "" && host != "localhost" && !strings.HasSuffix(host, ".localhost")
	default:
		return false
	}
}

func tailscaleCertDomains(ctx context.Context, runCmd func(context.Context, string, ...string) (string, string, error)) ([]string, error) {
	stdout, stderr, err := runCmd(ctx, "tailscale", "status", "--json")
	if err != nil {
		return nil, fmt.Errorf("tailscale status --json: %w (%s)", err, strings.TrimSpace(stderr))
	}
	var st struct {
		CertDomains []string
	}
	if err := json.Unmarshal([]byte(stdout), &st); err != nil {
		return nil, fmt.Errorf("parse tailscale status: %w", err)
	}
	return st.CertDomains, nil
}

func tailscaleOperator(ctx context.Context, runCmd func(context.Context, string, ...string) (string, string, error)) (string, error) {
	stdout, stderr, err := runCmd(ctx, "tailscale", "debug", "prefs")
	if err != nil {
		return "", fmt.Errorf("tailscale debug prefs: %w (%s)", err, strings.TrimSpace(stderr))
	}
	var prefs struct {
		OperatorUser string
	}
	if err := json.Unmarshal([]byte(stdout), &prefs); err != nil {
		return "", fmt.Errorf("parse tailscale prefs: %w", err)
	}
	return prefs.OperatorUser, nil
}

// tailscaleServeStatus returns the first HTTPS serve URL and whether any
// handler proxies to http://<addr>.
func tailscaleServeStatus(ctx context.Context, runCmd func(context.Context, string, ...string) (string, string, error), addr string) (servedURL string, matchesAddr bool, err error) {
	stdout, stderr, err := runCmd(ctx, "tailscale", "serve", "status", "--json")
	if err != nil {
		return "", false, fmt.Errorf("tailscale serve status --json: %w (%s)", err, strings.TrimSpace(stderr))
	}
	var st struct {
		Web map[string]struct {
			Handlers map[string]struct {
				Proxy string
			}
		}
	}
	if err := json.Unmarshal([]byte(stdout), &st); err != nil {
		return "", false, fmt.Errorf("parse tailscale serve status: %w", err)
	}
	wantProxy := "http://" + addr
	for hostPort, cfg := range st.Web {
		u := formatTailscaleServeURL(hostPort)
		if servedURL == "" {
			servedURL = u
		}
		for _, h := range cfg.Handlers {
			if strings.TrimRight(h.Proxy, "/") == wantProxy {
				return u, true, nil
			}
		}
	}
	return servedURL, false, nil
}

func formatTailscaleServeURL(hostPort string) string {
	host, port, err := net.SplitHostPort(hostPort)
	if err != nil {
		return "https://" + strings.TrimRight(hostPort, "/")
	}
	if port == "443" {
		return "https://" + host
	}
	return "https://" + net.JoinHostPort(host, port)
}

func checkHealth(ctx context.Context, client *http.Client, baseURL string) (ok bool, secure bool, err error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, strings.TrimRight(baseURL, "/")+"/api/v1/health", nil)
	if err != nil {
		return false, false, fmt.Errorf("build health request: %w", err)
	}
	resp, err := client.Do(req)
	if err != nil {
		return false, false, fmt.Errorf("health request: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return false, false, fmt.Errorf("HTTP %d", resp.StatusCode)
	}
	var body struct {
		OK     bool `json:"ok"`
		Secure bool `json:"secure"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		return false, false, fmt.Errorf("decode health response: %w", err)
	}
	return body.OK, body.Secure, nil
}
