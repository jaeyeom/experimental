package main

import (
	"bytes"
	"context"
	"encoding/xml"
	"errors"
	"flag"
	"fmt"
	"io"
	"log/slog"
	"os"
	"os/signal"
	"os/user"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"golang.org/x/sys/unix"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/dlp"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/harness"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/quota"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/sandbox"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/worker"
)

const (
	workerLaunchdLabel    = "com.agent-pool.worker"
	workerSystemdUnitName = "agent-pool-worker.service"
)

func cmdInitAuth(ctx context.Context, args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("init-auth", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", worker.DefaultConfigPath(), "path to worker.toml")
	skipLogin := fs.Bool("skip-login", false, "create isolated layout without launching interactive claude login")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "Usage: agent-pool init-auth [--config worker.toml]")
		return 2
	}

	isoHome := worker.DefaultIsolatedHome()
	if cfg, err := worker.LoadConfig(*configPath); err == nil {
		isoHome = cfg.Worker.IsolatedHome
	} else if !errors.Is(err, os.ErrNotExist) {
		fmt.Fprintf(stderr, "agent-pool init-auth: %v\n", err)
		return 1
	}

	res, err := worker.InitAuth(ctx, worker.InitAuthOptions{
		IsolatedHome: isoHome,
		SkipLogin:    *skipLogin,
		Stdout:       stdout,
		Stderr:       stderr,
	})
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool init-auth: %v\n", err)
		return 1
	}

	fmt.Fprintf(stdout, "isolatedHome: %s\n", res.IsolatedHome)
	fmt.Fprintf(stdout, "claudeConfigDir: %s\n", res.ClaudeConfigDir)
	fmt.Fprintf(stdout, "emptyMCPPath: %s\n", res.EmptyMCPPath)
	fmt.Fprintf(stdout, "gitConfigPath: %s\n", res.GitConfigPath)
	fmt.Fprintln(stdout, res.Instructions)
	return 0
}

func cmdWorker(ctx context.Context, args []string, stdout, stderr io.Writer, deps cliDeps) int {
	if len(args) == 0 {
		printWorkerUsage(stderr)
		return 2
	}
	switch args[0] {
	case "run":
		return cmdWorkerRun(ctx, args[1:], stderr)
	case "pause":
		return cmdWorkerPause(args[1:], stdout, stderr)
	case "resume":
		return cmdWorkerResume(args[1:], stdout, stderr)
	case "stats":
		return cmdWorkerStats(ctx, args[1:], stdout, stderr)
	case "install":
		return cmdWorkerInstall(ctx, args[1:], stdout, stderr, deps)
	case "-h", "--help", "help":
		printWorkerUsage(stdout)
		return 0
	default:
		fmt.Fprintf(stderr, "agent-pool worker: unknown subcommand %q\n", args[0])
		printWorkerUsage(stderr)
		return 2
	}
}

func printWorkerUsage(w io.Writer) {
	fmt.Fprintln(w, "Usage: agent-pool worker <run|pause|resume|stats|install> [flags]")
}

func cmdWorkerRun(ctx context.Context, args []string, stderr io.Writer) int {
	fs := flag.NewFlagSet("worker run", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", worker.DefaultConfigPath(), "path to worker.toml")
	dryRun := fs.Bool("dry-run", false, "connect and report heartbeats without claiming jobs")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "Usage: agent-pool worker run [--config worker.toml] [--dry-run]")
		return 2
	}

	cfg, err := worker.LoadConfig(*configPath)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker run: %v\n", err)
		return 1
	}
	cfg.DryRun = *dryRun

	if err := runWorkerDaemon(ctx, cfg, stderr); err != nil {
		fmt.Fprintf(stderr, "agent-pool worker run: %v\n", err)
		return 1
	}
	return 0
}

func runWorkerDaemon(ctx context.Context, cfg *worker.Config, stderr io.Writer) error {
	if strings.TrimSpace(cfg.Worker.BrokerURL) == "" {
		return errors.New("worker.broker_url must be configured in worker.toml")
	}
	token, err := cfg.ResolveToken()
	if err != nil {
		return fmt.Errorf("resolve worker token: %w", err)
	}

	logger := slog.New(slog.NewTextHandler(stderr, nil))
	ledgerPath := filepath.Join(cfg.Worker.StateDir, "usage.db")
	ledger, err := quota.OpenLedger(ledgerPath)
	if err != nil {
		return fmt.Errorf("open usage ledger: %w", err)
	}
	defer func() {
		if cerr := ledger.Close(); cerr != nil {
			logger.Warn("close usage ledger failed", "error", cerr)
		}
	}()

	claudeDir := filepath.Join(cfg.Worker.IsolatedHome, ".claude")
	guard, err := quota.NewClaudeGuard(cfg.Quota, quota.NewExecClaudeUsageReader(claudeDir))
	if err != nil {
		return fmt.Errorf("initialize quota guard: %w", err)
	}

	client := worker.NewBrokerClient(cfg.Worker.BrokerURL, token, nil)
	client.SetRepos(cfg.Worker.SupportedRepos)

	daemon := worker.NewDaemon(cfg, worker.Deps{
		Client:    client,
		Quota:     guard,
		Ledger:    ledger,
		Worktrees: worker.NewWorktreeManager(cfg.Worker.StateDir, cfg.RepoRemotes(), logger),
		Sandbox:   &sandbox.CpltRunner{},
		Harness:   harness.ClaudeRunner{},
		DLP:       dlp.NewScanner(),
		HostCtx:   currentHostContext(),
		Logger:    logger,
	})

	sigCtx, stop := signal.NotifyContext(ctx, os.Interrupt, unix.SIGTERM)
	defer stop()

	if err := daemon.Run(sigCtx); err != nil {
		return fmt.Errorf("worker daemon: %w", err)
	}
	return nil
}

func currentHostContext() dlp.HostContext {
	home, _ := os.UserHomeDir()
	hostname, _ := os.Hostname()
	var username string
	if u, err := user.Current(); err == nil {
		username = u.Username
	}
	return dlp.HostContext{
		HomeDir:  home,
		Username: username,
		Hostname: hostname,
	}
}

func cmdWorkerPause(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("worker pause", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", worker.DefaultConfigPath(), "path to worker.toml")
	reason := fs.String("reason", "paused", "reason for pausing worker")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "Usage: agent-pool worker pause [--config worker.toml] [--reason \"...\"]")
		return 2
	}

	stateDir, err := resolveWorkerStateDir(*configPath)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker pause: %v\n", err)
		return 1
	}
	if err := worker.Pause(stateDir, *reason); err != nil {
		fmt.Fprintf(stderr, "agent-pool worker pause: %v\n", err)
		return 1
	}
	fmt.Fprintf(stdout, "paused worker (stateDir=%s, reason=%s)\n", stateDir, *reason)
	return 0
}

func cmdWorkerResume(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("worker resume", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", worker.DefaultConfigPath(), "path to worker.toml")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "Usage: agent-pool worker resume [--config worker.toml]")
		return 2
	}

	stateDir, err := resolveWorkerStateDir(*configPath)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker resume: %v\n", err)
		return 1
	}
	if err := worker.Resume(stateDir); err != nil {
		fmt.Fprintf(stderr, "agent-pool worker resume: %v\n", err)
		return 1
	}
	fmt.Fprintf(stdout, "resumed worker (stateDir=%s)\n", stateDir)
	return 0
}

func cmdWorkerStats(ctx context.Context, args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("worker stats", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", worker.DefaultConfigPath(), "path to worker.toml")
	sinceStr := fs.String("since", "7d", "lookback duration (e.g. 24h, 7d, 30d, all)")
	asJSON := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "Usage: agent-pool worker stats [--config worker.toml] [--since 7d] [--json]")
		return 2
	}

	sinceTime, err := parseSinceTime(*sinceStr, time.Now().UTC())
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker stats: %v\n", err)
		return 2
	}

	stateDir, err := resolveWorkerStateDir(*configPath)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker stats: %v\n", err)
		return 1
	}

	ledger, err := quota.OpenLedger(filepath.Join(stateDir, "usage.db"))
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker stats: %v\n", err)
		return 1
	}
	defer func() { _ = ledger.Close() }()

	stats, err := ledger.Stats(ctx, sinceTime)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker stats: %v\n", err)
		return 1
	}
	if *asJSON {
		if err := writeJSONOut(stdout, stats); err != nil {
			fmt.Fprintf(stderr, "agent-pool worker stats: %v\n", err)
			return 1
		}
		return 0
	}

	fmt.Fprintf(stdout, "totalJobs: %d\n", stats.TotalJobs)
	fmt.Fprintf(stdout, "totalTokens: %d\n", stats.TotalTokens)
	fmt.Fprintf(stdout, "effectiveUnits: %d\n", stats.EffectiveUnits)
	fmt.Fprintf(stdout, "totalDurationMs: %d\n", stats.TotalDurationMs)
	return 0
}

func resolveWorkerStateDir(configPath string) (string, error) {
	cfg, err := worker.LoadConfig(configPath)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return worker.DefaultStateDir(), nil
		}
		return "", fmt.Errorf("load worker config: %w", err)
	}
	return cfg.Worker.StateDir, nil
}

func parseSinceTime(raw string, now time.Time) (time.Time, error) {
	s := strings.TrimSpace(raw)
	if s == "" || s == "all" || s == "0" {
		return time.Time{}, nil
	}
	if strings.HasSuffix(s, "d") {
		days, err := strconv.Atoi(strings.TrimSuffix(s, "d"))
		if err != nil || days < 0 {
			return time.Time{}, fmt.Errorf("invalid --since duration %q", raw)
		}
		return now.Add(-time.Duration(days) * 24 * time.Hour), nil
	}
	d, err := time.ParseDuration(s)
	if err != nil || d < 0 {
		return time.Time{}, fmt.Errorf("invalid --since duration %q", raw)
	}
	return now.Add(-d), nil
}

func cmdWorkerInstall(ctx context.Context, args []string, stdout, stderr io.Writer, deps cliDeps) int {
	fs := flag.NewFlagSet("worker install", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", worker.DefaultConfigPath(), "path to worker.toml")
	dryRun := fs.Bool("dry-run", false, "print rendered service file without writing or loading")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "Usage: agent-pool worker install [--config worker.toml] [--dry-run]")
		return 2
	}

	home, err := deps.homeDir()
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker install: resolve home dir: %v\n", err)
		return 1
	}
	exe, err := deps.executable()
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker install: resolve executable: %v\n", err)
		return 1
	}

	targetPath, err := workerUnitPath(deps.goos, home)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker install: %v\n", err)
		return 1
	}
	body, err := renderWorkerService(deps.goos, exe, *configPath)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool worker install: %v\n", err)
		return 1
	}

	if *dryRun {
		fmt.Fprintf(stdout, "# target: %s\n%s", targetPath, body)
		return 0
	}

	if err := writeFileAtomic(targetPath, []byte(body), 0o644); err != nil {
		fmt.Fprintf(stderr, "agent-pool worker install: %v\n", err)
		return 1
	}
	if err := loadWorkerService(ctx, deps, targetPath); err != nil {
		fmt.Fprintf(stderr, "agent-pool worker install: wrote %s, failed to load: %v\n", targetPath, err)
		return 1
	}
	fmt.Fprintf(stdout, "installed %s\n", targetPath)
	return 0
}

func workerUnitPath(goos, home string) (string, error) {
	switch goos {
	case "linux":
		cfgHome := strings.TrimSpace(os.Getenv("XDG_CONFIG_HOME"))
		if cfgHome == "" {
			cfgHome = filepath.Join(home, ".config")
		}
		return filepath.Join(cfgHome, "systemd", "user", workerSystemdUnitName), nil
	case "darwin":
		return filepath.Join(home, "Library", "LaunchAgents", workerLaunchdLabel+".plist"), nil
	default:
		return "", fmt.Errorf("worker install supports linux (systemd) and darwin (launchd), not %s", goos)
	}
}

func renderWorkerService(goos, binPath, configPath string) (string, error) {
	execArgs := []string{binPath, "worker", "run", "--config", configPath}
	switch goos {
	case "linux":
		return renderSystemdUnit(execArgs), nil
	case "darwin":
		return renderLaunchdPlist(workerLaunchdLabel, execArgs)
	default:
		return "", fmt.Errorf("worker install supports linux (systemd) and darwin (launchd), not %s", goos)
	}
}

func renderSystemdUnit(execArgs []string) string {
	var b strings.Builder
	b.WriteString("[Unit]\n")
	b.WriteString("Description=agent-pool consumer worker daemon\n")
	b.WriteString("After=network-online.target\n")
	b.WriteString("Wants=network-online.target\n\n")
	b.WriteString("[Service]\n")
	b.WriteString("ExecStart=" + formatSystemdLine(execArgs) + "\n")
	b.WriteString("Restart=on-failure\n")
	b.WriteString("RestartSec=5s\n\n")
	b.WriteString("[Install]\n")
	b.WriteString("WantedBy=default.target\n")
	return b.String()
}

func formatSystemdLine(args []string) string {
	quoted := make([]string, len(args))
	for i, arg := range args {
		quoted[i] = quoteSystemdArg(arg)
	}
	return strings.Join(quoted, " ")
}

func quoteSystemdArg(s string) string {
	if s == "" {
		return `""`
	}
	for _, r := range s {
		if isSystemdPlainRune(r) {
			continue
		}
		return `"` + strings.NewReplacer(`\`, `\\`, `"`, `\"`).Replace(s) + `"`
	}
	return s
}

func isSystemdPlainRune(r rune) bool {
	if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') {
		return true
	}
	return strings.ContainsRune("/._:@+-", r)
}

func renderLaunchdPlist(label string, args []string) (string, error) {
	escLabel, err := escapeXMLText(label)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString(`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>` + escLabel + `</string>
  <key>ProgramArguments</key>
  <array>
`)
	for _, arg := range args {
		esc, err := escapeXMLText(arg)
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&b, "    <string>%s</string>\n", esc)
	}
	b.WriteString(`  </array>
  <key>RunAtLoad</key>
  <true/>
  <key>KeepAlive</key>
  <dict>
    <key>SuccessfulExit</key>
    <false/>
  </dict>
</dict>
</plist>
`)
	return b.String(), nil
}

func escapeXMLText(s string) (string, error) {
	var b bytes.Buffer
	if err := xml.EscapeText(&b, []byte(s)); err != nil {
		return "", fmt.Errorf("escape XML: %w", err)
	}
	return b.String(), nil
}

func loadWorkerService(ctx context.Context, deps cliDeps, targetPath string) error {
	switch deps.goos {
	case "linux":
		if _, err := deps.runCmd(ctx, "systemctl", []string{"--user", "daemon-reload"}, nil); err != nil {
			return fmt.Errorf("systemctl daemon-reload: %w", err)
		}
		if _, err := deps.runCmd(ctx, "systemctl", []string{"--user", "enable", "--now", workerSystemdUnitName}, nil); err != nil {
			return fmt.Errorf("systemctl enable --now: %w", err)
		}
		return nil
	case "darwin":
		domain := "gui/" + strconv.Itoa(os.Getuid())
		_, _ = deps.runCmd(ctx, "launchctl", []string{"bootout", domain, targetPath}, nil)
		if _, err := deps.runCmd(ctx, "launchctl", []string{"bootstrap", domain, targetPath}, nil); err != nil {
			return fmt.Errorf("launchctl bootstrap: %w", err)
		}
		return nil
	default:
		return fmt.Errorf("unsupported OS %s", deps.goos)
	}
}

func writeFileAtomic(path string, data []byte, mode os.FileMode) error {
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("create dir %q: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, ".agent-pool-tmp-*")
	if err != nil {
		return fmt.Errorf("create temp file: %w", err)
	}
	tmpName := tmp.Name()
	committed := false
	defer func() {
		if !committed {
			_ = tmp.Close()
			_ = os.Remove(tmpName)
		}
	}()
	if _, err := tmp.Write(data); err != nil {
		return fmt.Errorf("write %q: %w", path, err)
	}
	if err := tmp.Chmod(mode); err != nil {
		return fmt.Errorf("chmod %q: %w", path, err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("close %q: %w", path, err)
	}
	if err := os.Rename(tmpName, path); err != nil {
		return fmt.Errorf("rename %q: %w", path, err)
	}
	committed = true
	return nil
}
