// Command agent-poold runs the central agent-pool broker server and manages
// broker API tokens.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"time"

	"golang.org/x/sys/unix"

	"github.com/chorong-sh/chorong/pkg/version"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/catsync"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/ghacl"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/httpapi"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/mcp"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/sqlite"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/dlp"
)

const (
	httpReadHeaderTimeout = 10 * time.Second
	httpShutdownTimeout   = 10 * time.Second
	maxSweeperInterval    = 10 * time.Second
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		printUsage(stderr)
		return 2
	}

	var err error
	switch args[0] {
	case "serve":
		err = runServe(args[1:], stderr)
	case "token":
		err = runToken(args[1:], stdout, stderr)
	case "version":
		err = runVersion(args[1:], stdout, stderr)
	case "-h", "--help", "help":
		printUsage(stdout)
		return 0
	default:
		fmt.Fprintf(stderr, "unknown subcommand %q\n", args[0])
		printUsage(stderr)
		return 2
	}
	if err != nil {
		fmt.Fprintf(stderr, "agent-poold: %v\n", err)
		return 1
	}
	return 0
}

func printUsage(w io.Writer) {
	fmt.Fprintln(w, "Usage: agent-poold <command> [flags]")
	fmt.Fprintln(w, "")
	fmt.Fprintln(w, "Commands:")
	fmt.Fprintln(w, "  serve   --config <broker.toml>")
	fmt.Fprintln(w, "  token   create|ls|revoke [flags]")
	fmt.Fprintln(w, "  version [--json]")
}

func runServe(args []string, stderr io.Writer) error {
	fs := flag.NewFlagSet("serve", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", "broker.toml", "path to broker TOML configuration file")
	if err := fs.Parse(args); err != nil {
		return fmt.Errorf("parse serve flags: %w", err)
	}

	cfg, err := LoadConfig(*configPath)
	if err != nil {
		return err
	}
	durs, err := cfg.ParsedDurations()
	if err != nil {
		return err
	}

	logger := slog.New(slog.NewTextHandler(stderr, nil))
	store, err := sqlite.Open(cfg.DBPath)
	if err != nil {
		return fmt.Errorf("open store: %w", err)
	}
	defer func() {
		if cerr := store.Close(); cerr != nil {
			logger.Warn("close store failed", "error", cerr)
		}
	}()

	ghVerifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: cfg.GitHubACL.APIBaseURL,
		CacheTTL:   durs.GitHubACLCacheTTL,
	}, nil)

	svc := broker.NewService(store, dlp.NewScanner(), broker.NewEventHub(), broker.Config{
		WorkerStaleAfter:          durs.WorkerStaleAfter,
		LeaseTTL:                  durs.LeaseTTL,
		MaxAttempts:               cfg.MaxAttempts,
		MaxConcurrentPerRequester: cfg.MaxConcurrentPerRequester,
		GitHubACLVerifier:         ghVerifier,
		RequireRepoAccessCheck:    cfg.GitHubACL.RequireRepoAccessCheck,
		Logger:                    logger,
	})
	handler := httpapi.NewHandler(svc, store, httpapi.Options{
		MCPHandler: mcp.NewHandler(svc, store),
	})

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, unix.SIGTERM)
	defer stop()

	go svc.StartSweeper(ctx, sweeperInterval(durs.LeaseTTL))
	if len(cfg.Repos) > 0 {
		syncer := catsync.NewSyncer(store, cfg.RepoCacheDir, cfg.Repos, logger)
		go syncer.Run(ctx, durs.CatalogSyncInterval)
	}

	return serveHTTP(ctx, cfg.ListenAddr, handler, logger)
}

func serveHTTP(ctx context.Context, addr string, handler http.Handler, logger *slog.Logger) error {
	srv := &http.Server{
		Addr:              addr,
		Handler:           handler,
		ReadHeaderTimeout: httpReadHeaderTimeout,
	}

	errCh := make(chan error, 1)
	go func() {
		logger.Info("agent-poold listening", "addr", addr)
		errCh <- srv.ListenAndServe()
	}()

	select {
	case err := <-errCh:
		if errors.Is(err, http.ErrServerClosed) {
			return nil
		}
		return fmt.Errorf("http server error: %w", err)
	case <-ctx.Done():
		logger.Info("shutting down agent-poold")
		shutCtx, cancel := context.WithTimeout(context.Background(), httpShutdownTimeout)
		defer cancel()
		if err := srv.Shutdown(shutCtx); err != nil {
			return fmt.Errorf("shutdown http server: %w", err)
		}
		return nil
	}
}

func sweeperInterval(leaseTTL time.Duration) time.Duration {
	d := leaseTTL / 4
	if d <= 0 {
		return time.Second
	}
	if d > maxSweeperInterval {
		return maxSweeperInterval
	}
	return d
}

func runToken(args []string, stdout, stderr io.Writer) error {
	if len(args) == 0 {
		return errors.New("token subcommand requires one of: create, ls, revoke")
	}
	switch args[0] {
	case "create":
		return runTokenCreate(args[1:], stdout, stderr)
	case "ls":
		return runTokenList(args[1:], stdout, stderr)
	case "revoke":
		return runTokenRevoke(args[1:], stdout, stderr)
	default:
		return fmt.Errorf("unknown token subcommand %q (expected create, ls, or revoke)", args[0])
	}
}

func runTokenCreate(args []string, stdout, stderr io.Writer) error {
	fs := flag.NewFlagSet("token create", flag.ContinueOnError)
	fs.SetOutput(stderr)
	dbPath := fs.String("db", "", "path to SQLite broker database")
	roleStr := fs.String("role", "", "token role: producer or worker")
	name := fs.String("name", "", "human-readable token alias")
	reposCSV := fs.String("repos", "", "comma-separated list of allowed repos (optional)")
	if err := fs.Parse(args); err != nil {
		return fmt.Errorf("parse token create flags: %w", err)
	}
	if *dbPath == "" || *name == "" {
		return errors.New("--db and --name are required")
	}
	role := broker.TokenRole(*roleStr)
	if role != broker.TokenRoleProducer && role != broker.TokenRoleWorker {
		return fmt.Errorf("invalid --role %q: must be producer or worker", *roleStr)
	}

	store, err := sqlite.Open(*dbPath)
	if err != nil {
		return fmt.Errorf("open store: %w", err)
	}
	defer func() { _ = store.Close() }()

	plaintext, _, err := store.CreateToken(context.Background(), role, *name, parseCSV(*reposCSV))
	if err != nil {
		return fmt.Errorf("create token: %w", err)
	}
	fmt.Fprintln(stdout, plaintext)
	return nil
}

func runTokenList(args []string, stdout, stderr io.Writer) error {
	fs := flag.NewFlagSet("token ls", flag.ContinueOnError)
	fs.SetOutput(stderr)
	dbPath := fs.String("db", "", "path to SQLite broker database")
	asJSON := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return fmt.Errorf("parse token ls flags: %w", err)
	}
	if *dbPath == "" {
		return errors.New("--db is required")
	}

	store, err := sqlite.Open(*dbPath)
	if err != nil {
		return fmt.Errorf("open store: %w", err)
	}
	defer func() { _ = store.Close() }()

	tokens, err := store.ListTokens(context.Background())
	if err != nil {
		return fmt.Errorf("list tokens: %w", err)
	}
	if *asJSON {
		if tokens == nil {
			tokens = []broker.TokenRecord{}
		}
		if err := json.NewEncoder(stdout).Encode(tokens); err != nil {
			return fmt.Errorf("encode tokens JSON: %w", err)
		}
		return nil
	}

	for _, tok := range tokens {
		status := "active"
		if tok.RevokedAt != nil {
			status = "revoked"
		}
		repos := "*"
		if len(tok.AllowedRepos) > 0 {
			repos = strings.Join(tok.AllowedRepos, ",")
		}
		fmt.Fprintf(stdout, "%s\t%s\t%s\t%s\t%s\n", tok.ID, tok.Role, tok.Name, repos, status)
	}
	return nil
}

func runTokenRevoke(args []string, stdout, stderr io.Writer) error {
	fs := flag.NewFlagSet("token revoke", flag.ContinueOnError)
	fs.SetOutput(stderr)
	dbPath := fs.String("db", "", "path to SQLite broker database")
	id := fs.String("id", "", "token ID to revoke")
	if err := fs.Parse(args); err != nil {
		return fmt.Errorf("parse token revoke flags: %w", err)
	}
	if *dbPath == "" || *id == "" {
		return errors.New("--db and --id are required")
	}

	store, err := sqlite.Open(*dbPath)
	if err != nil {
		return fmt.Errorf("open store: %w", err)
	}
	defer func() { _ = store.Close() }()

	if err := store.RevokeToken(context.Background(), *id); err != nil {
		return fmt.Errorf("revoke token: %w", err)
	}
	fmt.Fprintf(stdout, "revoked token %s\n", *id)
	return nil
}

func runVersion(args []string, stdout, stderr io.Writer) error {
	fs := flag.NewFlagSet("version", flag.ContinueOnError)
	fs.SetOutput(stderr)
	asJSON := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return fmt.Errorf("parse version flags: %w", err)
	}
	info := version.ReadInfo()
	if *asJSON {
		payload := struct {
			Tool        string `json:"tool"`
			ModulePath  string `json:"modulePath,omitempty"`
			Version     string `json:"version"`
			VCSRevision string `json:"vcsRevision,omitempty"`
			VCSModified bool   `json:"vcsModified,omitempty"`
			BuildTime   string `json:"buildTime,omitempty"`
		}{
			Tool:        "agent-poold",
			ModulePath:  info.ModulePath,
			Version:     info.Version,
			VCSRevision: info.VCSRevision,
			VCSModified: info.VCSModified,
			BuildTime:   info.BuildTime,
		}
		if err := json.NewEncoder(stdout).Encode(payload); err != nil {
			return fmt.Errorf("encode version JSON: %w", err)
		}
		return nil
	}
	fmt.Fprintln(stdout, info.Format("agent-poold"))
	return nil
}

func parseCSV(raw string) []string {
	if strings.TrimSpace(raw) == "" {
		return nil
	}
	var out []string
	for _, part := range strings.Split(raw, ",") {
		trimmed := strings.TrimSpace(part)
		if trimmed != "" {
			out = append(out, trimmed)
		}
	}
	return out
}
