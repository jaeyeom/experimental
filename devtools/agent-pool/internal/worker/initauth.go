package worker

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

const (
	defaultClaudeBinary = "claude"
	emptyMCPContent     = "{\"mcpServers\":{}}\n"
	neutralGitConfig    = "[commit]\n\tgpgsign = false\n[credential]\n\thelper =\n[core]\n\tautocrlf = false\n"
	initAuthReminder    = "IMPORTANT: Verify in claude.ai settings that all web-synced cloud connectors (Gmail, Google Drive, Slack, Calendar) are disabled on the donated account before starting the worker daemon."
)

// InitAuthOptions configures isolated consumer authentication setup.
type InitAuthOptions struct {
	IsolatedHome string
	ClaudeBinary string
	SkipLogin    bool
	Stdin        io.Reader
	Stdout       io.Writer
	Stderr       io.Writer
}

// InitAuthResult reports the created isolated paths and post-setup reminders.
type InitAuthResult struct {
	IsolatedHome    string
	ClaudeConfigDir string
	EmptyMCPPath    string
	GitConfigPath   string
	Instructions    string
}

// InitAuth creates the isolated HOME directory layout, empty MCP config, and
// neutral .gitconfig, then optionally runs interactive `claude login` under
// the isolated environment.
func InitAuth(ctx context.Context, opts InitAuthOptions) (InitAuthResult, error) {
	isoHome := strings.TrimSpace(opts.IsolatedHome)
	if isoHome == "" {
		isoHome = DefaultIsolatedHome()
	} else {
		isoHome = expandHome(isoHome)
	}

	claudeDir := filepath.Join(isoHome, ".claude")
	configDir := filepath.Join(isoHome, ".config")
	cpltDir := filepath.Join(configDir, "cplt")
	cacheDir := filepath.Join(isoHome, ".cache")

	for _, dir := range []string{isoHome, claudeDir, configDir, cpltDir, cacheDir} {
		if err := ensureStrictDir(dir); err != nil {
			return InitAuthResult{}, err
		}
	}

	emptyMCPPath := filepath.Join(isoHome, "empty-mcp.json")
	if err := writeStrictFile(emptyMCPPath, []byte(emptyMCPContent)); err != nil {
		return InitAuthResult{}, err
	}

	gitConfigPath := filepath.Join(isoHome, ".gitconfig")
	if err := writeStrictFile(gitConfigPath, []byte(neutralGitConfig)); err != nil {
		return InitAuthResult{}, err
	}

	if !opts.SkipLogin {
		if err := runIsolatedClaudeLogin(ctx, opts, isoHome, claudeDir); err != nil {
			return InitAuthResult{}, err
		}
	}

	return InitAuthResult{
		IsolatedHome:    isoHome,
		ClaudeConfigDir: claudeDir,
		EmptyMCPPath:    emptyMCPPath,
		GitConfigPath:   gitConfigPath,
		Instructions:    initAuthReminder,
	}, nil
}

func ensureStrictDir(dir string) error {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("create isolated directory %q: %w", dir, err)
	}
	if err := os.Chmod(dir, 0o700); err != nil {
		return fmt.Errorf("chmod isolated directory %q: %w", dir, err)
	}
	return nil
}

func writeStrictFile(path string, content []byte) error {
	if err := os.WriteFile(path, content, 0o600); err != nil {
		return fmt.Errorf("write isolated file %q: %w", path, err)
	}
	if err := os.Chmod(path, 0o600); err != nil {
		return fmt.Errorf("chmod isolated file %q: %w", path, err)
	}
	return nil
}

func runIsolatedClaudeLogin(ctx context.Context, opts InitAuthOptions, isoHome, claudeDir string) error {
	bin := strings.TrimSpace(opts.ClaudeBinary)
	if bin == "" {
		bin = defaultClaudeBinary
	}

	// Direct execve of the configured claude CLI with fixed "login" subcommand (no shell).
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, bin, "login")
	cmd.Env = isolatedLoginEnv(os.Environ(), isoHome, claudeDir)
	cmd.Stdin = opts.Stdin
	if cmd.Stdin == nil {
		cmd.Stdin = os.Stdin
	}
	cmd.Stdout = opts.Stdout
	if cmd.Stdout == nil {
		cmd.Stdout = os.Stdout
	}
	cmd.Stderr = opts.Stderr
	if cmd.Stderr == nil {
		cmd.Stderr = os.Stderr
	}
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("run %s login: %w", bin, err)
	}
	return nil
}

func isolatedLoginEnv(base []string, isoHome, claudeDir string) []string {
	env := make([]string, 0, len(base)+2)
	for _, kv := range base {
		if strings.HasPrefix(kv, "HOME=") || strings.HasPrefix(kv, "CLAUDE_CONFIG_DIR=") {
			continue
		}
		env = append(env, kv)
	}
	return append(env,
		"HOME="+isoHome,
		"CLAUDE_CONFIG_DIR="+claudeDir,
	)
}
