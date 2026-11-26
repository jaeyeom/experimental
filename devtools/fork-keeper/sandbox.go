package forkkeeper

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

const (
	sandboxAuto     = "auto"
	sandboxSeatbelt = "seatbelt"
	sandboxBwrap    = "bwrap"
	sandboxNone     = "none"
)

// DefaultSandboxMode returns the agent confinement mode from
// FORK_KEEPER_SANDBOX or "auto".
func DefaultSandboxMode() string {
	if m := os.Getenv("FORK_KEEPER_SANDBOX"); m != "" {
		return m
	}
	return sandboxAuto
}

func wrapAgentSandbox(mode, homeDir, gitDir string, args []string) ([]string, error) {
	return wrapAgentSandboxWith(mode, runtime.GOOS, exec.LookPath, homeDir, gitDir, args)
}

func wrapAgentSandboxWith(mode, goos string, lookPath func(string) (string, error), homeDir, gitDir string, args []string) ([]string, error) {
	resolved, err := resolveSandboxMode(mode, goos, lookPath)
	if err != nil {
		return nil, err
	}
	switch resolved {
	case sandboxNone:
		return args, nil
	case sandboxSeatbelt:
		profile := buildSeatbeltProfile(homeDir, gitDir)
		wrapped := make([]string, 0, 4+len(args))
		wrapped = append(wrapped, "sandbox-exec", "-p", profile, "--")
		wrapped = append(wrapped, args...)
		return wrapped, nil
	case sandboxBwrap:
		bwrapArgs := buildBwrapArgs(homeDir, gitDir, args)
		return append([]string{"bwrap"}, bwrapArgs...), nil
	default:
		return nil, fmt.Errorf("unknown sandbox mode %q", mode)
	}
}

func resolveSandboxMode(mode, goos string, lookPath func(string) (string, error)) (string, error) {
	switch mode {
	case "", sandboxAuto:
		switch goos {
		case "darwin":
			if _, err := lookPath("sandbox-exec"); err == nil {
				return sandboxSeatbelt, nil
			}
			return "", fmt.Errorf("sandbox-exec not found on PATH; set FORK_KEEPER_SANDBOX=none to opt out")
		case "linux":
			if _, err := lookPath("bwrap"); err == nil {
				return sandboxBwrap, nil
			}
			return "", fmt.Errorf("bwrap not found on PATH; install bubblewrap or set FORK_KEEPER_SANDBOX=none to opt out")
		default:
			return "", fmt.Errorf("no default agent sandbox for %s; set FORK_KEEPER_SANDBOX=none to opt out", goos)
		}
	case sandboxSeatbelt:
		if _, err := lookPath("sandbox-exec"); err != nil {
			return "", fmt.Errorf("sandbox-exec is required for seatbelt sandbox: %w", err)
		}
		return sandboxSeatbelt, nil
	case sandboxBwrap:
		if _, err := lookPath("bwrap"); err != nil {
			return "", fmt.Errorf("bwrap is required for bwrap sandbox: %w", err)
		}
		return sandboxBwrap, nil
	case sandboxNone:
		return sandboxNone, nil
	default:
		return "", fmt.Errorf("unknown sandbox mode %q (want auto, seatbelt, bwrap, or none)", mode)
	}
}

func credentialDirs(homeDir, gitDir string) []string {
	var dirs []string
	if homeDir != "" {
		dirs = append(dirs,
			filepath.Join(homeDir, ".ssh"),
			filepath.Join(homeDir, ".config", "gh"),
			filepath.Join(homeDir, ".gnupg"),
			filepath.Join(homeDir, ".aws"),
		)
	}
	if gitDir != "" {
		dirs = append(dirs, gitDir)
	}
	return dirs
}

func credentialFiles(homeDir string) []string {
	if homeDir == "" {
		return nil
	}
	return []string{
		filepath.Join(homeDir, ".netrc"),
		filepath.Join(homeDir, ".git-credentials"),
		filepath.Join(homeDir, ".config", "git", "credentials"),
	}
}

func buildSeatbeltProfile(homeDir, gitDir string) string {
	var b strings.Builder
	b.WriteString("(version 1)\n(allow default)\n(deny file-read* file-write*\n")
	for _, d := range credentialDirs(homeDir, gitDir) {
		for _, p := range canonicalPathVariants(d) {
			fmt.Fprintf(&b, "  (subpath %s)\n", quoteSBPL(p))
		}
	}
	for _, f := range credentialFiles(homeDir) {
		for _, p := range canonicalPathVariants(f) {
			fmt.Fprintf(&b, "  (literal %s)\n", quoteSBPL(p))
		}
	}
	b.WriteString(")\n")
	return b.String()
}

func buildBwrapArgs(homeDir, gitDir string, agentArgs []string) []string {
	args := []string{
		"--die-with-parent",
		"--bind", "/", "/",
		"--dev", "/dev",
		"--proc", "/proc",
	}
	for _, d := range credentialDirs(homeDir, gitDir) {
		if info, err := os.Stat(d); err == nil && info.IsDir() {
			args = append(args, "--tmpfs", d)
		}
	}
	for _, f := range credentialFiles(homeDir) {
		if info, err := os.Stat(f); err == nil && !info.IsDir() {
			args = append(args, "--ro-bind", "/dev/null", f)
		}
	}
	args = append(args, "--")
	args = append(args, agentArgs...)
	return args
}

// canonicalPathVariants returns the cleaned path plus its symlink-resolved
// counterpart when they differ (e.g. /var vs /private/var on macOS).
func canonicalPathVariants(p string) []string {
	clean := filepath.Clean(p)
	resolved, err := filepath.EvalSymlinks(clean)
	if err != nil || resolved == clean {
		return []string{clean}
	}
	return []string{clean, resolved}
}

func quoteSBPL(s string) string {
	escaped := strings.ReplaceAll(s, `\`, `\\`)
	escaped = strings.ReplaceAll(escaped, `"`, `\"`)
	return `"` + escaped + `"`
}
