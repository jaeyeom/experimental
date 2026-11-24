package remotecall

import (
	"context"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

const (
	gpgconfTimeout   = 2 * time.Second
	agentDialTimeout = 500 * time.Millisecond
)

// AuthLockedCause returns the CauseAuthLocked message, naming socket
// when non-empty.
func AuthLockedCause(socket string) string {
	if socket == "" {
		return CauseAuthLocked
	}
	return fmt.Sprintf("authentication locked via %s (unlock it, or set ssh_agent_socket)", socket)
}

// IsAuthLocked reports whether ue represents an authentication-locked
// failure.
func IsAuthLocked(ue *UnreachableError) bool {
	return ue != nil && (ue.Cause == CauseAuthLocked || strings.HasPrefix(ue.Cause, "authentication locked"))
}

// AgentSocketOptions configures how ResolveAgentSockets discovers live
// SSH agent sockets. Nil or zero fields fall back to OS defaults.
type AgentSocketOptions struct {
	Configured string
	Env        []string
	GPGConf    func(ctx context.Context) (string, error)
	IsLive     func(path string) bool
}

// ResolveAgentSockets returns the ordered, deduplicated list of live
// SSH agent sockets from:
//  1. opts.Configured
//  2. ~/.ssh/agent.sock
//  3. gpgconf --list-dirs agent-ssh-socket
//  4. inherited SSH_AUTH_SOCK
func ResolveAgentSockets(ctx context.Context, opts AgentSocketOptions) []string {
	env := opts.Env
	if env == nil {
		env = os.Environ()
	}
	isLive := opts.IsLive
	if isLive == nil {
		isLive = isLiveUnixSocket
	}
	gpgConf := opts.GPGConf
	if gpgConf == nil {
		gpgConf = defaultGPGAgentSocket
	}

	home := envGet(env, "HOME")
	var candidates []string
	if opts.Configured != "" {
		candidates = append(candidates, expandTilde(home, opts.Configured))
	}
	if home != "" {
		candidates = append(candidates, filepath.Join(home, ".ssh", "agent.sock"))
	}
	if gpgSock, err := gpgConf(ctx); err == nil && gpgSock != "" {
		candidates = append(candidates, gpgSock)
	}
	if inherited := envGet(env, "SSH_AUTH_SOCK"); inherited != "" {
		candidates = append(candidates, inherited)
	}

	seen := make(map[string]bool, len(candidates))
	var out []string
	for _, c := range candidates {
		if c == "" || !isLive(c) {
			continue
		}
		key := canonicalSocketPath(c)
		if seen[key] {
			continue
		}
		seen[key] = true
		out = append(out, c)
	}
	return out
}

func defaultGPGAgentSocket(ctx context.Context) (string, error) {
	cctx, cancel := context.WithTimeout(ctx, gpgconfTimeout)
	defer cancel()
	out, err := exec.CommandContext(cctx, "gpgconf", "--list-dirs", "agent-ssh-socket").Output()
	if err != nil {
		return "", fmt.Errorf("gpgconf: %w", err)
	}
	return strings.TrimSpace(string(out)), nil
}

func isLiveUnixSocket(path string) bool {
	info, err := os.Stat(path)
	if err != nil || info.Mode()&os.ModeSocket == 0 {
		return false
	}
	var d net.Dialer
	ctx, cancel := context.WithTimeout(context.Background(), agentDialTimeout)
	defer cancel()
	conn, err := d.DialContext(ctx, "unix", path)
	if err != nil {
		return false
	}
	_ = conn.Close()
	return true
}

func canonicalSocketPath(path string) string {
	if resolved, err := filepath.EvalSymlinks(path); err == nil {
		return filepath.Clean(resolved)
	}
	return filepath.Clean(path)
}

func expandTilde(home, path string) string {
	if home != "" && strings.HasPrefix(path, "~/") {
		return filepath.Join(home, path[2:])
	}
	return path
}

func envGet(env []string, key string) string {
	for _, entry := range env {
		if k, v, ok := strings.Cut(entry, "="); ok && k == key {
			return v
		}
	}
	return ""
}
