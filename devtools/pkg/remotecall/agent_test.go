package remotecall

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func shortTempDir(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "rc-*")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}

func listenUnix(t *testing.T, path string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = ln.Close() })
}

func TestResolveAgentSocketsOrder(t *testing.T) {
	dir := shortTempDir(t)
	configured := filepath.Join(dir, "configured.sock")
	linkTarget := filepath.Join(dir, "real-agent.sock")
	home := filepath.Join(dir, "home")
	dotSSH := filepath.Join(home, ".ssh", "agent.sock")
	gpgSock := filepath.Join(dir, "gpg.sock")
	inherited := filepath.Join(dir, "inherited.sock")

	listenUnix(t, configured)
	listenUnix(t, linkTarget)
	if err := os.MkdirAll(filepath.Dir(dotSSH), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(linkTarget, dotSSH); err != nil {
		t.Fatal(err)
	}
	listenUnix(t, gpgSock)
	listenUnix(t, inherited)

	env := []string{"HOME=" + home, "SSH_AUTH_SOCK=" + inherited}
	gpg := func(context.Context) (string, error) { return gpgSock, nil }

	got := ResolveAgentSockets(context.Background(), AgentSocketOptions{
		Configured: configured,
		Env:        env,
		GPGConf:    gpg,
	})
	want := []string{configured, dotSSH, gpgSock, inherited}
	if !slices.Equal(got, want) {
		t.Fatalf("with configured = %v, want %v", got, want)
	}

	// Without configured: ~/.ssh/agent.sock -> gpg-agent -> inherited.
	got = ResolveAgentSockets(context.Background(), AgentSocketOptions{
		Env:     env,
		GPGConf: gpg,
	})
	want = []string{dotSSH, gpgSock, inherited}
	if !slices.Equal(got, want) {
		t.Fatalf("without configured = %v, want %v", got, want)
	}

	// Dangling ~/.ssh/agent.sock is skipped; gpg-agent wins over inherited.
	if err := os.Remove(linkTarget); err != nil {
		t.Fatal(err)
	}
	got = ResolveAgentSockets(context.Background(), AgentSocketOptions{
		Env:     env,
		GPGConf: gpg,
	})
	want = []string{gpgSock, inherited}
	if !slices.Equal(got, want) {
		t.Fatalf("with dangling agent.sock = %v, want %v", got, want)
	}

	// gpgconf error falls back to inherited.
	got = ResolveAgentSockets(context.Background(), AgentSocketOptions{
		Env:     env,
		GPGConf: func(context.Context) (string, error) { return "", errors.New("no gpgconf") },
	})
	want = []string{inherited}
	if !slices.Equal(got, want) {
		t.Fatalf("without gpgconf = %v, want %v", got, want)
	}
}

func TestResolveAgentSocketsSkipsDeadAndDeduplicates(t *testing.T) {
	dir := shortTempDir(t)
	home := filepath.Join(dir, "home")
	dotSSH := filepath.Join(home, ".ssh", "agent.sock")
	gpgSock := filepath.Join(dir, "gpg.sock")
	deadSock := filepath.Join(dir, "dead.sock")

	listenUnix(t, gpgSock)
	// Point ~/.ssh/agent.sock to gpgSock so both resolve to the same socket.
	if err := os.MkdirAll(filepath.Dir(dotSSH), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(gpgSock, dotSSH); err != nil {
		t.Fatal(err)
	}
	// Create a stale (closed) unix socket for inherited SSH_AUTH_SOCK.
	ln, err := net.Listen("unix", deadSock)
	if err != nil {
		t.Fatal(err)
	}
	_ = ln.Close()

	got := ResolveAgentSockets(context.Background(), AgentSocketOptions{
		Env:     []string{"HOME=" + home, "SSH_AUTH_SOCK=" + deadSock},
		GPGConf: func(context.Context) (string, error) { return gpgSock, nil },
	})
	want := []string{dotSSH}
	if !slices.Equal(got, want) {
		t.Fatalf("got %v, want %v", got, want)
	}
}

func TestAuthLockedCauseAndIsAuthLocked(t *testing.T) {
	if got := AuthLockedCause(""); got != CauseAuthLocked {
		t.Fatalf("AuthLockedCause(\"\") = %q, want %q", got, CauseAuthLocked)
	}
	sock := "/run/user/1000/gcr/ssh"
	got := AuthLockedCause(sock)
	want := "authentication locked via /run/user/1000/gcr/ssh (unlock it, or set ssh_agent_socket)"
	if got != want {
		t.Fatalf("AuthLockedCause(%q) = %q, want %q", sock, got, want)
	}
	if !IsAuthLocked(&UnreachableError{Machine: "m2", Cause: CauseAuthLocked}) {
		t.Fatal("IsAuthLocked(CauseAuthLocked) = false")
	}
	if !IsAuthLocked(&UnreachableError{Machine: "m2", Cause: got}) {
		t.Fatal("IsAuthLocked(with socket) = false")
	}
	if IsAuthLocked(&UnreachableError{Machine: "m2", Cause: CauseHostUnreachable}) {
		t.Fatal("IsAuthLocked(CauseHostUnreachable) = true")
	}
	if IsAuthLocked(nil) {
		t.Fatal("IsAuthLocked(nil) = true")
	}
}
