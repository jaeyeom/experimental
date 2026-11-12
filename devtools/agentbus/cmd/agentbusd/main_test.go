package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func TestLeaseTimingTakeoverMode(t *testing.T) {
	for _, tt := range []struct {
		text, want, warn string
	}{
		{"", "log", ""},
		{"lease_takeover = \"log\"\n", "log", ""},
		{"lease_takeover = \"on\"\n", "on", ""},
		{"lease_takeover = \"maybe\"\n", "log", `agentbusd: config: lease_takeover = "maybe" is not "log" or "on"; using "log"`},
	} {
		cfg, err := config.Parse(tt.text)
		if err != nil {
			t.Fatalf("Parse(%q) = %v; the daemon must still start", tt.text, err)
		}
		var stderr bytes.Buffer
		tm := leaseTiming(cfg, &stderr)
		if string(tm.Takeover) != tt.want {
			t.Errorf("%q: takeover = %q, want %q", tt.text, tm.Takeover, tt.want)
		}
		if !strings.Contains(stderr.String(), "agentbusd: lease_takeover="+tt.want+"\n") {
			t.Errorf("%q: stderr = %q, want the effective mode", tt.text, stderr.String())
		}
		if tt.warn != "" && !strings.Contains(stderr.String(), tt.warn) {
			t.Errorf("%q: stderr = %q, want warning %q", tt.text, stderr.String(), tt.warn)
		}
		if tt.warn == "" && strings.Contains(stderr.String(), "config:") {
			t.Errorf("%q: unexpected warning in %q", tt.text, stderr.String())
		}
	}
}

func TestHibernatePolicyIsOffByDefault(t *testing.T) {
	var stderr bytes.Buffer
	if hibernatePolicy(config.Config{}, &stderr) != nil {
		t.Fatal("no hibernate_policy key made a policy")
	}
	if !strings.Contains(stderr.String(), "agentbusd: hibernate_policy=off\n") {
		t.Fatalf("stderr = %q, want the effective mode", stderr.String())
	}
}

func TestHibernatePolicyFromConfig(t *testing.T) {
	cfg, err := config.Parse(`hibernate_policy = "dry-run"
hibernate_below = "1GiB"
hibernate_target = "2GiB"
hibernate_exempt = ["ops/*"]
`)
	if err != nil {
		t.Fatal(err)
	}
	var stderr bytes.Buffer
	build := hibernatePolicy(cfg, &stderr)
	if build == nil {
		t.Fatal("dry-run made no policy")
	}
	if !strings.Contains(stderr.String(), "agentbusd: hibernate_policy=dry-run below=1.0 GiB target=2.0 GiB min_idle=30m0s interval=1m0s\n") {
		t.Fatalf("stderr = %q", stderr.String())
	}
	pol := build(nil)
	if pol.Mode != hibernate.PolicyDryRun || pol.Below != 1<<30 || pol.Target != 2<<30 || pol.MinIdle != 30*time.Minute || pol.Memory == nil {
		t.Fatalf("policy = %+v", pol)
	}
	h, ok := pol.Hibernator.(*hibernate.Hibernator)
	if !ok || h.Machine != "" || len(h.Exempt) != 1 || h.Exempt[0] != "ops/*" || h.Procs == nil || h.Panes == nil {
		t.Fatalf("hibernator = %+v, want the local one with hibernate_exempt", pol.Hibernator)
	}
}

func TestRunRejectsBadRetention(t *testing.T) {
	var stderr bytes.Buffer
	code := run(nil, []string{
		"XDG_RUNTIME_DIR=/tmp",
		"HOME=/Users/me",
		"AGENTBUS_LOG_RETENTION_DAYS=0",
	}, io.Discard, &stderr)
	if code == 0 {
		t.Fatal("run exited 0")
	}
	if !strings.Contains(stderr.String(), "AGENTBUS_LOG_RETENTION_DAYS") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestHelp(t *testing.T) {
	for _, arg := range []string{"--help", "-h"} {
		var stdout, stderr bytes.Buffer
		code := run([]string{arg}, []string{"HOME=/Users/me"}, &stdout, &stderr)
		if code != 0 || stderr.Len() != 0 {
			t.Fatalf("%s: code %d, stderr %q", arg, code, stderr.String())
		}
		for _, want := range []string{"Usage: agentbusd", "AGENTBUS_LOG_RETENTION_DAYS", "AGENTBUS_INLINE_BYTES", "XDG_RUNTIME_DIR", `lease_takeover = "log"`} {
			if !strings.Contains(stdout.String(), want) {
				t.Errorf("%s: stdout is missing %q", arg, want)
			}
		}
	}
}

func TestRunRejectsArguments(t *testing.T) {
	var stderr bytes.Buffer
	code := run([]string{"serve"}, []string{"HOME=/Users/me"}, io.Discard, &stderr)
	if code != 2 || !strings.Contains(stderr.String(), "agentbusd --help") {
		t.Fatalf("code %d, stderr %q", code, stderr.String())
	}
}

func TestRunExitsZeroWhenDaemonAnswers(t *testing.T) {
	env, dirs := daemonEnv(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errc := make(chan error, 1)
	go func() { errc <- daemon.Serve(ctx, dirs, daemon.Config{}) }()
	deadline := time.Now().Add(2 * time.Second)
	for {
		connErr := pingDaemon(dirs.SocketPath)
		if connErr == nil {
			break
		}
		if time.Now().After(deadline) {
			cancel()
			t.Fatalf("daemon did not listen: %v", connErr)
		}
		time.Sleep(10 * time.Millisecond)
	}
	t.Cleanup(func() {
		cancel()
		select {
		case err := <-errc:
			if err != nil {
				t.Error(err)
			}
		case <-time.After(2 * time.Second):
			t.Error("daemon did not exit")
		}
	})
	var stderr bytes.Buffer
	code := run(nil, env, io.Discard, &stderr)
	if code != 0 || !strings.Contains(stderr.String(), "already running") {
		t.Fatalf("code %d, stderr %q", code, stderr.String())
	}
}

func daemonEnv(t *testing.T) ([]string, paths.Dirs) {
	t.Helper()
	runtime := t.TempDir()
	// macOS rejects a unix socket path longer than about 104 bytes.
	if len(runtime) > 40 {
		var err error
		runtime, err = os.MkdirTemp("/tmp", "abd")
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { _ = os.RemoveAll(runtime) })
	}
	env := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + t.TempDir(),
		"XDG_CONFIG_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	return env, dirs
}

func pingDaemon(socketPath string) error {
	conn, err := net.DialTimeout("unix", socketPath, 200*time.Millisecond)
	if err != nil {
		return fmt.Errorf("dial: %w", err)
	}
	defer conn.Close()
	_ = conn.SetDeadline(time.Now().Add(200 * time.Millisecond))
	if err := json.NewEncoder(conn).Encode(daemon.Request{Op: "ping"}); err != nil {
		return fmt.Errorf("encode ping: %w", err)
	}
	var resp daemon.Response
	if err := json.NewDecoder(conn).Decode(&resp); err != nil {
		return fmt.Errorf("decode ping: %w", err)
	}
	if !resp.OK {
		return errors.New("ping not ok")
	}
	return nil
}

func TestRunRefusesInHubMode(t *testing.T) {
	config := t.TempDir()
	if err := os.MkdirAll(filepath.Join(config, "agentbus"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(config, "agentbus", "config.toml"), []byte("hub = \"me@hub\"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	state := t.TempDir()
	for _, env := range [][]string{
		{"XDG_CONFIG_HOME=" + config},
		{"XDG_CONFIG_HOME=" + t.TempDir(), "AGENTBUS_HUB=me@hub"},
	} {
		var stderr bytes.Buffer
		env = append(env, "XDG_RUNTIME_DIR=/tmp", "HOME=/Users/me", "XDG_STATE_HOME="+state)
		code := run(nil, env, io.Discard, &stderr)
		if code != 1 || !strings.Contains(stderr.String(), "agentbusd: this machine uses the hub me@hub; run the daemon there") {
			t.Fatalf("code %d, stderr %q", code, stderr.String())
		}
	}
	if entries, _ := os.ReadDir(state); len(entries) != 0 {
		t.Fatalf("hub mode created local state: %v", entries)
	}
}

func TestRunRejectsBadConfig(t *testing.T) {
	config := t.TempDir()
	if err := os.MkdirAll(filepath.Join(config, "agentbus"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(config, "agentbus", "config.toml"), []byte("machines = linux2\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var stderr bytes.Buffer
	code := run(nil, []string{
		"XDG_RUNTIME_DIR=/tmp",
		"HOME=/Users/me",
		"XDG_STATE_HOME=" + t.TempDir(),
		"XDG_CONFIG_HOME=" + config,
	}, io.Discard, &stderr)
	if code != 1 || !strings.Contains(stderr.String(), "machines must be a list of strings") {
		t.Fatalf("code %d, stderr %q", code, stderr.String())
	}
}
