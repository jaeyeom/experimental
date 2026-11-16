package main

import (
	"context"
	"encoding/json"
	"errors"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

type healthJSON struct {
	Daemon    string   `json:"daemon"`
	Transport string   `json:"transport"`
	Problems  []string `json:"problems"`
}

func decodeHealth(t *testing.T, stdout string) healthJSON {
	t.Helper()
	var got healthJSON
	dec := json.NewDecoder(strings.NewReader(stdout))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&got); err != nil {
		t.Fatalf("stdout is not health JSON: %v\n%s", err, stdout)
	}
	if !strings.Contains(stdout, `"problems":[`) {
		t.Fatalf("problems must be an array: %s", stdout)
	}
	return got
}

func TestHealthOK(t *testing.T) {
	env, _ := heldWorld(t, nil)
	code, stdout, stderr := runCmd([]string{"health", "--json"}, env(""))
	if code != 0 {
		t.Fatalf("health --json = %d, stderr %q", code, stderr)
	}
	got := decodeHealth(t, stdout)
	if got.Daemon != "up" || got.Transport != "ok" || len(got.Problems) != 0 {
		t.Fatalf("health = %+v", got)
	}
	code, stdout, stderr = runCmd([]string{"health"}, env(""))
	if code != 0 || stdout != "ok\n" {
		t.Fatalf("health = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
}

func TestHealthTransportDown(t *testing.T) {
	reason := "herdr agent list: no herdr server is running"
	env, _ := heldWorld(t, errors.New(reason))
	want := "herdr transport unreachable: " + reason
	code, stdout, stderr := runCmd([]string{"health", "--json"}, env(""))
	if code != 4 {
		t.Fatalf("health --json = %d, stderr %q", code, stderr)
	}
	got := decodeHealth(t, stdout)
	if got.Daemon != "up" || got.Transport != "down" || !slices.Equal(got.Problems, []string{want}) {
		t.Fatalf("health = %+v", got)
	}
	code, stdout, _ = runCmd([]string{"health"}, env(""))
	if code != 4 || stdout != want+"\n" {
		t.Fatalf("health = %d, stdout %q", code, stdout)
	}
}

func TestHealthDaemonUnreachable(t *testing.T) {
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	code, stdout, stderr := runCmd([]string{"health", "--json"}, base)
	if code != 1 {
		t.Fatalf("health --json = %d, stderr %q", code, stderr)
	}
	got := decodeHealth(t, stdout)
	if got.Daemon != "down" || got.Transport != "unknown" || len(got.Problems) != 1 || !strings.HasPrefix(got.Problems[0], "daemon unreachable: ") {
		t.Fatalf("health = %+v", got)
	}
	code, stdout, _ = runCmd([]string{"health"}, base)
	if code != 1 || !strings.HasPrefix(stdout, "daemon unreachable: ") {
		t.Fatalf("health = %d, stdout %q", code, stdout)
	}
	if _, err := dialDaemon(context.Background(), dirs.SocketPath, daemon.Request{Op: "ping"}); err == nil {
		t.Fatal("health started a daemon")
	}
}

func TestHealthRejectsArguments(t *testing.T) {
	if code, _, _ := runCmd([]string{"health", "extra"}, nil); code != 2 {
		t.Fatalf("health extra = %d", code)
	}
}

func TestHealthHubModeAsksTheHubOverTheTunnel(t *testing.T) {
	s := newTunnelSetup(t)
	s.startHubDaemon(t)
	code, stdout, stderr := s.run(t, s.env, "", "health", "--json")
	got := decodeHealth(t, stdout)
	if code != 0 || got.Daemon != "up" || got.Transport != "ok" {
		t.Fatalf("health = %d %+v, stderr %q", code, got, stderr)
	}
	if s.usedSSH() {
		t.Fatal("health used ssh with a live tunnel")
	}
}

func TestHealthHubModeWithoutATunnelRunsOnTheHub(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("agentbus.stdout", `{"daemon":"up","transport":"down","problems":["herdr transport unreachable: x"]}`+"\n")
	s.ssh.Write("agentbus.code", "4")
	code, stdout, stderr := s.run(t, s.env, "", "health", "--json")
	if code != 4 {
		t.Fatalf("health = %d, stderr %q", code, stderr)
	}
	if got := decodeHealth(t, stdout); got.Transport != "down" {
		t.Fatalf("health = %+v", got)
	}
	if got := s.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"health", "--json"}) {
		t.Fatalf("hub args = %q", got)
	}
}

func TestHealthHubUnreachable(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("down", "")
	s.ssh.Unreachable()
	code, stdout, stderr := s.run(t, s.env, "", "health", "--json")
	if code != 1 {
		t.Fatalf("health = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	got := decodeHealth(t, stdout)
	if got.Daemon != "down" || got.Transport != "unknown" || len(got.Problems) != 1 || !strings.Contains(got.Problems[0], "me@hub") {
		t.Fatalf("health = %+v", got)
	}
}
