package main

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestTunnelUnitTexts(t *testing.T) {
	vars := [][2]string{{"HOME", "/Users/me"}}
	spec := tunnelSpec{
		ssh:    "/usr/bin/ssh",
		target: "me@hub",
		local:  "/rt/agentbus/tunnel.sock",
		remote: "/run/user/1000/agentbus/hub.sock",
	}
	plist, err := tunnelBody("darwin", spec, vars)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"<string>" + tunnelLaunchdLabel + "</string>",
		"<string>/bin/sh</string>",
		"<string>/rt/agentbus</string>",
		"<string>/usr/bin/ssh</string>",
		"<string>-N</string>",
		"<string>BatchMode=yes</string>",
		"<string>ExitOnForwardFailure=yes</string>",
		"<string>StreamLocalBindUnlink=yes</string>",
		"<string>ServerAliveInterval=30</string>",
		"<string>-L</string>",
		"<string>/rt/agentbus/tunnel.sock:/run/user/1000/agentbus/hub.sock</string>",
		"<string>--</string>\n    <string>me@hub</string>",
		"<key>KeepAlive</key>",
		"<string>/Users/me</string>",
	} {
		if !strings.Contains(plist, want) {
			t.Fatalf("plist missing %s\n%s", want, plist)
		}
	}

	unit, err := tunnelBody("linux", spec, vars)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"Description=agentbus tunnel to the hub daemon\n",
		"ExecStartPre=/bin/mkdir -p /rt/agentbus\n",
		`ExecStart=/usr/bin/ssh -N -o "BatchMode=yes" -o "ExitOnForwardFailure=yes" -o "StreamLocalBindUnlink=yes" -o "ServerAliveInterval=30" -L /rt/agentbus/tunnel.sock:/run/user/1000/agentbus/hub.sock -- me@hub` + "\n",
		"Restart=always\n",
		"RestartSec=5\n",
		"Environment=HOME=/Users/me\n",
		"WantedBy=default.target\n",
	} {
		if !strings.Contains(unit, want) {
			t.Fatalf("unit missing %s\n%s", want, unit)
		}
	}
	path, err := tunnelPath("linux", "/home/me", []string{"XDG_CONFIG_HOME=/cfg"})
	if err != nil || path != filepath.Join("/cfg", "systemd", "user", tunnelUnitName) {
		t.Fatalf("systemd path = %s, %v", path, err)
	}
	path, err = tunnelPath("darwin", "/Users/me", nil)
	if err != nil || path != filepath.Join("/Users/me", "Library", "LaunchAgents", tunnelLaunchdLabel+".plist") {
		t.Fatalf("launchd path = %s, %v", path, err)
	}
	if _, err := tunnelBody("plan9", spec, nil); err == nil {
		t.Fatal("plan9 tunnel body succeeded")
	}
}

func TestHubInstallNeedsHubSocket(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	env := append(append([]string{}, h.env...), "AGENTBUS_INSTALL_NO_LOAD=1")
	code, _, stderr := h.runEnv(t, env, "", "install")
	if code == 0 || !strings.Contains(stderr, `hub_socket = "`) {
		t.Fatalf("install = %d, stderr %q", code, stderr)
	}
	if h.ssh.Read("argv") != "" {
		t.Fatal("install ran ssh")
	}
	home := strings.TrimPrefix(env[0], "HOME=")
	if path, err := tunnelPath(runtime.GOOS, home, env); err == nil {
		if _, err := os.Stat(path); !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("unit err = %v", err)
		}
	}
}

func TestHubInstallWritesTunnelUnitWithoutLoading(t *testing.T) {
	if runtime.GOOS != "darwin" && runtime.GOOS != "linux" {
		t.Skip("install supports launchd and systemd")
	}
	h := startHub(t, `hub = "me@hub"`, `hub_socket = "/run/user/1000/agentbus/hub.sock"`)
	env := append(append([]string{}, h.env...), "AGENTBUS_INSTALL_NO_LOAD=1")
	code, stdout, stderr := h.runEnv(t, env, "", "install")
	if code != 0 {
		t.Fatalf("install = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if h.ssh.Read("argv") != "" {
		t.Fatal("install ran ssh")
	}
	home := strings.TrimPrefix(env[0], "HOME=")
	path, err := tunnelPath(runtime.GOOS, home, env)
	if err != nil {
		t.Fatal(err)
	}
	if stdout != "wrote "+path+"\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	ssh, err := exec.LookPath("ssh")
	if err != nil {
		t.Fatal(err)
	}
	local := filepath.Join(h.runtime, "agentbus", "tunnel.sock")
	for _, want := range []string{ssh, local + ":/run/user/1000/agentbus/hub.sock", "me@hub"} {
		if !strings.Contains(string(data), want) {
			t.Fatalf("unit missing %s\n%s", want, data)
		}
	}
	if info, err := os.Stat(filepath.Dir(local)); err != nil || !info.IsDir() {
		t.Fatalf("tunnel socket dir: %v", err)
	}
}
