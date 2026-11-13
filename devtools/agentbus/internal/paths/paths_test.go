package paths

import (
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestFromEnv(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		goos    string
		env     []string
		want    Dirs
		wantErr string
	}{
		{
			name: "xdg state home",
			goos: "linux",
			env:  []string{"XDG_RUNTIME_DIR=/run/user/1", "XDG_STATE_HOME=/var/state"},
			want: Dirs{SocketPath: "/run/user/1/agentbus/daemon.sock", HubSocketPath: "/run/user/1/agentbus/hub.sock", TunnelSocketPath: "/run/user/1/agentbus/tunnel.sock", StateDir: "/var/state/agentbus"},
		},
		{
			name: "home default",
			goos: "darwin",
			env:  []string{"XDG_RUNTIME_DIR=/run/user/1", "HOME=/Users/me"},
			want: Dirs{SocketPath: "/run/user/1/agentbus/daemon.sock", HubSocketPath: "/run/user/1/agentbus/hub.sock", TunnelSocketPath: "/run/user/1/agentbus/tunnel.sock", StateDir: "/Users/me/.local/state/agentbus"},
		},
		{
			name:    "missing runtime",
			goos:    "linux",
			env:     []string{"HOME=/Users/me"},
			wantErr: "XDG_RUNTIME_DIR is not set",
		},
		{
			name:    "missing home",
			goos:    "linux",
			env:     []string{"XDG_RUNTIME_DIR=/run/user/1"},
			wantErr: "HOME",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := fromEnv(tc.env, tc.goos)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("fromEnv error = %v, want mention of %s", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("fromEnv: %v", err)
			}
			if got != tc.want {
				t.Fatalf("fromEnv = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestDarwinDefaultRuntimeDir(t *testing.T) {
	t.Parallel()
	home := shortHome(t)
	state := filepath.Join(home, "state")
	env := []string{"HOME=" + home, "XDG_STATE_HOME=" + state}
	runtimeDir := filepath.Join(home, "Library", "Caches", "agentbus", "run")
	want := Dirs{
		SocketPath:       filepath.Join(runtimeDir, "agentbus", "daemon.sock"),
		HubSocketPath:    filepath.Join(runtimeDir, "agentbus", "hub.sock"),
		TunnelSocketPath: filepath.Join(runtimeDir, "agentbus", "tunnel.sock"),
		StateDir:         filepath.Join(state, "agentbus"),
	}
	got, err := fromEnv(env, "darwin")
	if err != nil {
		t.Fatalf("fromEnv: %v", err)
	}
	if got != want {
		t.Fatalf("fromEnv = %+v, want %+v", got, want)
	}
	info, err := os.Stat(runtimeDir)
	if err != nil {
		t.Fatalf("stat runtime dir: %v", err)
	}
	if info.Mode().Perm() != 0o700 {
		t.Fatalf("runtime dir mode = %o, want 0700", info.Mode().Perm())
	}
}

func TestDarwinHonorsRuntimeDir(t *testing.T) {
	t.Parallel()
	home := shortHome(t)
	xdg := filepath.Join(home, "xdg")
	env := []string{"HOME=" + home, "XDG_RUNTIME_DIR=" + xdg, "XDG_STATE_HOME=" + filepath.Join(home, "state")}
	got, err := fromEnv(env, "darwin")
	if err != nil {
		t.Fatalf("fromEnv: %v", err)
	}
	if got.SocketPath != filepath.Join(xdg, "agentbus", "daemon.sock") {
		t.Fatalf("SocketPath = %s", got.SocketPath)
	}
	if _, err := os.Stat(filepath.Join(home, "Library")); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("default runtime dir exists: %v", err)
	}
}

func TestDarwinSocketPathTooLong(t *testing.T) {
	t.Parallel()
	home := "/" + strings.Repeat("h", 90)
	_, err := fromEnv([]string{"HOME=" + home, "XDG_STATE_HOME=/var/state"}, "darwin")
	if err == nil || !strings.Contains(err.Error(), home) || !strings.Contains(err.Error(), "104") || !strings.Contains(err.Error(), "too long") {
		t.Fatalf("fromEnv error = %v", err)
	}
	if _, statErr := os.Stat(filepath.Join(home, "Library")); !errors.Is(statErr, os.ErrNotExist) {
		t.Fatalf("created a directory for a too-long socket: %v", statErr)
	}
}

func TestFromEnvUsesTheHostOS(t *testing.T) {
	t.Parallel()
	home := shortHome(t)
	env := []string{"HOME=" + home, "XDG_STATE_HOME=" + filepath.Join(home, "state")}
	got, err := FromEnv(env)
	if runtime.GOOS == "darwin" {
		want := filepath.Join(home, "Library", "Caches", "agentbus", "run", "agentbus", "daemon.sock")
		if err != nil || got.SocketPath != want {
			t.Fatalf("FromEnv = %+v, %v", got, err)
		}
		return
	}
	if err == nil || !strings.Contains(err.Error(), "XDG_RUNTIME_DIR is not set") {
		t.Fatalf("FromEnv error = %v", err)
	}
}

func shortHome(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "ab")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}

func TestLeaseCacheDirIsBesideTheSocket(t *testing.T) {
	t.Parallel()
	d := Dirs{SocketPath: "/run/user/1/agentbus/daemon.sock"}
	if got, want := d.LeaseCacheDir(), "/run/user/1/agentbus/lease-cache"; got != want {
		t.Fatalf("LeaseCacheDir = %q, want %q", got, want)
	}
	if got := (Dirs{}).LeaseCacheDir(); got != "" {
		t.Fatalf("LeaseCacheDir without a socket = %q, want empty", got)
	}
}

func TestPaneCacheDirIsBesideTheSocket(t *testing.T) {
	t.Parallel()
	d := Dirs{SocketPath: "/run/user/1/agentbus/daemon.sock"}
	if got, want := d.PaneCacheDir(), "/run/user/1/agentbus/panes"; got != want {
		t.Fatalf("PaneCacheDir = %q, want %q", got, want)
	}
	if got := (Dirs{}).PaneCacheDir(); got != "" {
		t.Fatalf("PaneCacheDir without a socket = %q, want empty", got)
	}
}
