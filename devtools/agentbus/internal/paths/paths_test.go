package paths

import (
	"strings"
	"testing"
)

func TestFromEnv(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		env     []string
		want    Dirs
		wantErr string
	}{
		{
			name: "xdg state home",
			env:  []string{"XDG_RUNTIME_DIR=/run/user/1", "XDG_STATE_HOME=/var/state"},
			want: Dirs{SocketPath: "/run/user/1/agentbus/daemon.sock", HubSocketPath: "/run/user/1/agentbus/hub.sock", TunnelSocketPath: "/run/user/1/agentbus/tunnel.sock", StateDir: "/var/state/agentbus"},
		},
		{
			name: "home default",
			env:  []string{"XDG_RUNTIME_DIR=/run/user/1", "HOME=/Users/me"},
			want: Dirs{SocketPath: "/run/user/1/agentbus/daemon.sock", HubSocketPath: "/run/user/1/agentbus/hub.sock", TunnelSocketPath: "/run/user/1/agentbus/tunnel.sock", StateDir: "/Users/me/.local/state/agentbus"},
		},
		{
			name:    "missing runtime",
			env:     []string{"HOME=/Users/me"},
			wantErr: "XDG_RUNTIME_DIR",
		},
		{
			name:    "missing home",
			env:     []string{"XDG_RUNTIME_DIR=/run/user/1"},
			wantErr: "HOME",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := FromEnv(tc.env)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("FromEnv error = %v, want mention of %s", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("FromEnv: %v", err)
			}
			if got != tc.want {
				t.Fatalf("FromEnv = %+v, want %+v", got, tc.want)
			}
		})
	}
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
