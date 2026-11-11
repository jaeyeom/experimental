package config

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

func TestParse(t *testing.T) {
	got, err := Parse(`# saved herdr machines to search and deliver to
machines = ["linux2", "m3"]  # labels or ids
remote_timeout = "3s"
`)
	if err != nil {
		t.Fatal(err)
	}
	want := Config{Machines: []string{"linux2", "m3"}, RemoteTimeout: 3 * time.Second}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Parse = %+v, want %+v", got, want)
	}
}

func TestParseHub(t *testing.T) {
	got, err := Parse(`hub = "me@hub"   # ssh target that runs agentbus
machine = "linux2"
hub_socket = "/run/user/1000/agentbus/hub.sock"  # on the hub
`)
	if err != nil {
		t.Fatal(err)
	}
	if got.Hub != "me@hub" || got.Machine != "linux2" || got.HubSocket != "/run/user/1000/agentbus/hub.sock" {
		t.Fatalf("Parse = %+v", got)
	}
}

func TestParseEmpty(t *testing.T) {
	got, err := Parse("\n# nothing\n")
	if err != nil || !reflect.DeepEqual(got, Config{}) {
		t.Fatalf("Parse = %+v, %v", got, err)
	}
	got, err = Parse("machines = []\n")
	if err != nil || len(got.Machines) != 0 {
		t.Fatalf("Parse empty list = %+v, %v", got, err)
	}
}

func TestParseErrors(t *testing.T) {
	for _, tt := range []struct{ in, want string }{
		{"colour = \"red\"\n", "line 1: unknown key colour"},
		{"machines = linux2\n", "line 1: machines must be a list of strings"},
		{"machines = [\"local\"]\n", "line 1: machines must not list local"},
		{"machines = [\"\"]\n", "line 1: machines must be a list of strings"},
		{"\nremote_timeout = \"soon\"\n", "line 2: remote_timeout must be a positive duration"},
		{"remote_timeout = 5\n", "line 1: remote_timeout must be a positive duration"},
		{"machines\n", "line 1: want key = value"},
		{"hub = \"\"\n", "line 1: hub must be a non-empty string"},
		{"hub = \"-oProxyCommand=x\"\n", "line 1: hub must not start with -"},
		{"machine = \"local\"\n", "line 1: machine must name this machine, not local"},
		{"machine = \"a/b\"\n", "line 1: machine must not contain /"},
		{"hub_socket = \"\"\n", "line 1: hub_socket must be a non-empty string"},
		{"hub_socket = \"run/hub.sock\"\n", "line 1: hub_socket must be an absolute path"},
		{"hub_socket = \"/run/a:b.sock\"\n", "line 1: hub_socket must not contain a colon"},
	} {
		_, err := Parse(tt.in)
		if err == nil || !strings.Contains(err.Error(), tt.want) {
			t.Errorf("Parse(%q) = %v, want %q", tt.in, err, tt.want)
		}
	}
}

func TestLoad(t *testing.T) {
	home := t.TempDir()
	env := []string{"HOME=" + home}
	got, err := Load(env)
	if err != nil || !reflect.DeepEqual(got, Config{}) {
		t.Fatalf("Load with no file = %+v, %v", got, err)
	}
	dir := filepath.Join(home, ".config", "agentbus")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.toml"), []byte("machines = [\"m2\"]\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	got, err = Load(env)
	if err != nil || !reflect.DeepEqual(got.Machines, []string{"m2"}) {
		t.Fatalf("Load = %+v, %v", got, err)
	}

	xdg := t.TempDir()
	path, err := Path([]string{"HOME=" + home, "XDG_CONFIG_HOME=" + xdg})
	if err != nil || path != filepath.Join(xdg, "agentbus", "config.toml") {
		t.Fatalf("Path = %q, %v", path, err)
	}
}
