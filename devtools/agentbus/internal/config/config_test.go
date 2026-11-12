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

func TestParseLeaseCheckouts(t *testing.T) {
	got, err := Parse(`lease_checkouts = ["/home/u/src/repo", "/home/u/src/other/"]  # guarded
`)
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"/home/u/src/repo", "/home/u/src/other"}
	if !reflect.DeepEqual(got.LeaseCheckouts, want) {
		t.Fatalf("LeaseCheckouts = %q, want %q", got.LeaseCheckouts, want)
	}
}

func TestParseLeaseIgnoreProcs(t *testing.T) {
	got, err := Parse(`lease_ignore_procs = ["zsh", "claude", "tmux"]  # adopt ignores these
`)
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"zsh", "claude", "tmux"}; !reflect.DeepEqual(got.LeaseIgnoreProcs, want) {
		t.Fatalf("LeaseIgnoreProcs = %q, want %q", got.LeaseIgnoreProcs, want)
	}
	if got, err := Parse("lease_ignore_procs = []\n"); err != nil || got.LeaseIgnoreProcs == nil || len(got.LeaseIgnoreProcs) != 0 {
		t.Fatalf("empty list = %#v, %v", got.LeaseIgnoreProcs, err)
	}
	if got, _ := Parse(""); got.LeaseIgnoreProcs != nil {
		t.Fatalf("unset = %#v, want nil (the default)", got.LeaseIgnoreProcs)
	}
}

func TestParseLeaseTimers(t *testing.T) {
	got, err := Parse(`lease_check_interval = "1m"   # how often
lease_checkin_repeat = "10m"
lease_idle_grace = "3m"
`)
	if err != nil {
		t.Fatal(err)
	}
	if got.LeaseCheckInterval != time.Minute || got.LeaseCheckinRepeat != 10*time.Minute || got.LeaseIdleGrace != 3*time.Minute {
		t.Fatalf("Parse = %+v", got)
	}
}

func TestParseLeaseTakeover(t *testing.T) {
	for _, tt := range []struct {
		in, want string
		warn     bool
	}{
		{"", "log", false},
		{"lease_takeover = \"log\"  # stage one\n", "log", false},
		{"lease_takeover = \"on\"\n", "on", false},
		{"lease_takeover = \"maybe\"\n", "log", true},
		{"lease_takeover = \"ON\"\n", "log", true},
		{"lease_takeover = on\n", "log", true},
	} {
		got, err := Parse(tt.in)
		if err != nil {
			t.Fatalf("Parse(%q) = %v; an invalid lease_takeover must not fail the load", tt.in, err)
		}
		if mode := got.TakeoverMode(); mode != tt.want {
			t.Errorf("Parse(%q).TakeoverMode() = %q, want %q", tt.in, mode, tt.want)
		}
		if tt.warn != (len(got.Warnings) == 1) {
			t.Errorf("Parse(%q) warnings = %q, want warning %v", tt.in, got.Warnings, tt.warn)
		}
		if tt.warn && (!strings.Contains(got.Warnings[0], "lease_takeover") || !strings.Contains(got.Warnings[0], `"log"`)) {
			t.Errorf("Parse(%q) warning = %q, want it to name the key and the fallback", tt.in, got.Warnings[0])
		}
	}
	got, _ := Parse("lease_takeover = \"maybe\"\n")
	if !strings.Contains(got.Warnings[0], "maybe") {
		t.Errorf("warning = %q, want it to name the value", got.Warnings[0])
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

func TestParseHibernateExempt(t *testing.T) {
	got, err := Parse(`hibernate_exempt = ["ops/*", "herdr:local/s1/w1:p1"]  # never hibernate these
`)
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"ops/*", "herdr:local/s1/w1:p1"}; !reflect.DeepEqual(got.HibernateExempt, want) {
		t.Fatalf("HibernateExempt = %q, want %q", got.HibernateExempt, want)
	}
	if _, err := Parse("hibernate_exempt = \"ops\"\n"); err == nil || !strings.Contains(err.Error(), "hibernate_exempt must be a list") {
		t.Fatalf("not a list: %v", err)
	}
}

func TestParseWakeTimeout(t *testing.T) {
	got, err := Parse("wake_timeout = \"90s\"  # slow machine\n")
	if err != nil || got.WakeTimeout != 90*time.Second {
		t.Fatalf("Parse = %+v, %v", got, err)
	}
	if _, err := Parse("wake_timeout = \"0s\"\n"); err == nil || !strings.Contains(err.Error(), "wake_timeout must be a positive duration") {
		t.Fatalf("zero: %v", err)
	}
}

func TestParseListLivenessMaxAge(t *testing.T) {
	got, err := Parse("list_liveness_max_age = \"30s\"\n")
	if err != nil || got.ListLivenessMaxAge != 30*time.Second {
		t.Fatalf("Parse = %+v, %v", got, err)
	}
	if _, err := Parse("list_liveness_max_age = \"soon\"\n"); err == nil || !strings.Contains(err.Error(), "list_liveness_max_age must be a positive duration, such as \"15s\"") {
		t.Fatalf("bad value: %v", err)
	}
}

const gib = 1 << 30

func TestParseHibernatePolicy(t *testing.T) {
	got, err := Parse(`hibernate_policy = "dry-run"  # only log
hibernate_below = "1.5GiB"
hibernate_target = "3 GiB"
hibernate_min_idle = "45m"
hibernate_interval = "30s"
`)
	if err != nil {
		t.Fatal(err)
	}
	want := Policy{Mode: "dry-run", Below: 3 * gib / 2, Target: 3 * gib, MinIdle: 45 * time.Minute, Interval: 30 * time.Second}
	if p := got.Policy(); p != want || len(got.Warnings) != 0 {
		t.Fatalf("Policy() = %+v, warnings %q; want %+v", p, got.Warnings, want)
	}
}

func TestPolicyDefaults(t *testing.T) {
	got, err := Parse("")
	if err != nil {
		t.Fatal(err)
	}
	if p := got.Policy(); p != (Policy{Mode: "off", MinIdle: 30 * time.Minute, Interval: time.Minute}) {
		t.Fatalf("no keys: Policy() = %+v", p)
	}
	got, err = Parse("hibernate_policy = \"on\"\nhibernate_below = \"512MiB\"\n")
	if err != nil {
		t.Fatal(err)
	}
	if p := got.Policy(); p.Mode != "on" || p.Below != 512<<20 || p.Target != 512<<20 {
		t.Fatalf("no target: Policy() = %+v, want the target at hibernate_below", p)
	}
}

func TestPolicyIsOffWhenMisconfigured(t *testing.T) {
	for _, in := range []string{
		"hibernate_policy = \"yes\"\nhibernate_below = \"1GiB\"\n",
		"hibernate_policy = \"on\"\n",
	} {
		got, err := Parse(in)
		if err != nil {
			t.Fatal(err)
		}
		if p := got.Policy(); p.Mode != "off" || len(got.Warnings) != 1 {
			t.Errorf("Parse(%q): Policy() = %+v, warnings %q; want off with a warning", in, p, got.Warnings)
		}
	}
}

func TestParseSize(t *testing.T) {
	for _, tt := range []struct {
		in   string
		want int64
	}{
		{"1.5GiB", 3 * gib / 2},
		{"3GiB", 3 * gib},
		{"512 MiB", 512 << 20},
		{"64KiB", 64 << 10},
		{"1TiB", 1 << 40},
		{"2048", 2048},
		{"100B", 100},
		{"1gib", gib},
	} {
		if got, err := parseSize(tt.in); err != nil || got != tt.want {
			t.Errorf("parseSize(%q) = %d, %v; want %d", tt.in, got, err, tt.want)
		}
	}
	for _, in := range []string{"", "GiB", "1.5", "-1GiB", "0GiB", "1GB", "1.5.2GiB", "lots"} {
		if got, err := parseSize(in); err == nil {
			t.Errorf("parseSize(%q) = %d, want an error", in, got)
		}
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
		{"lease_check_interval = \"0s\"\n", "line 1: lease_check_interval must be a positive duration"},
		{"lease_checkin_repeat = \"often\"\n", "line 1: lease_checkin_repeat must be a positive duration"},
		{"lease_idle_grace = 5\n", "line 1: lease_idle_grace must be a positive duration"},
		{"lease_checkouts = \"/a\"\n", "line 1: lease_checkouts must be a list of absolute paths"},
		{"lease_checkouts = [\"src/repo\"]\n", "line 1: lease_checkouts must be a list of absolute paths"},
		{"lease_ignore_procs = \"zsh\"\n", "line 1: lease_ignore_procs must be a list of program names"},
		{"lease_ignore_procs = [\"/bin/zsh\"]\n", "line 1: lease_ignore_procs must be a list of program names"},
		{"hibernate_below = \"lots\"\n", "line 1: hibernate_below must be a size"},
		{"hibernate_target = 3\n", "line 1: hibernate_target must be a size"},
		{"hibernate_min_idle = \"0s\"\n", "line 1: hibernate_min_idle must be a positive duration"},
		{"hibernate_interval = \"often\"\n", "line 1: hibernate_interval must be a positive duration"},
		{"hibernate_below = \"2GiB\"\nhibernate_target = \"1GiB\"\n", "hibernate_target must be at least hibernate_below"},
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
