package config

import (
	"errors"
	"testing"
)

func TestSourceForDir(t *testing.T) {
	cfg := &Config{Sources: []Source{
		{Kind: "github", Name: "outer", Cwd: "/src/outer"},
		{Kind: "jira", Name: "inner", Cwd: "/src/outer/sub/inner/"},
		{Kind: "github", Name: "twin-a", Cwd: "/src/twin"},
		{Kind: "github", Name: "twin-b", Cwd: "/src/twin"},
		{Kind: "github", Name: "linked", Cwd: "/link/to/real"},
		{Kind: "github", Name: "var", Cwd: "/private/var/tmp"},
	}}
	links := map[string]string{"/link/to/real": "/real", "/home/me/real/x": "/real/x", "/home/me": "/home/me", "/var": "/private/var"}
	env := Env{EvalSymlinks: func(p string) (string, error) {
		if r, ok := links[p]; ok {
			return r, nil
		}
		return "", errors.New("not a link")
	}}
	tests := []struct {
		dir    string
		want   string
		wantOK bool
	}{
		{dir: "/src/outer", want: "github:outer", wantOK: true},
		{dir: "/src/outer/a/b", want: "github:outer", wantOK: true},
		{dir: "/src/outer/sub/inner", want: "jira:inner", wantOK: true},
		{dir: "/src/outer/sub/inner/deep", want: "jira:inner", wantOK: true},
		{dir: "/src/outer/sub", want: "github:outer", wantOK: true},
		{dir: "/src/outerx", wantOK: false},
		{dir: "/src", wantOK: false},
		{dir: "/elsewhere", wantOK: false},
		{dir: "/src/twin/x", wantOK: false},
		{dir: "/home/me/real/x", want: "github:linked", wantOK: true},
		{dir: "/home/me/real/x/missing/deeper", want: "github:linked", wantOK: true},
		{dir: "/var/tmp/nope", want: "github:var", wantOK: true},
		{dir: "", wantOK: false},
	}
	for _, tc := range tests {
		got, ok := cfg.SourceForDir(tc.dir, env)
		if got != tc.want || ok != tc.wantOK {
			t.Errorf("SourceForDir(%q) = %q, %t; want %q, %t", tc.dir, got, ok, tc.want, tc.wantOK)
		}
	}
}

func TestSourceCoveringDir(t *testing.T) {
	cfg := &Config{Sources: []Source{
		{Kind: "github", Name: "twin-a", Cwd: "/src/twin"},
		{Kind: "github", Name: "twin-b", Cwd: "/src/twin"},
		{Kind: "github", Name: "outer", Cwd: "/src/outer"},
	}}
	env := Env{}
	if got, ok := cfg.SourceCoveringDir("/src/twin/sub", env); !ok || got != "github:twin-a" {
		t.Fatalf("SourceCoveringDir(twin) = %q, %t; want github:twin-a, true", got, ok)
	}
	if got, ok := cfg.SourceCoveringDir("/src/outer", env); !ok || got != "github:outer" {
		t.Fatalf("SourceCoveringDir(outer) = %q, %t; want github:outer, true", got, ok)
	}
	if got, ok := cfg.SourceCoveringDir("/other", env); ok || got != "" {
		t.Fatalf("SourceCoveringDir(other) = %q, %t; want empty, false", got, ok)
	}
}

func TestGlobalConfigPathAndHomeHelpers(t *testing.T) {
	env := Env{
		LookupEnv: func(k string) (string, bool) {
			if k == "XDG_CONFIG_HOME" {
				return "/xdg/cfg", true
			}
			return "", false
		},
		HomeDir: func() (string, error) { return "/home/alice", nil },
	}
	got, err := GlobalConfigPath(env)
	if err != nil || got != "/xdg/cfg/factory/factory.toml" {
		t.Fatalf("GlobalConfigPath(xdg) = %q, %v", got, err)
	}
	env.LookupEnv = func(string) (string, bool) { return "", false }
	got, err = GlobalConfigPath(env)
	if err != nil || got != "/home/alice/.config/factory/factory.toml" {
		t.Fatalf("GlobalConfigPath(default) = %q, %v", got, err)
	}
	env.LookupEnv = func(k string) (string, bool) { return "", k == "XDG_CONFIG_HOME" }
	if _, err := GlobalConfigPath(env); err == nil {
		t.Fatal("GlobalConfigPath(empty XDG_CONFIG_HOME) succeeded, want error")
	}

	for _, tc := range []struct {
		in   string
		want string
	}{
		{in: "/home/alice/src/repo", want: "~/src/repo"},
		{in: "/home/alice", want: "~"},
		{in: "/home/alice2/repo", want: "/home/alice2/repo"},
		{in: "/tmp/repo", want: "/tmp/repo"},
	} {
		short, err := ContractHome(tc.in, env)
		if err != nil {
			t.Fatalf("ContractHome(%q) error = %v", tc.in, err)
		}
		if short != tc.want {
			t.Errorf("ContractHome(%q) = %q, want %q", tc.in, short, tc.want)
		}
	}
	expanded, err := ExpandHome("~/src/repo", env)
	if err != nil || expanded != "/home/alice/src/repo" {
		t.Fatalf("ExpandHome = %q, %v", expanded, err)
	}
}
