package config_test

import (
	"strings"
	"testing"
)

func TestLoadGHBin(t *testing.T) {
	tests := []struct {
		name    string
		key     string // top-level line, or empty
		env     *string
		want    string
		wantErr string
	}{
		{name: "default", want: "gh"},
		{name: "key", key: `gh_bin = "/path/fake-gh"`, want: "/path/fake-gh"},
		{name: "key over env", key: `gh_bin = "/path/fake-gh"`, env: strPtr("/env/gh"), want: "/path/fake-gh"},
		{name: "env", env: strPtr("gh-throttle"), want: "gh-throttle"},
		{name: "key home", key: `gh_bin = "~/bin/gh-throttle"`, want: "/home/u/bin/gh-throttle"},
		{name: "env home", env: strPtr("~/bin/gh"), want: "/home/u/bin/gh"},
		{
			name: "empty key", key: `gh_bin = ""`, env: strPtr("/env/gh"),
			wantErr: "gh_bin: expected a non-empty binary name or path, found empty",
		},
		{
			name: "empty env", env: strPtr(""),
			wantErr: "GH_BIN: expected a non-empty binary name or path, found empty",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			fs := newFake(t)
			if tc.env != nil {
				fs.env["GH_BIN"] = *tc.env
			}
			lines := githubLines()
			if tc.key != "" {
				lines = append([]string{tc.key}, lines...)
			}
			cfg, err := fs.load(tomlDoc(lines))
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("Load() error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if cfg.GHBin != tc.want {
				t.Fatalf("GHBin = %q, want %q", cfg.GHBin, tc.want)
			}
			for _, src := range cfg.Sources {
				if src.GHBin != tc.want {
					t.Fatalf("source %s GHBin = %q, want %q", src.Name, src.GHBin, tc.want)
				}
			}
		})
	}
}

func strPtr(s string) *string { return &s }

func TestLoadItemSource(t *testing.T) {
	fs := newFake(t)
	cfg, err := fs.load(tomlDoc(githubLines()))
	if err != nil || cfg.ItemSource != "" {
		t.Fatalf("unset: ItemSource = %q, err = %v", cfg.ItemSource, err)
	}
	fs.env["FACTORY_ITEM_SOURCE"] = "jira:acme"
	if cfg, err = fs.load(tomlDoc(githubLines())); err != nil || cfg.ItemSource != "jira:acme" {
		t.Fatalf("set: ItemSource = %q, err = %v", cfg.ItemSource, err)
	}
	for _, bad := range []string{"", "acme", ":acme", "jira:"} {
		fs.env["FACTORY_ITEM_SOURCE"] = bad
		_, err := fs.load(tomlDoc(githubLines()))
		if err == nil || !strings.Contains(err.Error(), "FACTORY_ITEM_SOURCE: expected kind:name") {
			t.Fatalf("%q: err = %v", bad, err)
		}
	}
}
