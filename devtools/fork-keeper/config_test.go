package forkkeeper

import (
	"slices"
	"strings"
	"testing"
)

func TestParseConfigValidation(t *testing.T) {
	t.Run("valid", func(t *testing.T) {
		raw := `{
			"gh_bin": "~/bin/gh-throttle",
			"reviewer_cmd": ["my-reviewer", "--read-only"],
			"forks": [
				{
					"name": "demo",
					"repo": "me/demo",
					"upstream_url": "https://github.com/up/demo.git",
					"branch": "main"
				}
			]
		}`
		cfg, err := ParseConfig([]byte(raw))
		if err != nil {
			t.Fatalf("ParseConfig: %v", err)
		}
		if cfg.GHBin != "~/bin/gh-throttle" {
			t.Errorf("GHBin = %q", cfg.GHBin)
		}
		if !slices.Equal(cfg.ReviewerCmd, []string{"my-reviewer", "--read-only"}) {
			t.Errorf("ReviewerCmd = %v", cfg.ReviewerCmd)
		}
	})

	cases := []struct {
		name    string
		json    string
		wantSub string
	}{
		{
			name:    "empty-gh-bin",
			json:    `{"gh_bin":"","forks":[]}`,
			wantSub: "gh_bin",
		},
		{
			name:    "empty-reviewer-cmd",
			json:    `{"reviewer_cmd":[],"forks":[]}`,
			wantSub: "reviewer_cmd",
		},
		{
			name:    "empty-reviewer-token",
			json:    `{"reviewer_cmd":["ok"," "],"forks":[]}`,
			wantSub: "command token must not be empty",
		},
		{
			name:    "duplicate-fork-name",
			json:    `{"forks":[{"name":"a","repo":"m/a","upstream_url":"u"},{"name":"a","repo":"m/b","upstream_url":"u"}]}`,
			wantSub: "duplicate fork name",
		},
		{
			name:    "missing-upstream",
			json:    `{"forks":[{"name":"a","repo":"m/a"}]}`,
			wantSub: "upstream_url must not be empty",
		},
		{
			name:    "flag-injection-repo",
			json:    `{"forks":[{"name":"a","repo":"--upload-pack=x","upstream_url":"u"}]}`,
			wantSub: "must not start with '-'",
		},
		{
			name:    "both-agent-and-reviewer-cmd",
			json:    `{"forks":[{"name":"a","repo":"m/a","upstream_url":"u","agent":["x"],"reviewer_cmd":["y"]}]}`,
			wantSub: "not both",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ParseConfig([]byte(tc.json))
			if err == nil || !strings.Contains(err.Error(), tc.wantSub) {
				t.Fatalf("ParseConfig err = %v, want containing %q", err, tc.wantSub)
			}
		})
	}
}

func TestResolveGHBin(t *testing.T) {
	tests := []struct {
		name     string
		cfgBin   string
		override string
		env      []string
		want     string
		wantErr  string
	}{
		{
			name: "default",
			env:  []string{"HOME=/home/u"},
			want: "gh",
		},
		{
			name: "env-gh-bin",
			env:  []string{"HOME=/home/u", "GH_BIN=/opt/bin/gh-throttle"},
			want: "/opt/bin/gh-throttle",
		},
		{
			name: "env-tilde",
			env:  []string{"HOME=/home/u", "GH_BIN=~/bin/gh-throttle"},
			want: "/home/u/bin/gh-throttle",
		},
		{
			name:   "config-overrides-env",
			cfgBin: "~/bin/cfg-gh",
			env:    []string{"HOME=/home/u", "GH_BIN=/opt/bin/gh-throttle"},
			want:   "/home/u/bin/cfg-gh",
		},
		{
			name:     "flag-overrides-config",
			cfgBin:   "cfg-gh",
			override: "/cli/gh-throttle",
			env:      []string{"HOME=/home/u", "GH_BIN=/opt/bin/gh-throttle"},
			want:     "/cli/gh-throttle",
		},
		{
			name:    "empty-env-error",
			env:     []string{"HOME=/home/u", "GH_BIN="},
			wantErr: "GH_BIN",
		},
		{
			name:    "tilde-without-home",
			cfgBin:  "~/bin/gh",
			wantErr: "HOME is not set",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfg := Config{GHBin: tc.cfgBin}
			got, err := cfg.ResolveGHBin(tc.override, tc.env)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want containing %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
		})
	}
}

func TestEffectiveReviewerCommand(t *testing.T) {
	fork := Fork{Name: "demo", Agent: []string{"fork-agent"}}
	cfgCmd := []string{"cfg-reviewer"}
	cliCmd := []string{"cli-reviewer"}

	got, err := fork.EffectiveReviewerCommand(cliCmd, cfgCmd, nil)
	if err != nil || !slices.Equal(got, cliCmd) {
		t.Fatalf("cli override: got=%v err=%v", got, err)
	}
	got, err = fork.EffectiveReviewerCommand(nil, cfgCmd, nil)
	if err != nil || !slices.Equal(got, []string{"fork-agent"}) {
		t.Fatalf("fork agent: got=%v err=%v", got, err)
	}
	got, err = (Fork{Name: "demo"}).EffectiveReviewerCommand(nil, cfgCmd, nil)
	if err != nil || !slices.Equal(got, cfgCmd) {
		t.Fatalf("cfg reviewer: got=%v err=%v", got, err)
	}
	got, err = (Fork{Name: "demo"}).EffectiveReviewerCommand(nil, nil, []string{"FORK_KEEPER_REVIEWER_CMD=my-rev --flag 'hello world'"})
	if err != nil || !slices.Equal(got, []string{"my-rev", "--flag", "hello world"}) {
		t.Fatalf("env reviewer: got=%v err=%v", got, err)
	}
	got, err = (Fork{Name: "demo"}).EffectiveReviewerCommand(nil, nil, nil)
	if err != nil || got != nil {
		t.Fatalf("standalone default: got=%v err=%v", got, err)
	}
	if _, err := (Fork{Name: "demo"}).EffectiveReviewerCommand(nil, nil, []string{"FORK_KEEPER_REVIEWER_CMD=   "}); err == nil {
		t.Fatalf("expected error on whitespace-only FORK_KEEPER_REVIEWER_CMD")
	}
}
