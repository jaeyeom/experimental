package config_test

import (
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestLoadStartPromptAppendPrecedence(t *testing.T) {
	src := githubLines()
	lane := []string{
		"[[lane]]",
		`name = "linux"`,
		`checkout = "/work/repo"`,
		"max_workers = 2",
	}
	order := []string{"[lanes]", `default = ["linux"]`}

	tests := []struct {
		name           string
		lines          []string
		wantTop        string
		wantSrc        string
		wantLaneSource string
	}{
		{
			name:  "default empty",
			lines: cat(src, lane, order),
		},
		{
			name:           "defaults inherited by source and lane",
			lines:          cat([]string{"[defaults]", `start_prompt_append = ["line 1", "line 2"]`}, src, lane, order),
			wantTop:        "line 1\nline 2",
			wantSrc:        "line 1\nline 2",
			wantLaneSource: "line 1\nline 2",
		},
		{
			name:           "source overrides defaults",
			lines:          cat([]string{"[defaults]", `start_prompt_append = ["from defaults"]`}, src, []string{`start_prompt_append = ["from source"]`}, lane, order),
			wantTop:        "from defaults",
			wantSrc:        "from source",
			wantLaneSource: "from source",
		},
		{
			name:           "source empty list clears defaults",
			lines:          cat([]string{"[defaults]", `start_prompt_append = ["from defaults"]`}, src, []string{`start_prompt_append = []`}, lane, order),
			wantTop:        "from defaults",
			wantSrc:        "",
			wantLaneSource: "",
		},
		{
			name:           "lane overrides source",
			lines:          cat([]string{"[defaults]", `start_prompt_append = ["from defaults"]`}, src, []string{`start_prompt_append = ["from source"]`}, lane, []string{`start_prompt_append = ["from lane"]`}, order),
			wantTop:        "from defaults",
			wantSrc:        "from source",
			wantLaneSource: "from lane",
		},
		{
			name:           "lane empty list clears source",
			lines:          cat(src, []string{`start_prompt_append = ["from source"]`}, lane, []string{`start_prompt_append = []`}, order),
			wantSrc:        "from source",
			wantLaneSource: "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfg, err := newFake(t).load(tomlDoc(tc.lines))
			if err != nil {
				t.Fatal(err)
			}
			if cfg.Defaults.StartPromptAppend != tc.wantTop {
				t.Fatalf("defaults = %q, want %q", cfg.Defaults.StartPromptAppend, tc.wantTop)
			}
			if cfg.Sources[0].StartPromptAppend != tc.wantSrc {
				t.Fatalf("source = %q, want %q", cfg.Sources[0].StartPromptAppend, tc.wantSrc)
			}
			gotLane := config.LaneSource(cfg.Sources[0], cfg.Lanes[0]).StartPromptAppend
			if gotLane != tc.wantLaneSource {
				t.Fatalf("lane source = %q, want %q", gotLane, tc.wantLaneSource)
			}
		})
	}
}

func TestLoadStartPromptAppendFile(t *testing.T) {
	f := newFake(t)
	appendPath := filepath.Join(f.home, "standing.txt")
	f.file(appendPath, "First line for {id}.\nSecond line: {report_back}\n", 0o600)
	lines := cat([]string{"[defaults]", `start_prompt_append = "@~/standing.txt"`}, githubLines())
	cfg, err := f.load(tomlDoc(lines))
	if err != nil {
		t.Fatal(err)
	}
	want := "First line for {id}.\nSecond line: {report_back}"
	if cfg.Defaults.StartPromptAppend != want {
		t.Fatalf("defaults = %q, want %q", cfg.Defaults.StartPromptAppend, want)
	}
	if cfg.Sources[0].StartPromptAppend != want {
		t.Fatalf("source = %q, want %q", cfg.Sources[0].StartPromptAppend, want)
	}
}

func TestLoadStartPromptAppendPlaceholderInStartPrompt(t *testing.T) {
	lines := replaceLine(githubLines(), "start_prompt", `start_prompt = "work {id}\n{{start_prompt_append}}"`)
	if _, err := newFake(t).load(tomlDoc(lines)); err != nil {
		t.Fatal(err)
	}
}

func TestLoadStartPromptAppendErrors(t *testing.T) {
	src := githubLines()
	lane := []string{
		"[[lane]]",
		`name = "linux"`,
		`checkout = "/work/repo"`,
		"max_workers = 2",
	}
	order := []string{"[lanes]", `default = ["linux"]`}

	tests := []struct {
		name    string
		lines   []string
		wantErr string
	}{
		{
			name:    "non-@ string in defaults",
			lines:   cat([]string{"[defaults]", `start_prompt_append = "plain string"`}, src),
			wantErr: `defaults field "start_prompt_append": expected a list of strings or @path`,
		},
		{
			name:    "number in source",
			lines:   cat(src, []string{`start_prompt_append = 42`}),
			wantErr: `source "experimental" field "start_prompt_append": expected a list of strings or @path`,
		},
		{
			name:    "non-string element in list",
			lines:   cat(src, []string{`start_prompt_append = ["ok", 42]`}),
			wantErr: `source "experimental" field "start_prompt_append": expected a list of strings or @path`,
		},
		{
			name:    "empty @ path",
			lines:   cat(src, []string{`start_prompt_append = "@"`}),
			wantErr: `source "experimental" field "start_prompt_append": expected a file path after @`,
		},
		{
			name:    "missing @file in defaults",
			lines:   cat([]string{"[defaults]", `start_prompt_append = "@~/does-not-exist.txt"`}, src),
			wantErr: `defaults field "start_prompt_append": expected to read`,
		},
		{
			name:    "missing @file in lane",
			lines:   cat(src, lane, []string{`start_prompt_append = "@~/does-not-exist.txt"`}, order),
			wantErr: `lane "linux" field "start_prompt_append": expected to read`,
		},
		{
			name:    "unknown placeholder in start_prompt_append",
			lines:   cat([]string{"[defaults]", `start_prompt_append = ["ask {bogus}"]`}, src),
			wantErr: `defaults field "start_prompt_append": expected one of`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := newFake(t).load(tomlDoc(tc.lines))
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want substring %q", err, tc.wantErr)
			}
		})
	}
}
