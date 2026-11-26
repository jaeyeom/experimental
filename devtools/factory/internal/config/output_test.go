package config_test

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestLoadOutput(t *testing.T) {
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
		wantTop        config.Output
		wantSrc        config.Output
		wantLane       config.Output
		wantLaneSource config.Output
		wantErr        string
	}{
		{
			name:    "default is pr",
			lines:   src,
			wantTop: config.Output{Mode: config.OutputModePR},
			wantSrc: config.Output{Mode: config.OutputModePR},
		},
		{
			name:           "top level merge inherited by source and lane",
			lines:          cat([]string{"[output]", `mode = "merge"`}, src, lane, order),
			wantTop:        config.Output{Mode: config.OutputModeMerge},
			wantSrc:        config.Output{Mode: config.OutputModeMerge},
			wantLane:       config.Output{},
			wantLaneSource: config.Output{Mode: config.OutputModeMerge},
		},
		{
			name:           "source overrides top and lane overrides source",
			lines:          cat([]string{"[output]", `mode = "merge"`}, src, []string{"[source.output]", `mode = "pr"`}, lane, []string{"[lane.output]", `mode = "merge"`}, order),
			wantTop:        config.Output{Mode: config.OutputModeMerge},
			wantSrc:        config.Output{Mode: config.OutputModePR},
			wantLane:       config.Output{Mode: config.OutputModeMerge},
			wantLaneSource: config.Output{Mode: config.OutputModeMerge},
		},
		{
			name:    "unknown top mode",
			lines:   cat([]string{"[output]", `mode = "rebase"`}, src),
			wantErr: `output.mode: expected pr or merge, found "rebase"`,
		},
		{
			name:    "empty top mode",
			lines:   cat([]string{"[output]", `mode = ""`}, src),
			wantErr: "output.mode: expected pr or merge, found empty",
		},
		{
			name:    "unknown source mode",
			lines:   cat(src, []string{"[source.output]", `mode = "push"`}),
			wantErr: `source "experimental" output.mode: expected pr or merge, found "push"`,
		},
		{
			name:    "unknown lane mode",
			lines:   cat(src, lane, []string{"[lane.output]", `mode = "push"`}, order),
			wantErr: `lane "linux" output.mode: expected pr or merge, found "push"`,
		},
		{
			name:    "empty lane mode",
			lines:   cat(src, lane, []string{"[lane.output]", `mode = ""`}, order),
			wantErr: `lane "linux" output.mode: expected pr or merge, found empty`,
		},
		{
			name:    "unknown output key",
			lines:   cat([]string{"[output]", `strategy = "squash"`}, src),
			wantErr: "unknown key output.strategy",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfg, err := newFake(t).load(tomlDoc(tc.lines))
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if cfg.Output != tc.wantTop {
				t.Fatalf("top = %+v, want %+v", cfg.Output, tc.wantTop)
			}
			if cfg.Sources[0].Output != tc.wantSrc {
				t.Fatalf("source = %+v, want %+v", cfg.Sources[0].Output, tc.wantSrc)
			}
			if len(cfg.Lanes) > 0 {
				if cfg.Lanes[0].Output != tc.wantLane {
					t.Fatalf("lane = %+v, want %+v", cfg.Lanes[0].Output, tc.wantLane)
				}
				if got := config.LaneSource(cfg.Sources[0], cfg.Lanes[0]).Output; got != tc.wantLaneSource {
					t.Fatalf("lane source output = %+v, want %+v", got, tc.wantLaneSource)
				}
			}
		})
	}
}
