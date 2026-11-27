package config_test

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestLoadLeaseGate(t *testing.T) {
	tests := []struct {
		name    string
		lines   []string
		want    string
		wantErr string
	}{
		{name: "default is auto", want: config.LeaseGateAuto},
		{name: "auto", lines: []string{"[lease]", `gate = "auto"`}, want: config.LeaseGateAuto},
		{name: "off", lines: []string{"[lease]", `gate = "off"`}, want: config.LeaseGateOff},
		{name: "required", lines: []string{"[lease]", `gate = "required"`}, want: config.LeaseGateRequired},
		{name: "queue", lines: []string{"[lease]", `gate = "queue"`}, want: config.LeaseGateQueue},
		{name: "queue-required", lines: []string{"[lease]", `gate = "queue-required"`}, want: config.LeaseGateQueueRequired},
		{name: "unknown", lines: []string{"[lease]", `gate = "maybe"`}, wantErr: `lease.gate: expected auto, off, required, queue, or queue-required, found "maybe"`},
		{name: "empty", lines: []string{"[lease]", `gate = ""`}, wantErr: "lease.gate: expected auto, off, required, queue, or queue-required, found empty"},
		{name: "unknown key", lines: []string{"[lease]", `gates = "off"`}, wantErr: "unknown key lease.gates"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfg, err := newFake(t).load(tomlDoc(append(tc.lines, githubLines()...)))
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if cfg.Lease.Gate != tc.want {
				t.Fatalf("lease gate = %q, want %q", cfg.Lease.Gate, tc.want)
			}
		})
	}
}

func cat(parts ...[]string) []string {
	var out []string
	for _, p := range parts {
		out = append(out, p...)
	}
	return out
}

func TestLoadLeasePrompt(t *testing.T) {
	src := githubLines()
	lane := []string{
		"[[lane]]",
		`name = "linux"`,
		`checkout = "/work/repo"`,
		"max_workers = 2",
	}
	order := []string{"[lanes]", `default = ["linux"]`}
	tests := []struct {
		name     string
		lines    []string
		wantTop  config.LeasePrompt
		wantSrc  config.LeasePrompt
		wantLane config.LeasePrompt
		wantErr  string
	}{
		{
			name:    "defaults",
			lines:   src,
			wantTop: config.LeasePrompt{Mode: "off", ClaimFor: "45m"},
			wantSrc: config.LeasePrompt{Mode: "off", ClaimFor: "45m"},
		},
		{
			name:    "top level is inherited",
			lines:   cat([]string{"[lease]", `mode = "prompt"`, `claim_for = "1h"`}, src),
			wantTop: config.LeasePrompt{Mode: "prompt", ClaimFor: "1h"},
			wantSrc: config.LeasePrompt{Mode: "prompt", ClaimFor: "1h"},
		},
		{
			name:    "source overrides top level",
			lines:   cat([]string{"[lease]", `mode = "prompt"`}, src, []string{"[source.lease]", `mode = "prompt+check"`}),
			wantTop: config.LeasePrompt{Mode: "prompt", ClaimFor: "45m"},
			wantSrc: config.LeasePrompt{Mode: "prompt+check", ClaimFor: "45m"},
		},
		{
			name:     "lane keeps only what it sets",
			lines:    cat(src, lane, []string{"[lane.lease]", `claim_for = "20m"`}, order),
			wantTop:  config.LeasePrompt{Mode: "off", ClaimFor: "45m"},
			wantSrc:  config.LeasePrompt{Mode: "off", ClaimFor: "45m"},
			wantLane: config.LeasePrompt{ClaimFor: "20m"},
		},
		{
			name:    "unknown mode",
			lines:   cat([]string{"[lease]", `mode = "maybe"`}, src),
			wantErr: `lease.mode: expected off, prompt, or prompt+check, found "maybe"`,
		},
		{
			name:    "unknown source mode",
			lines:   cat(src, []string{"[source.lease]", `mode = "maybe"`}),
			wantErr: `source "experimental" lease.mode: expected off, prompt, or prompt+check, found "maybe"`,
		},
		{
			name:    "unknown lane mode",
			lines:   cat(src, lane, []string{"[lane.lease]", `mode = "maybe"`}, order),
			wantErr: `lane "linux" lease.mode: expected off, prompt, or prompt+check, found "maybe"`,
		},
		{
			name:    "empty lane mode",
			lines:   cat(src, lane, []string{"[lane.lease]", `mode = ""`}, order),
			wantErr: `lane "linux" lease.mode: expected off, prompt, or prompt+check, found empty`,
		},
		{
			name:    "bad claim_for",
			lines:   cat([]string{"[lease]", `claim_for = "soon"`}, src),
			wantErr: `lease.claim_for: expected a positive duration such as "45m", found "soon"`,
		},
		{
			name:    "zero claim_for",
			lines:   cat(src, []string{"[source.lease]", `claim_for = "0s"`}),
			wantErr: `source "experimental" lease.claim_for: expected a positive duration such as "45m", found "0s"`,
		},
		{
			name:    "gate is top level only",
			lines:   cat(src, []string{"[source.lease]", `gate = "off"`}),
			wantErr: "unknown key source.lease.gate",
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
			if cfg.Lease.LeasePrompt != tc.wantTop {
				t.Fatalf("top = %+v, want %+v", cfg.Lease.LeasePrompt, tc.wantTop)
			}
			if cfg.Sources[0].Lease != tc.wantSrc {
				t.Fatalf("source = %+v, want %+v", cfg.Sources[0].Lease, tc.wantSrc)
			}
			if len(cfg.Lanes) > 0 && cfg.Lanes[0].Lease != tc.wantLane {
				t.Fatalf("lane = %+v, want %+v", cfg.Lanes[0].Lease, tc.wantLane)
			}
		})
	}
}

func TestLeasePromptOver(t *testing.T) {
	base := config.LeasePrompt{Mode: "prompt", ClaimFor: "45m"}
	if got := (config.LeasePrompt{}).Over(base); got != base {
		t.Fatalf("empty over = %+v", got)
	}
	got := config.LeasePrompt{ClaimFor: "10m"}.Over(base)
	if got != (config.LeasePrompt{Mode: "prompt", ClaimFor: "10m"}) {
		t.Fatalf("over = %+v", got)
	}
}

func TestLoadLeaseRulePlaceholder(t *testing.T) {
	lines := replaceLine(githubLines(), "start_prompt", `start_prompt = "work {id}\n{{lease_rule}}"`)
	if _, err := newFake(t).load(tomlDoc(lines)); err != nil {
		t.Fatal(err)
	}
	lines = replaceLine(githubLines(), "start_prompt", `start_prompt = "work {{other}}"`)
	_, err := newFake(t).load(tomlDoc(lines))
	if err == nil || !strings.Contains(err.Error(), "literal braces aren't supported") {
		t.Fatalf("err = %v", err)
	}
}

func TestLoadLeaseRestore(t *testing.T) {
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
		wantTop        config.LeasePrompt
		wantSrc        config.LeasePrompt
		wantLaneSource config.LeasePrompt
		wantErr        string
	}{
		{
			name:           "top restore inherited by source and lane",
			lines:          cat([]string{"[lease]", `mode = "prompt"`, "restore = true"}, src, lane, order),
			wantTop:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: true},
			wantSrc:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: true},
			wantLaneSource: config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: true},
		},
		{
			name:           "source restore false overrides top restore true",
			lines:          cat([]string{"[lease]", `mode = "prompt"`, "restore = true"}, src, []string{"[source.lease]", "restore = false"}, lane, order),
			wantTop:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: true},
			wantSrc:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: false},
			wantLaneSource: config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: false},
		},
		{
			name:           "source mode off clears inherited restore",
			lines:          cat([]string{"[lease]", `mode = "prompt"`, "restore = true"}, src, []string{"[source.lease]", `mode = "off"`}, lane, order),
			wantTop:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: true},
			wantSrc:        config.LeasePrompt{Mode: "off", ClaimFor: "45m", Restore: false},
			wantLaneSource: config.LeasePrompt{Mode: "off", ClaimFor: "45m", Restore: false},
		},
		{
			name:           "lane restore false overrides source restore true",
			lines:          cat([]string{"[lease]", `mode = "prompt"`, "restore = true"}, src, lane, []string{"[lane.lease]", "restore = false"}, order),
			wantTop:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: true},
			wantSrc:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: true},
			wantLaneSource: config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: false},
		},
		{
			name:           "lane restore true inherits top prompt mode",
			lines:          cat([]string{"[lease]", `mode = "prompt"`}, src, lane, []string{"[lane.lease]", "restore = true"}, order),
			wantTop:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m"},
			wantSrc:        config.LeasePrompt{Mode: "prompt", ClaimFor: "45m"},
			wantLaneSource: config.LeasePrompt{Mode: "prompt", ClaimFor: "45m", Restore: true},
		},
		{
			name:    "top restore true with mode off rejected",
			lines:   cat([]string{"[lease]", "restore = true"}, src),
			wantErr: `lease.restore: expected mode prompt or prompt+check when restore is true, found "off"`,
		},
		{
			name:    "source explicit mode off with restore true rejected",
			lines:   cat([]string{"[lease]", `mode = "prompt"`}, src, []string{"[source.lease]", `mode = "off"`, "restore = true"}),
			wantErr: `source "experimental" lease.restore: expected mode prompt or prompt+check when restore is true, found "off"`,
		},
		{
			name:    "lane restore true when source mode is off rejected",
			lines:   cat(src, lane, []string{"[lane.lease]", "restore = true"}, order),
			wantErr: `lane "linux" lease.restore: expected mode prompt or prompt+check when restore is true, found "off"`,
		},
		{
			name:    "lane explicit mode off with restore true rejected",
			lines:   cat([]string{"[lease]", `mode = "prompt"`}, src, lane, []string{"[lane.lease]", `mode = "off"`, "restore = true"}, order),
			wantErr: `lane "linux" lease.restore: expected mode prompt or prompt+check when restore is true, found "off"`,
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
			if cfg.Lease.LeasePrompt != tc.wantTop {
				t.Fatalf("top = %+v, want %+v", cfg.Lease.LeasePrompt, tc.wantTop)
			}
			if cfg.Sources[0].Lease != tc.wantSrc {
				t.Fatalf("source = %+v, want %+v", cfg.Sources[0].Lease, tc.wantSrc)
			}
			if got := config.LaneSource(cfg.Sources[0], cfg.Lanes[0]).Lease; got != tc.wantLaneSource {
				t.Fatalf("lane source = %+v, want %+v", got, tc.wantLaneSource)
			}
		})
	}
}
