package start

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestRenderCallerBus(t *testing.T) {
	it := source.Item{ID: "PRJ-1"}
	const tmpl = "bus={caller_bus} then {report_back} pane={caller}"
	tests := []struct {
		name, caller, bus, want string
	}{
		{
			name: "registered name", caller: "w1:p9", bus: "pm/coord",
			want: `bus=pm/coord then agentbus send pm/coord "<message>" pane=w1:p9`,
		},
		{
			name: "handle when no name", caller: "w1:p9", bus: "herdr:mac/s1/w1:p9",
			want: `bus=herdr:mac/s1/w1:p9 then agentbus send herdr:mac/s1/w1:p9 "<message>" pane=w1:p9`,
		},
		{
			name: "no agentbus falls back to herdr", caller: "w1:p9",
			want: `bus= then herdr agent prompt w1:p9 "<message>" pane=w1:p9`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := renderPrompt(tmpl, it, "jira:acme", "main", tc.caller, tc.bus); got != tc.want {
				t.Fatalf("prompt = %q\nwant     %q", got, tc.want)
			}
		})
	}
}

func TestRemoteLaneCallerWarning(t *testing.T) {
	tests := []struct {
		name, prompt, machine string
		warn                  bool
	}{
		{name: "remote lane, caller only", prompt: "Do {id}; tell {caller}", machine: "mac1", warn: true},
		{name: "remote lane, report_back", prompt: "Do {id}; {report_back}", machine: "mac1"},
		{name: "remote lane, caller and caller_bus", prompt: "Do {id}; {caller} or {caller_bus}", machine: "mac1"},
		{name: "local lane, caller only", prompt: "Do {id}; tell {caller}"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			h := newHarness(t)
			h.starter.Cfg.StartPrompt = tc.prompt
			h.starter.Caller = "w1:p9"
			if tc.machine != "" {
				h.starter.Lane, h.starter.Machine = "mac", tc.machine
			}
			if err := h.start(h.snap, okGate, Options{}); err != nil {
				t.Fatal(err)
			}
			got := strings.Count(h.out.String(), "warning: start_prompt reports back with {caller}")
			if (got == 1) != tc.warn || got > 1 {
				t.Fatalf("warnings = %d, want warn=%v; out =\n%s", got, tc.warn, h.out.String())
			}
		})
	}
}

func TestStartSetsItemSourceEnv(t *testing.T) {
	h := newHarness(t)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if len(h.herdr.env) != 2 || h.herdr.env[0] != "FACTORY_ITEM_SOURCE=jira:acme" || h.herdr.env[1] != "FACTORY_OUTPUT_MODE=pr" {
		t.Fatalf("tab env = %q, want [FACTORY_ITEM_SOURCE=jira:acme FACTORY_OUTPUT_MODE=pr]", h.herdr.env)
	}

	h2 := newHarness(t)
	h2.starter.Cfg.Output = config.Output{Mode: config.OutputModeMerge}
	if err := h2.start(h2.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if len(h2.herdr.env) != 2 || h2.herdr.env[0] != "FACTORY_ITEM_SOURCE=jira:acme" || h2.herdr.env[1] != "FACTORY_OUTPUT_MODE=merge" {
		t.Fatalf("tab env = %q, want [FACTORY_ITEM_SOURCE=jira:acme FACTORY_OUTPUT_MODE=merge]", h2.herdr.env)
	}
}
