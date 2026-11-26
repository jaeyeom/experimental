package config_test

import (
	"testing"
	"time"
)

func TestLoadWakeBeforePrompt(t *testing.T) {
	tests := []struct {
		name    string
		line    string // [defaults] line, or empty
		want    string
		wantErr string
	}{
		{name: "default", want: "auto"},
		{name: "auto", line: `wake_before_prompt = "auto"`, want: "auto"},
		{name: "off", line: `wake_before_prompt = "off"`, want: "off"},
		{
			name: "bad", line: `wake_before_prompt = "on"`,
			wantErr: `defaults.wake_before_prompt: expected "auto" or "off", found "on"`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lines := githubLines()
			if tc.line != "" {
				lines = append([]string{"[defaults]", tc.line}, lines...)
			}
			cfg, err := newFake(t).load(tomlDoc(lines))
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("Load() error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if cfg.Defaults.WakeBeforePrompt != tc.want {
				t.Fatalf("WakeBeforePrompt = %q, want %q", cfg.Defaults.WakeBeforePrompt, tc.want)
			}
		})
	}
}

func TestLoadAgentTimings(t *testing.T) {
	cfg, err := newFake(t).load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatal(err)
	}
	d := cfg.Defaults
	if d.AgentReadyTimeout != 60*time.Second || d.AgentReadySettle != time.Second || d.AgentBusyTimeout != 20*time.Second {
		t.Fatalf("defaults = %s %s %s", d.AgentReadyTimeout, d.AgentReadySettle, d.AgentBusyTimeout)
	}
	lines := append([]string{"[defaults]", `agent_ready_timeout = "90s"`, `agent_ready_settle = "0s"`, `agent_busy_timeout = "5s"`}, githubLines()...)
	if cfg, err = newFake(t).load(tomlDoc(lines)); err != nil {
		t.Fatal(err)
	}
	d = cfg.Defaults
	if d.AgentReadyTimeout != 90*time.Second || d.AgentReadySettle != 0 || d.AgentBusyTimeout != 5*time.Second {
		t.Fatalf("set = %s %s %s", d.AgentReadyTimeout, d.AgentReadySettle, d.AgentBusyTimeout)
	}
	for _, bad := range []string{`agent_ready_timeout = "0s"`, `agent_busy_timeout = "-1s"`, `agent_ready_settle = "-1s"`} {
		if _, err := newFake(t).load(tomlDoc(append([]string{"[defaults]", bad}, githubLines()...))); err == nil {
			t.Fatalf("%s: expected an error", bad)
		}
	}
}
