package memreport

import (
	"bytes"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

func TestHumanBytes(t *testing.T) {
	t.Parallel()

	tests := []struct {
		n    int64
		want string
	}{
		{0, "0 B"},
		{1023, "1023 B"},
		{1536, "1.5 KiB"},
		{471 * 1024 * 1024, "471.0 MiB"},
		{3 * 1024 * 1024 * 1024, "3.0 GiB"},
	}
	for _, tt := range tests {
		if got := HumanBytes(tt.n); got != tt.want {
			t.Errorf("HumanBytes(%d) = %q, want %q", tt.n, got, tt.want)
		}
	}
}

func TestFooterNamesTheHibernationPolicy(t *testing.T) {
	t.Parallel()

	avail := int64(8 << 30)
	for _, tt := range []struct {
		policy *Policy
		want   string
	}{
		{nil, "8.0 GiB available\n"},
		{&Policy{Mode: "off"}, "8.0 GiB available; hibernate policy off\n"},
		{
			&Policy{Mode: "dry-run", BelowBytes: 3 << 29, TargetBytes: 3 << 30, MinIdleSeconds: 1800},
			"8.0 GiB available; hibernate policy dry-run (below 1.5 GiB, target 3.0 GiB, min idle 30m0s)\n",
		},
	} {
		var buf bytes.Buffer
		if err := Build(Input{Available: &avail, Policy: tt.policy}).WriteText(&buf); err != nil {
			t.Fatal(err)
		}
		if !strings.HasSuffix(buf.String(), tt.want) {
			t.Errorf("policy %+v: text = %q, want it to end %q", tt.policy, buf.String(), tt.want)
		}
	}
}

func TestBuildKeepsAPaneWhoseProcessInfoFailed(t *testing.T) {
	t.Parallel()

	agents := []adapter.Agent{
		{Handle: "herdr:local/s/w1:p1", PaneID: "w1:p1", Kind: "claude", Status: "idle"},
		{Handle: "herdr:local/s/w1:p2", PaneID: "w1:p2", Kind: "claude", Status: "weird"},
	}
	r := Build(Input{
		Agents:     agents,
		Foreground: map[string][]int{"w1:p2": {7}},
		InfoErrors: map[string]error{"w1:p1": errors.New("pane_not_found")},
		Procs:      []procinfo.Process{{PID: 7, RSS: 10, Age: time.Hour}},
		Machine:    "m",
	})
	if len(r.Agents) != 2 || r.Agents[0].Pane != "w1:p2" || r.Agents[1].Error == "" {
		t.Fatalf("Build() = %+v", r.Agents)
	}
	if r.Agents[0].State != "unknown" || r.States["unknown"] != 1 || r.States["idle"] != 1 {
		t.Fatalf("states = %+v, rows %+v", r.States, r.Agents)
	}
	var buf bytes.Buffer
	if err := r.WriteText(&buf); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(buf.String(), "pane_not_found") || !strings.Contains(buf.String(), "available unknown") {
		t.Fatalf("text = %q", buf.String())
	}
}
