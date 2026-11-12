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

func TestBuildFillsIdleSinceForIdleAndDoneRows(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)
	agent := func(pane, status string) adapter.Agent {
		return adapter.Agent{Handle: "herdr:local/s/" + pane, PaneID: pane, Kind: "claude", Status: status}
	}
	r := Build(Input{
		Agents: []adapter.Agent{agent("w1:p1", "idle"), agent("w1:p2", "done"), agent("w1:p3", "working"), agent("w1:p4", "idle"), agent("w1:p5", "idle")},
		Idle: map[string]Idle{
			"herdr:local/s/w1:p1": {Status: "idle", For: 3*time.Hour + 12*time.Minute + 30*time.Second},
			"herdr:local/s/w1:p2": {Status: "done", For: 40 * time.Minute, LowerBound: true},
			"herdr:local/s/w1:p3": {Status: "working", For: time.Hour},
			// The tracker saw w1:p5 working: not the row's state.
			"herdr:local/s/w1:p5": {Status: "working", For: time.Hour},
		},
		Now: now,
	})
	rows := map[string]Row{}
	for _, row := range r.Agents {
		rows[row.Pane] = row
	}
	p1 := rows["w1:p1"]
	if p1.IdleSince != "2026-11-11T08:47:30Z" || p1.IdleSeconds == nil || *p1.IdleSeconds != 11550 || p1.IdleSinceLowerBound {
		t.Fatalf("w1:p1 = %+v", p1)
	}
	p2 := rows["w1:p2"]
	if p2.IdleSince != "2026-11-11T11:20:00Z" || p2.IdleSeconds == nil || *p2.IdleSeconds != 2400 || !p2.IdleSinceLowerBound {
		t.Fatalf("w1:p2 = %+v", p2)
	}
	for _, pane := range []string{"w1:p3", "w1:p4", "w1:p5"} {
		if row := rows[pane]; row.IdleSince != "unknown" || row.IdleSeconds != nil || row.IdleSinceLowerBound {
			t.Fatalf("%s = %+v, want unknown", pane, row)
		}
	}
	var buf bytes.Buffer
	if err := r.WriteText(&buf); err != nil {
		t.Fatal(err)
	}
	for pane, want := range map[string]string{"w1:p1": " 3h12m ", "w1:p2": " >=40m ", "w1:p3": " unknown ", "w1:p4": " unknown "} {
		var line string
		for _, l := range strings.Split(buf.String(), "\n") {
			if strings.HasPrefix(l, pane+" ") {
				line = l
			}
		}
		if !strings.Contains(line, want) {
			t.Errorf("%s line %q lacks %q", pane, line, want)
		}
	}
}

func TestIdleText(t *testing.T) {
	t.Parallel()

	for _, tt := range []struct {
		d     time.Duration
		lower bool
		want  string
	}{
		{0, false, "0m"},
		{59 * time.Second, false, "0m"},
		{40 * time.Minute, true, ">=40m"},
		{3 * time.Hour, false, "3h0m"},
		{50*time.Hour + 5*time.Minute, false, "50h5m"},
	} {
		if got := idleText(tt.d, tt.lower); got != tt.want {
			t.Errorf("idleText(%s, %v) = %q, want %q", tt.d, tt.lower, got, tt.want)
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
