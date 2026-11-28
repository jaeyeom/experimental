package main

import (
	"context"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/quotapace"
)

func TestPlanSetAndGateCLI(t *testing.T) {
	h := newHarness(t)
	// 2026-11-17T00:00:00Z is Tuesday. Previous Thu 15:00 UTC was 2026-11-12T15:00:00Z (105h earlier).
	// Wait: use 2026-11-14T22:00:00Z (55h after Thu 15:00 UTC) so line = 10 + 0.5*55 = 37.5.
	h.now = time.Date(2026, 11, 14, 22, 3, 0, 0, time.UTC)

	h.must(0, "", "plan", "set", "--profile", "work", "--window", "7d", "--reset", "Thu 15:00", "--tz", "UTC", "--rate", "0.5", "--offset", "10")
	h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "33.3", "--resets-at", "2026-11-19T15:00:00Z", "--at", "2026-11-14T22:00:00Z", "--source", "claude-usage")

	out := h.must(0, "", "gate", "--profile", "work", "--window", "7d", "--min-gap", "4.2")
	if out != "gap=4.2 line=37.5 used=33.3 (7d, profile work, reading 3m old)\n" {
		t.Fatalf("gate out = %q", out)
	}

	// Below min-gap -> exit 1.
	out = h.must(1, "", "gate", "--profile", "work", "--window", "7d", "--min-gap", "5.0")
	if !strings.HasPrefix(out, "gap=4.2 line=37.5 used=33.3") {
		t.Fatalf("gate below min-gap out = %q", out)
	}

	// JSON output.
	jsonOut := h.must(0, "", "gate", "--profile", "work", "--window", "7d", "--min-gap", "4.2", "--json")
	var gr quotapace.GateResult
	if err := json.Unmarshal([]byte(jsonOut), &gr); err != nil {
		t.Fatalf("unmarshal gate json: %v\n%s", err, jsonOut)
	}
	if gr.Exit != 0 || gr.Profile != "work" || gr.Window != "7d" || gr.Source != "claude-usage" {
		t.Fatalf("unexpected gate json: %+v", gr)
	}
	if gr.AgeSeconds == nil || *gr.AgeSeconds != 180 {
		t.Fatalf("ageSeconds = %v, want 180", gr.AgeSeconds)
	}
}

func TestRecordStdinFlag(t *testing.T) {
	h := newHarness(t)
	h.must(0, `{"window":"7d","percent":15,"resetsIn":"5d","source":"remote-box"}`+"\n", "record", "--profile", "remote", "--stdin")
	h.must(0, "", "plan", "set", "--profile", "remote", "--window", "7d", "--rate", "0.5", "--offset", "10")
	// 48h into 7d window -> line = 10 + 0.5*48 = 34, used = 15, gap = 19.
	h.must(0, "", "gate", "--profile", "remote", "--window", "7d", "--min-gap", "10")
}

func TestResetDisagreementWarnStatusAndDoctor(t *testing.T) {
	h := newHarness(t)
	h.must(0, "", "plan", "set", "--profile", "work", "--window", "7d", "--reset", "Thu 15:00", "--tz", "America/Los_Angeles", "--rate", "0.5", "--offset", "0")
	// Expected Thu 2026-11-19 15:00 PST is 23:00Z; give 2026-11-20 02:00Z (3h off).
	h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "20", "--resets-at", "2026-11-20T02:00:00Z")

	h.must(0, "", "status", "--profile", "work")
	if !strings.Contains(h.stderr.String(), "warning:") {
		t.Fatalf("expected warning on status stderr, got %q", h.stderr.String())
	}

	t.Setenv("XDG_DATA_HOME", h.data)
	checks := checkScheduleResets(context.Background(), true)
	if len(checks) != 1 || checks[0].Status != toolcli.StatusWarn {
		t.Fatalf("doctor checks = %+v, want 1 warn", checks)
	}
}

func TestGateWatchNotifiesOncePerCrossing(t *testing.T) {
	h := newHarness(t)
	h.now = time.Date(2026, 11, 14, 22, 0, 0, 0, time.UTC)
	h.must(0, "", "plan", "set", "--profile", "work", "--window", "7d", "--reset", "Thu 15:00", "--tz", "UTC", "--rate", "0.5", "--offset", "10")
	reset := "2026-11-19T15:00:00Z"
	// Tick 0: used=30 -> line=37.5 -> gap=7.5 >= 5 (open, initial)
	h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "30", "--resets-at", reset)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ticks := 0
	var notes []string
	var stdout, stderr strings.Builder
	code := run([]string{"gate", "--profile", "work", "--window", "7d", "--min-gap", "5", "--watch", "--every", "1m", "--notify-cmd", "agentbus send"}, env{
		getenv: func(k string) string {
			if k == "XDG_DATA_HOME" {
				return h.data
			}
			return ""
		},
		stdin:  strings.NewReader(""),
		stdout: &stdout,
		stderr: &stderr,
		now:    func() time.Time { return h.now },
		ctx:    ctx,
		after: func(time.Duration) <-chan time.Time {
			ticks++
			switch ticks {
			case 1:
				// Still open (gap 7.5 >= 5): no notify.
			case 2:
				// Cross below min-gap (used 35 -> gap 2.5 < 5): notify once.
				h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "35", "--resets-at", reset)
			case 3:
				// Still below min-gap: no notify.
			case 4:
				// Cross back above min-gap at next hour (line=38, used=31 -> gap=7 >= 5): notify once.
				h.now = h.now.Add(time.Hour)
				h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "31", "--resets-at", reset)
			case 5:
				cancel()
			}
			ch := make(chan time.Time, 1)
			if ticks < 5 {
				ch <- h.now
			}
			return ch
		},
		notify: func(argv []string, text string) error {
			notes = append(notes, text)
			return nil
		},
	})
	if code != 0 {
		t.Fatalf("watch exited %d, stderr=%s", code, stderr.String())
	}
	if len(notes) != 2 {
		t.Fatalf("got %d notifications, want 2: %v", len(notes), notes)
	}
	var first, second quotapace.GateResult
	if err := json.Unmarshal([]byte(notes[0]), &first); err != nil {
		t.Fatalf("first notify invalid JSON: %v (%q)", err, notes[0])
	}
	if err := json.Unmarshal([]byte(notes[1]), &second); err != nil {
		t.Fatalf("second notify invalid JSON: %v (%q)", err, notes[1])
	}
	if first.Exit != 1 || second.Exit != 0 {
		t.Fatalf("crossing exits = (%d, %d), want (1, 0)", first.Exit, second.Exit)
	}
}

func TestGateAndPlanSetUsageErrors(t *testing.T) {
	h := newHarness(t)
	for _, args := range [][]string{
		{"gate"},
		{"gate", "--profile", "work", "--window", "bogus"},
		{"gate", "--profile", "work", "--stale", "soon"},
		{"gate", "--profile", "work", "--every", "soon"},
		{"plan", "set"},
		{"plan", "set", "--profile", "work", "--window", "7d", "--reset", "Funday 15:00"},
		{"plan", "set", "--profile", "work", "--window", "5h", "--reset", "Thu 15:00"},
		{"plan", "set", "--profile", "work", "--window", "7d", "--tz", "Mars/Olympus"},
		{"plan", "set", "--profile", "work", "--window", "7d", "--rate", "-1"},
		{"record", "--profile", "work", "--window", "7d", "--percent", "10", "--resets-in", "1d", "--stdin"},
	} {
		if code := h.run("", args...); code != exitUsage {
			t.Errorf("%v: exit %d, want 64; stderr=%s", args, code, h.stderr.String())
		}
	}
}
