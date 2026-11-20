package main

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/quotapace"
)

type harness struct {
	t      *testing.T
	data   string
	now    time.Time
	notes  []string
	stdout bytes.Buffer
	stderr bytes.Buffer
}

func newHarness(t *testing.T) *harness {
	return &harness{t: t, data: t.TempDir(), now: time.Date(2026, 11, 17, 0, 0, 0, 0, time.UTC)}
}

func (h *harness) run(stdin string, args ...string) int {
	h.stdout.Reset()
	h.stderr.Reset()
	return run(args, env{
		getenv: func(k string) string {
			if k == "XDG_DATA_HOME" {
				return h.data
			}
			return ""
		},
		stdin:  strings.NewReader(stdin),
		stdout: &h.stdout,
		stderr: &h.stderr,
		now:    func() time.Time { return h.now },
		notify: func(argv []string, text string) error {
			h.notes = append(h.notes, strings.Join(argv, " ")+": "+text)
			return nil
		},
	})
}

func (h *harness) must(code int, stdin string, args ...string) string {
	h.t.Helper()
	if got := h.run(stdin, args...); got != code {
		h.t.Fatalf("%v: exit %d, want %d\nstdout: %s\nstderr: %s", args, got, code, h.stdout.String(), h.stderr.String())
	}
	return h.stdout.String()
}

func TestRecordAndStatusLine(t *testing.T) {
	h := newHarness(t)
	// 72h into the window that resets in 96h.
	h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "40", "--resets-in", "4d", "--source", "statusline")
	out := h.must(0, "", "status", "--profile", "work")
	if out != "7d 40% (on track 43%) green, projected 93%\n" {
		t.Fatalf("status = %q", out)
	}
	h.must(0, "", "record", "--profile", "home", "--window", "7d", "--percent", "50", "--resets-in", "4d")
	out = h.must(2, "", "status")
	if !strings.Contains(out, "home 7d 50% (on track 43%) red") || !strings.Contains(out, "work 7d 40%") {
		t.Fatalf("all profiles = %q", out)
	}
}

func TestRecordStdinFormats(t *testing.T) {
	h := newHarness(t)
	h.must(0, `{"window":"7d","percent":12,"resetsIn":"5d"}`+"\n", "record", "--profile", "work")
	usage := `{"planName":"Team","fiveHour":2,"sevenDay":13,"fiveHourResetAt":"2026-11-17T03:00:00Z","sevenDayResetAt":"2026-11-22T00:00:00Z"}`
	h.must(0, usage, "record", "--profile", "work", "--format", "claude-usage")
	var snap quotapace.Snapshot
	if err := json.Unmarshal([]byte(h.must(0, "", "status", "--json")), &snap); err != nil {
		t.Fatal(err)
	}
	ws := snap.Profiles[0].Windows
	if len(ws) != 2 || ws[0].Window != "5h" || ws[0].Latest.Source != "claude-usage" || ws[1].Latest.Percent != 13 {
		t.Fatalf("windows = %+v", ws)
	}
	if snap.Version != 1 {
		t.Fatalf("version = %d", snap.Version)
	}
}

func TestPlanStartRebaselineAndReport(t *testing.T) {
	h := newHarness(t)
	h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "27", "--resets-at", "2026-11-21T00:00:00Z")
	h.must(0, "", "plan", "start", "--profile", "work", "--window", "7d", "--baseline", "27", "--at", "2026-11-17T00:00:00Z",
		"--target", "95", "--checkpoints", "09:00,21:00", "--tz", "Asia/Seoul", "--offset", "1")
	// The offset makes the 27% reading 28%, a point over the baseline: amber.
	out := h.must(1, "", "report", "--profile", "work", "--window", "7d")
	for _, want := range []string{"## work 7d", "- Plan: 27% at 2026-11-17 09:00 KST to 95%", "| 2026-11-17 21:00 KST | 36% |", "- Now: 28%", "amber"} {
		if !strings.Contains(out, want) {
			t.Errorf("report lacks %q:\n%s", want, out)
		}
	}
}

func TestResetDetectedOnRecord(t *testing.T) {
	h := newHarness(t)
	h.must(0, "", "record", "--profile", "work", "--window", "5h", "--percent", "80", "--resets-in", "1h")
	h.must(0, "", "plan", "start", "--profile", "work", "--window", "5h")
	h.now = h.now.Add(90 * time.Minute)
	out := h.must(0, "", "record", "--profile", "work", "--window", "5h", "--percent", "3", "--resets-in", "4h30m", "--auto-start")
	if !strings.Contains(out, "window reset (reset time passed); plan closed at 80%") || !strings.Contains(out, "started a new plan") {
		t.Fatalf("record = %q", out)
	}
}

func TestCheckSilentThenPingsOnce(t *testing.T) {
	h := newHarness(t)
	h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "40", "--resets-in", "4d")
	if out := h.must(0, "", "check", "--notify-cmd", "notify-me --urgent"); out != "" || len(h.notes) != 0 {
		t.Fatalf("green check printed %q, notes %v", out, h.notes)
	}
	h.now = h.now.Add(time.Hour)
	h.must(0, "", "record", "--profile", "work", "--window", "7d", "--percent", "60", "--resets-in", "95h")
	out := h.must(2, "", "check", "--notify-cmd", "notify-me --urgent")
	if !strings.HasPrefix(out, "work 7d red: 60%") || len(h.notes) != 1 || !strings.HasPrefix(h.notes[0], "notify-me --urgent: work 7d red") {
		t.Fatalf("red check = %q, notes %v", out, h.notes)
	}
	h.now = h.now.Add(time.Hour)
	if out := h.must(2, "", "check", "--notify-cmd", "notify-me --urgent"); out != "" || len(h.notes) != 1 {
		t.Fatalf("unchanged red re-pinged: %q, notes %v", out, h.notes)
	}
	h.now = h.now.Add(7 * time.Hour)
	h.must(3, "", "check", "--notify-cmd", "notify-me --urgent")
	if len(h.notes) != 2 || !strings.Contains(h.notes[1], "missing") {
		t.Fatalf("stale notes = %v", h.notes)
	}
}

func TestUsageErrorsExit64(t *testing.T) {
	h := newHarness(t)
	for _, args := range [][]string{
		{},
		{"bogus"},
		{"record", "--profile", "work", "--percent", "3"},
		{"plan"},
		{"status", "--stale", "soon"},
		{"plan", "start", "--profile", "work", "--window", "7d", "--at", "2026-11-17T00:00:00Z"},
	} {
		if code := h.run("", args...); code != exitUsage {
			t.Errorf("%v: exit %d, want 64; stderr %s", args, code, h.stderr.String())
		}
	}
	if code := h.run("", "plan", "start", "--profile", "work", "--window", "7d"); code != exitSoftware {
		t.Errorf("plan with no reading: exit %d, want 70", code)
	}
}
