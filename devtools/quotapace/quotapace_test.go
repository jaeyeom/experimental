package quotapace

import (
	"encoding/json"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// The fictional 7d window starts 2026-11-14 00:00Z and resets
// 2026-11-21 00:00Z (168 hours).
func winStart() time.Time { return time.Date(2026, 11, 14, 0, 0, 0, 0, time.UTC) }

func winReset() time.Time { return winStart().Add(168 * time.Hour) }

func at(h float64) time.Time { return winStart().Add(time.Duration(h * float64(time.Hour))) }

func reading(profile string, h, pct float64) Reading {
	return Reading{At: at(h), Profile: profile, Window: "7d", Percent: pct, ResetsAt: winReset(), Source: "test"}
}

func near(t *testing.T, name string, got, want float64) {
	t.Helper()
	if math.Abs(got-want) > 0.01 {
		t.Errorf("%s = %.4f, want %.4f", name, got, want)
	}
}

func implicit(t *testing.T) Plan {
	t.Helper()
	p, err := ImplicitPlan(reading("work", 0, 0))
	if err != nil {
		t.Fatal(err)
	}
	return p
}

func TestProjectPaceAndProjection(t *testing.T) {
	readings := []Reading{reading("work", 48, 25), reading("work", 72, 40)}
	pr, ok := Project(implicit(t), readings, 0, 6*time.Hour)
	if !ok {
		t.Fatal("no projection")
	}
	near(t, "percent", pr.Percent, 40)
	near(t, "on track", pr.OnTrack, 72.0/168*100)
	near(t, "pace so far", pr.PaceSoFar, 40.0/72)
	if pr.PaceRecent != nil {
		t.Errorf("recent pace = %v, want nil (no reading in the last 6h)", *pr.PaceRecent)
	}
	near(t, "projected", pr.Projected, 40+40.0/72*96)
	if pr.State != Green {
		t.Errorf("state = %s, want green", pr.State)
	}
}

func TestHoursTo100AndNeededPace(t *testing.T) {
	p := implicit(t)
	p.Target = 95
	pr, _ := Project(p, []Reading{reading("work", 72, 40)}, 0, 0)
	near(t, "hours to 100", *pr.HoursTo100, 60/(40.0/72))
	near(t, "needed pace", *pr.NeededPace, 55.0/96)

	flat, _ := Project(p, []Reading{reading("work", 72, 0)}, 0, 0)
	if flat.HoursTo100 != nil {
		t.Errorf("hours to 100 at zero pace = %v, want nil", *flat.HoursTo100)
	}
}

func TestStatesAndExitCodes(t *testing.T) {
	p := implicit(t)
	cases := []struct {
		name     string
		readings []Reading
		want     string
	}{
		{"green under the line", []Reading{reading("work", 72, 40)}, Green},
		{"amber over the line, slow recent pace", []Reading{reading("work", 66, 43), reading("work", 72, 44)}, Amber},
		{"red when the recent pace passes the limit", []Reading{reading("work", 66, 36), reading("work", 72, 40)}, Red},
	}
	for _, tc := range cases {
		pr, _ := Project(p, tc.readings, 0, 6*time.Hour)
		if pr.State != tc.want {
			t.Errorf("%s: state %s (projected %.1f), want %s", tc.name, pr.State, pr.Projected, tc.want)
		}
	}
	for state, code := range map[string]int{Green: 0, Amber: 1, Red: 2, Missing: 3} {
		if got := ExitCode(state); got != code {
			t.Errorf("ExitCode(%s) = %d, want %d", state, got, code)
		}
	}
}

func TestCheckpointsAcrossDST(t *testing.T) {
	// US daylight saving time ends 2026-11-01 02:00 in Los Angeles.
	base := time.Date(2026, 10, 31, 0, 0, 0, 0, time.UTC)
	p := Plan{
		Window: "3d", BaselineAt: base, ResetsAt: base.Add(72 * time.Hour), Target: 100, Amber: 5,
		Clocks: []string{"09:00", "21:00"}, TZ: "America/Los_Angeles",
	}
	times, err := p.CheckpointTimes()
	if err != nil {
		t.Fatal(err)
	}
	var got []string
	for _, tm := range times {
		got = append(got, tm.UTC().Format("01-02T15:04Z"))
	}
	// Oct 30 21:00 PDT is after the baseline. Oct 31 is PDT (UTC-7),
	// Nov 1 and 2 are PST (UTC-8), and the reset ends the list.
	want := []string{
		"10-31T04:00Z", "10-31T16:00Z", "11-01T04:00Z",
		"11-01T17:00Z", "11-02T05:00Z", "11-02T17:00Z",
		"11-03T00:00Z",
	}
	if strings.Join(got, " ") != strings.Join(want, " ") {
		t.Fatalf("checkpoints\n got %v\nwant %v", got, want)
	}
}

func TestCheckpointsFillActual(t *testing.T) {
	p := implicit(t)
	p.Every = "24h"
	cps, err := Checkpoints(p, []Reading{reading("work", 20, 10), reading("work", 47, 31)}, 0)
	if err != nil {
		t.Fatal(err)
	}
	if len(cps) != 7 {
		t.Fatalf("%d checkpoints, want 7", len(cps))
	}
	if cps[0].Actual == nil || *cps[0].Actual != 10 || cps[0].State != Green {
		t.Errorf("day 1 = %+v", cps[0])
	}
	if cps[1].Actual == nil || *cps[1].Actual != 31 || cps[1].State != Red {
		t.Errorf("day 2 = %+v, want 31%% red", cps[1])
	}
	if cps[2].Actual != nil || cps[2].State != "" {
		t.Errorf("day 3 = %+v, want empty", cps[2])
	}
	near(t, "day 1 on track", cps[0].OnTrack, 24.0/168*100)
	near(t, "day 1 amber", cps[0].AmberLine, 24.0/168*100+5)
}

func newStore(t *testing.T) Store {
	t.Helper()
	return Store{Dir: t.TempDir()}
}

func record(t *testing.T, s Store, r Reading, autoStart bool) RecordResult {
	t.Helper()
	res, err := s.Record(r, autoStart)
	if err != nil {
		t.Fatal(err)
	}
	return res
}

func TestResetClosesThePlan(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 10, 5), false)
	if _, err := s.StartPlan("work", StartOptions{Window: "7d"}, at(10)); err != nil {
		t.Fatal(err)
	}
	record(t, s, reading("work", 160, 90), false)
	next := Reading{At: at(170), Profile: "work", Window: "7d", Percent: 1, ResetsAt: winReset().Add(168 * time.Hour)}
	res := record(t, s, next, false)
	if res.Closed == nil || res.Closed.Closed.Reason != "reset time passed" || res.Closed.Closed.Final != 90 {
		t.Fatalf("closed = %+v", res.Closed)
	}
	if res.Started != nil {
		t.Fatal("started a plan without autoStart")
	}
	prof, err := s.LoadProfile("work")
	if err != nil {
		t.Fatal(err)
	}
	if len(prof.Active) != 0 || len(prof.History) != 1 {
		t.Fatalf("profile = %+v", prof)
	}
}

func TestResetByDropAutoStarts(t *testing.T) {
	s := newStore(t)
	if _, err := s.StartPlan("work", StartOptions{Window: "7d", ResetsAt: winReset(), Clocks: []string{"09:00"}, TZ: "UTC"}, at(0)); err != nil {
		t.Fatal(err)
	}
	record(t, s, reading("work", 100, 70), true)
	res := record(t, s, reading("work", 101, 3), true)
	if res.Closed == nil || res.Closed.Closed.Reason != "percent dropped" {
		t.Fatalf("closed = %+v", res.Closed)
	}
	if res.Started == nil || res.Started.Clocks[0] != "09:00" || res.Started.Baseline != 0 {
		t.Fatalf("started = %+v", res.Started)
	}
}

func TestRebaselineMidWindow(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 40, 27), false)
	if _, err := s.StartPlan("work", StartOptions{Window: "7d"}, at(40)); err != nil {
		t.Fatal(err)
	}
	base := 27.0
	plan, err := s.StartPlan("work", StartOptions{Window: "7d", Baseline: &base, At: at(40), Target: 95}, at(41))
	if err != nil {
		t.Fatal(err)
	}
	near(t, "on track at baseline", plan.OnTrack(at(40)), 27)
	near(t, "on track halfway", plan.OnTrack(at(104)), 27+(95-27)*0.5)
	near(t, "on track at reset", plan.OnTrack(winReset()), 95)
	prof, _ := s.LoadProfile("work")
	if len(prof.History) != 1 || prof.History[0].Closed.Reason != "rebaselined" || prof.History[0].Closed.Final != 27 {
		t.Fatalf("history = %+v", prof.History)
	}
}

func TestProfileOffset(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 72, 40), false)
	off := 3.0
	if _, err := s.StartPlan("work", StartOptions{Window: "7d", Offset: &off}, at(72)); err != nil {
		t.Fatal(err)
	}
	snap, err := s.Status([]string{"work"}, at(73), StatusOptions{})
	if err != nil {
		t.Fatal(err)
	}
	pr := snap.Profiles[0].Windows[0].Projection
	near(t, "offset percent", pr.Percent, 43)
	if pr.State != Red {
		t.Errorf("state = %s, want red (43%% is over the 42.9%% line at the same pace)", pr.State)
	}
}

func TestProfilesStaySeparate(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 72, 40), false)
	record(t, s, reading("home", 72, 10), false)
	snap, err := s.Status([]string{"home", "work"}, at(73), StatusOptions{})
	if err != nil {
		t.Fatal(err)
	}
	if got := snap.Profiles[0].Windows[0].Projection.Percent; got != 10 {
		t.Errorf("home = %v, want 10", got)
	}
	if got := snap.Profiles[1].Windows[0].Projection.Percent; got != 40 {
		t.Errorf("work = %v, want 40", got)
	}
	if n := len(snap.Profiles[1].Windows[0].History.Readings); n != 1 {
		t.Errorf("work has %d readings, want 1", n)
	}
}

func TestStaleReadingIsMissing(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 72, 40), false)
	snap, err := s.Status([]string{"work"}, at(80), StatusOptions{Stale: 6 * time.Hour})
	if err != nil {
		t.Fatal(err)
	}
	ws := snap.Profiles[0].Windows[0]
	if ws.State != Missing || ws.StatusLine(at(80)) != "7d missing (last reading 8h0m0s ago)" {
		t.Fatalf("state %s, line %q", ws.State, ws.StatusLine(at(80)))
	}
}

// sampleStore is the golden-file log: two profiles, a re-baselined plan
// with clock checkpoints, and a 5h window with no plan.
func sampleStore(t *testing.T) Store {
	t.Helper()
	s := newStore(t)
	for _, r := range []Reading{reading("work", 20, 10), reading("work", 47, 22), reading("work", 66, 30), reading("work", 72, 33)} {
		record(t, s, r, false)
	}
	if _, err := s.StartPlan("work", StartOptions{Window: "7d"}, at(20)); err != nil {
		t.Fatal(err)
	}
	base := 22.0
	if _, err := s.StartPlan("work", StartOptions{
		Window: "7d", Baseline: &base, At: at(47), Target: 95, Clocks: []string{"09:00", "21:00"}, TZ: "Asia/Seoul",
	}, at(47)); err != nil {
		t.Fatal(err)
	}
	record(t, s, Reading{At: at(71), Profile: "work", Window: "5h", Percent: 12, ResetsAt: at(73), Source: "claude-usage"}, false)
	record(t, s, reading("home", 70, 5), false)
	return s
}

func golden(t *testing.T, name string, got []byte) {
	t.Helper()
	path := filepath.Join("testdata", name)
	if os.Getenv("QUOTAPACE_UPDATE_GOLDEN") != "" {
		if err := os.WriteFile(path, got, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	want, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("%v (rerun with QUOTAPACE_UPDATE_GOLDEN=1)", err)
	}
	if name == "status.golden.json" {
		got, err = canonicalStatusJSON(got)
		if err != nil {
			t.Fatal(err)
		}
		want, err = canonicalStatusJSON(want)
		if err != nil {
			t.Fatal(err)
		}
	}
	if string(got) != string(want) {
		t.Fatalf("%s differs; got:\n%s", name, got)
	}
}

// canonicalStatusJSON removes insignificant floating-point serialization
// differences from the status golden while preserving meaningful precision.
func canonicalStatusJSON(data []byte) ([]byte, error) {
	decoder := json.NewDecoder(strings.NewReader(string(data)))
	decoder.UseNumber()
	var value any
	if err := decoder.Decode(&value); err != nil {
		return nil, fmt.Errorf("decode status JSON: %w", err)
	}
	var normalize func(any) (any, error)
	normalize = func(value any) (any, error) {
		switch value := value.(type) {
		case json.Number:
			n, err := value.Float64()
			if err != nil {
				return nil, fmt.Errorf("parse status JSON number %q: %w", value, err)
			}
			return math.Round(n*1e12) / 1e12, nil
		case []any:
			for i := range value {
				normalized, err := normalize(value[i])
				if err != nil {
					return nil, err
				}
				value[i] = normalized
			}
			return value, nil
		case map[string]any:
			for key := range value {
				normalized, err := normalize(value[key])
				if err != nil {
					return nil, err
				}
				value[key] = normalized
			}
			return value, nil
		default:
			return value, nil
		}
	}
	value, err := normalize(value)
	if err != nil {
		return nil, fmt.Errorf("normalize status JSON: %w", err)
	}
	data, err = json.MarshalIndent(value, "", "  ")
	if err != nil {
		return nil, fmt.Errorf("marshal canonical status JSON: %w", err)
	}
	return append(data, '\n'), nil
}

func TestStatusJSONGolden(t *testing.T) {
	s := sampleStore(t)
	snap, err := s.Status([]string{"home", "work"}, at(72.5), StatusOptions{Recent: 6 * time.Hour, Stale: 6 * time.Hour})
	if err != nil {
		t.Fatal(err)
	}
	data, err := json.MarshalIndent(snap, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	golden(t, "status.golden.json", append(data, '\n'))
}

func TestReportGolden(t *testing.T) {
	s := sampleStore(t)
	snap, err := s.Status([]string{"work"}, at(72.5), StatusOptions{Recent: 6 * time.Hour, Stale: 6 * time.Hour})
	if err != nil {
		t.Fatal(err)
	}
	var b strings.Builder
	for _, ws := range snap.Windows() {
		md, err := ws.Report()
		if err != nil {
			t.Fatal(err)
		}
		if b.Len() > 0 {
			b.WriteString("\n")
		}
		b.WriteString(md)
	}
	golden(t, "report.golden.md", []byte(b.String()))
}

func TestCheckNotifiesOnChangeOnly(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 60, 20), false)
	snapAt := func(now time.Time) Snapshot {
		snap, err := s.Status([]string{"work"}, now, StatusOptions{Recent: 6 * time.Hour, Stale: 6 * time.Hour})
		if err != nil {
			t.Fatal(err)
		}
		return snap
	}
	res := Check(snapAt(at(60)), nil, at(60), 6*time.Hour)
	if len(res.Problems) != 0 || res.Notify || res.Code != 0 {
		t.Fatalf("green: %+v", res)
	}

	record(t, s, reading("work", 66, 44), false)
	record(t, s, reading("work", 72, 45), false) // over the line, slow recent pace: amber
	res = Check(snapAt(at(72)), nil, at(72), 6*time.Hour)
	if !res.Notify || res.Code != 1 || len(res.Problems) != 1 || !strings.Contains(res.Problems[0], "work 7d amber") {
		t.Fatalf("amber: %+v", res)
	}
	sent := res.Next

	res = Check(snapAt(at(74)), sent, at(74), 6*time.Hour)
	if res.Notify || res.Code != 1 {
		t.Fatalf("unchanged amber re-notified: %+v", res)
	}
	res = Check(snapAt(at(79)), sent, at(79), 6*time.Hour)
	if !res.Notify || res.Code != 3 || !strings.Contains(res.Problems[0], "missing") {
		t.Fatalf("stale: %+v", res)
	}

	record(t, s, reading("work", 79.5, 70), false) // red
	res = Check(snapAt(at(80)), sent, at(80), 6*time.Hour)
	if !res.Notify || res.Code != 2 {
		t.Fatalf("red: %+v", res)
	}
	sent = res.Next
	if res = Check(snapAt(at(81)), sent, at(81), 6*time.Hour); res.Notify {
		t.Fatalf("red re-notified before the repeat: %+v", res)
	}
	if res = Check(snapAt(at(85)), sent, at(85), 0); res.Notify {
		t.Fatalf("zero repeat re-notified: %+v", res)
	}
}

func TestCheckRepeatsAfterInterval(t *testing.T) {
	s := newStore(t)
	record(t, s, reading("work", 72, 44), false)
	snap, err := s.Status([]string{"work"}, at(72), StatusOptions{})
	if err != nil {
		t.Fatal(err)
	}
	sent := Check(snap, nil, at(72), 6*time.Hour).Next
	if res := Check(snap, sent, at(78), 6*time.Hour); !res.Notify {
		t.Fatal("did not repeat after 6h")
	}
}

func TestParseDuration(t *testing.T) {
	for in, want := range map[string]time.Duration{"7d": 168 * time.Hour, "5h": 5 * time.Hour, "6d5h": 149 * time.Hour, "90m": 90 * time.Minute} {
		got, err := ParseDuration(in)
		if err != nil || got != want {
			t.Errorf("ParseDuration(%q) = %v, %v; want %v", in, got, err, want)
		}
	}
	for _, bad := range []string{"", "0d", "xd", "-1h", "d5h"} {
		if _, err := ParseDuration(bad); err == nil {
			t.Errorf("ParseDuration(%q) succeeded", bad)
		}
	}
	if err := CheckProfileName("a/b"); err == nil {
		t.Error("profile a/b accepted")
	}
}
