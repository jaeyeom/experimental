package quotapace

import (
	"fmt"
	"slices"
	"strings"
	"time"
)

// SchemaVersion is the version of the status --json schema.
const SchemaVersion = 1

// Snapshot is status --json: every selected profile and window.
type Snapshot struct {
	Version  int             `json:"version"`
	Now      time.Time       `json:"now"`
	Profiles []ProfileStatus `json:"profiles"`
}

// ProfileStatus is one profile's windows, shortest first.
type ProfileStatus struct {
	Profile string         `json:"profile"`
	Offset  float64        `json:"offset"`
	Windows []WindowStatus `json:"windows"`
}

// WindowStatus is one window: the latest reading, the plan line at now,
// the state, the projection, and the history.
type WindowStatus struct {
	Profile    string      `json:"-"`
	Window     string      `json:"window"`
	State      string      `json:"state"`
	Warnings   []string    `json:"warnings,omitempty"`
	Latest     *Latest     `json:"latest"`
	Plan       *PlanLine   `json:"plan"`
	Projection *Projection `json:"projection"`
	History    History     `json:"history"`

	plan Plan
}

// Latest is the newest reading of the window and its age.
type Latest struct {
	At         time.Time `json:"at"`
	Percent    float64   `json:"percent"`
	ResetsAt   time.Time `json:"resetsAt"`
	Source     string    `json:"source,omitempty"`
	AgeSeconds int64     `json:"ageSeconds"`
}

// PlanLine is the plan and its lines at now. Implicit means no plan was
// started, so the whole window from 0% is used.
type PlanLine struct {
	Implicit       bool        `json:"implicit"`
	BaselineAt     time.Time   `json:"baselineAt"`
	Baseline       float64     `json:"baseline"`
	ResetsAt       time.Time   `json:"resetsAt"`
	Target         float64     `json:"target"`
	Amber          float64     `json:"amber"`
	OnTrack        float64     `json:"onTrack"`
	AmberLine      float64     `json:"amberLine"`
	Limit          float64     `json:"limit"`
	NextCheckpoint *Checkpoint `json:"nextCheckpoint"`
}

// History is the active plan's readings and checkpoints, and the
// window's closed plans.
type History struct {
	Readings    []Reading    `json:"readings"`
	Checkpoints []Checkpoint `json:"checkpoints"`
	ClosedPlans []Plan       `json:"closedPlans"`
}

// StatusOptions sets the recent-pace span and when a reading is stale.
// A zero Stale never marks a reading stale.
type StatusOptions struct {
	Recent time.Duration
	Stale  time.Duration
}

// Status builds the snapshot for the profiles at now.
func (s Store) Status(profiles []string, now time.Time, o StatusOptions) (Snapshot, error) {
	snap := Snapshot{Version: SchemaVersion, Now: now, Profiles: []ProfileStatus{}}
	for _, name := range profiles {
		ps, err := s.profileStatus(name, now, o)
		if err != nil {
			return snap, fmt.Errorf("profile %s: %w", name, err)
		}
		snap.Profiles = append(snap.Profiles, ps)
	}
	return snap, nil
}

func (s Store) profileStatus(name string, now time.Time, o StatusOptions) (ProfileStatus, error) {
	prof, err := s.LoadProfile(name)
	if err != nil {
		return ProfileStatus{}, err
	}
	readings, err := s.Readings(name)
	if err != nil {
		return ProfileStatus{}, err
	}
	ps := ProfileStatus{Profile: name, Offset: prof.Offset, Windows: []WindowStatus{}}
	for _, window := range windowsOf(prof, readings) {
		ws, err := windowStatus(name, window, prof, readings, now, o)
		if err != nil {
			return ps, err
		}
		ps.Windows = append(ps.Windows, ws)
	}
	return ps, nil
}

func windowsOf(prof Profile, readings []Reading) []string {
	seen := map[string]bool{}
	for w := range prof.Active {
		seen[w] = true
	}
	for w := range prof.Schedules {
		seen[w] = true
	}
	for _, r := range readings {
		seen[r.Window] = true
	}
	out := make([]string, 0, len(seen))
	for w := range seen {
		out = append(out, w)
	}
	slices.SortFunc(out, windowOrder)
	return out
}

func windowStatus(profile, window string, prof Profile, readings []Reading, now time.Time, o StatusOptions) (WindowStatus, error) {
	ws := WindowStatus{Profile: profile, Window: window, State: Missing}
	ws.History.ClosedPlans = []Plan{}
	for _, p := range prof.History {
		if p.Window == window {
			ws.History.ClosedPlans = append(ws.History.ClosedPlans, p)
		}
	}
	latest := latestOf(readings, window)
	if latest != nil {
		ws.Latest = &Latest{At: latest.At, Percent: latest.Percent, ResetsAt: latest.ResetsAt, Source: latest.Source, AgeSeconds: int64(now.Sub(latest.At) / time.Second)}
	}
	warnings, err := scheduleWarnings(prof, window, latest)
	if err != nil {
		return ws, err
	}
	ws.Warnings = warnings
	plan, implicit, ok, err := activePlan(prof, window, latest, now)
	if err != nil {
		return ws, err
	}
	if !ok {
		ws.History.Readings, ws.History.Checkpoints = []Reading{}, []Checkpoint{}
		return ws, nil
	}
	ws.plan = plan
	ws.History.Readings = windowReadings(plan, readings)
	if ws.History.Readings == nil {
		ws.History.Readings = []Reading{}
	}
	cps, err := Checkpoints(plan, readings, prof.Offset)
	if err != nil {
		return ws, err
	}
	ws.History.Checkpoints = cps
	ws.Plan = &PlanLine{
		Implicit: implicit, BaselineAt: plan.BaselineAt, Baseline: plan.Baseline, ResetsAt: plan.ResetsAt,
		Target: plan.Target, Amber: plan.Amber, OnTrack: plan.OnTrack(now), AmberLine: plan.OnTrack(now) + plan.Amber,
		Limit: Limit, NextCheckpoint: nextCheckpoint(cps, now),
	}
	if pr, ok := Project(plan, readings, prof.Offset, o.Recent); ok {
		ws.Projection = &pr
		ws.State = pr.State
	}
	if ws.Latest == nil || !now.Before(plan.ResetsAt) || (o.Stale > 0 && now.Sub(ws.Latest.At) > o.Stale) {
		ws.State = Missing
	}
	return ws, nil
}

func scheduleWarnings(prof Profile, window string, latest *Reading) ([]string, error) {
	if prof.Active[window] != nil || latest == nil {
		return nil, nil
	}
	sched := prof.Schedules[window]
	if sched == nil {
		return nil, nil
	}
	warn, msg, err := sched.ResetDisagreement(latest.ResetsAt)
	if err != nil || !warn {
		return nil, err
	}
	return []string{msg}, nil
}

func activePlan(prof Profile, window string, latest *Reading, now time.Time) (Plan, bool, bool, error) {
	if p := prof.Active[window]; p != nil {
		return *p, false, true, nil
	}
	if sched := prof.Schedules[window]; sched != nil {
		p, ok, err := sched.PlanAt(now, latest)
		return p, false, ok, err
	}
	if latest == nil {
		return Plan{}, false, false, nil
	}
	p, err := ImplicitPlan(*latest)
	return p, true, err == nil, err
}

func nextCheckpoint(cps []Checkpoint, now time.Time) *Checkpoint {
	for i := range cps {
		if cps[i].At.After(now) {
			cp := cps[i]
			return &cp
		}
	}
	return nil
}

// StatusLine is the one-line form, such as
// "7d 31% (on track 33%) green, projected 91%".
func (ws WindowStatus) StatusLine(now time.Time) string {
	if ws.State == Missing {
		if ws.Latest == nil {
			return ws.Window + " missing (no reading)"
		}
		return fmt.Sprintf("%s missing (last reading %s ago)", ws.Window, roundAge(now.Sub(ws.Latest.At)))
	}
	pr := ws.Projection
	return fmt.Sprintf("%s %.0f%% (on track %.0f%%) %s, projected %.0f%%", ws.Window, pr.Percent, pr.OnTrack, ws.State, pr.Projected)
}

func roundAge(d time.Duration) string {
	if d >= time.Hour {
		return d.Truncate(time.Hour).String()
	}
	return d.Truncate(time.Minute).String()
}

// Windows flattens the snapshot.
func (s Snapshot) Windows() []WindowStatus {
	var out []WindowStatus
	for _, p := range s.Profiles {
		for _, w := range p.Windows {
			w.Profile = p.Profile
			out = append(out, w)
		}
	}
	return out
}

// WorstCode is the highest exit code among the windows; 0 when empty.
func (s Snapshot) WorstCode() int {
	code := 0
	for _, w := range s.Windows() {
		code = max(code, ExitCode(w.State))
	}
	return code
}

// Report is the Markdown report of one window: the projection, then the
// checkpoint table. Times are in the plan's zone.
func (ws WindowStatus) Report() (string, error) {
	var b strings.Builder
	fmt.Fprintf(&b, "## %s %s\n\n", ws.Profile, ws.Window)
	if ws.Plan == nil {
		b.WriteString("No reading.\n")
		return b.String(), nil
	}
	loc, err := ws.plan.Location()
	if err != nil {
		return "", err
	}
	stamp := func(t time.Time) string { return t.In(loc).Format("2006-01-02 15:04 MST") }
	plan := "Plan"
	if ws.Plan.Implicit {
		plan = "Plan (none started; whole window)"
	}
	fmt.Fprintf(&b, "- %s: %.0f%% at %s to %.0f%% at the reset %s, amber +%.0f\n",
		plan, ws.Plan.Baseline, stamp(ws.Plan.BaselineAt), ws.Plan.Target, stamp(ws.Plan.ResetsAt), ws.Plan.Amber)
	writeProjection(&b, ws, stamp)
	b.WriteString("\n| Checkpoint | On track | Amber | Limit | Actual | Status |\n")
	b.WriteString("|------------+----------+-------+-------+--------+--------|\n")
	for _, cp := range ws.History.Checkpoints {
		actual, state := "", ""
		if cp.Actual != nil {
			actual, state = fmt.Sprintf("%.0f%%", *cp.Actual), cp.State
		}
		fmt.Fprintf(&b, "| %s | %.0f%% | %.0f%% | %.0f%% | %s | %s |\n",
			stamp(cp.At), cp.OnTrack, cp.AmberLine, cp.Limit, actual, state)
	}
	return b.String(), nil
}

func writeProjection(b *strings.Builder, ws WindowStatus, stamp func(time.Time) string) {
	pr := ws.Projection
	if pr == nil {
		fmt.Fprintf(b, "- State: %s (no reading in this window)\n", ws.State)
		return
	}
	fmt.Fprintf(b, "- Now: %.0f%% at %s (on track %.0f%%, amber line %.0f%%): %s\n",
		pr.Percent, stamp(pr.At), pr.OnTrack, pr.AmberLine, ws.State)
	fmt.Fprintf(b, "- Pace so far: %.2f points/h", pr.PaceSoFar)
	if pr.PaceRecent != nil {
		fmt.Fprintf(b, "; recent: %.2f points/h", *pr.PaceRecent)
	}
	b.WriteString("\n")
	fmt.Fprintf(b, "- Projected at reset: %.0f%%\n", pr.Projected)
	if pr.HoursTo100 != nil {
		fmt.Fprintf(b, "- Hours until 100%%: %.1f\n", *pr.HoursTo100)
	} else {
		b.WriteString("- Hours until 100%: never at this pace\n")
	}
	if pr.NeededPace != nil {
		fmt.Fprintf(b, "- Pace to end at %.0f%%: %.2f points/h\n", ws.Plan.Target, *pr.NeededPace)
	}
}
