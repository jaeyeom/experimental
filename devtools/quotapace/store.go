package quotapace

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"
)

// Store keeps each profile's readings and plans under Dir:
//
//	profiles/<profile>/readings.jsonl  append-only readings
//	profiles/<profile>/plans.json      offset, active plans, history
//	check.json                         what check last notified
type Store struct {
	Dir string
}

// DataDir is $XDG_DATA_HOME/quotapace, or ~/.local/share/quotapace.
func DataDir(getenv func(string) string) (string, error) {
	if dir := getenv("XDG_DATA_HOME"); dir != "" {
		return filepath.Join(dir, "quotapace"), nil
	}
	home := getenv("HOME")
	if home == "" {
		return "", errors.New("HOME is not set")
	}
	return filepath.Join(home, ".local", "share", "quotapace"), nil
}

// CheckProfileName rejects names that are not a single path segment.
func CheckProfileName(name string) error {
	if name == "" || name == "." || name == ".." || strings.ContainsAny(name, `/\`) {
		return fmt.Errorf("bad profile name %q", name)
	}
	return nil
}

func (s Store) profileDir(profile string) string {
	return filepath.Join(s.Dir, "profiles", profile)
}

// Profiles lists the profiles that have a directory, by name.
func (s Store) Profiles() ([]string, error) {
	entries, err := os.ReadDir(filepath.Join(s.Dir, "profiles"))
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("list profiles: %w", err)
	}
	var names []string
	for _, e := range entries {
		if e.IsDir() {
			names = append(names, e.Name())
		}
	}
	slices.Sort(names)
	return names, nil
}

// Readings returns the profile's readings in log order.
func (s Store) Readings(profile string) ([]Reading, error) {
	data, err := os.ReadFile(filepath.Join(s.profileDir(profile), "readings.jsonl"))
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read readings: %w", err)
	}
	var out []Reading
	sc := bufio.NewScanner(bytes.NewReader(data))
	for line := 1; sc.Scan(); line++ {
		if strings.TrimSpace(sc.Text()) == "" {
			continue
		}
		var r Reading
		if err := json.Unmarshal(sc.Bytes(), &r); err != nil {
			return nil, fmt.Errorf("readings.jsonl line %d: %w", line, err)
		}
		out = append(out, r)
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("read readings: %w", err)
	}
	return out, nil
}

func (s Store) appendReading(r Reading) error {
	dir := s.profileDir(r.Profile)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("create profile dir: %w", err)
	}
	line, err := json.Marshal(r)
	if err != nil {
		return fmt.Errorf("encode reading: %w", err)
	}
	f, err := os.OpenFile(filepath.Join(dir, "readings.jsonl"), os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
	if err != nil {
		return fmt.Errorf("open readings: %w", err)
	}
	_, err = f.Write(append(line, '\n'))
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	if err != nil {
		return fmt.Errorf("append reading: %w", err)
	}
	return nil
}

// LoadProfile reads the profile's plans; a missing file is empty.
func (s Store) LoadProfile(profile string) (Profile, error) {
	var p Profile
	data, err := os.ReadFile(filepath.Join(s.profileDir(profile), "plans.json"))
	switch {
	case errors.Is(err, fs.ErrNotExist):
	case err != nil:
		return p, fmt.Errorf("read plans: %w", err)
	default:
		if err := json.Unmarshal(data, &p); err != nil {
			return p, fmt.Errorf("decode plans: %w", err)
		}
	}
	if p.Active == nil {
		p.Active = map[string]*Plan{}
	}
	if p.Schedules == nil {
		p.Schedules = map[string]*Schedule{}
	}
	return p, nil
}

// SaveProfile replaces the profile's plans file atomically.
func (s Store) SaveProfile(profile string, p Profile) error {
	return writeJSON(filepath.Join(s.profileDir(profile), "plans.json"), p)
}

func writeJSON(path string, v any) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create dir: %w", err)
	}
	data, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		return fmt.Errorf("encode %s: %w", filepath.Base(path), err)
	}
	tmp := path + ".tmp"
	if err := os.WriteFile(tmp, append(data, '\n'), 0o600); err != nil {
		return fmt.Errorf("write %s: %w", filepath.Base(path), err)
	}
	if err := os.Rename(tmp, path); err != nil {
		return fmt.Errorf("replace %s: %w", filepath.Base(path), err)
	}
	return nil
}

// RecordResult says what recording a reading did to the plans.
type RecordResult struct {
	Closed  *Plan
	Started *Plan
}

// Record appends r. When r shows that the active plan's window has
// reset, that plan is closed with its last percent; autoStart then
// starts a fresh plan for the new window with the same settings.
func (s Store) Record(r Reading, autoStart bool) (RecordResult, error) {
	var res RecordResult
	if err := CheckProfileName(r.Profile); err != nil {
		return res, err
	}
	if _, err := WindowLength(r.Window); err != nil {
		return res, err
	}
	prof, err := s.LoadProfile(r.Profile)
	if err != nil {
		return res, err
	}
	readings, err := s.Readings(r.Profile)
	if err != nil {
		return res, err
	}
	if err := s.appendReading(r); err != nil {
		return res, err
	}
	plan := prof.Active[r.Window]
	if plan == nil {
		return res, nil
	}
	in := windowReadings(*plan, readings)
	var prev *Reading
	if len(in) > 0 {
		prev = &in[len(in)-1]
	}
	reason := ResetReason(*plan, prev, r)
	if reason == "" {
		return res, nil
	}
	final := plan.Baseline
	if prev != nil {
		final = prev.Percent + prof.Offset
	}
	closed := *plan
	closed.Closed = &Closure{At: r.At, Final: final, Reason: reason}
	prof.History = append(prof.History, closed)
	delete(prof.Active, r.Window)
	res.Closed = &closed
	if autoStart {
		next, err := ImplicitPlan(r)
		if err != nil {
			return res, err
		}
		next.StartedAt = r.At
		next.Target, next.Amber = plan.Target, plan.Amber
		next.Every, next.Clocks, next.TZ = plan.Every, plan.Clocks, plan.TZ
		prof.Active[r.Window] = &next
		res.Started = &next
	}
	return res, s.SaveProfile(r.Profile, prof)
}

// StartOptions configures a new plan. A nil Baseline starts from the
// window start at 0%; otherwise the plan starts at At with Baseline.
// A zero ResetsAt takes the reset time of the window's latest reading.
// A nil Offset keeps the profile's offset.
type StartOptions struct {
	Window   string
	Baseline *float64
	At       time.Time
	ResetsAt time.Time
	Target   float64
	Amber    float64
	Every    string
	Clocks   []string
	TZ       string
	Offset   *float64
}

// StartPlan makes a new active plan. An existing active plan for the
// window is closed as "rebaselined" and kept in the history.
func (s Store) StartPlan(profile string, o StartOptions, now time.Time) (Plan, error) {
	if err := CheckProfileName(profile); err != nil {
		return Plan{}, err
	}
	length, err := WindowLength(o.Window)
	if err != nil {
		return Plan{}, err
	}
	prof, err := s.LoadProfile(profile)
	if err != nil {
		return Plan{}, err
	}
	readings, err := s.Readings(profile)
	if err != nil {
		return Plan{}, err
	}
	if o.Offset != nil {
		prof.Offset = *o.Offset
	}
	plan, err := newPlan(o, length, latestOf(readings, o.Window), now)
	if err != nil {
		return Plan{}, err
	}
	if old := prof.Active[o.Window]; old != nil {
		final := old.Baseline
		if pr, ok := Project(*old, readings, prof.Offset, 0); ok {
			final = pr.Percent
		}
		closed := *old
		closed.Closed = &Closure{At: now, Final: final, Reason: "rebaselined"}
		prof.History = append(prof.History, closed)
	}
	prof.Active[o.Window] = &plan
	return plan, s.SaveProfile(profile, prof)
}

func newPlan(o StartOptions, length time.Duration, latest *Reading, now time.Time) (Plan, error) {
	resets := o.ResetsAt
	if resets.IsZero() {
		if latest == nil {
			return Plan{}, fmt.Errorf("window %s has no reading; pass --resets-at or --resets-in", o.Window)
		}
		resets = latest.ResetsAt
	}
	plan := Plan{
		Window: o.Window, StartedAt: now, BaselineAt: resets.Add(-length), ResetsAt: resets,
		Target: o.Target, Amber: o.Amber, Every: o.Every, Clocks: o.Clocks, TZ: o.TZ,
	}
	if plan.Target == 0 {
		plan.Target = DefaultTarget
	}
	if plan.Amber == 0 {
		plan.Amber = DefaultAmber
	}
	if o.Baseline != nil {
		plan.Baseline = *o.Baseline
		plan.BaselineAt = o.At
		if plan.BaselineAt.IsZero() {
			plan.BaselineAt = now
		}
		if !plan.BaselineAt.Before(resets) {
			return Plan{}, fmt.Errorf("baseline time %s is not before the reset %s", plan.BaselineAt.Format(time.RFC3339), resets.Format(time.RFC3339))
		}
	}
	if _, err := plan.CheckpointTimes(); err != nil {
		return Plan{}, err
	}
	return plan, nil
}

// latestOf is the window's latest reading, or nil.
func latestOf(readings []Reading, window string) *Reading {
	var latest *Reading
	for i := range readings {
		r := &readings[i]
		if r.Window == window && (latest == nil || !r.At.Before(latest.At)) {
			latest = r
		}
	}
	return latest
}
