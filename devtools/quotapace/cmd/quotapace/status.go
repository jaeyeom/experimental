package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/quotapace"
)

func runPlanStart(args []string, store quotapace.Store, e env) error {
	fs := newFlags("plan start", e)
	profile := fs.String("profile", "", "profile")
	window := fs.String("window", "", "window, such as 7d")
	baseline := fs.Float64("baseline", -1, "percent at --at; omit to start from the window start at 0%")
	atFlag := fs.String("at", "", "baseline time, RFC 3339 (default now)")
	resetsIn := fs.String("resets-in", "", "time until the reset")
	resetsAt := fs.String("resets-at", "", "reset time, RFC 3339 (default: the latest reading's)")
	offset := fs.Float64("offset", 0, "points added to every reading of the profile")
	target := fs.Float64("target", quotapace.DefaultTarget, "percent to end at")
	amber := fs.Float64("amber", quotapace.DefaultAmber, "amber line, in points above on track")
	every := fs.String("every", "", "checkpoint step, such as 12h (when --checkpoints is not set)")
	clocks := fs.String("checkpoints", "", "checkpoint clock times, such as 09:00,21:00")
	tz := fs.String("tz", "", "checkpoint time zone, such as Asia/Seoul (default UTC)")
	if err := parse(fs, args); err != nil {
		return err
	}
	if *profile == "" || *window == "" {
		return usageErr("plan start needs --profile and --window")
	}
	now := e.now()
	o := quotapace.StartOptions{Window: *window, Target: *target, Amber: *amber, Every: *every, TZ: *tz}
	var err error
	if o.At, err = parseTime("at", *atFlag); err != nil {
		return err
	}
	if o.ResetsAt, err = resetTime(*resetsAt, *resetsIn, now); err != nil {
		return err
	}
	if *baseline >= 0 {
		o.Baseline = baseline
	} else if *atFlag != "" {
		return usageErr("--at sets the baseline time; pass --baseline too")
	}
	if *clocks != "" {
		o.Clocks = strings.Split(*clocks, ",")
	}
	if flagSet(fs, "offset") {
		o.Offset = offset
	}
	plan, err := store.StartPlan(*profile, o, now)
	if err != nil {
		return fmt.Errorf("plan start: %w", err)
	}
	fmt.Fprintf(e.stdout, "%s %s: plan %.0f%% at %s to %.0f%% at %s\n", *profile, *window,
		plan.Baseline, plan.BaselineAt.Format(time.RFC3339), plan.Target, plan.ResetsAt.Format(time.RFC3339))
	return nil
}

func runPlanSet(args []string, store quotapace.Store, e env) error {
	fs := newFlags("plan set", e)
	profile := fs.String("profile", "", "profile")
	window := fs.String("window", "", "window, such as 7d")
	reset := fs.String("reset", "", `weekly reset weekday and time in --tz, such as "Thu 15:00" (default: latest reading's reset)`)
	tz := fs.String("tz", "", "time zone for --reset, such as America/Los_Angeles (default UTC)")
	rate := fs.Float64("rate", 0, "points per hour along the plan line (default: (100 - offset) / window hours)")
	offset := fs.Float64("offset", 0, "plan line starting percent at window start (capped at 100)")
	amber := fs.Float64("amber", quotapace.DefaultAmber, "amber line, in points above on track")
	if err := parse(fs, args); err != nil {
		return err
	}
	if *profile == "" || *window == "" {
		return usageErr("plan set needs --profile and --window")
	}
	if err := quotapace.CheckProfileName(*profile); err != nil {
		return usageErr("%v", err)
	}
	length, err := quotapace.WindowLength(*window)
	if err != nil {
		return usageErr("--window: %v", err)
	}
	r := *rate
	if !flagSet(fs, "rate") {
		r = max(0, (quotapace.DefaultTarget-*offset)/length.Hours())
	} else if r < 0 {
		return usageErr("--rate %.2f must be >= 0", r)
	}
	sched := quotapace.Schedule{
		Window: *window, Reset: *reset, TZ: *tz, Rate: r, Offset: *offset, Amber: *amber,
	}
	if err := validateSchedule(sched, length); err != nil {
		return err
	}
	saved, err := store.SetSchedule(*profile, sched, e.now())
	if err != nil {
		return fmt.Errorf("plan set: %w", err)
	}
	fmt.Fprintf(e.stdout, "%s %s: schedule %.1f%% + %.3f%%/h (%s)\n", *profile, *window, saved.Offset, saved.Rate, formatResetLabel(saved))
	return nil
}

func validateSchedule(sched quotapace.Schedule, length time.Duration) error {
	if _, err := sched.Location(); err != nil {
		return usageErr("%v", err)
	}
	if sched.Reset != "" {
		if _, _, _, err := quotapace.ParseWeeklyReset(sched.Reset); err != nil {
			return usageErr("%v", err)
		}
		if length != 7*24*time.Hour {
			return usageErr("--reset weekly schedule requires a 7d window, got %q", sched.Window)
		}
	}
	return nil
}

func formatResetLabel(sched quotapace.Schedule) string {
	switch {
	case sched.Reset == "":
		return "reading reset"
	case sched.TZ != "":
		return sched.Reset + " " + sched.TZ
	default:
		return sched.Reset + " UTC"
	}
}

func flagSet(fs *flag.FlagSet, name string) bool {
	set := false
	fs.Visit(func(f *flag.Flag) { set = set || f.Name == name })
	return set
}

type viewFlags struct {
	profile, window *string
	recent, stale   *string
	asJSON          *bool
}

func addViewFlags(fs *flag.FlagSet, withWindow bool) viewFlags {
	v := viewFlags{
		profile: fs.String("profile", "", "only this profile (default every profile)"),
		recent:  fs.String("recent", "6h", "span for the recent pace"),
		stale:   fs.String("stale", "6h", "a reading older than this is missing"),
	}
	if withWindow {
		v.window = fs.String("window", "", "only this window")
	}
	return v
}

func (v viewFlags) snapshot(store quotapace.Store, now time.Time) (quotapace.Snapshot, error) {
	recent, err := quotapace.ParseDuration(*v.recent)
	if err != nil {
		return quotapace.Snapshot{}, usageErr("--recent: %v", err)
	}
	stale, err := quotapace.ParseDuration(*v.stale)
	if err != nil {
		return quotapace.Snapshot{}, usageErr("--stale: %v", err)
	}
	profiles := []string{*v.profile}
	if *v.profile == "" {
		if profiles, err = store.Profiles(); err != nil {
			return quotapace.Snapshot{}, fmt.Errorf("list profiles: %w", err)
		}
	} else if err := quotapace.CheckProfileName(*v.profile); err != nil {
		return quotapace.Snapshot{}, usageErr("%v", err)
	}
	snap, err := store.Status(profiles, now, quotapace.StatusOptions{Recent: recent, Stale: stale})
	if err != nil {
		return snap, fmt.Errorf("status: %w", err)
	}
	if v.window != nil && *v.window != "" {
		for i := range snap.Profiles {
			var keep []quotapace.WindowStatus
			for _, w := range snap.Profiles[i].Windows {
				if w.Window == *v.window {
					keep = append(keep, w)
				}
			}
			snap.Profiles[i].Windows = keep
		}
	}
	return snap, nil
}

// runStatus prints status lines, or the report, or the snapshot JSON,
// and exits with the worst state.
func runStatus(args []string, store quotapace.Store, e env, report bool) (int, error) {
	name := "status"
	if report {
		name = "report"
	}
	fs := newFlags(name, e)
	v := addViewFlags(fs, report)
	v.asJSON = fs.Bool("json", false, "print the status JSON")
	if err := parse(fs, args); err != nil {
		return 0, err
	}
	now := e.now()
	snap, err := v.snapshot(store, now)
	if err != nil {
		return 0, err
	}
	switch {
	case *v.asJSON:
		data, err := json.MarshalIndent(snap, "", "  ")
		if err != nil {
			return 0, fmt.Errorf("encode status: %w", err)
		}
		fmt.Fprintln(e.stdout, string(data))
	case report:
		for i, ws := range snap.Windows() {
			md, err := ws.Report()
			if err != nil {
				return 0, fmt.Errorf("report %s %s: %w", ws.Profile, ws.Window, err)
			}
			if i > 0 {
				fmt.Fprintln(e.stdout)
			}
			fmt.Fprint(e.stdout, md)
		}
	default:
		for _, ws := range snap.Windows() {
			line := ws.StatusLine(now)
			if *v.profile == "" {
				line = ws.Profile + " " + line
			}
			fmt.Fprintln(e.stdout, line)
		}
	}
	for _, ws := range snap.Windows() {
		for _, w := range ws.Warnings {
			fmt.Fprintf(e.stderr, "warning: %s %s: %s\n", ws.Profile, ws.Window, w)
		}
	}
	return snap.WorstCode(), nil
}

// runCheck is silent when every window is green. Otherwise it prints one
// line per problem and runs the notify command once with them, but only
// when a state changed or the repeat interval passed.
func runCheck(args []string, store quotapace.Store, e env) (int, error) {
	fs := newFlags("check", e)
	v := addViewFlags(fs, false)
	repeat := fs.String("repeat", "6h", "notify an unchanged problem again after this long; 0 never")
	notifyCmd := fs.String("notify-cmd", "", "command run with the problems on stdin; split on spaces, no shell (default $QUOTAPACE_NOTIFY_CMD)")
	if err := parse(fs, args); err != nil {
		return 0, err
	}
	var every time.Duration
	if *repeat != "0" {
		d, err := quotapace.ParseDuration(*repeat)
		if err != nil {
			return 0, usageErr("--repeat: %v", err)
		}
		every = d
	}
	now := e.now()
	snap, err := v.snapshot(store, now)
	if err != nil {
		return 0, err
	}
	sent, err := store.LoadSent()
	if err != nil {
		return 0, fmt.Errorf("check: %w", err)
	}
	res := quotapace.Check(snap, sent, now, every)
	if !res.Notify {
		return res.Code, saveSent(store, res.Next)
	}
	text := strings.Join(res.Problems, "\n") + "\n"
	fmt.Fprint(e.stdout, text)
	cmd := *notifyCmd
	if cmd == "" {
		cmd = e.getenv("QUOTAPACE_NOTIFY_CMD")
	}
	if argv := strings.Fields(cmd); len(argv) > 0 {
		if err := e.notify(argv, text); err != nil {
			return 0, err
		}
	}
	return res.Code, saveSent(store, res.Next)
}

func saveSent(store quotapace.Store, sent map[string]quotapace.Sent) error {
	if err := store.SaveSent(sent); err != nil {
		return fmt.Errorf("check: %w", err)
	}
	return nil
}
