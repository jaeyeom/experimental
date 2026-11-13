package diskwatch

import (
	"context"
	"fmt"
	"time"
)

// Usage is free and total bytes on one filesystem.
type Usage struct {
	Free  uint64
	Total uint64
}

// StatFS reads free space for a mount path.
type StatFS func(path string) (Usage, error)

// Runner executes on_low. argv is not passed through a shell.
type Runner func(ctx context.Context, argv []string, env []string) error

// Report is one check pass.
type Report struct {
	Mounts  []MountReport
	Skipped bool
}

// MountReport is the outcome for one mount.
type MountReport struct {
	Path        string
	Free        uint64
	Total       uint64
	Triggered   bool
	Kind        string
	Summary     Summary
	ActionError string
	ReadError   string
	DryRun      bool
}

// Input is one check, status, or hook invocation.
type Input struct {
	Config         Config
	State          State
	Now            time.Time
	Stat           StatFS
	Lister         lister
	Run            Runner
	BaseEnv        []string
	Hostname       string
	SummaryDir     string
	Home           string
	DryRun         bool
	Hook           bool
	SummaryTimeout time.Duration
}

// Check evaluates every mount. A failed action is reported and is not
// stored as a trigger, so the next check retries it. Dry-run does not run
// the action and does not change cooldown state. Hook skips the pass when
// one already ran within the last minute.
func Check(ctx context.Context, in Input) (Report, State, error) {
	state := in.State
	if in.Hook {
		if !state.HookAt.IsZero() && in.Now.Sub(state.HookAt) < time.Minute {
			return Report{Skipped: true}, state, nil
		}
		state.HookAt = in.Now
	}
	timeout := in.SummaryTimeout
	if timeout <= 0 {
		timeout = DefaultSummaryTimeout
	}
	var report Report
	for _, mount := range in.Config.Mounts {
		outcome, next := applyMount(ctx, in, state, mount, timeout)
		state = next
		report.Mounts = append(report.Mounts, outcome)
	}
	return report, state, nil
}

func applyMount(ctx context.Context, in Input, state State, mount Mount, timeout time.Duration) (MountReport, State) {
	usage, err := in.Stat(mount.Path)
	st := state.mount(mount.Path)
	st.LastCheck = in.Now
	if err != nil {
		state.put(mount.Path, st)
		return MountReport{Path: mount.Path, ReadError: err.Error()}, state
	}
	st.LastFreeBytes = usage.Free
	st.LastTotalBytes = usage.Total
	st = rearm(usage.Free, usage.Total, mount, st)
	trigger, kind := decide(usage.Free, usage.Total, mount, st, in.Now)
	outcome := MountReport{Path: mount.Path, Free: usage.Free, Total: usage.Total, Kind: kind}
	if !trigger {
		state.put(mount.Path, st)
		return outcome, state
	}
	outcome.Summary = summarize(ctx, in.Config.ReportPaths, in.Home, in.Config.SummaryTop, in.Now.Add(timeout), time.Now, in.Lister)
	outcome.Triggered = true
	if in.DryRun {
		outcome.DryRun = true
		state.put(mount.Path, st)
		return outcome, state
	}
	if err := runTrigger(ctx, in, mount.Path, usage, outcome.Summary); err != nil {
		st.LastAction = "error"
		st.LastActionError = err.Error()
		outcome.ActionError = err.Error()
		outcome.Triggered = false
		state.put(mount.Path, st)
		return outcome, state
	}
	st.Armed = false
	st.TriggeredAt = in.Now
	st.LastAction = "ok"
	st.LastActionError = ""
	state.put(mount.Path, st)
	return outcome, state
}

func runTrigger(ctx context.Context, in Input, mount string, usage Usage, summary Summary) error {
	if in.Run == nil {
		return fmt.Errorf("on_low is not configured")
	}
	file, err := writeSummaryFile(in.SummaryDir, mount, summary)
	if err != nil {
		return err
	}
	env := actionEnv(in.BaseEnv, mount, usage.Free, usage.Total, in.Hostname, summary.Text(), file)
	if err := in.Run(ctx, in.Config.OnLow, env); err != nil {
		return err
	}
	return nil
}
