package healthsnap

import (
	"context"
	"fmt"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/notify"
)

// RunInput configures one healthsnap pass.
type RunInput struct {
	Config        Config
	StateDir      string
	BaseEnv       []string
	WriteMarkdown bool
	Force         bool
	DryRun        bool
	Deps          CheckDeps
	Notify        notify.Runner
}

// RunOutput summarizes one completed healthsnap pass.
type RunOutput struct {
	Snapshot      Snapshot
	SnapshotFiles SnapshotFiles
	State         State
}

// HasNotifyError reports whether any alert failed to deliver during the run.
func (o RunOutput) HasNotifyError() bool {
	for _, a := range o.Snapshot.Alerts {
		if a.NotifyError != "" {
			return true
		}
	}
	return false
}

// Run executes configured checks (honoring per-check intervals unless Force is
// set), evaluates threshold rules, sends deduplicated change alerts, and
// persists state and snapshot files unless DryRun is true.
func Run(ctx context.Context, in RunInput) (RunOutput, error) {
	cfg := in.Config
	if err := PrepareConfig(&cfg); err != nil {
		return RunOutput{}, err
	}
	deps := in.Deps.withDefaults()
	notifier := in.Notify
	if notifier == nil {
		notifier = notify.Exec
	}
	stateDir := resolveStateDir(in.StateDir, cfg.StateDir, in.BaseEnv)
	if stateDir == "" {
		return RunOutput{}, fmt.Errorf("resolve state directory: empty path")
	}

	state, err := LoadState(filepath.Join(stateDir, stateFileName))
	if err != nil {
		return RunOutput{}, err
	}

	now := deps.Now()
	snap := Snapshot{
		Timestamp: now.UTC(),
		State:     SeverityOK,
		Checks:    make([]CheckSnapshot, 0, len(cfg.Checks)),
	}
	rctx := runContext{
		ctx:      ctx,
		cfg:      cfg,
		in:       in,
		deps:     deps,
		notifier: notifier,
		now:      now,
		state:    &state,
	}

	for _, chk := range cfg.Checks {
		cs, alerts := rctx.processCheck(chk)
		snap.Checks = append(snap.Checks, cs)
		snap.State = MaxSeverity(snap.State, cs.State)
		snap.Alerts = append(snap.Alerts, alerts...)
	}

	out := RunOutput{Snapshot: snap, State: state}
	if in.DryRun {
		return out, nil
	}

	writeMD := cfg.Markdown || in.WriteMarkdown
	files, err := WriteSnapshot(stateDir, snap, writeMD, cfg.KeepSnapshots)
	if err != nil {
		return out, err
	}
	out.SnapshotFiles = files
	if err := SaveState(filepath.Join(stateDir, stateFileName), state); err != nil {
		return out, err
	}
	return out, nil
}

type runContext struct {
	ctx      context.Context
	cfg      Config
	in       RunInput
	deps     CheckDeps
	notifier notify.Runner
	now      time.Time
	state    *State
}

func (r *runContext) processCheck(chk CheckConfig) (CheckSnapshot, []AlertRecord) {
	prev := r.state.Checks[chk.ID]
	if shouldSkipByInterval(chk, prev, r.in.Force, r.now) {
		return CheckSnapshot{
			ID:         chk.ID,
			Kind:       chk.Kind,
			State:      prev.State,
			Value:      prev.Value,
			Display:    prev.Display,
			DurationMs: prev.DurationMs,
			Error:      prev.Error,
			Cached:     true,
		}, nil
	}

	eval := executeCheck(r.ctx, chk, r.in.BaseEnv, r.deps)
	if eval.execErr != "" {
		return r.handleExecError(chk, prev, eval.execErr, eval.durationMs)
	}

	var alerts []AlertRecord
	if prev.ExecError {
		a := r.emitExecClear(chk, prev)
		alerts = append(alerts, a)
		if a.NotifyError == "" {
			prev.ExecError = false
			prev.ExecLastChanged = r.now
		}
	}

	cs, ruleAlerts := r.evaluateRules(chk, eval)
	alerts = append(alerts, ruleAlerts...)

	prev.LastRun = r.now
	prev.State = cs.State
	prev.Value = cs.Value
	prev.Display = cs.Display
	prev.Error = ""
	prev.DurationMs = cs.DurationMs
	r.state.Checks[chk.ID] = prev

	return cs, alerts
}

func shouldSkipByInterval(chk CheckConfig, prev CheckState, force bool, now time.Time) bool {
	if force || chk.IntervalDuration() <= 0 || prev.LastRun.IsZero() {
		return false
	}
	return now.Sub(prev.LastRun) < chk.IntervalDuration()
}

func (r *runContext) handleExecError(chk CheckConfig, prev CheckState, errMsg string, durMs int64) (CheckSnapshot, []AlertRecord) {
	cs := CheckSnapshot{
		ID:         chk.ID,
		Kind:       chk.Kind,
		State:      SeverityFail,
		DurationMs: durMs,
		Error:      errMsg,
	}
	var alerts []AlertRecord
	if !prev.ExecError {
		a := AlertRecord{
			RuleID:   chk.ID + "/exec",
			CheckID:  chk.ID,
			Event:    EventFire,
			Previous: SeverityOK,
			State:    SeverityFail,
			Judgment: chk.Judgment,
			Error:    errMsg,
		}
		a.Message = formatAlertMessage(a, "check execution failed")
		r.dispatchAlert(&a, "", nil)
		alerts = append(alerts, a)
		if a.NotifyError == "" {
			prev.ExecError = true
			prev.ExecLastChanged = r.now
			prev.ExecLastNotified = r.now
		}
	}
	prev.LastRun = r.now
	prev.State = SeverityFail
	prev.Value = nil
	prev.Display = ""
	prev.Error = errMsg
	prev.DurationMs = durMs
	r.state.Checks[chk.ID] = prev
	return cs, alerts
}

func (r *runContext) emitExecClear(chk CheckConfig, prev CheckState) AlertRecord {
	a := AlertRecord{
		RuleID:   chk.ID + "/exec",
		CheckID:  chk.ID,
		Event:    EventClear,
		Previous: SeverityFail,
		State:    SeverityOK,
		Judgment: chk.Judgment,
	}
	a.Message = formatAlertMessage(a, "check execution recovered")
	r.dispatchAlert(&a, "", nil)
	return a
}

func (r *runContext) evaluateRules(chk CheckConfig, eval checkEvalResult) (CheckSnapshot, []AlertRecord) {
	cs := CheckSnapshot{
		ID:         chk.ID,
		Kind:       chk.Kind,
		State:      SeverityOK,
		Value:      eval.value,
		Display:    eval.display,
		DurationMs: eval.durationMs,
		Rules:      make([]RuleSnapshot, 0, len(chk.Rules)),
	}
	var alerts []AlertRecord
	for _, rule := range chk.Rules {
		curr, err := r.evalRuleState(rule, eval.vars)
		if err != nil {
			cs.State = SeverityFail
			if cs.Error == "" {
				cs.Error = err.Error()
			}
			rs := RuleSnapshot{ID: rule.ID, State: SeverityFail, Judgment: rule.Judgment, Message: err.Error()}
			cs.Rules = append(cs.Rules, rs)
			if a, ok := r.applyRuleTransition(chk.ID, rule, SeverityFail, nil, err.Error()); ok {
				alerts = append(alerts, a)
			}
			continue
		}
		cs.State = MaxSeverity(cs.State, curr)
		cs.Rules = append(cs.Rules, RuleSnapshot{
			ID:       rule.ID,
			State:    curr,
			Judgment: rule.Judgment,
			Message:  rule.Message,
		})
		if a, ok := r.applyRuleTransition(chk.ID, rule, curr, eval.value, ""); ok {
			alerts = append(alerts, a)
		}
	}
	return cs, alerts
}

// evalRuleState evaluates rule.Condition and, when hysteresis (rule.Clear) is
// configured and the rule is currently firing, keeps it firing unless
// condition is false and clear evaluates to true.
func (r *runContext) evalRuleState(rule RuleConfig, vars map[string]any) (Severity, error) {
	matched, err := rule.compiledExpr.Eval(vars)
	if err != nil {
		return SeverityFail, err
	}
	if matched {
		return rule.State, nil
	}
	prev := r.state.Rules[rule.ID]
	if prev.State != "" && prev.State != SeverityOK && rule.hasClear {
		cleared, err := rule.compiledClear.Eval(vars)
		if err != nil {
			return SeverityFail, err
		}
		if !cleared {
			return rule.State, nil
		}
	}
	return SeverityOK, nil
}

func (r *runContext) applyRuleTransition(checkID string, rule RuleConfig, curr Severity, value any, evalErr string) (AlertRecord, bool) {
	prev := r.state.Rules[rule.ID]
	if prev.State == "" {
		prev.State = SeverityOK
	}

	// Informational rules record transitions in state.json and snapshots but never notify.
	if rule.State == SeverityInfo && evalErr == "" {
		if curr != prev.State {
			prev.LastChanged = r.now
		}
		prev.State = curr
		prev.LastValue = value
		r.state.Rules[rule.ID] = prev
		return AlertRecord{}, false
	}

	event, emit := classifyTransition(prev, curr, rule.RepeatDuration(), r.now)
	if !emit {
		prev.LastValue = value
		r.state.Rules[rule.ID] = prev
		return AlertRecord{}, false
	}

	a := AlertRecord{
		RuleID:   rule.ID,
		CheckID:  checkID,
		Event:    event,
		Previous: prev.State,
		State:    curr,
		Value:    value,
		Judgment: rule.Judgment,
		Error:    evalErr,
	}
	a.Message = formatAlertMessage(a, rule.Message)
	r.dispatchAlert(&a, rule.Recipient, rule.Command)
	if a.NotifyError == "" {
		if event != EventRepeat {
			prev.LastChanged = r.now
		}
		prev.State = curr
		prev.LastNotified = r.now
	}
	prev.LastValue = value
	r.state.Rules[rule.ID] = prev
	return a, true
}

func classifyTransition(prev RuleState, curr Severity, repeat time.Duration, now time.Time) (EventKind, bool) {
	if prev.State == SeverityOK && curr != SeverityOK {
		return EventFire, true
	}
	if prev.State != SeverityOK && curr == SeverityOK {
		return EventClear, true
	}
	if prev.State != SeverityOK && curr != SeverityOK && repeat > 0 && !prev.LastNotified.IsZero() && now.Sub(prev.LastNotified) >= repeat {
		return EventRepeat, true
	}
	return "", false
}

func (r *runContext) dispatchAlert(a *AlertRecord, ruleRecipient string, ruleCommand []string) {
	tmpl, recipient, ok := resolveNotifyRoute(r.cfg.Notify, a.Judgment, ruleRecipient, ruleCommand)
	a.Recipient = recipient
	if !ok {
		return
	}
	if r.in.DryRun {
		a.DryRun = true
		return
	}
	argv, env := BuildNotifyCommand(tmpl, r.in.BaseEnv, recipient, *a)
	notifyCtx, cancel := context.WithTimeout(r.ctx, r.cfg.Notify.TimeoutDuration())
	defer cancel()
	if err := r.notifier(notifyCtx, argv, env); err != nil {
		a.NotifyError = err.Error()
		return
	}
	a.Sent = true
}

func resolveStateDir(override, cfgDir string, baseEnv []string) string {
	if override != "" {
		return ExpandHome(override, baseEnv)
	}
	if cfgDir != "" {
		return ExpandHome(cfgDir, baseEnv)
	}
	paths, err := LocatePaths(baseEnv)
	if err != nil {
		return ""
	}
	return paths.StateDir
}
