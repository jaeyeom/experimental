package authwatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"
)

// Input bundles dependencies and state for Check, Status, and ReportFailure.
type Input struct {
	Config    Config
	State     State
	Now       time.Time
	BaseEnv   []string
	ReadStore StoreReader
	CLIStatus CLIStatusChecker
	Notify    NotifyRunner
}

// ProfileReport is the result of checking one profile.
type ProfileReport struct {
	Profile          string    `json:"profile"`
	Level            Level     `json:"level"`
	Since            time.Time `json:"since,omitempty"`
	AccessExpiresAt  time.Time `json:"accessExpiresAt,omitempty"`
	RefreshExpiresAt time.Time `json:"refreshExpiresAt,omitempty"`
	AccessTimeLeft   string    `json:"accessTimeLeft,omitempty"`
	RefreshTimeLeft  string    `json:"refreshTimeLeft,omitempty"`
	Notified         bool      `json:"notified,omitempty"`
	NotifyError      string    `json:"notifyError,omitempty"`
	FormatError      string    `json:"formatError,omitempty"`
	ReadError        string    `json:"readError,omitempty"`
}

// Report is the result of checking all configured profiles.
type Report struct {
	Profiles []ProfileReport `json:"profiles"`
}

// WorstExitCode returns the highest-severity exit code across all profiles,
// or 5 if any profile encountered a store read error.
func (r Report) WorstExitCode() int {
	worst := LevelOK
	hasReadErr := false
	for _, p := range r.Profiles {
		if p.ReadError != "" {
			hasReadErr = true
			continue
		}
		if p.Level.Severity() > worst.Severity() {
			worst = p.Level
		}
	}
	if hasReadErr && worst == LevelOK {
		return 5
	}
	if hasReadErr && worst.ExitCode() < 5 {
		return 5
	}
	return worst.ExitCode()
}

// Check evaluates every configured profile, updates state, and runs the notifier
// once per level change (including recovery to ok).
func Check(ctx context.Context, in Input) (Report, State) {
	state := in.State
	var report Report
	for _, profile := range in.Config.Profiles {
		pr, next := checkProfile(ctx, in, state, profile)
		state = next
		report.Profiles = append(report.Profiles, pr)
	}
	return report, state
}

func checkProfile(ctx context.Context, in Input, state State, profile ProfileConfig) (ProfileReport, State) {
	st := state.profile(profile.Name)
	st.LastCheck = in.Now
	pr := ProfileReport{Profile: profile.Name}

	raw, err := in.ReadStore(ctx, profile, in.BaseEnv)
	if err != nil {
		if errors.Is(err, ErrStoreNotFound) {
			return applyLevel(ctx, in, &state, profile.Name, st, LevelLoggedOut, Expiries{}, "", pr)
		}
		pr.ReadError = err.Error()
		state.put(profile.Name, st)
		return pr, state
	}

	exp, err := ParseCredentialsMetadata(raw)
	if err != nil {
		var fmtErr *FormatError
		msg := err.Error()
		if errors.As(err, &fmtErr) {
			msg = fmtErr.Error()
		}
		return applyLevel(ctx, in, &state, profile.Name, st, LevelUnknownFormat, Expiries{}, msg, pr)
	}

	if in.Config.UseCLIStatus && in.CLIStatus != nil {
		cfgDir, dirErr := ResolveConfigDir(profile.ConfigDir, in.BaseEnv)
		if dirErr == nil {
			loggedIn, cliErr := in.CLIStatus(ctx, in.Config.CLICommand, cfgDir, in.BaseEnv)
			if cliErr == nil && !loggedIn {
				return applyLevel(ctx, in, &state, profile.Name, st, LevelLoggedOut, exp, "", pr)
			}
		}
	}

	// Clear reported failure once a newer access token appears.
	if !st.ReportedFailureAt.IsZero() && !exp.AccessExpiresAt.Equal(st.ReportedAccessExpiry) {
		st.ReportedFailureAt = time.Time{}
		st.ReportedAccessExpiry = time.Time{}
	}

	level := EvaluateLevel(exp, in.Now, in.Config, st.ReportedFailureAt, st.ReportedAccessExpiry)
	return applyLevel(ctx, in, &state, profile.Name, st, level, exp, "", pr)
}

func applyLevel(
	ctx context.Context,
	in Input,
	state *State,
	name string,
	st ProfileState,
	level Level,
	exp Expiries,
	formatErr string,
	pr ProfileReport,
) (ProfileReport, State) {
	prevLevel := st.Level
	if prevLevel != level || st.Since.IsZero() {
		st.Since = in.Now
	}
	st.Level = level
	st.AccessExpiresAt = exp.AccessExpiresAt
	st.RefreshExpiresAt = exp.RefreshExpiresAt
	st.FormatError = formatErr

	pr.Level = level
	pr.Since = st.Since
	pr.AccessExpiresAt = exp.AccessExpiresAt
	pr.RefreshExpiresAt = exp.RefreshExpiresAt
	pr.FormatError = formatErr
	if !exp.AccessExpiresAt.IsZero() {
		pr.AccessTimeLeft = formatDuration(exp.AccessExpiresAt.Sub(in.Now))
	}
	if !exp.RefreshExpiresAt.IsZero() {
		pr.RefreshTimeLeft = formatDuration(exp.RefreshExpiresAt.Sub(in.Now))
	}

	if shouldNotify(prevLevel, level, st.LastNotifiedLevel) && len(in.Config.Notify) > 0 && in.Notify != nil {
		info := NotifyInfo{
			Profile:  name,
			Level:    level,
			TimeLeft: notifyTimeLeft(level, exp, in.Now),
			Since:    st.Since,
		}
		argv, env := BuildNotifyCommand(in.Config.Notify, in.BaseEnv, info)
		if err := in.Notify(ctx, argv, env); err != nil {
			st.LastNotifyError = err.Error()
			pr.NotifyError = err.Error()
		} else {
			st.LastNotifiedLevel = level
			st.LastNotifiedAt = in.Now
			st.LastNotifyError = ""
			pr.Notified = true
		}
	}

	state.put(name, st)
	return pr, *state
}

func shouldNotify(prevLevel, newLevel, lastNotified Level) bool {
	if prevLevel == "" {
		// Baseline ok on first run does not alert; non-ok alerts once.
		return newLevel != LevelOK
	}
	if prevLevel != newLevel {
		return true
	}
	// Retry if a previous notification attempt failed for this level.
	return lastNotified != newLevel
}

// ReportFailure records failing for profileName with the current time and
// notifies if the level changed.
func ReportFailure(ctx context.Context, in Input, profileName string) (ProfileReport, State, error) {
	profile, ok := findProfile(in.Config.Profiles, profileName)
	if !ok {
		return ProfileReport{}, in.State, fmt.Errorf("unknown profile %q", profileName)
	}
	state := in.State
	st := state.profile(profile.Name)
	st.LastCheck = in.Now
	st.ReportedFailureAt = in.Now

	var exp Expiries
	if in.ReadStore != nil {
		if raw, err := in.ReadStore(ctx, profile, in.BaseEnv); err == nil {
			if parsed, parseErr := ParseCredentialsMetadata(raw); parseErr == nil {
				exp = parsed
				st.ReportedAccessExpiry = parsed.AccessExpiresAt
			}
		}
	}

	pr := ProfileReport{Profile: profile.Name}
	out, next := applyLevel(ctx, in, &state, profile.Name, st, LevelFailing, exp, "", pr)
	return out, next, nil
}

func findProfile(profiles []ProfileConfig, name string) (ProfileConfig, bool) {
	if name == "" && len(profiles) == 1 {
		return profiles[0], true
	}
	for _, p := range profiles {
		if p.Name == name {
			return p, true
		}
	}
	return ProfileConfig{}, false
}

// Status reads current credentials and returns status without modifying state
// or running notifiers.
func Status(ctx context.Context, in Input, profileFilter string) (Report, error) {
	var profiles []ProfileConfig
	if profileFilter != "" {
		p, ok := findProfile(in.Config.Profiles, profileFilter)
		if !ok {
			return Report{}, fmt.Errorf("unknown profile %q", profileFilter)
		}
		profiles = []ProfileConfig{p}
	} else {
		profiles = in.Config.Profiles
	}
	noNotify := in
	noNotify.Config.Notify = nil
	noNotify.Notify = nil
	noNotify.Config.Profiles = profiles
	rep, _ := Check(ctx, noNotify)
	return rep, nil
}

// FormatStatus renders a Report as text, JSON, or a single status-bar line.
func FormatStatus(rep Report, asJSON, oneLine bool) (string, error) {
	if asJSON {
		body, err := json.MarshalIndent(rep, "", "  ")
		if err != nil {
			return "", fmt.Errorf("encode status: %w", err)
		}
		return string(body) + "\n", nil
	}
	if oneLine {
		parts := make([]string, 0, len(rep.Profiles))
		for _, p := range rep.Profiles {
			if p.ReadError != "" {
				parts = append(parts, fmt.Sprintf("%s:error", p.Profile))
				continue
			}
			if p.RefreshTimeLeft != "" && p.Level != LevelOK {
				parts = append(parts, fmt.Sprintf("%s:%s(%s)", p.Profile, p.Level, p.RefreshTimeLeft))
			} else {
				parts = append(parts, fmt.Sprintf("%s:%s", p.Profile, p.Level))
			}
		}
		return strings.Join(parts, " ") + "\n", nil
	}
	var b strings.Builder
	for _, p := range rep.Profiles {
		if p.ReadError != "" {
			fmt.Fprintf(&b, "%s error %s\n", p.Profile, p.ReadError)
			continue
		}
		if p.Level == LevelUnknownFormat {
			fmt.Fprintf(&b, "%s %s since %s (%s)\n", p.Profile, p.Level, p.Since.UTC().Format(time.RFC3339), p.FormatError)
			continue
		}
		if p.Level == LevelLoggedOut {
			fmt.Fprintf(&b, "%s %s since %s\n", p.Profile, p.Level, p.Since.UTC().Format(time.RFC3339))
			continue
		}
		fmt.Fprintf(&b, "%s %s access-left %s refresh-left %s since %s\n",
			p.Profile, p.Level, p.AccessTimeLeft, p.RefreshTimeLeft, p.Since.UTC().Format(time.RFC3339))
	}
	return b.String(), nil
}

func notifyTimeLeft(level Level, exp Expiries, now time.Time) string {
	switch level {
	case LevelFailing:
		if exp.AccessExpiresAt.IsZero() {
			return "0s"
		}
		return formatDuration(exp.AccessExpiresAt.Sub(now))
	case LevelLoggedOut, LevelUnknownFormat:
		return "0s"
	default:
		if exp.RefreshExpiresAt.IsZero() {
			return "0s"
		}
		return formatDuration(exp.RefreshExpiresAt.Sub(now))
	}
}

func formatDuration(d time.Duration) string {
	return d.Round(time.Second).String()
}
