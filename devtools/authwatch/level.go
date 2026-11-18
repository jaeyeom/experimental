package authwatch

import "time"

// Level represents the health status of a profile's OAuth login.
type Level string

const (
	// LevelOK indicates valid credentials outside warning thresholds.
	LevelOK Level = "ok"
	// LevelWarn indicates the refresh token expires within warn_within.
	LevelWarn Level = "warn"
	// LevelCritical indicates the refresh token expires within critical_within.
	LevelCritical Level = "critical"
	// LevelFailing indicates the access token is past expiry beyond failing_after
	// without a refresh, or a CLI authentication failure hook was reported.
	LevelFailing Level = "failing"
	// LevelLoggedOut indicates no credentials were found or CLI status reported logged out.
	LevelLoggedOut Level = "logged-out"
	// LevelUnknownFormat indicates the credentials store schema changed or is missing expected fields.
	LevelUnknownFormat Level = "unknown-format"
)

// ExitCode returns the CLI exit code for this level.
func (l Level) ExitCode() int {
	switch l {
	case LevelOK:
		return 0
	case LevelWarn:
		return 1
	case LevelCritical:
		return 2
	case LevelFailing, LevelLoggedOut:
		return 3
	case LevelUnknownFormat:
		return 4
	default:
		return 5
	}
}

// Severity ranks levels so multi-profile commands can report the worst level.
func (l Level) Severity() int {
	switch l {
	case LevelOK:
		return 0
	case LevelWarn:
		return 1
	case LevelCritical:
		return 2
	case LevelLoggedOut:
		return 3
	case LevelFailing:
		return 4
	case LevelUnknownFormat:
		return 5
	default:
		return 6
	}
}

// EvaluateLevel determines the level from token expiries, thresholds, and any
// hook-reported failure that has not yet been superseded by a newer access token.
func EvaluateLevel(exp Expiries, now time.Time, cfg Config, reportedFailureAt, reportedAccessExpiry time.Time) Level {
	if now.Sub(exp.AccessExpiresAt) >= cfg.FailingAfter {
		return LevelFailing
	}
	if !reportedFailureAt.IsZero() && exp.AccessExpiresAt.Equal(reportedAccessExpiry) {
		return LevelFailing
	}
	rem := exp.RefreshExpiresAt.Sub(now)
	if rem <= cfg.CriticalWithin {
		return LevelCritical
	}
	if rem <= cfg.WarnWithin {
		return LevelWarn
	}
	return LevelOK
}
