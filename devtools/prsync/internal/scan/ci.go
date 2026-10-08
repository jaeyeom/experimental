package scan

import (
	"path"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

// CIState classifies statusCheckRollup into green | failing | pending | none.
// Commit-status states map FAILURE and ERROR to failing, PENDING and EXPECTED
// to pending, and SUCCESS to completed green.
func CIState(checks []gh.StatusCheck) string {
	if len(checks) == 0 {
		return "none"
	}
	hasPending := false
	for _, check := range checks {
		switch check.State {
		case "FAILURE", "ERROR":
			return "failing"
		case "PENDING", "EXPECTED":
			hasPending = true
			continue
		case "SUCCESS":
			continue
		}
		switch check.Conclusion {
		case "FAILURE", "CANCELLED", "ACTION_REQUIRED", "TIMED_OUT":
			return "failing"
		}
		if check.Status != "COMPLETED" {
			hasPending = true
		}
	}
	if hasPending {
		return "pending"
	}
	return "green"
}

// selectCIChecks returns the rollup entries that feed CIState.
// A non-empty only list wins: a check is kept when its CheckRun name or
// StatusContext context matches one of those globs. Otherwise a check whose
// name or context matches ignore is dropped. Both empty returns checks.
// Globs are path.Match patterns: '*' does not cross '/'.
func selectCIChecks(checks []gh.StatusCheck, ignore, only []string) []gh.StatusCheck {
	if len(only) == 0 && len(ignore) == 0 {
		return checks
	}
	out := make([]gh.StatusCheck, 0, len(checks))
	for _, check := range checks {
		if len(only) > 0 {
			if checkMatches(check, only) {
				out = append(out, check)
			}
			continue
		}
		if !checkMatches(check, ignore) {
			out = append(out, check)
		}
	}
	return out
}

func checkMatches(check gh.StatusCheck, patterns []string) bool {
	if check.Name != "" && globAny(patterns, check.Name) {
		return true
	}
	return check.Context != "" && check.Context != check.Name && globAny(patterns, check.Context)
}

func globAny(patterns []string, name string) bool {
	for _, pattern := range patterns {
		ok, err := path.Match(pattern, name)
		if err == nil && ok {
			return true
		}
	}
	return false
}
