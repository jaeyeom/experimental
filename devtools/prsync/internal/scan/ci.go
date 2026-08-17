package scan

import "github.com/jaeyeom/experimental/devtools/prsync/internal/gh"

// CIState classifies statusCheckRollup into green | failing | pending | none.
func CIState(checks []gh.StatusCheck) string {
	if len(checks) == 0 {
		return "none"
	}
	hasPending := false
	for _, check := range checks {
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
