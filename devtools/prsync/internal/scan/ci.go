package scan

import "github.com/jaeyeom/experimental/devtools/prsync/internal/gh"

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
