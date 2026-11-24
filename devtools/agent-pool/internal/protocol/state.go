// Package protocol defines shared wire DTOs, job state machine rules, and API
// error codes for the agent-pool broker and CLI.
package protocol

// JobStatus represents the lifecycle state of a pooled job.
type JobStatus string

const (
	// JobStatusQueued indicates the job is waiting for a worker lease.
	JobStatusQueued JobStatus = "QUEUED"
	// JobStatusLeased indicates a worker has acquired a lease on the job.
	JobStatusLeased JobStatus = "LEASED"
	// JobStatusRunning indicates the worker is actively executing the job.
	JobStatusRunning JobStatus = "RUNNING"
	// JobStatusSucceeded indicates the job completed successfully.
	JobStatusSucceeded JobStatus = "SUCCEEDED"
	// JobStatusFailed indicates the job failed during execution.
	JobStatusFailed JobStatus = "FAILED"
	// JobStatusBudgetExceeded indicates the job exceeded its token or unit budget.
	JobStatusBudgetExceeded JobStatus = "BUDGET_EXCEEDED"
	// JobStatusDLPBlocked indicates the job output was blocked by DLP checks.
	JobStatusDLPBlocked JobStatus = "DLP_BLOCKED"
	// JobStatusTimedOut indicates the job exceeded its execution time limit.
	JobStatusTimedOut JobStatus = "TIMED_OUT"
	// JobStatusCanceled indicates the job was canceled before completion.
	JobStatusCanceled JobStatus = "CANCELED"
)

// IsValid reports whether s is a recognized JobStatus value.
func (s JobStatus) IsValid() bool {
	switch s {
	case JobStatusQueued,
		JobStatusLeased,
		JobStatusRunning,
		JobStatusSucceeded,
		JobStatusFailed,
		JobStatusBudgetExceeded,
		JobStatusDLPBlocked,
		JobStatusTimedOut,
		JobStatusCanceled:
		return true
	default:
		return false
	}
}

// IsTerminal reports whether s is a final state with no outgoing transitions.
func (s JobStatus) IsTerminal() bool {
	switch s {
	case JobStatusSucceeded,
		JobStatusFailed,
		JobStatusBudgetExceeded,
		JobStatusDLPBlocked,
		JobStatusTimedOut,
		JobStatusCanceled:
		return true
	default:
		return false
	}
}

// CanTransition reports whether a job may move from state from to state to.
func CanTransition(from, to JobStatus) bool {
	switch from {
	case JobStatusQueued:
		return to == JobStatusLeased || to == JobStatusCanceled
	case JobStatusLeased:
		return to == JobStatusRunning || to == JobStatusQueued || to.IsTerminal()
	case JobStatusRunning:
		return to == JobStatusQueued || to.IsTerminal()
	default:
		return false
	}
}
