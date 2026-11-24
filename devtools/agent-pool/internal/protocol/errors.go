package protocol

import "fmt"

// ErrorCode is a machine-readable error identifier returned by the broker API.
type ErrorCode string

const (
	// ErrCodeInvalidArgs indicates invalid request arguments.
	ErrCodeInvalidArgs ErrorCode = "INVALID_ARGS"
	// ErrCodeUnknownRepo indicates the requested repository is not configured.
	ErrCodeUnknownRepo ErrorCode = "UNKNOWN_REPO"
	// ErrCodeUnknownSkill indicates the requested skill is not in the catalog.
	ErrCodeUnknownSkill ErrorCode = "UNKNOWN_SKILL"
	// ErrCodeCatalogMismatch indicates the worker catalog SHA does not match.
	ErrCodeCatalogMismatch ErrorCode = "CATALOG_MISMATCH"
	// ErrCodeNoCapacity indicates no eligible worker has available capacity.
	ErrCodeNoCapacity ErrorCode = "NO_CAPACITY"
	// ErrCodeRateLimited indicates the caller exceeded rate limits.
	ErrCodeRateLimited ErrorCode = "RATE_LIMITED"
	// ErrCodeUnauthorized indicates missing or invalid authentication credentials.
	ErrCodeUnauthorized ErrorCode = "UNAUTHORIZED"
	// ErrCodeForbidden indicates the caller is not authorized for the operation.
	ErrCodeForbidden ErrorCode = "FORBIDDEN"
	// ErrCodeNotFound indicates the requested resource does not exist.
	ErrCodeNotFound ErrorCode = "NOT_FOUND"
	// ErrCodeLeaseExpired indicates the job lease is no longer valid.
	ErrCodeLeaseExpired ErrorCode = "LEASE_EXPIRED"
	// ErrCodeInvalidTransition indicates an illegal job state transition.
	ErrCodeInvalidTransition ErrorCode = "INVALID_TRANSITION"
	// ErrCodeDLPBlocked indicates content was rejected by DLP checks.
	ErrCodeDLPBlocked ErrorCode = "DLP_BLOCKED"
	// ErrCodeOutputInvalid indicates the job deliverable failed schema validation.
	ErrCodeOutputInvalid ErrorCode = "OUTPUT_INVALID"
	// ErrCodeInternal indicates an unexpected internal server error.
	ErrCodeInternal ErrorCode = "INTERNAL"
)

// APIError is the structured error payload returned by broker HTTP endpoints.
type APIError struct {
	Code    ErrorCode `json:"code"`
	Message string    `json:"message"`
}

// Error implements the error interface.
func (e *APIError) Error() string {
	if e == nil {
		return ""
	}
	if e.Code == "" {
		return e.Message
	}
	if e.Message == "" {
		return string(e.Code)
	}
	return fmt.Sprintf("%s: %s", e.Code, e.Message)
}
