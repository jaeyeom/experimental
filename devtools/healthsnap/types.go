// Package healthsnap runs config-driven health checks, persists snapshots,
// and emits deduplicated change alerts with optional judgment routing.
package healthsnap

import "time"

// Severity represents the health state of a check or rule.
type Severity string

const (
	// SeverityOK indicates a passing check or cleared rule.
	SeverityOK Severity = "ok"
	// SeverityWarn indicates a warning threshold was crossed.
	SeverityWarn Severity = "warn"
	// SeverityFail indicates a failure threshold or execution error occurred.
	SeverityFail Severity = "fail"
)

// Rank returns numeric severity ordering (ok=0, warn=1, fail=2).
func (s Severity) Rank() int {
	switch s {
	case SeverityWarn:
		return 1
	case SeverityFail:
		return 2
	default:
		return 0
	}
}

// MaxSeverity returns the more severe of a and b.
func MaxSeverity(a, b Severity) Severity {
	if b.Rank() > a.Rank() {
		return b
	}
	return a
}

// CheckKind identifies how a check is executed.
type CheckKind string

const (
	// KindCommand runs an external command directly via argv without a shell.
	KindCommand CheckKind = "command"
	// KindFileAge checks the modification age of a file on disk.
	KindFileAge CheckKind = "file-age"
	// KindHost checks local host load average, memory pressure, or free disk.
	KindHost CheckKind = "host"
)

// ParseMode identifies how a command check's output is interpreted.
type ParseMode string

const (
	// ParseExit reads the command's exit status code.
	ParseExit ParseMode = "exit"
	// ParseJSONPath extracts a value from JSON stdout using a JSONPath expression.
	ParseJSONPath ParseMode = "jsonpath"
	// ParseRegex extracts a captured value or line match count from stdout via RE2.
	ParseRegex ParseMode = "regex"
)

// EventKind identifies a notification transition.
type EventKind string

const (
	// EventFire indicates a rule transitioned from ok to warn or fail.
	EventFire EventKind = "fire"
	// EventClear indicates a rule transitioned from warn or fail back to ok.
	EventClear EventKind = "clear"
	// EventRepeat indicates a still-firing rule passed its repeat interval.
	EventRepeat EventKind = "repeat"
)

// RuleSnapshot records the evaluation outcome of a single rule in one snapshot.
type RuleSnapshot struct {
	ID       string   `json:"id"`
	State    Severity `json:"state"`
	Judgment bool     `json:"judgment,omitempty"`
	Message  string   `json:"message,omitempty"`
}

// CheckSnapshot records the outcome of a single check in one snapshot.
type CheckSnapshot struct {
	ID         string         `json:"id"`
	Kind       CheckKind      `json:"kind"`
	State      Severity       `json:"state"`
	Value      any            `json:"value,omitempty"`
	Display    string         `json:"display,omitempty"`
	DurationMs int64          `json:"durationMs"`
	Error      string         `json:"error,omitempty"`
	Cached     bool           `json:"cached,omitempty"`
	Rules      []RuleSnapshot `json:"rules,omitempty"`
}

// AlertRecord records an alert emission (or attempt) during a run.
type AlertRecord struct {
	RuleID      string    `json:"ruleId"`
	CheckID     string    `json:"checkId"`
	Event       EventKind `json:"event"`
	Previous    Severity  `json:"previous"`
	State       Severity  `json:"state"`
	Value       any       `json:"value,omitempty"`
	Judgment    bool      `json:"judgment,omitempty"`
	Recipient   string    `json:"recipient,omitempty"`
	Message     string    `json:"message"`
	Error       string    `json:"error,omitempty"`
	Sent        bool      `json:"sent"`
	DryRun      bool      `json:"dryRun,omitempty"`
	NotifyError string    `json:"notifyError,omitempty"`
}

// Snapshot is the persisted result of a single healthsnap run pass.
type Snapshot struct {
	Timestamp time.Time       `json:"timestamp"`
	State     Severity        `json:"state"`
	Checks    []CheckSnapshot `json:"checks"`
	Alerts    []AlertRecord   `json:"alerts,omitempty"`
}

// RuleState persists the latest known state and alert timing for one rule.
type RuleState struct {
	State        Severity  `json:"state"`
	LastChanged  time.Time `json:"lastChanged,omitempty"`
	LastNotified time.Time `json:"lastNotified,omitempty"`
	LastValue    any       `json:"lastValue,omitempty"`
}

// CheckState persists per-check last run details for interval scheduling and
// execution-failure alert deduplication.
type CheckState struct {
	LastRun          time.Time `json:"lastRun,omitempty"`
	State            Severity  `json:"state"`
	Value            any       `json:"value,omitempty"`
	Display          string    `json:"display,omitempty"`
	Error            string    `json:"error,omitempty"`
	DurationMs       int64     `json:"durationMs,omitempty"`
	ExecError        bool      `json:"execError,omitempty"`
	ExecLastChanged  time.Time `json:"execLastChanged,omitempty"`
	ExecLastNotified time.Time `json:"execLastNotified,omitempty"`
}

// State is the persisted deduplication and interval state in state.json.
type State struct {
	Rules  map[string]RuleState  `json:"rules"`
	Checks map[string]CheckState `json:"checks"`
}
