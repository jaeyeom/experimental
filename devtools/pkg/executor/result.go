package executor

import (
	"encoding/json"
	"fmt"
	"time"
)

// ExecutionResult stores the result of executing a command.
type ExecutionResult struct {
	// Command is the full command that was executed
	Command string `json:"command"`

	// Args are the arguments passed to the command
	Args []string `json:"args"`

	// WorkingDir is the directory where the command was executed
	WorkingDir string `json:"workingDir"`

	// Output is the combined stdout output
	Output string `json:"output"`

	// Stderr is the stderr output
	Stderr string `json:"stderr"`

	// ExitCode is the exit code of the command
	ExitCode int `json:"exitCode"`

	// Error contains any error message if the execution failed
	Error string `json:"error,omitempty"`

	// StartTime is when the command execution started
	StartTime time.Time `json:"startTime"`

	// EndTime is when the command execution ended
	EndTime time.Time `json:"endTime"`

	// TimedOut indicates if the command was terminated due to timeout
	TimedOut bool `json:"timedOut,omitempty"`
}

// Duration calculates the execution time.
func (er *ExecutionResult) Duration() time.Duration {
	return er.EndTime.Sub(er.StartTime)
}

// Validate ensures the ExecutionResult has valid data.
func (er *ExecutionResult) Validate() error {
	if er.Command == "" {
		return fmt.Errorf("command cannot be empty")
	}

	if er.StartTime.IsZero() {
		return fmt.Errorf("startTime cannot be zero")
	}

	if er.EndTime.IsZero() {
		return fmt.Errorf("endTime cannot be zero")
	}

	if er.EndTime.Before(er.StartTime) {
		return fmt.Errorf("endTime cannot be before startTime")
	}

	return nil
}

// Custom JSON marshaling for time fields to ensure consistent format.
type executionResultJSON struct {
	Command    string   `json:"command"`
	Args       []string `json:"args"`
	WorkingDir string   `json:"workingDir"`
	Output     string   `json:"output"`
	Stderr     string   `json:"stderr"`
	ExitCode   int      `json:"exitCode"`
	Error      string   `json:"error,omitempty"`
	StartTime  string   `json:"startTime"`
	EndTime    string   `json:"endTime"`
	Duration   string   `json:"duration"`
	TimedOut   bool     `json:"timedOut,omitempty"`
}

// MarshalJSON implements custom JSON marshaling for ExecutionResult.
func (er ExecutionResult) MarshalJSON() ([]byte, error) {
	data, err := json.Marshal(executionResultJSON{
		Command:    er.Command,
		Args:       er.Args,
		WorkingDir: er.WorkingDir,
		Output:     er.Output,
		Stderr:     er.Stderr,
		ExitCode:   er.ExitCode,
		Error:      er.Error,
		StartTime:  er.StartTime.Format(time.RFC3339Nano),
		EndTime:    er.EndTime.Format(time.RFC3339Nano),
		Duration:   er.Duration().String(),
		TimedOut:   er.TimedOut,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to marshal ExecutionResult: %w", err)
	}
	return data, nil
}

// UnmarshalJSON implements custom JSON unmarshaling for ExecutionResult.
func (er *ExecutionResult) UnmarshalJSON(data []byte) error {
	var aux executionResultJSON
	if err := json.Unmarshal(data, &aux); err != nil {
		return fmt.Errorf("failed to unmarshal ExecutionResult: %w", err)
	}

	startTime, err := time.Parse(time.RFC3339Nano, aux.StartTime)
	if err != nil {
		return fmt.Errorf("invalid startTime format: %w", err)
	}

	endTime, err := time.Parse(time.RFC3339Nano, aux.EndTime)
	if err != nil {
		return fmt.Errorf("invalid endTime format: %w", err)
	}

	er.Command = aux.Command
	er.Args = aux.Args
	er.WorkingDir = aux.WorkingDir
	er.Output = aux.Output
	er.Stderr = aux.Stderr
	er.ExitCode = aux.ExitCode
	er.Error = aux.Error
	er.StartTime = startTime
	er.EndTime = endTime
	er.TimedOut = aux.TimedOut

	return nil
}
