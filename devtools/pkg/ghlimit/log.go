package ghlimit

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"time"
)

// Entry is one log line for a call that waited.
type Entry struct {
	Time time.Time `json:"time"`
	PID  int       `json:"pid"`
	// Caller is the name of the process that ran gh.
	Caller string `json:"caller"`
	// Args are the gh arguments with values redacted.
	Args string `json:"args"`
	// Reason names the waits, joined by "+".
	Reason string `json:"reason"`
	WaitMS int64  `json:"waitMs"`
}

// AppendLog appends e as one JSON line to path, creating its directory.
func AppendLog(path string, e Entry) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create log dir: %w", err)
	}
	line, err := json.Marshal(e)
	if err != nil {
		return fmt.Errorf("encode log entry: %w", err)
	}
	return appendLine(path, append(line, '\n'))
}
