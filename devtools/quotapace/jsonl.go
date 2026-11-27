package quotapace

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"time"
)

// ErrInvalidInput marks malformed or incomplete caller-supplied JSONL lines.
var ErrInvalidInput = errors.New("invalid input")

// stdinReading is one JSON line. Profile, at, and source default to the
// caller-provided defaults; resetsIn is an alternative to resetsAt.
type stdinReading struct {
	Reading
	ResetsIn string `json:"resetsIn"`
}

// ParseReadingsJSONL parses newline-delimited JSON readings, filling missing
// profile, source, and at fields from the supplied defaults.
func ParseReadingsJSONL(r io.Reader, profile, source string, now time.Time) ([]Reading, error) {
	var out []Reading
	sc := bufio.NewScanner(r)
	for line := 1; sc.Scan(); line++ {
		if strings.TrimSpace(sc.Text()) == "" {
			continue
		}
		var in stdinReading
		if err := json.Unmarshal(sc.Bytes(), &in); err != nil {
			return nil, fmt.Errorf("%w: stdin line %d: %w", ErrInvalidInput, line, err)
		}
		rd := in.Reading
		if rd.Profile == "" {
			rd.Profile = profile
		}
		if rd.At.IsZero() {
			rd.At = now
		}
		if rd.Source == "" {
			rd.Source = source
		}
		if in.ResetsIn != "" && rd.ResetsAt.IsZero() {
			d, err := ParseDuration(in.ResetsIn)
			if err != nil {
				return nil, fmt.Errorf("%w: stdin line %d: resetsIn: %w", ErrInvalidInput, line, err)
			}
			rd.ResetsAt = rd.At.Add(d)
		}
		if rd.Profile == "" || rd.Window == "" || rd.ResetsAt.IsZero() {
			return nil, fmt.Errorf("%w: stdin line %d: needs profile, window, and resetsAt or resetsIn", ErrInvalidInput, line)
		}
		out = append(out, rd)
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("read stdin: %w", err)
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("%w: record: no --percent and no reading on stdin", ErrInvalidInput)
	}
	return out, nil
}
