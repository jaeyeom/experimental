package secretwatch

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

// JournalKind identifies which systemd journal is being scanned.
type JournalKind string

const (
	// JournalUser selects the current user's journal (journalctl --user).
	JournalUser JournalKind = "user"
	// JournalSystem selects the system journal (journalctl --system).
	JournalSystem JournalKind = "system"
)

const (
	maxJournalLineBytes = 4 * 1024 * 1024
	maxStderrBytes      = 4096
)

// ErrJournalUnreadable reports that a journal could not be opened due to
// insufficient permissions.
var ErrJournalUnreadable = errors.New("journal unreadable")

// Query configures a journalctl invocation.
type Query struct {
	Kind        JournalKind
	AfterCursor string
	Since       string
	Until       string
	Unit        string
	Follow      bool
}

// Entry holds metadata and transient message text from one journalctl JSON
// record. Message must never be logged, printed, stored, or placed in errors.
type Entry struct {
	Cursor    string
	Timestamp time.Time
	Unit      string
	Process   string
	PID       string
	Message   string
}

// JournalStreamer streams entries matching q to emit, returning any tail
// cursor observed (for example from --show-cursor).
type JournalStreamer func(ctx context.Context, q Query, emit func(Entry) error) (tailCursor string, err error)

// FormatJournalTime formats t as a UTC systemd.time(7) timestamp.
func FormatJournalTime(t time.Time) string {
	return t.UTC().Format("2006-01-02 15:04:05 UTC")
}

func buildJournalctlArgs(q Query) []string {
	args := []string{"--output=json", "--no-pager", "--show-cursor"}
	if q.Kind == JournalUser {
		args = append(args, "--user")
	} else {
		args = append(args, "--system")
	}
	if q.AfterCursor != "" {
		args = append(args, "--after-cursor="+q.AfterCursor)
	} else if q.Since != "" {
		args = append(args, "--since="+q.Since)
	}
	if q.Until != "" {
		args = append(args, "--until="+q.Until)
	}
	if q.Unit != "" {
		args = append(args, "--unit="+q.Unit)
	}
	if q.Follow {
		args = append(args, "--follow")
	}
	return args
}

// ExecJournal invokes journalctl and streams parsed entries to emit.
func ExecJournal(ctx context.Context, q Query, emit func(Entry) error) (string, error) {
	args := buildJournalctlArgs(q)
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, "journalctl", args...) //nolint:gosec // G204: fixed binary with validated arguments
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return "", fmt.Errorf("journalctl stdout pipe: %w", err)
	}
	var stderr limitedBuffer
	stderr.limit = maxStderrBytes
	cmd.Stderr = &stderr

	if err := cmd.Start(); err != nil {
		return "", fmt.Errorf("start journalctl %s: %w", q.Kind, err)
	}

	entryCount := 0
	tailCursor, parseErr := parseJournalStream(q.Kind, stdout, func(e Entry) error {
		entryCount++
		return emit(e)
	})

	waitErr := cmd.Wait()
	if q.Follow && ctx.Err() != nil {
		return tailCursor, nil
	}
	if parseErr != nil {
		return tailCursor, parseErr
	}
	errText := strings.TrimSpace(stderr.String())
	if waitErr != nil {
		if isPermissionError(errText) {
			return "", fmt.Errorf("%w: %s", ErrJournalUnreadable, firstLine(errText))
		}
		if errText != "" {
			return "", fmt.Errorf("journalctl %s: %w: %s", q.Kind, waitErr, firstLine(errText))
		}
		return "", fmt.Errorf("journalctl %s: %w", q.Kind, waitErr)
	}
	if q.Kind == JournalSystem && entryCount == 0 && tailCursor == "" && isPermissionError(errText) {
		return "", fmt.Errorf("%w: %s", ErrJournalUnreadable, firstLine(errText))
	}
	return tailCursor, nil
}

func parseJournalStream(kind JournalKind, r io.Reader, emit func(Entry) error) (string, error) {
	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 0, 64*1024), maxJournalLineBytes)
	var tailCursor string
	lineNum := 0
	for scanner.Scan() {
		lineNum++
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		if strings.HasPrefix(line, "-- cursor:") {
			tailCursor = strings.TrimSpace(strings.TrimPrefix(line, "-- cursor:"))
			continue
		}
		if strings.HasPrefix(line, "-- ") {
			continue
		}
		entry, err := parseJournalRecord(kind, []byte(line))
		if err != nil {
			return tailCursor, fmt.Errorf("parse %s journal record %d: %w", kind, lineNum, err)
		}
		if entry.Cursor != "" {
			tailCursor = entry.Cursor
		}
		if err := emit(entry); err != nil {
			return tailCursor, err
		}
	}
	if err := scanner.Err(); err != nil {
		return tailCursor, fmt.Errorf("read %s journal stream: %w", kind, err)
	}
	return tailCursor, nil
}

func parseJournalRecord(kind JournalKind, data []byte) (Entry, error) {
	var fields map[string]json.RawMessage
	if err := json.Unmarshal(data, &fields); err != nil {
		// Return only a structural error message; never embed data in the error.
		return Entry{}, errors.New("invalid JSON object")
	}
	msg, err := decodeJournalMessage(fields["MESSAGE"])
	if err != nil {
		return Entry{}, err
	}
	return Entry{
		Cursor:    rawString(fields, "__CURSOR"),
		Timestamp: parseMicrosTimestamp(firstNonEmpty(rawString(fields, "__REALTIME_TIMESTAMP"), rawString(fields, "_SOURCE_REALTIME_TIMESTAMP"))),
		Unit:      resolveUnit(kind, fields),
		Process:   firstNonEmpty(rawString(fields, "_COMM"), rawString(fields, "SYSLOG_IDENTIFIER")),
		PID:       firstNonEmpty(rawString(fields, "_PID"), rawString(fields, "SYSLOG_PID")),
		Message:   msg,
	}, nil
}

func rawString(fields map[string]json.RawMessage, key string) string {
	raw := bytes.TrimSpace(fields[key])
	if len(raw) == 0 || bytes.Equal(raw, []byte("null")) {
		return ""
	}
	if raw[0] == '"' {
		var s string
		if err := json.Unmarshal(raw, &s); err == nil {
			return s
		}
	}
	return ""
}

func decodeJournalMessage(raw json.RawMessage) (string, error) {
	trimmed := bytes.TrimSpace(raw)
	if len(trimmed) == 0 || bytes.Equal(trimmed, []byte("null")) {
		return "", nil
	}
	if trimmed[0] == '"' {
		var s string
		if err := json.Unmarshal(trimmed, &s); err != nil {
			return "", errors.New("invalid string MESSAGE")
		}
		return s, nil
	}
	if trimmed[0] == '[' {
		var vals []int
		if err := json.Unmarshal(trimmed, &vals); err != nil {
			return "", errors.New("invalid byte-array MESSAGE")
		}
		buf := make([]byte, len(vals))
		for i, v := range vals {
			if v < 0 || v > 255 {
				return "", errors.New("out-of-range byte in MESSAGE")
			}
			buf[i] = byte(v)
		}
		return string(buf), nil
	}
	return "", errors.New("unsupported MESSAGE type")
}

func resolveUnit(kind JournalKind, fields map[string]json.RawMessage) string {
	if kind == JournalUser {
		return firstNonEmpty(
			rawString(fields, "_SYSTEMD_USER_UNIT"),
			rawString(fields, "USER_UNIT"),
			rawString(fields, "_SYSTEMD_UNIT"),
			rawString(fields, "UNIT"),
		)
	}
	return firstNonEmpty(
		rawString(fields, "_SYSTEMD_UNIT"),
		rawString(fields, "UNIT"),
		rawString(fields, "_SYSTEMD_USER_UNIT"),
		rawString(fields, "USER_UNIT"),
	)
}

func parseMicrosTimestamp(raw string) time.Time {
	if raw == "" {
		return time.Time{}
	}
	us, err := strconv.ParseInt(raw, 10, 64)
	if err != nil {
		return time.Time{}
	}
	return time.UnixMicro(us).UTC()
}

func firstNonEmpty(candidates ...string) string {
	for _, c := range candidates {
		if c != "" {
			return c
		}
	}
	return ""
}

func isPermissionError(stderr string) bool {
	lower := strings.ToLower(stderr)
	for _, marker := range []string{
		"insufficient permissions",
		"permission denied",
		"failed to open system journal",
		"not seeing messages from other users",
		"no journal files were opened",
	} {
		if strings.Contains(lower, marker) {
			return true
		}
	}
	return false
}

func firstLine(s string) string {
	if line, _, ok := strings.Cut(s, "\n"); ok {
		return strings.TrimSpace(line)
	}
	return strings.TrimSpace(s)
}

type limitedBuffer struct {
	buf   bytes.Buffer
	limit int
}

func (b *limitedBuffer) Write(p []byte) (int, error) {
	rem := b.limit - b.buf.Len()
	if rem > 0 {
		take := p
		if len(take) > rem {
			take = take[:rem]
		}
		_, _ = b.buf.Write(take)
	}
	return len(p), nil
}

func (b *limitedBuffer) String() string {
	return b.buf.String()
}
