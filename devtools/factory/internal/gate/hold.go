package gate

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
)

// PrepareHold checks an owner and a reason before they are written.
// An empty owner and an empty reason are valid. A whitespace-only value is not.
// A reason line that starts with "owner:" is an error, because that line is the owner.
func PrepareHold(owner, reason string) (string, string, error) {
	if strings.ContainsAny(owner, "\r\n") {
		return "", "", errors.New("hold: expected a single-line owner, found a newline")
	}
	trimmedOwner := strings.TrimSpace(owner)
	if owner != "" && trimmedOwner == "" {
		return "", "", errors.New("hold: expected a non-empty owner, found empty")
	}
	normalized, err := normalizeReason(reason)
	if err != nil {
		return "", "", err
	}
	return trimmedOwner, normalized, nil
}

func normalizeReason(reason string) (string, error) {
	if reason == "" {
		return "", nil
	}
	reason = strings.ReplaceAll(reason, "\r\n", "\n")
	reason = strings.ReplaceAll(reason, "\r", "\n")
	var lines []string
	for _, line := range strings.Split(reason, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		if strings.HasPrefix(line, "owner:") {
			return "", fmt.Errorf("hold: expected a reason, found an owner line %q", line)
		}
		lines = append(lines, line)
	}
	if len(lines) == 0 {
		return "", errors.New("hold: expected a non-empty reason, found empty")
	}
	return strings.Join(lines, "\n"), nil
}

// WriteHoldFile creates or replaces the hold file at path.
// An empty owner and reason still create the file, and that empty file holds.
func WriteHoldFile(path, owner, reason string) error {
	owner, reason, err := PrepareHold(owner, reason)
	if err != nil {
		return err
	}
	if path == "" {
		return errors.New("hold: expected a path, found empty")
	}
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("hold: expected to create %s: %w", dir, err)
	}
	if err := os.WriteFile(path, encodeHold(owner, reason), 0o600); err != nil {
		return fmt.Errorf("hold: expected to write %s: %w", path, err)
	}
	return nil
}

func encodeHold(owner, reason string) []byte {
	var b strings.Builder
	if owner != "" {
		fmt.Fprintf(&b, "owner: %s\n", owner)
	}
	if reason != "" {
		b.WriteString(reason)
		b.WriteByte('\n')
	}
	return []byte(b.String())
}

// RemoveHoldFile deletes the hold file at path.
// A missing file is an error that names the path.
func RemoveHoldFile(path string) error {
	if path == "" {
		return errors.New("release-hold: expected a path, found empty")
	}
	if err := os.Remove(path); err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("release-hold: expected hold file %s, found none", path)
		}
		return fmt.Errorf("release-hold: expected to remove %s: %w", path, err)
	}
	return nil
}

func holdFileReasons(in Inputs) []string {
	if reason := listShape("gate.hold_files", "path", in.HoldFiles); reason != "" {
		return []string{reason}
	}
	ttl := in.HoldTTL
	var reasons []string
	if ttl != nil && *ttl < 0 {
		reasons = append(reasons, fmt.Sprintf("gate.hold_ttl: expected a non-negative duration, found %s", *ttl))
		ttl = nil
	}
	for _, path := range in.HoldFiles {
		if reason := oneHoldFile(path, ttl, in.Now); reason != "" {
			reasons = append(reasons, reason)
		}
	}
	return reasons
}

func oneHoldFile(path string, ttl *time.Duration, now time.Time) string {
	info, err := os.Stat(path)
	if err != nil {
		return holdStatError(path, err)
	}
	stale, age, clock := holdStale(info.ModTime(), ttl, now)
	if clock != "" {
		return holdReason(path, false, 0, "", clock)
	}
	if info.IsDir() {
		return holdReason(path, stale, age, "", "expected a file, found a directory")
	}
	// The path is a configured hold file or the default state path.
	body, err := os.ReadFile(path) //nolint:gosec // G304: hold file path comes from config, not a request.
	if err != nil {
		return holdReason(path, stale, age, "", holdIOText(err))
	}
	owner, reason, problem := parseHold(string(body))
	if problem != "" {
		return holdReason(path, stale, age, "", problem)
	}
	return holdReason(path, stale, age, owner, reason)
}

func holdStatError(path string, err error) string {
	if errors.Is(err, fs.ErrNotExist) {
		return ""
	}
	return holdReason(path, false, 0, "", holdIOText(err))
}

func holdIOText(err error) string {
	if err == nil || err.Error() == "" {
		return "expected an error text, found empty"
	}
	return err.Error()
}

func holdStale(mod time.Time, ttl *time.Duration, now time.Time) (bool, time.Duration, string) {
	if ttl == nil {
		return false, 0, ""
	}
	if now.IsZero() {
		return false, 0, "gate.hold_ttl: expected a clock, found the zero time"
	}
	age := now.Sub(mod)
	if age < 0 {
		return false, 0, ""
	}
	return age > *ttl, age, ""
}

func holdReason(path string, stale bool, age time.Duration, owner, detail string) string {
	var b strings.Builder
	b.WriteString("hold ")
	b.WriteString(path)
	if stale {
		fmt.Fprintf(&b, " is stale (age %s)", age)
	}
	if owner != "" {
		b.WriteString(": owner ")
		b.WriteString(owner)
	}
	if detail != "" {
		b.WriteString(": ")
		b.WriteString(detail)
	}
	return b.String()
}

func parseHold(body string) (owner, reason, problem string) {
	var reasons []string
	var owners []string
	emptyOwner := false
	for _, line := range strings.Split(body, "\n") {
		trimmed := strings.TrimSpace(strings.TrimRight(line, "\r"))
		if trimmed == "" {
			continue
		}
		rest, ok := strings.CutPrefix(trimmed, "owner:")
		if ok {
			name := strings.TrimSpace(rest)
			if name == "" {
				emptyOwner = true
				continue
			}
			owners = append(owners, name)
			continue
		}
		reasons = append(reasons, trimmed)
	}
	switch {
	case emptyOwner:
		problem = "expected a non-empty owner, found empty"
	case len(owners) > 1:
		problem = fmt.Sprintf("expected one owner line, found %d", len(owners))
	case len(owners) == 1:
		owner = owners[0]
	}
	return owner, strings.Join(reasons, "; "), problem
}

func holdTabReasons(in Inputs) []string {
	if reason := listShape("gate.hold_tabs", "tab label", in.HoldTabs); reason != "" {
		return []string{reason}
	}
	if len(in.HoldTabs) == 0 {
		return nil
	}
	wanted := stringSet(in.HoldTabs)
	var reasons []string
	for _, tab := range in.Tabs {
		if _, ok := wanted[tab.Label]; !ok {
			continue
		}
		if line := holdTabLine(tab); line != "" {
			reasons = append(reasons, line)
		}
	}
	return reasons
}

func holdTabLine(tab herdr.Tab) string {
	switch tab.AgentStatus {
	case herdr.AgentStatusDone:
		return ""
	case herdr.AgentStatusIdle, herdr.AgentStatusWorking, herdr.AgentStatusBlocked, herdr.AgentStatusUnknown:
		return fmt.Sprintf("hold tab %s (%s) is %s", tab.Label, tab.ID, tab.AgentStatus)
	default:
		return fmt.Sprintf("hold tab %s (%s): expected agent_status one of idle, working, blocked, done, unknown, found %q", tab.Label, tab.ID, tab.AgentStatus)
	}
}
