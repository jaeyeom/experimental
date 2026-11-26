package doctor

import (
	"encoding/json"
	"fmt"
	"strings"
)

// FormatText groups findings by check.
// Each group starts with a count header such as "unknown-blocker (3)".
// No findings returns an empty string.
func FormatText(findings []Finding) string {
	findings = order(append([]Finding(nil), findings...))
	if len(findings) == 0 {
		return ""
	}
	var b strings.Builder
	for i := 0; i < len(findings); {
		check := findings[i].Check
		j := i + 1
		for j < len(findings) && findings[j].Check == check {
			j++
		}
		writef(&b, "%s (%d)\n", check, j-i)
		for _, found := range findings[i:j] {
			writef(&b, "  %s  %s\n", found.Severity, found.Message)
			writef(&b, "        %s\n", found.Suggestion)
		}
		i = j
	}
	return b.String()
}

// FormatJSON prints findings as an indented array.
// A nil or empty slice is an empty array, not null.
func FormatJSON(findings []Finding) (string, error) {
	findings = order(append([]Finding(nil), findings...))
	raw, err := json.MarshalIndent(findings, "", "  ")
	if err != nil {
		return "", fmt.Errorf("doctor json: %w", err)
	}
	return string(raw) + "\n", nil
}

func writef(b *strings.Builder, format string, args ...any) {
	if _, err := fmt.Fprintf(b, format, args...); err != nil {
		panic(fmt.Sprintf("format: expected a successful write, found %v", err))
	}
}
