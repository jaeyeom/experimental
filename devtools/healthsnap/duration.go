package healthsnap

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"
)

// ParseDuration parses a non-negative duration string supporting Go's
// time.ParseDuration units plus "d" (24h) and "w" (7*24h) prefixes.
func ParseDuration(raw string) (time.Duration, error) {
	s := strings.TrimSpace(raw)
	if s == "" {
		return 0, errors.New("empty duration")
	}
	if strings.HasPrefix(s, "-") {
		return 0, fmt.Errorf("duration %q must be non-negative", raw)
	}
	if s == "0" {
		return 0, nil
	}

	total, rest, err := parseDayWeekPrefix(s)
	if err != nil {
		return 0, fmt.Errorf("parse duration %q: %w", raw, err)
	}
	if rest == "" {
		return total, nil
	}
	tail, err := time.ParseDuration(rest)
	if err != nil {
		return 0, fmt.Errorf("parse duration %q: %w", raw, err)
	}
	return total + tail, nil
}

func parseDayWeekPrefix(s string) (time.Duration, string, error) {
	var total time.Duration
	for s != "" {
		idx := findUnitStart(s)
		if idx <= 0 || idx >= len(s) {
			break
		}
		unit := s[idx]
		if unit != 'w' && unit != 'd' {
			break
		}
		val, err := strconv.ParseFloat(s[:idx], 64)
		if err != nil || val < 0 {
			return 0, "", fmt.Errorf("invalid number %q", s[:idx])
		}
		mult := 24 * time.Hour
		if unit == 'w' {
			mult = 7 * 24 * time.Hour
		}
		total += time.Duration(val * float64(mult))
		s = s[idx+1:]
	}
	return total, s, nil
}

func findUnitStart(s string) int {
	for i := 0; i < len(s); i++ {
		c := s[i]
		if (c < '0' || c > '9') && c != '.' {
			return i
		}
	}
	return len(s)
}
