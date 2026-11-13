package diskwatch

import (
	"fmt"
	"strconv"
	"strings"
)

// Amount is a free-space threshold as a percent or a byte count.
// Suffixes K, M, G, and T are powers of 1024. A trailing B or iB is
// accepted and means the same thing.
type Amount struct {
	IsPercent bool
	Percent   float64
	Bytes     uint64
}

// ParseAmount parses "15%" or "40G".
func ParseAmount(raw string) (Amount, error) {
	text := strings.TrimSpace(raw)
	if text == "" {
		return Amount{}, fmt.Errorf("parse amount %q: empty", raw)
	}
	numEnd := 0
	for numEnd < len(text) && (text[numEnd] == '.' || (text[numEnd] >= '0' && text[numEnd] <= '9')) {
		numEnd++
	}
	if numEnd == 0 {
		return Amount{}, fmt.Errorf("parse amount %q: want a percent or a size such as 15%% or 40G", raw)
	}
	n, err := strconv.ParseFloat(text[:numEnd], 64)
	if err != nil || n < 0 {
		return Amount{}, fmt.Errorf("parse amount %q: want a percent or a size such as 15%% or 40G", raw)
	}
	unit := strings.ToUpper(strings.TrimSpace(text[numEnd:]))
	if unit == "%" {
		if n > 100 {
			return Amount{}, fmt.Errorf("parse amount %q: percent is over 100", raw)
		}
		return Amount{IsPercent: true, Percent: n}, nil
	}
	mul, err := byteScale(unit)
	if err != nil {
		return Amount{}, fmt.Errorf("parse amount %q: %w", raw, err)
	}
	if n > float64(^uint64(0))/float64(mul) {
		return Amount{}, fmt.Errorf("parse amount %q: overflows uint64", raw)
	}
	return Amount{Bytes: uint64(n * float64(mul))}, nil
}

func byteScale(unit string) (uint64, error) {
	switch unit {
	case "", "B":
		return 1, nil
	case "K", "KB", "KIB":
		return 1 << 10, nil
	case "M", "MB", "MIB":
		return 1 << 20, nil
	case "G", "GB", "GIB":
		return 1 << 30, nil
	case "T", "TB", "TIB":
		return 1 << 40, nil
	default:
		return 0, fmt.Errorf("unknown unit %q", unit)
	}
}

// Below reports whether free space is under this threshold.
func (a Amount) Below(free, total uint64) bool {
	if a.IsPercent {
		return FreePct(free, total) < a.Percent
	}
	return free < a.Bytes
}

// Above reports whether free space is over this threshold.
func (a Amount) Above(free, total uint64) bool {
	if a.IsPercent {
		return FreePct(free, total) > a.Percent
	}
	return free > a.Bytes
}

// Less reports whether a is a lower threshold than b.
// The amounts must use the same unit.
func (a Amount) Less(b Amount) (bool, error) {
	if a.IsPercent != b.IsPercent {
		return false, fmt.Errorf("thresholds must use the same unit")
	}
	if a.IsPercent {
		return a.Percent < b.Percent, nil
	}
	return a.Bytes < b.Bytes, nil
}

// FreePct is free/total as a percentage. A zero total is 0.
func FreePct(free, total uint64) float64 {
	if total == 0 {
		return 0
	}
	return float64(free) / float64(total) * 100
}
