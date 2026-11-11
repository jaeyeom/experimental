package procinfo

import (
	"context"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// Memory reports how much memory the system can still give out.
type Memory interface {
	// Available is in bytes.
	Available(ctx context.Context) (int64, error)
}

// LocalMemory is this machine's: MemAvailable from /proc/meminfo where
// it exists (Linux), else free, inactive, and purgeable pages from
// vm_stat (macOS).
func LocalMemory() Memory {
	if _, err := os.Stat("/proc/meminfo"); err == nil {
		return MeminfoFile{Path: "/proc/meminfo"}
	}
	return VMStat{Run: Run}
}

// MeminfoFile reads MemAvailable from a /proc/meminfo file.
type MeminfoFile struct {
	Path string
}

// Available reads and parses the file.
func (m MeminfoFile) Available(context.Context) (int64, error) {
	raw, err := os.ReadFile(m.Path)
	if err != nil {
		return 0, fmt.Errorf("read meminfo: %w", err)
	}
	return ParseMeminfo(string(raw))
}

// ParseMeminfo is MemAvailable in bytes from /proc/meminfo text.
func ParseMeminfo(text string) (int64, error) {
	for _, line := range strings.Split(text, "\n") {
		rest, ok := strings.CutPrefix(line, "MemAvailable:")
		if !ok {
			continue
		}
		fields := strings.Fields(rest)
		if len(fields) == 0 {
			break
		}
		n, err := strconv.ParseInt(fields[0], 10, 64)
		if err != nil {
			return 0, fmt.Errorf("meminfo MemAvailable %q: %w", fields[0], err)
		}
		if len(fields) > 1 && strings.EqualFold(fields[1], "kB") {
			n *= 1024
		}
		return n, nil
	}
	return 0, errors.New("meminfo has no MemAvailable")
}

// VMStat runs vm_stat for the available pages.
type VMStat struct {
	Run Runner
}

// Available runs vm_stat once and parses it.
func (v VMStat) Available(ctx context.Context) (int64, error) {
	out, err := v.Run(ctx, "vm_stat")
	if err != nil {
		return 0, fmt.Errorf("run vm_stat: %w", err)
	}
	return ParseVMStat(out)
}

var vmPageSize = regexp.MustCompile(`page size of (\d+) bytes`)

// ParseVMStat is free + inactive + purgeable pages, in bytes, from
// vm_stat output. Pages free and the page size must be present.
func ParseVMStat(out string) (int64, error) {
	m := vmPageSize.FindStringSubmatch(out)
	if m == nil {
		return 0, errors.New("vm_stat output has no page size")
	}
	pageSize, err := strconv.ParseInt(m[1], 10, 64)
	if err != nil {
		return 0, fmt.Errorf("vm_stat page size %q: %w", m[1], err)
	}
	pages := map[string]int64{}
	for _, line := range strings.Split(out, "\n") {
		key, value, ok := strings.Cut(line, ":")
		if !ok {
			continue
		}
		n, err := strconv.ParseInt(strings.TrimSuffix(strings.TrimSpace(value), "."), 10, 64)
		if err == nil {
			pages[strings.TrimSpace(key)] = n
		}
	}
	free, ok := pages["Pages free"]
	if !ok {
		return 0, errors.New("vm_stat output has no Pages free")
	}
	return (free + pages["Pages inactive"] + pages["Pages purgeable"]) * pageSize, nil
}
