package healthsnap

import (
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// LoadStats reports host CPU load averages.
type LoadStats struct {
	Load1      float64
	Load5      float64
	Load15     float64
	CPUs       int
	LoadPerCPU float64
}

// MemoryStats reports host memory pressure and capacity.
type MemoryStats struct {
	TotalBytes     uint64
	AvailableBytes uint64
	UsedPercent    float64
	PressureLevel  string
	PSISomeAvg10   float64
}

// DiskStats reports filesystem free and used capacity for a path.
type DiskStats struct {
	FreeBytes   uint64
	TotalBytes  uint64
	FreePercent float64
	UsedPercent float64
	FreeGiB     float64
}

// HostReader reads host-level load, memory, and disk metrics.
type HostReader interface {
	Load(ctx context.Context) (LoadStats, error)
	Memory(ctx context.Context) (MemoryStats, error)
	Disk(ctx context.Context, path string) (DiskStats, error)
}

// SystemHostReader reads live metrics from the local Linux or macOS host.
type SystemHostReader struct{}

// Load returns the local host's load averages.
func (SystemHostReader) Load(ctx context.Context) (LoadStats, error) {
	return readHostLoad(ctx)
}

// Memory returns the local host's memory pressure statistics.
func (SystemHostReader) Memory(ctx context.Context) (MemoryStats, error) {
	return readHostMemory(ctx)
}

// Disk returns filesystem capacity statistics for path.
func (SystemHostReader) Disk(_ context.Context, path string) (DiskStats, error) {
	free, total, err := statDiskUsage(path)
	if err != nil {
		return DiskStats{}, err
	}
	return ComputeDiskStats(free, total)
}

// ParseLinuxLoadAvg parses the contents of /proc/loadavg.
func ParseLinuxLoadAvg(text string, numCPU int) (LoadStats, error) {
	fields := strings.Fields(text)
	if len(fields) < 3 {
		return LoadStats{}, fmt.Errorf("parse /proc/loadavg: expected at least 3 fields, got %d", len(fields))
	}
	l1, err := strconv.ParseFloat(fields[0], 64)
	if err != nil {
		return LoadStats{}, fmt.Errorf("parse load1 %q: %w", fields[0], err)
	}
	l5, err := strconv.ParseFloat(fields[1], 64)
	if err != nil {
		return LoadStats{}, fmt.Errorf("parse load5 %q: %w", fields[1], err)
	}
	l15, err := strconv.ParseFloat(fields[2], 64)
	if err != nil {
		return LoadStats{}, fmt.Errorf("parse load15 %q: %w", fields[2], err)
	}
	return buildLoadStats(l1, l5, l15, numCPU), nil
}

// ParseDarwinLoadAvgRaw decodes the raw bytes returned by sysctl vm.loadavg.
func ParseDarwinLoadAvgRaw(raw []byte, numCPU int) (LoadStats, error) {
	return ParseDarwinLoadAvgBytes(raw, binary.NativeEndian, numCPU)
}

// ParseDarwinLoadAvgBytes decodes raw vm.loadavg bytes using the given byte order.
func ParseDarwinLoadAvgBytes(raw []byte, order binary.ByteOrder, numCPU int) (LoadStats, error) {
	var fscale uint64
	switch len(raw) {
	case 24:
		fscale = order.Uint64(raw[16:24])
	case 16:
		fscale = uint64(order.Uint32(raw[12:16]))
	default:
		return LoadStats{}, fmt.Errorf("unexpected vm.loadavg size %d", len(raw))
	}
	if fscale == 0 {
		return LoadStats{}, errors.New("vm.loadavg has zero fscale")
	}
	scale := float64(fscale)
	l1 := float64(order.Uint32(raw[0:4])) / scale
	l5 := float64(order.Uint32(raw[4:8])) / scale
	l15 := float64(order.Uint32(raw[8:12])) / scale
	return buildLoadStats(l1, l5, l15, numCPU), nil
}

func buildLoadStats(l1, l5, l15 float64, numCPU int) LoadStats {
	if numCPU < 1 {
		numCPU = 1
	}
	return LoadStats{
		Load1:      l1,
		Load5:      l5,
		Load15:     l15,
		CPUs:       numCPU,
		LoadPerCPU: l1 / float64(numCPU),
	}
}

// ParseLinuxMeminfo parses /proc/meminfo and optional /proc/pressure/memory text.
func ParseLinuxMeminfo(meminfoText, psiText string) (MemoryStats, error) {
	fields := parseMeminfoKB(meminfoText)
	totalKB, ok := fields["MemTotal"]
	if !ok || totalKB <= 0 {
		return MemoryStats{}, errors.New("meminfo missing positive MemTotal")
	}
	availBytes, err := procinfo.ParseMeminfo(meminfoText)
	if err != nil {
		availKB := fields["MemFree"] + fields["Buffers"] + fields["Cached"]
		if availKB <= 0 {
			return MemoryStats{}, errors.New("meminfo missing MemAvailable and fallback fields")
		}
		availBytes = availKB * 1024
	}
	if availBytes < 0 {
		availBytes = 0
	}
	totalBytes := uint64(totalKB) * 1024 //nolint:gosec // G115: totalKB > 0 checked above
	avail := uint64(availBytes)          //nolint:gosec // G115: availBytes >= 0 clamped above
	if avail > totalBytes {
		avail = totalBytes
	}
	usedPct := float64(totalBytes-avail) * 100.0 / float64(totalBytes)
	psi := parseLinuxPSISomeAvg10(psiText)
	return MemoryStats{
		TotalBytes:     totalBytes,
		AvailableBytes: avail,
		UsedPercent:    usedPct,
		PressureLevel:  linuxPressureLevel(usedPct, psi),
		PSISomeAvg10:   psi,
	}, nil
}

func parseMeminfoKB(text string) map[string]int64 {
	out := make(map[string]int64)
	for _, line := range strings.Split(text, "\n") {
		key, rest, ok := strings.Cut(line, ":")
		if !ok {
			continue
		}
		parts := strings.Fields(rest)
		if len(parts) == 0 {
			continue
		}
		val, err := strconv.ParseInt(parts[0], 10, 64)
		if err != nil {
			continue
		}
		out[strings.TrimSpace(key)] = val
	}
	return out
}

func parseLinuxPSISomeAvg10(psiText string) float64 {
	for _, line := range strings.Split(psiText, "\n") {
		if !strings.HasPrefix(line, "some ") {
			continue
		}
		for _, tok := range strings.Fields(line) {
			if rest, ok := strings.CutPrefix(tok, "avg10="); ok {
				if v, err := strconv.ParseFloat(rest, 64); err == nil {
					return v
				}
			}
		}
	}
	return 0
}

func linuxPressureLevel(usedPct, psiAvg10 float64) string {
	switch {
	case psiAvg10 >= 25 || usedPct >= 95:
		return "critical"
	case psiAvg10 >= 10 || usedPct >= 85:
		return "warn"
	default:
		return "normal"
	}
}

// BuildDarwinMemoryStats builds MemoryStats from macOS hw.memsize, available
// bytes (from vm_stat), and kern.memorystatus_vm_pressure_level.
func BuildDarwinMemoryStats(totalBytes uint64, availableBytes int64, pressureLevel uint32) (MemoryStats, error) {
	if totalBytes == 0 {
		return MemoryStats{}, errors.New("hw.memsize is zero")
	}
	if availableBytes < 0 {
		availableBytes = 0
	}
	avail := uint64(availableBytes) //nolint:gosec // G115: clamped >= 0 above
	if avail > totalBytes {
		avail = totalBytes
	}
	usedPct := float64(totalBytes-avail) * 100.0 / float64(totalBytes)
	return MemoryStats{
		TotalBytes:     totalBytes,
		AvailableBytes: avail,
		UsedPercent:    usedPct,
		PressureLevel:  darwinPressureLevelName(pressureLevel, usedPct),
	}, nil
}

func darwinPressureLevelName(level uint32, usedPct float64) string {
	switch level {
	case 4:
		return "critical"
	case 2:
		return "warn"
	case 1:
		return "normal"
	default:
		return linuxPressureLevel(usedPct, 0)
	}
}

// ComputeDiskStats computes free and used disk statistics from byte counts.
func ComputeDiskStats(freeBytes, totalBytes uint64) (DiskStats, error) {
	if totalBytes == 0 {
		return DiskStats{}, errors.New("filesystem total bytes is zero")
	}
	if freeBytes > totalBytes {
		freeBytes = totalBytes
	}
	freePct := float64(freeBytes) * 100.0 / float64(totalBytes)
	usedPct := float64(totalBytes-freeBytes) * 100.0 / float64(totalBytes)
	return DiskStats{
		FreeBytes:   freeBytes,
		TotalBytes:  totalBytes,
		FreePercent: freePct,
		UsedPercent: usedPct,
		FreeGiB:     float64(freeBytes) / float64(1<<30),
	}, nil
}
