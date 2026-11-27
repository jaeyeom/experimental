//go:build linux

package healthsnap

import (
	"context"
	"fmt"
	"os"
	"runtime"
)

func diskBlockSize(bsize int64) (uint64, error) {
	if bsize <= 0 {
		return 0, fmt.Errorf("invalid block size %d", bsize)
	}
	return uint64(bsize), nil //nolint:gosec // G115: bsize is positive and a block size fits
}

func readHostLoad(context.Context) (LoadStats, error) {
	raw, err := os.ReadFile("/proc/loadavg")
	if err != nil {
		return LoadStats{}, fmt.Errorf("read /proc/loadavg: %w", err)
	}
	return ParseLinuxLoadAvg(string(raw), runtime.NumCPU())
}

func readHostMemory(context.Context) (MemoryStats, error) {
	raw, err := os.ReadFile("/proc/meminfo")
	if err != nil {
		return MemoryStats{}, fmt.Errorf("read /proc/meminfo: %w", err)
	}
	psiRaw, _ := os.ReadFile("/proc/pressure/memory")
	return ParseLinuxMeminfo(string(raw), string(psiRaw))
}
