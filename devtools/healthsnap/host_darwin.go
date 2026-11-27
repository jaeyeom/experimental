//go:build darwin

package healthsnap

import (
	"context"
	"fmt"
	"runtime"

	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
	"golang.org/x/sys/unix"
)

func diskBlockSize(bsize uint32) (uint64, error) {
	if bsize == 0 {
		return 0, fmt.Errorf("invalid block size %d", bsize)
	}
	return uint64(bsize), nil
}

func readHostLoad(context.Context) (LoadStats, error) {
	raw, err := unix.SysctlRaw("vm.loadavg")
	if err != nil {
		return LoadStats{}, fmt.Errorf("sysctl vm.loadavg: %w", err)
	}
	return ParseDarwinLoadAvgRaw(raw, runtime.NumCPU())
}

func readHostMemory(ctx context.Context) (MemoryStats, error) {
	total, err := unix.SysctlUint64("hw.memsize")
	if err != nil {
		return MemoryStats{}, fmt.Errorf("sysctl hw.memsize: %w", err)
	}
	avail, err := procinfo.VMStat{Run: procinfo.Run}.Available(ctx)
	if err != nil {
		return MemoryStats{}, fmt.Errorf("read vm_stat memory: %w", err)
	}
	level, err := unix.SysctlUint32("kern.memorystatus_vm_pressure_level")
	if err != nil {
		level = 0
	}
	return BuildDarwinMemoryStats(total, avail, level)
}
