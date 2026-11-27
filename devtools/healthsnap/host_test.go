package healthsnap

import (
	"context"
	"encoding/binary"
	"math"
	"os"
	"os/exec"
	"runtime"
	"testing"
)

func TestParseLinuxLoadAvg(t *testing.T) {
	got, err := ParseLinuxLoadAvg("2.39 2.69 2.77 3/912 42101\n", 4)
	if err != nil {
		t.Fatalf("ParseLinuxLoadAvg = %v", err)
	}
	if got.Load1 != 2.39 || got.Load5 != 2.69 || got.Load15 != 2.77 {
		t.Errorf("loads = %+v", got)
	}
	if got.CPUs != 4 || math.Abs(got.LoadPerCPU-(2.39/4.0)) > 1e-9 {
		t.Errorf("CPUs=%d LoadPerCPU=%v", got.CPUs, got.LoadPerCPU)
	}
	if _, err := ParseLinuxLoadAvg("bad", 4); err == nil {
		t.Fatal("expected error for malformed /proc/loadavg")
	}
}

func TestParseDarwinLoadAvgBytes(t *testing.T) {
	// 24-byte 64-bit struct loadavg: ldavg[3] uint32 + 4 pad + int64 fscale.
	raw24 := make([]byte, 24)
	binary.LittleEndian.PutUint32(raw24[0:4], 4096)
	binary.LittleEndian.PutUint32(raw24[4:8], 2048)
	binary.LittleEndian.PutUint32(raw24[8:12], 1024)
	binary.LittleEndian.PutUint64(raw24[16:24], 2048)

	got, err := ParseDarwinLoadAvgBytes(raw24, binary.LittleEndian, 8)
	if err != nil {
		t.Fatalf("ParseDarwinLoadAvgBytes(24) = %v", err)
	}
	if got.Load1 != 2.0 || got.Load5 != 1.0 || got.Load15 != 0.5 || got.LoadPerCPU != 0.25 {
		t.Errorf("got = %+v", got)
	}

	// 16-byte 32-bit struct loadavg: ldavg[3] uint32 + uint32 fscale.
	raw16 := make([]byte, 16)
	binary.LittleEndian.PutUint32(raw16[0:4], 3072)
	binary.LittleEndian.PutUint32(raw16[4:8], 2048)
	binary.LittleEndian.PutUint32(raw16[8:12], 1024)
	binary.LittleEndian.PutUint32(raw16[12:16], 2048)

	got16, err := ParseDarwinLoadAvgBytes(raw16, binary.LittleEndian, 2)
	if err != nil {
		t.Fatalf("ParseDarwinLoadAvgBytes(16) = %v", err)
	}
	if got16.Load1 != 1.5 || got16.LoadPerCPU != 0.75 {
		t.Errorf("got16 = %+v", got16)
	}

	if _, err := ParseDarwinLoadAvgBytes([]byte{1, 2, 3}, binary.LittleEndian, 1); err == nil {
		t.Fatal("expected error for short vm.loadavg buffer")
	}
}

func TestParseLinuxMeminfo(t *testing.T) {
	meminfo := `MemTotal:       16000000 kB
MemFree:         1000000 kB
MemAvailable:    4000000 kB
Buffers:          500000 kB
Cached:          2500000 kB
`
	psi := `some avg10=12.34 avg60=5.00 avg300=1.20 total=1000
full avg10=0.00 avg60=0.00 avg300=0.00 total=0
`
	got, err := ParseLinuxMeminfo(meminfo, psi)
	if err != nil {
		t.Fatalf("ParseLinuxMeminfo = %v", err)
	}
	if got.TotalBytes != 16000000*1024 || got.AvailableBytes != 4000000*1024 {
		t.Errorf("bytes = %+v", got)
	}
	if math.Abs(got.UsedPercent-75.0) > 1e-6 {
		t.Errorf("UsedPercent = %v, want 75.0", got.UsedPercent)
	}
	if got.PSISomeAvg10 != 12.34 || got.PressureLevel != "warn" {
		t.Errorf("PSI/Level = %+v", got)
	}

	// Fallback when MemAvailable is missing.
	noAvail := "MemTotal: 1000 kB\nMemFree: 200 kB\nBuffers: 100 kB\nCached: 200 kB\n"
	fallback, err := ParseLinuxMeminfo(noAvail, "")
	if err != nil {
		t.Fatalf("ParseLinuxMeminfo fallback = %v", err)
	}
	if fallback.AvailableBytes != 500*1024 || math.Abs(fallback.UsedPercent-50.0) > 1e-6 {
		t.Errorf("fallback = %+v", fallback)
	}
}

func TestBuildDarwinMemoryStats(t *testing.T) {
	got, err := BuildDarwinMemoryStats(1000, 200, 2)
	if err != nil {
		t.Fatalf("BuildDarwinMemoryStats = %v", err)
	}
	if math.Abs(got.UsedPercent-80.0) > 1e-6 || got.PressureLevel != "warn" {
		t.Errorf("got = %+v", got)
	}
	crit, err := BuildDarwinMemoryStats(1000, 50, 4)
	if err != nil {
		t.Fatalf("BuildDarwinMemoryStats(4) = %v", err)
	}
	if crit.PressureLevel != "critical" {
		t.Errorf("crit PressureLevel = %q, want critical", crit.PressureLevel)
	}
}

func TestComputeDiskStats(t *testing.T) {
	got, err := ComputeDiskStats(25*(1<<30), 100*(1<<30))
	if err != nil {
		t.Fatalf("ComputeDiskStats = %v", err)
	}
	if math.Abs(got.FreePercent-25.0) > 1e-6 || math.Abs(got.UsedPercent-75.0) > 1e-6 || math.Abs(got.FreeGiB-25.0) > 1e-6 {
		t.Errorf("got = %+v", got)
	}
	if _, err := ComputeDiskStats(0, 0); err == nil {
		t.Fatal("expected error for zero total disk bytes")
	}
}

func TestSystemHostReaderLive(t *testing.T) {
	if runtime.GOOS != "linux" && runtime.GOOS != "darwin" {
		t.Skipf("skipping on %s", runtime.GOOS)
	}
	ctx := context.Background()
	r := SystemHostReader{}

	load, err := r.Load(ctx)
	if err != nil {
		t.Fatalf("Load() = %v", err)
	}
	if load.CPUs < 1 || load.Load1 < 0 {
		t.Fatalf("unexpected Load = %+v", load)
	}

	disk, err := r.Disk(ctx, "/")
	if err != nil {
		t.Fatalf("Disk(/) = %v", err)
	}
	if disk.TotalBytes == 0 || disk.FreePercent < 0 || disk.FreePercent > 100 {
		t.Fatalf("unexpected Disk = %+v", disk)
	}

	if runtime.GOOS == "darwin" {
		if _, err := exec.LookPath("vm_stat"); err != nil {
			t.Skip("vm_stat not on PATH")
		}
	}
	if runtime.GOOS == "linux" {
		if _, err := os.Stat("/proc/meminfo"); err != nil {
			t.Skip("/proc/meminfo unavailable")
		}
	}
	mem, err := r.Memory(ctx)
	if err != nil {
		t.Fatalf("Memory() = %v", err)
	}
	if mem.TotalBytes == 0 || mem.UsedPercent < 0 || mem.UsedPercent > 100 {
		t.Fatalf("unexpected Memory = %+v", mem)
	}
}
